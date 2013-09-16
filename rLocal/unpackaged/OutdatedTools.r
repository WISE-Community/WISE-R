## The read.xlsx.wiseDir function creates a data frame that contains all data from the given directory
## Since this is a "flat" data.frame, i.e., the different files and tabs are all stored in the
## same data.frame columns have been added to differentiate between runs, workgroups, etc.
read.xlsx.wiseDir = function (dir, DEBUG = FALSE, DEEPDEBUG = FALSE){
	files = list.files(dir);
	df = data.frame();
	# Iterate the first time through all the files to find the largest number of columns
	#  e.g. a questionarre could have multiple "student work" columns
	maxColumns = 0;
	sheetCounts = numeric();
	for (f in files){
		## don't include any files in temporary format
		if (substr(f,0,1) != "~"  && grepl(".xl", f)[1]){
			dirf = paste(dir, f, sep="");
			if (DEBUG) print(f)
			sheetCount = countExcelSheets(dirf);
			sheetCounts = c(sheetCounts, sheetCount);
			for (s in 1:sheetCount){
				if (DEBUG) print(s)
				# get first row
				head = read.xlsx2(dirf, sheetIndex = s, startRow=1, endRow=1, stringsAsFactors = FALSE);
				if (DEBUG) print("Got header")
				if (DEEPDEBUG) print(head)
				pbody = read.xlsx2(dirf, sheetIndex = s, startRow=4, endRow = 4, stringsAsFactors = FALSE);
				if (DEBUG) print("Got body")
				if (DEEPDEBUG) print(pbody)
				pbody = subset(pbody,TRUE,which(names(pbody) != "Workgroup.Id"))[-1,]
				if (DEBUG) print("Got body with character classes")
				firstline = cbind(head, pbody);
				# we validate that this is a WISE data file by looking for the Workgroup.Id in the top left 
				if (names(head)[1] == "Workgroup.Id"){
					if (ncol(firstline) > maxColumns){
						maxColumns = ncol(firstline);
						maxFile = f;
						maxSheetIndex = s;
					}
				}
			}
		}
	}
	## replace Student.Work with Student.Work.Part.1
	if (length(which(names(df) == "Student.Work")) == 1){
		names(df)[which(names(df) == "Student.Work")] = "Student.Work.Part.1";
	}

	### recreate data frame with the correct column classes
	if (maxColumns > 0){
		dirf = paste(dir, maxFile, sep="");
		head = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=1, endRow=1, stringsAsFactors = FALSE);
		colClassesHead = getColClasses.read(names(head));
		head = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=1, endRow=1, colClasses=colClassesHead, stringsAsFactors = FALSE);
		if (oldHeaderFormat){
			pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=4, endRow = 4, stringsAsFactors = FALSE)
			pbody = subset(pbody,TRUE,which(names(pbody) != "Workgroup.Id"))[-1,] ## remove Workgroup.Id column from body, if it exists
			colClassesBody = getColClasses.read(names(pbody));
			pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=4, endRow = 4, colClasses=colClassesBody, stringsAsFactors = FALSE)
			pbody = subset(pbody,TRUE,which(names(pbody) != "Workgroup.Id"))[-1,] ## remove Workgroup.Id column from body, if it exists
		} else {
			pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=4, endRow = 4, colClasses=colClassesHead, stringsAsFactors = FALSE)
		}
		df = cbind(head, pbody);
	} else { return (df);}

	## now extract data
	findex = 0;
	for (f in files){
		findex = findex + 1;
		## don't include any files in temporary format
		if (substr(f,0,1) != "~"  && grepl(".xl", f)[1]){
			dirf = paste(dir, f, sep="");
			sheetCount = countExcelSheets(dirf);
			#sheetCount = sheetCounts[findex];
			for (s in 1:sheetCount){
				# get header information
				head = read.xlsx2(dirf, sheetIndex = s, startRow=1, endRow=2, colClasses=colClassesHead, stringsAsFactors = FALSE);
				# we validate that this is a WISE data file by looking for the Workgroup.Id in the top left 
				if (names(head)[1] == "Workgroup.Id"){
					body = read.xlsx2(dirf, sheetIndex = s, startRow=4, colClasses=colClassesBody, stringsAsFactors = FALSE)
					if (nrow(body) > 0 && prod(is.na(body))==0){  #is there data here?
						body = subset(body,TRUE,which(names(body) != "Workgroup.Id")) ## remove Workgroup.Id column from body, if it exists
						full = cbind(head[rep(seq_len(nrow(head)),each=nrow(body)),],body);
						## if full has less cols than df, populate with dummy data
						if (ncol(full) < ncol(df)){
							for (c in (ncol(full)+1):ncol(df)){
								full = cbind(full, dum=rep("",nrow(full)))
								names(full)[c] = names(df)[c]; 
							}
						}
						## replace Student.Work with Student.Work.Part.1
						if (length(which(names(full) == "Student.Work")) == 1){
							names(full)[which(names(full) == "Student.Work")] = "Student.Work.Part.1";
						}
						## look for a mismatch error between number of cols
						if (ncol(df) != 0 && ncol(full) != ncol(df)){print(names(full)); print(names(df)); print(f); print(s); print("column length mismatch")}
						if (length(names(df)) != length(intersect(names(df),names(full)))){print(names(full)); print(names(df)); print(f); print(s); print("name mismatch")}
						df = rbind(df, full);
					}
				}
			}
		}
	}
	# make sure there aren't any empty rows - i.e. those without a wise id
	df = subset(df, !is.nan(Wise.Id.1))
	# create an index
	df = cbind(Index=1:nrow(df), df);
	# create a Step.Num column
	df = cbind(df, Step.Num = getStepNum(df));  
	df = cbind(df, Step.Num.NoBranch = collapseStepNumBranches(df$Step.Num))
	
	## place revision numbers on this data frame.
	df = cbind(df, Rev.Num = getRevisionNumber(df,FALSE));
	df = cbind(df, IRev.Num = getRevisionNumber(df,TRUE));
	df = cbind(df, URev.Num = getRevisionNumber(df,FALSE, TRUE));
	df = cbind(df, IURev.Num = getRevisionNumber(df,TRUE, TRUE));

	## place a couple rows for researchers 
	df = cbind(df, Research.Score = rep(NA,nrow(df)));
	df = cbind(df, Research.FeedbackGiven = rep("",nrow(df)));
	df = cbind(df, Research.Notes = rep("",nrow(df)));


	### clean up names a bit (remove .., . at end, "if applicable")
	names(df) = gsub("..if.applicable.", "", names(df));
	names(df) = gsub("\\.\\.", "\\.", names(df));
	names(df) = gsub("\\.$", "", names(df));
	### get final classes and update
	fcolClasses = getColClasses.final(df);
	for (c in 1:ncol(df)){
		if (fcolClasses[c] == "numeric"){
			df[,c] = as.numeric(df[,c]);
		} else if (fcolClasses[c] == "factor"){
			df[,c] = as.factor(df[,c]);
		} else if (fcolClasses[c] == "character"){
			df[,c] = as.character(df[,c]);
		}
	}

	class(df) = c("wisedata.frame",class(df));
	return (df);
}
