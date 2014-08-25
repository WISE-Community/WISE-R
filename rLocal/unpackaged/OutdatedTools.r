## The read.xlsx.wiseDir function creates a data frame that contains all data from the given directory
## Since this is a "flat" data.frame, i.e., the different files and tabs are all stored in the
## same data.frame columns have been added to differentiate between runs, workgroups, etc.
## UPDATES:
### - excel file now contains a workgroup Id column, will check for this
#\name{read.xlsx.wiseDir}
#\description
read.xlsx.wiseDir <- function (dir, Student.Work.Count = 4, do.collapseMultipleRevisions = TRUE, sheetIndices = 1:1000, fileIndices = NULL, DEBUG = FALSE, DEEPDEBUG = FALSE, ...){
	library(plyr)
	files = list.files(dir);
	# rm open files
	files = files[!grepl("~",files)]
	if (!is.null(fileIndices)) files = files[fileIndices]
	df = data.frame();
	# Iterate the first time through all the files to find the largest number of columns
	#  e.g. a questionarre could have multiple "student work" columns
	maxColumns = 0;
	maxStudentWorkCols = 0;
	for (f in files){
		## don't include any files in temporary format
		if (substr(f,0,1) != "~"  && grepl(".xl", f)[1]){
			filename = paste(dir, f, sep="");
			if (DEBUG) print(f)
			colClasses = vector();
			sheetCount = length(getSheets(loadWorkbook(filename)))
			filedf = data.frame()
			for (s in sheetIndices[sheetIndices%in%(1:sheetCount)]){
				if (DEBUG) print(s)
				if (length(colClasses) == 0){
					### if we haven't found our first sheet yet check here and then retrieve column classes
					head = read.xlsx(filename, sheetIndex = s, startRow=1, endRow=2, stringsAsFactors = FALSE);
					#print(names(head))
					colClasses = getColClasses.read(names(head));
				}
				# get sheet
				sheetdf = read.xlsx(filename, sheetIndex = s, colClasses=colClasses, stringsAsFactors = FALSE);
				#print(names(sheetdf))
				#print(names(filedf))
				filedf = rbind.fill(filedf, sheetdf)
			} 
			if (!is.null(filedf)){
				#print(filedf)
				## Make sure we are using a standard Student.Work.Part.1 for first column of student work
				if (length(which(names(filedf) == "Student.Work")) == 1){
					names(filedf)[which(names(filedf) == "Student.Work")] = "Student.Work.Part.1";
				}
				# assume that any columns after last student work are more student work
				lindex = max(grep("Student.Work", names(filedf)))
				print(paste(f,is.null(filedf), lindex, ncol(filedf)))
				if (lindex < ncol(filedf)){
					for (li in (lindex+1):ncol(filedf)){
						names(filedf)[li] = paste("Student.Work.Part", length(grep("Student.Work", names(filedf)))+1,sep=".")
					}
				}

				countStudentWorkCols = length(grep("Student.Work", names(filedf)))
				## are there less student work columns than expected?
				if (countStudentWorkCols < Student.Work.Count){
					for (wi in (countStudentWorkCols+1):Student.Work.Count){
						filedf$TEMP = rep("",nrow(filedf))
						names(filedf)[which(names(filedf) == "TEMP")] = paste("Student.Work.Part", wi ,sep=".")
					}
				}

				if (length(df) == 0){
					# if this is the first sheet just set df 
					df = filedf
					maxStudentWorkCols = countStudentWorkCols;
				} else {
					# if this is 2-N sheet, must make sure that there are enough columns 
					if (countStudentWorkCols < maxStudentWorkCols){
						### add columns to sheet
						for (wi in (countStudentWorkCols+1):maxStudentWorkCols){
							filedf[paste('Student.Work.Part',wi,sep='.')] = rep("",nrow(filedf))
						}
					} else if (countStudentWorkCols > maxStudentWorkCols){
						### add columns to df
						for (wi in (maxStudentWorkCols+1):countStudentWorkCols){
							df[paste('Student.Work.Part',wi,sep='.')] = rep("",nrow(df))
						}
					}
					### combine
					df = rbind.fill(df, filedf);
				}			
			}
		}
	}
	# make sure there aren't any empty rows - i.e. those without a wise id
	df = subset(df, !is.nan(as.numeric(as.character(Wise.Id.1))))
	# create an index
	df = cbind(Index=1:nrow(df), df);
	# create a Step.Num column
	df = cbind(df, Step.Num = getStepNum(df));  
	df = cbind(df, Step.Num.NoBranch = collapseStepNumBranches(df$Step.Num))

	if (do.collapseMultipleRevisions) df = collapseMultipleRevisions(df)
	# Make sure all Step.Work.Id are valid
	df = df[!is.na(df$Step.Work.Id),];
	
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
	df = setColClasses.final(df);
	# replace NA in Student Work with blank
	indices = grep("Student.Work", names(df))
	if (length(indices) > 0){
		for (i in indices){
			df[is.na(df[,i]),i] = ""
		}
	}

	class(df) = c("wisedata.frame",class(df));
	return (df);
}


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

read.xlsx.wiseDir.old = function (dir, Student.Work.Count = 4, do.collapseMultipleRevisions = TRUE, sheetIndices = 1:1000, fileIndices = NULL, perl = NULL, use.XLConnect = FALSE, DEBUG = FALSE, DEEPDEBUG = FALSE, ...){
	files = list.files(dir);
	# rm open files
	files = files[!grepl("~",files)]
	if (!is.null(fileIndices)) files = files[fileIndices]
	df = data.frame();
	# Iterate the first time through all the files to find the largest number of columns
	#  e.g. a questionarre could have multiple "student work" columns
	maxColumns = 0;
	maxStudentWorkCols = 0;
	for (f in files){
		## don't include any files in temporary format
		if (substr(f,0,1) != "~"  && (grepl(".xl", f)[1] | grepl(".csv", f)[1]) ){
			filename = paste(dir, f, sep="");
			if (DEBUG) print(f)
			colClasses = vector();
			if (grepl(".xl", f)[1]){
				filedf = data.frame()
				for (s in sheetIndices){
					if (DEBUG) print(s)
					if (length(colClasses) == 0){
						### if we haven't found our first sheet yet check here and then retrieve column classes
						head = tryCatch({
							if (!is.null(perl)) {
								read.xls(filename, sheet = s, perl = perl, nrows=1, stringsAsFactors = FALSE);
							} else if (use.XLConnect) {
								readWorksheetFromFile(filename, sheet = s, startRow=1, endRow=1);								
							} else {
								read.xlsx(filename, sheetIndex = s, startRow=1, endRow=2, stringsAsFactors = FALSE);
							}
						}, error = function(e){
							return(NULL)
						});	
						if (is.null(head)) break;
						#print(names(head))
						colClasses = getColClasses.read(names(head));
					}
					# get sheet
					sheetdf = tryCatch({
						if (!is.null(perl)) {
							read.xls(filename, sheet = s, perl = perl, colClasses=colClasses, stringsAsFactors = FALSE);
						} else if (use.XLConnect) {
							readWorksheetFromFile(filename, sheet = s, colTypes = colClasses);								
						} else {
							read.xlsx(filename, sheetIndex = s, colClasses=colClasses, stringsAsFactors = FALSE);
						}
					}, error = function(e){
						return(NULL)
					});	
					if (is.null(sheetdf)) break;
					#print(names(sheetdf))
					#print(names(filedf))
					filedf = rbind.fill(filedf, sheetdf)
				}
			} else if (grepl(".csv", f)[1]){
				head = tryCatch({
					read.csv(filename, nrows=1, stringsAsFactors = FALSE)
				}, error = function(e){
					return(NULL)
				});	
				if (is.null(head)) break;
				colClasses = getColClasses.read(names(head))
				filedf = tryCatch({
					read.csv(filename, colClasses=colClasses, stringsAsFactors = FALSE)
				}, error = function(e){
					return(NULL)
				});	
				if (is.null(filedf)) break;
			}
			if (!is.null(filedf)){
				#print(filedf)
				## Make sure we are using a standard Student.Work.Part.1 for first column of student work
				if (length(which(names(filedf) == "Student.Work")) == 1){
					names(filedf)[which(names(filedf) == "Student.Work")] = "Student.Work.Part.1";
				}
				# assume that any columns after last student work are more student work
				lindex = max(grep("Student.Work", names(filedf)))
				print(paste(f,is.null(filedf), lindex, ncol(filedf)))
				if (lindex < ncol(filedf)){
					for (li in (lindex+1):ncol(filedf)){
						names(filedf)[li] = paste("Student.Work.Part", length(grep("Student.Work", names(filedf)))+1,sep=".")
					}
				}

				countStudentWorkCols = length(grep("Student.Work", names(filedf)))
				## are there less student work columns than expected?
				if (countStudentWorkCols < Student.Work.Count){
					for (wi in (countStudentWorkCols+1):Student.Work.Count){
						filedf$TEMP = rep("",nrow(filedf))
						names(filedf)[which(names(filedf) == "TEMP")] = paste("Student.Work.Part", wi ,sep=".")
					}
				}

				if (length(df) == 0){
					# if this is the first sheet just set df 
					df = filedf
					maxStudentWorkCols = countStudentWorkCols;
				} else {
					# if this is 2-N sheet, must make sure that there are enough columns 
					if (countStudentWorkCols < maxStudentWorkCols){
						### add columns to sheet
						for (wi in (countStudentWorkCols+1):maxStudentWorkCols){
							filedf[paste('Student.Work.Part',wi,sep='.')] = rep("",nrow(filedf))
						}
					} else if (countStudentWorkCols > maxStudentWorkCols){
						### add columns to df
						for (wi in (maxStudentWorkCols+1):countStudentWorkCols){
							df[paste('Student.Work.Part',wi,sep='.')] = rep("",nrow(df))
						}
					}
					### combine
					df = rbind.fill(df, filedf);
				}			
			}
		}
	}
	# make sure there aren't any empty rows - i.e. those without a wise id
	df = subset(df, !is.nan(as.numeric(as.character(Wise.Id.1))))
	# create an index
	df = cbind(Index=1:nrow(df), df);
	# create a Step.Num column
	df = cbind(df, Step.Num = getStepNum(df));  
	df = cbind(df, Step.Num.NoBranch = collapseStepNumBranches(df$Step.Num))

	if (do.collapseMultipleRevisions) df = collapseMultipleRevisions(df)
	# Make sure all Step.Work.Id are valid
	df = df[!is.na(df$Step.Work.Id),];
	
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
	df = setColClasses.final(df);
	# replace NA in Student Work with blank
	indices = grep("Student.Work", names(df))
	if (length(indices) > 0){
		for (i in indices){
			df[is.na(df[,i]),i] = ""
		}
	}

	class(df) = c("wisedata.frame",class(df));
	return (df);
}

getRevisionNumber.bad <- function(df, inverse=FALSE, incrementIntegerOnChange=FALSE, betweenChangeIncrements=0.001){
	# place the index in the first column of the df so we can find them again
	if (is.null(df$Index))
	df = cbind(Index=1:nrow(df), df);
	Index = numeric();
	Rev.Num = numeric();
	## filter down to each step for each student in each run of each project
	stepIds = unique(df$Step.Work.Id);
	stepIds = stepIds[!is.na(stepIds)];
	for (sid in stepIds){
		df.step = subset(df,Step.Work.Id==sid);
		# single student's set of responses to a question
		Index = c(Index, df.step$Index);
		if (incrementIntegerOnChange && length(grep("Student.Work",names(df.step))) > 0){
			## between changes increments used when value of student work has not changed 
			swindices = grep("Student.Work",names(df.step));
			sw = do.call(paste, c(df.step[swindices],sep=""))
			usw = unique(sw);
			## remove non-answers from unique answers
			usw = usw[!is.na(usw)&!grepl("^ *$",usw)&usw!="N/A"&!grepl("Response #[0-9]*: \\[\\]",usw)]
			rev = numeric();
			running_index = 0; running_value = 0;
			for (val in sw){
				index = which(val == usw);
				if (length(index) > 0 && index != running_index){
					running_value = floor(running_value) + 1;
					running_index = index;
					rev = c(rev, running_value);
				} else {
					running_value = running_value + betweenChangeIncrements;
					if (floor(running_value) == 0) {
						rev = c(rev, running_value-betweenChangeIncrements);
					} else {
						rev = c(rev, running_value);
					}								
				}							
			}
		} else {
			rev = 1:length(df.step$Index);
		}
			
		if (inverse){
			if (incrementIntegerOnChange){
				### Note if the student doesn't do any work he or she will not get a 0.0
				# IURev.Num, instead will get -1.00, -1.001, etc.
				rev = max(c(1,floor(rev))) - rev;
				rev = -(rev + 2*(1.0 - (rev - floor(rev)))%%1);
				Rev.Num = c(Rev.Num, rev);
			} else {
				Rev.Num = c(Rev.Num, rev - max(rev));
			}
		} else {
			Rev.Num = c(Rev.Num, rev);
		}
		if (length(Rev.Num) != length(Index)){
			return(NULL);
		}
	}
	
	## combine Index and Rev.Num into a data.frame, order by index and return Rev.Num
	tdf = data.frame(Index, Rev.Num);
	tdf = tdf[order(tdf$Index),];
	return (tdf$Rev.Num)
}


