############### PACKAGES #######################################
# A package provides non-standard utilities, which will be needed for WISE analysis.
# To get a package go to "Packages" on the menu and follow instructions on "Install packages", it's easy.

## rJava is a tricky package because it needs access to a file called jvm.dll which is located in the Java Runtime Environment directory.
## More specifically, the folder containing jvm.dll must be part of the environment variable PATH.  This means that any program can
## "call" this program from any directory.  
## In Windows you would need to go into the Control Panel -> System -> Advanced system settings -> Environment Variables...
##   and add to the user variable 'Path' the path to the folder containing jvm.dll
##   when using 32 bit R it might be here:
##      C:\Program Files (x86)\Java\jre7\bin\client
##   when using 64 bit R it might be here:
##      C:\Program Files\Java\jre7\bin\server
##   For Mac OS X, you'll need to figure this out on your own.
##   You may want to do this before starting an R session.
options(java.parameters = "-Xmx1024m"); #increase Java's memory, necessary for large workbooks (many tabs)
library(rJava);
## These library is necessary for reading and writing Excel files.  
library(xlsx);

#################################### MAIN OPENING TOOLS #######################################
# These functions are primarilly used for opening and parsing data in preparation for analysis

## The newWISEDF function creates a data frame that contains all data from the given directory
## Since this is a "flat" data.frame, i.e., the different files and tabs are all stored in the
## same data.frame columns have been added to differentiate between runs, workgroups, etc.
read.xlsx.wiseDir = function (dir){
	files = list.files(dir);
	df = data.frame();
	# Iterate the first time through all the files to find the largest number of columns
	#  e.g. a questionarre could have multiple "student work" columns
	maxColumns = 0;
	sheetCounts = numeric();
	for (f in files){
		## don't include any files in temporary format
		if (substr(f,0,1) != "~"){
			dirf = paste(dir, f, sep="");
			sheetCount = countExcelSheets(dirf);
			sheetCounts = c(sheetCounts, sheetCount);
			for (s in 1:sheetCount){
				# get first row
				head = read.xlsx2(dirf, sheetIndex = s, startRow=1, endRow=1, stringsAsFactors = FALSE);
				pbody = read.xlsx2(dirf, sheetIndex = s, startRow=4, endRow = 4, stringsAsFactors = FALSE)
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
		colClassesHead = getColClasses(names(head));
		head = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=1, endRow=1, colClasses=colClassesHead, stringsAsFactors = FALSE);
		pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=4, endRow = 4, stringsAsFactors = FALSE)
		colClassesBody = getColClasses(names(pbody));
		pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow=4, endRow = 4, colClasses=colClassesBody, stringsAsFactors = FALSE)
		df = cbind(head, pbody);
	} else { return (df);}

	## now extract data
	findex = 0;
	for (f in files){
		findex = findex + 1;
		## don't include any files in temporary format
		if (substr(f,0,1) != "~"){
			dirf = paste(dir, f, sep="");
			sheetCount = sheetCounts[findex];
			for (s in 1:sheetCount){
				# get header information
				head = read.xlsx2(dirf, sheetIndex = s, startRow=1, endRow=2, colClasses=colClassesHead, stringsAsFactors = FALSE);
				# we validate that this is a WISE data file by looking for the Workgroup.Id in the top left 
				if (names(head)[1] == "Workgroup.Id"){
					body = read.xlsx2(dirf, sheetIndex = s, startRow=4, colClasses=colClassesBody, stringsAsFactors = FALSE)
					full = cbind(head[rep(seq_len(nrow(head)),each=nrow(body)),],body);
					## if full has less cols than df, populate with dummy data
					if (ncol(full) < ncol(df)){
						for (c in (ncol(full)+1):ncol(df)){
							full = cbind(full, dum=rep("",nrow(full)))
							names(full)[c] = names(df)[c]; 
						}
					}
					## replace Student.Work with Student.Work.Part.1
					if (length(which(names(full) == "Student.Work")) == 1)?{
						names(full)[which(names(full) == "Student.Work")] = "Student.Work.Part.1";
					}
					## look for a mismatch error between number of cols
					if (ncol(df) != 0 && ncol(full) != ncol(df)){print(names(full)); print(names(df)); print(f); print(s)}
					if (length(names(df)) != length(intersect(names(df),names(full)))){print(names(full)); print(names(df)); print(f); print(s)}
					df = rbind(df, full);
				}
			}
		}
	}
	# creat an index
	df = cbind(Index=1:nrow(df), df);

	# create a Step.Num column
	df = cbind(df, Step.Num = getStepNum(df));  

	## place revision numbers on this data frame.
	df = cbind(df, Rev.Num = getRevisionNumber(df,FALSE));
	df = cbind(df, IRev.Num = getRevisionNumber(df,TRUE));
	df = cbind(df, URev.Num = getRevisionNumber(df,FALSE, TRUE));

	## place a couple rows for researchers 
	df = cbind(df, Research.Score = rep(NA,nrow(df)));
	df = cbind(df, Research.Notes = rep("",nrow(df)));

	### clean up names a bit (remove .., . at end, "if applicable")
	names(df) = gsub("..if.applicable.", "", names(df));
	names(df) = gsub("\\.\\.", "\\.", names(df));
	names(df) = gsub("\\.$", "", names(df));
	class(df) = c("wisedata.frame",class(df));
	return (df);
}

read.csv.dupMap = function (filename){	
	df = read.csv(filename, header=TRUE);
	df$firstname = tolower(df$firstname)
	df$lastname = tolower(df$lastname)
	ndf = with(df,aggregate(df, by=list(firstname,lastname), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))
	iddf = with(df,aggregate(df, by=list(studentId), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))
	dupIds = iddf$studentId[!(tolower(iddf$studentId) %in% tolower(ndf$studentId))]	
	dupFirsts = tolower(iddf$firstname[!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	dupLasts = tolower(iddf$lastname[!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	replacements = numeric();
	## for each first,last combo find id in ndf
	for (i in 1:length(dupFirsts)){
		newId = subset(ndf, firstname==dupFirsts[i]&lastname==dupLasts[i])$studentId;
		if (length(newId) == 0) { print(paste(dupFirsts[i], dupLasts[i], "not found"))}
		else {
			if (length(newId) > 1) {print(paste(dupFirsts[i], dupLasts[i], "found more than once"))}
			newId = newId[1];			
		}
		
		replacements = c(replacements, newId)
	}
	dups = data.frame(from=dupIds, to=replacements)
	return (dups);
}

replaceDuplicateIds = function (wiseDF, dupMap){
	### make sure that the appropiate columns exist
	if (sum(names(wiseDF)=="Wise.Id.1"|names(wiseDF)=="Wise.Id.2"|names(wiseDF)=="Wise.Id.3") != 3){
		print("Are you sure this is a valid wise data object? Couldn't find Id columns");
		return (wiseDF);
	}
	# iterate through first column of duplicate match, look for duplicates and replace
	for (i in 1:nrow(dupMap)){
		d_id = dupMap[i,1]; 
		r_id = dupMap[i,2];
		wiseDF$Wise.Id.1[wiseDF$Wise.Id.1==d_id] = r_id;
		wiseDF$Wise.Id.2[wiseDF$Wise.Id.2==d_id] = r_id;
		wiseDF$Wise.Id.3[wiseDF$Wise.Id.3==d_id] = r_id;
	}
	return (wiseDF);
}

###
#	In the case where there are more than one files for a given student this function can be used to 
#	transfer the values in one file to the corresponding row in a target dataframe.
#
transferColValues = function (targetDF, sourceDF, targetColName="Student.Work.Part.1", sourceColName="Student.Work.Part.1"){
	for (r in 1:nrow(sourceDF)){
		srow = sourceDF[r,];
		## filter down to a single row of the targetDF (hopefully)
		tindices = with(targetDF, which(Workgroup.Id==srow$Workgroup.Id[1]&Wise.Id.1==srow$Wise.Id.1[1]&Project.Id==srow$Project.Id[1]&Step.Num==srow$Step.Num[1]&Start.Time==srow$Start.Time[1]&End.Time==srow$End.Time[1], arr.ind=TRUE));
		if (length(tindices) == 0){
			print("No corresponding row in target data frame");
		} else if (length(tindices)  > 1){
			print(paste(length(tindices), "matching rows found, using first"));
			print(targetDF[tindices,]);
			tindices = tindices[1];
		}
		if (length(tindices) == 1){
			targetDF[tindices[1],which(names(targetDF)==targetColName)] = srow[1,which(names(sourceDF)==sourceColName)];
		}
	}
	return (targetDF);
}



######################  Summary functions ############################################
## These functions are used to turn a complete, step-by-step data.frame into summaries


aggregate.wisedata.frame = function (x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE){
	df = x;
	class(df) = "data.frame";  # this way when we call aggregate it won't recursivley call this.
	# create a new data frame with just by factor
	first =  function(fdf){return(fdf[1])}
	by_call = substitute(by);
	b = deparse(by_call);
	b = sub("^ *list *\\( *","", b); b = sub(" *\\) *$", "", b);
	by_index = which(b == names(x));
	by = eval(by_call,x,parent.frame());
	odf = data.frame(aggregate(df, by, FUN = first, simplify=TRUE)[,c(1,by_index+1)]);
	#names(odf)[1] = deparse(substitute(b));
	### seb2lect.first are those items that we just want the top value
	if (!missing(select.first)){
		nl = as.list(seq_along(x));
		names(nl) = names(x);
		vars = eval(substitute(select.first), nl, parent.frame());
		df.select.first = df[,c(by_index,vars)];
		odf = aggregate(df.select.first, by, FUN = first, simplify=TRUE);
	}
	if (!missing(select.numerical)&& !missing(FUNS.numerical)){
		# we need to aggregate by step first
		nl = as.list(seq_along(x));
		names(nl) = names(x);
		vars = eval(substitute(select.numerical), nl, parent.frame());
		index.step = which("Step.Num" == names(x));
		index.wg = which("Workgroup.Id" == names(x));
		agg_indices = union(by_index, union(vars, union(index.step, index.wg)));
		df.agg = df[,agg_indices];
		agg = aggregate(df.agg, by = list(Workgroup.Id, Step.Num), FUN="max", na.rm=TRUE, simplify = TRUE);
		# there has to be a better way to do this (but apply seems to chnange to a matrix)
		print(class(agg))
		for (c in 1:ncol(agg)) {for(r in 1:nrow(agg)) if (is.infinite(agg[r, c])) agg[r,c] = NA; }  
		#agg = apply(agg, 2, function(x) {x[is.infinite(x)] <- NA; x}) 
		print(class(agg))
		#return(list(by_call, agg, parent.frame()))
		by = eval(by_call,agg,parent.frame());
		for (fun in FUNS.numerical){
			
			#df.select.numerical = agg[,c(by_index,vars)];
			adf = aggregate(agg, by, FUN = fun, na.rm = TRUE, simplify=TRUE);
			## if there is more than one numerical function attach the name of the funciton
			if (length(FUNS.numerical) > 1){ names(adf) = paste(names(adf),".",fun,sep="") }
			if (!missing(select.first) && ncol(odf) > 2){
				odf = cbind(odf, adf[,2:ncol(adf)]);
			} else {
				odf = adf;
			}
		}
		
	}
	# remove repeats of by variable
	reps = !grepl(b,names(odf)); #reps[1:length(by)] = FALSE; reps[length(by)+1] = TRUE;
	odf = odf[,reps];
	class(odf) = c("aggwisedata.frame", "data.frame");
	return(odf);
}
awiseDF = aggregate (wiseDF, by=list(Workgroup.Id), select.first = c(Project.Id, Run.Id, Wise.Id.1, Wise.Id.2), c(Teacher.Score, Research.Score), c("sum", "mean", "sd", "median", "min", "max")); #awiseDF[150:170,]

aggregate.aggwisedata.frame = function (x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE){
	df = x; class(df) = c("wisedata.frame", "data.frame");
	by = eval(substitute(by),x,parent.frame());
	aggregate(df,by, select.first, select.numerical, FUNS.numerical, simplify = TRUE);
}



######################  Subsetting functions ############################################
## These functions are used to take a data frame and reduce it to specific projects, wise Ids, runs, etc.

## Same functionality as data frame except a subset of Wise.Id will search for the
## given id in all three possible slots.
subset.wisedata.frame = function(x, subset, select, drop = FALSE, ...){
	if (missing(subset)) 
	       r <- TRUE
	else {
	   e <- substitute(subset)
	   # in case of Call Wise.Id (no .1, .2, etc) perform match against all three
	   if (as.character(e)[2] == "Wise.Id"){
	   	  id = as.numeric(as.character(e)[3]);
	   	  c = call("|", call("|", call("==",quote(Wise.Id.1),id), call("==",quote(Wise.Id.2),id)),call("==",quote(Wise.Id.3),id))
	   	  e = substitute(c);
	   }
	   r <- eval(e, x, parent.frame())
	   if (!is.logical(r)) 
	      stop("'subset' must evaluate to logical")
	   r <- r & !is.na(r)
	 }
	if (missing(select)) 
	    vars <- TRUE
	else {
	    nl <- as.list(seq_along(x))
	    names(nl) <- names(x)
	    vars <- eval(substitute(select), nl, parent.frame())
	 }
	x[r, vars, drop = drop]
}

############### PRIVATE FUNCTIONS ##############################
# These are mainly used by the functions above, you can use if you'd like

## Counts the number of sheets, or tabs, in an excel file
countExcelSheets = function(fileName)
{
	wb = loadWorkbook(fileName);
	sheets = getSheets(wb);
	return (length(sheets));
}

## A second implementation that uses error checking
## This is a SLOW brute force method, but requires less memory from java
countExcelSheets2 = function(fileName, maxSheets=200){
	count = 0;
	for (i in 1:maxSheets){
		completed = TRUE;
		tryCatch({df=read.xlsx2(fileName,i,startRow=1,endRow=1); completed=FALSE}, 
				error = function(err){}, 
				finally={if (!completed) {count = count + 1;}}
		)	
	}
	return (count);
}
## Gets sheet indices for this excel file
getSheetNames = function(fileName, maxSheets=100)
{
	wb = loadWorkbook(fileName);
	sheets = getSheets(wb);
	return (names(sheets));
}

### Looks for repeat of rows (same project/run/workgroup id) and makes a count
##  of each revision
getRevisionNumber = function(df, inverse=FALSE, incrementIntegerOnChange=FALSE, betweenChangeIncrements=0.001){
	# place the index in the first column of the df so we can find them again
	df = cbind(Index=1:nrow(df), df);
	Index = numeric();
	Rev.Num = numeric();
	## filter down to each step for each student in each run of each project
	projectIds = unique(df$Project.Id);
	for (pid in projectIds){
		df.p = subset(df, Project.Id==pid);
		runIds = unique(df.p$Run.Id);
		for (rid in runIds){
			df.p.r = subset(df.p, Run.Id==rid);
			wgIds = unique(df.p.r$Workgroup.Id);
			for (wgid in wgIds){
				df.p.r.wg = subset(df.p.r, Workgroup.Id==wgid);
				stepTitles = unique(df.p.r.wg$Step.Title);
				for (st in stepTitles){
					df.p.r.wg.st = subset(df.p.r.wg,Step.Title==st);
					# after all that we finally have a single student's set of responses to a question
					Index = c(Index, df.p.r.wg.st$Index);
					if (incrementIntegerOnChange){
						## between changes increments used when value of student work has not changed 
						swindices = grep("Student.Work",names(df.p.r.wg.st));
						sw = do.call(paste, c(df.p.r.wg.st[swindices],sep=""))
						usw = unique(sw);
						## remove non-answers from unique answers
						usw = usw[!is.na(usw)&!grepl("^ *$",usw)&usw!="N/A"&!grepl("Response #[0-9]*: \\[\\]",usw)]
						running_index = 0; running_value = 0;
						rev = numeric();
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
						rev = 1:length(df.p.r.wg.st$Index);
					}
					
					if (inverse){
						Rev.Num = c(Rev.Num, rev - max(rev));
					} else {
						Rev.Num = c(Rev.Num, rev);
					}
					
				}
			}
		}
	}
	## combine Index and Rev.Num into a data.frame, order by index and return Rev.Num
	tdf = data.frame(Index, Rev.Num);
	tdf = tdf[order(tdf$Index),];
	return (tdf$Rev.Num)
}

## Uses regular expression matching to find the step number in the front of a step title
getStepNum = function (df){
	stepTitles = as.character(df$Step.Title);
	matchNum = regexpr("[0-9]+\\.?[0-9]*", stepTitles);
	# if any non-matches replace stepTitles with "0" and do again
	stepTitles[matchNum==-1] = "0"
	matchNum = regexpr("[0-9]+\\.?[0-9]*", stepTitles);	
	stepNums = as.numeric(regmatches(stepTitles,matchNum));
	return (stepNums);
}



### Goes through a series of column names and converts to a column class.
getColClasses = function (colNames){
	### clean up names a bit (remove .., . at end, "if applicable")
	colNames= gsub("..if.applicable.", "", colNames);
	colNames = gsub("\\.\\.", "\\.", colNames);
	colNames = gsub("\\.$", "", colNames);

	colClasses = character();
	for (colName in colNames)
	{
		if (colName == "Workgroup.Id" ||
			colName ==  "Wise.Id.1" ||
			colName ==  "Wise.Id.2" ||
			colName ==  "Wise.Id.3" ||
			colName ==  "Class.Period" ||
			colName ==  "Project.Id" ||
			colName ==  "Parent.Project.Id" ||
			colName ==  "Run.Id" ||
			colName ==  "Time.Spent.Seconds" ||
			colName ==  "Teacher.Score" ||
			colName == "Rev.Num" ||
			colName == "IRev.Num" ||
			colName == "Step.Num"
		){
			colClasses = c(colClasses, "numeric");
		} else {
			colClasses = c(colClasses, "character");
		}
	}
	return (colClasses);
}