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
#options(java.parameters = "-Xmx1024m"); #increase Java's memory, necessary for large workbooks (many tabs)
#library(rJava);
## These library is necessary for reading and writing Excel files.  
#library(xlsx);
#library(plyr)

#################################### MAIN OPENING TOOLS #######################################
# These functions are primarilly used for opening and parsing data in preparation for analysis

rbind.wisedata.frame <- function(obj1, obj2){
	library(plyr)
	obj = rbind.fill (obj1, obj2)
	# reindex
	obj$Index = 1:nrow(obj)
	class(obj) = class(obj1)
	return(obj)
}

## The read.xlsx.wiseDir function creates a data frame that contains all data from the given directory
## Since this is a "flat" data.frame, i.e., the different files and tabs are all stored in the same data.frame 
## columns have been added to differentiate between runs, workgroups, etc.
### assumes that the column headers are the same across tabs and files.
### Put names of columns into "character.columns" if you want them interpeted as characters (not numeric)
read.xlsx.wiseDir <- function(dir, sheetIndices = 1:1000, fileIndices = NULL, high.memory = FALSE, character.columns=character()){
	library(plyr)
	library(xlsx)
	filenames <- list.files(dir);
	# rm open files
	filenames <- filenames [!grepl("~",filenames)]
	if (!is.null(fileIndices)) filenames  <- filenames[fileIndices]
	filenames <- paste(dir,filenames,sep="")
	
	# get the column classes
	head <-	read.xlsx2(filenames[1], sheetIndex = sheetIndices[1], startRow=1, endRow=2, stringsAsFactors = FALSE)
	cnames <- names(head)
	if (length(character.columns)>0) cnames[cnames%in%character.columns] <- "OTHER"
	colClasses <- getColClasses.read(cnames);
	### add extra characters at end
	colClasses <- c(colClasses, rep("character",20))
	# count number of sheets, but have to load first, which is annoying
	scounts <- data.frame(filenames=filenames,sheet.count=do.call("c", lapply(1:length(filenames), function(x){wb <- loadWorkbook(filenames[x]); return(length(getSheets(wb)))})), stringsAsFactors=FALSE)
	scounts.mat <- do.call("rbind", apply(scounts, 1, function(row){return(data.frame(filename=row[1],sheet=1:row[2], row.names=NULL, stringsAsFactors=FALSE))}))
	if (high.memory) df <- do.call("rbind.fill", mapply(function(f,x){return(read.xlsx2(f, sheetIndex = x, colClasses=colClasses, stringsAsFactors = FALSE))},scounts.mat[,1],scounts.mat[,2]))
	else df <- do.call("rbind.fill", mapply(function(f,x){return(read.xlsx(f, sheetIndex = x, colClasses=colClasses, stringsAsFactors = FALSE))},scounts.mat[,1],scounts.mat[,2]))
	
	# replace imported NA with ""
	df[,apply(df,2,class)=="character"] <-  as.data.frame(apply(df[,apply(df,2,class)=="character"],2,function(x){x[is.na(x)]<-"";return(x)}),stringsAsFactors=FALSE)

	# Make all Student work and NA columns into Student.Work.Part.X
	swindices <- grep("Student\\.Work|NA\\.", names(df))
	names(df)[swindices] <- paste("Student.Work.Part",1:length(swindices),sep=".") 

	# make sure there aren't any empty rows - i.e. those without a wise id
	df <- subset(df, !is.nan(as.numeric(as.character(Wise.Id.1))))
	# NA in student work should just be blank
	for(swi in swindices) df[is.na(df[,swi]),swi] <- ""
	for(swi in swindices) df[grepl("^ +$",df[,swi]),swi] <- ""

	### clean up names a bit (remove .., . at end, "if applicable")
	names(df) <- gsub("..if.applicable.", "", names(df));
	names(df) <- gsub("\\.\\.", "\\.", names(df));
	names(df) <- gsub("\\.$", "", names(df));

	# one row per step work id
	df <- collapseMultipleRevisions(df)
	df <- subset(df, !is.na(Workgroup.Id))
	# create an index
	df$Index <- 1:nrow(df)
	## in new versions the visit and revision time spent are distinguished, create another column for historical purposes
	df$Time.Spent.Seconds <- df$Visit.Time.Spent.Seconds
	
	# create a Step.Num column
	df$Step.Title[is.na(df$Step.Title)] <- "0 Unknown"
	df$Step.Num <- getStepNum(df)
	df$Step.Num.NoBranch <- collapseStepNumBranches(df$Step.Num)

	## place revision numbers on this data frame
	df$Rev.Num <- getRevisionNumber(df,FALSE)
	df$IRev.Num <- getRevisionNumber(df,TRUE)
	df$URev.Num <- getRevisionNumber(df,FALSE, TRUE)
	df$IURev.Num <- getRevisionNumber(df,TRUE, TRUE)

	## place a couple rows for researchers 
	df$Research.Score = NA
	df$Research.FeedbackGiven = ""
	df$Research.Notes = ""

	### get final classes and update
	cnames <- names(df)
	if (length(character.columns)>0) cnames[cnames%in%character.columns] <- "OTHER"
	df <- setColClasses.final(df, cnames);
	df <- subset(df, !is.na(as.numeric(as.character(Wise.Id.1))))
	class(df) = c("wisedata.frame",class(df));
	return(df)
}

### assumes that the column headers are the same.
### Put names of columns into "character.columns" if you want them interpeted as characters (not numeric)
read.xlsx.wise <- function(filename, sheetIndices = 1:1, sheetName=NULL, character.columns=character()){
	library(plyr)
	if (!is.null(sheetName)){
		head <- read.xlsx(filename, sheetName = sheetName[1], startRow=1, endRow=2, stringsAsFactors = FALSE);
	} else {
		head <-	read.xlsx(filename, sheetIndex = sheetIndices[1], startRow=1, endRow=2, stringsAsFactors = FALSE);
	}
	cnames <- names(head)
	if (length(character.columns)>0) cnames[cnames%in%character.columns] <- "OTHER"
	colClasses <- getColClasses.read(cnames);

	if (!is.null(sheetName)){
		df <- do.call("rbind.fill", sapply(sheetName, function(x){return(read.xlsx(filename, sheetName = x, colClasses=colClasses, stringsAsFactors = FALSE))}))
	} else{
		df <- do.call("rbind.fill", lapply(sheetIndices, function(x){return(read.xlsx(filename, sheetIndex = x, colClasses=colClasses, stringsAsFactors = FALSE))}))
	}
	# replace imported NA with ""
	df[,apply(df,2,class)=="character"] <-  as.data.frame(apply(df[,apply(df,2,class)=="character"],2,function(x){x[is.na(x)]<-"";return(x)}),stringsAsFactors=FALSE)

	swindices <- grep("Student\\.Work|NA\\.", names(df))
	names(df)[swindices] <- paste("Student.Work.Part",1:length(swindices),sep=".") 
	# make sure there aren't any weird columns (with an NA)
	df <- df[,!grepl("NA\\.",names(df))]

	# make sure there aren't any empty rows - i.e. those without a wise id
	df <- subset(df, !is.nan(as.numeric(as.character(Wise.Id.1))))
	# create an index
	df <- cbind(Index=1:nrow(df), df);
	### clean up names a bit (remove .., . at end, "if applicable")
	names(df) <- gsub("..if.applicable.", "", names(df));
	names(df) <- gsub("\\.\\.", "\\.", names(df));
	names(df) <- gsub("\\.$", "", names(df));
	# one row per step work id
	#print(subset(df,Step.Work.Id==7943080))

	df <- collapseMultipleRevisions(df)
	# create a Step.Num column
	df$Step.Num <- getStepNum(df)
	df$Step.Num.NoBranch <- collapseStepNumBranches(df$Step.Num)
	## create a Step.Num column
	df$Step.Num <- getStepNum(df)
	df$Step.Num.NoBranch <- collapseStepNumBranches(df$Step.Num)
	# place revision numbers on this data frame
	df$Rev.Num <- getRevisionNumber(df,FALSE)
	df$IRev.Num <- getRevisionNumber(df,TRUE)
	df$URev.Num <- getRevisionNumber(df,FALSE, TRUE)
	df$IURev.Num <- getRevisionNumber(df,TRUE, TRUE)

	## place a couple rows for researchers 
	df$Research.Score = NA
	df$Research.FeedbackGiven = ""
	df$Research.Notes = ""

	### get final classes and update
	cnames <- names(df)
	if (length(character.columns)>0) cnames[cnames%in%character.columns] <- "OTHER"
	df <- setColClasses.final(df, cnames);
	df <- subset(df, !is.na(as.numeric(as.character(Wise.Id.1))))
	class(df) = c("wisedata.frame",class(df));
	return(df)
}
#test <- read.xlsx.wise("C:\\Users\\Jonathan Vitale\\Documents\\Data\\WISE\\GraphingStories\\fall2014\\Excel-test\\TestSheet.xlsx")
#print(subset(test,Step.Work.Id==7943080))

#wise = read.xlsx.wiseDir(excelDirectory, Student.Work.Count = 7,perl="C:\\perl64\\bin\\perl.exe", DEBUG=TRUE);
#wise = read.xlsx.wiseDir(excelDirectory, Student.Work.Count = 4, Parent.Project.Id.pretest=Parent.Project.Id.pretest); 

### In the current version of the downloader multiple responses in the single step are 
### separated into multiple rows.  This will place each revision in a separate Response #:{}
collapseMultipleRevisions <- function (df, include.Visit.Revision.Count = TRUE, response.number.columns=c("Student\\.Work","Revision\\.Time", "Teacher\\.Score\\.Timestamp", "Teacher\\.Score", "Teacher.Comment\\.Timestamp", "Teacher\\.Comment", "Auto\\.Score", "Auto\\.Feedback")){
	#re-affirm integrity of Index
	df$Index = 1:nrow(df)
	swindices <- numeric()
	for (rnc in response.number.columns){
		swindices <- union(swindices, grep(rnc, names(df)))
	}
	# get the first index of actual student work
	swindex <- grep("Student\\.Work", names(df))
	print(swindex)
	df.out <- df
	df.out <- subset(df, !duplicated(Step.Work.Id, fromLast=TRUE))
	# empty student work out
	df.out[,swindices] <- ""
	if(include.Visit.Revision.Count) df.out$Visit.Revision.Count <- 1
	
	### loop through all number of revisions
	rev.num <- 1
	df.in <- df
	dups <- duplicated(df$Step.Work.Id)
	repeat {
		df.rev <- subset(df.in, !dups)
		# place "Response #X: {...} ", if it isn't 
		for (swi in swindices){
			# only take rows with some student work
			if (length(swindex) > 1){
				df.rev.u <- subset(df.rev,apply(df.rev[,swindex],1,function(x)sum(nchar(x)))>0&!grepl("Response #",df.rev[,swindex[1]]))
			} else {
				df.rev.u <- subset(df.rev,nchar(df.rev[,swindex])>0&!is.na(df.rev[,swindex])&!grepl("Response #",df.rev[,swindex]))	
			}
			m <- match(df.rev$Index,df.rev.u$Index)
			df.rev.u2 <- df.rev
			df.rev.u2[!is.na(m), swi] <- paste("Response #", rev.num, ": {", df.rev.u[m[!is.na(m)],swi], "}",sep="")
			# now place in out
			if (length(swindex) > 1){
				df.rev.u2 <- subset(df.rev.u2, apply(df.rev.u2[,swindex],1,function(x)sum(nchar(x)))>0)
			} else {	
				df.rev.u2 <- subset(df.rev.u2, nchar(df.rev.u2[,swindex])>0&!is.na(df.rev.u2[,swindex]))			
			}
			m <- match(df.out$Step.Work.Id,df.rev.u2$Step.Work.Id)
			if (sum(!is.na(m)) > 0){
				df.out[!is.na(m),swi] <- paste(df.out[!is.na(m),swi],df.rev.u2[m[!is.na(m)],swi])
				if(include.Visit.Revision.Count) df.out[!is.na(m),]$Visit.Revision.Count <- rev.num
			}
		}

		rev.num <- rev.num + 1
		dups <- duplicated(df.in$Step.Work.Id)
		if (sum(dups, na.rm=TRUE) == 0 || rev.num > 100) break
		df.in <- subset(df.in, dups)
		dups <- duplicated(df.in$Step.Work.Id)
	}
	return(df.out)
}

### Update multiple responses will insert the format of Response # X: {} around all appropriate columns to match the pattern in Student.Work.Part.1
updateMultipleResponses <- function (df, regexp.split=" *Response #[0-9]+: "){
	if (length(which(class(df)=="wisedata.frame")) == 0){print("You need to use a valid wise data frame."); return (NULL)}
	# get just those columns that have the Response # indicator
	indices <- as.numeric(which(apply(df, 2, function(c){return(length(grep(regexp.split,c))>0)})))
	#print(names(df[,indices]))
	if (length(indices) > 0){
		#grep("Student\\.Work",names(df))
		# make sure that the number of responses in student work part 1 matches # of responses in other columsn
		nresponses.1 <- sapply(strsplit(df$Student.Work.Part.1, regexp.split),function(x)return(max(length(x)-1,0)))
		work <- sapply(df[,indices], as.character)
		nresponses <- apply(work,c(1,2),function(x){y<-strsplit(x, regexp.split)[[1]];return(max(0,length(y)-1))})
		nresponses.toadd <- nresponses.1 - nresponses
		for (ci in 1:ncol(work)){
			work.col <- work[nresponses.toadd[,ci]>0,ci]
			nresponses.toadd.col <- nresponses.toadd[nresponses.toadd[,ci]>0,ci]
			nresponses.toadd.list <- lapply(nresponses.toadd.col, function(n)seq(n))
			newcol <- sapply(seq_along(work.col), function(i){
				y<-paste(" Response #",nresponses.toadd.list[[i]],work.col[i],sep="",collapse=paste(": {",work.col[i],"}",sep=""));
				y<-paste(y,": {",work.col[i],"} ",sep="");
				return(y)
			})
			# print(paste(length(newcol),ci,newcol))
			if (length(newcol)>0) work[nresponses.toadd[,ci]>0,ci] <- newcol
		}
		df[,indices] <- work
		return (df);
	}
	return (NULL)
}
#wise2 <- updateMultipleResponses(wise)

## Given a wisedata.frame will iterate through all rows looking for multiple student responses that are concatatenated into a single column
## a row will be repeated for each response
## regexp.split is the regular expression used to split responses
expandMultipleResponses <- function (df, regexp.split=" *Response #[0-9]+: "){
	if (length(which(class(df)=="wisedata.frame")) == 0){print("You need to use a valid wise data frame."); return (NULL)}
	# get just those columns that have the Response # indicator
	indices <- as.numeric(which(apply(df, 2, function(c){return(length(grep(regexp.split,c))>0)})))
	print(names(df[,indices]))
	if (length(indices) > 0){
		if (nrow(subset(df,grepl("Response #1:",Student.Work.Part.1))) != nrow(subset(df,grepl("Response #1:",Student.Work.Part.2)))){
			df <- updateMultipleResponses(df, regexp.split = regexp.split)
		}
		# make sure every student work part 1 has some value
		max.rev <- 1
		Student.Work.expanded <- do.call("cbind", lapply(indices,function(i){
			#print(names(df)[i])
			df[!is.na(df[,i])&df[,i]=="",i] <- " "; 
			u <- unlist(strsplit(df[,i],regexp.split)); 
			u <- u[nchar(u)>0]; 
			u[u==" "] <- "";
			u <- gsub("^ *\\{","",u)
			u <- gsub("\\} *$","",u)
			if(length(u)>max.rev) max.rev <<- length(u); 
			if(length(u)<max.rev) u <-c(u,rep("",max.rev-length(u))); 
			#print(length(u))
			return(u)
		}))
		# which rows were expanded
		expanded <- unlist(lapply(strsplit(df[,indices[1]],regexp.split),function(l)return(length(l))))
		expanded[expanded > 1] <- expanded[expanded > 1] - 1
		expanded[expanded == 0] <- 1
		# expand df by repeating rows
		returndf <- df[rep(seq_len(nrow(df)),expanded),]
		# convert columns if actually numeric
		Student.Work.expanded.num <- apply (Student.Work.expanded, 2, function(x){
			if (length(suppressWarnings(as.numeric(unique(x))))+1 >= length(unique(x))){
				return (suppressWarnings(as.numeric(x)))
			} else {
				return (x)
			}
		})
		# substitue expanded student work for repeated student work
		print(apply(Student.Work.expanded.num, 2, class))
		returndf[,indices] <- Student.Work.expanded.num
		
		return (returndf)
	} else {
		return (df)
	}
}
wise.exp <- expandMultipleResponses(wise)

# expand each workgroup into the number of participants,
# If one Wise User, then row remains the same
# If two Wise users, then row one remains the same, row two switches placement of wiseUsers
# If three Wise users, then row one remains th same, row two shifts back one place, row three shifts back two places 
expandWorkgroupToWiseId <- function (df){
	class.out <- class(df)
	df.out = df
	# just swap 1 and 2
	df12 = subset(df,!is.na(as.numeric(as.character(Wise.Id.2)))&is.na(as.numeric(as.character(Wise.Id.3))))
	if (nrow(df12)>0){
		temp = df12$Wise.Id.1
		df12$Wise.Id.1 = df12$Wise.Id.2
		df12$Wise.Id.2 = temp
		df.out = rbind(df.out, df12)
	}

	## if we have a third wise id, do two rotations, so each can be wise.id.1
	df123 = subset(df,!is.na(as.numeric(as.character(Wise.Id.2)))&!is.na(as.numeric(as.character(Wise.Id.3))))
	if (nrow(df123)>0){ 
		temp = df123$Wise.Id.1
		df123$Wise.Id.1 = df123$Wise.Id.2
		df123$Wise.Id.2 = df123$Wise.Id.3
		df123$Wise.Id.3 = temp
		df.out = rbind(df.out, df123)
		
		df123 = subset(df,!is.na(as.numeric(as.character(Wise.Id.2)))&!is.na(as.numeric(as.character(Wise.Id.3))))
		temp = df123$Wise.Id.3
		df123$Wise.Id.3 = df123$Wise.Id.2
		df123$Wise.Id.2 = df123$Wise.Id.1
		df123$Wise.Id.1 = temp	
		df.out = rbind(df.out, df123)
	}
	class(df.out) <- class.out
	return(df.out)
}

refactor <- function(df, colNames=NULL){
	if (is.null(colNames)){
		colNames = names(df)
	}
	for (i in which(names(df)%in%colNames)){
		if (is.factor(df[,i])){
			df[,i] = factor(df[,i])
		}
	}
	return (df)
}

### import file to gather column values
readForColValues <- function (targetDF, sourceFile, sourceColNames.except = NULL, sourceColNames = NULL, targetIdentifier="Step.Work.Id", sourceIdentifier="Step.Work.Id", includes.formulas = TRUE, ...){
	if (includes.formulas) {
		sourceDF = read.xlsx(sourceFile, stringsAsFactors=FALSE, keepFormulas=FALSE, ...)
	} else {
		sourceDF = read.xlsx2(sourceFile, stringsAsFactors=FALSE,...)
	}
	if (!is.null(sourceColNames.except) || is.null(sourceColNames)){
		sourceColNames = names(sourceDF)[!(names(sourceDF) %in% sourceColNames.except)]
	}
	## make sure step work id is in the correct format
	sourceDF[,sourceIdentifier] = as.factor(as.numeric(as.character(sourceDF[,sourceIdentifier])))
	#print(unique(sourceDF[,sourceIdentifier]))

	#print(unique(subset(targetDF, (targetDF[,targetIdentifier] %in% sourceDF[,sourceIdentifier]))$Step.Id)) 
	df.out = transferColValues(targetDF=targetDF, sourceDF=sourceDF, targetColNames=sourceColNames, sourceColNames=sourceColNames, targetIdentifier=targetIdentifier, sourceIdentifier=sourceIdentifier)
	return (df.out)
}

#### reads "prepost" files
#### if sourceColNames.except is not NULL (or sourceColNames is NULL) will read all columns except ones specified
#### the target column names will be the same as sourceColNames (minus strip.postfixes)
#### strip.postfixes is a character 
readForColValues.prepost <- function (targetDF, sourceFile, sourceColNames.except = NULL, sourceColNames = NULL, postfixes = c("\\.Pretest","\\.Posttest"), targetIdentifier="Step.Work.Id", sourceIdentifier="Step.Work.Id", includes.formulas = TRUE, ...){
	if (includes.formulas) {
		sourceDF = read.xlsx(sourceFile, stringsAsFactors=FALSE, keepFormulas=FALSE, ...)
	} else {
		sourceDF = read.xlsx2(sourceFile, stringsAsFactors=FALSE, ...)
	}
	if (!is.null(sourceColNames.except) || is.null(sourceColNames)){
		sourceColNames = names(sourceDF)[!(names(sourceDF) %in% sourceColNames.except)]
	}
	## make sure step work id is in the correct format
	#sourceDF[,sourceIdentifier] = as.factor(as.numeric(as.character(sourceDF[,sourceIdentifier])))
	#print(unique(sourceDF[,sourceIdentifier]))
	#print(unique(subset(targetDF, (targetDF[,targetIdentifier] %in% sourceDF[,sourceIdentifier]))$Step.Id))
	for (postfix in postfixes){
		sourceColNames.postfix = grep(postfix,sourceColNames,value=TRUE)
		sourceColNames.postfix.removed = sub(postfix,"",sourceColNames.postfix)
		sourceIdentifier.postfix = paste(sourceIdentifier,gsub("\\\\","",postfix),sep="")
		#print(sourceColNames.postfix)
		#print(sourceColNames.postfix.removed)
		targetDF = transferColValues(targetDF=targetDF, sourceDF=sourceDF, targetColNames=sourceColNames.postfix.removed, sourceColNames=sourceColNames.postfix, targetIdentifier=targetIdentifier, sourceIdentifier=sourceIdentifier.postfix)
	} 
	return (targetDF)
}

###
#	In the case where there are more than one files for a given student this function can be used to 
#	transfer the values in one file to the corresponding row in a target dataframe.
#
transferColValues <- function (targetDF, sourceDF, targetColNames="Student.Work.Part.1", sourceColNames="Student.Work", targetIdentifier="Step.Work.Id", sourceIdentifier="Step.Work.Id"){
	if(nrow(targetDF)!=length(unique(targetDF$Index))) stop("Re-index the target data frame")
	sindices <- sourceDF[,sourceIdentifier] %in% targetDF[,targetIdentifier]
	# eliminate any rows of source that don't have a match in target
	sourceDF <- sourceDF[sindices,]
	tindices <- targetDF[,targetIdentifier] %in% sourceDF[,sourceIdentifier]
	sIdentity <- as.character(sourceDF[,sourceIdentifier])
	tIdentity <- as.character(targetDF[tindices,targetIdentifier])
	
	# Iterate through
	for (s in 1:length(sourceColNames)){
		sourceColName <- sourceColNames[s]
		targetColName <- targetColNames[s]
		
		# add missing columns
		if (sum(names(targetDF)==targetColName) == 0){
			print(paste("Adding:", targetColName))
			### no, add it
			sclass = class(sourceDF[,sourceColName])
			if (sclass[1] == "numeric" || sclass[1] == "integer"){
				targetDF[,targetColName] = rep(NA, nrow(targetDF))	
			} else {
				targetDF[,targetColName] = rep("", nrow(targetDF))	
			}
		} 
		#if (targetColName == "KI.Score") print(cbind(targetDF[tindices,c(targetIdentifier,targetColName)], sourceDF[match(as.character(tIdentity),sIdentity),c(sourceIdentifier, sourceColName)]))
		# replace only those whose identity in source can be found in target, careful of ordering!
		targetDF[tindices,targetColName] <-sourceDF[match(as.character(tIdentity),sIdentity),sourceColName]
	}
	return (targetDF);
}
wise <- readForColValues.prepost (wise, paste(dir.scored,"GraphingStories-embedded-Fall2014-jv.xlsx",sep=""), postfixes = c("\\.Initial","\\.Final"), sourceColNames.except = names(wise.vijay.explain), sheetIndex=1)

### FEATURE FUNCTIONS - intended to add new columns to wise data frame providing more information:

# In a wise data frame all steps should have Ids.
# Ids will have prefixes that designate what part of the study they encompass (e.g. "Pretest", "Postest", "Unit")
# Each Prefix is represented by three arguments, which must be put in this order
# Prefix - the label assigned to the Step.Id
# Parent.Project.Ids - the Parent Project Id of projects associated with this label
# Activity.Nums - The Activity numbers (i.e., the whole number in front of Step.Num) associated with the label.
# Example: assignStepId(df, "Pretest.Act1", Parent.Project.Ids = c(1111,1112), Activity.Nums = 1); 
assignStepId <- function (df, prefix, Parent.Project.Ids, Project.Ids, Run.Ids, Activity.Nums = 1:100){
	if (missing(df) || sum(names(df) == "Step.Title") == 0) stop("Not a valid data frame with a Step.Title column.")
	#if (sum(names(df) == "Step.Id") == 0) 
	if (is.null(df$Step.Id)){ 
		df$Step.Id = gsub("\\.\\-\\.",".",gsub(" +", ".", sub("^[0-9]+.?[0-9]*.?[0-9]* ?","",df[, "Step.Title"])))
	} else {
		# must convert to non-factor, then reconvert later
		df$Step.Id = as.character(df$Step.Id)
	}
	if (!missing(Parent.Project.Ids)){
		if (sum(paste("Is",prefix,sep=".") %in% names(df)) == 0) df[,paste("Is",prefix,sep=".")] = FALSE
		df[(df$Parent.Project.Id %in% Parent.Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Id"] = paste(prefix, gsub("\\.\\-\\.",".",gsub(" +", ".", sub("^[0-9]+.?[0-9]*.?[0-9]* ?","",df[(df$Parent.Project.Id %in% Parent.Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Title"]))),sep=".")
		df[(df$Parent.Project.Id %in% Parent.Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), paste("Is",prefix,sep=".")] = TRUE
		df$Step.Id = as.factor(df$Step.Id)
	} else if (!missing(Project.Ids)){
		if (sum(paste("Is",prefix,sep=".") %in% names(df)) == 0) df[,paste("Is",prefix,sep=".")] = FALSE
		df[(df$Project.Id %in% Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Id"] = paste(prefix, gsub("\\.\\-\\.",".",gsub(" +", ".", sub("^[0-9]+.?[0-9]*.?[0-9]* ?","",df[(df$Project.Id %in% Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Title"]))),sep=".")
		df[(df$Project.Id %in% Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), paste("Is",prefix,sep=".")] = TRUE
		df$Step.Id = as.factor(df$Step.Id)
	} else if (!missing(Run.Ids)){
		if (sum(paste("Is",prefix,sep=".") %in% names(df)) == 0) df[,paste("Is",prefix,sep=".")] = FALSE
		df[(df$Run.Id %in% Run.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Id"] = paste(prefix, gsub("\\.\\-\\.",".",gsub(" +", ".", sub("^[0-9]+.?[0-9]*.?[0-9]* ?","",df[(df$Run.Id %in% Run.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Title"]))),sep=".")
		df[(df$Run.Id %in% Run.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), paste("Is",prefix,sep=".")] = TRUE
		df$Step.Id = as.factor(df$Step.Id)
	} 
	df$Step.Id = as.factor(gsub("(\\.|\\?|!)$","",as.character(df$Step.Id)))
	df$Step.Id = as.factor(gsub(":\\.|-",".",as.character(df$Step.Id)))
	df$Step.Id = as.factor(gsub(",","",as.character(df$Step.Id)))
	df$Step.Id = as.factor(gsub("'","",as.character(df$Step.Id)))
	df$Step.Id = as.factor(gsub("\\(|\\)","",as.character(df$Step.Id)))
	return (df)
}

##  Applies a Condition to wise data frame based on one of two methods (for now)
##  If method is "Parent.Project.Id" then
###    Parent.Project.Id should include more than one id number and Parent.Project.Condition should be of same length
###    If a Wise.User.Id can
###    be found in any rows of a given Parent.Project.Id, will receive a condition associated
###    with the index of that ID in the Parent.Project.Id vector within Parent.Project.Condition
## If method is "Workgroup.Id.mod" then
###    Condition is assigned by each Wise.User's mod of his or her Workgroup Id in the Parent Project
### Note if multiple Wise User Ids are in projects that are not the main projects (pointed to be Parent.Project.Id, e.g. pre post test), will look for first only and print an error
applyCondition <- function (df, method="Parent.Project.Id", Parent.Project.Ids, Parent.Project.Conditions = seq(1:length(Parent.Project.Ids)), Workgroup.Id.mod=1, as.data.frame.out=TRUE, VERBOSE=TRUE){
	if (length(which(class(df)=="wisedata.frame")) == 0){print("You need to use a valid wise data frame."); return (NULL)}
	df.pp = subset(df, Parent.Project.Id %in% Parent.Project.Ids);
	Condition = numeric();
	error1 = "";
	error2 = "";
	if (method=="Parent.Project.Id" && length(Parent.Project.Ids) == length(Parent.Project.Conditions)){
		### TODO - improve this with merge
		#Condition = rep(NA, nrow(df))
		#for (ppid in Parent.Project.Ids){
		#	dfc = subset(df, Parent.Project.Id==ppid)
		#		dfc = expandWorkgroupToWiseId(dfc)
		#	df = merge(df, , all.x=TRUE, all.y=TRUE, suffixes=c())
		#}

		for (i in 1:nrow(df)){
			row = df[i,];
			if (length(which(Parent.Project.Ids == row$Parent.Project.Id[1])) > 0){
				### this row is part of one of the target parent projects
				Condition = c(Condition, Parent.Project.Conditions[which(Parent.Project.Ids == row$Parent.Project.Id[1])[1]])
			} else{
				### this row is not part of one of the target parent projects
				### find a row in the parent project that has same Wise.User.Id
				id1 = row$Wise.Id.1[1];
				idf = subset(df.pp, Wise.Id.1%in%id1|Wise.Id.2%in%id1|Wise.Id.3%in%id1)
				if (nrow(idf) == 0){
					Condition = c(Condition, NA);
					error = paste("Wise User", id1, "did not complete an experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else if (length(unique(idf$Parent.Project.Id)) > 1){
					# bad, id is in more than one exp project
					Condition = c(Condition, NA);
					error = paste("Wise User", id1, "is associated with more than one experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else {
					if (!is.na(as.numeric(as.character(row$Wise.Id.2[1]))) || !is.na(as.numeric(as.character(row$Wise.Id.3[1])))){
						if(VERBOSE) print(paste("Wise users from workgroup",row$Workgroup.Id,"in non-experimental project has more than one member."))
						## okay so the teacher did a non-experimental project in groups (bad), but were they all at least in the same condition?
						id23 = ifelse(!is.nan(as.numeric(as.character(row$Wise.Id.2[1]))) , row$Wise.Id.2[1] , numeric())
						id23 = ifelse(!is.nan(as.numeric(as.character(row$Wise.Id.3[1]))) , c(id23, row$Wise.Id.3[1]) , id23)
						id23 = id23[!is.na(id23)]
						idf23 = subset(df.pp, Wise.Id.1%in%id23|Wise.Id.2%in%id23|Wise.Id.3%in%id23)
						if (nrow(idf23) == 0){
							Condition = c(Condition, Parent.Project.Conditions[which(Parent.Project.Ids == idf$Parent.Project.Id[1])[1]])
							error = paste("Wise User", id23, "did not complete an experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else if (length(unique(idf23$Parent.Project.Id)) > 1){
							# bad, id is in more than one exp project
							Condition = c(Condition, NA);
							error = paste("Wise Users from Workgroup", row$Workgroup.Id, "are associated with more than one experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else {
							## folks were all in the same condition
							Condition = c(Condition, Parent.Project.Conditions[which(Parent.Project.Ids == idf23$Parent.Project.Id[1])[1]])
						}
						
					} else {
						## Ideal, one user in non-experiment, associated with one experimental project
						Condition = c(Condition, Parent.Project.Conditions[which(Parent.Project.Ids == idf$Parent.Project.Id[1])[1]])
					}
				}
			}
		}
	} else if (method=="Workgroup.Id.mod"){
		#### Generate conditions based on the mod value of Workgroup.Id within the given parent project
		for (i in 1:nrow(df)){
			row = df[i,];
			if (row$Parent.Project.Id[1] %in% Parent.Project.Ids){
				### this row is part of one of the target parent projects, find the mod of workgroup id
				Condition = c(Condition, as.numeric(as.character(row$Workgroup.Id))%%Workgroup.Id.mod+1);
			} else{
				### this row is not part of one of the target parent projects
				### find a row in the parent project that has same Wise.User.Id
				id1 = row$Wise.Id.1[1];
				idf = subset(df.pp, Wise.Id.1%in%id1|Wise.Id.2%in%id1|Wise.Id.3%in%id1)
				if (nrow(idf) == 0){
					Condition = c(Condition, NA);
					error = paste("Wise User", id1, "did not complete an experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else if (length(unique(idf$Parent.Project.Id)) > 1){
					# bad, id is in more than one exp project
					Condition = c(Condition, NA);
					error = paste("Wise User", id1, "is associated with more than one experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else {
					if (!is.nan(row$Wise.Id.2[1]) || !is.nan(row$Wise.Id.3[1])){
						#if(VERBOSE) print(paste("Wise users from workgroup",row$Workgroup.Id,"in non-experimental project has more than one member."))
						## okay so the teacher did a non-experimental project in groups (bad), but were they all at least in the same condition?
						id23 = ifelse(!is.nan(row$Wise.Id.2[1]) , row$Wise.Id.2[1] , numeric())
						id23 = ifelse(!is.nan(row$Wise.Id.3[1]) , c(id23, row$Wise.Id.3[1]) , id23)
						id23 = id23[!is.na(id23)]
						idf23 = subset(df.pp, Wise.Id.1%in%id23|Wise.Id.2%in%id23|Wise.Id.3%in%id23)
						if (nrow(idf23) == 0){
							Condition = c(Condition, as.numeric(as.character(idf$Workgroup.Id))[1]%%Workgroup.Id.mod+1)
							error = paste("Wise User", id23, "did not complete an experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else if (length(unique(idf23$Parent.Project.Id)) > 1){
							# bad, id is in more than one exp project
							Condition = c(Condition, 0);
							error = paste("Wise Users from Workgroup", row$Workgroup.Id, "are associated with more than one experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else {
							## folks were all in the same condition
							Condition = c(Condition, as.numeric(as.character(idf23$Workgroup.Id))[1]%%Workgroup.Id.mod+1)
							error = paste("Wise users from workgroup",row$Workgroup.Id,"in run",row$Run.Id, "has more than one member, but they all were same condition.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						}
						
					} else {
						## Ideal, one user in non-experiment, associated with one experimental project
						Condition = c(Condition, as.numeric(as.character(idf$Workgroup.Id))[1]%%Workgroup.Id.mod+1)
					}
				}
			}
		}
	} else {
		print(paste("The method"), method, "does not exist.")
	}
	Condition = as.factor(Condition);
	if (as.data.frame.out){
		oclass = class(df)
		df = cbind(df, Condition=Condition)
		class(df) = oclass
		return (df)
	} else {
		return (Condition);
	}
}

### Looks for repeat of rows (same project/run/workgroup id) and makes a count
##  of each revision
getRevisionNumber <- function(df, inverse=FALSE, incrementIntegerOnChange=FALSE, betweenChangeIncrements=0.001){
	# place the index in the first column of the df so we can find them again
	df = cbind(Index=1:nrow(df), df);
	Index = numeric();
	Rev.Num = numeric();
	## filter down to each step for each student in each run of each project
	wgIds = unique(df$Workgroup.Id);
	for (wgid in wgIds){
		df.wg = subset(df, Workgroup.Id==wgid);
		stepTitles = unique(df.wg$Step.Title);
		stepTitles = stepTitles[!is.na(stepTitles)];
		for (st in stepTitles){
			df.wg.st = subset(df.wg,Step.Title==st);
			# after all that we finally have a single student's set of responses to a question
			Index = c(Index, df.wg.st$Index);
			if (incrementIntegerOnChange && length(grep("Student.Work",names(df.wg.st))) > 0){
				## between changes increments used when value of student work has not changed 
				swindices = grep("Student.Work",names(df.wg.st));
				df.wg.st[swindices][is.na(df.wg.st[swindices])|df.wg.st[swindices]=="NA"|df.wg.st[swindices]=="N/A"] = ""
				sw = do.call(paste, c(df.wg.st[swindices],sep=""))
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
				rev = 1:length(df.wg.st$Index);
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
	}
	## combine Index and Rev.Num into a data.frame, order by index and return Rev.Num
	tdf = data.frame(Index, Rev.Num);
	tdf = tdf[order(tdf$Index),];
	return (tdf$Rev.Num)
}

# Looks for how many took place in a single step
getRevisionNumberInStep <- function(df, as.data.frame.out = FALSE){
	URev.Step.Num <- numeric()
	for (r in 1:nrow(df)){
		row <- df[r,]
		sw <- as.wiseSW(row)
		l <- length(sw)
		if (l == 1 && is.na(sw)) l <- 0
		URev.Step.Num <- c(URev.Step.Num, l)
	}
	if (as.data.frame.out){
		df$URev.Step.Num <- URev.Step.Num
		return (df)
	} else {
		return (URev.Step.Num)
	}
}

######################  Summary functions ############################################
## These functions are used to turn a complete, step-by-step data.frame into summaries

###
# select.first are those columns we want to keep intact from the first row
# select.numerical are those columns where we are applying some function to get aggegrate data
# FUNS.numerical correspond to each select.mumerical
# awiseDF = :? (wiseDF, by=list(Workgroup.Id), select.first = c(Project.Id, Run.Id, Wise.Id.1, Wise.Id.2), c(Teacher.Score, Research.Score), c("sum", "mean", "sd", "median", "min", "max")); #awiseDF[150:170,]
aggregate.wisedata.frame <- function (x, by = list(Workgroup.Id), select.first = c(Project.Id, Parent.Project.Id, Run.Id, Wise.Id.1, Wise.Id.2, Wise.Id.3, Condition), select.numerical, FUNS.numerical, include.median.splits = FALSE, ..., simplify = TRUE){
	df = x;
	class(df) = "data.frame";  # this way when we call aggregate it won't recursivley call this.
	# create a new data frame with just by factor
	first =  function(fdf){return(fdf[1])}
	by_call = substitute(by);
	b = deparse(by_call);
	b = sub("^ *list *\\( *","", b); b = sub(" *\\) *$", "", b);
	b = strsplit(b, ", *")[[1]]
	by_index = which(names(x) %in% b);
	by = eval(by_call,x,parent.frame());
	### Get just a dataframe with the grouping variables
	odf = aggregate(subset(df,TRUE,by_index), by, FUN = first, simplify=FALSE)[,c(1,length(by_index))];
	names(odf) = b
	### seb2lect.first are those items that we just want the top value
	if (!missing(select.first)  && !is.null(select.first)){
		nl = as.list(seq_along(x));
		names(nl) = names(x);
		vars = eval(substitute(select.first), nl, parent.frame());
		df.select.first = df[,c(vars)];
		odf = aggregate(df.select.first, by, FUN = first, simplify=TRUE);
		names(odf)[1:length(b)] = b
	}
	if (!missing(select.numerical)&& !missing(FUNS.numerical)){
		# we need to aggregate by step first
		nl = as.list(seq_along(x));
		names(nl) = names(x);
		vars = eval(substitute(select.numerical), nl, parent.frame());
		### in some cases we have a list of variable names
		if (class(vars)[1] == "list"){
			tvars = integer()
			for (sn in vars){
				tvars = c(tvars, eval(sn, nl, parent.frame()));
			}
			vars = tvars
		} else if (class(vars)[1] != "integer" && class(vars)[1] != "numeric"){
			vars = which(names(df) %in% vars)
		}
		# we'll be using and expanded df to account for multiple revisions
		df.exp <- expandMultipleResponses(df)		

		for (fun in FUNS.numerical){			
			## special for median split
			if (fun == "first"){
				adf = subset(df.exp, URev.Num==1.000, c(by_index, vars))
				adf = subset(adf, !duplicated(adf[,b]))
				adf = merge(subset(odf,TRUE,b), adf, all.x=TRUE, all.y=FALSE, suffixes=c("",""))
			} else if (fun == "last"){
				adf = subset(df.exp, IURev.Num==0.000, c(by_index, vars))
				adf = subset(adf, !duplicated(adf[,b], fromLast=TRUE))
				adf = merge(subset(odf,TRUE,b), adf, all.x=TRUE, all.y=FALSE, suffixes=c("",""))
			} else if (fun == "first.to.last"){
				adf.f = subset(df.exp, URev.Num==1.000, c(by_index, vars))
				adf.f = subset(adf.f, !duplicated(adf.f[,b]))
				adf = subset(df.exp, IURev.Num==0.000, c(by_index, vars))
				adf = subset(adf, !duplicated(adf[,b], fromLast=TRUE))
				adf[2:ncol(adf)] = adf[2:ncol(adf)] - adf.f[2:ncol(adf.f)] 
				adf = merge(subset(odf,TRUE,b), adf, all.x=TRUE, all.y=FALSE, suffixes=c("",""))
			} else {
				df.select.numerical = df.exp[,vars];
				print (names(df.select.numerical))
				print (fun)
				adf = aggregate(df.select.numerical, by, FUN = fun, na.rm = TRUE, simplify=TRUE);
			}
			## if there is more than one numerical function attach the name of the funciton
			#if (length(FUNS.numerical) > 1){ 
			names(adf) = paste(names(df)[c(by_index,vars)],fun,sep=".") 
			adf = subset(adf,TRUE,-1*c(1:length(b)))
			if (include.median.splits){
				mdf = adf
				for (c in ncol(mdf):1){
					# only if there are more than three levels (not including NA)
					#print(names(mdf)[c])
					if (length(!is.na(unique(adf[,c]))) > 3 ){
						if (!is.null(ncol(mdf))){
							mdf[,c] = adf[,c] > median(adf[,c], na.rm=TRUE)
							names(mdf)[c] = paste(names(mdf)[c],"median.split",sep=".")	
						} else {
							mdf = adf[,c] > median(adf[,c], na.rm=TRUE)
							missing.name = names(adf)[c]
						}
					} else {
						if (!is.null(ncol(mdf))){
							mdf = mdf[,-c]
						} else {
							mdf = NULL
						}
					}
				}
				if (!is.null(mdf)) {
					adf = cbind(adf, mdf)
					if (is.null(ncol(mdf))){
						names(adf)[ncol(adf)] = paste(missing.name,"median.split",sep=".")	
					} 
				}
			}
			odf = cbind(odf, adf);
		}
	}
	
	Collapse.Count = numeric();
	for (i in 1:nrow(odf)){
		row = odf[i,]
		sdf = df
		for (b.name in b){
			sdf = subset(sdf, sdf[,which(names(sdf)==b.name)]==row[1,which(names(row)==b.name)])
		}
		Collapse.Count = c(Collapse.Count, nrow(sdf))
	}
	odf$Collapse.Count = Collapse.Count
	class(odf) = c("aggwisedata.frame", "wisedata.frame", "data.frame");
	return(odf);
}
magna <- magna.carta.holy.grail(subset(wise, Step.Id%in%c(items.pretest,items.posttest,items.unit)), by="Wise.Id.1", step.identifier = "Step.Id", select.numerical = sapply(grep("Index|Time\\.Spent\\.Seconds|Rev\\.Num|^URev\\.Num|Research\\.Score|^KI\\.|^C\\.|.*?Score|NWords",names(wise[,!grepl("((KI)|C|(Score))?.*2",names(wise))]), value=TRUE),as.name), FUNS.numerical = c("sum", "mean","min", "max", "first","last", "first.to.last"))

aggregate.aggwisedata.frame <- function (x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE){
	df = x; class(df) = c("wisedata.frame", "data.frame");
	by = eval(substitute(by),x,parent.frame());
	odf = aggregate(df,by, select.first, select.numerical, FUNS.numerical, simplify = TRUE);
	class(odf) = c("aggwisedata.frame", "wisedata.frame", "data.frame");
	return(odf);
}



######################  Subsetting functions ############################################
## These functions are used to take a data frame and reduce it to specific projects, wise Ids, runs, etc.

## Same functionality as data frame except a subset of Wise.Id will search for the
## given id in all three possible slots.
subset.wisedata.frame <- function(x, subset, select, drop = FALSE, ...){
	if (missing(subset)) 
	       r <- TRUE
	else {
	   e <- substitute(subset)
	   # in case of Call Wise.Id (no .1, .2, etc) perform match against all three
	   if (length(as.character(e)) > 1 && as.character(e)[2] == "Wise.Id"){
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

### This function will return all steps between the first instance of Step.Id.start and last instance of Step.Id.end
###   for each workgroup. Should run fast - no looping (mostly).
subset.StepsBetween <- function (obj, Step.Id.start, Step.Id.stop, inclusive=FALSE){
	d <- obj[,c("Workgroup.Id","Step.Id")]
	start <- Step.Id.start
	stop <- Step.Id.stop
	d[,"start"] <- sapply(obj$Step.Id,function(x)if(x==start){return(1)}else{return(0)})
	d[,"stop"] <- sapply(obj$Step.Id,function(x)if(x==stop){return(-1)}else{return(0)})
	# remove repeat starts and stops
	d[,"start"] <- as.numeric(!duplicated(d[,c("Workgroup.Id","start")])&d$start==1)
	d[,"stop"] <- -1*as.numeric(!duplicated(d[,c("Workgroup.Id","stop")],fromLast=TRUE)&d$stop==-1)
	if (inclusive){
		# shift "stop" down one
		d[,"stop"] <- c(0,d$stop[1:(length(d$stop)-1)])
	} else {
		# shift "start" down one
		d[,"start"] <- c(0,d$start[1:(length(d$start)-1)])
	}
	
	d[,"s"] <- d$start + d$stop
	#d$up <- 1-as.numeric(duplicated(d[,c("Workgroup.Id","start")])&d$s==1)
	#d$down <- 1-as.numeric(duplicated(d[,c("Workgroup.Id","stop")],fromLast=TRUE)&d$s==-1)
	#d$u <- d$up + d$down - 1
	d$s2 <- d$s #*d$u
	# who didn't reach stop?
	ended <- unique(d$Workgroup.Id[d$s2==-1])
	noend <- unique(d$Workgroup.Id[!(d$Workgroup.Id %in% ended)])
	# cheat a bit, find last row of unended workgroups (didn't reach stop), make sure that
	# the following row indices that there should be a stoppage
	for (wg in noend){
		index <- max(which(d$Workgroup.Id == wg))
		if (index + 1 < nrow(d)){
			d$s2[index + 1] <- d$s2[index + 1] - 1
		}
	}
	# who didn't start?
	started <- unique(d$Workgroup.Id[d$s2==1])
	notstarted <- unique(d$Workgroup.Id[!(d$Workgroup.Id %in% started)])
	# cheat a bit, find last row of unended workgroups (didn't reach stop), make sure that
	# the following row indices that there should be a stoppage
	for (wg in notstarted){
		index <- max(which(d$Workgroup.Id == wg))
		if (index - 1 > 0){
			d$s2[index - 1] <- d$s2[index - 1] + 1
		}
	}

	d$between <- cumsum(d$s2)
	#print(subset(d, Workgroup.Id==158892))

	return (obj[as.logical(d$between),])
}


### For each workgroup subsets to only those steps between the first revision (unique if given) of a step and the last revision of a step
### We can start from the first instance of a step or the last instance of a step (start.first, start.last)
subset.StepRange <- function (df, Step.Num.NoBranch.first, Step.Num.NoBranch.last, Step.Id.first = NULL, Step.Id.last = NULL, start.first=TRUE, start.last=FALSE, unique.first=FALSE, unique.last=TRUE){
	library(plyr)
	if (missing(Step.Num.NoBranch.first) && !is.null(Step.Id.first) && !is.null(Step.Id.last)){
		Step.Num.NoBranch.first <- as.numeric(as.character(unique(subset(df, Step.Id==Step.Id.first)$Step.Num.NoBranch)[1]))
		Step.Num.NoBranch.last <- as.numeric(as.character(unique(subset(df, Step.Id==Step.Id.last)$Step.Num.NoBranch)[1]))
		print(paste(Step.Num.NoBranch.first, Step.Num.NoBranch.last))
	}
	
	df.out = df[0,]
	for (wg in unique(df$Workgroup.Id)){
		if (unique.first){
			if (start.first){
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.first&URev.Num==1)$Index[1]	
			} else {
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.first&IURev.Num==0)$Index[1]	
			}		
		} else {
			if (start.first){
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.first&Rev.Num==1)$Index[1]
			} else {
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.first&IRev.Num==0)$Index[1]
			}
		}

		if (unique.last){
			if (start.last){
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.last&URev.Num==1)$Index[1]	
			} else {
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.last&IURev.Num==0)$Index[1]	
			}		
		} else {
			if (start.last){
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.last&Rev.Num==1)$Index[1]
			} else {
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==Step.Num.NoBranch.last&IRev.Num==0)$Index[1]
			}
		}
		df.temp <- subset(df, Index>=index.first&Index<=index.last&Workgroup.Id==wg)	
		if (nrow(df.temp) > 0) df.out <- rbind(df.out, df.temp)
	}
	return (df.out)
}
#wise.between.revision <- subset.Step.Range (subset(gcc, Is.Unit==TRUE), Step.Id.first=stid, Step.Id.last=stid, unique.first=FALSE, unique.last=TRUE, start.first=TRUE, start.last=FALSE)

#group <- subset(gcc, Workgroup.Id==wgid)
#group.between.revision <- subset.Step.Range (group, Step.Id.first=stid, Step.Id.last=stid, unique.first=FALSE, unique.last=TRUE, start.first=TRUE, start.last=FALSE)
#group.between.revision <- subset(group.between.revision, Step.Id %in% vstid)

#gmagna <- subset(magna, Workgroup.Id==wgid)[,1:5]
#merge(gmagna, aggregate(group.between.revision, by=list(Workgroup.Id)), by="Workgroup.Id", all.x=TRUE, all.y=FALSE)$Collapse.Count
############### PRIVATE FUNCTIONS ##############################
# These are mainly used by the functions above, you can use if you'd like

## Counts the number of sheets, or tabs, in an excel file
countExcelSheets <- function(fileName){
	wb = loadWorkbook(fileName);
	sheets = getSheets(wb);
	return (length(sheets));
}

## A second implementation that uses error checking
## This is a SLOW brute force method, but requires less memory from java
countExcelSheets2 <- function(fileName, maxSheets=200){
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
getSheetNames <- function(fileName, maxSheets=100){
	wb = loadWorkbook(fileName);
	sheets = getSheets(wb);
	return (names(sheets));
}


## Uses regular expression matching to find the step number in the front of a step title
getStepNum <- function (df){
	stepTitles = as.character(df$Step.Title);
	matchNum = regexpr("[0-9]+", stepTitles);
	stepTitles[matchNum==-1] = "0"
	matchNum = regexpr("[0-9]+", stepTitles);
	stepNums = as.numeric(substr(stepTitles,matchNum,attr(matchNum,"match.length")))
	## second level
	stepTitles2 = substr(stepTitles,matchNum+attr(matchNum,"match.length"),nchar(stepTitles))
	matchNum = regexpr("\\.[0-9]+", stepTitles2);
	if (length(matchNum) != -1*sum(matchNum)){
		stepTitles2[matchNum==-1] = ".0"
		matchNum = regexpr("\\.[0-9]+", stepTitles2);
		stepNums = stepNums + as.numeric(substr(stepTitles2,matchNum+1,attr(matchNum,"match.length")))/100
		#third level
		stepTitles3 = substr(stepTitles2,matchNum+attr(matchNum,"match.length"),nchar(stepTitles))
		matchNum = regexpr("\\.[0-9]+", stepTitles3);
		if (length(matchNum) != -1*sum(matchNum)){
			stepTitles3[matchNum==-1] = ".0"
			matchNum = regexpr("\\.[0-9]+", stepTitles3);
			stepNums = stepNums + as.numeric(substr(stepTitles3,matchNum+1,attr(matchNum,"match.length")))/10000
		}
	}
	return (stepNums);
}

collapseStepNumBranches <- function (Step.Num){
	Step.Num.NoBranch = round(10000*as.numeric(as.character(Step.Num)))
	branchActivities = 10000*floor(Step.Num.NoBranch[Step.Num.NoBranch%%100!=0]/10000) + 100
	Step.Num.NoBranch[!is.na(Step.Num.NoBranch)&Step.Num.NoBranch%in%branchActivities] = Step.Num.NoBranch[!is.na(Step.Num.NoBranch)&Step.Num.NoBranch%in%branchActivities] - 100
	## convert first step of branchActivities to zero, e.g. 5.1 to 5.0
	#levels(Step.Num) = c(levels(Step.Num),unique(as.character(branchActivities)))
	#Step.Num.NoBranch[Step.Num%in%(branchActivities+0.01)] = as.factor(as.character(as.numeric(as.character(Step.Num[Step.Num%in%(branchActivities+0.01)])) - 0.01))

	Step.Num.NoBranch[!is.na(Step.Num.NoBranch)&Step.Num.NoBranch%%100!=0] = 1000*floor(Step.Num.NoBranch[!is.na(Step.Num.NoBranch)&Step.Num.NoBranch%%100!=0]/1000) + 100*(Step.Num.NoBranch[!is.na(Step.Num.NoBranch)&Step.Num.NoBranch%%100!=0]%%100)
	Step.Num.NoBranch = as.factor(as.numeric(as.character(Step.Num.NoBranch/10000)))
	return (Step.Num.NoBranch)
}

### This function is used on reading excel files into data and intrepets columns as numeric or character
### These classes will not be the final classes, many will be factors, use getColClasses.final to
### set their final classes
getColClasses.read <- function (colNames){
	## is this a data frame with names on top or just a list of names
	if (is.null(nrow(colNames))){
		cnames = colNames;
	} else {
		cnames = names(colNames);
	}

	### clean up names a bit (remove .., . at end, "if applicable")
	cnames= gsub("..if.applicable.", "", cnames);
	cnames = gsub("\\.\\.", "\\.", cnames);
	cnames = gsub("\\.$", "", cnames);

	colClasses = character();
	for (i in 1:length(cnames))
	{
		colName = cnames[i];	
		if (
			grepl("time\\.spent",tolower(colName))[1] ||
			grepl("score",tolower(colName))[1] ||
			grepl("count",tolower(colName))[1] ||
			grepl("crater",tolower(colName))[1] ||
			grepl("c\\.",tolower(colName))[1] ||
			grepl("num",tolower(colName))[1] ||
			grepl("step\\.work\\.id",tolower(colName))[1] ||
			grepl("workgroup\\.id",tolower(colName))[1] ||
			grepl("wise\\.id",tolower(colName))[1] ||
			grepl("project\\.id",tolower(colName))[1] ||
			grepl("run\\.id",tolower(colName))[1] ||
			grepl("index",tolower(colName))[1] ||
			grepl("Period",tolower(colName))[1] ||
			grepl("action\\.performer",tolower(colName))[1] ||
			grepl("revision",tolower(colName))[1] ||
			grepl("position",tolower(colName))[1]
		){
			colClasses = c(colClasses, "numeric");
		}else {
			colClasses = c(colClasses, "character");
		}
	}
	return (colClasses);
}

### Goes through a series of column names and converts to a column class.
getColClasses.final <- function (cnames){	
	### clean up names a bit (remove .., . at end, "if applicable")
	cnames = gsub("..if.applicable.", "", cnames);
	cnames = gsub("\\.\\.", "\\.", cnames);
	cnames = gsub("\\.$", "", cnames);

	colClasses = character();
	for (i in 1:length(cnames))
	{
		colName = cnames[i];
		
		if (colName == "Workgroup.Id" ||
			colName == "Condition" ||
			colName ==  "Wise.Id.1" ||
			colName ==  "Wise.Id.2" ||
			colName ==  "Wise.Id.3" ||
			colName ==  "Class.Period" ||
			colName ==  "Project.Id" ||
			colName ==  "Parent.Project.Id" ||
			colName ==  "Run.Id" ||
			colName == "Step.Type" ||
			colName == "Step.Work.Id" ||
			colName == "Step.Id" ||
			colName == "Node.Id" ||
			colName == "Action"  ||
			colName == "Action.Performer"  ||
			colName == "Changed.Idea.Workgroup.Id"  ||
			colName == "Idea.Workgroup.Id"  ||
			colName == "Node.Type" ||
			colName == "Teacher.Login" ||
			colName == "Run.Name"  ||
			colName == "Project.Name" ||
			colName == "Step.Title"
		){
			colClasses = c(colClasses, "factor");
		} else if (
			grepl("C\\.", colName)[1] ||
			grepl("KI\\.", colName)[1] ||
			grepl("\\.KI$", colName)[1] ||
			grepl("Time\\.Spent",colName)[1] ||
			grepl("Rev\\.Num",colName)[1] ||
			grepl("Research\\.Score",colName)[1] ||
			grepl("Max\\.Score",colName)[1] ||
			grepl("score",colName)[1] ||
			grepl("Count",colName)[1] ||
			colName == "Step.Num" ||
			colName == "Step.Num.NoBranch" ||
			colName == "Index" ||
			colName == "Times.Copied" ||
			colName == "Basket.Revision" ||
			colName == "Idea.X.Position" ||
			colName == "Idea.Y.Position" 
		){
			colClasses = c(colClasses, "numeric");
		} else if (
			grepl("Student\\.Work",colName)[1] ||
			grepl("Idea\\.Text", colName)[1] ||
			colName == "Teacher.Comment" ||
			colName == "Teacher.Comment.Timestamp" ||
			colName == "Teacher.Score" ||
			colName == "Teacher.Score.Timestamp" ||
			colName == "Auto.Feedback" ||
			colName == "Auto.Score" ||
			colName == "Revision.Time"
		){
			colClasses = c(colClasses, "character");
		} else {
			colClasses = c(colClasses, "character");
		}
	}
	return (colClasses);
}

setColClasses.final <- function (df, column.names=names(df)){
	fcolClasses = getColClasses.final(column.names);
	for (c in 1:ncol(df)){
		if (fcolClasses[c] == "numeric"){
			df[,c] = as.numeric(df[,c]);
		} else if (fcolClasses[c] == "factor"){
			df[,c] = as.factor(df[,c]);
		} else if (fcolClasses[c] == "character"){
			df[,c] = as.character(df[,c]);
		}
	}
	return (df)
}
