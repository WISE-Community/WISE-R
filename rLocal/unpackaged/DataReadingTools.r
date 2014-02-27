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

## The read.xlsx.wiseDir.old function creates a data frame that contains all data from the given directory
## Since this is a "flat" data.frame, i.e., the different files and tabs are all stored in the
## same data.frame columns have been added to differentiate between runs, workgroups, etc.
## UPDATES:
### - excel file now contains a workgroup Id column, will check for this
#\name{read.xlsx.wiseDir}
#\description
read.xlsx.wiseDir = function (dir, Student.Work.Count = 4, Parent.Project.Id.pretest = numeric(), Parent.Project.Id.posttest = numeric(), do.collapseMultipleRevisions = TRUE, sheetIndices = 1:1000, fileIndices = NULL, perl = NULL, use.XLConnect = FALSE, DEBUG = FALSE, DEEPDEBUG = FALSE, ...){
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
								read.xlsx(filename, sheetIndex = s, startRow=1, endRow=1, stringsAsFactors = FALSE);
							}
						}, error = function(e){
							return(NULL)
						});	
						if (is.null(head)) break;
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
					df = rbind(df, filedf);
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

	df$Is.Posttest = as.factor(df$Parent.Project.Id %in% Parent.Project.Id.posttest)
	df$Is.Pretest = as.factor(df$Parent.Project.Id %in% Parent.Project.Id.pretest)
	df$Is.Unit = as.factor(!(df$Parent.Project.Id %in% Parent.Project.Id.posttest | df$Parent.Project.Id %in% Parent.Project.Id.pretest))
	df$Step.Id = gsub("\\:\\.",":",gsub("\\?\\.","?",gsub("-.", "",gsub(" +", ".", paste(c("Unit","Pretest","Posttest")[as.logical(df$Is.Pretest) + 2*as.logical(df$Is.Posttest) + 1], sub("^([0-9]+. ?)+","",df$Step.Title),sep=".")))))
	
	class(df) = c("wisedata.frame",class(df));
	return (df);
}

read.xlsx.wise = function(filename, sheetIndex, sheetName=NULL){
	head = tryCatch({
		if (!is.null(sheetName)){
			read.xlsx(filename, sheetName = sheetName, startRow=1, endRow=1, stringsAsFactors = FALSE);
		} else {
			read.xlsx(filename, sheetIndex = sheetIndex, startRow=1, endRow=1, stringsAsFactors = FALSE);
		}
	}, error = function(e){
		return(NULL)
	});	
	if (is.null(head)) return (NULL)
	colClasses = getColClasses.read(names(head));

	df = tryCatch({
		if (!is.null(sheetName)){
			read.xlsx(filename, sheetName = sheetName, colClasses=colClasses, stringsAsFactors = FALSE);
		} else {
			read.xlsx(filename, sheetIndex = sheetIndex, colClasses=colClasses, stringsAsFactors = FALSE);
		}
	}, error = function(e){
		return(NULL)
	});	
	if (is.null(df)) return(NULL)

	# make sure there aren't any empty rows - i.e. those without a wise id
	df = subset(df, !is.nan(as.numeric(as.character(Wise.Id.1))))
	# create an index
	df = cbind(Index=1:nrow(df), df);
	### clean up names a bit (remove .., . at end, "if applicable")
	names(df) = gsub("..if.applicable.", "", names(df));
	names(df) = gsub("\\.\\.", "\\.", names(df));
	names(df) = gsub("\\.$", "", names(df));
	### get final classes and update
	df = setColClasses.final(df);
	df = subset(df, !is.na(as.numeric(as.character(Wise.Id.1))))
	return(df)
}

#wise = read.xlsx.wiseDir(excelDirectory, Student.Work.Count = 7,perl="C:\\perl64\\bin\\perl.exe", DEBUG=TRUE);
#wise = read.xlsx.wiseDir(excelDirectory, Student.Work.Count = 4, Parent.Project.Id.pretest=Parent.Project.Id.pretest); 

### In the current version of the downloader multiple responses in the single step are 
### separated into multiple rows.  This will reduce the data to only the last revision within a step visit
collapseMultipleRevisions = function (df, use.last = TRUE){
	### take last of each step work id
	for (s.id in unique(df$Step.Work.Id)){
		Index = subset(df, Step.Work.Id == s.id)$Index
		### eliminate extra indices
		if (length(Index) > 1){
			Index.rm = Index[1:(length(Index)-1)]
			df = subset(df, !(Index %in% Index.rm))
		}
	}
	return (df);
}

## Given a wisedata.frame will iterate through all rows looking for multiple student responses that are concatatenated into a single column
## if expandToColumn is TRUE, then each response will be placed in a new Student.Work.Column, if we run out of columns the entire data frame will be expanded
## if expandToColumn is FALSE, then a row will be repeated for each response
## regexp.split is the regular expression used to split responses
expandMultipleResponses = function (df, expandToColumn = TRUE, regexp.split="Response #[0-9]+: "){
	if (length(which(class(df)=="wisedata.frame")) == 0){print("You need to use a valid wise data frame."); return (NULL)}
	returndf = df[1,];
	returndf = returndf[-1,];
	if (expandToColumn){
		for (i in 1:nrow(df)){
			row = df[i,];
			indices =  grep("Student.Work",names(row))
			index = indices[1];	
			## make sure that only one row has student work
			if (sum(row[1,indices]!="")==1){
				sw = row[1, index];
				responses = strsplit(sw, regexp.split)[[1]];
				### remove blanks
				responses = responses[nchar(responses) > 0];
				### if we have more responses than student work columns, start adding columns
				if (length(responses) > length(indices)){
					for (j in 1:(length(responses) - length(indices))){
						colname  = paste("Student.Work.Part.", j + length(indices),sep=""); 
						### add new column in both returndf and row
						row = cbind(row,  newcol = rep("",nrow(row)));
						names(row)[which(names(row)=="newcol")] = colname;
						returndf = cbind(returndf,  newcol = rep("",nrow(returndf)));
						names(returndf)[which(names(returndf)=="newcol")] = colname;
						df = cbind(df,  newcol = rep("",nrow(df)));
						names(df)[which(names(df)=="newcol")] = colname;
					}
					## get new indices
					indices =  grep("Student.Work",names(row))
				}
				## place each response in a new column of student work
				for (j in 1:length(responses)){
					r = responses[j];
					levels(row[,indices[j]]) = c(levels(row[,indices[j]]), r)
					row[1,indices[j]] = r;
				}
			}
			returndf = rbind(returndf, row);
		}
		return (returndf);
	} else {
		### Create new rows for multiple responses, keep everything the same except student work and add an incremental value to Index
		for (i in 1:nrow(df)){
			row = df[i,];
			indices =  grep("Student.Work",names(row))
			index = indices[1];	
			## make sure that only one row has student work
			if (sum(row[1,indices]!="")==1){
				sw = row[1, index];
				responses = strsplit (sw, regexp.split)[[1]];
				### remove blanks
				responses = responses[nchar(responses) > 0];

				## create duplicate rows for each response
				rowdf = row; 
				rowdf = rowdf[-1,]
				for (j in 1:length(responses)){
					r = responses[j];
					rowdf = rbind(rowdf, row);
					rowdf[j,index] = r;
					rowdf$Index[j] = rowdf$Index[j]+(j-1)/1000
				} 
				returndf = rbind(returndf, rowdf);
			} else {
				returndf = rbind(returndf, row);
			}
		}
		return (returndf);
	}
}

# expand each workgroup into the number of participants,
# If one Wise User, then row remains the same
# If two Wise users, then row one remains the same, row two switches placement of wiseUsers
# If three Wise users, then row one remains th same, row two shifts back one place, row three shifts back two places 
expandWorkgroupToWiseId = function (df){
	df.out = data.frame();
	for (r in 1:nrow(df)){
		row = df[r,];
		if (!is.na(as.numeric(as.character(row$Wise.Id.1))) && !is.na(as.numeric(as.character(row$Wise.Id.2))) && !is.na(as.numeric(as.character(row$Wise.Id.3)))) {
			df.out = rbind(df.out,row);
			temp = row$Wise.Id.1;
			row$Wise.Id.1 = row$Wise.Id.2
			row$Wise.Id.2 = row$Wise.Id.3
			row$Wise.Id.3 = temp
			df.out = rbind(df.out, row);
			temp = row$Wise.Id.1;
			row$Wise.Id.1 = row$Wise.Id.2
			row$Wise.Id.2 = row$Wise.Id.3
			row$Wise.Id.3 = temp
			df.out = rbind(df.out, row);
		} else if (!is.na(as.numeric(as.character(row$Wise.Id.1))) && !is.na(as.numeric(as.character(row$Wise.Id.2)))){
			df.out = rbind(df.out,row);
			temp = row$Wise.Id.1;
			row$Wise.Id.1 = row$Wise.Id.2
			row$Wise.Id.2 = temp;
			df.out = rbind(df.out, row);
		} else if (!is.na(row$Wise.Id.1)){
			df.out = rbind(df.out,row);
		}
	}
	return (df.out);
}

refactor = function(df, colNames=NULL){
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
readForColValues = function (targetDF, sourceFile, targetColNames=NULL, sourceColNames="Student.Work.Part.1", targetIdentifier="Step.Work.Id", sourceIdentifier="Step.Work.Id", includes.formulas = TRUE, ...){
	if (includes.formulas) {
		sourceDF = read.xlsx(sourceFile, stringsAsFactors=FALSE, keepFormulas=FALSE, ...)
	} else {
		sourceDF = read.xlsx2(sourceFile, stringsAsFactors=FALSE,...)
	}
	## make sure step work id is in the correct format
	sourceDF[,sourceIdentifier] = as.factor(as.numeric(as.character(sourceDF[,sourceIdentifier])))
	print(unique(sourceDF[,sourceIdentifier]))

	print(unique(subset(targetDF, (targetDF[,targetIdentifier] %in% sourceDF[,sourceIdentifier]))$Step.Id)) 
	df.out = transferColValues(targetDF=targetDF, sourceDF=sourceDF, targetColNames=targetColNames, sourceColNames=sourceColNames, targetIdentifier=targetIdentifier, sourceIdentifier=sourceIdentifier)
	return (df.out)
}

#### reads "prepost" files
#### if sourceColNames.except is not NULL (or sourceColNames is NULL) will read all columns except ones specified
#### the target column names will be the same as sourceColNames (minus strip.postfixes)
#### strip.postfixes is a character 
readForColValues.prepost = function (targetDF, sourceFile, sourceColNames.except = NULL, sourceColNames = NULL, postfixes = c("\\.Pretest","\\.Posttest"), targetIdentifier="Step.Work.Id", sourceIdentifier="Step.Work.Id", includes.formulas = TRUE, ...){
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
		targetDF = transferColValues(targetDF=targetDF, sourceDF=sourceDF, targetColNames=sourceColNames.postfix.removed, sourceColNames=sourceColNames.postfix, targetIdentifier=targetIdentifier, sourceIdentifier=paste(sourceIdentifier,gsub("\\\\","",postfix),sep=""))
	} 
	return (targetDF)
}


###
#	In the case where there are more than one files for a given student this function can be used to 
#	transfer the values in one file to the corresponding row in a target dataframe.
#
transferColValues = function (targetDF, sourceDF, targetColNames="Student.Work.Part.1", sourceColNames="Student.Work", targetIdentifier="Step.Work.Id", sourceIdentifier="Step.Work.Id"){
	for (r in 1:nrow(sourceDF)){
		srow = sourceDF[r,];
		#print(sourceIdentifier)
		#print(names(sourceDF))
		if (!is.null(targetIdentifier) && !is.null(sourceIdentifier) && length(which(names(targetDF)==targetIdentifier))>0 && length(which(names(sourceDF)==sourceIdentifier))>0){
			tindices = subset(targetDF, targetDF[,targetIdentifier]%in%srow[,sourceIdentifier])$Index;	
		} else {
			tindices = subset(targetDF, Workgroup.Id%in%srow$Workgroup.Id&Wise.Id.1%in%srow$Wise.Id.1&Project.Id%in%srow$Project.Id&Step.Num%in%srow$Step.Num&Start.Time%in%srow$Start.Time&End.Time%in%srow$End.Time)$Index;	
		}
		### now attach to target
		if (length(tindices) == 0){
			print("No corresponding row in target data frame");
		} else {
			#print(paste(length(tindices), "matching rows found, using first"));
			#print(targetDF[tindices,]);
			#tindices = tindices[1];
			for (s in 1:length(sourceColNames)){
				sourceColName = sourceColNames[s]
				if (s <= length(targetColNames)){
					targetColName = targetColNames[s]
				} else {
					targetColName = sourceColName
				}
				### does the column exist in the target?
				if (sum(names(targetDF)==targetColName) == 0){
					### no, add it
					sclass = class(sourceDF[,sourceColName])
					if (sclass[1] == "numeric" || sclass[1] == "integer"){
						targetDF[,targetColName] = rep(NA, nrow(targetDF))	
					} else {
						targetDF[,targetColName] = rep("", nrow(targetDF))	
					}
				} 
				#print(class(targetDF[,targetColName]))
				#print(class(sourceDF[,sourceColName]))

				targetDF[targetDF$Index%in%tindices,which(names(targetDF)==targetColName)] = srow[1,which(names(sourceDF)==sourceColName)];	
				#$print(paste("from", srow[1,which(names(sourceDF)==sourceColName)], "to", targetDF[targetDF$Index%in%tindices,which(names(targetDF)==targetColName)]))
			}
		}
	}
	return (targetDF);
}

# In a wise data frame all steps should have Ids.
# Ids will have prefixes that designate what part of the study they encompass (e.g. "Pretest", "Postest", "Unit")
# Each Prefix is represented by three arguments, which must be put in this order
# Prefix - the label assigned to the Step.Id
# Parent.Project.Ids - the Parent Project Id of projects associated with this label
# Activity.Nums - The Activity numbers (i.e., the whole number in front of Step.Num) associated with the label.
assignStepId = function (df, prefix, Parent.Project.Ids, Activity.Nums = 1:100, ...){
	l = list(...)
	if (missing(df) || sum(names(df) == "Step.Title") == 0) stop("Not a valid data frame with a Step.Title column.")
	## add first triad to the list
	if (!missing(prefix) && !missing(Parent.Project.Ids)){
		l = c(list(prefix, Parent.Project.Ids, Activity.Nums), l)
	}
	if (length(l) %% 3 != 0) stop("Incorrect number of arguments, each prefix needs parent project ids and activity numbers")
	#if (sum(names(df) == "Step.Id") == 0) 
	df$Step.Id = gsub(" +", ".", sub("^[0-9]+.?[0-9]*.?[0-9]* ?","",df$Step.Title))
	if (length(l) > 0){
		for (i in 1:(length(l)/3)){
			prefix = l[[i * 3 - 2]]
			Parent.Project.Ids = l[[i * 3 - 1]]
			Activity.Nums = l[[i * 3]]
			df$TEMP = rep(FALSE, nrow(df))
			names(df)[which(names(df) == "TEMP")] = paste("Is",prefix,sep=".")
			# set relevant steps
			df[(df$Parent.Project.Id %in% Parent.Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), paste("Is",prefix,sep=".")] = TRUE
			df[(df$Parent.Project.Id %in% Parent.Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Id"] = paste(prefix, df[(df$Parent.Project.Id %in% Parent.Project.Ids) & (floor(as.numeric(as.character(df$Step.Num))) %in% Activity.Nums), "Step.Id"], sep=".")
		}
	}
	df$Step.Id = as.factor(df$Step.Id)
	return (df)
}

##  Applies a Condition to wise data frame based on one of two methods (for now)
##  If method is "Parent.Project.Id" then
###    Parent.Project.Id should include more than id number,  If a Wise.User.Id can
###    be found in any rows of a given Parent.Project.Id, will receive a condition associated
###    with the index of that ID in the Parent.Project.Id vector
## If method is "Workgroup.Id.mod" then
###    Condition is assigned by each Wise.User's mod of his or her Workgroup Id in the Parent Project
### Note if multiple Wise User Ids are in projects that are not the main projects (pointed to be Parent.Project.Id, e.g. pre post test), will look for first only and print an error
applyCondition = function (df, method="Parent.Project.Id", Parent.Project.Ids, Workgroup.Id.mod=1, as.data.frame.out=TRUE, VERBOSE=TRUE){
	if (length(which(class(df)=="wisedata.frame")) == 0){print("You need to use a valid wise data frame."); return (NULL)}
	df.pp = subset(df, Parent.Project.Id %in% Parent.Project.Ids);
	Condition = numeric();
	error1 = "";
	error2 = "";
	if (method=="Parent.Project.Id"){
		for (i in 1:nrow(df)){
			row = df[i,];
			if (length(which(Parent.Project.Ids == row$Parent.Project.Id[1])) > 0){
				### this row is part of one of the target parent projects
				Condition = c(Condition, which(Parent.Project.Ids == row$Parent.Project.Id[1])[1])
			} else{
				### this row is not part of one of the target parent projects
				### find a row in the parent project that has same Wise.User.Id
				id1 = row$Wise.Id.1[1];
				idf = subset(df.pp, Wise.Id.1%in%id1|Wise.Id.2%in%id1|Wise.Id.3%in%id1)
				if (nrow(idf) == 0){
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "did not complete an experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else if (length(unique(idf$Parent.Project.Id)) > 1){
					# bad, id is in more than one exp project
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "is associated with more than one experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else {
					if (!is.nan(as.numeric(as.character(row$Wise.Id.2[1]))) || !is.nan(as.numeric(as.character(row$Wise.Id.3[1])))){
						if(VERBOSE) print(paste("Wise users from workgroup",row$Workgroup.Id,"in non-experimental project has more than one member."))
						## okay so the teacher did a non-experimental project in groups (bad), but were they all at least in the same condition?
						id23 = ifelse(!is.nan(as.numeric(as.character(row$Wise.Id.2[1]))) , row$Wise.Id.2[1] , numeric())
						id23 = ifelse(!is.nan(as.numeric(as.character(row$Wise.Id.3[1]))) , c(id23, row$Wise.Id.3[1]) , id23)
						id23 = id23[!is.na(id23)]
						idf23 = subset(df.pp, Wise.Id.1%in%id23|Wise.Id.2%in%id23|Wise.Id.3%in%id23)
						if (nrow(idf23) == 0){
							Condition = c(Condition, which(Parent.Project.Ids == idf$Parent.Project.Id[1])[1])
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
							Condition = c(Condition, which(Parent.Project.Ids == idf23$Parent.Project.Id[1])[1])
						}
						
					} else {
						## Ideal, one user in non-experiment, associated with one experimental project
						Condition = c(Condition, which(Parent.Project.Ids == idf$Parent.Project.Id[1])[1])
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
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "did not complete an experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else if (length(unique(idf$Parent.Project.Id)) > 1){
					# bad, id is in more than one exp project
					Condition = c(Condition, 0);
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

######################  Summary functions ############################################
## These functions are used to turn a complete, step-by-step data.frame into summaries

###
# select.first are those columns we want to keep intact from the first row
# select.numerical are those columns where we are applying some function to get aggegrate data
# FUNS.numerical correspond to each select.mumerical
# awiseDF = :? (wiseDF, by=list(Workgroup.Id), select.first = c(Project.Id, Run.Id, Wise.Id.1, Wise.Id.2), c(Teacher.Score, Research.Score), c("sum", "mean", "sd", "median", "min", "max")); #awiseDF[150:170,]
aggregate.wisedata.frame = function (x, by = list(Workgroup.Id), select.first = c(Project.Id, Parent.Project.Id, Run.Id, Wise.Id.1, Wise.Id.2, Wise.Id.3, Condition), select.numerical, FUNS.numerical, include.median.splits = FALSE, ..., simplify = TRUE){
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
	if (!missing(select.first)){
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
		for (fun in FUNS.numerical){			
			## special for median split
			if (fun == "first"){
				adf = subset(df, URev.Num==1.000, c(by_index, vars))
				adf = subset(adf, !duplicated(adf[,b]))
				adf = merge(subset(odf,TRUE,b), adf, all.x=TRUE, all.y=FALSE, suffixes=c("",""))
			} else if (fun == "last"){
				adf = subset(df, IURev.Num==0.000, c(by_index, vars))
				adf = subset(adf, !duplicated(adf[,b]))
				adf = merge(subset(odf,TRUE,b), adf, all.x=TRUE, all.y=FALSE, suffixes=c("",""))
			} else if (fun == "first.to.last"){
				adf.f = subset(df, URev.Num==1.000, c(by_index, vars))
				adf.f = subset(adf.f, !duplicated(adf.f[,b]))
				adf = subset(df, IURev.Num==0.000, c(by_index, vars))
				adf = subset(adf, !duplicated(adf[,b]))
				adf[2:ncol(adf)] = adf[2:ncol(adf)] - adf.f[2:ncol(adf.f)] 
				adf = merge(subset(odf,TRUE,b), adf, all.x=TRUE, all.y=FALSE, suffixes=c("",""))
			} else {
				df.select.numerical = df[,c(vars)];
				adf = aggregate(df.select.numerical, by, FUN = fun, na.rm = TRUE, simplify=TRUE);
			}
			## if there is more than one numerical function attach the name of the funciton
			#if (length(FUNS.numerical) > 1){ 
			names(adf) = paste(names(df)[c(by_index,vars)],fun,sep=".") 
			adf = subset(adf,TRUE,-1*c(1:length(b)))
			if (include.median.splits){
				#print(adf)
				mdf = adf
				for (c in ncol(mdf):1){
					# only if there are more than three levels (not including NA)
					#print(c)
					#print(ncol(mdf))
					#print(mdf)
					if (length(!is.na(unique(adf[,c]))) > 3){
						mdf[,c] = adf[,c] > median(adf[,c], na.rm=TRUE)
						names(mdf)[c] = paste(names(mdf)[c],"median.split",sep=".")
					} else {
						if (!is.null(ncol(mdf))){
							mdf = mdf[,-c]
						} else {
							mdf = NULL
						}
					}
				}
				if (!is.null(mdf)) adf = cbind(adf, mdf)
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
#magna.carta.holy.grail(fake,rubrics.list=list(rubrics.NoBack))

#awise.2.3 = aggregate(wise.2.3, by = list(Workgroup.Id), select.first = c(Project.Id, Parent.Project.Id, Run.Id, Wise.Id.1, Wise.Id.2, Wise.Id.3, Condition), select.numerical = c(Research.Score, Time.Spent.Seconds, URev.Num), FUNS.numerical = c("mean","sum","first","last"), include.median.splits=TRUE)

aggregate.aggwisedata.frame = function (x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE){
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
subset.wisedata.frame = function(x, subset, select, drop = FALSE, ...){
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

### For each workgroup subsets to only those steps between the first revision (unique if given) of a step and the last revision of a step
### We can start from the first instance of a step or the last instance of a step (start.first, start.last)
subset.Step.Range = function (df, step.Num.NoBranch.first, step.Num.NoBranch.last, start.first=TRUE, start.last=FALSE, unique.first=FALSE, unique.last=TRUE){
	df.out = df[1,]
	df.out= df.out[-1,]
	for (wg in unique(df$Workgroup.Id)){
		print(paste("Workgroup", wg))
		if (unique.first){
			if (start.first){
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.first&URev.Num==1)$Index[1]	
			} else {
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.first&IURev.Num==0)$Index[1]	
			}		
		} else {
			if (start.first){
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.first&Rev.Num==1)$Index[1]
			} else {
				index.first = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.first&IRev.Num==0)$Index[1]
			}
		}

		if (unique.last){
			if (start.last){
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.last&URev.Num==1)$Index[1]	
			} else {
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.last&IURev.Num==0)$Index[1]	
			}		
		} else {
			if (start.last){
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.last&Rev.Num==1)$Index[1]
			} else {
				index.last = subset(df, Workgroup.Id==wg&Step.Num.NoBranch==step.Num.NoBranch.last&IRev.Num==0)$Index[1]
			}
		}	
		df.out = rbind(df.out, subset(df, Workgroup.Id==wg&Index>=index.first&Index<=index.last))
	}
	return (df.out)
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
getRevisionNumber.bad = function(df, inverse=FALSE, incrementIntegerOnChange=FALSE, betweenChangeIncrements=0.001){
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



getRevisionNumber = function(df, inverse=FALSE, incrementIntegerOnChange=FALSE, betweenChangeIncrements=0.001){
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

## Uses regular expression matching to find the step number in the front of a step title
getStepNum = function (df){
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

collapseStepNumBranches = function (Step.Num){
	Step.NumNB = round(10000*as.numeric(as.character(Step.Num)))
	branchActivities = 10000*floor(Step.NumNB[Step.NumNB%%100!=0]/10000) + 100
	Step.NumNB[Step.NumNB%in%branchActivities] = Step.NumNB[Step.NumNB%in%branchActivities] - 100
	## convert first step of branchActivities to zero, e.g. 5.1 to 5.0
	#levels(Step.Num) = c(levels(Step.Num),unique(as.character(branchActivities)))
	#Step.NumNB[Step.Num%in%(branchActivities+0.01)] = as.factor(as.character(as.numeric(as.character(Step.Num[Step.Num%in%(branchActivities+0.01)])) - 0.01))

	Step.NumNB[Step.NumNB%%100!=0] = 1000*floor(Step.NumNB[Step.NumNB%%100!=0]/1000) + 100*(Step.NumNB[Step.NumNB%%100!=0]%%100)
	Step.NumNB = as.factor(as.numeric(as.character(Step.NumNB/10000)))
	return (Step.NumNB)
}


### This function is used on reading excel files into data and intrepets columns as numeric or character
### These classes will not be the final classes, many will be factors, use getColClasses.final to
### set their final classes
getColClasses.read = function (colNames){
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
			grepl("Time.Spent",colName)[1] ||
			grepl("Score",colName)[1] ||
			grepl("score",colName)[1] ||
			grepl("Count",colName)[1] ||
			grepl("Rater",colName)[1] ||
			colName == "Rev.Num" ||
			colName == "IRev.Num" ||
			colName == "Step.Num"  ||
			colName == "Step.Work.Id" ||
			colName == "Index" ||
			colName == "Workgroup.Id" ||
			colName ==  "Wise.Id.1" ||
			colName ==  "Wise.Id.2" ||
			colName ==  "Wise.Id.3" ||
			colName ==  "Class.Period" ||
			colName ==  "Project.Id" ||
			colName ==  "Parent.Project.Id" ||
			colName ==  "Run.Id" ||
			colName == "Changed.Idea.Workgroup.Id"  ||
			colName == "Idea.Workgroup.Id" ||
			colName == "Idea.Id" ||
			colName == "Changed.Idea.Id" ||
			colName == "Action.Performer" ||
			colName == "Basket.Revision" || 
			colName == "Idea.X.Position" ||
			colName == "Idea.Y.Position"
		){
			colClasses = c(colClasses, "numeric");
		}else {
			colClasses = c(colClasses, "character");
		}
	}
	return (colClasses);
}

### Goes through a series of column names and converts to a column class.
getColClasses.final = function (colNames){
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
			colName == "Step.Num" ||
			colName == "Action"  ||
			colName == "Action.Performer"  ||
			colName == "Changed.Idea.Workgroup.Id"  ||
			colName == "Idea.Workgroup.Id"  ||
			colName == "Node.Type" ||
			colName == "Teacher.Login" ||
			colName == "Run.Name"  ||
			colName == "Project.Name" 
		){
			colClasses = c(colClasses, "factor");
		} else if (
			grepl("Time.Spent",colName)[1] ||
			grepl("Rev.Num",colName)[1] ||
			grepl("Score",colName)[1] ||
			grepl("score",colName)[1] ||
			grepl("Count",colName)[1] ||
			grepl("Rater",colName)[1] ||
			colName == "Index" ||
			colName == "Times.Copied" ||
			colName == "Basket.Revision" ||
			colName == "Idea.X.Position" ||
			colName == "Idea.Y.Position"
		){
			colClasses = c(colClasses, "numeric");
		} else if (
			grepl("Student.Work",colName)[1] ||
			grepl("Idea.Text", colName)[1] ||
			colName == "Teacher.Comment"
		){
			colClasses = c(colClasses, "character");
		} else {
			#if (!is.null(nrow(colNames)) && nrow(colNames) > 0){
			#	colClasses = c(colClasses, class(colNames[1,i]));
			#} else{
				colClasses = c(colClasses, "factor");
			#}
			
		}
	}
	return (colClasses);
}

setColClasses.final = function (df){
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
	return (df)
}
