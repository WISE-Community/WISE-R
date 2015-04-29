#################################### PARSING TOOLS #######################################
## These functions are generic utilities not requiring wise data type
##
##

# mergeWorksheets will merge all worksheets named by either sheetIndices or sheetNames
# Will merge based on column name provided by "by". Make sure that this column exists in all of the target worksheets.
# As the worksheets are merged new column names are made by appending the name of the worksheet to the repeated column
# Only columns specified in "select" will be repeated, default is NULL which means to copy all columns
# Columns in select.first are repeats of the same information, and so only will be taken from the first worksheet
# intersperse TRUE means to paste repeat consecutively, FALSE means to block all columns from same worksheet    
# New: sheetIndices.embed allows - using by = "Wise.Id.1" - to find matches to any Id in an embedded sheet
#  Also, if the embed sheet has more than one row per Id, multiple columns (representing revision) will be produced
mergeWorksheetRows <- function (filename, by = "Wise.Id.1", select = NULL, select.first = character(), sheetIndices = NULL, sheetNames = NULL, select.embed = NULL, select.first.embed = character(), sheetIndices.embed = NULL, sheetNames.embed = NULL, filename.out=NULL, append=FALSE){
	#detach('package:XLConnect', unload=TRUE)
	library(xlsx)
	wb <- loadWorkbook (filename)
	sheets <- getSheets (wb)
	if (length(sheets) == 0) stop("No worksheets were loaded, check spelling of path and, if appplicable, sheet names")
	snames <- sapply(sheets,function(s)return(s$getSheetName()))
	
	if (!is.null(sheetIndices)){
		#sheetIndices <- (1:length(sheets))[(1:length(length(sheets))) %in% sheetIndices]
	} else if (!is.null(sheetNames)){
		sheetIndices <- which(snames %in% sheetNames)
	}
	obj <- data.frame()
	if (!is.null(sheetIndices)){
		## load first sheet and take selection of columns
		sheet <- sheets[[sheetIndices[1]]]
		obj <- suppressWarnings(readColumns(sheet, startColumn=1, endColumn=100,startRow=1, endRow=sheet$getLastRowNum()+1))
		if (!is.null(select)){
			select.in.sheet <- c(by, select.first, select)
			select.in.sheet <- select.in.sheet[select.in.sheet %in% names(obj)]
			obj <- obj[,select.in.sheet]
		} else {
			obj <- obj[,!grepl("^X\\.",names(obj))]
		}
		# remove rows with missing ids
		obj <- obj[!is.na(obj[,by])&nchar(as.character(obj[,by]))>0,]
		# put sheet name at end
		names(obj)[which(names(obj) != by)] <- paste(names(obj)[which(names(obj) != by)], " - ",snames[sheetIndices[1]],sep="")
		## merge subsequent worksheets
		if (length(sheetIndices) > 1){
			for (s in 2:length(sheetIndices)){
				sheet <- sheets[[sheetIndices[s]]]
				obj.temp <- suppressWarnings(readColumns(sheet, startColumn=1, endColumn=100,startRow=1, endRow=sheet$getLastRowNum()+1))
				if (!is.null(select)){
					select.in.sheet <- c(by, select)
					select.in.sheet <- select.in.sheet[select.in.sheet %in% names(obj.temp)]
					obj.temp <- obj.temp[,select.in.sheet]
				} else {
					obj.temp <- obj.temp[,!grepl("^X\\.",names(obj.temp))]
				}
				obj.temp <- obj.temp[!is.na(obj.temp[,by])&nchar(as.character(obj.temp[,by]))>0,]
				#print(nrow(obj.temp))
				if (nrow(obj.temp) > 0){
					# put sheet name at end
					names(obj.temp)[which(names(obj.temp) != by)] <- paste(names(obj.temp)[which(names(obj.temp) != by)], " - ",snames[sheetIndices[s]],sep="")
					obj <- merge (obj, obj.temp, by=by, all = TRUE, suffixes=c(paste(" - ",snames[sheetIndices[1]],sep=""), paste(" - ",snames[sheetIndices[s]],sep="")))			
				}
			}
		}
	}
	# connect with embedded
	if (!is.null(sheetIndices.embed)){
		#sheetIndices.embed <- (1:length(sheets))[(1:length(length(sheets))) %in% sheetIndices.embed]
	} else if (!is.null(sheetNames)){
		sheetIndices.embed <- which(snames %in% sheetNames)
	} 

	if (!is.null(sheetIndices.embed)){
		max.rev <- 0
		for (s in 1:length(sheetIndices.embed)){
			sheet <- sheets[[sheetIndices.embed[s]]]
			obj.temp <- suppressWarnings(readColumns(sheet, startColumn=1, endColumn=100,startRow=1, endRow=sheet$getLastRowNum()+1))
			if (!is.null(select.embed)){
				select.in.sheet <- c(by, select.first.embed, select.embed)
				select.in.sheet <- select.in.sheet[select.in.sheet %in% names(obj.temp)]
				obj.temp <- obj.temp[,select.in.sheet]
			} else {
				obj.temp <- obj.temp[,!grepl("^X\\.",names(obj.temp))]
			}
			# which indices are Wise Id?
			if (by == "Wise.Id.1"){
				indices.id <- grep("wise\\.id", tolower(names(obj.temp)))
			} else if (by == "Workgroup.Id") {
				indices.id <- which(names(obj.temp) == "Workgroup.Id")
			} else {
				stop (paste("have not implemented by =", by))
			}
			
			# get ids
			if (nrow(obj) > 0){
				ids <- obj[,by]
				ids <- ids[!is.na(ids)]
			} else {
				ids <- obj.temp[,by]
				ids <- ids[!is.na(ids)]
				ids <- ids[!duplicated(ids)]
				obj <- data.frame(TEMP = ids)
				names(obj)[1] <- by
			}
			
			# loop through ids already in main data, look for matches here
			for (id in ids){
				print(id)
				if (length(indices.id) > 1){
					obj.sub <- obj.temp[apply(obj.temp[,indices.id],1,function(x)id%in%x),]
				} else if(length(indices.id) == 1){
					obj.sub <- obj.temp[obj.temp[,indices.id] %in% id,]
				} else {
					stop("Incorrect 'by' column")
				}
				
				if (nrow(obj.sub) > 0){
					for (ri in 1:nrow(obj.sub)){
						#print(paste(ri, "out of", nrow(obj.sub)))
						# rename columns with revision numbers
						row <- obj.sub[ri,]
						names(row) <- paste(names(row),snames[[sheetIndices.embed[s]]],ri,sep=".")
						index.obj <- which(names(obj) == by)
						index.row <- indices.id[which(row[,indices.id] == id)]
						print(row[,indices.id])
						# if this a new revision add appropriate columns to obj
						if (ri > max.rev){
							obj <- merge(obj, row[0,], by.x=names(obj)[index.obj], by.y=names(row)[index.row], all = TRUE)
							max.rev <- ri
						}
						
						# now replace appropriate matching rows
						matches <- as.numeric(as.character(obj[,index.obj]))==as.numeric(as.character(id))
						matches[is.na(matches)] <- FALSE
						repl <- names(row)[!(1:ncol(row) %in% indices.id)]
						obj[matches,repl] <- row[,!(1:ncol(row) %in% indices.id)]
					}
				}
			}
		}
	}

	if (is.null(filename.out)){
		filename.out <- paste(sub("(.*)?\\..*","\\1",filename),"-merged",sub("(.*)?(\\..*)","\\2",filename),sep="")
	}
	# cleanup names
	name <- gsub("\\.", " ", names(obj))
	names(obj) <- gsub("  *"," ", name)

	write.xlsx(obj, filename.out,sheetName="Merged", append=append, row.names=FALSE)
}
# example
#mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\FINAL_Thermo-2013-pre-post-crater-background.xlsx", by = "Wise.Id.1", sheetIndices =  1:3)
#mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Ocean bottom trawling\\Spring2014\\Excel-scored\\MASTER_OBT-Spring2014-pre-post-scored.xlsx", by = "Wise.Id.1", sheetIndices =  1:4)
#mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Webb_Mitosis_KIvST.xlsx", by = "Wise.Id.1", sheetIndices =  1:2)
mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Mosteiro-Thermo-Cohort 1.xlsx", by = "Wise.Id.1", sheetIndices =  1:2, sheetIndices.embed = 3)
#mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\MASTER_OBT-Spring2014-pre-post-scored-merged_modified.xlsx", by = "Wise.Id.1", sheetIndices =  1:3, sheetIndices.embed = 4)
#mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Spoons (Embedded) Cohort 2 ST-KI 2 rows.xlsx", by = "Workgroup.Id", sheetIndices.embed = 1)
#mergeWorksheetRows(filename = "C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Coal (Embedded) Cohort 2 ST-KI 2 rows.xlsx", by = "Workgroup.Id", sheetIndices.embed = 1)

## Will score a data file stored
## TODO, expand type of input files
scoreFileWithCRATER <- function (filename, itemId, response.index = 3, strip.pattern="\\[Response\\]=", filename.out=NULL, append=FALSE, verbose=FALSE, ...){
	library(RCurl)
	filetype <- sub("(.*)?\\.(.*)","\\2",filename)
	if (is.null(filename.out)){
		filename.out <- paste(sub("(.*)?\\..*","\\1",filename),"-wCR",sub("(.*)?(\\..*)","\\2",filename),sep="")
	}
	if (filetype == "csv"){
		obj <- read.csv(filename, stringsAsFactors=FALSE, ...)
	} else {
		stop(paste(filetype, "is not supported"))
	}

	i <- 1
	strikes <- 5
	vals <- numeric()
	while (i <= nrow(obj) && strikes >= 1){
		r <- obj[i,response.index]
		tryCatch(
			{
			val <- scoreResponseWithCRATER(r,itemId, strip.pattern, verbose)
			vals <- c(vals, val)
			i = i + 1
			}, 
			error = function(e){
				print(paste("error on", i))
				strikes <- strikes - 1
				return(NULL)
			}
		)
	}
	if (length(vals) == nrow(obj)){
		obj[,paste(names(obj)[response.index],"CRater",sep="-")] <- vals
		if (filetype == "csv"){
			write.csv(obj, filename.out)
		}

		print(paste(filename.out))
	} else {
		stop("Didn't work, stopped at" , i)
	}
}
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\How does heat energy move- [A]-6700-custom-all-student-work.csv", itemId="SPOON-II", response.index=40, strip.pattern="\\[Response\\]=", verbose=TRUE)
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\How does heat energy move- [P]-6701-custom-all-student-work.csv", itemId="SPOON-II", response.index=40, strip.pattern="\\[Response\\]=", verbose=TRUE)
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\SchrieberWork-GreenRoof.csv", itemId="GREENROOF-II", response.index=15, strip.pattern="Student Response: |, Check Answer: \\[\\], CRater Score: \\[\\], CRater Feedback: \\[\\]", verbose=TRUE)
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\SchrieberWork-TadPole.csv", itemId="Tadpole", response.index=14, strip.pattern="Student Response: |, Check Answer: \\[\\], CRater Score: \\[\\], CRater Feedback: \\[\\]", verbose=TRUE)
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Nourse_Photo&CellResp.csv", itemId="GREENROOF-II", response.index=14, strip.pattern="Student Response: |, Check Answer: \\[\\], CRater Score: \\[\\], CRater Feedback: \\[\\]", verbose=TRUE)
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Nourse_Photo&CellResp-wCR.csv", itemId="Tadpole", response.index=16, strip.pattern="Student Response: |, Check Answer: \\[\\], CRater Score: \\[\\], CRater Feedback: \\[\\]", verbose=TRUE)
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\GraphingInventory\\Fall2014\\CGI_VijayItem.csv", "Vijay", response.index = 4, strip.pattern="")
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Webb_Tadpole.csv", itemId="Tadpole", response.index=14, strip.pattern="")
#scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\Webb_GreenRoof.csv", itemId="GREENROOF-II", response.index=14, strip.pattern="Student Response: |, Check Answer: \\[\\], CRater Score: \\[\\], CRater Feedback: \\[\\]")
scoreFileWithCRATER(filename="C:\\Users\\Jonathan Vitale\\Documents\\DataAnalysis\\WISE\\Scoring\\DL_GreenRoof.csv", itemId="GREENROOF-II", response.index=6, strip.pattern="Student Response: |, Check Answer: \\[\\], CRater Score: \\[\\], CRater Feedback: \\[\\]")

###
#   In some cases we have used an open response to accomodate multiple choice
#   by asking students to put an x to all that apply.  This will find those
#   strings to the left or right of the check.
#   strarr - is the array of strings to be processed
#   mark - what the student is supposed to put as a mark (e.g. x)
#   labels - a list of the possible labels being checked
#   header - any string we want to remove from the beginning of the string, "Eg check all that apply"
#   markRightOfLabel - boolean - should we be looking for the mark on the right side of a label?
#      #TODO implement left of label
#   Will return an array of strings where marked labels are separated by commas:
#   e.g.
#   school x
#   library
#   homeX
#   Returns "school, home"
###
parseOpenResponseForChecks <- function(strarr, mark, labels, header = NULL, markRightOfLabel = TRUE){
	strarr.out = character();
	strarr = sub(header, "", strarr)
	for (str in strarr){
		if (markRightOfLabel){
			arr = strsplit(paste(str,"TAIL"),mark)[[1]]
			if (length(arr) > 1){
				# remove last item
				arr = arr[1:(length(arr)-1)]
				# iterate through each label looking for its place in
				# array, if two in multiple place in arr take second
				labelArr = rep("", length(arr))
				indexArr = rep(-1, length(arr))
				for (l in 1:length(labels)){
					label = labels[l]
					sindexes = regexpr(label, arr)
					if (length(which(sindexes > 0)) > 0){
						index = which(sindexes > 0)[1]
						if (indexArr[index] < sindexes[index]){
							indexArr[index] = sindexes[index]
							labelArr[index] = label
						}
					}
				}
				labelArr = labelArr[labelArr != ""]
			} else {
				labelArr = ""
			}
		}
		strarr.out = c(strarr.out, paste(labelArr, collapse = ","))
	}
	return(strarr.out)
}

# report cohen's d, m, sd
my.t.test <- function(x, y=NULL, paired=FALSE, ...){
	library(lsr);
	library(plotrix)
	print(t.test(x=x,y=y,paired=paired,...))
	if(!is.null(y)){
		if(paired){
			print(paste("cohen's d:",round(cohensD(x=x,y=y,method="paired"),3)))
		} else {
			print(paste("cohen's d:",round(cohensD(x=x,y=y),3)))
		}
	} else {
		print(paste("cohen's d:",round(cohensD(x=x,method="paired"),3)))
	}
	print(paste("x: mean: ",round(mean(x,na.rm=TRUE),3), " sd: ", round(sd(x,na.rm=TRUE),3), " se: ", round(std.error(x,na.rm=TRUE),3)))
	if (!is.null(y)){
		print(paste("y: mean: ",round(mean(y,na.rm=TRUE),3), " sd: ", round(sd(y,na.rm=TRUE),3), " se: ", round(std.error(y,na.rm=TRUE),3)))
	} 
}

### my.kappa.files will load columns of data from files and use cohen's kappa to find inter-rater agreement
### If files, sheetnames, or colnames contain more than one element, columns will be stacked vertically
###  (for example, may include pretest and posttest in single calculation)
###  there should be the same number of columns for r1 and r2
### Each sheetIndex will be searched in each File, Each ColName will be searched in each Sheet
### There should be one stepIdentifier for each colName
my.kappa.files <- function (r1.files, r2.files, r1.sheetIndices, r2.sheetIndices, r1.colNames, r2.colNames, weight = "squared", stepIdentifiers = "Step.Work.Id"){
	if (missing(r2.files)) r2.files <- r1.files
	if (missing(r2.sheetIndices)) r2.sheetIndices <- r1.sheetIndices
	if (missing(r2.colNames)) r2.colNames <- r1.colNames

	r1 <- data.frame()
	r2 <- data.frame()
	for (f in 1:length(r1.files)){
		for (s in 1:length(r1.sheetIndices)){
			w1 <- read.xlsx(r1.files[f], stringsAsFactors=FALSE, keepFormulas=FALSE, colClasses="numeric", sheetIndex=r1.sheetIndices[s])
			for (n in 1:length(r1.colNames)){
				if (sum(grepl(r1.colNames[n], names(w1)))>0) {
					w1.temp <- w1[,c(grep(stepIdentifiers[n], names(w1)), grep(r1.colNames[n], names(w1)))]
					names(w1.temp)[1] = "Identifier"
					names(w1.temp)[2] = "R1"
					r1 <- rbind(r1, w1.temp)
				}
			}
			w2 <- read.xlsx(r2.files[f], stringsAsFactors=FALSE, keepFormulas=FALSE, colClasses="numeric", sheetIndex=r2.sheetIndices[s])
			for (n in 1:length(r2.colNames)) {
				if (sum(grepl(r2.colNames[n], names(w2)))>0){ 
					w2.temp <- w2[,c(grep(stepIdentifiers[n], names(w2)),grep(r2.colNames[n], names(w2))) ]
					names(w2.temp)[1] = "Identifier"
					names(w2.temp)[2] = "R2"
					r2 <- rbind(r2, w2.temp) 
				}
			}
		}
	}
	# remove duplicated rows
	r1 <- subset(r1, !duplicated(r1, fromLast=TRUE))
	r2 <- subset(r2, !duplicated(r2, fromLast=TRUE))

	df <- merge(r1, r2, by="Identifier", all=FALSE)
	fit <- kappa2(df[,2:3], weight=weight)
	
	return(fit)
}

# For some reason the regular regression table doesn't show beta values, do that here
my.summary.lm <- function(fit, round.digits = 2){
	library(QuantPsyc)
	betas = c(NA, lm.beta(fit))
	tbl = summary(fit)$coef
	if (nrow(tbl) == 1){
		one.row <- TRUE
		tbl <- rbind(tbl, c(NA, NA, NA, 0))
	} else {
		one.row <- FALSE
	}
	
	tbl.out = as.data.frame(cbind(cbind(tbl[,1:2], Beta=betas), tbl[,3:4]))
	Estimate.index <- which(colnames(tbl.out) == "Estimate")
	Std.Error.index <- which(colnames(tbl.out) == "Std. Error")
	Beta.index <- which(colnames(tbl.out) == "Beta")
	t.value.index <- which(colnames(tbl.out) == "t value")
	Pr.t.index <- which(colnames(tbl.out) == "Pr(>|t|)")
	
	tbl.out[,Estimate.index] = round(tbl.out[,Estimate.index], round.digits)
	tbl.out[,Std.Error.index] = round(tbl.out[,Std.Error.index], round.digits)
	tbl.out[,Beta.index] = round(tbl.out[,Beta.index], round.digits)
	tbl.out[,t.value.index] = round(tbl.out[,t.value.index], round.digits)
	names(tbl.out)[t.value.index] <- paste("t value [",summary(fit)$df[2],"]",sep="")
	p = tbl.out[,Pr.t.index]
	tbl.out[p < 10^-(round.digits),Pr.t.index] = round(tbl.out[p < 10^-(round.digits),Pr.t.index], digits=round.digits+1)
	tbl.out[p >= 10^-(round.digits),Pr.t.index] = round(tbl.out[p >= 10^-(round.digits),Pr.t.index], digits=round.digits)
	if (one.row) tbl.out <- tbl.out[-2,]
	return (tbl.out)
}

### returns a clean dataset where all of vars are not na
clean <- function (obj, vars=NULL){
	if (is.null(vars)) vars = names(obj)
	vars.numeric <- logical()
	for (v in 1:length(vars)) vars.numeric <- c(vars.numeric, is.numeric(obj[,vars[v]]))
	vars <- vars[vars.numeric]
	cleanrows = as.logical(!apply(subset(obj,TRUE,vars),1,function(x)sum(is.na(x))))
	return(obj[cleanrows,])
}
