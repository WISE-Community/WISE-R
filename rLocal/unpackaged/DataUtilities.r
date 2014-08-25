#################################### PARSING TOOLS #######################################
## These functions are generic utilities not requiring wise data type
##
##


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
parseOpenResponseForChecks = function(strarr, mark, labels, header = NULL, markRightOfLabel = TRUE){
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
my.t.test <- function(x, y=NULL, paired=FALSE, ...){library(lsr);print(t.test(x=x,y=y,paired=paired,...));if(!is.null(y)){if(paired){print(paste("cohen's d:",cohensD(x=x,y=y,method="paired")))}else{print(paste("cohen's d:",cohensD(x=x,y=y)))}}else{print(paste("cohen's d:",cohensD(x=x,method="paired")))};print(paste("x mean:",mean(x,na.rm=TRUE)));print(paste("x sd:", sd(x,na.rm=TRUE)));if(!is.null(y))print(paste("y mean:", mean(y,na.rm=TRUE)));if(!is.null(y))print(paste("y sd:", sd(y,na.rm=TRUE)))}

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
regressionTableWithBeta <- function(fit, round.digits = 2){
	library(QuantPsyc)
	betas = c(NA, lm.beta(fit))
	tbl = summary(fit)$coef
	
	tbl.out = as.data.frame(cbind(cbind(tbl[,1:2], Beta=betas), tbl[,3:4]))
	tbl.out$Estimate = round(tbl.out$Estimate, round.digits)
	tbl.out[,"Std. Error"] = round(tbl.out[,"Std. Error"], round.digits)
	tbl.out$Beta = round(tbl.out$Beta, round.digits)
	tbl.out[,"t value"] = round(tbl.out[,"t value"], round.digits)
	p = tbl.out[,"Pr(>|t|)"]
	tbl.out[p < 10^-(round.digits),"Pr(>|t|)"] = round(tbl.out[p < 10^-(round.digits),"Pr(>|t|)"], digits=round.digits+1)
	tbl.out[p >= 10^-(round.digits),"Pr(>|t|)"] = round(tbl.out[p >= 10^-(round.digits),"Pr(>|t|)"], digits=round.digits)
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
