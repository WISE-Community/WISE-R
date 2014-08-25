### Old version
magna.carta.holy.grail.wisedata.frame = function (obj, by = "Workgroup.Id", rubrics.list = list(), select.numerical = sapply(grep("URev\\.Num", names(which(sapply(obj,class) == "numeric")),invert=TRUE,value=TRUE),as.name), step.identifier = "Step.Id",  FUNS.numerical = c("sum", "mean", "max", "first", "last", "first.to.last"), total.names.pretest = c("KI\\.Score\\.max\\.Pretest"), total.names.posttest =c("KI\\.Score\\.max\\.Posttest"), run.statistics=FALSE, include.median.splits=TRUE, include.partner.scores = TRUE, total.zero.is.na=TRUE,...){
	### every combination of steps with column values
	if (class(select.numerical) == "list"){
		# convert to vector of strings
		ColName = as.character(select.numerical[-1])
	} else {
		ColName = as.character(substitute(select.numerical))[-1]
	}
	
	#Step.Num.NoBranch = unique(obj$Step.Num.NoBranch)
	Step.Id = unique(obj[,step.identifier])

	## score based on rubrics
	if (length(rubrics.list)>0){
	   	for (rub in 1:length(rubrics.list)){
	   		rubrics = rubrics.list[[rub]]
	   		obj$TEMPSCORE = score(obj, score.rubric = rubrics, is.data.frame.out=FALSE)
	   		if (!is.null(rubrics$Title)){
				names(obj)[which(names(obj)=="TEMPSCORE")] = paste("Score",rubrics$Title,sep=".")
				ColName = c(ColName, paste("Score",rubrics$Title,sep="."))
				select.numerical = c(select.numerical, as.name(paste("Score",rubrics$Title,sep=".")))
			} else {
				names(obj)[which(names(obj)=="TEMPSCORE")] = paste("Score",rub,sep=".")
				ColName = c(ColName, paste("Score",rub,sep="."))
				select.numerical = c(select.numerical, as.name(paste("Score",rub,sep=".")))
			}		
	   	}
	 }

	## create an aggregate data frame
	# go though each step to make individual aggregrate data frame
	if (grepl("Wise.Id", by)){
		obj = expandWorkgroupToWiseId (obj)
		by = "Wise.Id.1"
		by.asName = as.name("Wise.Id.1")
	} else if (by == "Workgroup.Id"){
		by.asName = as.name("Workgroup.Id")
	} else {
   		## TODO by Wise.Id
   		return (NULL)
   	}
	agg = data.frame()
	for (s in 1:length(Step.Id)){
		step.id = Step.Id[s]
		#print(paste("step id",step.id))
		obj.step = subset(obj, obj[,step.identifier] == step.id)
		#present.name = paste(step.id,"Present",sep=".")
		#print(step.id)
		if (by == "Workgroup.Id") {
			agg2 = suppressWarnings(aggregate(obj.step,by=list(Workgroup.Id),select.first=c(Condition),select.numerical=lapply(ColName,as.name), FUNS.numerical=FUNS.numerical, include.median.splits=include.median.splits))
		} else if (by == "Wise.Id.1"){
			print(lapply(ColName,as.name))
			agg2 = suppressWarnings(aggregate(obj.step,by=list(Wise.Id.1),select.first=c(Condition),select.numerical=lapply(ColName,as.name), FUNS.numerical=FUNS.numerical, include.median.splits=include.median.splits))
		}
		names(agg2)[2] = "Condition"
		#names(agg2)[2] = "Teacher.Login"
		agg2[,1] = as.factor(agg2[,1])
		agg2[,2] = as.factor(agg2[,2])
		#agg2[,3] = as.factor(agg2[,3])
		
		if (s == 1){
			agg = agg2
		} else if (s == 2){
			agg = merge(agg, agg2, by = c(by, "Condition"), suffixes = c(paste(".",step.id.prev,sep=""),paste(".",step.id,sep="")), all=TRUE)
		} else {
			names(agg2)[3:ncol(agg2)] = paste(names(agg2)[3:ncol(agg2)],".",step.id,sep="")
			agg = merge(agg, agg2, by = c(by, "Condition"), suffixes = c("",""), all=TRUE)
		}
		step.id.prev = step.id
	}
	## add the workgroup and wise id's of partners from unit (must be only one unit- TODO: do for multiple units)
	obj.unit = subset(obj, Is.Unit==TRUE)
	if (nrow(obj.unit) > 0){
		if (by == "Workgroup.Id") {
			agg = merge(subset(obj.unit,!duplicated(Workgroup.Id),c("Workgroup.Id","Wise.Id.1","Wise.Id.2","Wise.Id.3", "Teacher.Login")), agg, by="Workgroup.Id", all.x=FALSE, all.y=TRUE)
		} else if (by == "Wise.Id.1") {
			agg = merge(subset(obj.unit,!duplicated(Wise.Id.1),c("Wise.Id.1","Workgroup.Id","Wise.Id.2","Wise.Id.3", "Teacher.Login")), agg, by="Wise.Id.1", all.x=FALSE, all.y=TRUE)
		}
		rm(obj.unit)
	}
	
	### apply linearity measures
	if (by == "Workgroup.Id") {
		agg = merge(agg, scoreNavigation.linearity(obj, by.Time = TRUE, by.Workgroup.Id = TRUE, as.data.frame.out=TRUE), by = c(by), suffixes = c("",""), all=TRUE)
		agg = merge(agg, scoreNavigation.linearity(obj, by.Time = FALSE, by.Workgroup.Id = TRUE, as.data.frame.out=TRUE), by = c(by), suffixes = c("",""), all=TRUE)
	} else if (by == "Wise.Id.1"){
		agg = merge(agg, scoreNavigation.linearity(obj, by.Time = TRUE, by.Workgroup.Id = FALSE, as.data.frame.out=TRUE), by = c(by), suffixes = c("",""), all=TRUE)
		agg = merge(agg, scoreNavigation.linearity(obj, by.Time = FALSE, by.Workgroup.Id = FALSE, as.data.frame.out=TRUE), by = c(by), suffixes = c("",""), all=TRUE)
	}
	#print("pre linearity")
	if(include.median.splits) agg$Linearity.by.Time.median.split = agg$Linearity.by.Time >= median(agg$Linearity.by.Time)
	if(include.median.splits) agg$Linearity.by.Step.median.split = agg$Linearity.by.Step >= median(agg$Linearity.by.Step)
	### create gain scores (find matching pre and post test items)
	names.post = grep("Posttest",names(agg),value=TRUE)
	names.pre = grep("Pretest", names(agg),value=TRUE)
	names.post = names.post[sub(".Posttest","",names.post) %in% sub(".Pretest","",names.pre)]
	
	# create new colunn for each match
	for (n in names.post){	
		agg[ ,sub(".Posttest",".Gain",n)] = 
		tryCatch({agg[,n] - agg[ ,sub(".Posttest",".Pretest",n)]}, error=function(e){
			print(n);
			print(sub(".Posttest",".Pretest",n));
			return (NULL)
			}
		)
	}
	# get totals and gains
	if (by == "Wise.Id.1") {
		## get a pretest total
		pretest.i <- numeric(); for (n in total.names.pretest) pretest.i = union(pretest.i, grep(n,names(agg)))
		
		### get max score assuming that at least one person got top score one each item
		if (length(pretest.i) == 1){
			pretest.items.max = max(agg[,pretest.i], na.rm=TRUE)
			pretest.score.max = sum(pretest.items.max, na.rm=TRUE)
			pretest.total = agg[,pretest.i]
		} else {
			pretest.items.max = as.numeric(apply(agg[,pretest.i], 2, max, na.rm=TRUE))
			pretest.items.max[!is.finite(pretest.items.max)] = NA
			pretest.score.max = sum(pretest.items.max, na.rm=TRUE)
			pretest.total = apply(agg[,pretest.i], 1,sum,na.rm=TRUE)
		}
		agg$Total.Score.Pretest = pretest.total
		if (total.zero.is.na) agg$Total.Score.Pretest[agg$Total.Score.Pretest==0] = NA
		agg$Percent.Score.Pretest = agg$Total.Score.Pretest / pretest.score.max
		if(include.median.splits) agg$Total.Score.Pretest.median.split = agg$Total.Score.Pretest >= median(agg$Total.Score.Pretest,na.rm=TRUE)

		## get a posttest total
		posttest.i = numeric(); for (n in total.names.posttest) posttest.i = union(posttest.i, grep(n,names(agg)))
		
		### get max score assuming that at least one peoston got top score one each item
		if (length(posttest.i) == 1){
			posttest.items.max = max(agg[,posttest.i], na.rm=TRUE)
			posttest.score.max = sum(posttest.items.max, na.rm=TRUE)
			posttest.total = agg[,posttest.i]
		} else {
			posttest.items.max = as.numeric(apply(agg[,posttest.i], 2, max, na.rm=TRUE))
			posttest.items.max[!is.finite(posttest.items.max)] = NA
			posttest.score.max = sum(posttest.items.max, na.rm=TRUE)
			posttest.total = apply(agg[,posttest.i], 1,sum,na.rm=TRUE)
		}
		agg$Total.Score.Posttest = posttest.total
		if (total.zero.is.na) agg$Total.Score.Posttest[agg$Total.Score.Posttest==0] = NA
		agg$Percent.Score.Posttest = agg$Total.Score.Posttest / posttest.score.max
		agg$Total.Score.Gain = agg$Total.Score.Posttest - agg$Total.Score.Pretest
		agg$Percent.Score.Gain = agg$Percent.Score.Posttest - agg$Percent.Score.Pretest
	} 

	### put the scores of partner in same row
	if (include.partner.scores){
		names.for.partner = names(agg)[sapply(agg,is.numeric)]
		agg = merge(agg,agg[,c("Wise.Id.2", names.for.partner)],by.x="Wise.Id.1",by.y="Wise.Id.2",suffixes=c("",".Partner"),all.x=TRUE)
		# rm identical columns
		col.rm = numeric()
		for (n in 1:length(names.for.partner)){
			name = names.for.partner[n]
			name.p = paste(name,"Partner",sep=".")
			if (sum(abs(apply(agg[,c(name,name.p)],1,diff)),na.rm=TRUE)==0) col.rm = c(col.rm,which(names(agg)==name.p))
		}
		agg = agg[,-col.rm]
	}

	# rm empty columns
	for (c in ncol(agg):6){
		#print(class(agg[,c]))
		if (sum(is.na(agg[,c]) == length(agg[,c]) || !is.finite(agg[,c])) == length(agg[,c])){
			agg = agg[,-c]
		}
	}
	# change all infinity into NA
	for (c in 4:ncol(agg))	agg[!is.finite(agg[,c]),c] <- NA 
	for (c in 4:ncol(agg)) 	agg[is.nan(agg[,c]),c] <- NA
	
	#rm empty rows
	agg = agg[agg[,1]!="NaN",]
	agg = agg[!is.na(as.numeric(as.character(agg[,1]))),]

	
	class(agg) = c("magna",class(agg))
	return(agg)
	
	if (run.statistics) analyze.magna.carta.holy.grail.magna(agg, by = by, ...)
	
   	return(agg);
}


########################## PICKUP PATTERNS ACROSS WORKGROUP SUBSETS
PatternBuilder = function (obj = NULL, pattern, ColName, Step.Num.NoBranch = NULL, Parent.Project.Id = NULL, Project.Id = NULL){
	### In the case where the pattern, ColName, Step.Num.NoBranch, and Parent.Project.Id or Project.Id are all of the same length
	###   iterate through array looking for a match in each subset of Workgroups.  Return Workgroup Id of hits.
	### In cases where the input arrays are not of equal length, concat to smaller arrays be repeating last element and continue as above.   
	# pattern: A pattern that will be checked in the associated column. Some possible uses
		# regular expressions:  e.g. "^density$""
		# logical: TRUE
		# evaluate expression (in string form): For example "x<3&x>1", will match against a numerical response
	# ColName: Apply regular expression matching these columns: Typically Student.Work or Research.Score or whatever
	# Step.Num.NoBranch: What steps to find match
	# Parent.Project.Ids: What project of find pattern in, if null (and Project.Id is null) then use first unique
	# Project.Ids: If Parent.Project.Ids is null use this to find relevant projects, else  first unique parent
	if (is.null(Parent.Project.Id)){
		if (is.null(Project.Id)){
			if (is.null(obj)){
				return (NULL)
			} else {
				Parent.Project.Id = unique(obj$Parent.Project.Id)[1]
			}
		} else {
			if (is.null(obj)){
				return (NULL)
			} else {# replace project ids with parent for consistency
				Parent.Project.Id = unique(subset(obj, Project.Id %in% Project.Id)$Parent.Project.Id)
			}
		}
	}
	Parent.Project.Id = as.numeric(as.character(Parent.Project.Id))
	if (is.null(Step.Num.NoBranch)){
		if (is.null(obj)){
			return (NULL)
		} else {
			Step.Num.NoBranch = unique(obj$Step.Num.NoBranch)[1]
		}
	}
	Step.Num.NoBranch = as.numeric(as.character(Step.Num.NoBranch))

	maxlength = max(c(length(pattern), length(ColName), length(Step.Num.NoBranch), length(Parent.Project.Id)))
	if (length(pattern) < maxlength) pattern = c(pattern, rep(tail(pattern,1), maxlength - length(pattern)))
	if (length(ColName) < maxlength) ColName = c(ColName, rep(tail(ColName,1), maxlength - length(ColName)))
	if (length(Step.Num.NoBranch) < maxlength) Step.Num.NoBranch = c(Step.Num.NoBranch, rep(as.numeric(as.character(tail(Step.Num.NoBranch,1))), maxlength - length(Step.Num.NoBranch)))
	if (length(Parent.Project.Id) < maxlength) Parent.Project.Id = c(Parent.Project.Id, rep(as.numeric(as.character(tail(Parent.Project.Id,1))), maxlength - length(Parent.Project.Id)))
	
	out = list(pattern = pattern, ColName = ColName, Step.Num.NoBranch=Step.Num.NoBranch, Parent.Project.Id = Parent.Project.Id)
	class(out) = c("Pattern" , class(out))
	return (out)
}

patternDetector = function (obj, Pattern, ...) UseMethod ("patternDetector");
patternDetector.default = function (obj, ...){return(NULL);}
patternDetector.wisedata.frame = function (obj, Pattern, ...){
	if (class(Pattern)[1] != "Pattern") return (NULL);
	### Searches specific pattern in any sequence of columns, steps, project
	
	Workgroup.Id = unique(obj$Workgroup.Id)
	out = logical()

	for (w in 1:length(Workgroup.Id)){
		wid = Workgroup.Id[w]
		obj.wg = subset(obj, Workgroup.Id == wid)
		currow = 1
		
		for (i in 1:length(Pattern$pattern)){
			hit = FALSE
			pattern = Pattern$pattern[i];
			colname = Pattern$ColName[i];
			step.num.nobranch = Pattern$Step.Num.NoBranch[i];
			parent.project.id = Pattern$Parent.Project.Id[i];
			while (currow < nrow(obj.wg)){
				row = obj.wg[currow,]
				if (row$Step.Num.NoBranch == step.num.nobranch && row$Parent.Project.Id == parent.project.id){
					x = subset(row, TRUE, colname)[,1]
					if ( grepl(pattern, x) || 
						 (is.logical(pattern) && pattern == x) ||
						  eval(parse(text=paste(pattern, collapse="+")))
						 ){
						hit = TRUE
						break
					} else {
						currow = currow + 1
					}
				} else{
					currow = currow + 1
				}
			}
			# did we break from this while because we hit the end or because we got a hit?
			if (!hit){
				break;
			}
		}
		## if we had a hit at the end then add this workgroup 
		if (hit){
			out = c(out, TRUE)
		} else {
			out = c(out, FALSE)
		}
	}
	return (out)
}


