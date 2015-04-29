
########################## SCORING NAVIGATION
scoreNavigation.linearity <- function (df, Step.Num.NoBranch=sort(unique(df$Step.Num.NoBranch)), by.Workgroup.Id=TRUE, by.Time=FALSE, as.data.frame.out = TRUE, ylim=c(as.numeric(as.character(Step.Num.NoBranch[1])), as.numeric(as.character(tail(Step.Num.NoBranch,1))))){
	object = as.list(substitute(list(...)))[-1L]
	### make sure there is a single individual
	Step.Count.NoBranch = 1:length(Step.Num.NoBranch)
	
	if (by.Workgroup.Id){
		W.Id = unique(df$Workgroup.Id)
	} else {
		W.Id = as.numeric(as.character(unique(union(union(df$Wise.Id.1, df$Wise.Id.2), df$Wise.Id.3))))
		W.Id = W.Id[!is.na(W.Id)]
	}
	Linearity = numeric()
	for (wid in W.Id){
		if (by.Workgroup.Id){
			sdf = subset(df, Workgroup.Id == wid)
		} else {
			sdf = subset(df, Wise.Id.1 == wid | Wise.Id.2 == wid | Wise.Id.3 == wid)
		}
		#print(paste("id", wid))
		#print(paste("nrow",nrow(sdf)))
		#print(length(sdf$Time.Spent.Seconds))
		no.vals = FALSE
		if (by.Time){
			sdf$Time.Spent.Seconds[is.na(sdf$Time.Spent.Seconds)] = 0
			ctime = cumsum(sdf$Time.Spent.Seconds)
			if (length(ctime) == 0){
				no.vals = TRUE; break;
			}
			xlim = c(0, tail(ctime,1))
			xvals = 0:tail(ctime,1)
			xvals.inc = c(0,ctime)
			yvals.inc = c(match(sdf$Step.Num.NoBranch,Step.Num.NoBranch), tail(match(sdf$Step.Num.NoBranch,Step.Num.NoBranch),1))
			x = match(xvals, xvals.inc)
			if(is.na(x[1]))x[1] = 0
			for (i in 1:length(x))if(is.na(x[i]))x[i]=x[i-1]
			yvals = yvals.inc[x]
		} else {
			xvals = 1:length(sdf$Step.Num)
			yvals = match(sdf$Step.Num.NoBranch,Step.Num.NoBranch)
		}
		### translate from step number to step count
		if (no.vals){
			Linearity = c(Linearity, NA)
		} else {
			Linearity = c(Linearity, summary(lm(yvals ~ xvals))$r.squared)
		}
		
	}
	if (as.data.frame.out){
		if (by.Workgroup.Id){
			if (by.Time){
				return (data.frame(Workgroup.Id = W.Id, Linearity.by.Time=Linearity))	
			} else {
				return (data.frame(Workgroup.Id = W.Id, Linearity.by.Step=Linearity))	
			}
		} else {
			if (by.Time){
				return (data.frame(Wise.Id.1 = W.Id, Linearity.by.Time=Linearity))
			} else {
				return (data.frame(Wise.Id.1 = W.Id, Linearity.by.Step=Linearity))
			}
		}
	} else {
		return (Linearity)
	}
}

############## WARNING: BELOW THERE BE DRAGONS ################################
##############
############################
################
##########################################
### total.zero.is.na: for pretest and posttest totals that are zero probably represent no test being completed, replace with NA
magna.carta.holy.grail = function (obj, ...) UseMethod ("magna.carta.holy.grail");
magna.carta.holy.grail.default = function (obj, ...){return("N/A");}
magna.carta.holy.grail.wisedata.frame = function (obj, by = "Workgroup.Id", select.numerical = sapply(grep("URev\\.Num", names(which(sapply(obj,class) == "numeric")),invert=TRUE,value=TRUE),as.name), step.identifier = "Step.Id",  FUNS.numerical = c("sum", "mean", "max", "first", "last", "first.to.last"), find.gains = FALSE, total.names.pretest = NULL, total.names.posttest = NULL, run.statistics=FALSE, include.median.splits=TRUE, include.partner.scores = TRUE, total.zero.is.na=TRUE,...){
	### every combination of steps with column values
	if (class(select.numerical) == "list"){
		# convert to vector of strings
		ColName = unique(as.character(select.numerical))
	} else {
		ColName = unique(as.character(substitute(select.numerical)))
	}
	
	#Step.Num.NoBranch = unique(obj$Step.Num.NoBranch)
	Step.Id = unique(obj[,step.identifier])

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
   	# expand columns
   	obj <- expandMultipleResponses(obj)
   	
	agg = data.frame()
	for (s in 1:length(Step.Id)){
		step.id = Step.Id[s]
		#print(step.id)
		obj.step = subset(obj, obj[,step.identifier] == step.id)
		if (by == "Workgroup.Id") {
			agg2 = suppressWarnings(aggregate(obj.step,by=list(Workgroup.Id),select.first=c("Condition","Teacher.Login"),select.numerical=lapply(ColName,as.name), FUNS.numerical=FUNS.numerical, include.median.splits=include.median.splits))
		} else if (by == "Wise.Id.1"){
			agg2 = suppressWarnings(aggregate(obj.step,by=list(Wise.Id.1),select.first=c("Condition","Teacher.Login"),select.numerical=lapply(ColName,as.name), FUNS.numerical=FUNS.numerical, include.median.splits=include.median.splits))
		}
		names(agg2)[2] = "Condition"
		names(agg2)[3] = "Teacher.Login"
		agg2[,1] = as.factor(agg2[,1])
		agg2[,2] = as.factor(agg2[,2])
		agg2[,3] = as.factor(agg2[,3])
		
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
			agg = merge(subset(obj.unit,!duplicated(Workgroup.Id),c("Workgroup.Id","Wise.Id.1","Wise.Id.2","Wise.Id.3")), agg, by="Workgroup.Id", all.x=FALSE, all.y=TRUE)
		} else if (by == "Wise.Id.1") {
			agg = merge(subset(obj.unit,!duplicated(Wise.Id.1),c("Wise.Id.1","Workgroup.Id","Wise.Id.2","Wise.Id.3")), agg, by="Wise.Id.1", all.x=FALSE, all.y=TRUE)
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
	if(include.median.splits) {
		agg$Linearity.by.Time.median.split = agg$Linearity.by.Time >= median(agg$Linearity.by.Time)
		agg$Linearity.by.Step.median.split = agg$Linearity.by.Step >= median(agg$Linearity.by.Step)
	}

	if (find.gains){
		### create gain scores (find matching pre and post test items)
		names.post = grep("Posttest",names(agg),value=TRUE)
		names.pre = grep("Pretest", names(agg),value=TRUE)
		names.post = names.post[sub(".Posttest","",names.post) %in% sub(".Pretest","",names.pre)]
		
		# create new colunn for each match
		for (n in names.post){	
			agg[ ,sub(".Posttest",".Gain",n)] = 
			tryCatch({agg[,n] - agg[ ,sub(".Posttest",".Pretest",n)]}, error=function(e){
				print(sub(".Posttest",".Pretest",n));
				return (NULL)
				}
			)
		}
	}
	if (!is.null(total.names.pretest) && !is.null(total.names.pretest)){
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
# magna <- magna.carta.holy.grail(subset(wise, Step.Id%in%c(items.pretest,items.posttest,items.unit)), by="Wise.Id.1", step.identifier = "Step.Id", select.numerical = sapply(grep("Index|Time\\.Spent\\.Seconds|Rev\\.Num|^URev\\.Num|Research\\.Score|^KI\\.|^C\\.|.*?Score|NWords",names(wise[,!grepl("((KI)|C|(Score))?.*2",names(wise))]), value=TRUE),as.name), FUNS.numerical = c("sum", "mean","min", "max", "first","last", "first.to.last") )


readForColValues.magna <- function (agg, sourceFile, sourceColNames.pattern = "Score|C\\.", by="Wise.Id.1", step.identifier="Step.Id", includes.formulas = TRUE, ...){
	if (includes.formulas) {
		sourceDF <- read.xlsx(sourceFile, stringsAsFactors=FALSE, keepFormulas=FALSE, ...)
	} else {
		sourceDF <- read.xlsx2(sourceFile, stringsAsFactors=FALSE, ...)
	}
	if (!is.null(sourceColNames.pattern) && nchar(sourceColNames.pattern) > 0){
		sourceColNames <- grep(sourceColNames.pattern, names(sourceDF), value=TRUE)	
	} else {
		sourceColNames <- naems(sourceDF)
	}
	if (!is.null(step.identifier) && step.identifier %in% sourceColNames){
		step.id <- unique(sourceDF[,step.identifier])
		step.id <- step.id[!is.na(step.id)&nchar(step.id)>0]
		if (length(step.id) == 1){
			sourceColNames <- paste(sourceColNames, step.id,sep=".")
		} else if (length(step.id) == 0){
			print(step.identifier, "is not located in this sheet")
		} else {
			print ("There are multiple", step.identifier, "in this file")
			print (step.id)			
		}		
	}
	# get source that is just target columns
	source <- sourceDF[,c(by, sourceColNames)]
	agg <- merge(agg, source, by=by)

	for (s in 1:length(sourceColNames)){


}

analyze.magna.carta.holy.grail.magna = function(agg, by = "Workgroup.Id", p.value = 0.05, median.split.only = TRUE, dependent.measures = character(), covariates = character(), show.sig.correlations = FALSE, use.dependents.as.covariates = FALSE, show.statistics = TRUE, ...){
	### will create an output data frame with significant factors
	out = data.frame(dv=character(), cv.X.condition = character(), cv=character(), dv.p.value = numeric(), cv.X.condition.p.value = numeric(), cv.p.value = numeric(), interaction.p.value = numeric()) 

    ### if there are any covariates add an "empty" covariate at the front
	covariates = c("", covariates)

	# make dependent character (in case they are column names)
	if (class(dependent.measures) == "list"){
		dependent.measures = as.character(dependent.measures)
	}
	
	## search for covariates
	dependent.measures.all = character()
	if (length(dependent.measures) > 0){
		for (d in dependent.measures){
			print(grep(d, names(agg), value=TRUE))
			dependent.measures.all = c(dependent.measures.all, grep(d, names(agg), value=TRUE))
		}
		dependent.measures = dependent.measures.all
	} 
	# remove any covariates from dependent measures
	dependent.measures = dependent.measures[!(dependent.measures %in% covariates)]
	print(dependent.measures)
	if (ncol(agg) > 1 && nrow(agg) > 1){
		### run t-test over all columns after workgroup (1) and condition (2)
		for (d in 3:ncol(agg)){
	   		### do not include median splits or completely NaN or Inf columns
	   		#if (names(agg)[d] %in% dependent.measures) print(names(agg)[d])
	   		if (!grepl("median.split",names(agg)[d]) && sum(is.na(agg[,d]),is.infinite(agg[,d])) < length(agg[,d]) && (length(dependent.measures) == 0 || names(agg)[d] %in% dependent.measures )){
		   		dv = names(agg)[d]
				if (show.statistics) print(paste("--------------------------------",dv,"-----------------------------------"))
				dv.c1 = subset(agg,agg$Condition==1)[,d]
				dv.c2 = subset(agg,agg$Condition==2)[,d]
				dv.c1[!is.finite(dv.c1)] = NA
				dv.c2[!is.finite(dv.c2)] = NA
		   		t = tryCatch({
					t.test(dv.c1, dv.c2, var.equal=TRUE)
				}, error = function(e){
					NULL
				});
				#print(paste("pre",names(agg)[d], ":  t[", t$parameter,"] = ", round(t$statistic,2), " , p = ", round(t$p.value,3),sep=""))
		   		if (!is.null(t) && !is.nan(t$p.value)){ 
			   		if (t$p.value <= p.value){
			   			if (show.statistics) print(paste(names(agg)[d], ":  t[", t$parameter,"] = ", round(t$statistic,2), " , p = ", round(t$p.value,3),sep=""))
			   			out=rbind(out, data.frame(dv=dv, cv.X.condition = "", cv="", dv.p.value = t$p.value, cv.X.condition.p.value = NA, cv.p.value = NA, interaction.p.value=NA))
			   		}
			   	}
			   	## use current column as dependent variable
			   	for (c.x in 3:ncol(agg)){
			   		cv.X.condition = names(agg)[c.x]
			   		if ((cv.X.condition %in% dependent.measures || use.dependents.as.covariates) && d != c.x && (!median.split.only || (median.split.only && (grepl("median.split",names(agg)[c.x]) || length(unique(agg[!is.na(agg[,c.x]),c.x])) <= 3))) && sum(is.na(agg[,c.x]),is.infinite(agg[,c.x])) < length(agg[,c.x])){  
						for (cv in covariates){
					   		cvi = which(names(agg) == cv)			   		
					   		#cvi = cvi[1]
					   		agg.na.rm = summary.aov.clean(agg, by=by, dv=dv, cv.X.condition = cv.X.condition, cv = cv, as.data.frame.out=TRUE)
						   	s = summary.aov.clean(agg, by=by, dv=dv, cv.X.condition = cv.X.condition, cv = cv, as.data.frame.out=FALSE)
						   	s = s[[1]][[1]]
						   		
						   	if (!is.null(s)){ 
						   		row.names(s) = gsub("agg[, c.x]",cv.X.condition,row.names(s), fixed=TRUE)
						   		ps = s$'Pr(>F)'
						   		# Check for condition, interaction for condition, and if show.sig.correlations then association with main covariate
						   		if ((length(ps) >= 3 && !is.na(ps[1]) && ps[1] <= p.value) ||
						   			(length(ps) == 4 && !is.na(ps[3]) && ps[3] <= p.value) ||
						   			(length(ps) == 5 && !is.na(ps[4]) && ps[4] <= p.value) ||
						   			(show.sig.correlations && !is.na(ps[2]) && ps[2] <= p.value)
						   		){
						   			if (nchar(cv) > 0 && length(cvi) > 0){
						   				if (show.statistics) print(paste("dv:", dv, "cv.X.condition:", cv.X.condition, "cv2:",cv))
						   				out=rbind(out, data.frame(dv=dv, cv.X.condition = cv.X.condition, cv=cv, dv.p.value = ps[1], cv.X.condition.p.value = ps[2], cv.p.value = ps[3], interaction.p.value = ps[4]))
						   			} else {
						   				if (show.statistics) print(paste("dv:", dv, "cv.X.condition:", cv.X.condition))
						   				out=rbind(out, data.frame(dv=dv, cv.X.condition = cv.X.condition, cv="", dv.p.value = ps[1], cv.X.condition.p.value = ps[2], cv.p.value = NA, interaction.p.value = ps[3]))
						   			}
						   			if (!is.na(ps[1]) && ps[1] <= p.value && (is.na(ps[3]) || (!is.na(ps[3]) && ps[3] > p.value))) {
						   				if (mean(subset(agg.na.rm, Condition == 1)[,d], na.rm=TRUE) > mean(subset(agg.na.rm, Condition == 2)[,d], na.rm=TRUE)){
						   			 		if (show.statistics) print(paste("Condition 1 had higher",dv,"than Condition 2 (controlling for",cv.X.condition,")."))
						   			 	} else {
						   			 		if (show.statistics) print(paste("Condition 2 had higher",dv,"than Condition 1 (controlling for",cv.X.condition,")."))
						   			 	}
						   			} else if (!is.na(ps[3]) && ps[3] <= p.value){
						   				### For interaction with two levels can make a precise interpretation
						   				if (sum(!is.na(unique(agg.na.rm[,c.x]))) == 2){
						   					level1 = unique(agg.na.rm[,c.x])[!is.na(unique(agg.na.rm[,c.x]))][1]
						   					level2 = unique(agg.na.rm[,c.x])[!is.na(unique(agg.na.rm[,c.x]))][2]
						   					c1l1 = subset(agg.na.rm,agg.na.rm$Condition==1&agg.na.rm[,c.x]==level1)[,d]
						   					c2l1 = subset(agg.na.rm,agg.na.rm$Condition==2&agg.na.rm[,c.x]==level1)[,d]
						   					c1l2 = subset(agg.na.rm,agg.na.rm$Condition==1&agg.na.rm[,c.x]==level2)[,d]
						   					c2l2 = subset(agg.na.rm,agg.na.rm$Condition==2&agg.na.rm[,c.x]==level2)[,d]
						   					mC1L1 = mean(c1l1, na.rm=TRUE)
						   					mC1L2 = mean(c1l2, na.rm=TRUE)
						   					mC2L1 = mean(c2l1, na.rm=TRUE)
						   					mC2L2 = mean(c2l2, na.rm=TRUE)
						   					
						   					# Formulation 1
						   					t = tryCatch({
						   						t.test(c1l1, c2l1, var.equal=TRUE, paired=FALSE)
						   					}, error = function(e){
						   						NULL
						   					});
						   					if (!is.null(t) && !is.na(t$p.value) && t$p.value < p.value){
												if (mC1L1 > mC2L1){
							   						if (show.statistics) print(paste("Where", cv.X.condition,"was",level1, ": Condition 1 had higher",dv,"than Condition 2."))
							   					} else {
							   						if (show.statistics) print(paste("Where", cv.X.condition,"was",level1, ": Condition 2 had higher",dv,"than Condition 1."))
							   					}
							   				}
							   				t = tryCatch({
						   						t.test(c1l2, c2l2, var.equal=TRUE, paired=FALSE)
						   					}, error = function(e){
						   						NULL
						   					});
							   				if (!is.null(t) && !is.na(t$p.value) && t$p.value < p.value){
							   					if (mC1L2 > mC2L2){
							   						if (show.statistics) print(paste("Where", cv.X.condition,"was",level2, ": Condition 1 had higher",dv,"than Condition 2."))
							   					} else {
							   						if (show.statistics) print(paste("Where", cv.X.condition,"was",level2, ": Condition 2 had higher",dv,"than Condition 1."))
							   					}
							   				}
						   					# Formulation 2
						   					t = tryCatch({
						   						t.test(c1l1, c1l2, var.equal=TRUE, paired=FALSE)
						   					}, error = function(e){
						   						NULL
						   					});
						   					if (!is.null(t) && !is.na(t$p.value) && t$p.value < p.value){
												if (mC1L1 > mC1L2){
							   						if (show.statistics) print(paste("For Condition 1:",dv, "was higher when",cv.X.condition,"was", level1, "than",level2))
							   					} else {
							   						if (show.statistics) print(paste("For Condition 1:",dv, "was higher when",cv.X.condition,"was", level2, "than",level1))
							   					}
							   				}
							   				t = tryCatch({
						   						t.test(c2l1, c2l2, var.equal=TRUE, paired=FALSE)
						   					}, error = function(e){
						   						NULL
						   					});
						   					if (!is.null(t) && !is.na(t$p.value) && t$p.value < p.value){
								   				if (mC2L1 > mC2L2){
							   						if (show.statistics) print(paste("For Condition 2:",dv, "was higher when",cv.X.condition,"was", level1, "than",level2))
							   					} else {
							   						if (show.statistics) print(paste("For Condition 2:",dv, "was higher when",cv.X.condition,"was", level2, "than",level1))
							   					}
							   				}
						   				} else {
						   					if (show.statistics) print(paste("Condition 1 and Condition 2 have different patterns of", dv,"over",cv.X.condition))
						   				}
						   			} else if (!is.na(ps[2]) && ps[2] <= p.value){
						   				if (show.statistics) print(paste(dv, "covaries with", cv.X.condition))
						   			} else {
						   				if (show.statistics) print(paste("Error", ps[1], ps[2], ps[3]))
						   			}
						   			if (show.statistics) print(s)
						   		}
						   	}
					   	}
					}
			   	}
			   	if (show.statistics) print("-----------------------------------------------------------------------------")
			}
	   	}
    }
	return (out);   	
}

summary.aov.clean = function (agg, dv, condition.measure = "Condition", conditions, cv.X.condition = "", cv = "", as.data.frame.out=FALSE, by = names(agg)[1], show.means = FALSE, report.dv.only.as.t.test = FALSE, DEBUG=FALSE){
	dv.index = which(names(agg) == dv)
	cv.X.condition.index = which(names(agg) == cv.X.condition)
	cv.index =  which(names(agg) == cv)
	if (missing(conditions) && length(which(names(agg) == condition.measure))>0){
		conditions = sort(unique(agg[,condition.measure]))
		print(conditions)
	} else {
		# if an invalid condition measure is supplied, then use all
		agg$ALL = TRUE
		condition.measure = "ALL"
		conditions = TRUE
	}
	
	s = tryCatch({
		# remove NA
		agg.na.rm = agg
		if(DEBUG) print(paste(nchar(cv.X.condition), length(cv.X.condition.index), nchar(cv), length(cv)))
		if (nchar(cv.X.condition) > 0 && length(cv.X.condition.index) > 0){
			if (nchar(cv) > 0 && length(cv.index) > 0){
		   		agg.na.rm = subset(agg.na.rm, !is.na(agg.na.rm[,dv.index])&!is.na(agg.na.rm[,cv.X.condition.index])&!is.na(agg.na.rm[,cv.index])&!is.infinite(agg.na.rm[,dv.index])&!is.infinite(agg.na.rm[,cv.X.condition.index])&!is.infinite(agg.na.rm[,cv.index]))
				if (show.means) print(paste("dv: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index])))
				if (show.means) print(paste("cv.X.condition: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.X.condition.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.X.condition.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.X.condition.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.X.condition.index])))
				if (show.means) print(paste("cv: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.index])))
				if (by == "Workgroup.Id"){
					if (DEBUG) print(paste("dv", "cv.X.condition", cv))
					summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]*agg.na.rm[,cv.X.condition.index]+agg.na.rm[,cv.index]+Error(Workgroup.Id),data=agg.na.rm))
			   	} else if (by == "Wise.Id.1"){
			   		if (DEBUG) print(paste("dv", "cv.X.condition", cv))
			   		summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]*agg.na.rm[,cv.X.condition.index]+agg.na.rm[,cv.index]+Error(Wise.Id.1),data=agg.na.rm))
			   	}
			} else {
				agg.na.rm = subset(agg.na.rm, !is.na(agg.na.rm[,dv.index])&!is.na(agg.na.rm[,cv.X.condition.index])&!is.infinite(agg.na.rm[,dv.index])&!is.infinite(agg.na.rm[,cv.X.condition.index]))
				if (show.means) print(paste("dv: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index])))
				if (show.means) print(paste("cv.X.condition: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.X.condition.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.X.condition.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.X.condition.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.X.condition.index])))
				if (by == "Workgroup.Id"){
					if (DEBUG) print(paste("dv", "cv.X.condition"))
					summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]*agg.na.rm[,cv.X.condition.index] +Error(Workgroup.Id),data=agg.na.rm))
				} else if (by == "Wise.Id.1"){
					if (DEBUG) print(paste("dv", "cv.X.condition"))
					summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]*agg.na.rm[,cv.X.condition.index] +Error(Wise.Id.1),data=agg.na.rm))
				}
			}
		} else {
			if (nchar(cv) > 0 && length(cv.index) > 0){
		   		agg.na.rm = subset(agg.na.rm, !is.na(agg.na.rm[,dv.index])&!is.na(agg.na.rm[,cv.index])&!is.infinite(agg.na.rm[,dv.index])&!is.infinite(agg.na.rm[,cv.index]))
				if (show.means) print(paste("dv: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index])))
				if (show.means) print(paste("cv: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],cv.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],cv.index])))
				if (by == "Workgroup.Id"){
					if (DEBUG) print(paste("dv", "cv"))
					summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]+agg.na.rm[,cv.index]+Error(Workgroup.Id),data=agg.na.rm))
			   	} else if (by == "Wise.Id.1"){
			   		if (DEBUG) print(paste("dv", "cv"))
			   		summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]+agg.na.rm[,cv.index]+Error(Wise.Id.1),data=agg.na.rm))
			   	}
			} else {
				agg.na.rm = subset(agg.na.rm, !is.na(agg.na.rm[,dv.index])&!is.infinite(agg.na.rm[,dv.index]))
				if (show.means) print(paste("dv: C1:", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index]), "C2", mean(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index]), sd(agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index])))
				if (by == "Workgroup.Id"){
					if (DEBUG) print(paste("dv"))
					if (report.dv.only.as.t.test){
						t.test(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index],agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index], var.equal=TRUE)
					} else {
						summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]+Error(Workgroup.Id),data=agg.na.rm))
					}
					
				} else if (by == "Wise.Id.1"){
					if (DEBUG) print(paste("dv"))
					if (report.dv.only.as.t.test){
						t.test(agg.na.rm[agg.na.rm[,condition.measure]==conditions[1],dv.index],agg.na.rm[agg.na.rm[,condition.measure]==conditions[2],dv.index], var.equal=TRUE)
					} else {
						summary(aov(agg.na.rm[,dv.index] ~ agg.na.rm[,condition.measure]+Error(Wise.Id.1),data=agg.na.rm))
					}
				}
			}
		}	
	}, error = function(e){
	   			NULL
	});
	if (as.data.frame.out){
		return (agg.na.rm);
	} else {
		return(s);
	}
}
summary.aov.clean(magna,"KI.Score.max.Posttest.Vijay",cv.X.condition="KI.Score.max.Prettest.Vijay",condition.measure="Teacher.Login")

#magna = magna.carta.holy.grail(magna, p.value=0.05, by="Wise.Id.1", step.identifier = "Step.Id", dependent.measures=c("KI.Score.max", "Research.Score.max.Posttest","Research.Score.max.Gain"), show.sig.correlations = FALSE)
#magna = magna.carta.holy.grail(wise.scored, run.statistics=TRUE, p.value=0.05, by="Wise.Id.1", step.identifier = "Step.Id", dependent.measures=c("KI.Score.max", "Research.Score.max.Posttest","Research.Score.max.Gain"), show.sig.correlations = FALSE)



#magna = magna.carta.holy.grail(wise.scored, by="Wise.Id.1", run.statistics = TRUE, rubrics.list = list(rubrics.NoBack, rubrics.BackInTime, rubrics.HasBack, rubrics.StartOrPressCount, rubrics.StartOrPressDuration), p.value=0.02, step.identifier = "Step.Id", dependent.measures=c("KI.Score.max.Posttest","KI.Score.max.Gain","Research.Score.max.Posttest","Research.Score.max.Gain"), show.sig.correlations = FALSE)
#magna = magna.carta.holy.grail(magna, by="Wise.Id.1", p.value=0.05, dependent.measures=c("KI.Score.max.Posttest","KI.Score.max.Gain", "Research.Score.max.Posttest","Research.Score.max.Gain"), show.sig.correlations = FALSE)


#magna = magna.carta.holy.grail(wise.scored, rubrics.list = list(rubrics.NoBack, rubrics.BackInTime, rubrics.HasBack, rubrics.StartCount, rubrics.GraphPressedDuration), p.value=0.05, by="Wise.Id.1", step.identifier = "Step.Id", dependent.measures=c("KI.Score.max", "Research.Score.max.Posttest","Research.Score.max.Gain"), show.sig.correlations = FALSE)

#magna = magna.carta.holy.grail(subset(wise.cur,Step.Num.NoBranch %in% c(2.02, 2.03, 2.04)), rubrics.list = list(rubrics.NoBack, rubrics.BackInTime, rubrics.HasBack, rubrics.StartCount, rubrics.GraphPressedDuration), p.value=0.05, dependent.measures=c("Research.Score.max"))

#magna = magna.carta.holy.grail(subset(wise.cur,Step.Num.NoBranch %in% c(2.03, 2.04)), rubrics.list = list(rubrics.HasBack), p.value=0.05, dependent.measures=c("Research.Score.max"))

#fake = data.frame(Workgroup.Id=c(1:10,1:10,1:10), Condition=rep(c(2,1),30), Research.Score=c(c(10,4,8,1,12,9,15,3,12,4),c(5,3,2,2,6,8,10,3,3,3),rep(NA,10)), Research.Score.2=c(c(5,2,4,0,6,4,7,1,6,2),rep(NA,10), c(5,3,2,2,6,8,10,3,3,3)),Time.Spent.Seconds=c(c(100,300,150,80,60,320,90,140,40,10),c(300,150,80,60,320,90,140,40,10,20),c(100,150,80,90,320,30,140,140,60,70)),Step.Num.NoBranch=c(rep(1.01,10),rep(1.02,10),rep(1.01,10)));class(fake) = c("wisedata.frame", "data.frame")
#magna.carta.holy.grail(fake, rubrics.list=list(rubrics.HasBack))

create.cor.table = function(agg, dvs, cvs, round.to=0.01, by.condition=FALSE, use.dagger=TRUE){
	conditions = sort(unique(agg$Condition))
	dvs = sort(dvs)
	if (by.condition){
		dvs.c = sort(rep(dvs,length(conditions)))
		dv.cv.cors = data.frame(matrix(rep(NA,length(cvs)*length(dvs.c)),nrow=length(cvs),ncol=length(dvs.c)));names(dv.cv.cors) = paste(dvs.c,conditions,sep=".");row.names(dv.cv.cors)=cvs
		dv.cv.cors.p.value = data.frame(matrix(rep(NA,length(cvs)*length(dvs)*length(conditions)),nrow=length(cvs),ncol=length(dvs)*length(conditions)));names(dv.cv.cors.p.value) = paste(dvs.c,conditions,sep=".");row.names(dv.cv.cors.p.value)=cvs
	} else {
		dv.cv.cors = data.frame(matrix(rep(NA,length(cvs)*length(dvs)),nrow=length(cvs),ncol=length(dvs)));names(dv.cv.cors) = dvs;row.names(dv.cv.cors)=cvs
		dv.cv.cors.p.value = data.frame(matrix(rep(NA,length(cvs)*length(dvs)),nrow=length(cvs),ncol=length(dvs)));names(dv.cv.cors.p.value) = dvs;row.names(dv.cv.cors.p.value)=cvs
	}
	c = 1
	for (d in 1:length(names(dv.cv.cors))){
		condition = conditions[c]
		if (by.condition){
			dv = dvs[ceiling(d/length(conditions))]
		} else {
			dv = dvs[1]
		}
		dv.c = names(dv.cv.cors)[d]
		col = numeric()
		col.p.value = numeric()
		for (cv in cvs){
			if (by.condition){
				df = summary.aov.clean (subset(agg, Condition==condition), dv, cv = cv, as.data.frame.out=TRUE)
			} else {
				df = summary.aov.clean (agg, dv, cv = cv, as.data.frame.out=TRUE)
			}
				
			correlation = cor.test(df[,dv],df[,cv])
			col = c(col, as.numeric(correlation$estimate))
			col.p.value = c(col.p.value, as.numeric(correlation$p.value))
		}
		dv.cv.cors[,dv.c] = col
		dv.cv.cors.p.value[,dv.c] = col.p.value
		c = (c) %% length(conditions) + 1
	}
	out = dv.cv.cors
	# first round to nearest "round.to"
	for (i in 1:ncol(dv.cv.cors)){
		out[,i] = round(out[,i] * 1/round.to) * round.to
	}
	# make character string
	for (i in 1:ncol(dv.cv.cors)){
		out[,i] = as.character(out[,i])
	}
	# apply significance levels
	# make character string
	for (i in 1:ncol(dv.cv.cors)){
		for (j in 1:nrow(dv.cv.cors)){
			if (dv.cv.cors.p.value[j,i] < 0.001) {
				out[j,i] = paste(out[j,i], "***")
			} else if (dv.cv.cors.p.value[j,i] < 0.01) {
				out[j,i] = paste(out[j,i], "**")
			} else if (dv.cv.cors.p.value[j,i] < 0.05) {
				out[j,i] = paste(out[j,i], "*")
			} else if (use.dagger && dv.cv.cors.p.value[j,i] < 0.1) {
				out[j,i] = paste(out[j,i], "\u2020")
			}
		}
	}
	return(out)
}

