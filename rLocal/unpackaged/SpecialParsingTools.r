### For creating special data frames

as.wiseSpecial <- function(obj, type, ...) UseMethod("as.wiseSpecial")
as.wiseSpecial.default <- function (obj, type, ...){return("Cannot convert input into a special data frame")}
as.wiseSpecial.wisedata.frame <- function (obj, type, ...){
	if (is.null(nrow(obj)) && length(which(names(obj) == "Step.Type")) > 0){
		print("The inputted data frame must contain column names and a step type.");
		return (NULL);
	}
	if (type == "Box2dModel.history"){
		return (as.wiseSpecial.Box2dModel.history(obj, ...))
	} else if (type == "MySystem.history"){
		return (as.wiseSpecial.MySystem.history(obj, ...))
	} else {
		return (NULL)
	}
}

####### private functions for as.wiseSpecial
as.wiseSpecial.Box2dModel.history <- function (obj, ...){
	out.df = data.frame()
	### use only box2d steps
	obj.b2d = subset(obj, Step.Type=="Box2dModel")
	# for each workgroup in obj.b2d get an initial time for the project
	wgs = unique(obj.b2d$Workgroup.Id)
	Initial.Time = numeric()
	for (wg in wgs){
		Initial.Time = c(Initial.Time,  min(sapply(subset(obj, Workgroup.Id==wg)$Post.Time.Server.Clock, function(t) as.numeric(as.POSIXlt(strptime(t, "%b %d, %Y %I:%M:%S %p")))),na.rm=TRUE))
	}
	df.wg = data.frame(Workgroup.Id = wgs, Initial.Time = Initial.Time)

	### go through steps
	for (s in 1:nrow(obj.b2d)){
		#print(s)
		row = obj.b2d[s,]
		sw = as.wiseSW(row)
		if (nrow(sw$history) > 0){
			table = sw$history
			info = data.frame(Workgroup.Id=rep(row$Workgroup.Id,nrow(table)), Step.Work.Id = rep(row$Step.Work.Id,nrow(table)), Step.Num = rep(row$Step.Num,nrow(table)), Step.Num.NoBranch = rep(row$Step.Num.NoBranch,nrow(table)), Post.Time.Server.Clock = rep(row$Post.Time.Server.Clock,nrow(table)), Time.Spent.Seconds = rep(row$Time.Spent.Seconds,nrow(table)))
			table = cbind(info, table)
			out.df = rbind.fill(out.df, table)
		}
	}
	out.df = out.df[!duplicated(subset(out.df,TRUE,c(Workgroup.Id, Step.Num, index, type))),]
	post = numeric()
	for (p in 1:nrow(out.df)){
		post = c(post, as.numeric(as.POSIXlt(strptime(out.df$Post.Time.Server.Clock[p], "%b %d, %Y %I:%M:%S %p"))))
	}
	#post = sapply(out.df$Post.Time.Server.Clock, function(t) as.numeric(as.POSIXlt(strptime(t, "%b %d, %Y %I:%M:%S %p"))))
	out.df$Initial.Time = merge(out.df, df.wg, by="Workgroup.Id")$Initial.Time
	out.df$Start.Time.Seconds = post - out.df$Initial.Time - out.df$Time.Spent.Seconds
	out.df$Time.From.Start = out.df$Start.Time.Seconds + out.df$time

	return (out.df)
}
#box2d.history = as.wiseSpecial(wise, type = "Box2dModel.history")

#### Provides a data frame of MySystem links
#### rubricsList is a list of rubrics to auto-score each individual revision of the diagram 
#### 	this may be important to do because we typically only score the final revision for a step
#### extra.columns are simply columns from the original data frame that should be repeated in the output data frame
####   (if you only care about a single score per visit, may be better to score elsewhere and use Research.Score as an extra column)
as.wiseSpecial.MySystem.history <- function (obj, rubricsList = list(), extra.columns = c("Step.Num", "Rev.Num", "URev.Num"), ...){
	out.df = data.frame()
	### use only box2d steps
	obj.b2d = subset(obj, Step.Type=="Mysystem2")
	# for each workgroup in obj.b2d get an initial time for the project
	wgs = unique(obj.b2d$Workgroup.Id)
	Initial.Time = numeric()
	for (wg in wgs){
		Initial.Time = c(Initial.Time,  min(sapply(subset(obj, Workgroup.Id==wg)$Post.Time.Server.Clock, function(t) as.numeric(as.POSIXlt(strptime(t, "%b %d, %Y %I:%M:%S %p")))),na.rm=TRUE))
	}
	df.wg = data.frame(Workgroup.Id = wgs, Initial.Time = Initial.Time, stringsAsFactors = FALSE)
	
	### go through steps
	for (s in 1:nrow(obj.b2d)){
		#print(s)
		row = obj.b2d[s,]
		
		## if this step was seen before get the last revision number and add to it.
		sindex <- 1
		if (nrow(out.df) > 0){
			obj.prev <- subset(out.df, Workgroup.Id==row$Workgroup.Id&Step.Id==row$Step.Id)
			if (nrow(obj.prev) > 0) sindex <- max(obj.prev$Revision.Num, na.rm=TRUE) + 1		
		} 
		## if this step was seen before get the last unique revision number BUT DON'T ADD TO IT
		usindex <- 0
		if (nrow(out.df) > 0){
			uobj.prev <- subset(out.df, Workgroup.Id==row$Workgroup.Id&Step.Id==row$Step.Id)
			if (nrow(uobj.prev) > 0) usindex <- max(uobj.prev$URevision.Num, na.rm=TRUE)		
		} 

		swl = as.wiseSW(row)
		if (length(swl) > 0){	
			for (swi in 1:length(swl)){
				sw <- swl[[swi]]
				if (nrow(sw$table) > 0){
					table = sw$table
					if (nrow(out.df) > 0 && row$Workgroup.Id==104258 && row$Step.Id=="Unit.MySystem.with.GHGases"){
						print(paste("swi",swi,"sindex",sindex, "prev usindex", usindex))
						print("StartNode")
						print(table$StartNode)
						print(subset(out.df,Workgroup.Id==104258&Step.Id=="Unit.MySystem.with.GHGases"&URevision.Num==usindex)$StartNode)
						print(mode(all.equal(table$StartNode, subset(out.df,Workgroup.Id==104258&Step.Id=="Unit.MySystem.with.GHGases"&URevision.Num==usindex)$StartNode))=="character")
						print("EndNode")
						print(table$EndNode)
						print(subset(out.df,Workgroup.Id==104258&Step.Id=="Unit.MySystem.with.GHGases"&URevision.Num==usindex)$EndNode)
						print(mode(all.equal(table$EndNode, subset(out.df,Workgroup.Id==104258&Step.Id=="Unit.MySystem.with.GHGases"&URevision.Num==usindex)$EndNode))=="character")
						print("Link")
						print(table$Link)
						print(subset(out.df,Workgroup.Id==104258&Step.Id=="Unit.MySystem.with.GHGases"&URevision.Num==usindex)$Link)
						print(mode(all.equal(table$Link, subset(out.df,Workgroup.Id==104258&Step.Id=="Unit.MySystem.with.GHGases"&URevision.Num==usindex)$Link))=="character")
					}
					# is this table the same as the previous unique revision?
					usindex.prev <- usindex
					if (usindex == 0) { # this is the first revision
						usindex <- 1
					} else {
						if ( mode(all.equal(table$StartNode, subset(out.df,Workgroup.Id==row$Workgroup.Id&Step.Id==row$Step.Id&URevision.Num==usindex)$StartNode))=="character" || mode(all.equal(table$EndNode, subset(out.df,Workgroup.Id==row$Workgroup.Id&Step.Id==row$Step.Id&URevision.Num==usindex)$EndNode)) == "character" || mode(all.equal(table$Link, subset(out.df,Workgroup.Id==row$Workgroup.Id&Step.Id==row$Step.Id&URevision.Num==usindex)$Link)) == "character" ){
							usindex <- usindex + 1
						} else {
							usindex <- NA
						}					
					}
					if (nrow(out.df) > 0 && row$Workgroup.Id==104258 && row$Step.Id=="Unit.MySystem.with.GHGases"){
						print(paste("new usindex", usindex))
						print("------------------------------")
					}
					info = data.frame(Workgroup.Id=rep(row$Workgroup.Id,nrow(table)), Step.Work.Id = rep(row$Step.Work.Id,nrow(table)), Step.Id = rep(row$Step.Id,nrow(table)),  Revision.Num = rep(sindex,nrow(table)), URevision.Num = rep(usindex,nrow(table)), IRevision.Num = rep(0,nrow(table)), IURevision.Num = rep(0,nrow(table)), Post.Time.Server.Clock = rep(row$Post.Time.Server.Clock,nrow(table)), Time.Spent.Seconds = rep(row$Time.Spent.Seconds,nrow(table)))
					if (is.na(usindex)) usindex <- usindex.prev
					# score this revision
					for (rubi in 1:length(rubricsList)){
						info[,names(rubricsList)[rubi]] <- score(row, score.rubrics=rubricsList[[rubi]],revision.number=swi, as.data.frame.out=FALSE)
					}

					for (col.name in extra.columns){
						if (col.name %in% names(row)){
							info[,col.name] <- row[col.name]
						}
					}
					table = cbind(info, table)
					table$feedback <- rep(sw$Auto.Feedback, nrow(table))
					out.df = rbind.fill(out.df, table)
					sindex <- sindex + 1
				}
			}
		}
	}
	### update inverse columns
	out.df$IRevision.Num <- apply(subset(out.df,TRUE,c("Workgroup.Id","Step.Id","Revision.Num")), 1, function(row){m<-max(subset(out.df,Workgroup.Id==row[1]&Step.Id==row[2])$Revision.Num, na.rm=TRUE);return(as.numeric(row[3])- m)})
	out.df$IURevision.Num <- apply(subset(out.df,TRUE,c("Workgroup.Id","Step.Id","URevision.Num")), 1, function(row){m<-max(subset(out.df,Workgroup.Id==row[1]&Step.Id==row[2])$URevision.Num, na.rm=TRUE);return(as.numeric(row[3])- m)})

	post = numeric()
	for (p in 1:nrow(out.df)){
		post = c(post, as.numeric(as.POSIXlt(strptime(out.df$Post.Time.Server.Clock[p], "%b %d, %Y %I:%M:%S %p"))))
	}
	#post = sapply(out.df$Post.Time.Server.Clock, function(t) as.numeric(as.POSIXlt(strptime(t, "%b %d, %Y %I:%M:%S %p"))))
	out.df$Initial.Time = merge(out.df, df.wg, by="Workgroup.Id")$Initial.Time
	out.df$Start.Time.Seconds = post - out.df$Initial.Time - out.df$Time.Spent.Seconds
	Neg.Time = numeric()
	for (wg in wgs){
		print(paste("Workgroup", wg, "nrow in df", nrow(subset(out.df, Workgroup.Id==wg))))
		print(subset(out.df, Workgroup.Id==wg)$Start.Time.Seconds)
		min.time <- ifelse(nrow(subset(out.df, Workgroup.Id==wg)) > 0, min(subset(out.df, Workgroup.Id==wg)$Start.Time.Seconds, na.rm=TRUE), NA)
		Neg.Time = c(Neg.Time, min.time)
	}
	df.wg$Neg.Time <- Neg.Time

	out.df$Start.Time.Seconds <- apply(subset(out.df,TRUE,c(Workgroup.Id,Start.Time.Seconds)), 1, function(row)return(as.numeric(row[2])-subset(df.wg,Workgroup.Id==row[1])$Neg.Time))
	
	return (out.df)
}
#mysys <- as.wiseSpecial(gcc, type = "MySystem.history", extra.columns = c("Condition", "Step.Num", "Rev.Num", "URev.Num"), rubricsList = list(Research.Score = rubrics.standard, C.Sun=rubrics.Sun, C.Rad=rubrics.Rad, C.ToHeat=rubrics.ToHeat,C.HeatToIR=rubrics.HeatToIr, C.Irtrap=rubrics.Irtrap,C.OzoneUV=rubrics.OzoneUV, C.HeatFromSun=rubrics.HeatFromSun, C.WarmProcess=rubrics.WarmProcess, C.OzoneDec=rubrics.OzoneSR, C.OzoneInc =rubrics.OzoneIR))

#as.wiseSpecial(row, type = "MySystem.history", extra.columns = c("Condition","Step.Num", "Rev.Num", "URev.Num"), rubricsList = list(Research.Score = rubrics.standard, C.Sun=rubrics.Sun, C.Rad=rubrics.Rad, C.ToHeat=rubrics.ToHeat,C.HeatToIR=rubrics.HeatToIr, C.Irtrap=rubrics.Irtrap,C.OzoneUV=rubrics.OzoneUV, C.HeatFromSun=rubrics.HeatFromSun, C.WarmProcess=rubrics.WarmProcess, C.OzoneDec=rubrics.OzoneSR, C.OzoneInc =rubrics.OzoneIR))