library(plyr)

## Writes an excel sheet for each item that has a matching pre/post response (and alternatively, delayed)
write.wisedata.frame.prepost <- function(obj, dir.out, filename=NULL, byCondition=FALSE, pretest.identifier = "Pretest", posttest.identifier = "Posttest", delayed.identifier = NULL, extra.columns=character()){
	library(xlsx)
	# get all unique step titles
	obj.pre = subset(obj, grepl(pretest.identifier,obj$Step.Id))
	obj.post = subset(obj, grepl(posttest.identifier,obj$Step.Id))
	if (!is.null(delayed.identifier)) obj.delayed = subset(obj, grepl(delayed.identifier,obj$Step.Id))
	Step.Id.pre = unique(sub(paste(pretest.identifier,"\\.",sep=""),"",unique(obj.pre$Step.Id)))
	Step.Id.post = unique(sub(paste(posttest.identifier,"\\.",sep=""),"",unique(obj.post$Step.Id)))
	if (!is.null(delayed.identifier)) Step.Id.delayed = unique(sub(paste(delayed.identifier,"\\.",sep=""),"",unique(obj.delayed$Step.Id)))
	Step.Id = Step.Id.pre[Step.Id.pre %in% Step.Id.post]
	# we include those with pre/post but no delayed so don't remove items without delayed version
	Step.Id = gsub("\\?", "\\.", Step.Id)
	if (!is.null(delayed.identifier)) Step.Id.delayed = gsub("\\?", "\\.", Step.Id.delayed)
	append = FALSE
	if (is.null(filename)) filename = paste(paste(Step.Id,collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	for (s in 1:length(Step.Id)){
		step.title = Step.Id[s]
		print(step.title)
		if (!is.null(delayed.identifier) && step.title %in% Step.Id.delayed) {
			include.delayed <- TRUE
		} else {
			include.delayed <- FALSE
		}
		obj.pre.step = subset(obj.pre, Step.Id==paste(pretest.identifier,step.title,sep="."))
		obj.post.step = subset(obj.post, Step.Id==paste(posttest.identifier,step.title,sep="."))
		if (include.delayed) obj.delayed.step = subset(obj.delayed, Step.Id==paste(delayed.identifier,step.title,sep="."))
		
		# make sure that there are responses to this step
		indices = grep("Student.Work", names(obj.pre.step))
		if (length(unique(obj.pre.step[,indices[1]])) > 1 && length(unique(obj.post.step[,indices[1]])) > 1){
			for (i in 1:length(indices)){
				index = indices[i]
				# get student response for all student work with some actual responses
				if (length(unique(obj.pre.step[,index])) > 0){
					obj.pre.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.pre.step, Student.Work.Part = i, as.data.frame.out=FALSE)
					obj.post.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.post.step, Student.Work.Part = i, as.data.frame.out=FALSE)
					if (include.delayed) obj.delayed.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.delayed.step, Student.Work.Part = i, as.data.frame.out=FALSE)
				}		
			}
			indices = grep("Student.Response", names(obj.pre.step))
			# get only those who completed both pre and post
			obj.pre.step = subset(obj.pre.step, (IURev.Num==0) & (Wise.Id.1 %in% obj.post.step$Wise.Id.1))
			obj.post.step = subset(obj.post.step, (IURev.Num==0)& (Wise.Id.1 %in% obj.pre.step$Wise.Id.1))
			if (include.delayed) obj.delayed.step = subset(obj.delayed.step, (IURev.Num==0)& (Wise.Id.1 %in% obj.pre.step$Wise.Id.1))
			relevant.columns = c("Wise.Id.1","Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns)
			relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
			relevant.columns = c(relevant.columns, paste("Student.Response",1:length(indices),sep="."))
			## use only relevant columns
			#print(names(obj.pre.step))
			print(relevant.columns)
			obj.pre.step = subset(obj.pre.step, TRUE, relevant.columns)
			obj.post.step = subset(obj.post.step, TRUE, relevant.columns)
			if (include.delayed) obj.delayed.step = subset(obj.delayed.step, TRUE, relevant.columns)
			# merge data frames
			by <- c("Wise.Id.1","Condition", "Teacher.Login")[c("Wise.Id.1","Condition", "Teacher.Login") %in% relevant.columns]
			notby <- relevant.columns[!(relevant.columns %in% by)] 
			if (!include.delayed){
				obj.step = merge(obj.pre.step, obj.post.step, by = by, all=TRUE, suffixes=c(paste(".",pretest.identifier,sep=""),paste(".",posttest.identifier,sep="")))
				new.order = names(obj.step)[!grepl(paste(".",pretest.identifier,"|",".",posttest.identifier,sep=""),names(obj.step))]
			} else {
				# attach delayed identifier to all not by columns of delayed
				names(obj.delayed.step)[names(obj.delayed.step) %in% notby] <- paste(names(obj.delayed.step)[names(obj.delayed.step) %in% notby], delayed.identifier, sep=".")
				#print(names(obj.delayed.step))
				obj.step = merge(obj.pre.step, obj.post.step, by = by, all=TRUE, suffixes=c(paste(".",pretest.identifier,sep=""),paste(".",posttest.identifier,sep="")))
				obj.step = merge(obj.step, obj.delayed.step, by = by, all=TRUE, suffixes=c(paste(".",pretest.identifier,sep=""),paste(".",delayed.identifier,sep="")))
				new.order = names(obj.step)[!grepl(paste(".",pretest.identifier,"|",".",posttest.identifier,"|",".",delayed.identifier,sep=""),names(obj.step))]
			}
			names.pre = names(obj.step)[grepl(paste(".", pretest.identifier, sep=""),names(obj.step))]
			names.post = names(obj.step)[grepl(paste(".",posttest.identifier, sep=""),names(obj.step))]
			if (include.delayed) names.delayed = names(obj.step)[grepl(paste(".",delayed.identifier, sep=""),names(obj.step))]
			for (n in 1:length(names.pre)){
				if (!include.delayed){ new.order = c(new.order, names.pre[n], names.post[n]) }
				else {new.order = c(new.order, names.pre[n], names.post[n], names.delayed[n])}
			}
			# re-sort
			#print(new.order)
			obj.step = subset(obj.step,select=new.order)
			#remove empty columns
			for (ci in ncol(obj.step):1){
				if (is.character(obj.step[,ci])&&sum((is.na(obj.step[,ci]))|(nchar(obj.step[,ci])==0)) == nrow(obj.step)) obj.step <- obj.step[,-ci]
			}

			write.xlsx(obj.step,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=paste(step.title,"-All",sep=""), append=append)
			if (byCondition){
				for (c in unique(obj.step$Condition)){
					write.xlsx(subset(obj.step,Condition==c),paste(dir.out,filename,sep=""),sheetName=paste(step.title,"-Condition ", c,sep=""),row.names=FALSE, append=TRUE)
				}
			} 
			append = TRUE
		}
	}
	return(obj.step)
}

# wise.prepost.dylan.out <- write.wisedata.frame.prepost(wise.prepost.dylan, dir.out=dir.out, filename="GS-Fall2014-prepost-Dylan.xlsx", extra.columns=c("C.Rater.Score", "Dylan"))
#prepost.out = write.wisedata.frame.prepost(subset(wise.2013,wise.2013$Step.Id %in% c("Pretest.Temperature.and.Heat.energy", "Posttest.Temperature.and.Heat.energy","Posttest.Temperature.and.Heat.energy.explanation")), dir.out.2013, byCondition=FALSE,filename="Thermo-2013-pre-post-Heat-full.xlsx", extra.columns=c("Spoon.CRater.Score","Heat.CRater.Score"))

### This is an embedded version of the pre-post writer. Step.Id.1 corresponds to pre, Step.Id.2 corresponds to post.
### Will also provide statistics on steps between 1 and 2, including how many steps in between
### TODO: Allow function to check list of step ids to see if student visited step between step 1 and step 2
write.wisedata.frame.step.to.step <- function(obj, Step.Id.1, Step.Id.2, dir.out=NULL, Step.Ids.Intermediate=NULL, filename=NULL, byCondition=FALSE, extra.columns=character(), append=FALSE){
	library(xlsx)
	if (length(which(names(obj) == "Time.Spent.Seconds"))==0 && length(which(names(obj) == "Visit.Time.Spent.Seconds"))>0) obj$Time.Spent.Seconds <- obj$Visit.Time.Spent.Seconds
	# get all unique step titles
	obj.pre = subset(obj, Step.Id %in% Step.Id.1)
	obj.post = subset(obj, Step.Id %in% Step.Id.2)
	Step.Title.pre <- Step.Id.1
	Step.Title.post <- Step.Id.2
	Step.Title <- paste(Step.Title.pre,"to",Step.Title.post, sep=".")
	if (is.null(filename)) filename = paste(paste(Step.Title,collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))

	for (s in 1:length(Step.Id.1)){
		step.title <- Step.Title[s]
		obj.pre.step <- subset(obj.pre, Step.Id%in%Step.Id.1[s])
		obj.post.step <- subset(obj.post, Step.Id%in%Step.Id.2[s])
		obj.between.step <- subsetStepsBetween (obj, Step.Id.1[s], Step.Id.2[s])
		agg.between.step <- aggregate(obj.between.step, select.first=c("Step.Id","Condition"), select.numerical=c("Time.Spent.Seconds"), FUNS.numerical=c("sum"))
		agg.between.step <- subset(agg.between.step,TRUE,c("Workgroup.Id","Time.Spent.Seconds.sum","Collapse.Count"))
		names(agg.between.step)[2:3] <- c("Time.Between.Steps", "Visits.Between.Steps") 
		# make sure that there are responses to this step
		indices = grep("Student.Work", names(obj.pre.step))
		if (length(unique(obj.pre.step[,indices[1]])) > 1 && length(unique(obj.post.step[,indices[1]])) > 1){
			for (i in 1:length(indices)){
				index = indices[i]
				# get student response for all student work with some actual responses
				if (length(unique(obj.pre.step[,index])) > 0){
					obj.pre.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.pre.step, Student.Work.Part = i, as.data.frame.out=FALSE)
					obj.post.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.post.step, Student.Work.Part = i, as.data.frame.out=FALSE)
					#print(paste(obj.pre.step[1,paste("Student.Response",i,sep=".")]))
				}		
			}
			indices = grep("Student.Response", names(obj.pre.step))
			# get only those who completed both pre and post
			obj.pre.step = subset(obj.pre.step, (IURev.Num==0) & (Wise.Id.1 %in% obj.post.step$Wise.Id.1))
			obj.post.step = subset(obj.post.step, (IURev.Num==0)& (Wise.Id.1 %in% obj.pre.step$Wise.Id.1))
			relevant.columns = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3", "Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Prompt","Time.Spent.Seconds",extra.columns)
			relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
			relevant.columns = c(relevant.columns, paste("Student.Response",1:length(indices),sep="."))
			## use only relevant columns
			#print(relevant.columns)
			obj.pre.step = subset(obj.pre.step, TRUE, relevant.columns)
			obj.post.step = subset(obj.post.step, TRUE, relevant.columns)
			# merge data frames
			obj.step = merge(obj.pre.step, obj.post.step, by = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3", "Condition","Teacher.Login"), all=TRUE, suffixes=c(".pre",".post"))
			print(names(agg.between.step))
			obj.step <- merge(obj.step, agg.between.step, by = c("Workgroup.Id"), all.x=TRUE, all.y=FALSE)
			new.order = names(obj.step)[!grepl(".pre|.post",names(obj.step))]
			names.pre = names(obj.step)[grepl(".pre",names(obj.step))]
			names.post = names(obj.step)[grepl(".post",names(obj.step))]
			for (n in 1:length(names.pre)){
				new.order = c(new.order, names.pre[n], names.post[n])
			}
			# resort
			obj.step = subset(obj.step,select=new.order)
			if (!is.null(dir)) write.xlsx(obj.step,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=paste(step.title,"-All",sep=""), append=append)
			if (byCondition) write.xlsx(subset(obj.step,Condition==1),paste(dir.out,filename,sep=""),sheetName=paste("C1-",step.title,sep=""),row.names=FALSE, append=TRUE)
			if (byCondition) write.xlsx(subset(obj.step,Condition==2),paste(dir.out,filename,sep=""),sheetName=paste("C2-",step.title,sep=""),row.names=FALSE, append=TRUE)
			append = TRUE
		}
	}
	return (obj.step)
}
#feedback.immediate.steps = write.wisedata.frame.step.to.step(subset(wise.FT,Is.Unit==TRUE), Step.Id.1="Unit.Explain.the.Mystery", Step.Id.2="Unit.ContinentCritique", byCondition=TRUE, dir.out=dir.out, filename="PlateTectonics-Spring2014-Embedded1-FT.xlsx")

#feedback.immediate.steps = write.wisedata.frame.step.to.step(subset(wise,Is.Unit==TRUE), Step.Id.1="Unit.Explain.the.Mystery", Step.Id.2="Unit.ContinentCritique", byCondition=TRUE, dir.out=dir.out, filename="PlateTectonics-Spring2014-Embedded1.xlsx")

## Specifically aimed at steps with automated guidance and an opportunity to revise on the step.
## Looks for each round of guidance and places triplets of response, feedback, score, and then ends with final response
write.wisedata.frame.step.guidance.revisions <- function(obj, final.score.column="Research.Score", Step.Id.1=NULL, dir.out=NULL, filename=NULL, extra.columns=c()){
	library(xlsx)
	obj$Is.Unique = ifelse((obj$URev.Num * 10) %% 10 == 0, TRUE, FALSE)
	if (!is.null(Step.Id)) obj <- subset(obj, Step.Id==Step.Id.1)
	obj.exp <- expandMultipleResponses(obj)
	obj.exp$Index <- 1:nrow(obj.exp)
	obj.exp$Received.Guidance <- nchar(obj.exp$Auto.Feedback) > 0 & !is.na(obj.exp$Auto.Feedback) & obj.exp$Auto.Feedback != "NA"
	
	# remove that are not final or where there was no guidance
	relevant.columns = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3","Condition", "Teacher.Login", "Index", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns,"Received.Guidance","Student.Work.Part.1","Student.Work.Part.4","Auto.Score","Auto.Feedback",final.score.column)
			
	obj.final <- subset(obj.exp, IURev.Num==0)
	obj.final <- subset(obj.final, !duplicated(Workgroup.Id, fromLast=TRUE))
	obj.final <- subset(obj.final, !duplicated(Workgroup.Id, fromLast=TRUE), relevant.columns)
	obj.final$Received.Guidance <- ifelse(obj.final$Received.Guidance, FALSE, TRUE)
	names(obj.final)[which(names(obj.final)=="Received.Guidance")] <- "Revised.After.Final.Guidance"
	names(obj.final)[which(names(obj.final)=="Student.Work.Part.1")] <- "Response.Final"
	names(obj.final)[which(names(obj.final)==final.score.column)] <- "Score.Final"
	
	obj.guidance <- subset(obj.exp, Received.Guidance)
	obj.guidance.remain <- obj.guidance
	rev.num <- 1
	# repeatedly add columns for each revision
	repeat {
		obj.guidance.rev <- subset(obj.guidance.remain, !duplicated(Workgroup.Id))
		# get the step work id
		obj.final[,paste("Step.Work.Id",rev.num,sep=".")] <- ""
		obj.final[obj.final$Workgroup.Id %in% obj.guidance.rev$Workgroup.Id,paste("Step.Work.Id",rev.num,sep=".")] <- as.numeric(as.character(obj.guidance.rev$Step.Work.Id))
		# get the response
		obj.final[,paste("Response",rev.num,sep=".")] <- ""
		obj.final[obj.final$Workgroup.Id %in% obj.guidance.rev$Workgroup.Id,paste("Response",rev.num,sep=".")] <- studentResponse(obj.guidance.rev, as.data.frame.out=FALSE)
		# get the score
		obj.final[,paste("Score",rev.num,sep=".")] <- ""
		obj.final[obj.final$Workgroup.Id %in% obj.guidance.rev$Workgroup.Id,paste("Score",rev.num,sep=".")] <- obj.guidance.rev$Auto.Score
		# get the score
		obj.final[,paste("Feedback",rev.num,sep=".")] <- ""
		obj.final[obj.final$Workgroup.Id %in% obj.guidance.rev$Workgroup.Id,paste("Feedback",rev.num,sep=".")] <- obj.guidance.rev$Auto.Feedback
		
		# prepare for next round
		obj.guidance.remain <- subset(obj.guidance.remain, duplicated(Workgroup.Id))
		rev.num <- rev.num + 1
		if (nrow(obj.guidance.remain) == 0 || rev.num > 100){
			break
		}
	}
	if (!is.null(dir.out)){
		write.xlsx(obj.final,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=as.character(unique(obj$Step.Id)[1]))
	}
	return (obj.final)
}

write.step.record.between.steps <- function(obj, Step.Id.1, Step.Id.2, repeated.column.names=c("Step.Work.Id","Step.Title","Time.Spent.Seconds","Student.Work.Part.1"), dir.out=NULL, filename=NULL, byCondition=FALSE, extra.columns=character()){
	library(xlsx)
	# get all unique step titles
	obj.pre = subset(obj, Step.Id %in% Step.Id.1)
	obj.post = subset(obj, Step.Id %in% Step.Id.2)
	Step.Title.pre = Step.Id.1
	Step.Title.post = Step.Id.2
	Step.Title = paste(Step.Title.pre,"to",Step.Title.post, sep=".")
	append = FALSE
	if (is.null(filename)) filename = paste(paste(Step.Title,collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	for (rc in repeated.column.names){
		step.title = Step.Title[1]
		obj.pre.step = subset(obj.pre, Step.Id%in%Step.Id.1[1])
		obj.post.step = subset(obj.post, Step.Id%in%Step.Id.2[1])
		# get the first instance of the "pre" step and the last instance of the "post" step
		obj.pre.step = subset(obj.pre.step, (Rev.Num==1) & (Workgroup.Id %in% obj.post.step$Workgroup.Id))
		obj.post.step = subset(obj.post.step, (IURev.Num==0)& (Workgroup.Id %in% obj.pre.step$Workgroup.Id))
		obj.pre.step = subset(obj.pre.step, Workgroup.Id %in% obj.post.step$Workgroup.Id)
		obj.post.step = subset(obj.post.step, Workgroup.Id %in% obj.pre.step$Workgroup.Id)
		
		# iterate through each workgroup and gather all steps between pre and post.
		step.ids.between = data.frame(stringsAsFactors=FALSE)
		for (w in 1:length(unique(obj.pre.step$Workgroup.Id))) {
			wgid = unique(obj.pre.step$Workgroup.Id)[w]
			# get all steps with index starting from pre to post
			step.index.pre = subset(obj.pre.step, Workgroup.Id == wgid)$Index[1]
			step.index.post = tail(subset(obj.post.step, Workgroup.Id == wgid)$Index,1)
				
			step.ids = as.character(subset(obj,Workgroup.Id==wgid&Index>=step.index.pre&Index<=step.index.post)[, rc])
			numeric.steps = c("Step.Work.Id", "Time.Spent.Seconds")
			if (rc %in% numeric.steps) step.ids=as.numeric(step.ids)
				
			# if there are more steps make the data frame larger
			if (length(step.ids) > 0 && length(step.ids.between) == 0){
				for (x in 1:length(step.ids)){
					if (length(step.ids.between) == 0){
						step.ids.between = data.frame(V = step.ids[x], stringsAsFactors=FALSE)
					} else {
						step.ids.between = cbind(step.ids.between, V = step.ids[x], stringsAsFactors=FALSE)
					}
					names(step.ids.between)[names(step.ids.between)=="V"] = paste(rc,"V",x,sep=".")
				}
			} else if (length(step.ids) > length(step.ids.between)){
				for (x in (length(step.ids.between)+1):length(step.ids)){
					if (rc %in% numeric.steps){
						step.ids.between = cbind(step.ids.between, V = rep(NA,nrow(step.ids.between)), stringsAsFactors=FALSE)
					} else {
						step.ids.between = cbind(step.ids.between, V = rep("",nrow(step.ids.between)), stringsAsFactors=FALSE)
					}
					names(step.ids.between)[names(step.ids.between)=="V"] = paste(rc,"V",x,sep=".")
				}
				step.ids.between = rbind(step.ids.between, step.ids)
			} else if (length(step.ids) < length(step.ids.between)){
				if (rc %in% numeric.steps){
					step.ids = c(step.ids, rep(NA, length(step.ids.between) - length(step.ids)))
				} else {
					step.ids = c(step.ids, rep("", length(step.ids.between) - length(step.ids)))
				}
				step.ids.between = rbind(step.ids.between, step.ids)
			} else {
				step.ids.between = rbind(step.ids.between, step.ids)
			}
		}
		step.ids.between = cbind(Workgroup.Id = unique(obj.pre.step$Workgroup.Id),step.ids.between)

		relevant.columns = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3", "Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Prompt","Time.Spent.Seconds",extra.columns)
		relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
		obj.pre.step = subset(obj.pre.step, TRUE, relevant.columns)
		obj.post.step = subset(obj.post.step, TRUE, relevant.columns)
		# merge data frames
		obj.step = merge(obj.pre.step, obj.post.step, by = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3", "Condition","Teacher.Login"), all=TRUE, suffixes=c(".pre",".post"))
		new.order = names(obj.step)[!grepl(".pre|.post",names(obj.step))]
		names.pre = names(obj.step)[grepl(".pre",names(obj.step))]
		names.post = names(obj.step)[grepl(".post",names(obj.step))]
		for (n in 1:length(names.pre)){
			new.order = c(new.order, names.pre[n], names.post[n])
		}
		# re-sort
		obj.step = subset(obj.step,select=new.order)
		#names(obj.step)
		#names(step.ids.between)
		obj.step.ids = merge(obj.step, step.ids.between, by=c("Workgroup.Id"))
		## use only relevant columns
		#print(relevant.columns)
		
		if (!is.null(dir)){
			write.xlsx(obj.step.ids,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=paste(rc,sep=""), append=append)
		}
		if (byCondition) write.xlsx(subset(obj.step.ids,Condition==1),paste(dir.out,filename,sep=""),sheetName=paste(rc,"-Condition 1",sep=""),row.names=FALSE, append=TRUE)
		if (byCondition) write.xlsx(subset(obj.step.ids,Condition==2),paste(dir.out,filename,sep=""),sheetName=paste(rc,"-Condition 2",sep=""),row.names=FALSE, append=TRUE)
		append = TRUE
	}
	return (obj.step.ids)
}

### All of the data from a single step will be recorded (such as time.spent.seconds, student.work, etc.)
### The range of steps can be limited to beginning after a given step (Step.Id.start), or ending before another step (Step.Id.stop)
### In the case of Step.Id.start, if after.initial.start.step is TRUE then records will begin after the first appearance of the start step
###     If false, then records will begin after the last time the start step is observed
### In the case of Step.Id.stop, if before.initial.stop.step is TRUE then records will stop before the first appearance of the stop step
###     If false, then records will stop before the last appearance of the stop step.
write.step.record <- function(obj, Step.Id.target, Step.Id.start=NULL, Step.Id.stop=NULL, after.initial.start.step = TRUE, before.initial.stop.step = TRUE, repeated.column.names=c("Step.Work.Id","Time.Spent.Seconds","Student.Work.Part.1"), dir.out=NULL, filename=NULL, byCondition=FALSE, extra.columns=character(), append=FALSE){
	library(xlsx)
	# get all unique step titles
	if (!is.null(Step.Id.start)) obj.start = subset(obj, Step.Id %in% Step.Id.start)
	if (!is.null(Step.Id.stop)) obj.stop = subset(obj, Step.Id %in% Step.Id.stop)
	
	Step.Title = Step.Id.target
	if (!is.null(Step.Id.start)) Step.Title = paste(Step.Title,"after",Step.Id.start,sep="-")
	if (!is.null(Step.Id.stop)) Step.Title = paste(Step.Title,"until",Step.Id.stop,sep="-")
	if (is.null(filename)) filename = paste(Step.Title,".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	for (rc in repeated.column.names){
		obj.step = subset(obj, Step.Id == Step.Id.target)
		print(rc)
		step.ids.between = data.frame(Workgroup.Id = unique(obj.step$Workgroup.Id),stringsAsFactors=FALSE)
		for (w in 1:length(unique(obj.step$Workgroup.Id))) {
			wgid = unique(obj.step$Workgroup.Id)[w]
			# get indices for all steps in target step
			step.index = subset(obj.step, Workgroup.Id == wgid)$Index
			# if there is a start condition remove any step.index before start
			if (!is.null(Step.Id.start)){
				start.index = subset(obj, Step.Id == Step.Id.start & Workgroup.Id == wgid)$Index
				if (after.initial.start.step && length(start.index) > 0){
					step.index = step.index[step.index > min(start.index)]
				} else if (length(start.index) > 0) {
					step.index = step.index[step.index > max(start.index)]
				}
			}
			if (!is.null(Step.Id.stop)){
				stop.index = subset(obj, Step.Id == Step.Id.stop & Workgroup.Id == wgid)$Index
				if (before.initial.stop.step && length(stop.index) > 0){
					step.index = step.index[step.index < min(stop.index)]
				} else if (length(stop.index) > 0){
					step.index = step.index[step.index < max(stop.index)]
				}
			}
			#print(unique(subset(obj,Workgroup.Id==wgid)[,rc]))
			step.ids = as.character(subset(obj,Workgroup.Id==wgid & Index %in% step.index)[, rc])
			numeric.steps = c("Step.Work.Id", "Time.Spent.Seconds")
			if (rc %in% numeric.steps) step.ids=as.numeric(step.ids)
			#print(step.ids)
			
			# if there are more steps make the data frame larger
			if (length(step.ids) + 1 > ncol(step.ids.between)){
				for (x in (ncol(step.ids.between)):(length(step.ids))){
					if (rc %in% numeric.steps){
						step.ids.between = cbind(step.ids.between, V = rep(NA,nrow(step.ids.between)), stringsAsFactors=FALSE)
					} else {
						step.ids.between = cbind(step.ids.between, V = rep("",nrow(step.ids.between)), stringsAsFactors=FALSE)
					}
					names(step.ids.between)[names(step.ids.between)=="V"] = paste(rc,"V",x,sep=".")
				}
				#step.ids.between = rbind(step.ids.between, step.ids)
			} else if (length(step.ids) > 0 && length(step.ids)+1< ncol(step.ids.between)){
				if (rc %in% numeric.steps){
					step.ids = c(step.ids, rep(NA, length(step.ids.between)-1 - length(step.ids)))
				} else {
					step.ids = c(step.ids, rep("", length(step.ids.between)-1 - length(step.ids)))
				}
				#print(step.ids)
				#print(names(step.ids.between))
				#step.ids.between = rbind(step.ids.between, step.ids)
			} 

			### find row and fill in columns
			if (length(step.ids) > 0) step.ids.between[w,2:ncol(step.ids.between)] = step.ids
		}
		#step.ids.between = cbind(Workgroup.Id = unique(obj.step$Workgroup.Id),step.ids.between)

		## remove all but last row for each workgroup
		obj.step = obj.step[order(obj.step$Index, decreasing=TRUE),]
		obj.step = subset(obj.step,!duplicated(Workgroup.Id))
		obj.step = obj.step[order(obj.step$Index),]

		relevant.columns = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3", "Condition", "Teacher.Login", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id","Step.Title","Step.Prompt", "Step.Visit.Count", "Step.Revision.Count",extra.columns)
		relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
		obj.step = subset(obj.step, TRUE, relevant.columns)

		# merge data frames
		obj.step.ids = merge(obj.step, step.ids.between, by=c("Workgroup.Id"))
		if (!is.null(dir)){
			write.xlsx(obj.step.ids,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=paste(rc,sep=""), append=append)
			if (byCondition) write.xlsx(subset(obj.step.ids,Condition==1),paste(dir.out,filename,sep=""),sheetName=paste("C1-",rc,sep=""),row.names=FALSE, append=TRUE)
			if (byCondition) write.xlsx(subset(obj.step.ids,Condition==2),paste(dir.out,filename,sep=""),sheetName=paste("C2-",rc,sep=""),row.names=FALSE, append=TRUE)
		}
		append = TRUE
	}
	return (obj.step.ids)
}

write.wisedata.frame.allsteps <- function(obj, dir.out, filename=NULL, byCondition=FALSE, extra.columns=character()){
	library(xlsx)
	# get all unique step titles
	Step.Id = gsub("\\?", "\\.", unique(obj$Step.Id))
	append = FALSE
	if (is.null(filename)) filename = paste(paste(Step.Id,collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	sheetnames = character()
	for (s in 1:length(Step.Id)){
		step.title = Step.Id[s]
		obj.step = subset(obj, Step.Id == step.title)
		# make sure that there are responses to this step
		indices = grep("Student\\.Work", names(obj.step))
		if (length(unique(obj.step[,indices[1]])) > 1){
			for (i in 1:length(indices)){
				index = indices[i]
				# get student response for all student work with some actual responses
				if (length(unique(obj.step[,index])) > 0){
					obj.step[,paste("Student.Response",i,sep=".")] <- studentResponse(obj.step, Student.Work.Part = i, as.data.frame.out=FALSE)
				}		
			}
			indices = grep("Student.Response", names(obj.step))
			# get only those who completed both pre and post
			obj.step = subset(obj.step, IURev.Num==0)
			relevant.columns = c("Wise.Id.1","Wise.Id.2","Wise.Id.3","Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns)
			relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
			relevant.columns = c(relevant.columns, paste("Student.Response",1:length(indices),sep="."))
			## use only relevant columns
			obj.step = subset(obj.step, TRUE, relevant.columns)
			# merge data frames
			sheetname = paste(substr(step.title,1,26),"-All",sep="")
			if (sheetname %in% sheetnames) sheetname = paste(sheetname, ".2",sep="")
			sheetnames = c(sheetnames, sheetname)
			print(sheetname)
			write.xlsx(obj.step,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=sheetname, append=append)
			if (byCondition){
				for (c in unique(obj.step$Condition)){
					write.xlsx(subset(obj.step,Condition==c),paste(dir.out,filename,sep=""),sheetName=paste(substr(step.title,1,23),"-Condition ", c,sep=""),row.names=FALSE, append=TRUE)
				}
			} 
			append = TRUE
		}
	}
	return(obj.step)
}

## main.eval allows a main title to be placed on a graph, but the text needs to be evaluated to allow for specific values
write.wisedata.frame.Grapher <- function (obj, dir.out, dir.plot, filename=NULL, parameters.by.step = NULL, extra.columns=c("Research.Score"), alternate.colors=FALSE){
	suppressPackageStartupMessages(library("XLConnect"))
	suppressPackageStartupMessages(library("rjson"))
	# get all steps with a grapher or sensor
	obj = subset(obj, (Step.Type=="Grapher"|Step.Type=="Sensor")&nchar(Student.Work.Part.1)>0)

	Step.Id = gsub("\\?", "\\.", unique(obj$Step.Id))
	append = FALSE
	if (is.null(filename)) filename = paste("GraphSteps.xlsx",sep="")
	wb <- loadWorkbook(paste(dir.out,filename,sep=""), create = !append)
	
	if(alternate.colors){
		
		tryCatch( {whiteStyle <- getCellStyle(wb, name="whiteStyle"); grayStyle <- getCellStyle(wb, name="grayStyle"); exists <- TRUE}, 
			error = function(e){exists <- FALSE})
		if (!exists){
			whiteStyle <- createCellStyle(wb, name="whiteStyle")
			setFillPattern(whiteStyle, fill = XLC$"FILL.SOLID_FOREGROUND")
			setFillForegroundColor(whiteStyle, color = XLC$"COLOR.WHITE")
			grayStyle <- createCellStyle(wb, name="grayStyle")
			setFillPattern(grayStyle, fill = XLC$"FILL.SOLID_FOREGROUND")
			setFillForegroundColor(grayStyle, color = XLC$"COLOR.GREY_25_PERCENT")
		}
	} else {
		setStyleAction(wb, type = XLC$"STYLE_ACTION.NONE")
	}
	
	print(paste(dir.out,filename,sep=""))
	sheetnames <- character()
	
	for (s in 1:length(Step.Id)){
		step.title = Step.Id[s]
		obj.step = subset(obj, Step.Id == step.title)
		# make sure that there are responses to this step
		indices = grep("Student.Work", names(obj.step))
		if (length(unique(obj.step[,indices[1]])) > 1){
			# get only those who completed both pre and post
			relevant.columns = c("Workgroup.Id", "Wise.Id.1","Wise.Id.2","Wise.Id.3", "Condition", "Teacher.Login", "Index", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns)
			relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
			imgcol = length(relevant.columns) + 1
			# merge data frames
			sheetname = paste(substr(step.title,1,26),sep="")
			if (sheetname %in% sheetnames) sheetname = paste(sheetname, ".2",sep="")
			sheetnames = c(sheetnames, sheetname)
			sheetname = gsub("'","",sheetname)
			sheetname = gsub("\\(","",sheetname)
			sheetname = gsub("\\)","",sheetname)
				
			print(sheetname)
			createSheet(wb, name = sheetname)
			writeWorksheet(wb, data = subset(obj.step, TRUE, relevant.columns), sheet = sheetname)
			by.prev <- NA
			# iterate through all rows of this worksheet adding images
			for (r in 1:nrow(obj.step)){
				row = obj.step[r,]
				step.num = row$Step.Num
				step.numnb = row$Step.Num.NoBranch
				step.id = row$Step.Id
				#look for step parameters
				if (!is.null(parameters.by.step)){
					for (param in parameters.by.step){
						if ((!is.null(param$Step.Num) && step.num %in% param$Step.Num) || (!is.null(param$Step.Id) && step.id %in% param$Step.Id) || (!is.null(param$Step.Num.NoBranch) && step.numnb %in% param$Step.Num.NoBranch)){
							#use this param
							break
						} else {
							param = list()
						}
					}
				}
				sw = as.wiseSW(row)
				for (sindex in 1:length(sw)){
					predictions = sw[[sindex]]$predictions
					if (!is.null(param$expected)){expected = param$expected} else {expected = data.frame(id = character(), x = numeric(), y = numeric())}
					if (!is.null(sw[[sindex]]$xMin) && !is.null(sw[[sindex]]$xMax) && length(sw[[sindex]]$xMin) > 0 && length(sw[[sindex]]$xMax) > 0 && !is.na(sw[[sindex]]$xMin) && !is.na(sw[[sindex]]$xMax)) {
						xlim = c(sw[[sindex]]$xMin, sw[[sindex]]$xMax)
					} else {
						# if scale does not match, then auto-adjust
						if (!is.null(param$xlim) && length(param$xlim) == 2){
							xlim = param$xlim
							#print(paste(row$Step.Work.Id))
							if (!is.null(predictions$x) && length(predictions$x) > 1 && (max(predictions$x,na.rm=TRUE) <= 1 || max(predictions$x,na.rm=TRUE) > xlim[2])) {
								xlim[2] = max(c(1,predictions$x))
							}
						} else {
							xlim = NULL
						}
					}
					if (!is.null(sw[[sindex]]$yMin) && !is.null(sw[[sindex]]$yMax) && length(sw[[sindex]]$yMin) > 0 && length(sw[[sindex]]$yMax) > 0 && !is.na(sw[[sindex]]$yMin) && !is.na(sw[[sindex]]$yMax)) {
						ylim = c(sw[[sindex]]$yMin, sw[[sindex]]$yMax)
					}  else {
						# if scale does not match, then auto-adjust
						if (!is.null(param$ylim) && length(param$ylim) == 2){
							ylim = param$ylim
							if (!is.null(predictions$y) && length(predictions$y) > 1 && (max(predictions$y,na.rm=TRUE) <= 1 || max(predictions$y,na.rm=TRUE) > ylim[2])) {
								ylim[2] = max(c(1,predictions$y))
							}
						} else {
							ylim = NULL
						}
					}
					
					if (!is.null(param$cols)){cols = param$cols} else {cols = NULL}
					if (!is.null(param$cols.expected)){cols.expected = param$cols.expected} else {cols.expected = NULL}
					if (!is.null(param$plot.title.eval)){plot.title <- eval(parse(text=param$plot.title.eval))} else {plot.title <- ""}
					plot.width = 40 * 7
					plot.height = 30 * 7
					png(paste(dir.plot,"tmp-image.png", sep=""), plot.width, plot.height)
					if (nchar(plot.title) > 0){
						op = par(mar=c(2,2,2,0))
					} else {
						op = par(mar=c(2,2,0,0))
					}
					plot(row, 'predictions', expected=expected, xlim=xlim,ylim=ylim, cols=cols, cols.expected=cols.expected, revision.number=sindex, plot.title=plot.title)
					dev.off()
					par(op)
					setColumnWidth(wb, sheet = sheetname, column = imgcol+sindex-1, width = 256 * 40)
					## are there enough rows for this behavior for a single image?
					if (15 * 4/3 >= plot.height){
						createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol+sindex-1],"$",r+1,sep=""), overwrite=TRUE)
					} else {
						## need to adjust the rows to fit an image, just increase top row
						hdiff = (plot.height - 15 * 4/3) * 3/4
						setRowHeight(wb, sheet = sheetname, r+1, height = 15 + hdiff)
						createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol+sindex-1],"$",r+1,sep=""), overwrite=TRUE)
					}
					addImage(wb, filename = paste(dir.plot,"tmp-image.png", sep=""), name = "pic", originalSize=TRUE)
				}
				if (alternate.colors){
					if (is.na(by.prev)){
						cellColor <- "gray"
						style <- grayStyle
					} else if (by.prev != row$Workgroup.Id){
						if (cellColor == "gray"){
							cellColor <- "white"
							style <- whiteStyle
						} else {
							cellColor <- "gray"
							style <- grayStyle
						}
					}
					#print(paste(r, cellColor, by.prev))
					setCellStyle(object = wb, sheet=sheetname, row = r+1, col = 1:(imgcol+10), cellstyle = style)
				}
				by.prev <- row$Workgroup.Id
			}
		}
	}
	saveWorkbook(wb)
}
#write.wisedata.frame.Grapher(wise.embedded.plots, dir.out=dir.out, dir.plot=dir.plot, filename="Unit-Density-Graphs.xlsx", parameters.by.step=parameters.DensityVsTemp, alternate.colors=TRUE)
#write.wisedata.frame.Grapher(subset(wise, Step.Id%in%items.unit[5]), dir.out=dir.out, dir.plot=dir.plot, filename="GraphingStories-Unit-Graphs-Fall2013-All.xlsx", parameters.by.step=parameters.Unit.Graphs, extra.columns=c("Research.Score","C.SlopeInterpretationError","C.Functional", "C.InitialPoint","C.Backwards","C.Motionless","C.ChangeSpeedForwards"))
#as.wiseSW(subset(wise,Step.Work.Id==5956928))


################ UTILITIES


#Step.Id.start <-"Unit.Explain.the.Mystery"
#Step.Id.stop <- "Unit.ContinentCritique"
#row <- subset(wise,Workgroup.Id%in%c(158892))
#row2 <- subsetStepsBetween (row, Step.Id.start, Step.Id.stop)
#subset(row2,TRUE,c("Workgroup.Id","Step.Id","Step.Work.Id","Time.Spent.Seconds"))

#feedback.immediate.steps = write.wisedata.frame.step.to.step(subset(wise,Is.Unit==TRUE), Step.Id.1="Unit.Explain.the.Mystery", Step.Id.2="Unit.ContinentCritique", byCondition=TRUE, dir.out=dir.out, filename="PlateTectonics-Spring2014-Embedded1.xlsx")
