library(plyr)

write.wisedata.frame.prepost = function(obj, dir.out, filename=NULL, byCondition=FALSE, pretest.identifier = "Pretest", posttest.identifier = "Posttest", extra.columns=character()){
	# get all unique step titles
	obj.pre = subset(obj, grepl(pretest.identifier,obj$Step.Id))
	obj.post = subset(obj, grepl(posttest.identifier,obj$Step.Id))
	Step.Id.pre = unique(sub(paste(pretest.identifier,"\\.",sep=""),"",unique(obj.pre$Step.Id)))
	Step.Id.post = unique(sub(paste(posttest.identifier,"\\.",sep=""),"",unique(obj.post$Step.Id)))
	Step.Id = Step.Id.pre[Step.Id.pre %in% Step.Id.post]
	Step.Id = gsub("\\?", "\\.", Step.Id)
	append = FALSE
	if (is.null(filename)) filename = paste(paste(Step.Id,collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	for (s in 1:length(Step.Id)){
		step.title = Step.Id[s]
		obj.pre.step = subset(obj.pre, grepl(step.title, Step.Id))
		obj.post.step = subset(obj.post, grepl(step.title, Step.Id))
		# make sure that there are responses to this step
		indices = grep("Student.Work", names(obj.pre.step))
		if (length(unique(obj.pre.step[,indices[1]])) > 1 && length(unique(obj.post.step[,indices[1]])) > 1){
			for (i in 1:length(indices)){
				index = indices[i]
				# get student response for all student work with some actual responses
				if (length(unique(obj.pre.step[,index])) > 0){
					obj.pre.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.pre.step, Student.Work.Part = i, as.data.frame.out=FALSE)
					obj.post.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.post.step, Student.Work.Part = i, as.data.frame.out=FALSE)
				}		
			}
			indices = grep("Student.Response", names(obj.pre.step))
			# get only those who completed both pre and post
			obj.pre.step = subset(obj.pre.step, (IURev.Num==0) & (Wise.Id.1 %in% obj.post.step$Wise.Id.1))
			obj.post.step = subset(obj.post.step, (IURev.Num==0)& (Wise.Id.1 %in% obj.pre.step$Wise.Id.1))
			relevant.columns = c("Wise.Id.1","Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns)
			relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
			relevant.columns = c(relevant.columns, paste("Student.Response",1:length(indices),sep="."))
			## use only relevant columns
			print(relevant.columns)
			obj.pre.step = subset(obj.pre.step, TRUE, relevant.columns)
			obj.post.step = subset(obj.post.step, TRUE, relevant.columns)
			# merge data frames
			by = c("Wise.Id.1","Condition", "Teacher.Login")[c("Wise.Id.1","Condition", "Teacher.Login") %in% relevant.columns]
			obj.step = merge(obj.pre.step, obj.post.step, by = by, all=TRUE, suffixes=c(paste(".",pretest.identifier,sep=""),paste(".",posttest.identifier,sep="")))
			new.order = names(obj.step)[!grepl(paste(".",pretest.identifier,"|",".",posttest.identifier,sep=""),names(obj.step))]
			names.pre = names(obj.step)[grepl(paste(".", pretest.identifier, sep=""),names(obj.step))]
			names.post = names(obj.step)[grepl(paste(".",posttest.identifier, sep=""),names(obj.step))]
			for (n in 1:length(names.pre)){
				new.order = c(new.order, names.pre[n], names.post[n])
			}
			# resort
			obj.step = subset(obj.step,select=new.order)
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
#values.prepost = write.wisedata.frame.prepost(wise.mdhs, excelOutDirectory,"MDHS-Values-PrePost.xlsx", pretest.identifier="ValuesPre", posttest.identifier="ValuesMid")

write.wisedata.frame.step.to.step = function(obj, Step.Id.1, Step.Id.2, dir.out=NULL, filename=NULL, byCondition=FALSE, extra.columns=character()){
	# get all unique step titles
	obj.pre = subset(obj, Step.Id %in% Step.Id.1)
	obj.post = subset(obj, Step.Id %in% Step.Id.2)
	Step.Title.pre = Step.Id.1
	Step.Title.post = Step.Id.2
	Step.Title = paste(Step.Title.pre,"to",Step.Title.post, sep=".")
	append = FALSE
	if (is.null(filename)) filename = paste(paste(Step.Title,collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	for (s in 1:length(Step.Id.1)){
		step.title = Step.Title[s]
		obj.pre.step = subset(obj.pre, Step.Id%in%Step.Id.1[s])
		obj.post.step = subset(obj.post, Step.Id%in%Step.Id.2[s])
		# make sure that there are responses to this step
		indices = grep("Student.Work", names(obj.pre.step))
		if (length(unique(obj.pre.step[,indices[1]])) > 1 && length(unique(obj.post.step[,indices[1]])) > 1){
			for (i in 1:length(indices)){
				index = indices[i]
				# get student response for all student work with some actual responses
				if (length(unique(obj.pre.step[,index])) > 0){
					obj.pre.step[paste("Student.Response",i,sep=".")] = studentResponse(obj.pre.step, Student.Work.Part = i, is.data.frame.out=FALSE)
					obj.post.step[paste("Student.Response",i,sep=".")] = studentResponse(obj.post.step, Student.Work.Part = i, is.data.frame.out=FALSE)
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
			new.order = names(obj.step)[!grepl(".pre|.post",names(obj.step))]
			names.pre = names(obj.step)[grepl(".pre",names(obj.step))]
			names.post = names(obj.step)[grepl(".post",names(obj.step))]
			for (n in 1:length(names.pre)){
				new.order = c(new.order, names.pre[n], names.post[n])
			}
			# resort
			obj.step = subset(obj.step,select=new.order)
			if (!is.null(dir)) write.xlsx(obj.step,paste(dir.out,filename,sep=""),row.names=FALSE,sheetName=paste(step.title,"-All",sep=""), append=append)
			if (byCondition) write.xlsx(subset(obj.step,Condition==1),paste(dir.out,filename,sep=""),sheetName=paste(step.title,"-Condition 1",sep=""),row.names=FALSE, append=TRUE)
			if (byCondition) write.xlsx(subset(obj.step,Condition==2),paste(dir.out,filename,sep=""),sheetName=paste(step.title,"-Condition 2",sep=""),row.names=FALSE, append=TRUE)
			append = TRUE
		}
	}
	return (obj.step)
}
write.step.record.between.steps = function(obj, Step.Id.1, Step.Id.2, repeated.column.names=c("Step.Work.Id","Step.Title","Time.Spent.Seconds","Student.Work.Part.1"), dir.out=NULL, filename=NULL, byCondition=FALSE, extra.columns=character()){
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
write.step.record = function(obj, Step.Id.target, Step.Id.start=NULL, Step.Id.stop=NULL, after.initial.start.step = TRUE, before.initial.stop.step = TRUE, repeated.column.names=c("Step.Work.Id","Time.Spent.Seconds","Student.Work.Part.1"), dir.out=NULL, filename=NULL, byCondition=FALSE, extra.columns=character()){
	# get all unique step titles
	if (!is.null(Step.Id.start)) obj.start = subset(obj, Step.Id %in% Step.Id.start)
	if (!is.null(Step.Id.stop)) obj.stop = subset(obj, Step.Id %in% Step.Id.stop)
	append = FALSE
	
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
			step.ids = as.character(subset(obj,Workgroup.Id==wgid & Index %in% step.index)[, rc])
			
			numeric.steps = c("Step.Work.Id", "Time.Spent.Seconds")
			if (rc %in% numeric.steps) step.ids=as.numeric(step.ids)
			
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
			} else if (length(step.ids) > 0 && length(step.ids) < ncol(step.ids.between)+1){
				if (rc %in% numeric.steps){
					step.ids = c(step.ids, rep(NA, length(step.ids.between)-1 - length(step.ids)))
				} else {
					step.ids = c(step.ids, rep("", length(step.ids.between)-1 - length(step.ids)))
				}
				step.ids.between = rbind(step.ids.between, step.ids)
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
			if (byCondition) write.xlsx(subset(obj.step.ids,Condition==1),paste(dir.out,filename,sep=""),sheetName=paste(rc,"-Condition 1",sep=""),row.names=FALSE, append=TRUE)
			if (byCondition) write.xlsx(subset(obj.step.ids,Condition==2),paste(dir.out,filename,sep=""),sheetName=paste(rc,"-Condition 2",sep=""),row.names=FALSE, append=TRUE)
		}
		append = TRUE
	}
	return (obj.step.ids)
}

write.wisedata.frame.allsteps = function(obj, dir.out, filename=NULL, byCondition=FALSE, extra.columns=character()){
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
		indices = grep("Student.Work", names(obj.step))
		if (length(unique(obj.step[,indices[1]])) > 1){
			for (i in 1:length(indices)){
				index = indices[i]
				# get student response for all student work with some actual responses
				if (length(unique(obj.step[,index])) > 0){
					obj.step[,paste("Student.Response",i,sep=".")] = studentResponse(obj.step, Student.Work.Part = i, as.data.frame.out=FALSE)
				}		
			}
			indices = grep("Student.Response", names(obj.step))
			# get only those who completed both pre and post
			obj.step = subset(obj.step, IURev.Num==0)
			relevant.columns = c("Wise.Id.1","Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns)
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

write.wisedata.frame.Grapher = function (obj, dir.out, filename=NULL, parameters.by.step = NULL, extra.columns=c("Research.Score")){
	suppressPackageStartupMessages(library("XLConnect",character.only=TRUE))
	suppressPackageStartupMessages(library("rjson",character.only=TRUE))
	# get all steps with a grapher or sensor
	obj = subset(obj, (Step.Type=="Grapher"|Step.Type=="Sensor")&nchar(Student.Work.Part.1)>0)

	Step.Id = gsub("\\?", "\\.", unique(obj$Step.Id))
	append = FALSE
	if (is.null(filename)) filename = paste("GraphSteps.xlsx",sep="")
	wb = loadWorkbook(paste(dir.out,filename,sep=""), create = !append)
	setStyleAction(wb, type = XLC$"STYLE_ACTION.NONE")
	
	print(paste(dir.out,filename,sep=""))
	sheetnames = character()
	for (s in 1:length(Step.Id)){
		step.title = Step.Id[s]
		obj.step = subset(obj, Step.Id == step.title)
		# make sure that there are responses to this step
		indices = grep("Student.Work", names(obj.step))
		if (length(unique(obj.step[,indices[1]])) > 1){
			# get only those who completed both pre and post
			relevant.columns = c("Wise.Id.1","Condition", "Teacher.Login", "Index", "Workgroup.Id", "Class.Period", "Parent.Project.Id", "Project.Id", "Run.Id", "Step.Work.Id", "Step.Visit.Count", "Step.Revision.Count","Step.Title","Step.Id","Step.Prompt","Time.Spent.Seconds",extra.columns)
			relevant.columns = relevant.columns[relevant.columns %in% names(obj)]
			imgcol = length(relevant.columns) + 1
			# merge data frames
			sheetname = paste(substr(step.title,1,26),sep="")
			if (sheetname %in% sheetnames) sheetname = paste(sheetname, ".2",sep="")
			sheetnames = c(sheetnames, sheetname)
			print(sheetname)
			createSheet(wb, name = sheetname)
			writeWorksheet(wb, data = subset(obj.step, TRUE, relevant.columns), sheet = sheetname)
			setColumnWidth(wb, sheet = sheetname, column = imgcol, width = 256 * 40)
			# iterate through all rows of this worksheet adding images
			for (r in 1:nrow(obj.step)){
				row = obj.step[r,]
				step.num = row$Step.Num
				#look for step parameters
				if (!is.null(parameters.by.step)){
					for (param in parameters.by.step){
						if (!is.null(param$Step.Num) && step.num %in% param$Step.Num){
							#use this param
							break
						} else {
							param = list()
						}
					}
				}
				sw = as.wiseSW(row)
				predictions = sw$predictions
				if (is.list(predictions) && length(predictions) > 0) predictions = tail(predictions, 1)[[1]]
				if (!is.null(param$expected)){expected = param$expected} else {expected = data.frame(id = character(), x = numeric(), y = numeric())}
				if (!is.null(sw$xMin) && !is.null(sw$xMax) && length(sw$xMin) > 0 && length(sw$xMax) > 0 && !is.na(tail(sw$xMin,1)) && !is.na(tail(sw$xMax,1))) {
					xlim = c(tail(sw$xMin,1), tail(sw$xMax,1))
				} else {
					# if scale does not match, then auto-adjust
					if (!is.null(param$xlim) && length(param$xlim) == 2){
						xlim = param$xlim
						if (!is.null(predictions$x) && length(predictions$x) > 1 && (max(predictions$x) <= 1 || max(predictions$x) > xlim[2])) {
							xlim[2] = max(c(1,predictions$x))
						}
					} else {
						xlim = NULL
					}
				}
				if (!is.null(sw$yMin) && !is.null(sw$yMax) && length(sw$yMin) > 0 && length(sw$yMax) > 0 && !is.na(tail(sw$yMin,1)) && !is.na(tail(sw$yMax,1))) {
					ylim = c(tail(sw$yMin,1), tail(sw$yMax,1))
				}  else {
					# if scale does not match, then auto-adjust
					if (!is.null(param$ylim) && length(param$ylim) == 2){
						ylim = param$ylim
						if (!is.null(predictions$y) && length(predictions$y) > 1 && (max(predictions$y) <= 1 || max(predictions$y) > ylim[2])) {
							ylim[2] = max(c(1,predictions$y))
						}
					} else {
						ylim = NULL
					}
				}
				
				if (!is.null(param$cols)){cols = param$cols} else {cols = NULL}
				
				plot.width = 40 * 7
				plot.height = 30 * 7
				png(paste(dir.plot,"tmp-image.png", sep=""), plot.width, plot.height)
				op = par(mar=c(2,2,0,0))
				plot(row, 'predictions', expected=expected, xlim=xlim,ylim=ylim, cols=cols)
				dev.off()
				par(op)
				## are there enough rows for this behavior for a single image?
				if (15 * 4/3 >= plot.height){
					createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
				} else {
					## need to adjust the rows to fit an image, just increase top row
					hdiff = (plot.height - 15 * 4/3) * 3/4
					setRowHeight(wb, sheet = sheetname, r+1, height = 15 + hdiff)
					createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
				}
				addImage(wb, filename = paste(dir.plot,"tmp-image.png", sep=""), name = "pic", originalSize=TRUE)
			}
		}
	}
	saveWorkbook(wb)
}
write.wisedata.frame.Grapher(subset(wise,Is.Unit==TRUE), dir.out=dir.out, filename="GraphingStories-Spring2013-Graphs.xlsx", parameters.by.step=parameters.by.step.spring2013)
