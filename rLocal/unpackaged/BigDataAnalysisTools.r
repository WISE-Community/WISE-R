### Prompt users to "tag" relevant steps in each project
### auto.pattern, include steps that match this pattern
### auto.not.pattern, do not include steps that match this pattern
### auto.step.type, include steps that are of this step type
### auto.only, just use patterns to find step types, do not prompt user
### auto.progression, after each prompt, step titles are either placed in a yes or no array and used to match future titles (without leading numbers) 
### prompt.remaining, if set to false will not prompt user if any titles have been matched, else (default) will prompt with leftover
tagStepsInEachProject <- function (wise, tagList=NULL, startingProjectNum=NA, auto.pattern="", auto.not.pattern="", auto.step.type="", auto.only=FALSE, auto.progression=FALSE, prompt.remaining=TRUE, DEBUG=FALSE){
	if (is.null(tagList)){
		tagList = list()
		start = 1
	} else {
		if (is.na(startingProjectNum)){
			start = length(tagList) + 1
		} else {
			start = startingProjectNum
		}
	}
	p = start
	match.steps = character()
	match.steps.parts = list()
	no.match.steps = character()
	while (p <= length(unique(wise$Project.Id))) {
		auto.progression.project <- auto.progression # we can overwrite for this project
		pr = unique(wise$Project.Id)[p]
		Project.Id = pr
		Step.Titles = character()
		step.titles.auto = character()
		step.titles.match = character()
		wise.remain = subset(wise, Project.Id==pr)
		if (auto.progression){
			if (length(match.steps) > 0){
			### strip leading digits off step titles
				step.titles = as.character(sapply(as.character(wise.remain$Step.Title),function(x)return(substring(x,regexpr(" ",x)+1))))
				### do any step titles match the hit list?
				step.titles.match = step.titles[step.titles %in% match.steps]
				step.titles.auto = as.character(unique(subset(wise.remain, step.titles %in% match.steps)$Step.Title))
				### remove these steps from remaining
				wise.remain = subset(wise.remain, !(step.titles %in% match.steps))
			}
			if (length(no.match.steps) > 0){
				### are any step titles on the do not match list?
				step.titles = as.character(sapply(as.character(wise.remain$Step.Title),function(x)return(substring(x,regexpr(" ",x)+1))))
				
				### remove these steps from remaining
				wise.remain = subset(wise.remain, !(step.titles %in% no.match.steps))
			}
		}
		if (nchar(auto.pattern) > 0){
			step.titles.auto = union(step.titles.auto, as.character(grep(auto.pattern, unique(wise.remain$Step.Title),value=TRUE)))
			wise.remain = subset(wise.remain, !(Step.Title %in% step.titles.auto))
		} 
		# make sure these matched steps don't match the not list
		if (nchar(auto.not.pattern) > 0 && length(step.titles.auto)>0) {
			step.titles.auto = step.titles.auto[!grepl(auto.not.pattern, step.titles.auto)]
			wise.remain = subset(wise.remain, !(Step.Title %in% step.titles.auto))
		}
		### add to the auto step titles regardless of above
		if (nchar(auto.step.type) > 0){
			step.titles.auto = union(step.titles.auto, as.character(unique(subset(wise,Project.Id==pr&Step.Type%in%as.character(grep(auto.step.type, unique(subset(wise,Project.Id==pr)$Step.Type),value=TRUE)))$Step.Title)))
			wise.remain = subset(wise.remain, !(Step.Title %in% step.titles.auto))
		} 
		## if auto.only don't prompt user with any remaining steps in this project
		## if auto.progression DO prompt user with remaining steps, saved titles will be propogated forwards (both hits and misses)
		## else just prompt user with remaining
		if (auto.only || nrow(wise.remain) == 0 || (length(step.titles.auto)>0 && !prompt.remaining)){
			Step.Titles = step.titles.auto
			Student.Work.Indices = match.steps.parts[match.steps %in% step.titles.match]
		} else { 
			# if the last element in the list is this project, remove it so that we can try again
			if (length(tagList)>0 && tagList[[length(tagList)]]$Project.Id == pr){
				if (length(tagList)>1){
					tagList = tagList[1:(length(tagList)-1)]
				} else {
					tagList = list()
				}
			}
			Step.Titles = step.titles.auto
			if (length(step.titles.match) > 0){
				if(DEBUG)print( match.steps.parts[match.steps %in% step.titles.match])
				Student.Work.Indices = match.steps.parts[match.steps %in% step.titles.match]
			} else {
				Student.Work.Indices = list()
			}
			
			print(paste("Step titles in project num:", pr))
			for (s in 1:length(unique(wise.remain$Step.Title))){
				st = unique(wise.remain$Step.Title)[s]
				print(paste(s,": ",st,sep=""))
			}
			completed = FALSE
			while (!completed){
				print("0: Next")
				print("-1: I made a mistake")
				print("-2: I'm done")
				print("-3: Next (and don't use auto progression for this project)")
				print("Enter a choice")
				val = scan(nmax=1)
				if (length(val) == 0){
					# do nothing, ask again
				} else if (val == -1){
					# made a mistake
					Project.Id = vector()
					p = p - 2
					completed = TRUE
				} else if (val == -2){
					return (tagList)
				} else if (val == 0) {
					# move on
					completed = TRUE
				} else if (val == -3) {
					# move on
					auto.progression.project <- FALSE
					completed = TRUE
				} else if (val > 0 && val <= length(unique(wise.remain$Step.Title))) {			
					#picked a step
					st <- as.character(unique(wise.remain$Step.Title))[val]
					Step.Titles = union(Step.Titles, st)
					if (unique(subset(wise.remain, Step.Title==st)$Step.Type)=="AssessmentList"){
						parts <- numeric()
						p.max <- length(grep("Student\\.Work", names(wise.remain)))
						print(paste("Student Work Parts: 1-",p.max,":",sep=""))
						print("0: None")
						print("1-9: Pick part")
						print("Enter a choice")
						completed.2 <- FALSE
						while (!completed.2){
							val = scan(nmax=1)
							if (length(val) == 0){
								# do nothing	
							} else if (val >= 1 && val <= p.max){
								parts <- sort(union(parts, val))
							} else {
								completed.2 <- TRUE
							}
						}
						Student.Work.Indices[[which(Step.Titles==st)]] = parts
					} else {
						Student.Work.Indices[[which(Step.Titles==st)]] = numeric()
					}
					print(Step.Titles)
				}
			}
		}
		# update match and no match lists
		if (auto.progression.project){
			Step.Titles.not <- unique(subset(wise, Project.Id==pr&!(Step.Title %in% Step.Titles))$Step.Title)
			match.steps <- union(match.steps, as.character(sapply(Step.Titles,function(x)return(substring(x,regexpr(" ",x)+1)))))
			# cycle through Student Work Indices
			if (length(Student.Work.Indices)){
				for (swi in 1:length(Student.Work.Indices)){
					match.index <- which(match.steps == as.character(sapply(Step.Titles[swi],function(x)return(substring(x,regexpr(" ",x)+1)))))
					match.steps.parts[[match.index]] <- Student.Work.Indices[[swi]]
				}
			}
			no.match.steps <- union(no.match.steps, as.character(sapply(Step.Titles.not,function(x)return(substring(x,regexpr(" ",x)+1)))))
		}
		if (length(Project.Id) > 0){
			tagList[[length(tagList)+1]] = list(Project.Id=Project.Id, Step.Title=Step.Titles, Student.Work.Index=Student.Work.Indices)
		}

		p = p + 1
	}
	return (tagList)
}

# Looks for work that has a mean word count of a given minimum, and a mean sd of a given minimum
# basically, looking for energy-story like items
tagStepsInEachProject.byWords <- function (wise, wordCountMean.min, wordCountSD.min, DEBUG=FALSE){
	tagList = list()
	p = 1
	match.steps = character()
	match.student.work.indices = list()
	wise.remain = subset(wise, Step.Type%in%c("OpenResponse","Note","AssessmentList"))
	swindices = grep("Student\\.Work",names(wise.remain))
	wise.remain$step.titles.all = gsub("^.*? | *$","",tolower(as.character(wise.remain$Step.Title)))
	step.titles = unique(wise.remain$step.titles.all)
	for (stid in 1:length(step.titles)){
		step.title = step.titles[stid]
		wise.step = subset(wise.remain, step.titles.all == step.title)
		Student.Work.Index = numeric()
		any.found = FALSE
		for (si in 1:length(swindices)){
			swi = swindices[si]
			wc = sapply(strsplit(wise.step[,swi], " +"),length)
			# remove blank responses
			wc = wc[!is.na(wc)]
			wc = wc[wc > 1]
			if (length(wc) > 1 && mean(wc,na.rm=TRUE) >= wordCountMean.min && sd(wc, na.rm=TRUE) >= wordCountSD.min){
				Student.Work.Index = union(Student.Work.Index,si)
				any.found = TRUE
			}
		}
		if (any.found){
			len = length(match.steps)
			match.steps = union(match.steps, step.title)
			if (length(match.steps) > len) match.student.work.indices[[length(match.student.work.indices)+1]] = Student.Work.Index
		} 
	}
	if(DEBUG)print(match.steps)
	## now that we found a list of acceptable items, associate them with projects
	
	for (p in 1:length(unique(wise.remain$Project.Id))){
		Step.Title = character()
		Student.Work.Index = list()
		pr = unique(wise.remain$Project.Id)[p]
		for (mi in 1:length(match.steps)){
			step.title = match.steps[mi]
			wise.step = subset(wise.remain,Project.Id==pr&step.titles.all==step.title)
			if (nrow(wise.step) > 0){
				step.title.real = unique(wise.step$Step.Title)
				if(wise.step$Project.Id[1]==261)print(step.title.real)
				for (str in step.title.real){
					Step.Title = c(Step.Title, str)
					Student.Work.Index[[length(Student.Work.Index)+1]] = match.student.work.indices[[mi]]
				}				
			} 
		}
		tagList[[length(tagList)+1]] = list(Project.Id = pr, Step.Title=Step.Title, Student.Work.Index = Student.Work.Index)
	}
	
	return(tagList)
}
### subsetTagList uses the list created from tagStepsInEachProject to subset
### the structure of a tag list should be each list element will have some fields like Project.Id
### and Step.Title. Will return subset with all steps that match any of the list elements
### inverse may be an array of strings that match an outcome, if met will find those who do not meet criterial
subsetTagList <- function(wise, tagList, inverse = NULL){
	wise.out = wise[0,]
	for (l in tagList){
		wise.project = wise
		# cycle through all keys, must be a match for each key
		for (key in names(l)){
			if (key %in% names(wise.project)){
				if (key %in% inverse){
					wise.project = subset(wise.project, !(wise.project[,key] %in% l[[key]]))	
				} else {
					wise.project = subset(wise.project, wise.project[,key] %in% l[[key]])
				}
			}
		}
		if (nrow(wise.project)>0){
			wise.out = rbind(wise.out, wise.project)
		}
	}
	return (wise.out)
}

subsetTagList.byWords <- function(wise, tagList, wordCount.min){
	wise.out = wise[0,]
	for (l in tagList){
		wise.project = wise
		# cycle through all keys, must be a match for each key
		for (key in names(l)){
			if (key %in% names(wise.project)){
				wise.project = subset(wise.project, wise.project[,key] %in% l[[key]])
			}
		}
		swindices = grep("Student\\.Work",names(wise.project))
		# remove all steps without the minimum work
		for (sti in 1:length(l$Step.Title)){
			student.work.index = 1
			if (!is.null(l$Student.Work.Index) && length(l$Student.Work.Index) > sti) student.work.index = l$Student.Work.Index[[sti]]
			for (swi in student.work.index){
				wise.step = subset(wise.project,Step.Title==l$Step.Title[sti])
				work = wise.step[,swindices]
				wc = sapply(strsplit(work[,swi], " +"),length)
				#print(sum(wc > wordCount.min))
				#print(nrow(wise.step))
				wise.step.out = subset(wise.step, wc < wordCount.min)
				wise.project = subset(wise.project, !(Index %in% wise.step.out$Index))
				#print(nrow(wise.project))
				#print("  -after-     ")
			}
		}
		if (nrow(wise.project)>0){
			wise.out = rbind(wise.out, wise.project)
		}
	}
	return (wise.out)
}

### subset only those steps where students "do" work
subsetWorkSteps <- function (wise, Step.Types = c("AssessmentList", "OpenResponse", "MultipleChoice", "MySystem", "MySystem2", "Note")){
	return (subset(wise, Step.Type %in% Step.Types))
}



### This function creates (or replaces) a vector 'Step.Id.Common' which replaces any two
	#### steps that are the same with a common (i.e., the first observed) Step Id, thus reducing
	#### the number of unique Step Ids. 
	#### Uses both similarity of Step Id's and Prompts.
replaceCommonStepIds <- function (wise,  max.distance = 0.1, as.data.frame.out = TRUE, report.only = FALSE, ask.possible = TRUE){
	StepInfo = subset(wise,!duplicated(Step.Id),c(Step.Id, Step.Prompt))
		# clean up ids and prompts
	StepInfo$Step.Id.clean = as.character(tolower(StepInfo$Step.Id))
	StepInfo$Is.Pretest = grepl(".*pre\\.|.*pretest\\.",StepInfo$Step.Id.clean)
	StepInfo$Is.Posttest = grepl(".*post\\.|.*posttest\\.",StepInfo$Step.Id.clean)
	StepInfo$Is.Extra = grepl(".*extra\\.(credit\\.)?",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub(".*pre\\.|.*pretest\\.","pretest ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub(".*post\\.|.*posttest\\.","posttest ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub(".*extra\\.", "extra ", StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("\\.|\\,|\\:|\\;|\\-|\\?\\!", " ", StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("  +", " ", StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("pretest pre(test)? ","pretest ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("posttest post(test)? ","posttest ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("posttest revise ","posttest ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("extra extra ","extra ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub("extra credit ","extra ",StepInfo$Step.Id.clean)
	StepInfo$Step.Id.clean = gsub(" $|^  ", "", StepInfo$Step.Id.clean)
	### get array of clean Step Ids that are mapped to multiple original Step Ids
	Step.Id.clean.reused = subset(StepInfo,duplicated(Step.Id.clean))$Step.Id.clean
	# pass through each reused name and replace with common (1st) instance
	StepInfo$Step.Id.Common = StepInfo$Step.Id
	for (sid.clean in Step.Id.clean.reused){
		sid = subset(StepInfo, Step.Id.clean==sid.clean)$Step.Id[1]
		StepInfo$Step.Id.Common[StepInfo$Step.Id.clean == sid.clean] = sid
	}
	#print(subset(StepInfo,TRUE,c(Step.Id,Step.Id.clean, Step.Id.Common)))
	if(ask.possible){
		StepInfo$Step.Prompt.clean = gsub("<.*?>", "",StepInfo$Step.Prompt)
		StepInfo$Step.Prompt.clean = gsub("\\.|\\,|\\:|\\;|\\-|\\?\\!", " ", StepInfo$Step.Prompt.clean)
		StepInfo$Step.Prompt.clean = gsub("  +", " ", StepInfo$Step.Prompt.clean)
		StepInfo$Step.Prompt.clean = gsub(" $|^  ", "", StepInfo$Step.Prompt.clean)
		
		# don't bother with those already the same according to step id
		StepInfo.reduced = subset(StepInfo, !duplicated(Step.Id.Common))
		matched.indices = numeric()
		#print("The following steps may match:");
		for (s in 1:(length(StepInfo.reduced$Step.Prompt.clean)-1)){
			sp.clean = StepInfo.reduced$Step.Prompt.clean[s]
			if (nchar(sp.clean) > 10 && !(s %in% matched.indices)){
				reused = agrep(sp.clean, StepInfo.reduced$Step.Prompt.clean[(s+1):length(StepInfo.reduced$Step.Prompt.clean)], max.distance = max.distance)
				if (length(reused) > 0){
					for (r in reused){
						print("Are the following the same (1:yes, 0:no):")
						print(paste(subset(wise, Step.Id==StepInfo.reduced$Step.Id[s])$Project.Id[1],subset(wise, Step.Id==StepInfo.reduced$Step.Id[s])$Step.Num[1],as.character(StepInfo.reduced$Step.Id[s])))
						print(paste(subset(wise, Step.Id==StepInfo.reduced$Step.Id[s+r])$Project.Id[1],subset(wise, Step.Id==StepInfo.reduced$Step.Id[s+r])$Step.Num[1],as.character(StepInfo.reduced$Step.Id[s+r])))
						is.match = scan(nmax=1)
						if (is.match == 1){
							matched.indices = c(matched.indices, reused+s)
							# replace alternative common ids with first common id
							StepInfo$Step.Id.Common[StepInfo$Step.Id == StepInfo.reduced$Step.Id[s+r]] = StepInfo.reduced$Step.Id.Common[s]
						}
					}
				}
			}
		}
	}
	if (report.only){
		return (unique(StepInfo$Step.Id.Common))
	} else {
		if (sum(grepl("Step.Id.Common", names(wise)))>0) wise = subset(wise,TRUE,!grepl("Step.Id.Common", names(wise)))
		wise$Step.Id.Common = wise$Step.Id
		for (s in 1:length(StepInfo$Step.Id)){
			wise[wise$Step.Id == StepInfo$Step.Id[s],"Step.Id.Common"] = StepInfo$Step.Id.Common[s]
		}
		#wise = merge(wise, subset(StepInfo,TRUE,c(Step.Id, Step.Id.Common)) , by = "Step.Id", all.x=TRUE)
		if (as.data.frame.out){
			return(wise)
		} else {
			return(wise$Step.Id.Common)
		}
	}
}

### reportCommonStepIds will search through a wisedata.frame looking for steps that are in
 ### common across all Parent.Project.Ids
 ### In some cases a project could either have the pretest and posttest within the main unit,
 ### while in other projects the pretest and posttest may be separated.
 ### For the latter case include these "linked" projects in Linked.Parent.Project.Ids
 ### For example if Parent Project 1111 has steps "Pretest.A" and "Unit.B"
 ### but these same steps are separated for other students in Project 1112 and 1113
 ### then use Linked.Parent.Project.Ids = list (c(1112, 1113))
subsetCommonStepIds <- function (wise, Linked.Parent.Project.Ids = list(), inverse=FALSE, report.only=FALSE){
	if (sum(grepl("Step.Id.Common", names(wise)))==0){
		print("Finding common step ids...")
		wise = replaceCommonStepIds (wise,  max.distance = 0.1, as.data.frame.out = TRUE, report.common.step.ids.only = FALSE, ask.possible = FALSE)
	}

	Step.Ids = unique(wise$Step.Id.Common)
	# Put Linked projects into a flat vector
	Linked.Parent.Project.Ids.Vector = unlist(Linked.Parent.Project.Ids)
	# create a list of all independent Parent.Project.Id or linked Projects
	Parent.Project.Id.List = Linked.Parent.Project.Ids
	for (ppid in unique(wise$Parent.Project.Id)){
		if (!(ppid %in% Linked.Parent.Project.Ids.Vector)) Parent.Project.Id.List = c(Parent.Project.Id.List, ppid)
	}
	# iterate through each step id and parent project id to see if it exists or not
	Step.Id.all = vector()
	Step.Id.some = vector()
	for (sid in Step.Ids){
		all.found = TRUE
		for (ppid in Parent.Project.Id.List){
			# do project id to be extra sure
			found = FALSE
			for (pid in unique(subset(wise, Parent.Project.Id %in% ppid)$Project.Id)){
				pwise = subset(wise, Project.Id %in% pid)
				if (sid %in% pwise$Step.Id.Common){
					found <- TRUE
					break
				}
			}
			if (!found){
				all.found <- FALSE
				break
			}
		}
		if (all.found){
			Step.Id.all = c(Step.Id.all, sid)
		} else {
			Step.Id.some = c(Step.Id.some, sid)
		}
	}

	if (report.only){
		if (inverse){
			return (Step.Id.some)
		} else {
			return (Step.Id.all)
		}
	} else {
		if (inverse){
			return (subset(wise,Step.Id.Common %in% Step.Id.some))
		} else {
			return (subset(wise,Step.Id.Common %in% Step.Id.all))
		}
	}
}

### uses both a general wise data frame and a subset with only common steps
 ## Features for each student - broken down for [common | work | nonwork | all] common steps, work steps, nonwork steps, and all steps
 ### Completion.pct.[] - on common steps what is the percent completion 
 ### Time.Spent.Seconds.sum.[] duration on 
 ### Step.Visit.Count.sum.[] - times student has visited this step
 ### Step.Revisit.Count.sum.[] - times student has visited this step after first time
 ### Step.Visit.Count
compileStudentFeatures <- function(wise, wise.common = NULL, Linked.Parent.Project.Ids = list(), ask.possible=FALSE){
	# get work steps only	
	wise.work = subsetWorkSteps(wise)
	# is completed if not a work step or if work step there is something in Student.Work.Part.1
	wise$Is.Completed = rep(TRUE, nrow(wise))
	wise$Is.Completed[wise$Step.Type %in% unique(wise.work$Step.Type) & (nchar(wise$Student.Work.Part.1)<=1 | is.na(wise$Student.Work.Part.1))] = FALSE
	if (!is.null(wise.common) && sum(grepl("Is.Completed", names(wise.common)) == 0)){
		wise.common$Is.Completed = rep(TRUE, nrow(wise.common))
		wise.common$Is.Completed[wise.common$Step.Type %in% unique(wise.work$Step.Type) & (nchar(wise.common$Student.Work.Part.1)<=1 | is.na(wise.common$Student.Work.Part.1))] = FALSE
	}
	if (is.null(wise.common)){
		wise.work$Step.Id.Common = replaceCommonStepIds(wise.THERM.work,  as.data.frame.out = FALSE, report.only = FALSE, ask.possible=ask.possible)
		wise.common = subsetCommonStepIds(wise.THERM.work, Linked.Parent.Project.Ids = Linked.Parent.Project.Ids, inverse = FALSE)
	}

	wise$Step.Visit = rep(1,nrow(wise))
	wise.work$Step.Visit = rep(1,nrow(wise.work))
	wise.common$Step.Visit = rep(1,nrow(wise.common))
	
	### create data frames with only revisits 
	wise$Same.as.previous = rep(FALSE, nrow(wise))
	wise$Same.as.previous[2:nrow(wise)] = as.numeric(as.character(wise$Step.Num[2:nrow(wise)])) == as.numeric(as.character(wise$Step.Num[1:(nrow(wise)-1)]))
	wise$Step.Visit.Count.real = wise$Step.Visit.Count - as.numeric(wise$Same.as.previous)
	wise.work$Same.as.previous = rep(FALSE, nrow(wise.work))
	wise.work$Same.as.previous[2:nrow(wise.work)] = as.numeric(as.character(wise.work$Step.Num[2:nrow(wise.work)])) == as.numeric(as.character(wise.work$Step.Num[1:(nrow(wise.work)-1)]))
	wise.work$Step.Visit.Count.real = wise.work$Step.Visit.Count - as.numeric(wise.work$Same.as.previous)
	wise.common$Same.as.previous = rep(FALSE, nrow(wise.common))
	wise.common$Same.as.previous[2:nrow(wise.common)] = as.numeric(as.character(wise.common$Step.Num[2:nrow(wise.common)])) == as.numeric(as.character(wise.common$Step.Num[1:(nrow(wise.common)-1)]))
	wise.common$Step.Visit.Count.real = wise.common$Step.Visit.Count - as.numeric(wise.common$Same.as.previous)
	
	wise.revisit = subset(wise,Step.Visit.Count.real > 1)
	wise.work.revisit = subset(wise.work, Step.Visit.Count.real > 1)
	wise.common.revisit = subset(wise.common, Step.Visit.Count.real > 1)

	wise.energystory = subset(wise,grepl("energy\\.story",tolower(Step.Id)))
	wise.work.energystory = subset(wise.work,grepl("energy\\.story",tolower(Step.Id)))
	wise.common.energystory = subset(wise.common,grepl("energy\\.story",tolower(Step.Id)))

	wise.mysystem = subset(wise,grepl("mysystem",tolower(Step.Type)))
	wise.work.mysystem = subset(wise.work,grepl("mysystem",tolower(Step.Type)))
	wise.common.mysystem = subset(wise.common,grepl("mysystem",tolower(Step.Type)))
	
	wise.stufea = aggregate(wise,by=list(Workgroup.Id),select.first=c(Wise.Id.1, Wise.Id.2, Wise.Id.3, Teacher.Login, Class.Period, Project.Id, Parent.Project.Id,Run.Id), include.median.splits=FALSE)
	
	wise.stufea = merge(wise.stufea, aggregate(wise, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE)
	wise.stufea = merge(wise.stufea, aggregate(wise.work, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".work"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".common"))
	wise.stufea = merge(wise.stufea, aggregate(wise, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE)
	wise.stufea = merge(wise.stufea, aggregate(wise.work, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".work"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".common"))
	
	wise.stufea = merge(wise.stufea, aggregate(wise.revisit, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".revisit"))
	wise.stufea = merge(wise.stufea, aggregate(wise.work.revisit, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".revisit.work"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common.revisit, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".revisit.common"))
	wise.stufea = merge(wise.stufea, aggregate(wise.revisit, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".revisit"))
	wise.stufea = merge(wise.stufea, aggregate(wise.work.revisit, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".revisit.work"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common.revisit, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".revisit.common"))
	
	wise.stufea = merge(wise.stufea, aggregate(wise.energystory, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".energystory"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common.energystory, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".energystory.common"))
	wise.stufea = merge(wise.stufea, aggregate(wise.energystory, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".energystory"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common.energystory, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".energystory.common"))
	
	wise.stufea = merge(wise.stufea, aggregate(wise.mysystem, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".mysystem"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common.mysystem, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Time.Spent.Seconds), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".mysystem.common"))
	wise.stufea = merge(wise.stufea, aggregate(wise.mysystem, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".mysystem"))
	wise.stufea = merge(wise.stufea, aggregate(wise.common.mysystem, by=list(Workgroup.Id), select.first=vector(), select.numerical=c(Step.Visit), FUNS.numerical=c("sum"), include.median.splits=FALSE)[,1:2], by="Workgroup.Id", all.x = TRUE, suffixes=c("", ".mysystem.common"))
	
	return(wise.stufea)
}
