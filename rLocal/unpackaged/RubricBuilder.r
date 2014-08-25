createRuleDataFrame.grapher = function(seriesId=""){
	df = data.frame(label=character(),x1.min=numeric(),x1.max=numeric(), y1.min=numeric(), y1.max=numeric(), x2.min=numeric(), x2.max=numeric(), y2.min=numeric(),y2.max=numeric(), width.min = numeric(), width.max = numeric(), height.min = numeric(), height.max = numeric(), rotation.min=numeric(), rotation.max=numeric(), angle.min=numeric(), angle.max=numeric(), xbounds.min = numeric(), xbounds.max = numeric(), ybounds.min = numeric(), ybounds.max = numeric(), npoints.min = numeric(), npoints.max = numeric(), return.to.point = numeric(), relation = character(), collapse.inline.degrees = numeric(), stringsAsFactors=FALSE)
	if (nchar(seriesId) > 0) df[,paste("seriesId",seriesId,sep=".")] = character()
	return (df)
}
addNewRule.grapher = function(rules, label="",x1.min=NA, x1.max=NA, y1.min=NA, y1.max=NA, x2.min=NA, x2.max=NA, y2.min=NA, y2.max=NA, width.min = NA, width.max = NA, height.min = NA, height.max = NA, rotation.min=NA, rotation.max=NA, angle.min=NA, angle.max=NA, xbounds.min = NA, xbounds.max = NA, ybounds.min = NA, ybounds.max = NA, npoints.min = NA, npoints.max = NA, return.to.point = NA, relation = "", collapse.inline.degrees = NA){
	n = nrow(rules)+1
	rules[n,] = NA
	rules$label[n]=label; rules$x1.min[n]=x1.min; rules$x1.max[n]=x1.max; rules$y1.min[n]=y1.min; rules$y1.max[n]=y1.max; rules$x2.min[n]=x2.min; rules$x2.max[n]=x2.max; rules$y2.min[n]=y2.min; rules$y2.max[n]=y2.max; rules$width.min[n] = width.min; rules$width.max[n] = width.max; rules$height.min[n] = height.min; rules$height.max[n] = height.max; rules$rotation.min[n] = rotation.min; rules$rotation.max[n] = rotation.max; rules$angle.min[n] = angle.min; rules$angle.max[n] = angle.max; rules$xbounds.min[n] = xbounds.min; rules$xbounds.max[n] = xbounds.max; rules$ybounds.min[n] = ybounds.min; rules$ybounds.max[n] = ybounds.max; rules$npoints.min[n] = npoints.min; rules$npoints.max[n] = npoints.max; rules$return.to.point[n] = return.to.point; rules$relation[n] = relation; rules$collapse.inline.degrees[n] = collapse.inline.degrees
	if (n > 1 && is.na(rules$xbounds.min[n])) rules$xbounds.min[n] = rules$xbounds.min[n-1]
	if (n > 1 && is.na(rules$xbounds.max[n])) rules$xbounds.max[n] = rules$xbounds.max[n-1]
	if (n > 1 && is.na(rules$ybounds.min[n])) rules$ybounds.min[n] = rules$ybounds.min[n-1]
	if (n > 1 && is.na(rules$ybounds.max[n])) rules$ybounds.max[n] = rules$ybounds.max[n-1]
	sindex = grep("seriesId",names(rules))
	if (length(sindex) > 0){
		# if this is the first row use the suffix after seriesId and rename column to just seriesId
		if (n == 1){
			seriesId = strsplit(names(rules)[sindex], "\\.")[[1]][2]
			names(rules)[sindex] = "seriesId"
			rules$seriesId[n] = seriesId
		} else {
			rules$seriesId[n] = rules$seriesId[n-1]
		}
	}
	return(rules)
}
createRuleDataFrame.mysystem = function(){
	df = data.frame(label=character(), StartNode = character(), EndNode = character(), Link = character(), require.All=logical(), stringsAsFactors=FALSE)
	return (df)
}
addNewRule.mysystem = function(rules, label="", StartNode = ".*", EndNode = ".*", Link = ".*", require.All=TRUE){
	rules = rbind(rules, data.frame(label=label, StartNode = StartNode, EndNode = EndNode, Link = Link, require.All=require.All, stringsAsFactors=FALSE))
	return(rules)
}

createScoreDataFrame = function(){
	return (data.frame(pattern = character(), score = numeric(), stringsAsFactors=FALSE))
}
addNewScore = function(scores, pattern="", score = NA, rules = NULL){
	if (!is.null(rules)){
		### Replace rule labels with indices
		ids = character()
		repls = numeric()
		greg = gregexpr("\\[.*?[a-z]+.*?\\]",pattern)[[1]]
		for (g in 1:length(greg)){
			id = substr(pattern, start=greg[g]+1, stop=greg[g]+attr(greg,"match.length")[g]-2)
			repl = which(rules$label == id)
			if (length(repl) > 0){
				ids = c(ids, id)
				repls = c(repls, repl)
			} else {
				print(paste("No Match for", id))
			}
		}
		for (i in 1:length(ids)){
			pattern = gsub(paste("[",ids[i],"]",sep=""),paste("[",repls[i],"]",sep=""),pattern, fixed=TRUE)
		}
	}
	df = data.frame(pattern=pattern, score=score, stringsAsFactors=FALSE)
	# if pattern includes labels like [2pts], ie is not pre numeric
	return (rbind(scores, df))
}
addNewRubric = function(rubrics, type, rubric, Parent.Project.Id=NULL, Project.Id=NULL, Run.Id=NULL, Step.Num=NULL, Step.Num.NoBranch=NULL, Step.Id=NULL){
	rubricList = list()
	if (!is.null(Parent.Project.Id)){
		rubricList$Parent.Project.Id = Parent.Project.Id
	} else if (!is.null(Project.Id)){
		rubricList$Project.Id = Project.Id
	} else if (!is.null(Run.Id)){
		rubricList$Run.Id = Run.Id
	} else {
		print("Need a project identifier")
		return (rubrics)
	}

	if (!is.null(Step.Num)){
		rubricList$Step.Num = Step.Num
	} else if (!is.null(Step.Num.NoBranch)){
		rubricList$Step.Num.NoBranch = Step.Num.NoBranch
	} else if (!is.null(Step.Id)){
		rubricList$Step.Id = Step.Id
	} else {
		print("Need a step identifier")
		return (rubrics)
	}
	rubricList$type = type
	rubricList$rubric = rubric
	rubrics[[length(rubrics)+1]] = rubricList
	return (rubrics)
}

# This function iterates through each item in a rubrics list against the given object.
# Shows work for a random response fitting rubric and score. Continues until user inputs to skip to next item.
test.Rubric <- function (obj, score.rubrics, start.item=1){
	for (r in start.item:length(score.rubrics)){
		rubric <- score.rubrics[[r]]
		pid.type <- NA
		pid <- NA
		if (!is.null(rubric$Parent.Project.Id)){
			pid <- rubric$Parent.Project.Id
			pid.type <- "Parent.Project.Id"
		} else if (!is.null(rubric$Project.Id)){
			pid <- rubric$Project.Id
			pid.type <- "Project.Id"
		} else if (!is.null(rubric$Run.Id)){
			pid <- rubric$Run.Id
			pid.type <- "Run.Id"
		} else {
			print(paste("No Project Id for rubric", r))
		}

		sid.type <- NA
		sid <- NA
		if (!is.null(rubric$Step.Id)){
			sid <- rubric$Step.Id
			sid.type <- "Step.Id"
		} else if (!is.null(rubric$Step.Title)){
			sid <- rubric$Step.Title
			sid.type <- "Step.Title"
		} else if (!is.null(rubric$Step.Num.NoBranch)){
			sid <- rubric$Step.Num.NoBranch
			sid.type <- "Step.Num.NoBranch"
		} else if (!is.null(rubric$Step.Num)){
			sid <- rubric$Step.Num
			sid.type <- "Step.Num"
		} else {
			print(paste("No Step Id for rubric", r))
		}

		df <- obj[obj[,sid.type]==sid&obj[,pid.type]==pid,]
		if (nrow(df) == 0){
			print(paste("No matches for",sid.type,sid,"or",pid.type,pid))
		} else {
			completed <- FALSE
			while (!completed){
				rnum = sample(1:nrow(df),1)
				row = df[rnum,]
				print(paste("row #:", rnum, "Step.Work.Id", df[rnum,"Step.Work.Id"]))
				print(as.wiseSW(row))
				print(paste("Score:",score(row,score.rubrics,as.data.frame.out=FALSE)))
				cat("1: see more\n0: next item\n")
				val = scan(nmax=1)
				if (val == 0){
					completed <- TRUE
				}
			}
		}
	}
}
#test.Rubric(gcc.ams, rubrics.standard, start.item=1)
