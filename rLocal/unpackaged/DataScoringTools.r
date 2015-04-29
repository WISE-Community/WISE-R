
### fOR BUILDING RUBRICS
createRuleDataFrame.grapher <- function(seriesId=""){
	df = data.frame(label=character(),x1.min=numeric(),x1.max=numeric(), y1.min=numeric(), y1.max=numeric(), x2.min=numeric(), x2.max=numeric(), y2.min=numeric(),y2.max=numeric(), width.min = numeric(), width.max = numeric(), height.min = numeric(), height.max = numeric(), rotation.min=numeric(), rotation.max=numeric(), angle.min=numeric(), angle.max=numeric(), xbounds.min = numeric(), xbounds.max = numeric(), ybounds.min = numeric(), ybounds.max = numeric(), npoints.min = numeric(), npoints.max = numeric(), return.to.point = numeric(), relation = character(), collapse.inline.degrees = numeric(), stringsAsFactors=FALSE)
	if (nchar(seriesId) > 0) df[,paste("seriesId",seriesId,sep=".")] = character()
	return (df)
}
addNewRule.grapher <- function(rules, seriesId="", label="",x1.min=NA, x1.max=NA, y1.min=NA, y1.max=NA, x2.min=NA, x2.max=NA, y2.min=NA, y2.max=NA, width.min = NA, width.max = NA, height.min = NA, height.max = NA, rotation.min=NA, rotation.max=NA, angle.min=NA, angle.max=NA, xbounds.min = NA, xbounds.max = NA, ybounds.min = NA, ybounds.max = NA, npoints.min = NA, npoints.max = NA, return.to.point = NA, relation = "", collapse.inline.degrees = NA, fixed = NA){
	n = nrow(rules)+1
	rules[n,] = NA
	rules$label[n]=label; rules$x1.min[n]=x1.min; rules$x1.max[n]=x1.max; rules$y1.min[n]=y1.min; rules$y1.max[n]=y1.max; rules$x2.min[n]=x2.min; rules$x2.max[n]=x2.max; rules$y2.min[n]=y2.min; rules$y2.max[n]=y2.max; rules$width.min[n] = width.min; rules$width.max[n] = width.max; rules$height.min[n] = height.min; rules$height.max[n] = height.max; rules$rotation.min[n] = rotation.min; rules$rotation.max[n] = rotation.max; rules$angle.min[n] = angle.min; rules$angle.max[n] = angle.max; rules$xbounds.min[n] = xbounds.min; rules$xbounds.max[n] = xbounds.max; rules$ybounds.min[n] = ybounds.min; rules$ybounds.max[n] = ybounds.max; rules$npoints.min[n] = npoints.min; rules$npoints.max[n] = npoints.max; rules$return.to.point[n] = return.to.point; rules$relation[n] = relation; rules$fixed[n] = fixed; rules$collapse.inline.degrees[n] = collapse.inline.degrees
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
createRuleDataFrame.table <- function(){
	df = data.frame(label=character(), row.index=numeric(), column.index=numeric(), value.min=numeric(), value.max = numeric(), text=character(), fixed=logical(), stringsAsFactors=FALSE)
	return (df)
}
addNewRule.table <- function(rules, label="", row.index=NA, column.index=NA, value.min=NA, value.max=NA, text="", relation="", fixed=FALSE){
	n <- nrow(rules)+1
	rules[n,] <- NA
	rules$label[n]<-label; rules$row.index[n]=row.index; rules$column.index[n]=column.index; rules$value.min[n]=value.min; rules$value.max[n]=value.max; rules$text[n]=text; rules$relation[n]=relation; rules$fixed[n]=fixed;
	return(rules)
}
createRuleDataFrame.mysystem <- function(){
	df = data.frame(label=character(), StartNode = character(), EndNode = character(), Link = character(), require.All=logical(), stringsAsFactors=FALSE)
	return (df)
}
addNewRule.mysystem <- function(rules, label="", StartNode = ".*", EndNode = ".*", Link = ".*", require.All=TRUE){
	rules = rbind(rules, data.frame(label=label, StartNode = StartNode, EndNode = EndNode, Link = Link, require.All=require.All, stringsAsFactors=FALSE))
	return(rules)
}

createRuleDataFrame.MatchSequence <- function(){
	df = data.frame(label=character(), fieldText=character(), isTargetBucket=character(), fieldIdentifier=character(), choiceText=character(), choiceBucket=character(), choiceIdentifier=character(), stringsAsFactors=FALSE)
	return (df)
}
addNewRule.MatchSequence <- function(rules, label="", fieldText="", isTargetBucket="", fieldIdentifier="", choiceText="", choiceBucket="", choiceIdentifier="", fixed=TRUE){
	n <- nrow(rules)+1
	rules[n,] <- NA
	rules$label[n]<-label; rules$fieldText[n]<-fieldText; rules$isTargetBucket[n]=isTargetBucket; rules$fieldIdentifier[n]=fieldIdentifier; rules$choiceText[n]=choiceText; rules$choiceBucket[n]=choiceBucket; rules$choiceIdentifier[n]=choiceIdentifier;
	rules$fixed[n]=fixed;
	return(rules)
}

createScoreDataFrame <- function(){
	return (data.frame(pattern = character(), score = numeric(), stringsAsFactors=FALSE))
}

addNewScore <- function(scores, pattern="", score = NA, rules = NULL){
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
			} else if (nchar(id) == 0){
				# do nothing
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
addNewRubric <- function(rubrics, type, rubric, Parent.Project.Id=NULL, Project.Id=NULL, Run.Id=NULL, Step.Num=NULL, Step.Num.NoBranch=NULL, Step.Id=NULL){
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


### score a wise dataframe based on a given rubric
### All rubrics are gathered together in a list, each element of the rubrics list has two parts
### Step.Num: A vector of step numbers to apply the rubric to
### type: indicates what kind of rubric this is, processing of this type of rubric should be hard-coded
### rubric: A rubric that is recognized by the specific step type to assign a score
score <- function (obj, ...) UseMethod ("score");
score.default <- function (obj, ...){return(NA);}
score.wisedata.frame <- function (obj, score.rubrics, as.data.frame.out = TRUE, out.colName = "Research.Score", file.out = NULL, overwrite.all=FALSE, DEBUG=FALSE, ...){
	index = which("Index" == names(obj));
	obj.sub = obj
	# iterate threw rows of subset score each value and put in 
	scores = numeric();
	for (r in 1:nrow(obj.sub)){
		## is there a rubric for this step number?
		found_item = list()
		for (l in score.rubrics){
			if(DEBUG)print(paste(obj.sub$Step.Num[r], "?in?", l$Step.Num, obj.sub$Step.Num.NoBranch[r], "?in?", l$Step.Num.NoBranch, obj.sub$Step.Id[r], "?in?", l$Step.Id, obj.sub$Parent.Project.Id[r], "?in?",  l$Parent.Project.Id, obj.sub$Project.Id[r], "?in?", l$Project.Id,  obj.sub$Run.Id[r], "?in?", l$Run.Id))
			if (is.list(l) && ((!is.null(l$Step.Num) && obj.sub$Step.Num[r] %in% l$Step.Num) || (!is.null(l$Step.Num.NoBranch) && obj.sub$Step.Num.NoBranch[r] %in% l$Step.Num.NoBranch) || (!is.null(l$Step.Id) && obj.sub$Step.Id[r] %in% l$Step.Id)) && ((!is.null(l$Parent.Project.Id) && obj.sub$Parent.Project.Id[r] %in% l$Parent.Project.Id) || (!is.null(l$Project.Id) && obj.sub$Project.Id[r] %in% l$Project.Id) || (!is.null(l$Run.Id) && obj.sub$Run.Id[r] %in% l$Run.Id))){
				
				found_item = l
				break;
			}
		}
		if (length(found_item) > 0){
			sw <- as.wiseSW(obj.sub[r,])
			s = score(obj=sw, found_item$type, found_item$rubric, ...);
			if (!is.na(s) && s == "break") break;
		} else {
			if (overwrite.all || !(out.colName %in% names(obj))){
				s = NA
			} else {
				s = obj.sub[r,out.colName]
			}
		}
		### in the case of an assessment list (possibly others) there may be a list of scores
		### in this case, if scores is not already a matrix, recode into matrix form
		if (is.matrix(scores)){
			if (ncol(scores) >= length(s)){
				scores = rbind(scores, c(s, rep(NA, ncol(scores) - length(s))))	
			} else {
				scores = cbind(scores, matrix(NA, nrow(scores), length(s)-ncol(scores)))
				scores = rbind(scores, s);
			}
		} else {
			if (length(s) == 0){
				scores = c(scores, s);
			} else {
				### transform
				scores = cbind(scores, matrix(NA, length(scores), length(s)-1))
				scores = rbind(scores, s)
			}
		}		
	}
	### update data frame
	dfClass = class(obj)
	sindex = which(out.colName == names(obj));
	if (is.matrix(scores) && ncol(scores) > 1){
		## rename out.colName to Part.1 and then add new columns
		names(obj)[sindex] = paste(out.colName,"Part.1",sep=".")
		for (i in 2:ncol(scores)){
			obj = cbind(obj, tempCol = rep(NA, nrow(obj)))
			sindex = c(sindex, which(names(obj)=="tempCol"))
			names(obj)[which(names(obj)=="tempCol")] = paste(out.colName,"Part", i, sep=".")
		}
	}
	obj[obj$Index %in% obj.sub$Index,sindex] = scores;
	class(obj) = dfClass

	### should we write scores to an external file? if so provide some additional information for re-integration
	if (!is.null(file.out)){
		interleave <- function(v1,v2){ord1 <- 2*(1:length(v1))-1;ord2 <- 2*(1:length(v2));c(v1,v2)[order(c(ord1,ord2))]}
		obj.out = subset(obj, TRUE, c(which(names(obj)=="Workgroup.Id"),which(names(obj)=="Step.Work.Id"),interleave(grep("Student.Work",names(obj)), grep("Research.Score",names(obj)))))
		write.xlsx(obj.out, file.out);
	}
	if (as.data.frame.out){
		return (obj);
	} else {
		return (as.numeric(scores));
	}
}
score(row.save,score.rubrics=rubrics.standard,as.data.frame.out=FALSE, DEBUG=TRUE)

#score(row,score.rubrics=rubrics.standard,as.data.frame.out=FALSE, DEBUG=TRUE)

### For assessment list score.type tells us whether we will "sum", "mean", or leave results as "separate"
### item.type is in the rubric and tells us whether this an OpenResponse or MultipleChoice type item, delegates to appropriate function
score.wiseSW.AssessmentList <- function (obj, score.type, score.rubric, revision.number = 999, DEBUG=FALSE, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up <- FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]
		if(DEBUG)print(score.rubric)
		s = numeric();
		if (length(obj.rev) > 0){
			for (i in 1:length(obj.rev)){
				sobj = obj.rev[i] # each element of vector is one column of student work
				if (length (score.rubric) >= i){
					sscore.rubric = score.rubric[[i]]
					#print(sscore.rubric)
					if (length(sscore.rubric) > 0 && !is.null(sscore.rubric$item.type)){
						score = do.call(paste("score.wiseSW",sscore.rubric$item.type,sep="."), list(sobj, sscore.rubric$score.type, sscore.rubric, ...))
						s = c(s, score)
					} else {
						s = c(s, NA)
					}
				} else {
					s = c(s, NA)
				}	
			}
			if (!missing(score.type)){
				if (score.type=="sum"){
					score <- sum(s, na.rm=TRUE)
				} else if (score.type=="mean"){
					score <- mean(s, na.rm=TRUE)
				} else {
					score <- s
				}
			} else {
				score <- s
			}
		} 
		
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}
#score(row,score.rubrics=rubrics.standard,as.data.frame.out=FALSE, DEBUG=TRUE)

### may need library(RCurl) for url responses
score.wiseSW.OpenResponse <- function (obj, score.type, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA);
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]
		ans <- obj.rev$data
		if (!missing(score.type)){
			if (score.type == "auto" && !is.null(obj.rev$Auto.Score) && sum(!is.na(obj.rev$Auto.Score)) > 0){
				score <- max(obj.rev$Auto.Score,na.rm=TRUE)
			} else if (score.type == "nwords"){
				s = strsplit(ans, " +")[[1]];
				wc = length(s[nchar(s)>0])
				score <- wc
			} else if (score.type == "min.words"){
				s = strsplit(ans, " +")[[1]];
				wc = length(s[nchar(s)>0])
				if (wc >= score.rubric$min.words){
					score <- score.rubric$weight
				} else {
					score <- 0
				}
			} else if (score.type == "url"){
				if (length(ans) > 0){
					library(RCurl)
					if (!is.null(score.rubric$gsubPattern)&&!is.null(score.rubric$gsubReplace)) ans=gsub(score.rubric$gsubPattern,score.rubric$gsubReplace,ans)
					url = score.rubric$urlStart
					url = paste(url,gsub(" ","+",ans),sep="")
					if (!is.null(score.rubric$urlEnd)) url = paste(url, score.rubric$urlEnd, sep="")
					h = basicTextGatherer()
					curlPerform(url=url, transfertext=TRUE, verbose=FALSE,writefunction = h$update)
					str = h$value()
					str = gsub("\"","",str)
					str = gsub("\\n","",str)
					match = regexpr(score.rubric$scorePattern,str)
					score <- as.numeric(substr(str,match[[1]]+nchar(sub("\\\\d\\+","",score.rubric$scorePattern)),match[[1]]+attr(match,"match.length")-1))
				} 
			} else if (score.type == "skip"){
				### leave scores the way they are
			} else if (score.type == "hand"){
				textplot(strwrap(ans, width=dev.size(units="px")[1]/8), cex=1, fixed.width=FALSE)
				score = readline("Score the Text in the graphics device: \n");
				if (score == "end" || score == "break"){
					return ("break");
				}
				score <- suppressWarnings(as.numeric(score))
			} 		
		} else {
			if (!is.null(obj.rev$Auto.Score) && sum(!is.na(obj.rev$Auto.Score)) > 0){
				score <- max(obj.rev$Auto.Score,na.rm=TRUE)
			} 
		} 
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

score.wiseSW.Note <- function (obj, score.type, score.rubric, ...){
	return(score.wiseSW.OpenResponse(obj, score.type, score.rubric, ...))
}
#score(row, score.rubrics = rubrics.2013.cr, as.data.frame.out = FALSE, parse.colNames = "Spoon.Response")

score.wiseSW.MultipleChoice <- function (obj, score.type = NULL, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]
		ans <- obj.rev[1]
		if (!is.null(score.rubric$pattern)){
			scores = numeric()
			for (p in 1:length(score.rubric$pattern)){
				pattern = score.rubric$pattern[p]
				b = grepl(pattern, ans)
				if (b){
					if (!is.null(score.rubric$weight[p])) scores = c(scores, score.rubric$weight[p])
					else scores = c(scores, 1)
				} else {
					scores = c(scores, 0)
				} 
			}
			score <- max(scores, na.rm=TRUE)
		}
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

## If revision.number is 0 will find the first non-NA score
score.wiseSW.Mysystem2 <- function (obj, score.type, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]
		if (!missing(score.type)){
			if (score.type == "auto"){
				if (!is.null(obj.rev$Auto.Score) && length(obj.rev$Auto.Score) > 0){
					score <- obj.rev$Auto.Score
				} 
			} else if (score.type == "ruleRubric") {
				if (!is.null(obj.rev$table) && length(obj.rev$table) > 0 ){
					if (nrow(obj.rev$table) > 0){
						score <- scoreMysystem2.ruleRubric(obj.rev$table, score.rubric, ...)
					} 
				} 
			} 
		} 
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

score.wiseSW.Sensor <- function (obj, score.type, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]
		## make sure there are data points to score
		if (!is.null(obj.rev$predictions) && length(obj.rev$predictions) >= 0){
			if (!missing(score.type)){
				prediction <- obj.rev$predictions
				if (score.type == "ruleRubric"){
					score <- scoreGraph.ruleRubric(prediction, score.rubric)
				} else if (score.type == "rawpoints"){
					score <- scoreGraph.rawpoints(prediction, score.rubric)
				} else if (score.type == "countpoints"){
					score <- nrow(prediction)
				} else if (score.type == "rmse"){
					score <- scoreGraph.rmse(prediction, score.rubric)
				} 
			}
		}
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

score.wiseSW.Grapher <- function (obj, score.type, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]
		## make sure there are data points to score
		if (!is.null(obj.rev$predictions) && nrow(obj.rev$predictions) >= 0){
			if (!missing(score.type)){
				if (score.type == "ruleRubric"){
					score <- scoreGraph.ruleRubric(obj.rev$predictions, score.rubric, ...)
				} else if (score.type == "rawpoints"){
					score <- scoreGraph.rawpoints(obj.rev$predictions, score.rubric, ...)
				} else if (score.type == "countpoints"){
					score <- nrow(obj.rev$predictions, ...)
				} else if (score.type == "rmse"){
					score <- scoreGraph.rmse(obj.rev$predictions, score.rubric, ...)
				} 
			} 
		}
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}
#x <- score(wise[i,], score.rubric = rubrics.standard, as.data.frame.out=FALSE,DEBUG=FALSE)

score.wiseSW.CarGraph <- function (obj, score.type = NULL, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]

		if (is.null(obj.rev) || length(obj.rev) == 0) return (NA)

		args = list(...)
		if (is.null(args$DEBUG)){
			DEBUG = FALSE
		} else {
			DEBUG = eval(args$DEBUG)
		}

		### observation.count: looks for an Action (Start, Stop, GraphPressed) with certain requirements (min, max, length of t and x)
		   					# and counts number of these actions.
		   					# If more than one Action will look for a sequence of these actions and then start over.
		### observation.t.length, .x.length: same as above but finds total duration(t) of action or total length of x
		if (grepl("observation", score.type)){			
			obs = obj.rev$observations			
			if (!is.null(obs) && nrow(obs) > 0 && !is.null(score.rubric$Action.type) && length(score.rubric$Action.type) > 0){
				score = 0
				t.length.total = 0
				x.length.total = 0
				index = 1
				action = score.rubric$Action.type[index]
				action.found = FALSE
				o = 1
				while(o < nrow(obs)){
					a = obs$Action[o]
					if (!action.found){
						if (!is.na(a) && grepl(action,a)){
							if(DEBUG) print(paste("Action found at",o))
							if(DEBUG) print(obs[o,])
							action.found = TRUE
							t.min = obs$t[o]
							t.max = obs$t[o]
							t.length = 0
							x.min = obs$x[o]
							x.max = obs$x[o]
							x.length = 0
						}
						o = o + 1
					} else if (action.found){
						if (is.na(t.min)) t.min = obs$t[o]
						t.max = obs$t[o]
						t.length = t.max - t.min
						if (is.na(x.min) || (!is.na(obs$x[o]) && obs$x[o] < x.min)) x.min = obs$x[o]
						if (is.na(x.max) || (!is.na(obs$x[o]) && obs$x[o] > x.max)) x.max = obs$x[o]
						x.length = x.max - x.min

						if ( (!is.na(a) && !grepl(action,a)) || obs$Index[o] != (obs$Index[o-1]+1)){
							if(DEBUG) print(paste("Action stopped at",o))
							if(DEBUG) print(obs[o,])
							# done, does it qualify
							if ((is.null(score.rubric$t.min) || length(score.rubric$t.min) < index || is.na(score.rubric$t.min[index]) || score.rubric$t.min[index] <= t.min) &&
								(is.null(score.rubric$t.max) || length(score.rubric$t.max) < index || is.na(score.rubric$t.max[index]) || score.rubric$t.max[index] >= t.max) &&
								(is.null(score.rubric$t.duration.min) || length(score.rubric$t.duration.min) < index || is.na(score.rubric$t.duration.min[index]) || score.rubric$t.duration.min[index] <= t.duration) &&
								(is.null(score.rubric$t.duration.max) || length(score.rubric$t.duration.max) < index || is.na(score.rubric$t.duration.max[index]) || score.rubric$t.duration.max[index] >= t.duration) &&
								(is.null(score.rubric$x.min) || length(score.rubric$x.min) < index || is.na(score.rubric$x.min[index]) || score.rubric$x.min[index] <= x.min) &&
								(is.null(score.rubric$x.max) || length(score.rubric$x.max) < index || is.na(score.rubric$x.max[index]) || score.rubric$x.max[index] >= x.max) &&
								(is.null(score.rubric$x.duration.min) || length(score.rubric$x.duration.min) < index || is.na(score.rubric$x.duration.min[index]) || score.rubric$x.duration.min[index] <= x.duration) &&
								(is.null(score.rubric$x.duration.max) || length(score.rubric$x.duration.max) < index || is.na(score.rubric$x.duration.max[index]) || score.rubric$x.duration.max[index] >= x.duration)
							){
								if ( (index + 1) > length(score.rubric$Action.type)){
									score = score + 1
									if(!is.na(t.length)) t.length.total = t.length.total + t.length
									if(!is.na(x.length)) x.length.total = x.length.total + x.length
									index = 1
								} else {
									index = index + 1
								}
								action = score.rubric$Action.type[index]
							}
							## don't increment o index because we need to check the current action
							action.found = FALSE
						} else {
							o = o + 1
						}
					}
				}
				if (action.found){
					if(DEBUG) print(paste("Action stopped at",o))
					if(DEBUG) print(obs[o,])
					# done, does it qualify
					if ((is.null(score.rubric$t.min) || length(score.rubric$t.min) < index || is.na(score.rubric$t.min[index]) || score.rubric$t.min[index] <= t.min) &&
						(is.null(score.rubric$t.max) || length(score.rubric$t.max) < index || is.na(score.rubric$t.max[index]) || score.rubric$t.max[index] >= t.max) &&
						(is.null(score.rubric$t.duration.min) || length(score.rubric$t.duration.min) < index || is.na(score.rubric$t.duration.min[index]) || score.rubric$t.duration.min[index] <= t.duration) &&
						(is.null(score.rubric$t.duration.max) || length(score.rubric$t.duration.max) < index || is.na(score.rubric$t.duration.max[index]) || score.rubric$t.duration.max[index] >= t.duration) &&
						(is.null(score.rubric$x.min) || length(score.rubric$x.min) < index || is.na(score.rubric$x.min[index]) || score.rubric$x.min[index] <= x.min) &&
						(is.null(score.rubric$x.max) || length(score.rubric$x.max) < index || is.na(score.rubric$x.max[index]) || score.rubric$x.max[index] >= x.max) &&
						(is.null(score.rubric$x.duration.min) || length(score.rubric$x.duration.min) < index || is.na(score.rubric$x.duration.min[index]) || score.rubric$x.duration.min[index] <= x.duration) &&
						(is.null(score.rubric$x.duration.max) || length(score.rubric$x.duration.max) < index || is.na(score.rubric$x.duration.max[index]) || score.rubric$x.duration.max[index] >= x.duration)
					){
						if ( (index + 1) > length(score.rubric$Action.type)){
							score = score + 1
							if(!is.na(t.length)) t.length.total = t.length.total + t.length
							if(!is.na(x.length)) x.length.total = x.length.total + x.length
						}
					}
				}
				if (grepl("count", score.type)){	
					# score already captured above
				} else if (grepl("t.length", score.type)){
					score <- t.length.total
				} else if (grepl("x.length", score.type)){
					score <- x.length.total
				} 
			} 
		} 

		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

score.wiseSW.Table <- function (obj, score.type = NULL, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]

		if (!missing(score.type)){
			if (score.type == "ruleRubric"){
				return (scoreTable.ruleRubric(obj.rev$table, score.rubric));
			} else {
				return (NA);
			}
		} else {
			return (NA);
		}
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj.rev)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

score.wiseSW.MatchSequence <- function (obj, score.type = NULL, score.rubric, revision.number = 999, ...){
	if (length(obj) == 0) return (NA)
	cascade.down <- FALSE
	if (revision.number > 100){
		cascade.down <- TRUE
		revision.number <- length(obj)
	}
	cascade.up = FALSE
	if (revision.number == 0){
		cascade.up <- TRUE
		revision.number <- 1
	}
	looping <- TRUE
	while (looping){
		looping <- FALSE
		score <- NA
		obj.rev <- obj[[revision.number]]

		if (!missing(score.type)){
			if (score.type == "ruleRubric"){
				return (scoreMatchSequence.ruleRubric(obj.rev$table, score.rubric));
			} else {
				return (NA);
			}
		} else {
			return (NA);
		}
		if (cascade.up || cascade.down){
			if (!is.na(score)){
				return (score)
			} else {
				if (cascade.up){
					revision.number <- revision.number + 1
					if (revision.number > length(obj)) return (score)
				} else {
					revision.number <- revision.number - 1
					if (revision.number < 1) return (score)
				}
				looping <- TRUE
			}
		} else {
			return (score)
		}
	}
}

## if there is no function dedicated to a specific type return NA
score.wiseSW <- function (obj, score.type, score.rubric, ...){
	return (NA);
}

################# SPECIFIC FUNCTIONS

#### FUNCTIONS FOR SCORING GRAPH DATA ###########
## For scoring graphs use a rubric in the following form:
# rubric = list()
# rubric$rules = data.frame(x1.min=numeric(),x1.max=numeric(), y1.min=numeric(), y1.max=numeric(), x2.min=numeric(), x2.max=numeric(), y2.min=numeric(),y2.max=numeric(), width.min = numeric(), width.max = numeric(), height.min = numeric(), width.max = numeric(), angle.min=numeric(), angle.max=numeric())
# rubric$scores = data.frame(pattern = character(), score = numeric())
# rubric$non.functional = score [a number]
# each row of the rules dataframe provides the spatial boundaries for a given point or segment.  Will evaluate with scoreGraph.element to true or false.
# All given values are necessary, so if x (min, max), y(min, max), and slope (min, max) are all given then the points must meet all criterion
# each row of the scores dataframe includes a 'stringed' logical pattern corresponding to rows of rules dataframe, for example
#  R[1] && R[2] || !R[3]  (either row 1 and row 2 of the rules, or not rule 3)
# the other column of the scores dataframe with a numerical value
# A score will be evaluated from top-down, once a score is found the loop breaks
# New - a data frame of spatial elements is tracked for each rule, this allows 
## for relational statements such as R[1].x1 > R[2].x2 to be computed (also can be R[label])
## collapse.inline.degrees - if this flag is set to TRUE in a rule the rule will be evaluated
##   over all next points that do not deviate from the slope (in degrees) of the first segment. Useful if students are plotting slope by unitizing x-axis.
scoreGraph.ruleRubric <- function (xy, rubric, invertIfBackwards=TRUE, debug=FALSE){
	if (is.na(xy) || nrow(xy) == 0) return (NA)

	non.critical.labels = c("label", "xbounds.min", "xbounds.max", "ybounds.min", "ybounds.max", "return.to.point", "collapse.inline.degrees", "relation", "seriesId")
	## remove adjacent identical rows
	duplicates = c(FALSE)
	if (nrow(xy) > 1){
		for (i in 2:nrow(xy)){
			duplicates = c(duplicates,duplicated(xy[(i-1):i,])[2])
		}
	}
	xy = xy[!duplicates,]
	## remove rows with NA
	xy = xy[!is.na(xy$x)&!is.na(xy$y),]
	if (invertIfBackwards){
		# in case user started from right and went to left
		if (sum(diff(xy$x)<0) == (nrow(xy)-1)){
			xy = xy[rev(rownames(xy)),]
		}
	}
	if(debug)print(xy)
	# if there is a seriesId in the rubric only include those that match from xy
	if (!is.null(rubric$rules$seriesId) && !is.numeric(xy[,1])){
		xy[,1] = as.character(xy[,1])
		xy = xy[xy[,1]%in%rubric$rules$seriesId[1],]
	}

	#print(rubric)
	R = rep(FALSE, nrow(rubric$rules))
	cp = 1; ### we start looking from the first point in xy, but once that is found we don't search it again, we don't go backwards
	RVALS = data.frame(label=character(), x1=numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric(), width = numeric(), height = numeric(), rotation = numeric(), angle = numeric(), npoints = numeric()) 
	for (r in 1:nrow(rubric$rules)){
		rule = rubric$rules[r,]
		if (!is.null(rubric$rules[r, "collapse.inline.points"])&&!is.na(rubric$rules[r, "collapse.inline.points"])) {collapse = rubric$rules[r, "collapse.inline.points"]}
		else {collapse = FALSE}

		# if this rule has a "return.to.point" value set, we will begin looking at the corresponding point
		if (!is.null(rule$return.to.point) && !is.na(rule$return.to.point)){
			if (rule$return.to.point > nrow(xy)){ 
				cp <- nrow(xy)
			} else { 
				cp <- rule$return.to.point
			}
		}
		# initial checks:
		# first check whether there is between min and max points
		if (((!is.null(rule$npoints.min) && !is.na(rule$npoints.min) && nrow(xy) < rule$npoints.min) || (!is.null(rule$npoints.max) && !is.na(rule$npoints.max) && nrow(xy) > rule$npoints.max))
		){
			matchFound = FALSE
		} else {
			## search each adjacent pair of points for a hit of these rules
			matchFound = FALSE
			## are we ONLY checking npoints?
			npoints.only = TRUE
			for (ci in 1:ncol(rule)){
				if (names(rule)[ci] == "npoints.min" || names(rule)[ci] == "npoints.max"){
					if (is.na(rule[1,ci])){npoints.only=FALSE; break}
				} else if (!(names(rule)[ci] %in% non.critical.labels)){
					if (!is.na(rule[1,ci])){npoints.only=FALSE; break}
				}
			}
			if (npoints.only){
				matchFound = TRUE ## remember, we've already checked that we've met npoint criteria
				matchedValues = data.frame()
			}

			## do we have more points to check?
			if (cp <= nrow(xy)){
				#print(paste("nrow xy", nrow(xy), "cp", cp))
				for (p in cp:nrow(xy)){
					x1 = xy$x[p];
					y1 = xy$y[p];
					x2 = NA;
					y2 = NA;
					if (p+1 <= nrow(xy)){
						x2 = xy$x[p+1]
						y2 = xy$y[p+1]
					}
					### if collapsing across multiple points loop until we exceed threshold
					if (collapse && !is.na(x2) && !is.na(y2)){
						degrees = atan(((y2-y1)/(rubric$rules[r, "ybounds.max"]-rubric$rules[r, "ybounds.min"]))/((x2-x1)/(rubric$rules[r, "xbounds.max"]-rubric$rules[r, "xbounds.min"])))
						if (debug) print(paste("degrees of first segment"), degrees)
						while (p+2 <= nrow(xy)){
							tx2 = xy$x[p+2]
							ty2 = xy$y[p+2]
							tdegrees = atan(((ty2-y2)/(rubric$rules[r, "ybounds.max"]-rubric$rules[r, "ybounds.min"]))/((tx2-x2)/(rubric$rules[r, "xbounds.max"]-rubric$rules[r, "xbounds.min"])))
							if (debug) print(paste("degrees of next segment"), degrees, "diff of", abs(degrees - tdegrees))
							if (abs(degrees - tdegrees) < rubric$rules[r, "collapse.inline.points"]){
								x2 = tx2
								y2 = ty2
								p = p + 1
							} else {
								break
							}							
						}
					}
					x3 = NA;
					y3 = NA;
					if (p+2 <= nrow(xy)){
						x3 = xy$x[p+2]
						y3 = xy$y[p+2]
					}
					#print(rule)
					matchedValues = scoreGraph.element(x1, y1, x2, y2, x3, y3, rule, debug)
					#print(matchFound)
					if (!is.null (matchedValues)){
						matchFound = TRUE
						## advance if we are not just looking at absolute position of first point
						#print(paste(is.na(rule$x1.min) , is.na(rule$x1.max) , is.na(rule$y1.min) , is.na(rule$y1.max) , !is.na(rule$x2.min) , !is.na(rule$x2.max) , !is.na(rule$y2.min) , !is.na(rule$y2.max) , !is.na(rule$width.min) , !is.na(rule$width.max) , !is.na(rule$height.min) , !is.na(rule$height.max) , !is.na(rule$angle.min) , !is.na(rule$angle.max)))
						if (!is.na(rule$x2.min) || !is.na(rule$x2.max) || !is.na(rule$y2.min) || !is.na(rule$y2.max) || !is.na(rule$width.min) || !is.na(rule$width.max) || !is.na(rule$height.min) || !is.na(rule$height.max) || !is.na(rule$rotation.min)&&!is.null(rule$rotation.min) || !is.na(rule$rotation.max)&&!is.null(rule$rotation.max) || !is.na(rule$angle.min) || !is.na(rule$angle.max)){
							if (debug) print("increment")
							cp = p + 1;
						}
						break;
					}
				}
			}
		}

		if (matchFound){
			rval = data.frame(label=rule$label, x1=NA, y1 = NA, x2 = NA, y2 = NA, width = NA, height = NA, rotation = NA, angle = NA, npoints = nrow(xy)) 
			# go through each key of the matchedValues and put in this
			for (key in names(matchedValues)){
				rval[1,key] = matchedValues[1,key]
			}
		} else {
			rval = data.frame(label=rule$label, x1=NA, y1 = NA, x2 = NA, y2 = NA, width = NA, height = NA, rotation = NA, angle = NA, npoints = NA) 	
		}
		RVALS = rbind(RVALS, rval)
		R[r] = matchFound
		
	}
	RVALS <- cbind(RVALS, match.found=R)
	# pass through rules again to see if there are any relational type rules
	for (r in 1:nrow(rubric$rules)){
		rule = rubric$rules[r,]
		if (!is.null(rule$relation) && nchar(rule$relation) > 0){
			pattern = rule$relation
			# replace R[  with RVALS[
			pattern = gsub("R[","RVALS[", pattern, fixed=TRUE)
			# Replace labels in pattern with row index
			ids = character()
			repls = list()
			greg = gregexpr("\\[.*?[a-z]+.*?[\\,| ]",pattern)[[1]]
			if (length(greg) > 0){
				for (g in 1:length(greg)){
					id = substr(pattern, start=greg[g]+1, stop=greg[g]+attr(greg,"match.length")[g]-2)
					# update, this was originally only looking for a single match, now can find multiple
					if (!is.na(rule$fixed) && !rule$fixed){
						repl <- grep(id, rubric$rules$label)
					} else {
						repl <- which(rubric$rules$label == id)
					}
					if (length(repl) > 0){
						ids = c(ids, id)
						repls[[length(repls)+1]] <- repl
					}
				}
				for (i in 1:length(ids)){
					pattern = gsub(paste("[",ids[i],",",sep=""),paste("[c(",paste(repls[[i]],collapse=","),"),",sep=""),pattern, fixed=TRUE)
				}
			}
			# Surround the column index with single quotes (make sure it is after c(),)
			pattern = gsub("\\)\\, *","),'", pattern)
			pattern = gsub("\\]","']", pattern)
			if (debug) print(pattern)
			val = as.logical(eval(parse(text=pattern)))
			if (is.na(val)) val = FALSE
			R[r] = val
			RVALS[r,"match.found"] <- val
			if (debug) print(paste(R[r]))
		}
	}	

	if (debug) print(RVALS)
	for (s in 1:nrow(rubric$scores)){
		patternResult = eval(parse(text=rubric$scores$pattern[s]))
		## if is logical and TRUE return a given score, if numerical just return value 
		if (!is.na(patternResult) && is.logical(patternResult) && patternResult){
			return (rubric$scores$score[s]);
		} else if (is.numeric(patternResult)) {
			return (patternResult);
		}
	}
	### No rule found 
	return (1)
}

# Score an xy graph by finding the root mean square error from expected
# If there are more than one expected graphs then the minimum root mean square will be returned
scoreGraph.rmse <- function (xy, expectedList, rm.nonfunctional=TRUE, debug=FALSE){
	if(debug==TRUE) print(xy)
	if (nrow(xy) < 2) return (Inf)
	if (rm.nonfunctional){
		dif = which(diff(xy$x)<=0)
		if (length(dif)>0){
			dif = dif[1]
			# in the case where we are taking away all but one, keep the second points
			if (dif == 1 && xy$x[1] != xy$x[2]) dif = 2
			xy = xy[1:dif,]
		}
	}
	if (nrow(xy) < 2) return (Inf)
	if (is.data.frame(expectedList)) expectedList = list(expectedList)
	rmse.out = Inf
	for (l in expectedList){
		exy = data.frame(x=l$x, y=l$y)
		#if(debug==TRUE) print(exy)
		# if there are less than 100 points in the expected, interpolated
		if (nrow(exy) < 100){
			elxy = approx(x=exy$x, y=exy$y, n=100)
			exy = data.frame(x=elxy$x, y=elxy$y)
		}

		lxy = approx(x=xy$x, y=xy$y, xout=exy$x)
		oxy = data.frame(x=lxy$x, y=lxy$y)
		#ssres = sum((oxy$y - exy$y)^2)
		#sstot = sum((oxy$y - mean(oxy$y))^2)
		#rsq = 1 - (ssres/sstot)
		rmse = sqrt(sum((oxy$y - exy$y)^2,na.rm=TRUE)/nrow(oxy))
		if (debug)print(rmse)
		if (!is.na(rmse) && rmse < rmse.out) rmse.out = rmse
	}
	return (rmse.out)
}

#### FUNCTIONS FOR SCORING MYSYSTEM2 DATA ###########
## For scoring graphs use a rubric in the following form:
# rubric = list()
# rubric$scores = data.frame(pattern = character(), score = numeric())
# rubric$rules = data.frame(StartNode = character(), EndNode = character(), Link = character())
#
# each row of the rules dataframe provides a regular expression to match against StartNode, EndNode, and Link
# All given values are necessary, so use ".*" to match anything if, for example, you don't care about the endnode
# each row of the scores dataframe includes a 'stringed' logical pattern corresponding to rows of rules dataframe, for example
#  R[1] && R[2] || !R[3]  (either row 1 and row 2 of the rules, or not rule 3)
# the other column of the scores dataframe with a numerical value
# A score will be evaluated from top-down, once a score is found the loop breaks
scoreMysystem2.ruleRubric <- function (table, rubric, debug=FALSE){
	# if empty table or no Links return 1
	if (is.na(table) || nrow(table) == 0 || (unique(table$Link) == 1 && nchar(table$Link[1]) == 0)) return (NA)
	R = logical();
	if (debug) print(rubric)
	if (debug) print(table)
	for (cr in 1:nrow(rubric$rules)){
		rule = rubric$rules[cr,]
		matchFound = FALSE
		# unlike table step order doesn't matter try every row looking for row
		if (nrow(table) > 0){
			for (r in 1:nrow(table)){
				row = table[r,]
				start.matched = grepl(rule$StartNode, row$StartNode)[1]
				end.matched = grepl(rule$EndNode, row$EndNode)[1]
				link.matched = grepl(rule$Link, row$Link)[1]
				if (rule$require.All && start.matched && end.matched && link.matched){
					matchFound = TRUE
					break	
				} else if (!rule$require.All && (start.matched || end.matched || link.matched)){
					matchFound = TRUE
					break	
				} 
			}
		}
		R[[length(R)+1]] = matchFound
	}
	if (debug) print(cbind(rubric$rules$label,R))
	for (s in 1:nrow(rubric$scores)){
		if(debug) print(paste("Attempt score pattern", s, "pattern",rubric$scores$pattern[s] ))
		patternResult = eval(parse(text=rubric$scores$pattern[s]))
		## if is logical and TRUE return a given score, if numerical just return value 
		if (is.logical(patternResult) && patternResult){
			return (as.numeric(rubric$scores$score[s]));
		} else if (is.numeric(patternResult)) {
			return (patternResult);
		}
	}
	### No rule found 
	return (1)
}

scoreTable.ruleRubric <- function (table, rubric, debug=FALSE){	
	if (is.na(table) || nrow(table) == 0) return (NA)
	R = logical();
	RVALS = data.frame();
	cp = 0; # we start looking from the first point in xy, but once that is found we don't search it again, we don't go backwards
	for (r in 1:nrow(rubric$rules)){
		rule <- rubric$rules[r,]
		if (!is.na(rule$row.index) && !is.na(rule$column.index)){
			text <- as.character(table[rule$row.index, rule$column.index])
		} else {return (NA)}
		
		matchValues <- list()
		matchFound <- TRUE
		if ((is.na(rule$text) || nchar(rule$text)==0) || grepl(rule$text, tolower(text))){
			matchValues['text'] <- text;
		} else {matchFound <- FALSE}
		
		if ((is.na(rule$value.min) || (!is.na(suppressWarnings(as.numeric(text))) && suppressWarnings(as.numeric(text)) >= rule$value.min)) && (is.na(rule$value.max) || (!is.na(suppressWarnings(as.numeric(text))) && suppressWarnings(as.numeric(text)) <= rule$value.max))) {
			matchValues['value'] <- suppressWarnings(as.numeric(as.character(text)))
		} else {matchFound <- FALSE}
		rval <- data.frame(label=rule$label, text=NA, value=NA)
		if (matchFound){
			# go through each key of the matchedValues and put in this  obj
			for (key in names(matchValues)){
				if (!is.na(matchValues[[key]])) rval[1,key] <- matchValues[[key]];
			}
		} 
		RVALS <- rbind(RVALS, rval);
		R <- c(R, matchFound)
		
	}
	RVALS <- cbind(RVALS, match.found=R)
	# pass through rules again to see if there are any relational type rules
	for (r in 1:nrow(rubric$rules)){
		rule = rubric$rules[r,]
		if (!is.null(rule$relation) && nchar(rule$relation) > 0){
			pattern = rule$relation
			if (debug) print(pattern)
			# replace R[  with RVALS[
			pattern = gsub("R[","RVALS[", pattern, fixed=TRUE)
			# Replace labels in pattern with row index
			ids = character()
			repls = list()
			greg = gregexpr("\\[.*?[a-z]+.*?[\\,| ]",pattern)[[1]]
			if (length(greg) > 0){
				if (r==2) print(greg)
				for (g in 1:length(greg)){
					id = substr(pattern, start=greg[g]+1, stop=greg[g]+attr(greg,"match.length")[g]-2)
					# update, this was originally only looking for a single match, now can find multiple
					if (!is.na(rule$fixed) && !rule$fixed){
						repl <- grep(id, rubric$rules$label)
					} else {
						repl <- which(rubric$rules$label == id)
					}
					if (length(repl) > 0){
						ids = c(ids, id)
						repls[[length(repls)+1]] <- repl
					}
				}
				for (i in 1:length(ids)){
					pattern = gsub(paste("[",ids[i],",",sep=""),paste("[c(",paste(repls[[i]],collapse=","),"),",sep=""),pattern, fixed=TRUE)
				}
			}
			# Surround the column index with single quotes (make sure it is after c(),)
			pattern = gsub("\\)\\, *","),'", pattern)
			pattern = gsub("\\]","']", pattern)
			if (debug) print(pattern)
			val = as.logical(eval(parse(text=pattern)))
			if (is.na(val)) val = FALSE
			R[r] = val
			RVALS[r,"match.found"] <- val
		}
	}	
	if (debug) print(RVALS)
	for (s in 1:nrow(rubric$scores)){
		patternResult = eval(parse(text=rubric$scores$pattern[s]))
		## if is logical and TRUE return a given score, if numerical just return value 
		if (!is.na(patternResult) && is.logical(patternResult) && patternResult){
			return (rubric$scores$score[s]);
		} else if (is.numeric(patternResult)) {
			return (patternResult);
		}
	}
	### No rule found 
	return (1)
}

scoreMatchSequence.ruleRubric <- function (table, rubric, debug=FALSE){	
	if (is.na(table) || nrow(table) == 0) return (NA)
	R = logical();
	cp = 0; # we start looking from the first point in xy, but once that is found we don't search it again, we don't go backwards
	for (r in 1:nrow(rubric$rules)){
		rule <- rubric$rules[r,]

		if ( (!is.na(rule$fieldIdentifier) && nchar(rule$fieldIdentifier) > 0) && (!is.na(rule$choiceIdentifier) && nchar(rule$choiceIdentifier) > 0)){
			if (nrow(subset(table,fieldIdentifier==rule$fieldIdentifier&choiceIdentifier==rule$choiceIdentifier)) > 0){matchFound <- TRUE} else {matchFound <- FALSE}
		} else if ( (!is.na(rule$fieldIdentifier) && nchar(rule$fieldIdentifier) > 0) && (!is.na(rule$choiceText) && nchar(rule$choiceText) > 0)){
			if (nrow(subset(table,fieldIdentifier==rule$fieldIdentifier&choiceText==rule$choiceText)) > 0){matchFound <- TRUE} else {matchFound <- FALSE}
		} else if ( (!is.na(rule$fieldText) && nchar(rule$fieldText) > 0) && (!is.na(rule$choiceIdentifier) && nchar(rule$choiceIdentifier) > 0)){
			if (nrow(subset(table,fieldText==rule$fieldText&choiceIdentifier==rule$choiceIdentifier)) > 0){matchFound <- TRUE} else {matchFound <- FALSE}
		} else if ( (!is.na(rule$fieldText) && nchar(rule$fieldText) > 0) && (!is.na(rule$choiceText) && nchar(rule$choiceText) > 0)){
			if (nrow(subset(table,fieldText==rule$fieldText&choiceText==rule$choiceText)) > 0){matchFound <- TRUE} else {matchFound <- FALSE}
		} else {
			return (NA)
		}
		if (debug)print(cbind(rule,R=matchFound))
		R <- c(R, matchFound)	
	}
	
	for (s in 1:nrow(rubric$scores)){
		patternResult = eval(parse(text=rubric$scores$pattern[s]))
		## if is logical and TRUE return a given score, if numerical just return value 
		if (!is.na(patternResult) && is.logical(patternResult) && patternResult){
			return (rubric$scores$score[s]);
		} else if (is.numeric(patternResult)) {
			return (patternResult);
		}
	}
	### No rule found 
	return (1)
}

#scoreMatchSequence.ruleRubric(NA,rubric,TRUE)

#score(row, score.rubrics=rubrics.standard, as.data.frame.out=FALSE)
#scoreTable.ruleRubric(table, rubric, TRUE)
############################################ GRAPHING ################################################################################333
### Given two points and their min and maximum values, as well as a slope (+ min and max), will find if a given pair of points meet criterion
### Now returns a data frame with the matched values, NULL if no match
#### TODO: EDIT RULES TO MAKE RESPONSIVE TO ONLY MIN WITHOUT MAX, OR VICE VERSA
scoreGraph.element <- function (x1, y1, x2=NA, y2=NA, x3=NA, y3=NA, rule, debug = FALSE){
	if(debug)print("------------------------------------------")
	if (debug)print(rule)
	if (debug)print(paste(x1,y1,x2,y2,x3,y3))
	df = data.frame()
	#if(rule$label=="rest.ex"){print(paste(x1,y1,x2,y2))}
	## just look at point 1 first
	if (!is.na(x1)){
		if (!is.na(rule$x1.min) && !is.na(rule$x1.max) && (x1 < rule$x1.min[1] || x1 > rule$x1.max[1])) {return (NULL)}
		else {df[1,'x1'] = x1}
	} else if (!is.na(rule$x1.min) || !is.na(rule$x1.max)) {
		return(NULL)
	}
	if (!is.na(y1)){
		if (!is.na(rule$y1.min) && !is.na(rule$y1.max) && (y1 < rule$y1.min[1] || y1 > rule$y1.max[1])) {return (NULL)}
		else {df[1,'y1'] = y1}
	} else if (!is.na(rule$y1.min) || !is.na(rule$y1.max)) {
		return(NULL)
	}
	if (!is.na(x2)){
		if (!is.na(rule$x2.min) && !is.na(rule$x2.max) && (x2 < rule$x2.min[1] || x2 > rule$x2.max[1])) {return (NULL)}
		else {df[1,'x2'] = x2}
	} else if (!is.na(rule$x2.min) || !is.na(rule$x2.max)) {
		return(NULL)
	}
	if (!is.na(y2)){
		if (!is.na(rule$y2.min) && !is.na(rule$y2.max) && (y2 < rule$y2.min[1] || y2 > rule$y2.max[1])) {return (NULL)}
		else {df[1,'y2'] = y2}
	} else if (!is.na(rule$y2.min) || !is.na(rule$y2.max)) {
		return(NULL)
	}

	if (!is.na(x1) && !is.na(x2)){
		if (!is.na(rule$width.min) && !is.na(rule$width.max) && ((x2 - x1) < rule$width.min[1] || (x2 - x1) > rule$width.max[1])) { return (NULL) } 
		else {df[1,'width'] = x2 - x1}
	} else if (!is.na(rule$width.min) || !is.na(rule$width.max)) {
		return(NULL)
	}

	if (!is.na(y1) && !is.na(y2)){
		if (!is.na(rule$height.min) && !is.na(rule$height.max) && ((y2 - y1) < rule$height.min[1] || (y2 - y1) > rule$height.max[1])) { return (NULL) } 
		else {df[1,'height'] = y2 - y1}
	} else if (!is.na(rule$height.min) || !is.na(rule$height.max)) {
		return(NULL)
	}

	if (!is.na(x1) && !is.na(y1) && !is.na(x2) && !is.na(y2)){
		py2 = y2; py1 = y1; px2 = x2; px1 = x1;
		if (!is.na(rule$xbounds.min) && !is.na(rule$xbounds.max) && !is.na(rule$ybounds.min) && !is.na(rule$ybounds.min)){
			width = abs(x2 - x1);
			height = abs(y2 - y1);
			py2 = y2/(rule$ybounds.max-rule$ybounds.min); py1 = y1/(rule$ybounds.max-rule$ybounds.min); px2 = x2/(rule$xbounds.max-rule$xbounds.min); px1 = x1/(rule$xbounds.max-rule$xbounds.min);
			if (height != 0){
				rotation = atan((py2 - py1) / (px2 - px1)) * 180 / pi		
			} else {
				rotation = 0
			}
		} else {				
			rotation = atan((y2 - y1) / (x2 - x1)) * 180 / pi	
		}
		if (!is.null(rule$rotation.min) && !is.null(rule$rotation.max) && !is.na(rule$rotation.min) && !is.na(rule$rotation.max) && (rotation < rule$rotation.min[1] || rotation > rule$rotation.max[1])) { return (NULL) } 
		else {df[1,'rotation'] = rotation}
	} else if (!is.na(rule$rotation.min) || !is.na(rule$rotation.max)){
		return (NULL)
	}
	
	if (!is.na(x1) && !is.na(y1) && !is.na(x2) && !is.na(y2) && !is.na(x3) && !is.na(y3)){
		### if we have the bounds create the angle proportional
		py3 = y3; py2 = y2; py1 = y1; px3 = x3; px2 = x2; px1 = x1;
		if (!is.na(rule$xbounds.min) && !is.na(rule$xbounds.max) && !is.na(rule$ybounds.min) && !is.na(rule$ybounds.min)){
			width1 = abs(x2 - x1);
			height1 = abs(y2 - y1);
			width2 = abs(x3 - x2);
			height2 = abs(y3 - y2);
			#print(paste(x1, y1, x2, y2, x3, y3))
			#print(paste((rule$xbounds.max-rule$xbounds.min), (rule$ybounds.max-rule$ybounds.min)))
			py3 = y3/(rule$ybounds.max-rule$ybounds.min); py2 = y2/(rule$ybounds.max-rule$ybounds.min); py1 = y1/(rule$ybounds.max-rule$ybounds.min); px3 = x3/(rule$xbounds.max-rule$xbounds.min); px2 = x2/(rule$xbounds.max-rule$xbounds.min); px1 = x1/(rule$xbounds.max-rule$xbounds.min);
			#print(paste(px1, py1, px2, py2, px3, py3))
			if (height1 != 0){
				rotation1 = atan((py2 - py1) / (px2 - px1)) * 180 / pi		
			} else {
				rotation1 = 0
			}
			if (height2 != 0){
				rotation2 = atan((py3 - py2) / (px3 - px2)) * 180 / pi		
			} else {
				rotation2 = 0
			}
		} else {				
			rotation1 = atan((y2 - y1) / (x2 - x1)) * 180 / pi	
			rotation2 = atan((y3 - y2) / (x3 - x2)) * 180 / pi	
		}
		angle = rotation2 - rotation1
		#print(paste(angle, rotation2, rotation1))
		if (!is.na(rule$angle.min) && !is.na(rule$angle.max) && (angle < rule$angle.min[1] || angle > rule$angle.max[1])) { return (NULL) } 
		else {df[1,'angle'] = angle}	
	} else if (!is.na(rule$angle.min) || !is.na(rule$angle.max)){
		return (NULL)
	}

	if (debug)print("hit")
	return (df);
}

### Do the scores in xy correspond to a list of min and max x and y values
scoreGraph.rawpoints <- function (xy, score.rubric, debug=FALSE){
	s = 0;
	val.found = FALSE;
	x.min = score.rubric$x.min
	y.min = score.rubric$y.min
	x.max = score.rubric$x.max
	y.max = score.rubric$y.max
	len = max(length(x.min),length(x.max),length(y.min),length(y.max));
	x = xy$x; y = xy$y;
	for (i in 1:len){
		if (length(x) >= i && length(y) >= i){
			val.found = TRUE;
			correct = TRUE;
			if (!is.na(x.min[i]) && x[i] < x.min[i]) correct = FALSE;
			if (!is.na(y.min[i]) && y[i] < y.min[i]) correct = FALSE;
			if (!is.na(x.max[i]) && x[i] > x.max[i]) correct = FALSE;
			if (!is.na(y.max[i]) && y[i] > y.max[i]) correct = FALSE;
			if(debug) print(paste(correct, x[i], "[", x.min[i], x.max[i], "]", y[i], "[",y.min[i], y.max[i],"]"))
			
			if (correct){
				if (!missing(score.weights) && length(score.weights) >= i){
					s = s + score.weights[i];
				} else {
					s = s + 1;
				} 
			}
		}		
	}
	if (val.found) { return (s);}
	else {return (NA);}
}

######### Note for URL response, you can also look at a single response with url by following codde

# Given an item id and a response (r) will score using ETS' c-rater.
scoreResponseWithCRATER <- function(r, itemId, strip.pattern="\\[Response\\]=", verbose=FALSE){
	library(RCurl)
	if (verbose) print(r)
	if (!is.na(r)){
		r <- gsub(strip.pattern,"",r); 
		if (nchar(r) == 0){
			if (verbose) print(NA);
			return (NA);
		}
		vcraterurl = paste("http://cogstudies.org/vitale/hostedFiles/crater_php_files/callCRater.php?cRaterClientId=WISETEST&cRaterItemId=",itemId,"&cRaterStudentResponse=",gsub(" ", "+",r),sep="");
		h = basicTextGatherer();
		curlPerform(url=vcraterurl, transfertext=TRUE, verbose=TRUE,writefunction = h$update);
		str = h$value();
		str = gsub("\"","",str);
		str = gsub("\\n","",str);
		score = sub(".*?score=(\\d+).*", "\\1", str);
		if (verbose) print(score);
		return(as.numeric(score));
	} else {
		if (verbose) print(NA)
		return (NA);
	} 	
}
