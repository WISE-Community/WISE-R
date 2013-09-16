### score a wise dataframe based on a given rubric
### All rubrics are gathered together in a list, each element of the rubrics list has two parts
### Step.Num: A vector of step numbers to apply the rubric to
### type: indicates what kind of rubric this is, processing of this type of rubric should be hard-coded
### rubric: A rubric that is recognized by the specific step type to assign a score
score = function (obj, ...) UseMethod ("score");
score.default = function (obj, ...){return(NA);}
score.wisedata.frame = function (obj, subset=TRUE, select, drop = FALSE, score.rubrics, is.data.frame.out = TRUE, score.colName = "Research.Score", file.out = NULL, ...){
	index = which("Index" == names(obj));
	if (index < 0){
		obj = cbind(Index = 1:nrow(obj), obj);
	}
	if (!missing(select)){
		if (!missing(subset)){obj.sub = subset(obj, subset=subset, select=select, drop=drop);}
		else {obj.sub = subset(obj, subset=TRUE, select=select, drop=drop);}
	}else{
		if (!missing(subset)){obj.sub = subset(obj, subset=subset, drop=drop);}
		else {obj.sub = subset(obj, subset=TRUE, drop=drop);}
	}
	# iterate threw rows of subset score each value and put in 
	scores = numeric();
	for (r in 1:nrow(obj.sub)){
		## is there a rubric for this step number?
		found_item = list()
		for (l in score.rubrics){

			#print(paste(obj.sub$Step.Num[r], l$Step.Num, obj.sub$Step.Num[r] %in% l$Step.Num))
			if (obj.sub$Step.Num[r] %in% l$Step.Num && ((!is.null(l$Parent.Project.Id) && obj.sub$Parent.Project.Id[r] %in% l$Parent.Project.Id) || (!is.null(l$Project.Id) && obj.sub$Project.Id[r] %in% l$Project.Id))){
				#print(paste("Step Num", obj.sub$Step.Num[r]))
				#print(paste("Possible Steps", l$Step.Num))
				#if (!is.null(l$Parent.Project.Id)){
				#	print(paste("Parent Project", obj.sub$Parent.Project.Id[r]))
				#	print(paste("Possible Parent Project", l$Parent.Project.Id))					
				#}
				#if (!is.null(l$Project.Id)){
				#	print(paste("Project", obj.sub$Project.Id[r]))
				#	print(paste("Possible Project", l$Project.Id))					
				#}
				found_item = l;
				break;
			}
		}
		if (length(found_item) > 0){
			sw = as.wiseSW(obj.sub[r,]);
			#print(sw)
			#print(found_item)
			s = score(obj=sw, found_item$type, found_item$rubric);
			if (!is.na(s) && s == "break") break;
		} else {
			s = NA
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
	sindex = which(score.colName == names(obj));
	if (is.matrix(scores) && ncol(scores) > 1){
		## rename score.colName to Part.1 and then add new columns
		names(obj)[sindex] = paste(score.colName,"Part.1",sep=".")
		for (i in 2:ncol(scores)){
			obj = cbind(obj, tempCol = rep(NA, nrow(obj)))
			sindex = c(sindex, which(names(obj)=="tempCol"))
			names(obj)[which(names(obj)=="tempCol")] = paste(score.colName,"Part", i, sep=".")
		}
	}
	obj[obj$Index %in% obj.sub$Index,sindex] = scores;
	class(obj) = dfClass

	### should we write scores to an external file? if so provide some additional information for re-integration
	if (!is.null(file.out)){
		interleave = function(v1,v2){ord1 <- 2*(1:length(v1))-1;ord2 <- 2*(1:length(v2));c(v1,v2)[order(c(ord1,ord2))]}
		obj.out = subset(obj, TRUE, c(which(names(obj)=="Workgroup.Id"),which(names(obj)=="Step.Work.Id"),interleave(grep("Student.Work",names(obj)), grep("Research.Score",names(obj)))))
		write.xlsx(obj.out, file.out);
	}
	if (is.data.frame.out){
		return (obj);
	} else {
		return (scores);
	}
}

#score(wiseDF.exp[31,], score.rubrics=rubrics)

### For assessment list score.type tells us whether we will "sum", "mean", or leave results as "separate"
score.wiseSW.AssessmentList = function (obj, score.type, score.rubric, ...){
	if (length(obj) == 0) return (NA);
	s = numeric();
	for (i in 1:length(obj)){
		sobj = obj[[i]]
		if (length (score.rubric) >= i){
			sscore.rubric = score.rubric[[i]]
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
			return (sum(s, na.rm=TRUE))
		} else if (score.type=="mean"){
			return (mean(s, na.rm=TRUE))
		} else {
			return (s)
		}
	} else {
		return (s)
	}
}

score.wiseSW.OpenResponse = function (obj, score.type, score.rubric, ...){
	if (length(obj) == 0) return (NA);
	if (!missing(score.type)){
		if (score.type == "auto" && !is.null(obj$Auto.Score) && sum(!is.na(obj$Auto.Score)) > 0){
			return (max(obj$Auto.Score,na.rm=TRUE));
		} else if (score.type == "min.words"){
			ans = obj[[1]]
			ans = ans[which(nchar(ans)==max(nchar(ans)))[1]]
			s = strsplit(ans, " +")[[1]];
			wc = length(s[nchar(s)>0])
			if (wc >= score.rubric$min.words){
				return(score.rubric$weight)
			} else {
				return(0)
			}
		} else if (score.type == "url"){
			ans = obj[[1]]
			ans = ans [nchar(ans) > 0]
			ans = tail(ans,1)
			if (length(ans) > 0){
				url = score.rubric$urlStart
				url = paste(url,gsub(" ","+",ans),sep="")
				if (!is.null(score.rubric$urlEnd)) url = paste(url, score.rubric$urlEnd, sep="")
				h = basicTextGatherer()
				curlPerform(url=url, transfertext=TRUE, verbose=FALSE,writefunction = h$update)
				str = h$value()
				str = gsub("\"","",str)
				str = gsub("\\n","",str)
				match = regexpr(score.rubric$scorePattern,str)
				return (as.numeric(substr(str,match[[1]]+nchar(sub("\\\\d\\+","",score.rubric$scorePattern)),match[[1]]+attr(match,"match.length")-1)))	
			} else {
				return (NA)
			}
		} else if (score.type == "skip"){
			### leave scores the way they are
		} else if (score.type == "hand"){
			textplot(strwrap(ans, width=dev.size(units="px")[1]/8), cex=1, fixed.width=FALSE)
			score = readline("Score the Text in the graphics device: \n");
			if (score == "end" || score == "break"){
				return ("break");
			}
			score = suppressWarnings(as.numeric(score))
			return (score)
		} else {
			return (NA);
		}		
	} else {
		if (!is.null(obj$Auto.Score) && sum(!is.na(obj$Auto.Score)) > 0){
			return (max(obj$Auto.Score,na.rm=TRUE));
		} else {
			return (NA);
		}
	} 
}

score.wiseSW.MultipleChoice = function (obj, score.type = NULL, score.rubric, ...){
	if (is.null(obj[[1]]) || length(obj[[1]]) == 0) return (0);

	if (!is.null(score.rubric$pattern)){
		scores = numeric()
		for (p in 1:length(score.rubric$pattern)){
			pattern = score.rubric$pattern[p]
			b = grepl(pattern, obj[[1]])
			if (b){
				if (!is.null(score.rubric$weight[p])){
					scores = c(scores, score.rubric$weight[p])
				} else {
					scores = c(scores, 1)
				}
			} else {
				scores = c(scores, 0)
			} 
		}
		return (max(scores, na.rm=TRUE))
	} else {
		return (NA)
	}
}
#i=11;as.wiseSW(wiseDF.eei[i,]);score(wiseDF.eei[i,], score.rubrics=rubrics.eei, is.data.frame.out=FALSE)

score.wiseSW.Sensor = function (obj, score.type, score.rubric, ...){
	## make sure there are data points to score
	if (is.null(obj$data) || length(obj$data) == 0) return (0);

	if (!missing(score.type)){
		if (score.type == "ruleRubric"){
			return (scoreGraph.ruleRubric(tail(obj$data,1)[[1]], score.rubric))
		} else if (score.type == "rawpoints"){
			return (scoreGraph.rawpoints(tail(obj$data,1)[[1]], score.rubric));
		} else if (score.type == "countpoints"){
			return (nrow(tail(obj$data,1)[[1]]))
		} else {
			return (NA);
		}
	} else {
		return (NA);
	}
}
## if there is no function dedicated to a specific type return NA
score.wiseSW = function (obj, score.type, score.rubric, ...){
	return (NA);
}

################# SPECIFIC FUNCTIONS
#### FUNCTIONS FOR SCORING GRAPH DATA ###########
## For scoring graphs use a rubric in the following form:
# rubric = list()
# rubric$rules = data.frame(x1.min=numeric(),x1.max=numeric(), y1.min=numeric(), y1.max=numeric(), x2.min=numeric(), x2.max=numeric(), y2.min=numeric(),y2.max=numeric(), width.min = numeric(), width.max = numeric(), height.min = numeric(), width.max = numeric(), angle.min=numeric(), angle.max=numeric())
# rubric$scores = data.frame(pattern = character(), score = numeric())
#
# each row of the rules dataframe provides the spatial boundaries for a given point or segment.  Will evaluate with scoreGraph.element to true or false.
# All given values are necessary, so if x (min, max), y(min, max), and slope (min, max) are all given then the points must meet all criterion
# each row of the scores dataframe includes a 'stringed' logical pattern corresponding to rows of rules dataframe, for example
#  R[1] && R[2] || !R[3]  (either row 1 and row 2 of the rules, or not rule 3)
# the other column of the scores dataframe with a numerical value
# A score will be evaluated from top-down, once a score is found the loop breaks
scoreGraph.ruleRubric = function (xy, rubric, debug=FALSE){
	R = logical();
	cp = 1; ### we start looking from the first point in xy, but once that is found we don't search it again, we don't go backwards
	for (r in 1:nrow(rubric$rules)){
		rule = rubric$rules[r,]
		## search each adjacent pair of points for a hit of these rules
		matchFound = FALSE
		## do we have more points to check?

		if (cp <= nrow(xy)){
			#print(paste("Rule", r, "starting with point: ", cp))
			for (p in cp:nrow(xy)){
				x1 = xy$x[p];
				y1 = xy$y[p];
				x2 = NA;
				y2 = NA;
				if (p+1 <= nrow(xy)){
					x2 = xy$x[p+1]
					y2 = xy$y[p+1]
				}
				matchFound = scoreGraph.element(x1, y1, x2, y2, rule, debug)
				if (matchFound){
					### advance if we are not just looking at absolute position of first point
					#print(paste(is.na(rule$x1.min) , is.na(rule$x1.max) , is.na(rule$y1.min) , is.na(rule$y1.max) , !is.na(rule$x2.min) , !is.na(rule$x2.max) , !is.na(rule$y2.min) , !is.na(rule$y2.max) , !is.na(rule$width.min) , !is.na(rule$width.max) , !is.na(rule$height.min) , !is.na(rule$height.max) , !is.na(rule$angle.min) , !is.na(rule$angle.max)))
					if (is.na(rule$x1.min) || is.na(rule$x1.max) || is.na(rule$y1.min) || is.na(rule$y1.max) || !is.na(rule$x2.min) || !is.na(rule$x2.max) || !is.na(rule$y2.min) || !is.na(rule$y2.max) || !is.na(rule$width.min) || !is.na(rule$width.max) || !is.na(rule$height.min) || !is.na(rule$height.max) || !is.na(rule$angle.min) || !is.na(rule$angle.max))
						cp = p + 1;
					break;
				}
			}
		}
		R[[length(R)+1]] = matchFound
	}
	if (debug) print(R)
	for (s in 1:nrow(rubric$scores)){
		if (eval(parse(text=rubric$scores$pattern[s]))) return (rubric$scores$score[s]);
	}
	### No rule found 
	return (1)
}

################## HAND SCORING a#############################
scoreStudentResponse = function (df, outfile){
	for (r in 1:nrow(df)){
		sw = as.wiseSW(df[r,])
		string == studentDataString(sw);
	}
}

subsetForScoring = function (df){
	### make sure there is a Student.Response column, if not add one
	if (is.null(df$Student.Response)){
		df$Student.Response = rep("",nrow(df))
	}
	df = studentResponse(df)
	return (subset(df, TRUE, c(Index, Step.Work.Id, Parent.Project.Id, Project.Id, Run.Id, Student.Response)))
}

readScoredResponses = function(targetDF, sourceFile){
	sourceDF = read.xlsx2(sourceFile, sheetIndex = 1, startRow=1, endRow=1, stringsAsFactors = FALSE);
	colClasses = getColClasses.read(names(sourceDF));
	sourceDF = read.xlsx2(sourceFile, sheetIndex = 1, colClasses=colClasses, stringsAsFactors = FALSE);
	### clean up names a bit (remove .., . at end, "if applicable")
	names(sourceDF) = gsub("..if.applicable.", "", names(sourceDF));
	names(sourceDF) = gsub("\\.\\.", "\\.", names(sourceDF));
	names(sourceDF) = gsub("\\.$", "", names(sourceDF));
	### get final classes and update
	fcolClasses = getColClasses.final(sourceDF);
	for (c in 1:ncol(sourceDF)){
		if (fcolClasses[c] == "numeric"){
			sourceDF[,c] = as.numeric(sourceDF[,c]);
		} else if (fcolClasses[c] == "factor"){
			sourceDF[,c] = as.factor(sourceDF[,c]);
		} else if (fcolClasses[c] == "character"){
			sourceDF[,c] = as.character(sourceDF[,c]);
		}
	}

	index = which(names(sourceDF) == "Student.Response")
	if (index <= length(names(sourceDF))){
		print(sourceDF[1:10,])
		sourceColNames = names(sourceDF)[(index+1):length(names(sourceDF))]
		return(transferColValues(targetDF, sourceDF, sourceColNames, sourceColNames))	
	}
}


### For retreiving student data without any automated scores or feedback
### Allows for human scoring without auto-score interference
studentDataString = function (obj, ...) UseMethod ("studentDataString");
studentDataString.default = function (obj, ...){return("N/A");}
studentDataString.wisedata.frame = function (obj, subset=TRUE, select, drop = FALSE, is.data.frame.out = TRUE, studentDataString.colName = "Student.Work.Part.2", ...){
	index = which("Index" == names(obj));
	if (index < 0){
		obj = cbind(Index = 1:nrow(obj), obj);
	}
	if (!missing(select)){
		if (!missing(subset)){obj.sub = subset(obj, subset=subset, select=select, drop=drop);}
		else {obj.sub = subset(obj, subset=TRUE, select=select, drop=drop);}
	}else{
		if (!missing(subset)){obj.sub = subset(obj, subset=subset, drop=drop);}
		else {obj.sub = subset(obj, subset=TRUE, drop=drop);}
	}
	# iterate threw rows of subset studentDataString each value and put in 
	studentDataStrings = character();
	for (r in 1:nrow(obj.sub)){
		sw = as.wiseSW(obj.sub[r,]);
		s = studentDataString(obj=sw);
		studentDataStrings = c(studentDataStrings, s);
	}
	#print(studentDataStrings)
	if (is.data.frame.out){
		sindex = which(studentDataString.colName == names(obj));
		## update levels
		levels(obj[,sindex]) = c(levels(obj[,sindex]), unique(studentDataStrings))
		obj[obj$Index %in% obj.sub$Index,sindex] = studentDataStrings;
		return (obj);
	} else {
		return (studentDataStrings);
	}	
}

studentDataString.wiseSW.OpenResponse = function (obj){
	if (length(obj) == 0) return ("N/A");
	if (TRUE){
		if (!is.null(obj$data)){
			#if (length(obj$data) > 1) print(obj$data)
			return (tail(obj$data,1));
		} else {
			return ("N/A");
		}
	} 
}

############################################ GRAPHING ################################################################################333
### Given two points and their min and maximum values, as well as a slope (+ min and max), will find if a given pair of points meet criterion
scoreGraph.element = function (x1, y1, x2=NA, y2=NA, rule, debug = FALSE){
	## just look at point 1 first
	if (!is.na(rule$x1.min) && !is.na(rule$x1.max)){
		if (is.na(x1) || x1 < rule$x1.min[1] || x1 > rule$x1.max[1]) return (FALSE);
	}
	if (!is.na(rule$y1.min) && !is.na(rule$y1.max)){
		if (is.na(y1) || y1 < rule$y1.min[1] || y1 > rule$y1.max[1]) return (FALSE);
	}
	
	if (!is.na(rule$x2.min) && !is.na(rule$x2.max)){
		if (is.na(x2) || x2 < rule$x2.min[1] || x2 > rule$x2.max[1]) return (FALSE);
	}
	if (!is.na(rule$y2.min) && !is.na(rule$y2.max)){
		if (is.na(y2) || y2 < rule$y2.min[1] || y2 > rule$y2.max[1]) return (FALSE);
	}
	
	if (!is.na(rule$width.min) && !is.na(rule$width.max)){
		if (is.na(x1) || is.na(x2) || (x2 - x1) < rule$width.min[1] || (x2 - x1) > rule$width.max[1]) return (FALSE);
	}
	if (!is.na(rule$height.min) && !is.na(rule$height.max)){
		if (is.na(y1) || is.na(y2) || (y2 - y1) < rule$height.min[1] || (y2 - y1) > rule$height.max[1]) return (FALSE);
	}

	if (!is.na(rule$angle.min) && !is.na(rule$angle.max)){
		if (!is.na(x1) && !is.na(y1) && !is.na(x2) && !is.na(y2)){
			### if we have the bounds create the angle proportional
			py2 = y2; py1 = y1; px2 = x2; px1 = x1;
			if (!is.na(rule$xbounds.min) && !is.na(rule$xbounds.max) && !is.na(rule$ybounds.min) && !is.na(rule$ybounds.min)){
				width = abs(x2 - x1);
				height = abs(y2 - y1);

				py2 = y2/height; py1 = y1/height; px2 = x2/width; px1 = x1/width;
				if (height != 0){
					angle = atan((py2 - py1) / (px2 - px1)) * 180 / pi		
				} else {
					angle = 0
				}
			} else {
				
				angle = atan((y2 - y1) / (x2 - x1)) * 180 / pi	
			}
			
		} else {
			return (FALSE);
		}
		#print(paste(y2, y1, py2, py1, px2, px1))
		#print(angle)
		#print(rule$angle.min[1])
		#print(rule$angle.max[1])
		if (angle < rule$angle.min[1] || angle > rule$angle.max[1]) return (FALSE);
	}
	
	return (TRUE);
}

### Do the scores in xy correspond to a list of min and max x and y values
scoreGraph.rawpoints = function (xy, score.rubric, debug=FALSE){
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


########################## SCORING NAVIGATION
scoreNavigation.linearity = function (sdf, Step.Num.NoBranch=sort(unique(sdf$Step.Num.NoBranch)), by.Workgroup.Id=TRUE, by.Time=FALSE, ylim=c(as.numeric(as.character(Step.Num.NoBranch[1])), as.numeric(as.character(tail(Step.Num.NoBranch,1))))){
	object = as.list(substitute(list(...)))[-1L]
	### make sure there is a single individual
	Step.Count.NoBranch = 1:length(Step.Num.NoBranch)
	
	if (by.Workgroup.Id){
		wid = sdf$Workgroup.Id
		sdf = subset(sdf, Workgroup.Id == wid)
	} else {
		wid = sdf$Wise.Id.1
		sdf = subset(sdf, Wise.Id.1 == wid)
	}
	if (by.Time){
		sdf$Time.Spent.Seconds[is.na(sdf$Time.Spent.Seconds)] = 0
		ctime = cumsum(sdf$Time.Spent.Seconds)
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
	return(summary(lm(yvals ~ xvals))$r.squared)
}