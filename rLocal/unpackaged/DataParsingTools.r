#################################### PARSING TOOLS #######################################
## These functions will be used to parse student work according to Step Type
##
##

## need this library for some step types that download data in json format.
library(rjson);

### Converts student work into a maneagable format
### the special DF will be a special download of a step type.
### the otherDF will be something else which may be of use to pulling student data
### the contents of which are determined by the step type
as.wiseSW = function(obj, ...) UseMethod("as.wiseSW")
as.wiseSW.default = function (obj, ...){return("Cannot convert input into parsed wise student work")}
as.wiseSW.wisedata.frame = function(obj, ...){
	if (is.null(nrow(obj)) && length(which(names(obj) == "Step.Type")) > 0){
		print("The inputted row must contain column names and a step type.");
		return (NULL);
	} else if (nrow(obj) > 1){
		## data frame has more than one row, return a list of student work
		l = list()
		for (r in 1:nrow(obj)){
			sw = list();
			class(sw) = c(paste("wiseSW", obj$Step.Type[r], sep="."), "wiseSW","list");
			sw = as.wiseSW(sw, obj[r,], ...)
			l[[r]] = sw;	
		}
		return (l)
	} else {
		# single row, just return student work, not in a list
		sw = list();
		class(sw) = c(paste("wiseSW", obj$Step.Type[1], sep="."), "wiseSW","list");
		sw = as.wiseSW(sw, obj[1,], ...)
		return(sw)
	}	
}
as.wiseSW.wiseSW = function (sw, row, ...){
	indices = grep("Student.Work", names(row));
	for (i in 1:length(indices)){
		sw[[i]] = row[1,indices[i]]
	}
	return(sw)
}

as.wiseSW.wiseSW.Questionnaire = function(sw, row){
	### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	# This type of step may have work on all columns of Student.Work
	for (i in indices){
		val = row[1, i];
		if (val == "" || val == "N/A") val = "";
		sw[[length(sw)+1]] = val;
	}
	return(sw)
}
as.wiseSW.wiseSW.OpenResponse = function(sw, row){
	#### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	## if we don't want to work with multiple responses distributed over columns call expandMultipleResponses with expandToColumn = FALSE prior to using this function
	row = expandMultipleResponses(row, TRUE)
	responses = row[1,indices];
	responses.notblankL = responses != "";
	responses.notblankL[1] = TRUE; ### keep the first
	responses = responses[responses.notblankL];
	data = character(); Check.Answer=logical(); Auto.Score.Type = character(); Auto.Score = numeric(); Auto.Feedback = character();
	## with c-rater feedback student responses are stuck in brackets like Student Response: []
	for (r in responses){
		if (regexpr("Student Response: \\[",r)[1] > -1){
			exp = regexpr("Student Response: \\[",r)
			start = exp[1] + attr(exp, "match.length") + 1
			exp = regexpr("Student Response: \\[.*?\\]",r)
			stop = exp[1] + attr(exp, "match.length") - 3
			d = substring(r, start, stop);
			data = c(data, d);
			## autoscoring?
			if (regexpr("Check Answer: \\[",r)[1] > -1){
				exp = regexpr("Check Answer: \\[",r)
				start = exp[1] + attr(exp, "match.length")
				exp = regexpr("Check Answer: \\[.*?\\]",r)
				stop = exp[1] + attr(exp, "match.length") - 2
				d = substring(r, start, stop);
				if (d == "true"){
					Check.Answer = c(Check.Answer, TRUE);
				} else {
					Check.Answer = c(Check.Answer, FALSE);
				}
			}
			## Yes we are checking answer for auto-score
			if (tail(Check.Answer,1)){
				### c-rater?
				if (regexpr("CRater",r)[1] > -1){
					Auto.Score.Type = c(Auto.Score.Type, "CRater");
					if (regexpr("CRater Score: \\[",r)[1] > -1){
						exp = regexpr("CRater Score: \\[",r)
						start = exp[1] + attr(exp, "match.length")
						#exp = regexpr("CRater Score: \\[[^\\[]*?\\]",r)
						exp = regexpr("CRater Score: \\[.*?\\]",r)
						stop = exp[1] + attr(exp, "match.length") - 2
						d = substring(r, start, stop);
						Auto.Score = c(Auto.Score, as.numeric(d))
					} else {
						Auto.Score = c(Auto.Score, NA)
					}
					if (regexpr("CRater Feedback: \\[",r)[1] > -1){
						exp = regexpr("CRater Feedback: \\[",r)
						start = exp[1] + attr(exp, "match.length")
						exp = regexpr("CRater Feedback: \\[.*?\\]",r)
						stop = exp[1] + attr(exp, "match.length") - 2
						d = substring(r, start, stop);
						Auto.Feedback = c(Auto.Feedback, d)
					} else {
						Auto.Feedback = c(Auto.Feedback, "")
					}
				} else {
					Auto.Score.Type = c(Auto.Score.Type, "none")
					Auto.Score = c(Auto.Score, NA)
					Auto.Feedback = c(Auto.Feedback, "")
				}
			} else {
				Auto.Score.Type = c(Auto.Score.Type, "none")
				Auto.Score = c(Auto.Score, NA)
				Auto.Feedback = c(Auto.Feedback, "")
			}
		} else {
			data = c(data, r);
			Check.Answer = c(Check.Answer, FALSE);
			Auto.Score.Type = c(Auto.Score.Type, "none")
			Auto.Score = c(Auto.Score, NA)
			Auto.Feedback = c(Auto.Feedback, "")
		}
	}
	sw[["data"]] = data;
	sw[["Check.Answer"]] = Check.Answer
	sw[["Auto.Score.Type"]] = Auto.Score.Type
	sw[["Auto.Score"]] = Auto.Score
	sw[["Auto.Feedback"]] = Auto.Feedback
	return(sw)
}
as.wiseSW.wiseSW.Mysystem2 = function (sw, row){
	#### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	## if we don't want to work with multiple responses distributed over columns call expandMultipleResponses with expandToColumn = FALSE prior to using this function
	row = expandMultipleResponses(row, TRUE)
	responses = row[1,indices];
	responses.notblankL = responses != "";
	responses.notblankL[1] = TRUE; ### keep the first
	responses = responses[responses.notblankL];
	data = character(); Check.Answer=logical(); Auto.Score.Type = character(); Auto.Score = numeric(); Auto.Feedback = character();
	## with c-rater feedback student responses are stuck in brackets like Student Response: []
	for (r in responses){
		if (TRUE){
			data = c(data, r);
			## autoscoring?
			if (regexpr("Is Submit: ",r)[1] > -1){
				exp = regexpr("Is Submit: ",r)
				start = exp[1] + attr(exp, "match.length")
				exp = regexpr("Is Submit: .*?,",r)
				stop = exp[1] + attr(exp, "match.length") - 2
				d = substring(r, start, stop);
				if (d == "true"){
					Check.Answer = c(Check.Answer, TRUE);
				} else {
					Check.Answer = c(Check.Answer, FALSE);
				}
			}
			## Yes we are checking answer for auto-score
			if (tail(Check.Answer,1)){
				### c-rater?
				if (regexpr("Success: ",r)[1] > -1){
					exp = regexpr("Success: ",r)
						start = exp[1] + attr(exp, "match.length")
						#exp = regexpr("CRater Score: \\[[^\\[]*?\\]",r)
						exp = regexpr("Success: .*?,",r)
						stop = exp[1] + attr(exp, "match.length") - 2
						d = substring(r, start, stop);
						score = 0
						if (d == "true") score = 1
						Auto.Score = c(Auto.Score, score)
				} else {
					Auto.Score = c(Auto.Score, NA)
				}
				if (regexpr("Feedback: ",r)[1] > -1){
					exp = regexpr("Feedback: ",r)
					start = exp[1] + attr(exp, "match.length")
					exp = regexpr("Feedback: .*",r)
					stop = exp[1] + attr(exp, "match.length") - 2
					d = substring(r, start, stop);
					Auto.Feedback = c(Auto.Feedback, d)
				} else {
					Auto.Feedback = c(Auto.Feedback, "")
				}
			} else {
				Auto.Score = c(Auto.Score, NA)
				Auto.Feedback = c(Auto.Feedback, "")
			}
		} else {
			data = c(data, r);
			Check.Answer = c(Check.Answer, FALSE);
			Auto.Score = c(Auto.Score, NA)
			Auto.Feedback = c(Auto.Feedback, "")
		}
	}
	sw[["data"]] = data;
	sw[["Check.Answer"]] = Check.Answer
	sw[["Auto.Score"]] = Auto.Score
	sw[["Auto.Feedback"]] = Auto.Feedback
	return (sw)
}
as.wiseSW.wiseSW.Sensor = function (sw, row){
	#### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	### Assumes that data comes from the spcial parser, there may be multiple student responses, the data will contain
	### a list of responses and prediction values
	val = row[1,indices[1]]; 
	lval = strsplit(val, "Response #[0-9]+: ")[[1]]

	responses = list();
	data = list();
	if (length(lval) > 1){
		lval = lval[2:length(lval)];
		for (i in 1:length(lval)){
			ldata = fromJSON(lval[i]);
			predictions = convertListOfListToListOfVectors(ldata$predictionArray)
			predictionsdf = data.frame(x=predictions$x, y=predictions$y)
			data[[i]] = predictionsdf
			response = ldata$response
			responses[[i]] = response
		}
	}
	sw[['predictions']] = data;
	sw[['response']] = responses;
	return(sw)
}
as.wiseSW.wiseSW.Grapher = function (sw, row, ...){
	#### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	# the car graph step has json data
	val = row[1, indices[1]];
	if (val == "" || val == "N/A") val = NA;
	if (grepl("Response #[0-9]+: ", val)){ 
		datal = fromJSON(sub("Response #[0-9]+: ", "",val))
		### create a data frame that captures:
		# id, x, y;
		sw[['predictions']] = data.frame(id = character(), x = numeric(), y = numeric())
		if (length(datal$predictionArray) > 0){
			for (p in 1:length(datal$predictionArray)){
				if (length(datal$predictionArray[[p]]$predictions) > 0){
					pdf = data.frame(
						id = datal$predictionArray[[p]]$id,
						x = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$x), simplify=TRUE),
						y = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$y), simplify=TRUE)
					)
					sw[['predictions']] = rbind(sw[['predictions']], pdf)	
				}
			}
		}
	} else {
		sw[["data"]] = NA;
	} 				
	return(sw)
}
as.wiseSW.wiseSW.CarGraph = function (sw, row, ...){
	#### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	# the car graph step has json data
	val = row[1, indices[1]];
	if (val == "" || val == "N/A") val = NA;
	if (grepl("Response #[0-9]+: ", val)){ 
		datal = fromJSON(sub("Response #[0-9]+: ", "",val))
		### create a data frame that captures:
		# id, x, y;
		sw[['predictions']] = data.frame(id = character(), x = numeric(), y = numeric())
		if (length(datal$predictionArray) > 0){
			for (p in 1:length(datal$predictionArray)){
				if (length(datal$predictionArray[[p]]$predictions) > 0){
					pdf = data.frame(
						id = datal$predictionArray[[p]]$id,
						x = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$x), simplify=TRUE),
						y = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$y), simplify=TRUE)
					)
					sw[['predictions']] = rbind(sw[['predictions']], pdf)	
				}
			}
		}
		if (length(datal$observationArray) > 0){
			### create a data frame that captures:
			# Action, index in array, t, x
			sw[['observations']] = data.frame()
			for (p in 1:length(datal$observationArray)){
				obs = datal$observationArray[[p]]
				if (length(obs) > 1){
					obs = obs[2:length(obs)]
					Action = rep(head(datal$observationArray[[p]], 1)[[1]],length(obs))
					Index = 1:length(obs)
					t = sapply(obs, function(l)tryCatch({return(l[[1]])},error=function(e){print(datal$observationArray[[p]])}), simplify=TRUE)
					if (is.list(t)) t = rep(NA, length(obs))
					x = sapply(obs, function(l)tryCatch({return(l[[2]])},error=function(e){print(datal$observationArray[[p]])}), simplify=TRUE)
					if (is.list(x)) x = rep(-1, length(obs))
					pdf = data.frame(
						Action = Action,
						Index = Index,
						t = t,
						x = x
					)
					sw[['observations']] = rbind(sw[['observations']], pdf)
				}
			}
		}
	} else {
		sw[["data"]] = NA;
	} 				
	return(sw)
}

as.wiseSW.wiseSW.ExplanationBuilder = function (sw, row, ...){
	args = list(...)
	if (is.null(args$specialDF)){
		specialDF = NULL
	} else {
		specialDF = eval(args$specialDF)
	}
	if (is.null(args$otherDF)){
		otherDF = NULL
	} else {
		otherDF = eval(args$otherDF)
	}
	#### There is no data in the student work, so look in special for some fields
	### special holds the ideas that were sorted and into where
	if (!is.null(specialDF)){
		## find the appropriate row
		#srows= subset(specialDF, as.character(Workgroup.Id)==as.character(row$Workgroup.Id)&as.character(Step.Title)==as.character(row$Step.Title)&as.character(Start.Time)==as.character(row$Start.Time));
		srows= subset(specialDF, as.character(Workgroup.Id)==as.character(row$Workgroup.Id)&as.character(Step.Title)==as.character(row$Step.Title)&as.character(specialDF[,grep("Start.Time",names(specialDF))[1]])==as.character(row[,grep("Start.Time",names(row))[1]]));
		sw[["answer"]] = srows$Answer[1];
		idea.used = character();
		idea.x = numeric();
		idea.y = numeric();
		idea.color = character();
		for (i in 1:nrow(srows)){
			srow = srows[i,];
			idea.used = c(idea.used, as.character(srow$Idea.Text)[1])
			idea.x = c(idea.x, srow$Idea.X.Position[1])
			idea.y = c(idea.y, srow$Idea.Y.Position[1])
			idea.color = c(idea.color, as.character(srow$Idea.Color)[1])
		}
		sw[['idea.used']] = idea.used;
		sw[['idea.x']] = idea.x;
		sw[['idea.y']] = idea.y;
		sw[['idea.color']] = idea.color;
	}
	## the other df is the idea manager export, has the source of all ideas
	if (!is.null(otherDF)){
		idea.source = character();
		fromPublic = vector()
		Idea.Id = vector()
		# get timestamp for )step
		ts.start = timestampAsNumeric(row[,grep("Start.Time",names(row))[1]])
		tindex = grep("time.spent",tolower(names(row)))[1]
		ts.end = ts.start + row[1,tindex]
		## subset from idea manager the relevant rows from the private basket
		orows = subset(otherDF, as.character(Workgroup.Id)==as.character(row$Workgroup.Id))
		if (nrow(orows) > 0){
			### get basket revision number of first idea created after timestamp
			rowsAfterTime = timestampAsNumeric(orows[,grep("Timestamp.Idea.Created",names(orows))[1]])>ts.end
			
			if (sum(rowsAfterTime) > 0){
				xrow = subset(orows, rowsAfterTime)[1,];
				### there is a row in which the time is
				br = xrow$Basket.Revision[1]-1	
			} else {
				### the last revision is valid
				br = tail(orows$Basket.Revision,1)
			}
			## get last basket revision
			orows = subset(orows, Basket.Revision == br);
			if (nrow(orows)>0){
				for (i in 1:nrow(orows)){
					orow = orows[i,]
					if (as.character(orow$Trash) != "1.0"){
						idea.source = c(idea.source, as.character(orow$Idea.Text)[1])
						fromPublic = c(fromPublic, as.numeric(as.character(orow$Was.Copied.From.Public)[1]))
						Idea.Id = c(Idea.Id, as.numeric(as.character(orow$Idea.Id)[1]))
					}	
				}
			} 
		}

		sw[['idea.source']] = idea.source;
		sw[['Was.Copied.From.Public']] = fromPublic;
		sw[['Idea.Id']] = Idea.Id;
	}
	return (sw)
}

feedbackGiven = function (obj, ...) UseMethod ("feedbackGiven");
feedbackGiven.default = function (obj, ...){return("N/A");}
feedbackGiven.wisedata.frame = function (obj, subset=TRUE, select, drop = FALSE, is.data.frame.out = TRUE, feedbackGiven.colName = "Research.FeedbackGiven", ...){
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
	# iterate threw rows of subset feedbackGiven each value and put in 
	feedbackGivens = character();
	for (r in 1:nrow(obj.sub)){
		sw = as.wiseSW(obj.sub[r,]);
		s = feedbackGiven(obj=sw);
		feedbackGivens = c(feedbackGivens, s);
	}
	#print(feedbackGivens)
	if (is.data.frame.out){
		sindex = which(feedbackGiven.colName == names(obj));
		## update levels
		levels(obj[,sindex]) = c(levels(obj[,sindex]), unique(feedbackGivens))
		obj[obj$Index %in% obj.sub$Index,sindex] = feedbackGivens;
		return (obj);
	} else {
		return (feedbackGivens);
	}
	
}
feedbackGiven.wiseSW.OpenResponse = function (obj){
	if (length(obj) == 0) return ("N/A");
	if (TRUE){
		if (!is.null(obj$Auto.Feedback)){
			return (obj$Auto.Feedback);
		} else {
			return ("N/A");
		}
	} 
}

studentResponse = function (obj, ...) UseMethod ("studentResponse");
studentResponse.default = function (obj, ...){return("N/A");}
studentResponse.wisedata.frame = function (obj, subset=TRUE, select, drop = FALSE, is.data.frame.out = TRUE, studentResponse.colName = "Student.Response", ...){
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
	# iterate threw rows of subset studentResponse each value and put in 
	studentResponses = character();
	for (r in 1:nrow(obj.sub)){
		sw = as.wiseSW(obj.sub[r,]);
		s = studentResponse(obj=sw);
		studentResponses = c(studentResponses, s);
	}
	#print(studentResponses)
	if (is.data.frame.out){
		sindex = which(studentResponse.colName == names(obj));
		## update levels
		levels(obj[,sindex]) = c(levels(obj[,sindex]), unique(studentResponses))
		obj[obj$Index %in% obj.sub$Index,sindex] = studentResponses;
		return (obj);
	} else {
		return (studentResponses);
	}
	
}
studentResponse.wiseSW.OpenResponse = function (obj){
	if (length(obj) == 0) return ("N/A");
	if (TRUE){
		if (!is.null(obj$data)){
			#### TODO - MAKE SURE THIS MATCHES THE VERSION OF THE RESPONSE THAT RECEIVES AN AUTO-SCORE (OKAY WHEN EXPANDED INTO MULTIPLE ROWS)
			return (obj$data[1]);
		} else {
			return ("N/A");
		}
	} 
}

##  Applies a Condition to wise data frame based on one of two methods (for now)
##  If method is "Parent.Project.Id" then
###    Parent.Project.Id should include more than id number,  If a Wise.User.Id can
###    be found in any rows of a given Parent.Project.Id, will receive a condition associated
###    with the index of that ID in the Parent.Project.Id vector
## If method is "Workgroup.Id.mod" then
###    Condition is assigned by each Wise.User's mod of his or her Workgroup Id in the Parent Project
### Note if multiple Wise User Ids are in projects that are not the main projects (pointed to be Parent.Project.Id, e.g. pre post test), will look for first only and print an error
applyCondition = function (df, method="Parent.Project.Id", Parent.Project.Ids, Workgroup.Id.mod=1, is.data.frame.out=TRUE, VERBOSE=TRUE){
	if (length(which(class(df)=="wisedata.frame")) == 0){print("You need to use a valid wise data frame."); return (NULL)}
	df.pp = subset(df, Parent.Project.Id %in% Parent.Project.Ids);
	Condition = numeric();
	error1 = "";
	error2 = "";
	if (method=="Parent.Project.Id"){
		for (i in 1:nrow(df)){
			row = df[i,];
			if (length(which(Parent.Project.Ids == row$Parent.Project.Id[1])) > 0){
				### this row is part of one of the target parent projects
				Condition = c(Condition, which(Parent.Project.Ids == row$Parent.Project.Id[1])[1])
			} else{
				### this row is not part of one of the target parent projects
				### find a row in the parent project that has same Wise.User.Id
				id1 = row$Wise.Id.1[1];
				idf = subset(df.pp, Wise.Id.1%in%id1|Wise.Id.2%in%id1|Wise.Id.3%in%id1)
				if (nrow(idf) == 0){
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "did not complete an experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else if (length(unique(idf$Parent.Project.Id)) > 1){
					# bad, id is in more than one exp project
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "is associated with more than one experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else {
					if (!is.nan(row$Wise.Id.2[1]) || !is.nan(row$Wise.Id.3[1])){
						if(VERBOSE) print(paste("Wise users from workgroup",row$Workgroup.Id,"in non-experimental project has more than one member."))
						## okay so the teacher did a non-experimental project in groups (bad), but were they all at least in the same condition?
						id23 = ifelse(!is.nan(row$Wise.Id.2[1]) , row$Wise.Id.2[1] , numeric())
						id23 = ifelse(!is.nan(row$Wise.Id.3[1]) , c(id23, row$Wise.Id.3[1]) , id23)
						id23 = id23[!is.na(id23)]
						idf23 = subset(df.pp, Wise.Id.1%in%id23|Wise.Id.2%in%id23|Wise.Id.3%in%id23)
						if (nrow(idf23) == 0){
							Condition = c(Condition, which(Parent.Project.Ids == idf$Parent.Project.Id[1])[1])
							error = paste("Wise User", id23, "did not complete an experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else if (length(unique(idf23$Parent.Project.Id)) > 1){
							# bad, id is in more than one exp project
							Condition = c(Condition, 0);
							error = paste("Wise Users from Workgroup", row$Workgroup.Id, "are associated with more than one experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else {
							## folks were all in the same condition
							Condition = c(Condition, which(Parent.Project.Ids == idf23$Parent.Project.Id[1])[1])
						}
						
					} else {
						## Ideal, one user in non-experiment, associated with one experimental project
						Condition = c(Condition, which(Parent.Project.Ids == idf$Parent.Project.Id[1])[1])
					}
				}
			}
		}
	} else if (method=="Workgroup.Id.mod"){
		#### Generate conditions based on the mod value of Workgroup.Id within the given parent project
		for (i in 1:nrow(df)){
			row = df[i,];
			if (length(which(Parent.Project.Ids == row$Parent.Project.Id[1])) > 0){
				### this row is part of one of the target parent projects, find the mod of workgroup id
				Condition = c(Condition, as.numeric(as.character(row$Workgroup.Id))%%Workgroup.Id.mod+1);
			} else{
				### this row is not part of one of the target parent projects
				### find a row in the parent project that has same Wise.User.Id
				id1 = row$Wise.Id.1[1];
				idf = subset(df.pp, Wise.Id.1%in%id1|Wise.Id.2%in%id1|Wise.Id.3%in%id1)
				if (nrow(idf) == 0){
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "did not complete an experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else if (length(unique(idf$Parent.Project.Id)) > 1){
					# bad, id is in more than one exp project
					Condition = c(Condition, 0);
					error = paste("Wise User", id1, "is associated with more than one experimental project.")
					if(VERBOSE && error!=error1 && error!=error2) print(error)
					error2 = error1
					error1 = error;
				} else {
					if (!is.nan(row$Wise.Id.2[1]) || !is.nan(row$Wise.Id.3[1])){
						#if(VERBOSE) print(paste("Wise users from workgroup",row$Workgroup.Id,"in non-experimental project has more than one member."))
						## okay so the teacher did a non-experimental project in groups (bad), but were they all at least in the same condition?
						id23 = ifelse(!is.nan(row$Wise.Id.2[1]) , row$Wise.Id.2[1] , numeric())
						id23 = ifelse(!is.nan(row$Wise.Id.3[1]) , c(id23, row$Wise.Id.3[1]) , id23)
						id23 = id23[!is.na(id23)]
						idf23 = subset(df.pp, Wise.Id.1%in%id23|Wise.Id.2%in%id23|Wise.Id.3%in%id23)
						if (nrow(idf23) == 0){
							Condition = c(Condition, as.numeric(as.character(idf$Workgroup.Id))[1]%%Workgroup.Id.mod+1)
							error = paste("Wise User", id23, "did not complete an experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else if (length(unique(idf23$Parent.Project.Id)) > 1){
							# bad, id is in more than one exp project
							Condition = c(Condition, 0);
							error = paste("Wise Users from Workgroup", row$Workgroup.Id, "are associated with more than one experimental project.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						} else {
							## folks were all in the same condition
							Condition = c(Condition, as.numeric(as.character(idf23$Workgroup.Id))[1]%%Workgroup.Id.mod+1)
							error = paste("Wise users from workgroup",row$Workgroup.Id,"in run",row$Run.Id, "has more than one member, but they all were same condition.")
							if(VERBOSE && error!=error1 && error!=error2) print(error)
							error2 = error1
							error1 = error;
						}
						
					} else {
						## Ideal, one user in non-experiment, associated with one experimental project
						Condition = c(Condition, as.numeric(as.character(idf$Workgroup.Id))[1]%%Workgroup.Id.mod+1)
					}
				}
			}
		}
	} else {
		print(paste("The method"), method, "does not exist.")
	}
	Condition = as.factor(Condition);
	if (is.data.frame.out){
		oclass = class(df)
		df = cbind(df, Condition=Condition)
		class(df) = oclass
		return (df)
	} else {
		return (Condition);
	}
}

### for each Wise Id in the target look for condition in source. 
transferCondition = function (sourceDF, targetDF){
	eeids = unique(targetDF$Wise.Id.1)
	for (eeid in eeids){
		eeid = as.character(eeid)
		sdf = subset(sourceDF, Wise.Id.1==eeid|Wise.Id.2==eeid|Wise.Id.3==eeid)
		if (nrow(sdf) > 0){
			c = sdf$Condition[1]
			tdf = subset(targetDF, Wise.Id.1==eeid)
			if (nrow(tdf) > 0){
				cs = rep(c, nrow(tdf))
				targetDF[targetDF$Wise.Id.1 %in% eeid,]$Condition = cs
			}
		}
	}
	return (targetDF)
}

parseTimestamp = function (ts){
	out.list = list()
	r = regexpr("[A-Za-z]+ ", ts)
	out.list$Month_Name = substring(ts, r[1], r[1] + attr(r, "match.length")-2)
	out.list$Month = as.vector(sapply(out.list$Month_Name,function(x)grep(x,month.abb)))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+, ", ts)
	out.list$Day = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-3))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+ ", ts)
	out.list$Year = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+:", ts)
	Hour = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+:", ts)
	out.list$Minute = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+ ", ts)
	out.list$Second = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("AM|PM", ts)
	AM_PM = substring(ts, r[1], r[1] + attr(r, "match.length")-1)
	out.list$Hour = ifelse(AM_PM=="PM",Hour%%12+12, Hour%%12)
	class(out.list) = "timestamp";
	return (out.list);
}
## In seconds since midnight Jan 1st 2000
timestampAsNumeric = function (ts){
	if (class(ts)=="timestamp"){
		out.list = ts
	} else { 
		out.list = parseTimestamp(ts)
	}
	year = out.list$Year;
	if (year%%4==0 && (year%%100!=0 || year%%400==0)){
		daysInMonthsSoFar = c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
	} else {
		daysInMonthsSoFar = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
	}	
	daysInYearSoFar = daysInMonthsSoFar[out.list$Month]
	
	s = (out.list$Year - 2000)*365.25*24*60*60 + daysInYearSoFar*24*60*60 + out.list$Day*24*60*60 + out.list$Hour*60*60 + out.list$Minute*60 +out.list$Second;
	return (s);
}


summary.wiseSW.Note = function (obj){print("Note");return(summary.wiseSW(obj))}
summary.wiseSW.AssessmentList = function (obj){return(summary.wiseSW(obj))}
summary.wiseSW.Challenge = function (obj){return(summary.wiseSW(obj))}
summary.wiseSW.Html = function (obj){print("Html");return(summary.wiseSW(obj))}
summary.wiseSW = function (obj){
	for (i in 1:length(obj)){
		print (paste("Student.Work.Part.",i,sep=""))
		print(obj[i])
	}
}

#  Often JSON data will be list of lists like {{x=1, y=2},{x=2, y=3}}
#   but we would rather have a list of vectors like {x=[1,2],y=[2,3]}
convertListOfListToListOfVectors = function (listlist){
	# get key names in second level of lists
	olist = list();
	if (length(listlist) == 0) return (olist);
	keys = names(listlist[[1]]);
	df = data.frame(); ## for temporarily storing values
	for (l in listlist){
		v = c();
		for (k in keys){
			v = c(v, l[[k]]);
		}
		df = rbind(df, v);
	}
	names(df) = keys;
	# convert data frame to a list of vectors;
	for (k in keys){
		olist[[k]] = df[k];
	}
	return (olist);
}