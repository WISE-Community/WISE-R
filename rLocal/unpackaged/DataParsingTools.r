#################################### PARSING TOOLS #######################################
## These functions will be used to parse student work according to Step Type
##
##

## need this library for some step types that download data in json format.
library(rjson);
library(plyr)

### Converts student work into a maneagable format
### the special DF will be a special download of a step type.
### the otherDF will be something else which may be of use to pulling student data
### the contents of which are determined by the step type
as.wiseSW <- function(obj, ...) UseMethod("as.wiseSW")
as.wiseSW.default <- function (obj, ...){return("Cannot convert input into parsed wise student work")}
as.wiseSW.wisedata.frame <- function(obj, ...){
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
as.wiseSW.wiseSW <- function (sw, row, ...){
	indices = grep("Student\\.Work", names(row))
	row <- expandMultipleResponses(row)
	if (nrow(row) == 0) return (NULL)
	for (ri in 1:nrow(row)){
		vals <- row[ri,indices]
		if (nchar(vals[1])==0) vals[1] <- " "
		vals <- vals[nchar(vals) > 0]
		print(vals[1])
		if (!is.na(vals[1] && vals[1]==" ") vals[1] <- ""
		sw[[length(sw)+1]] <- vals
	}
	return(sw)
}
gcc <- getRevisionNumberInStep(gcc, as.data.frame.out=TRUE)

as.wiseSW.wiseSW.AssessmentList <- function(sw, row, colNames = "Student\\.Work"){
	### which indices contain Student Work?
	indices <- numeric()
	for (cn in colNames){
		indices <- union(indices, grep(cn, names(row)))
	}

	row <- expandMultipleResponses(row)
	responses <- row[,indices]
	
	for (ri in 1:nrow(responses)){
		lindex <- length(sw) + 1
		vals <- character()
		for (ci in 1:ncol(responses)){
			val <- responses[ri, ci]
			if (is.na(val) || val == " " || val == "N/A") val = ""
			vals <- c(vals, val)
		}
		val.chars <- nchar(vals)
		# remove trailing empties
		vals <- vals[rev(length(vals) - which(cumsum(rev(nchar(vals)))>0) + 1)]
		#vals <- vals[nchar(vals)>0]
		sw[[lindex]] <- vals
	}
	return(sw)
}
#as.wiseSW(obj.sub[r,]);

as.wiseSW.wiseSW.MatchSequence <- function (sw, row){
	library(rjson)
	
	#### which indices contain Student Work?
	index = grep("Student\\.Work\\.Part\\.1$", names(row));
	## if we don't want to work with multiple responses distributed over columns call expandMultipleResponses with expandToColumn = FALSE prior to using this function
	rows <- expandMultipleResponses(row)
	responses <- rows[,index];
	#responses <- responses[nchar(responses)>0]
	
	for (r in responses){
		lindex <- length(sw)+1
		sw[[lindex]] <- list()
		tableData <- data.frame()
		ldata <- tryCatch(fromJSON(as.character(r)), error = function(e) return (NA)) 
		if (!is.na(ldata) && length(ldata) > 0){
			for (li in 1:length(ldata)){
				fieldText <- ifelse(!is.null(ldata[[li]]$text), ldata[[li]]$text, NA)
				isTargetBucket <- ifelse(!is.null(ldata[[li]]$isTargetBucket), ldata[[li]]$isTargetBucket, NA)
				fieldIdentifier <- ifelse(!is.null(ldata[[li]]$identifier), ldata[[li]]$identifier, NA)
				if (length(ldata[[li]]$choices) > 0){
					for (ci in 1:length(ldata[[li]]$choices)){
						choiceText <- ifelse(!is.null(ldata[[li]]$choices[[ci]]$text), ldata[[li]]$choices[[ci]]$text, NA)
						choiceBucket <- ifelse(!is.null(ldata[[li]]$choices[[ci]]$bucket), ldata[[li]]$choices[[ci]]$bucket, NA)
						choiceIdentifier <- ifelse(!is.null(ldata[[li]]$choices[[ci]]$identifier), ldata[[li]]$choices[[ci]]$identifier, NA)
						tableData <- rbind(tableData, data.frame(fieldText = fieldText, isTargetBucket = isTargetBucket, fieldIdentifier = fieldIdentifier, choiceText = choiceText, choiceBucket = choiceBucket, choiceIdentifier = choiceIdentifier))
					}
				}
			}
			
			sw[[lindex]]$table <- tableData
		} else {
			sw[[lindex]]$table <- NA
		}
	}
	return (sw)
}


## Will capture all of the columns from colNames and replace the names with replNames (should be matching length of each set)
as.wiseSW.wiseSW.OpenResponse <- function(sw, row ){
	#, colNames = c("Student\\.Work","^Auto\\.Score"), replNames = c("data", "Auto.Score"), replClass = c("character", "numeric")
	#### which indices contain Student Work?
	indices <- numeric()
	#replNames.temp <- character();	replClass.temp <- character(); 	for (cni in 1:length(colNames)){cn <- colNames[cni];index.temp <- grep(cn, names(row));	replNames.temp <- c(replNames.temp, rep(replNames[cni],length(index.temp)));replClass.temp <- c(replClass.temp, rep(replClass[cni],length(index.temp)));	indices <- c(indices, index.temp);	};	replNames <- replNames.temp;	replClass <- replClass.temp;	index <- grep("Student\\.Work\\.Part\\.1", names(row))
	## if we don't want to work with multiple responses distributed over columns call expandMultipleResponses with expandToColumn = FALSE prior to using this function
	rows <- expandMultipleResponses(row)
	#responses <- row[,indices]
	
	# only use those rows where there is some work in the first student work column
	#responses <- responses[nchar(row[,index]) > 0,]
	## Look in multiple rows for data
	for (ri in 1:nrow(rows)){
		row <- rows[ri,]
		lindex <- length(sw) + 1
		sw[[lindex]] <- list()
		# In some strange cases where there is data in Student.Work.Part.4 it is the data
		if (length(which(names(row)=="Student.Work.Part.4"))>0 && nchar(row$Student.Work.Part.4)>0){
			sw[[lindex]][["data"]] <- row$Student.Work.Part.4
			sw[[lindex]][["Auto.Score"]] <- as.numeric(row$Auto.Score)
			sw[[lindex]][["Auto.Feedback"]] <- row$Auto.Feedback
			sw[[lindex]][["Max.Score"]] <- as.numeric(row$Student.Work.Part.5)
		} else {
			sw[[lindex]][["data"]] <- row$Student.Work.Part.1
		}

		#for (ci in 1:ncol(responses)){val <- responses[ri, ci];	val <- suppressWarnings(do.call(paste("as",replClass[ci],sep="."),list(val)));sw[[lindex]][[replNames[ci]]] <- val}
	}
	return(sw)
}
#as.wiseSW(row)
#score(wise[r,], score.rubric = rubrics.nwords, out.colName = "NWords", as.data.frame.out=TRUE,DEBUG=TRUE)
as.wiseSW.wiseSW.Mysystem2 <- function (sw, row, colNames = "Student\\.Work\\.Part\\.1"){
	library(rjson)
	#### which indices contain Student Work?
	index = grep(colNames, names(row));
	## if we don't want to work with multiple responses distributed over columns call expandMultipleResponses with expandToColumn = FALSE prior to using this function
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	#data = character(); Is.Submit = logical(); Auto.Score = numeric(); Success = logical(); Auto.Feedback = character();tables = list();Svg = character()
	for (r in responses){
		lindex <- length(sw) + 1
		sw[[lindex]] <- list()
		# remove (escaped) newlines
		r <- gsub("\\\\\\\\n","",r) # this is weird, in one example I found \\\\n instead of //n
		r <- gsub("\\\\n","",r)
		r <- gsub("\\\\c","",r)
		### Get table of Nodes and Link
		### Setup startNode endNode link
		### Start with valid links, and then end with unlinked nodes
		table <- data.frame(StartNode=character(), EndNode=character(), Link=character(), stringsAsFactors=FALSE)
		info <- tryCatch(fromJSON(r)$response, error = function(e) return (NULL)) 
		if (!is.null(info)){
			is.submit <- tryCatch(as.logical(fromJSON(r)$isSubmit), error = function(e) return (FALSE)) 
			links <- tryCatch(fromJSON(info)$MySystem.Link, error = function(e) return (list()))
			nodes <- tryCatch(fromJSON(info)$MySystem.Node, error = function(e) return (list()))
			if (length(links) > 0){
				for (i in 1:length(links)){
					link <- links[[i]]
					if (!is.null(link$startNode) && !is.null(link$endNode) && !is.null(link$text)){
						table <- rbind(table, data.frame(StartNode=link$startNode, EndNode=link$endNode, Link=link$text, stringsAsFactors=FALSE))
					}
				}
			}
			if (length(nodes) > 0){
				for (i in 1:length(nodes)){
					node = nodes[[i]]
					node.id = node$guid
					node.text = node$title
					found = FALSE
					### has this id been used in table, if not add it (as text)
					if (nrow(table) > 0){
						for (j in 1:nrow(table)){
							if (table$StartNode[j] == node.id){
								table$StartNode[j] <- node.text
								found <- TRUE
							}
							if (table$EndNode[j] == node.id){
								table$EndNode[j] <- node.text
								found <- TRUE
							}
						}
					}
					if (!found){
						table <- rbind(table, data.frame(StartNode=node.text, EndNode="", Link="", stringsAsFactors=FALSE))
					}
				}
				### refactor
				#table$Link = as.factor(table$Link)
				#table$StartNode = as.factor(table$StartNode)
				#table$EndNode = as.factor(table$EndNode)
				#levels(table$StartNode) = union(levels(table$StartNode), levels(table$EndNode))
				#levels(table$EndNode) = levels(table$StartNode)
			}
			lastscore = tryCatch(as.numeric(fromJSON(info)$MySystem.RubricScore$LAST_SCORE_ID$score), error = function(e) return (NA)) 
			if (length(lastscore) == 0) lastscore <- NA
			success = tryCatch(as.logical(fromJSON(info)$MySystem.RuleFeedback$LAST_FEEDBACK$success), error = function(e) return (FALSE)) 
			if (length(success) == 0) success <- FALSE
			feedback = tryCatch(fromJSON(info)$MySystem.RuleFeedback$LAST_FEEDBACK$feedback, error = function(e) return ("")) 
			if (length(feedback) == 0) feedback <- ""
			lastsvg = tryCatch(fromJSON(info)$MySystem.GraphicPreview$LAST_GRAPHIC_PREVIEW$svg, error = function(e) return ("")) 
			if (length(lastsvg) == 0) lastsvg <- ""
		} else {
			is.submit <- NA
			lastscore <- NA
			success <- FALSE
			feedback <- ""
			lastsvg <- ""
		}		
		sw[[lindex]][["data"]] <- r
		sw[[lindex]][["table"]] <- table
		sw[[lindex]][["Is.Submit"]] <- is.submit
		sw[[lindex]][["Success"]] <- success
		sw[[lindex]][["Auto.Score"]] <- lastscore
		sw[[lindex]][["Auto.Feedback"]] <- feedback
		sw[[lindex]][["Svg"]] <- lastsvg
	}
	return (sw)
}

as.wiseSW.wiseSW.Note <- function (sw, row){
	return (as.wiseSW.wiseSW.OpenResponse(sw,row))
}
as.wiseSW.wiseSW.Box2dModel <- function(sw, row, colNames = "Student\\.Work\\.Part\\.1"){
	#### which indices contain Student Work?
	index = grep(colNames, names(row));
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	for (r in responses){
		r = gsub("\\\\n","",r)
		lindex <- length(sw) + 1
		sw[[lindex]] <- list()
		data = character(); 
		history = data.frame()
		models = data.frame()
		
		info = tryCatch(fromJSON(r)$response, error = function(e) return (NULL)) 
		if (!is.null(info) && is.list(info)){
			if (!is.null(info$history) && is.list(info$history) && length(info$history) > 0){
				historyData = info$history
				for (l in 1:length(historyData)){
					hrow = data.frame(historyData[[l]][sapply(historyData[[l]],length)>0])
					history = rbind.fill(history, hrow)
				}
			}
			if (!is.null(info$tableData) && is.list(info$tableData) && length(info$tableData) > 0 && length(info$tableData[[1]]) > 1){
				tableData = info$tableData
				### put two dummy columns to make this a data frame
				models = data.frame(D1 = rep(NA,length(tableData[[1]])-1), D2 = rep(NA,length(tableData[[1]])-1))
				for (col in 1:length(tableData)){
					# first column will be column name, subsequent will be values
					column = vector()
					for (row in 2:length(tableData[[1]])){
						text = tableData[[col]][[row]]$text
						if (is.null(text)) text = ""
						column = c(column, text)
					} 

					models$TEMP = column
					names(models)[names(models)=="TEMP"] = tableData[[col]][[1]]$text
				}
				# rm dummies
				models = models[,c(3:ncol(models))]
				sw[[lindex]][["history"]] = history
				sw[[lindex]][["models"]] = models	
			} else {
				sw[[lindex]] <- NA
			}
		} else {
			sw[[lindex]] <- NA
		}
	}
	return(sw)
}
#sw = as.wiseSW(row)

as.wiseSW.wiseSW.Sensor <- function (sw, row, colNames = "Student\\.Work\\.Part\\.1$"){
	#### which indices contain Student Work?
	index = grep(colNames, names(row));
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	### Assumes that data comes from the spcial parser, there may be multiple student responses, the data will contain
	### a list of responses and prediction values
	#val = row[1,index[1]]; 
	#lval = strsplit(val, "Response #[0-9]+: ")[[1]]

	xMin = numeric()
	xMax = numeric()
	yMin = numeric()
	yMax = numeric()
	data = list()
	for (r in responses){
		r <- gsub("\\\\n","",r)
		lindex <- length(sw) + 1
		ldata <- fromJSON(r);
		predictions = convertListOfListToListOfVectors(ldata$predictionArray)
		if (length(predictions) > 0){
			predictionsdf = data.frame(id=rep("prediction",length(predictions$x)),x=predictions$x, y=predictions$y)
			sw[[lindex]] <- list()
			sw[[lindex]][['predictions']] <- predictionsdf
			sw[[lindex]][['response']] <- ldata$response
			sw[[lindex]][['xMin']] <- as.numeric(ldata$xMin)
			sw[[lindex]][['xMax']] <- as.numeric(ldata$xMax)
			sw[[lindex]][['yMin']] <- as.numeric(ldata$yMin)
			sw[[lindex]][['yMax']] <- as.numeric(ldata$yMax)
		} else {
			sw[[lindex]] <- NA
		}
	}
	return(sw)
}

### update xMin and all that
as.wiseSW.wiseSW.Grapher <- function (sw, row, ...){
	#### which indices contain Student Work?
	index <- grep("Student\\.Work\\.Part\\.1$", names(row));
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	for (r in responses){
		r <- gsub("\\\\n","",r)
		lindex <- length(sw) + 1
		datal = fromJSON(paste("{", r, "}"))
		### create a data frame that captures:
		# id, x, y;
		predictions <- data.frame(id = character(), x = numeric(), y = numeric())
		if (length(datal$predictionArray) > 0){
			for (p in 1:length(datal$predictionArray)){
				if (length(datal$predictionArray[[p]]$predictions) > 0){
					id = datal$predictionArray[[p]]$id
					x = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$x), simplify=TRUE)
					if (is.list(x)) x = sapply(x,function(t){if(is.null(t)){return(NA)}else{return(t)}})
					y = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$y), simplify=TRUE)
					if (is.list(y)) y = sapply(y,function(t){if(is.null(t)){return(NA)}else{return(t)}})
					pdf = data.frame(
						id = id,
						x = x,
						y = y
					)
					predictions <- rbind(predictions, pdf)	
				}
			}
			sw[[lindex]] <- list()
			sw[[lindex]][['predictions']] <- predictions
			if (length(datal$feedbackMessage) > 0) sw[[lindex]][['feedbackMessage']] <- datal$feedbackMessage
			if (length(datal$score) > 0) sw[[lindex]][['score']] <- datal$score
		} else {
			sw[[lindex]] <- NA
		}

	} 			
	return(sw)
}

as.wiseSW.wiseSW.CarGraph <- function (sw, row, ...){
	#### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	for (r in responses){
		r <- gsub("\\\\n","",r)
		lindex <- length(sw) + 1
		sw[[lindex]] <- list()
		datal = fromJSON(sub("Response #[0-9]+: ", "",r))
		### create a data frame that captures:
		# id, x, y;
		predictions <- data.frame(id = character(), x = numeric(), y = numeric())
		if (length(datal$predictionArray) > 0){
			for (p in 1:length(datal$predictionArray)){
				if (length(datal$predictionArray[[p]]$predictions) > 0){
					pdf = data.frame(
						id = datal$predictionArray[[p]]$id,
						x = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$x), simplify=TRUE),
						y = sapply(datal$predictionArray[[p]]$predictions, function(l)return(l$y), simplify=TRUE)
					)
					predictions <- rbind(predictions, pdf)	
				}
			}
			sw[[lindex]][['predictions']] <- predictions
		} else {
			sw[[lindex]][['predictions']] <- NA
		}
		if (length(datal$observationArray) > 0){
			### create a data frame that captures:
			# Action, index in array, t, x
			observations <- data.frame()
			for (p in 1:length(datal$observationArray)){
				obs = datal$observationArray[[p]]
				if (length(obs) > 0){
					# check if head is label
					if (length(head(obs,1)[[1]]) == 1 && is.character(head(obs,1)[[1]])){
						h = head(obs,1)[[1]]
						if (length(obs) > 1){
							obs = obs[2:length(obs)]
						} else {
							obs = c()
						}
					} else {
						h = ""
					}
					if (length(obs) > 0){		
						Action = rep(h,length(obs))
						Index = 1:length(obs)
						t = sapply(obs, function(l)tryCatch({return(l[[1]])},error=function(e){print(datal$observationArray[[p]])}), simplify=TRUE)
						if (is.list(t)) t = rep(NA, length(obs))
						x = sapply(obs, function(l)tryCatch({return(l[[2]])},error=function(e){print(datal$observationArray[[p]])}), simplify=TRUE)
						if (is.list(x)) x = rep(NA, length(obs))
						pdf = data.frame(
							Action = Action,
							Index = Index,
							t = t,
							x = x
						)
					} else {
						pdf = data.frame(Action=h,Index=1,t=NA, x=NA)
					}
					observations = rbind(observations, pdf)
				}
			} 
			sw[[lindex]][['observations']] <- observations
		} else {
			sw[[lindex]][['observations']] <- NA
		}
	} 				
	return(sw)
}
as.wiseSW.wiseSW.Table <- function (sw, row, ...){
	args = list(...)
	colNames = "Student\\.Work\\.Part\\.1$"
	if (is.null(args$nrow.titles)){
		nrow.titles = 0
	} else {
		nrow.titles = eval(args$nrow.titles)
	}
	if (is.null(args$ncol.titles)){
		ncol.titles = 1
	} else {
		ncol.titles = eval(args$ncol.titles)
	}
	index = grep(colNames, names(row));
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	
	for (r in responses){
		r <- gsub("\\\\n","",r)
		r <- paste("{", r, "}",sep="")
		#r <- gsub('\\\\"','',r)
		tableData <- data.frame()
		tableDimData <- data.frame()
		lindex <- length(sw)+1
		sw[[lindex]] <- list()
		datal = fromJSON(r)
		if (!is.null(datal$tableData) && length(datal$tableData) > nrow.titles && length(datal$tableData[[1]]) > ncol.titles){
			ncols = length(datal$tableData)
			nrows = length(datal$tableData[[1]])
			
			rtitles = character()
			if (nrow.titles > 0){
				for (r in (ncol.titles+1):nrows){
					rtitle = "";
					for (c in 1:nrow.titles){
						rtitle = paste(rtitle, as.character(datal$tableData[[c]][[r]]$text), collapse=".");
					}
					rtitles = c(rtitles, gsub("^\\s+|\\s+$", "", rtitle));
				}
			}

			#iterate through each column
			for (ci in (nrow.titles+1):ncols){
				ctitle = ""
				if (ncol.titles > 0){
					for (ri in 1:ncol.titles){
						ctitle = paste(ctitle, as.character(datal$tableData[[ci]][[ri]]$text), collapse=".");
					}
				} else {
					ctitle = paste("Col",c,sep=".")
				}
				column = character()
				dimcolumn = character()
				for (ri in (ncol.titles+1):nrows){
					column = c(column, as.character(datal$tableData[[ci]][[ri]]$text));
					dimcolumn = c(dimcolumn, ifelse(!is.null(datal$tableData[[ci]][[ri]]$cellSize),datal$tableData[[ci]][[ri]]$cellSize, 15))
				}
				if (length(tableData) == 0){
					tableData = data.frame(TEMP = column)
					tableDimData = data.frame(TEMP = dimcolumn)
				} else {
					tableData$TEMP = column;
					tableDimData$TEMP = dimcolumn;					
				}
				names(tableData)[which(names(tableData) == "TEMP")] = gsub("^\\s+|\\s+$", "", ctitle);
				names(tableDimData)[which(names(tableDimData) == "TEMP")] = gsub("^\\s+|\\s+$", "", ctitle);
			}
			if (nrow.titles > 0){
				row.names(tableData) = rtitles 
				row.names(tableDimData) = rtitles
			} 
			sw[[lindex]][['table']] <- tableData
			sw[[lindex]][['tableDims']] <- tableDimData
		} else {
			sw[[lindex]] <- NA
		}
		if (length(datal$feedbackMessage) > 0) sw[[lindex]][['feedbackMessage']] <- datal$feedbackMessage
		if (length(datal$score) > 0) sw[[lindex]][['score']] <- datal$score
		if (length(datal$response) > 0) sw[[lindex]][['response']] <- datal$response
		if (length(datal$timestamp) > 0) sw[[lindex]][['timestamp']] <- datal$timestamp
	}
	return(sw)
}
#as.wiseSW(row)


#TODO
as.wiseSW.wiseSW.ExplanationBuilder <- function (sw, row,  colNames = "Student\\.Work\\.Part\\.1"){
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
	index = grep(colNames, names(row));
	row <- expandMultipleResponses(row)
	responses <- row[,index];
	responses <- responses[nchar(responses)>0]
	
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

feedbackGiven <- function (obj, ...) UseMethod ("feedbackGiven");
feedbackGiven.default <- function (obj, ...){return("N/A");}
feedbackGiven.wisedata.frame <- function (obj,as.data.frame.out = TRUE, feedbackGiven.colName = "Research.FeedbackGiven", ...){
	index = which("Index" == names(obj));
	if (index < 0){
		obj = cbind(Index = 1:nrow(obj), obj);
	}
	obj.sub = obj
	
	# iterate threw rows of subset feedbackGiven each value and put in 
	feedbackGivens = character();
	for (r in 1:nrow(obj.sub)){
		sw = as.wiseSW(obj.sub[r,]);
		s = feedbackGiven(obj=sw);
		feedbackGivens = c(feedbackGivens, s);
	}
	#print(feedbackGivens)
	if (as.data.frame.out){
		sindex = which(feedbackGiven.colName == names(obj));
		## update levels
		levels(obj[,sindex]) = c(levels(obj[,sindex]), unique(feedbackGivens))
		obj[obj$Index %in% obj.sub$Index,sindex] = feedbackGivens;
		return (obj);
	} else {
		return (feedbackGivens);
	}
	
}
feedbackGiven.wiseSW.OpenResponse <- function (obj){
	if (length(obj) == 0) return ("N/A");
	if (TRUE){
		if (!is.null(obj$Auto.Feedback)){
			return (tail(obj$Auto.Feedback,1))
		} else {
			return ("N/A");
		}
	} 
}
feedbackGiven.wiseSW.Note <- function (obj){
	return(feedbackGiven.wiseSW.OpenResponse(obj))
}

studentResponse <- function (obj, ...) UseMethod ("studentResponse");
studentResponse.default <- function (obj, ...){return("N/A");}
studentResponse.wisedata.frame <- function (obj, as.data.frame.out = FALSE, studentResponse.colName = "Student.Response", ...){
	if (length(which("Index" == names(obj))) == 0) obj$Index <- 1:nrow(obj)
	obj.sub = obj
	
	# iterate through rows of subset studentResponse each value and put in 
	studentResponses = character();
	for (r in 1:nrow(obj.sub)){
		sw = as.wiseSW(obj.sub[r,]);
		s = studentResponse(obj=sw, ...);
		studentResponses = c(studentResponses, s);
	}
	#print(studentResponses)
	if (as.data.frame.out){
		sindex = which(studentResponse.colName == names(obj));
		## update levels
		levels(obj[,sindex]) = c(levels(obj[,sindex]), unique(studentResponses))
		obj[obj$Index %in% obj.sub$Index,sindex] = studentResponses;
		return (obj);
	} else {
		return (studentResponses);
	}	
}
#studentResponse(obj.final,as.data.frame.out=FALSE)

studentResponse.wiseSW.OpenResponse <- function (obj, revision.number=999, ...){
	if (length(obj) == 0) return ("");
	if (revision.number > length(obj)) revision.number <- length(obj)
	obj <- obj[[revision.number]]
	if (!is.null(obj$data)){
		#### TODO - MAKE SURE THIS MATCHES THE VERSION OF THE RESPONSE THAT RECEIVES AN AUTO-SCORE (OKAY WHEN EXPANDED INTO MULTIPLE ROWS)
		return (obj$data);
	} else {
		return ("N/A");
	} 
}

studentResponse.wiseSW.Note <- function (obj, ...){
	return (studentResponse.wiseSW.OpenResponse(obj, ...))
}
studentResponse.wiseSW.MultipleChoice <- function (obj, revision.number=999, Student.Work.Part = 1, ...){
	if (Student.Work.Part > 1 || length(obj) == 0) return ("");
	if (revision.number > length(obj)) revision.number <- length(obj)
	obj <- obj[[revision.number]]
	if (!is.null(obj[[1]])){
		#### TODO - MAKE SURE THIS MATCHES THE VERSION OF THE RESPONSE THAT RECEIVES AN AUTO-SCORE (OKAY WHEN EXPANDED INTO MULTIPLE ROWS)
		return (obj[[1]]);
	} else {
		return ("N/A");
	} 
}

studentResponse.wiseSW.AssessmentList <- function (obj, revision.number=999, Student.Work.Part = 1, ...){
	if (length(obj) == 0) return ("N/A");
	if (revision.number > length(obj)) revision.number <- length(obj)
	obj <- obj[[revision.number]]
	
	if (length(obj) < Student.Work.Part) return ("")
	else return (obj[[Student.Work.Part]])
}
#studentResponse(row, as.data.frame.out=FALSE, revision.number=1,Student.Work.Part =1)


### for each Wise Id in the target look for condition in source. 
transferCondition <- function (sourceDF, targetDF){
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

parseTimestamp <- function (ts){
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
timestampAsNumeric <- function (ts){
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


summary.wiseSW.Note <- function (obj){print("Note");return(summary.wiseSW(obj))}
summary.wiseSW.AssessmentList <- function (obj){return(summary.wiseSW(obj))}
summary.wiseSW.Challenge <- function (obj){return(summary.wiseSW(obj))}
summary.wiseSW.Html <- function (obj){print("Html");return(summary.wiseSW(obj))}
summary.wiseSW <- function (obj){
	for (i in 1:length(obj)){
		print (paste("Student.Work.Part.",i,sep=""))
		print(obj[i])
	}
}

#  Often JSON data will be list of lists like {{x=1, y=2},{x=2, y=3}}
#   but we would rather have a list of vectors like {x=[1,2],y=[2,3]}
convertListOfListToListOfVectors <- function (listlist){
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