#################################### PARSING TOOLS #######################################
## These functions will be used to parse student work according to Step Type
##
##

## need this library for some step types that download data in json format.
library(rjson);

as.wiseSW = function (row){
	if (is.null(nrow(row))){
		print("The inputted row must contain column names.");
		return (NULL);
	} else if (nrow(row) > 1){
		print ("Your inputted row data frame contains more than one row, top one being used.");
		row = row[1,];
	}
	### which indices contain Student Work?
	indices = grep("Student.Work", names(row));
	sw = list();
	class(sw) = c("wiseSW","list");
	###### differentiate by type of step
	stepType = row$Step.Type[1];
	# get thte class name from the step type and prepend e.g. wise.Html is a type of wise
	class(sw) = c(paste("wiseSW.", stepType, sep=""),class(sw));
	if (stepType == "Questionnaire" || stepType == "AssessmentList"){
		# This type of step may have work on all columns of Student.Work
		for (i in indices){
			val = row[1, i];
			if (val == "" || val == "N/A") val = NA;
			sw[[length(sw)+1]] = val;
		}
	} else if (stepType == "Sensor"){
		# the sensor step may have both an open response text and a set of data points in json format
		val1 = row[1,indices[1]]; if (val1 == "" || val1 == "N/A") val1 = NA;
		val2 = row[1,indices[2]]; if (val2 == "" || val2 == "N/A") val2 = NA;
		## are either of these data points?
		if (grepl("Response #[0-9]+: ", val1) || grepl("Response #[0-9]+: ", val2)){
			if (grepl("Response #[0-9]+: ", val1)){ datal = fromJSON(sub("Response #[0-9]+: ", "",val1)); val = val2;} 
			else {datal = fromJSON(sub("Response #[0-9]+: ", "",val2)); val = val1;}
			# convert data from a long list of point pairs to a list of two numeric vectors (better for analysis)
			data = convertListOfListToListOfVectors(datal);
			sw[[1]] = val;
			sw[["data"]] = data;
		} else {
			sw[[1]] = if (nchar(val2) > nchar(val1)){val2} else {val1}
			sw[["data"]] = NA
		}
	} else if (stepType == "CarGraph"){	
		# the car graph step has json data
		val = row[1, indices[1]];
		if (val == "" || val == "N/A") val = NA;
		if (grepl("Response #[0-9]+: ", val)){ 
			datal = fromJSON(sub("Response #[0-9]+: ", "",val));
			sw[["id"]] = datal[[1]]$id; 
			sw[["data"]] = convertListOfListToListOfVectors(datal[[1]]$predictions);
		} else {
			sw[["data"]] = NA;
		} 				
	} else {
		sw[[1]] = row[1, indices[1]];
	}
	
	return (sw);
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

score = function (obj, ...) UseMethod ("score");
score.default = function (obj, ...){return(NA);}
score.wisedata.frame = function (obj, subset, select, drop = FALSE, score.type, score.templates, score.weights, is.data.frame.out = TRUE, score.colName = "Research.Score", ...){
	index = which("Index" == names(obj));
	if (index < 0){
		obj = cbind(Index = 1:nrow(obj), obj);
	}
	if (!missing(select)){obj.sub = subset(obj, subset=subset, select=select, drop=drop);}
	else{obj.sub = subset(obj, subset=subset, drop=drop);}
	# iterate threw rows of subset score each value and put in 
	scores = numeric();
	for (r in 1:nrow(obj.sub)){
		sw = as.wiseSW(obj.sub[r,]);
		s = score(obj=sw, score.type=score.type, score.templates=score.templates, score.weights=score.weights);
		scores = c(scores, s);
	}
	if (is.data.frame.out){
		sindex = which(score.colName == names(obj));
		obj[obj.sub$Index,sindex] = scores;
		return (obj);
	} else {
		return (scores);
	}
	
}
score.wiseSW.AssessmentList = function (obj, score.type, score.templates, score.weights, ...){
	if (length(obj) == 0) return (NA);

	s = 0;
	if (!missing(score.type)){
		if (score.type == "match.simple"){
			val.found = FALSE;
			for (i in 1:length(score.templates)){
				m = score.templates[i];
				if (!is.na(m) && !is.na(obj[[i]])){
					val.found = TRUE;
					if (m == obj[[i]]){
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
	}
}
score.wiseSW.Sensor = function (obj, score.type, score.templates, score.weights, ...){
	## make sure there are data points to score
	if (is.null(obj$data) || is.na(obj$data) || is.null(obj$data$x) || is.na(obj$data$x) ) return (NA);

	if (!missing(score.type)){
		if (score.type == "segments"){

		}else {
			return (pscore.data.pairs.points(obj$data$x, obj$data$y, score.templates$x.min, score.templates$x.max, score.templates$y.min, score.templates$y.max, score.weights));
		}
	}else {
		return (pscore.data.pairs.points(obj$data$x, obj$data$y, score.templates$x.min, score.templates$x.max, score.templates$y.min, score.templates$y.max, score.weights));
	}
}

pscore.data.pairs.points = function (x, y, x.min, x.max, y.min, y.max, score.weights, debug=FALSE){
	s = 0;
	val.found = FALSE;
	len = max(length(x.min),length(x.max),length(y.min),length(y.max));
	x = as.numeric(unlist(x)); y = as.numeric(unlist(y));
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

parseTimestamp = function (timestamp){
	out.list = list()
	ts = timestamp;
	r = regexpr("[A-Za-z]+ ", ts)
	out.list$Month_Name = substring(ts, r[1], r[1] + attr(r, "match.length")-2)
	out.list$Month = grep(substr(ts,1,3),month.abb)
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+, ", ts)
	out.list$Day = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-3))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+ ", ts)
	out.list$Year = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+:", ts)
	out.list$Hour = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+:", ts)
	out.list$Minute = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("[0-9]+ ", ts)
	out.list$Second = as.numeric(substring(ts, r[1], r[1] + attr(r, "match.length")-2))
	ts = substring(ts, r[1] + attr(r, "match.length"))
	r = regexpr("AM|PM", ts)
	out.list$AM_PM = substring(ts, r[1], r[1] + attr(r, "match.length")-1)
	class(out.list) = "timestamp";
	return (out.list);
}
## In seconds since midnight Jan 1st 2000
timestampAsNumeric = function (ts){
	if (class(ts) == "character") ts = parseTimestamp(ts);
	s = (ts$Year - 2000)*365.25
}


# if ts1 < ts2 return -1, if ts1 > ts2 return 1, if same return 0
compareTimestamps = function (ts1, ts2){
	if (class(ts1) == "character") ts1 = parseTimestamp(ts1);
	if (class(ts2) == "character") ts2 = parseTimestamp(ts2);
	if (ts1$Year < ts2$Year){return(-1)}
	else if (ts1$Year > ts2$Year){return(1)}
	if (ts1$Month < ts2$Month){return(-1)}
	else if (ts1$Month > ts2$Month){return(1)}
	if (ts1$Day < ts2$Day){return(-1)}
	else if (ts1$Day > ts2$Day){return(1)}
	if (ts1$AM_PM=="AM" && ts2$AM_PM=="PM"){return(-1)}
	else if (ts1$AM_PM=="PM" && ts2$AM_PM=="AM"){return(1)}
	if (ts1$Hour < ts2$Hour){return(-1)}
	else if (ts1$Hour > ts2$Hour){return(1)}
	if (ts1$Minute < ts2$Minute){return(-1)}
	else if (ts1$Minute > ts2$Minute){return(1)}
	if (ts1$Second < ts2$Second){return(-1)}
	else if (ts1$Second > ts2$Second){return(1)}
	return(0);
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