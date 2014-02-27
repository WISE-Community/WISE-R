library(diagram)
library(plotrix)

############# SPECIFIC STEP TYPE PLOTS
plot.wisedata.frame = function(df,plotTypes, ...){
	args = list(...)
	if (is.null(args$plot.dir)){
		plot.dir = NULL
	} else {
		plot.dir = eval(args$plot.dir)
	}
	if (!is.null(args$ncols)){
		ncols = eval(args$ncols)
		nrows = ceiling(nrow(df) * length(plotTypes) / ncols)
	} else if (!is.null(args$nrows)){
		## we need to make sure that nrows is a multiple of plotTypes
		nrows = ceiling(eval(args$nrows) / length(plotTypes)) * length(plotTypes)
		ncols = ceiling(nrow(df) * length(plotTypes) / nrows)
	} else {
		ncols = nrow(df)
		nrows = length(plotTypes)
	}
	# If we are plotting more than one step create appropriate par
	if (nrow(df) > 1){
		l = nrow(df)
		par(mfrow=c(nrows, ncols))
		reps = nrows / length(plotTypes)
		s = numeric()
		for (r in 1:nrows){
			rep = ceiling(r / length(plotTypes))
			rmod = (r-1) %% length(plotTypes)
			for (c in 1:ncols){
				s = c(s, ((rep-1)*length(plotTypes)*ncols)+rmod+1+(c-1)*length(plotTypes))
			}
		}
		layout(matrix(s, nrows, ncols, byrow=TRUE))
	}
	value = list()
	
	for (r in 1:nrow(df)){
		sw = as.wiseSW(df[r,])
		value[[length(value)+1]] = plot.wiseSW(sw, plotTypes, Step.Num = df[r,]$Step.Num, Rev.Num = df[r,]$Rev.Num, Workgroup.Id = df[r,]$Workgroup.Id, ...)
	}
	if (length(value)==1) value = value[[1]]
	return (value)
}

plot.wiseSW = function(sw, plotTypes, ...) UseMethod ("plot")
plot.wiseSW.default = function(sw, ...){return("No plot function for this type of step.")}
plot.wiseSW.CarGraph = function (sw, plotTypes, ...){
	#args = as.list(substitute(list(...)))[-1L]
	args = list(...)
	if (is.null(args$xlim)){
		xlim=c(0, max(c(sw$predictions$x,sw$observations$x)))
	} else {
		xlim = eval(args$xlim)
	}
	if (is.null(args$plot.dir)){
		plot.dir = NULL
	} else {
		plot.dir = eval(args$plot.dir)
	}
	if (is.null(args$Workgroup.Id)){
		Workgroup.Id = ""
	} else {
		Workgroup.Id = eval(args$Workgroup.Id)
	}
	if (is.null(args$Step.Num)){
		Step.Num = NULL
	} else {
		Step.Num = eval(args$Step.Num)
	}
	if (is.null(args$Rev.Num)){
		Rev.Num = NULL
	} else {
		Rev.Num = eval(args$Rev.Num)
	}
	if (is.null(args$plot.title)){
		plot.title = paste("Revision",(Rev.Num-1))
	} else {
		plot.title = eval(args$plot.title)
	}

	### first time count to create appropriate # of rows
	count = 0
	for (type in plotTypes){
		if (type == "timedObservations"){
			count = count + 1
		} else if (type == "predictions"){
			count = count + 1
		}
	}

	if (count > 0){
		for (type in plotTypes){
			if (type == "timedObservations"){
				if (!is.null(plot.dir)){
					value = plot.timedObservations(sw$observations, xlim, plot.file = paste(plot.dir,Step.Num,"-","-",Workgroup.Id,"-",Rev.Num,"-timedObservations.png",sep=""), ...)
				} else {
					value = plot.timedObservations(sw$observations, xlim, ...)
				}
				
			} else if (type == "predictions"){
				if (!is.null(plot.dir)){
					value = plot.predictions(sw$predictions, xlim, plot.file = paste(plot.dir,Step.Num,"-",Workgroup.Id,"-",Rev.Num,"-predictions.png",sep=""), plot.title=plot.title, ...)
				} else {
					value = plot.predictions(sw$predictions, xlim, plot.title=plot.title, ...)
				}				
			}
		}	
	}	
	return (value)
}
plot.wiseSW.Grapher = function (sw, plotTypes, ...){
	#args = as.list(substitute(list(...)))[-1L]
	args = list(...)
	if (is.null(args$xlim)){
		xlim=c(0, max(c(sw$predictions$x,sw$observations$x)))
	} else {
		xlim = eval(args$xlim)
	}
	if (is.null(args$plot.dir)){
		plot.dir = NULL
	} else {
		plot.dir = eval(args$plot.dir)
	}
	if (is.null(args$Workgroup.Id)){
		Workgroup.Id = ""
	} else {
		Workgroup.Id = eval(args$Workgroup.Id)
	}
	if (is.null(args$Step.Num)){
		Step.Num = NULL
	} else {
		Step.Num = eval(args$Step.Num)
	}
	if (is.null(args$Rev.Num)){
		Rev.Num = NULL
	} else {
		Rev.Num = eval(args$Rev.Num)
	}
	if (is.null(args$plot.title)){
		plot.title = NULL
	} else {
		plot.title = eval(args$plot.title)
	}

	### first time count to create appropriate # of rows
	count = 0
	for (type in plotTypes){
		if (type == "predictions"){
			count = count + 1
		}
	}

	if (count > 0){
		for (type in plotTypes){
			if (type == "predictions"){
				if (!is.null(plot.dir)){
					plot.predictions(sw$predictions, xlim, plot.file = paste(plot.dir,Step.Num,"-",Workgroup.Id,"-",Rev.Num,"-predictions.png",sep=""), plot.title=plot.title, ...)
				} else {
					plot.predictions(sw$predictions, xlim, plot.title=plot.title, ...)
				}				
			}
		}	
	}	
}
plot.wiseSW.Sensor = function (sw, plotTypes, ...){
	#args = as.list(substitute(list(...)))[-1L]
	args = list(...)
	predictions = as.data.frame(tail(sw$predictions, 1))
	if (is.null(args$xlim)){
		if (nrow(predictions) > 0) { xlim=c(0, max(predictions$x)) }
		else {xlim = c(0,1)}
	} else {
		xlim = eval(args$xlim)
	}
	if (is.null(args$plot.dir)){
		plot.dir = NULL
	} else {
		plot.dir = eval(args$plot.dir)
	}
	if (is.null(args$Workgroup.Id)){
		Workgroup.Id = ""
	} else {
		Workgroup.Id = eval(args$Workgroup.Id)
	}
	if (is.null(args$Step.Num)){
		Step.Num = NULL
	} else {
		Step.Num = eval(args$Step.Num)
	}
	if (is.null(args$Rev.Num)){
		Rev.Num = NULL
	} else {
		Rev.Num = eval(args$Rev.Num)
	}
	if (is.null(args$plot.title)){
		plot.title = NULL
	} else {
		plot.title = eval(args$plot.title)
	}

	### first time count to create appropriate # of rows
	count = 0
	for (type in plotTypes){
		if (type == "predictions"){
			count = count + 1
		}
	}

	if (count > 0){
		for (type in plotTypes){
			if (type == "predictions"){
				if (nrow(predictions) == 0){
					plot(0, 0, xmain=plot.title, ...)
				} else if (!is.null(plot.dir)){
					plot.predictions(predictions, xlim, plot.file = paste(plot.dir,Step.Num,"-",Workgroup.Id,"-",Rev.Num,"-predictions.png",sep=""), plot.title=plot.title, ...)
				} else {
					plot.predictions(predictions, xlim, plot.title=plot.title, ...)
				}				
			}
		}	
	}	
}
#plot(sdf[1,],plotTypes="predictions", xlim=c(0,120), ylim=c(0,2200))
# library(utils)
plot.wiseSW.Mysystem2 = function (sw, plotTypes, ...){
	if (plotTypes[1] == "svg"){
		svgString ='';
		svgOpenTagPattern = '<svg [^>]+>';
		imagePattern = '(<image .*?>)(<\\/image>)?';
		dimensionPattern = '(<image x="\\d+" y="\\d+") (width="10" height="10)'
		goodSVGTag  = '<svg style="position:relative; left:0; top:0; width:100%; height:100%;" preserveAspectRatio="xMinYMin meet" viewBox="0 40 800 600" width="300" height="150" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">';

		if(!is.null(sw$Svg[1])) {
		  	#get the MySystem.GraphicPreview field
			svgString = sw$Svg[1];

			#unescape the svg string
			svgString = URLdecode(svgString);

			#decompress the svg string
			#svgString = lz77.decompress(svgString);

			svgString = gsub(svgOpenTagPattern,goodSVGTag, svgString);

			#replace all instances of imagexlink with image xlink
			svgString = gsub('imagexlink', 'image xlink', svgString);

			#fix incorrect image widths and heights...

			svgString = gsub(dimensionPattern,"$1 width='100' height='110'", svgString);

			#find the first match
			match = grep(imagePattern, svgString, value=TRUE);

			#loop until we have found all the image tags
			while(length(match) != 0) {
			    #get the whole match
			    wholeMatch = match[0];

			    #get the image opening tag
			    match1 = match[1];

			    #get the image closing tag if it exists
			    match2 = match[2];

			    if(match2 == null) {
			        #the image closing tag does not exist so we will insert it
			        svgString = gsub(wholeMatch, wholeMatch + "</image>", svgString);
			    }

			    #find the next match
			    match = imagePattern.exec(svgString)
			}

			if(svgString == '') {
			  #this student didn't do any work
			  studentDataHtml = 'No Student Work';
			} else {
			  #the svg string is the student work
			  studentDataHtml = svgString;
			}
			#$('#stepwork'+stepWorkId).html(studentDataHtml);
			#$('#score'+stepWorkId).html(score);
			#$('#categories'+stepWorkId).html(categories);
		}
	}
}
############# GENERAL GRAPH TYPES
plot.predictions = function (predictions, ...){
	#args = as.list(substitute(list(...)))[-1L]
	args = list(...)
	if (is.null(args$xlim)){
		xlim=c(0, max(predictions$x))
	} else {
		xlim = eval(args$xlim)
	}
	if (is.null(args$ylim)){
		ylim=c(0, max(predictions$y))
	} else {
		ylim = eval(args$ylim)
	}
	if (is.null(args$expected)){
		expected = NULL
	} else {
		expected = eval(args$expected)
	}
	if (is.null(args$cols)){
		cols = rainbow(length(unique(predictions$id)))
	} else {
		cols = eval(args$cols)
	}
	if (is.null(args$plot.width)){
		plot.width = 480
	} else {
		plot.width = eval(args$plot.width)
	}
	if (is.null(args$plot.height)){
		plot.height = 320
	} else {
		plot.height = eval(args$plot.height)
	}
	if (is.null(args$plot.file)){
		plot.file = NULL
	} else {
		plot.file = eval(args$plot.file)
		op = par(mar=c(0,0,0,0))
		png(plot.file, plot.width, plot.height)
	}
	if (is.null(args$lwd)){
		lwd = 2
	} else {
		lwd = eval(args$lwd)
	}
	if (is.null(args$plot.title)){
		main = ""
	} else {
		main = eval(args$plot.title)
	}
	plot(-100, -100, xlab="Time", ylab="Position", xlim=xlim, ylim=ylim, main=main)
	for (i in 1:length(unique(predictions$id))){
		sid = as.character(unique(predictions$id)[i])
		pred = subset(predictions, id == sid)
		if (!is.null(pred) && nrow(pred) > 0){
			lines(pred$x, pred$y,col=cols[i],lwd=lwd,type='b')
		}
		if (!is.null(expected)){
			expec = subset(expected, id == sid)
			if (!is.null(pred) && nrow(expec) > 0){
				lines(expec$x, expec$y,col=cols[i],lwd=lwd, lty=3,type='l')
			}
		}
	}
	if (!is.null(plot.file)){
	 dev.off()
	 par(op)
	}
}

# t.range range of t values, t(0, T)
# return dimensions of plot
plot.timedObservations = function (observations, ...){
	if (length(observations$x) == 0) return(NULL);

	#args = as.list(substitute(list(...)))[-1L]
	args = list(...)
	if (is.null(args$xlim)){ xlim=c(0, max(observations$x))
	} else { xlim = eval(args$xlim) }

	if (is.null(args$t.range)){ t.range = c(0, max(observations$t))
	} else { t.range = eval (args$t.range)}
	# update observations for t.range
	observations = subset(observations, t >= t.range[1] & t <= t.range[2])

	if (is.null(args$plot.width)){plot.width = 480
	} else {plot.width = eval(args$plot.width)}

	if (is.null(args$plot.height)){ plot.height = (t.range[2]/1000-t.range[1]/1000) * 20
	} else { plot.height = eval(args$plot.height) } 
	
	if (is.null(args$lwd)){ lwd = 2
	} else { lwd = eval(args$lwd)}

	if (is.null(args$xlab)){ xlab = "Graph Time"
	} else { xlab = eval(args$xlab)}

	if (is.null(args$ylab)){ ylab = "Ellapsed Time (Seconds)"
	} else { ylab = eval(args$ylab)}

	if (is.null(args$plot.file)){
		plot.file = NULL
	} else {
		plot.file = eval(args$plot.file)
		op = par(mar=c(2,2,0.1,0.1))
		if (is.null(args$pointsize)){ pointsize = 12
		} else { pointsize = eval(args$pointsize)}

		png(plot.file, plot.width, plot.height, pointsize = pointsize)
	}
	

	plot(-100, -100, xlab=xlab, ylab=ylab, xlim=xlim, ylim=c(t.range[2]/1000,t.range[1]/1000))
	### iterate through observations to look for a change in type
	prevIndex = 0
	t = numeric()
	x = numeric()
	if (is.na(observations$Action[1]) || is.null(observations$Action[1])) { Action = ""}
	else {Action = observations$Action[1]}
	for (o in 1:nrow(observations)){
		obs = observations[o,]
		Index = obs$Index
		if (Index != prevIndex + 1){
			if (Action == "GraphPressed"){
				lines(x,t,col="#ff00ff",lwd=lwd)
			} else if (Action == "Play" || Action == "Start"){
				t = c(t, obs$t/1000)
				x = c(x, obs$x)
				lines(x,t,col="#44ff44",lwd=lwd, lty="dashed")
			} 
			t = numeric()
			x = numeric()
		}
		t = c(t, obs$t/1000)
		x = c(x, obs$x)
		Action = obs$Action
		if (is.na(obs$Action) || is.null(obs$Action)) { Action = ""}
		else {Action = obs$Action}
		prevIndex = Index
	}
	
	if (!is.null(plot.file)){
		dev.off()
		par(op)
	}
	return (c(plot.width, plot.height))
}

error.bar = function(x, y, upper, lower=upper, length=0.1,...){
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
	stop("vectors must be same length")
	arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

scoreTransitionGraph = function(df, Step.Num.1 = NA, Step.Num.2 = NA, Step.Num.NoBranch.1 = NA, Step.Num.NoBranch.2 = NA, colName="Research.Score", VERBOSE=FALSE, DEBUG = FALSE){
	if (!is.na(Step.Num.1) && !is.na(Step.Num.2)){
		df = subset(df, Step.Num %in% c(Step.Num.1, Step.Num.2))
	} else if (!is.na(Step.Num.NoBranch.1) && !is.na(Step.Num.NoBranch.2)){
		df = subset(df, Step.Num.NoBranch %in% c(Step.Num.NoBranch.1, Step.Num.NoBranch.2))
	} else {
			stop("You did not specify a valid Step Num or Step Num NoBranch")
	}
	cindex = which(names(df) == colName)[1]
	kiVals = sort(unique(df[,colName]))
	M = matrix(nrow = length(kiVals), ncol = length(kiVals), byrow = TRUE, data = 0)
	C = matrix(nrow = length(kiVals), ncol = length(kiVals), byrow = TRUE, data = -0.001)
	A = M
	col = M
	# cycle through workgroup and tally transition
	for (w in 1:length(unique(df$Workgroup.Id))){
		wid = unique(df$Workgroup.Id)[w]
		val.1 = NA
		val.2 = NA
		if (!is.na(Step.Num.1) && !is.na(Step.Num.2)){
			val.1 = subset(df, Workgroup.Id==wid&Step.Num==Step.Num.1)[, cindex][1]
			val.2 = subset(df, Workgroup.Id==wid&Step.Num==Step.Num.2)[, cindex][1]
			main = paste("KI Scores from Step", Step.Num.1,"to Step",Step.Num.2);
		} else if (!is.na(Step.Num.NoBranch.1) && !is.na(Step.Num.NoBranch.2)){
			val.1 = subset(df, Workgroup.Id==wid&Step.Num.NoBranch==Step.Num.NoBranch.1)[, cindex][1]
			val.2 = subset(df, Workgroup.Id==wid&Step.Num.NoBranch==Step.Num.NoBranch.2)[, cindex][1]
			main = paste("KI Scores from Step", Step.Num.NoBranch.1,"to Step",Step.Num.NoBranch.2);
		} else {
			stop("You did not specify a valid Step Num or Step Num NoBranch")
		}
		if(DEBUG) print(paste("Workgroup.Id",wid,"KI 1", val.1, "KI 2", val.2))
		### update matrices
		#if (!is.na(val.1) && length(val.1) > 1) val.1 = val.1[1]
		#if (!is.na(val.2) && length(val.2) > 1) val.2 = val.2[1]

		if (!is.na(val.1) && !is.na(val.2) && val.1 != val.2){
			index.1 = which(kiVals == val.1)
			index.2 = which(kiVals == val.2)
			M[index.2, index.1] = M[index.2, index.1] + 1
			#A[index.2, index.1] = max(A[index.2, index.1] + 0.2, 1.0)
			A[index.2, index.1] = A[index.2, index.1] + 2
			if (val.2 > val.1) {
				col[index.2, index.1] = "blue"
			} else {
				col[index.2, index.1] = "red" 
			}
		}
	}
	pos = numeric()
	for (v in unique(floor(kiVals/100))){
		pos = c(pos, sum(floor(kiVals/100)==v))
	}
	#coords  = coordinates(NULL, N=length(unique(kiVals)))
	coords  = coordinates(pos)
	openplotmat()
	# draw rectangles
	radx = 0.06
	rady = 0.04
	for (i in 1:nrow(coords)){
		print(textrect (mid = coords[i,], radx = radx, rady = rady, lab = kiVals[i]))
	}
	for (i in 1:nrow(coords)){
		## find links from this item
		
		if (i + 1 <= nrow(coords)){
			for (j in i:nrow(coords)){
				val.pos = M[j,i]
				val.neg = M[i,j]
				d.t = 0.00
				d.a = 0.025
				if (val.pos > 0){
					angle = atan((coords[j,2] - coords[i,2]) / (coords[j,1] - coords[i,1]))
					dist = sqrt((coords[j,2] - coords[i,2])^2 +  (coords[j,1] - coords[i,1])^2)
					from = coords[i,]
					to = coords[j,]
					#from[1] = from[1] + d.a*cos(angle+pi/2)
					#from[2] = from[2] + d.a*sin(angle+pi/2)
					#to[1] = to[1] + d.a*cos(angle+pi/2)
					#to[2] = to[2] + d.a*sin(angle+pi/2)
					if (from[2] == to[2]){
						## on same plane
						from[1] = from[1] + radx/1
						from[2] = from[2] + rady/2
						to[1] = to[1] - radx/1
						to[2] = to[2] + rady/2
					} else {
						from[1] = from[1] + radx/2
						from[2] = from[2] - rady/1
						to[1] = to[1] + radx/2
						to[2] = to[2] + rady/1
					}
					curve = 0
					if (from[1] == to[1] && dist > 0.25){
						curve = -radx
					} else if (from[2] == to[2] && dist > 0.25){
						curve = -rady
					}
					arr.width = 0.6
					arr.length = 0.6;#sqrt(arr.width^2 - (arr.width/2)^2)
					print(paste("positive: from (", from[1], from[2], ") to (", to[1], to[2],")"))
					#curvedarrow(from, to, lwd = 1 + val.pos, lcol="blue", curve = curve, arr.pos=0.5+max(0.4,dist/2),endhead=TRUE)
					mid = curvedarrow(from, to, lwd = 1 + val.pos, lcol="blue", curve = curve, arr.type="triangle",arr.length=arr.length,arr.width=arr.width)
					text(mid[1]+d.t*cos(angle+pi/2), mid[2]+d.t*sin(angle+pi/2), val.pos, adj=c(0.5, 0.5), cex=1.0, col="white")
				}
				if (val.neg > 0){
					angle = atan((coords[i,2] - coords[j,2]) / (coords[i,1] - coords[j,1]))
					dist = sqrt((coords[i,2] - coords[j,2])^2 +  (coords[i,1] - coords[j,1])^2)
					from = coords[j,]
					to = coords[i,]
					#from[1] = from[1] + d.a*cos(angle+pi/2)
					#from[2] = from[2] + d.a*sin(angle+pi/2)
					#to[1] = to[1] + d.a*cos(angle+pi/2)
					#to[2] = to[2] + d.a*sin(angle+pi/2)
					if (from[2] == to[2]){
						from[1] = from[1] - radx/1
						from[2] = from[2] - rady/2
						to[1] = to[1] + radx/1
						to[2] = to[2] - rady/2
					} else {
						from[1] = from[1] - radx/2
						from[2] = from[2] + rady/1
						to[1] = to[1] - radx/2
						to[2] = to[2] - rady/1
					}
					curve = 0
					if (from[1] == to[1] && dist > 0.25){
						curve = -radx
					} else if (from[2] == to[2] && dist > 0.25){
						curve = -rady
					}
					arr.width = 0.6
					arr.length = 0.6;#sqrt(arr.width^2 - (arr.width/2)^2)
					print(paste("negative: from (", from[1], from[2], ") to (", to[1], to[2],")"))
					#curvedarrow(from, to, lwd = 1 + val.neg, lcol="red", curve = curve, arr.pos=0.5+max(0.4,dist/2),endhead=TRUE)
					mid = curvedarrow(from, to, lwd = 1 + val.neg, lcol="red", curve = curve,arr.type="triangle",arr.length=arr.length,arr.width=arr.width)
					text(mid[1]+d.t*cos(angle+pi/2), mid[2]+d.t*sin(angle+pi/2), val.neg, adj=c(0.5, 0.5), cex=1.0, col="white")
				}
			}
		}
	}
}

# library(plotrix)
plotNavigation = function(sdf, Step.Num.NoBranch=sort(unique(sdf$Step.Num.NoBranch)), by.Workgroup.Id=TRUE, by.Time=FALSE, ylim=c(as.numeric(as.character(Step.Num.NoBranch[1])), as.numeric(as.character(tail(Step.Num.NoBranch,1)))), show.score.colName="Research.Score", ...){
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
		xlab = "Cumulative Time (seconds)"
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
		xlab = "Step Advancement"
		xlim = c(1,length(sdf$Step.Num))
		xvals = 1:length(sdf$Step.Num)
		yvals = match(sdf$Step.Num.NoBranch,Step.Num.NoBranch)
	}
	### translate from step number to step count
	ylim = c(match(ylim[1],Step.Num.NoBranch), match(ylim[2],Step.Num.NoBranch))
	plot(xvals, yvals, xlim = xlim, ylim=ylim, yaxt="n", type="l", xlab = xlab, ylab="Step", col="#666666", xpd = NA, ...)
	axis(2, at = Step.Count.NoBranch, labels=Step.Num.NoBranch)
	prevxpos = -max(xvals)
	prevypos = -max(yvals)
	if (!is.null(show.score.colName) &&!is.null(show.score.colName) && show.score.colName %in% names(sdf)){
		sindex = which(names(sdf) == show.score.colName)
		count = 0
		for (r in 1:nrow(sdf)){
			row = sdf[r,]
			if (!is.na(row[1,sindex])){
				if (by.Time){
					xpos = ctime[r]
					ypos = yvals.inc[r]
				} else {
					xpos = r
					ypos = yvals[r]
				}
				
				val = row[1,sindex]

				txpos = xpos-0.05 * max(xvals)
				typos = ypos+0.1 * max(yvals)
				tcol = "blue" 
				adj = c(1, 0)	
				wcount = 0
				while (((abs(txpos - prevxpos) < (0.1 * max(xvals))) && (abs(typos - prevypos) < (0.05 * max(yvals)))) && wcount < 10){
					dir = 1
					#if (ypos > max(yvals)/2) dir = -1
					typos = prevypos + dir * wcount * 0.01 * max(yvals)
					wcount = wcount + 1
					
				}
				text(txpos, typos, labels=val, col=tcol, xpd = NA, adj=adj)
				segments(txpos, typos, xpos, ypos, col="blue", lwd=0.5, xpd = NA)
				draw.circle(xpos, ypos, radius=max(xvals)/150,col="red")
				count = count + 1
				prevxpos = txpos
				prevypos = typos
			} 
		}
	}
}

### Given an aggregrated data frame
### Show comparison of condition across all vars
### e.g. vars = c("Score.Pretest", "Score.Posttest")
### if want to show over different subsets of agg use agg.subset (if length of vars < length of aggs.subset vars will be repeated)
### e.g. agg.subsets = c(Score.Pretest < 3, SCORE.Pretest > 2)
barplot.by.condition = function (agg, vars, agg.subsets, conditions = sort(unique(agg$Condition)), var.names=as.character(vars), condition.names=conditions, show.p.markers.between = TRUE, show.p.markers.across = FALSE, col = rainbow(length(conditions)), hide.legend = FALSE, legend.text = paste("Conditon", conditions),legend.padding.factor=5, legend.padding.xoffset=0, yaxis.at = NULL, mar.lims = NULL, ...){
	#if (class(vars)[1] == "character"){
	#	vars = which(names(agg) %in% vars)
	#}
	if (missing(agg.subsets)) 
        subset.logical = TRUE
    else {
		exp = substitute(agg.subsets)
		subset.logical = eval(exp, agg, parent.frame())
		if (is.logical(subset.logical)) {
			subset.logical = subset.logical & ! is.na(subset.logical)
		} else if (is.list(subset.logical)){
			for (i in 1:length(subset.logical))subset.logical[[i]] = subset.logical[[i]] & !is.na(subset.logical[[i]])
		}
	}
	
	if (is.list(subset.logical) && length(vars) < length(subset.logical)) vars = c(vars, rep(vars[length(vars)], length(subset.logical) - length(vars)))
	means = matrix (rep(NA,length(vars)*length(conditions)),nrow=length(conditions),ncol=length(vars))
	ses = matrix (rep(NA,length(vars)*length(conditions)),nrow=length(conditions),ncol=length(vars))
	for (c in 1:ncol(means)){
		for (r in 1:nrow(means)){
			if (is.list(subset.logical)){
				means[r, c] = mean(subset(agg,subset.logical[[c]]&Condition==conditions[r])[,vars[c]], na.rm=TRUE)
				ses[r, c] = std.error(subset(agg,subset.logical[[c]]&Condition==conditions[r])[,vars[c]], na.rm=TRUE)
			} else {
				means[r, c] = mean(subset(agg,subset.logical&Condition==conditions[r])[,vars[c]], na.rm=TRUE)
				ses[r, c] = std.error(subset(agg,subset.logical&Condition==conditions[r])[,vars[c]], na.rm=TRUE)
			}
		}
	}
	### repeat title if necessary
	#print(means)
	ten.percent.y = max(means+ses)/10
	if (is.null(mar.lims)) {par(mar=c(5,4,4,2))}
	else {par(mar = mar.lims)}
	#print(mat)
	#barx = barplot(means, beside=TRUE, col=c("blue", "red"), names.arg=var.names, legend.text=legend.text, legend.args=list(x=)...)
	#print(barx)
	if (is.null(yaxis.at)){
		barx = barplot(means, beside=TRUE, col=col, names.arg=var.names, xpd=FALSE,...)
		par(xpd=TRUE)
	} else {
		barx = barplot(means, beside=TRUE, col=col, names.arg=var.names, xpd=FALSE, axes=FALSE, ...)
		par(xpd=TRUE)
		axis(2, at=yaxis.at)
	}
	
	if (!hide.legend) legend(x = max(barx)*2/3+legend.padding.xoffset, y = max(means+ses) + legend.padding.factor*ten.percent.y, legend = legend.text, col = col, pt.bg = col, pch=22)
	axis(1, at=apply(barx,2,mean), labels=FALSE)
	error.bar(barx, means, ses)
	if (show.p.markers.between && length(conditions)==2){
		p.values = numeric()
		for (c in 1:ncol(means)){
			if (is.list(subset.logical)){
				vals.1 = subset(agg,subset.logical[[c]]&Condition==conditions[1])[,vars[c]]
				vals.2 = subset(agg,subset.logical[[c]]&Condition==conditions[2])[,vars[c]]
			} else {
				vals.1 = subset(agg,subset.logical&Condition==conditions[1])[,vars[c]]
				vals.2 = subset(agg,subset.logical&Condition==conditions[2])[,vars[c]]
			}
			p.value = t.test(vals.1, vals.2, var.equal=TRUE)$p.value
			p.values = c(p.values,p.value)
			if (p.value < 0.1){
				segments(barx[1,c],max(means[,c]+ses[,c])+ten.percent.y,barx[2,c],max(means[,c]+ses[,c])+ten.percent.y)
				segments(barx[1,c],max(means[,c]+ses[,c])+ten.percent.y/2,barx[1,c],max(means[,c]+ses[,c])+ten.percent.y)
				segments(barx[2,c],max(means[,c]+ses[,c])+ten.percent.y/2,barx[2,c],max(means[,c]+ses[,c])+ten.percent.y)
				segments(mean(barx[,c]),max(means[,c]+ses[,c])+ten.percent.y,mean(barx[,c]),max(means[,c]+ses[,c])+ten.percent.y*3/2)
				if (p.value < 0.001){
					marker = "***"
				} else if (p.value < 0.01){
					marker = "**"
				} else if (p.value < 0.05){
					marker = "*"
				} else if (p.value < 0.1){
					marker = "\u2020"
				}
				text(mean(barx[,c]),max(means[,c]+ses[,c])+ten.percent.y*2,labels=marker)
			}
		}
	}
	if (show.p.markers.across && length(vars)==2){
		p.values = numeric()
		for (r in 1:nrow(means)){
			if (is.list(subset.logical)){
				vals.1 = subset(agg,subset.logical[[1]]&Condition==conditions[r])[,vars[1]]
				vals.2 = subset(agg,subset.logical[[2]]&Condition==conditions[r])[,vars[2]]
			} else {
				vals.1 = subset(agg,subset.logical&Condition==conditions[r])[,vars[1]]
				vals.2 = subset(agg,subset.logical&Condition==conditions[r])[,vars[2]]
			}
			p.value = t.test(vals.1, vals.2, paired=TRUE,var.equal=TRUE)$p.value
			p.values = c(p.values,p.value)
			if (p.value < 0.1){
				segments(barx[r,1],max(means[r,]+ses[r,])+ten.percent.y,barx[r,2],max(means[r,]+ses[r,])+ten.percent.y)
				segments(barx[r,1],max(means[r,]+ses[r,])+ten.percent.y/2,barx[r,1],max(means[r,]+ses[r,])+ten.percent.y)
				segments(barx[r,2],max(means[r,]+ses[r,])+ten.percent.y/2,barx[r,2],max(means[r,]+ses[r,])+ten.percent.y)
				segments(mean(barx[r,]),max(means[r,]+ses[r,])+ten.percent.y,mean(barx[r,]),max(means[r,]+ses[r,])+ten.percent.y*3/2)
				if (p.value < 0.001){
					marker = "***"
				} else if (p.value < 0.01){
					marker = "**"
				} else if (p.value < 0.05){
					marker = "*"
				} else if (p.value < 0.1){
					marker = "\u2020"
				}
				text(mean(barx[r,]),max(means[r,]+ses[r,])+ten.percent.y*2,labels=marker)
			}
		}
	}
}

