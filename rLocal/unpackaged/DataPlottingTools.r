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
		#print(s)
		#print(matrix(s, nrows, ncols, byrow=TRUE))
		layout(matrix(s, nrows, ncols, byrow=TRUE))
	}

	for (r in 1:nrow(df)){
		sw = as.wiseSW(df[r,], ...)
		plot.wiseSW(sw, plotTypes, Step.Num = df[r,]$Step.Num, Rev.Num = df[r,]$Rev.Num, Workgroup.Id = df[r,]$Workgroup.Id, ...)
	}
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
					plot.timedObservations(sw$observations, xlim, plot.file = paste(plot.dir,Step.Num,"-","-",Workgroup.Id,"-",Rev.Num,"-timedObservations.png",sep=""), ...)
				} else {
					plot.timedObservations(sw$observations, xlim, ...)
				}
				
			} else if (type == "predictions"){
				if (!is.null(plot.dir)){
					plot.predictions(sw$predictions, xlim, plot.file = paste(plot.dir,Step.Num,"-",Workgroup.Id,"-",Rev.Num,"-predictions.png",sep=""), plot.title=paste("Revision",(Rev.Num-1)), ...)
				} else {
					plot.predictions(sw$predictions, xlim, plot.title=paste("Revision",(Rev.Num-1)), ...)
				}				
			}
		}	
	}	
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
		id = unique(predictions$id)[i]
		pred = subset(predictions, id == id)
		if (nrow(pred) > 0){
			lines(pred$x, pred$y,col=cols[i],lwd=lwd,type='l')
		}
		if (!is.null(expected)){
			expec = subset(expected, id == id)
			if (nrow(expec) > 0){
				lines(expec$x, expec$y,col=cols[i],lwd=lwd/2, lty=3,type='l')
			}
		}

	}
	if (!is.null(plot.file)){
	 dev.off()
	 par(op)
	}
}

plot.timedObservations = function (observations, ...){
	if (length(observations$x) == 0) return(NULL);

	#args = as.list(substitute(list(...)))[-1L]
	args = list(...)
	if (is.null(args$xlim)){
		xlim=c(0, max(observations$x))
	} else {
		xlim = eval(args$xlim)
	}
	if (is.null(args$plot.width)){
		plot.width = 480
	} else {
		plot.width = eval(args$plot.width)
	}
	if (is.null(args$plot.height)){
		plot.height = max(observations$t/1000)*20
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

	plot(-100, -100, xlab="Graph Time", ylab="Ellapsed Time (Seconds)", xlim=xlim, ylim=c(max(observations$t/1000),0))
	### iterate through observations to look for a change in type
	prevIndex = 0
	t = numeric()
	x = numeric()
	Action = observations$Action[1]
	for (o in 1:nrow(observations)){
		obs = observations[o,]
		Index = obs$Index
		if (Index != prevIndex + 1){
			if (Action == "GraphPressed"){
				lines(x,t,col="#ff00ff",lwd=lwd)
			} else if (Action == "Play" || Action == "Start"){
				t = c(t, obs$t/1000)
				x = c(x, obs$x)
				lines(x,t,col="#44ff44",lwd=lwd)
			} 
			t = numeric()
			x = numeric()
		}
		t = c(t, obs$t/1000)
		x = c(x, obs$x)
		Action = obs$Action
		prevIndex = Index
	}
	if (!is.null(plot.file)){
	 dev.off()
	 par(op)
	}
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
i=9; plotNavigation(subset(wiseDF.gcc.cur,Workgroup.Id==unique(Workgroup.Id)[i]), Step.Num.NoBranch=sort(unique(wiseDF.gcc.cur$Step.Num.NoBranch)), by.Time = TRUE, main=paste("Linearity",1/100*round(scoreNavigation.linearity(subset(wiseDF.gcc.cur,Workgroup.Id==unique(Workgroup.Id)[i]), Step.Num.NoBranch=sort(unique(wiseDF.gcc.cur$Step.Num.NoBranch)), by.Time = TRUE)*100)))
