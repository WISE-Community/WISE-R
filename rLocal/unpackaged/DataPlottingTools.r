library(diagram)
library(plotrix)
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
