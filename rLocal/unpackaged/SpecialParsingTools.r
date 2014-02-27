### For creating special data frames

as.wiseSpecial = function(obj, type, ...) UseMethod("as.wiseSpecial")
as.wiseSpecial.default = function (obj, type, ...){return("Cannot convert input into a special data frame")}
as.wiseSpecial.wisedata.frame = function (obj, type, ...){
	if (is.null(nrow(obj)) && length(which(names(obj) == "Step.Type")) > 0){
		print("The inputted data frame must contain column names and a step type.");
		return (NULL);
	}
	if (type == "Box2dModel.history"){
		return (as.wiseSpecial.Box2dModel.history(obj, ...))
	} else {
		return (NULL)
	}
}

####### private functions for as.wiseSpecial
as.wiseSpecial.Box2dModel.history = function (obj, ...){
	out.df = data.frame()
	### use only box2d steps
	obj.b2d = subset(obj, Step.Type=="Box2dModel")
	# for each workgroup in obj.b2d get an initial time for the project
	wgs = unique(obj.b2d$Workgroup.Id)
	Initial.Time = numeric()
	for (wg in wgs){
		Initial.Time = c(Initial.Time,  min(sapply(subset(obj, Workgroup.Id==wg)$Post.Time.Server.Clock, function(t) as.numeric(as.POSIXlt(strptime(t, "%b %d, %Y %I:%M:%S %p")))),na.rm=TRUE))
	}
	df.wg = data.frame(Workgroup.Id = wgs, Initial.Time = Initial.Time)

	### go through steps
	for (s in 1:nrow(obj.b2d)){
		#print(s)
		row = obj.b2d[s,]
		sw = as.wiseSW(row)
		if (nrow(sw$history) > 0){
			table = sw$history
			info = data.frame(Workgroup.Id=rep(row$Workgroup.Id,nrow(table)), Step.Work.Id = rep(row$Step.Work.Id,nrow(table)), Step.Num = rep(row$Step.Num,nrow(table)), Step.Num.NoBranch = rep(row$Step.Num.NoBranch,nrow(table)), Post.Time.Server.Clock = rep(row$Post.Time.Server.Clock,nrow(table)), Time.Spent.Seconds = rep(row$Time.Spent.Seconds,nrow(table)))
			table = cbind(info, table)
			out.df = rbind.fill(out.df, table)
		}
	}
	out.df = out.df[!duplicated(subset(out.df,TRUE,c(Workgroup.Id, Step.Num, index, type))),]
	post = numeric()
	for (p in 1:nrow(out.df)){
		post = c(post, as.numeric(as.POSIXlt(strptime(out.df$Post.Time.Server.Clock[p], "%b %d, %Y %I:%M:%S %p"))))
	}
	#post = sapply(out.df$Post.Time.Server.Clock, function(t) as.numeric(as.POSIXlt(strptime(t, "%b %d, %Y %I:%M:%S %p"))))
	out.df$Initial.Time = merge(out.df, df.wg, by="Workgroup.Id")$Initial.Time
	out.df$Start.Time.Seconds = post - out.df$Initial.Time - out.df$Time.Spent.Seconds
	out.df$Time.From.Start = out.df$Start.Time.Seconds + out.df$time

	return (out.df)
}
box2d.history = as.wiseSpecial(wise, type = "Box2dModel.history")
