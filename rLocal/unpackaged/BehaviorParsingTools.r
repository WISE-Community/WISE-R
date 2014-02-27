############
### This function iterates through a wise data frame to construct individual behaviors
behaviors = function (obj, ...) UseMethod ("behaviors");
behaviors.default = function (obj, ...){return(NULL);}
behaviors.wisedata.frame = function (obj, Column.Names = c("Workgroup.Id", "Wise.Id.1", "Wise.Id.2", "Wise.Id.3", "Step.Num", "Step.Num.NoBranch", "Step.Title", "Step.Id", "Step.Work.Id", "Start.Time.Student.Clock", "Time.Spent.Seconds", "Step.Type"), ...){
	behaviors = data.frame()	
	wids = unique(obj$Workgroup.Id)
	for (wid in wids){
		obj.wg = subset(obj, Workgroup.Id == wid)
		behavior.count = 0
		for (r in 1:nrow(obj.wg)){
			# we are pasting on name of type to function being called, if no such function, than use a default
			step.type = obj.wg[r,]$Step.Type
			if (step.type == "Sensor") step.type = "Grapher"
			behs = tryCatch( 
				do.call(paste("behaviors", step.type, sep="."), list(obj = obj.wg, rownum = r, Column.Names = Column.Names))
				, error = function(e) return (NULL)) 
			if (is.null(behs)){
				behs = do.call("behaviors.generic", list(obj = obj.wg, rownum = r, Column.Names = Column.Names))
			}		
			if (nrow(behs) > 0){
				#print(behs$Behavior.Name)
				#print(behs$Behavior.Num)
				behs$Behavior.Num = behs$Behavior.Num + behavior.count
				#print(names(behs))
				behaviors = rbind(behaviors, behs)
				behavior.count = tail(behs$Behavior.Num, 1)
			}
		}
	}
	
	return (behaviors)
}
behaviors.Table = function (obj, rownum, include.same = TRUE, Column.Names = c("Workgroup.Id", "Wise.Id.1", "Wise.Id.2", "Wise.Id.3", "Step.Num", "Step.Num.NoBranch", "Step.Title", "Step.Id", "Step.Work.Id", "Start.Time.Student.Clock", "Time.Spent.Seconds", "Step.Type"), ...){
	row = obj[rownum, ]
	sw = as.wiseSW(row)
	### Table behaviors are given in relationship to any previous instances of the table on this step
	### (or from imported steps [TODO])
	Behavior.Name = character() ## can be "Add Cell", "Delete Cell", "Revise Cell"
	Value.Old = character() ## what was previously in cell for "Delete Cell", "Revise Cell", Blank for "Add Cell"
	Value.New = character() ## what is now in cell for "Add Cell" and "Revise Cell", blank for "Delete Cell"
	Location.Old = character() # what cell did this take place in? (row, col)
	Location.New = character() # what cell did this take place in? (row, col)
	### is there a previous instance of this step for this workgroup?
	prevrows = subset(obj,obj$Workgroup.Id==row$Workgroup.Id & obj$Step.Num==row$Step.Num & Index<row$Index & nchar(Student.Work.Part.1) > 1)
	if (nrow(prevrows) > 0 && !is.null(sw$table) && nrow(sw$table) > 0 && !is.null(as.wiseSW(tail(prevrows, 1))$table) && nrow(as.wiseSW(tail(prevrows, 1))$table) > 0){
		tbl = sw$table
		oldtbl = as.wiseSW(tail(prevrows, 1))$table

		### TODO in case a row can be added or deleted should look for max of rows, cols between both tables
		for (r in 1:nrow(tbl)){
			for (c in 1:ncol(tbl)){
				if (as.character(tbl[r, c]) != as.character(oldtbl[r, c]) && nchar(as.character(oldtbl[r, c])) == 0){
					Behavior.Name = c(Behavior.Name, "Add Cell")
					Location.Old = c(Location.Old, paste("(Row: ",r,", Col: ", c, ")",sep=""))
					Location.New = c(Location.New, paste("(Row: ",r,", Col: ", c, ")",sep=""))	
					Value.Old = c(Value.Old, "")
					Value.New = c(Value.New, as.character(tbl[r, c]))	
				} else if (as.character(tbl[r, c]) != as.character(oldtbl[r, c]) && nchar(as.character(tbl[r, c])) == 0){
					Behavior.Name = c(Behavior.Name, "Delete Cell")
					Location.Old = c(Location.Old, paste("(Row: ",r,", Col: ", c, ")",sep=""))
					Location.New = c(Location.New, paste("(Row: ",r,", Col: ", c, ")",sep=""))	
					Value.Old = c(Value.Old, as.character(oldtbl[r, c]))
					Value.New = c(Value.New, "")		
				} else if (as.character(tbl[r, c]) != as.character(oldtbl[r, c])){
					Behavior.Name = c(Behavior.Name, "Revise Cell")
					Location.Old = c(Location.Old, paste("(Row: ",r,", Col: ", c, ")",sep=""))
					Location.New = c(Location.New, paste("(Row: ",r,", Col: ", c, ")",sep=""))		
					Value.Old = c(Value.Old, as.character(oldtbl[r, c]))
					Value.New = c(Value.New, as.character(tbl[r, c]))		

				} else {
					## same as before, don't add anything
					if (include.same){
						Behavior.Name = c(Behavior.Name, "Keep Cell")
						Location.Old = c(Location.Old, paste("(Row: ",r,", Col: ", c, ")",sep=""))
						Location.New = c(Location.New, paste("(Row: ",r,", Col: ", c, ")",sep=""))		
						Value.Old = c(Value.Old, as.character(oldtbl[r, c]))
						Value.New = c(Value.New, as.character(tbl[r, c]))		
					}
				}
			}
		}
	} else if (!is.null(sw$table) && nrow(sw$table) > 0) {
		### everything is now added.
		tbl = sw$table
		for (r in 1:nrow(tbl)){
			for (c in 1:ncol(tbl)){
				if (nchar(as.character(tbl[r, c])) > 0){
					Behavior.Name = c(Behavior.Name, "Add Cell")
					Location.Old = c(Location.Old, paste("(Row: ",r,", Col: ", c, ")",sep=""))
					Location.New = c(Location.New, paste("(Row: ",r,", Col: ", c, ")",sep=""))	
					Value.Old = c(Value.Old, "")
					Value.New = c(Value.New, as.character(tbl[r, c]))
				}
			}
		}
	} else {
		Behavior.Name = "None"
		Location.Old = "NA"
		Location.New = "NA"
		Value.Old = "None"
		Value.New = "None"
	}
	behavior = subset(row, TRUE, Column.Names)
	behavior = behavior[rep(seq_len(nrow(behavior)), each=length(Behavior.Name)),]
	behavior$Behavior.Num = rep(1, length(Behavior.Name))
	behavior$Behavior.Time.Spent = rep(row$Time.Spent.Seconds, length(Behavior.Name))
	behavior$Behavior.Name = Behavior.Name
	behavior$Location.Old = Location.Old
	behavior$Location.New = Location.New
	behavior$Value.Old = Value.Old
	behavior$Value.New = Value.New
	return (behavior)
}
behaviors.Grapher = function (obj, rownum, include.same = TRUE, Column.Names = c("Workgroup.Id", "Wise.Id.1", "Wise.Id.2", "Wise.Id.3", "Step.Num", "Step.Num.NoBranch", "Step.Title", "Step.Id", "Step.Work.Id", "Start.Time.Student.Clock", "Time.Spent.Seconds", "Step.Type"), ...){
	row = obj[rownum, ]
	sw = as.wiseSW(row)
	### Graph behaviors are given in relationship to any previous instances of the graph on this step
	### (or from imported steps [TODO])
	Behavior.Name = character() ## can be "Add Point", "Delete Point", "Revise Point"
	Location.Old = character() # where was the point at this index placed previously? (x, y)
	Location.New = character() # where is this point placed? (x, y)
	Value.Old = character() ## same as new
	Value.New = character() ## what is the name of the series for this pont
	
	### is there a previous instance of this step for this workgroup?
	prevrows = subset(obj,obj$Workgroup.Id==row$Workgroup.Id & obj$Step.Num==row$Step.Num & Index<row$Index & nchar(Student.Work.Part.1) > 1)
	if (nrow(prevrows) > 0){
		if (!is.null(sw$predictions)){
			predictions = sw$predictions
			if (is.list(predictions)) predictions = tail(predictions,1)[[1]]
		} else {
			predictions = data.frame(id=character(), x=numeric(), y=numeric())
		}
		oldsw =  as.wiseSW(tail(prevrows, 1))
		if (!is.null(oldsw$predictions)){
			oldpredictions = oldsw$predictions
			if (is.list(oldpredictions)) oldpredictions = tail(oldpredictions,1)[[1]]
		} else {
			oldpredictions = data.frame(id=character(), x=numeric(), y=numeric())
		}
		ids = unique(c(as.character(predictions$id),as.character(oldpredictions$id)))
		### TODO in case a row can be added or deleted should look for max of rows, cols between both tables
		for (i in ids){
			offset = 0 # in case of deleting intermediate points the correspondance may be off by this offset 
			predictions.id = subset(predictions, id==i)
			oldpredictions.id = subset(oldpredictions, id==i)
			for (r in 1:max(nrow(predictions.id),nrow(oldpredictions.id))){
				#print(paste(r, r+offset,nrow(predictions.id), nrow(oldpredictions.id)+offset,predictions.id$x[r] , oldpredictions.id$x[r+offset]))
				#print(predictions)
				#print(oldpredictions)
				if (r + offset > nrow(oldpredictions.id)){
					Behavior.Name = c(Behavior.Name, "Add Point")
					Location.Old = c(Location.Old, "")	
					Location.New = c(Location.New, paste("(ith: ", r, ", X: ", predictions.id$x[r],", Y: ", predictions.id$y[r],")",sep=""))	
					Value.Old = c(Value.Old, "")
					Value.New = c(Value.New, i)	
				} else if (r > nrow(predictions.id)){
					Behavior.Name = c(Behavior.Name, "Delete Point")
					Location.Old = c(Location.Old, paste("(ith: ", r+offset, ", X: ", oldpredictions.id$x[r+offset],", Y: ", oldpredictions.id$y[r+offset],")",sep=""))	
					Location.New = c(Location.New, "")	
					Value.Old = c(Value.Old, i)
					Value.New = c(Value.New, "")	
				} else if (r <= nrow(predictions.id) && r + offset <= nrow(oldpredictions.id) && (predictions.id$x[r] != oldpredictions.id$x[r+offset] || predictions.id$y[r] != oldpredictions.id$y[r+offset])){
					### typically just revise, but could be the case of deleting an intermediate point
					### search through old, if another point corresponds to new, then set offset, delete points skipped and continue
					match.found = FALSE
					if (r+offset+1 <= nrow(oldpredictions.id)){
						for (r2 in (r+offset+1):nrow(oldpredictions.id)){
							if (predictions.id$x[r] == oldpredictions.id$x[r2] && predictions.id$y[r] == oldpredictions.id$y[r2]){
								offset = r2 - r
								match.found = TRUE
								#delete all rows skipped
								for (r3 in (r+offset+1):r2){
									Behavior.Name = c(Behavior.Name, "Delete Point")
									Location.Old = c(Location.Old, paste("(ith: ", r3, ", X: ", oldpredictions.id$x[r3],", Y: ", oldpredictions.id$y[r3],")",sep=""))	
									Location.New = c(Location.New, "")	
									Value.Old = c(Value.Old, i)
									Value.New = c(Value.New, "")	
								}
								# new point stays the same
								if (include.same){
									Behavior.Name = c(Behavior.Name, "Keep Point")
									Location.Old = c(Location.Old, paste("(ith: ", r+offset, ", X: ", oldpredictions.id$x[r+offset],", Y: ", oldpredictions.id$y[r+offset],")",sep=""))	
									Location.New = c(Location.New, paste("(ith: ", r, ", X: ", predictions.id$x[r],", Y: ", predictions.id$y[r],")",sep=""))	
									Value.Old = c(Value.Old, i)
									Value.New = c(Value.New, i)
								}
								break
							}
						}
					}
					if (!match.found){
						Behavior.Name = c(Behavior.Name, "Revise Point")
						Location.Old = c(Location.Old, paste("(ith: ", r+offset, ", X: ", oldpredictions.id$x[r+offset],", Y: ", oldpredictions.id$y[r+offset],")",sep=""))	
						Location.New = c(Location.New, paste("(ith: ", r, ", X: ", predictions.id$x[r],", Y: ", predictions.id$y[r],")",sep=""))	
						Value.Old = c(Value.Old, i)
						Value.New = c(Value.New, i)
					}
				} else {
					## same as before, don't add anything
					if (include.same){
						Behavior.Name = c(Behavior.Name, "Keep Point")
						Location.Old = c(Location.Old, paste("(ith: ", r+offset, ", X: ", oldpredictions.id$x[r+offset],", Y: ", oldpredictions.id$y[r+offset],")",sep=""))	
						Location.New = c(Location.New, paste("(ith: ", r, ", X: ", predictions.id$x[r],", Y: ", predictions.id$y[r],")",sep=""))	
						Value.Old = c(Value.Old, i)
						Value.New = c(Value.New, i)
					}
				}
			}
		}
	} else if (!is.null(sw$predictions) && nrow(sw$predictions) > 0){
		### everything is now added.
		predictions = sw$predictions
		ids = unique(predictions$id)
		for (i in ids){
			predictions.id = subset(predictions, id==i)
			for (r in 1:nrow(predictions.id)){
				Behavior.Name = c(Behavior.Name, "Add Point")
				Location.Old = c(Location.Old, "")	
				Location.New = c(Location.New, paste("(ith: ", r, ", X: ", predictions.id$x[r],", Y: ", predictions.id$y[r],")",sep=""))	
				Value.Old = c(Value.Old, "")
				Value.New = c(Value.New, i)
			}
		}
	} else {
		Behavior.Name = row$Step.Type
		Location.Old = ""
		Location.New = ""
		Value.Old = ""
		Value.New = ""
	}
	behavior = subset(row, TRUE, Column.Names)
	behavior = behavior[rep(seq_len(nrow(behavior)), each=length(Behavior.Name)),]
	behavior$Behavior.Num = rep(1, length(Behavior.Name))
	behavior$Behavior.Time.Spent = rep(row$Time.Spent.Seconds, length(Behavior.Name))
	behavior$Behavior.Name = Behavior.Name
	behavior$Location.Old = Location.Old
	behavior$Location.New = Location.New
	behavior$Value.Old = Value.Old
	behavior$Value.New = Value.New
	return (behavior)
}
behaviors.CarGraph = function (obj, rownum, include.same = TRUE, Column.Names = c("Workgroup.Id", "Wise.Id.1", "Wise.Id.2", "Wise.Id.3", "Step.Num", "Step.Num.NoBranch", "Step.Title", "Step.Id", "Step.Work.Id", "Start.Time.Student.Clock", "Time.Spent.Seconds", "Step.Type"), ...){
	row = obj[rownum, ]
	sw = as.wiseSW(row)
	if (!is.null(sw$observations) && nrow(sw$observations) > 0){
		Behavior.Num = numeric()
		Behavior.Name = character() ## can be "Play-Stop", "Play-Pause", "Drag"
		Behavior.Time.Spent = numeric()
		Location.Old = character() # What x value did this action start from
		Location.New = character() # What x value did this action end at
		Value.Old = character() ## the starting time in msec
		Value.New = character() ## the ending time in msex
		r = 1
		behavior.num = 0
		action.started = "" # Can be "Play" or "Drag"
		start.time = 0
		start.location = 0
		start.index = 0
		last.time = 0
		last.location = 0
		last.index = 0
		while (r <= nrow(sw$observations)){
			#print(paste(r, sw$observations$Action[r], action.started))
			if (action.started == "Start"){
				if (sw$observations$Action[r] == "Stop" || sw$observations$Action[r] == "Play" || sw$observations$Action[r] == "GraphPressed"){
					Behavior.Num = c(Behavior.Num, behavior.num)
					Behavior.Name = c(Behavior.Name, "Play-Stop")
					Behavior.Time.Spent = c(Behavior.Time.Spent, (sw$observations$t[r] - start.time)/1000)
					Location.Old = c(Location.Old, paste("X:",start.location))
					Location.New = c(Location.New, paste("X:",sw$observations$x[r]))
					Value.Old = c(Value.Old, paste("T:",start.time))
					Value.New = c(Value.New, paste("T:",sw$observations$t[r]))
					action.started = ""
				} else if (sw$observations$Action[r] == "Pause"){
					Behavior.Num = c(Behavior.Num, behavior.num)
					Behavior.Name = c(Behavior.Name, "Play-Pause")
					Behavior.Time.Spent = c(Behavior.Time.Spent, (sw$observations$t[r] - start.time)/1000)
					Location.Old = c(Location.Old, paste("X:",start.location))
					Location.New = c(Location.New, paste("X:",sw$observations$x[r]))
					Value.Old = c(Value.Old, paste("T:",start.time))
					Value.New = c(Value.New, paste("T:",sw$observations$t[r]))
					action.started = ""
				}
			} else if (action.started == "GraphPressed"){
				 if (sw$observations$Action[r] == "GraphPressed" && sw$observations$Index[r] > last.index && !is.na(sw$observations$t[r])){
				 	last.time = sw$observations$t[r]
				 	last.index = sw$observations$Index[r]
				 	last.location = sw$observations$x[r]
				 } else {
				 	Behavior.Num = c(Behavior.Num, behavior.num)
				 	Behavior.Name = c(Behavior.Name, "Drag")
				 	Behavior.Time.Spent = c(Behavior.Time.Spent, (last.time - start.time)/1000)
				 	Location.Old = c(Location.Old, paste("X:",start.location))
				 	Location.New = c(Location.New, paste("X:",last.location))
				 	Value.Old = c(Value.Old, paste("T:",start.time))
				 	Value.New = c(Value.New, paste("T:",last.time))
				 	action.started = ""
				 }
			}
			# do this separately because it can occur simultaneously with ending of action
			if (action.started == ""){
				if (sw$observations$Action[r] == "GraphPressed" && !is.na(sw$observations$t[r])){
					behavior.num = behavior.num + 1
					action.started = "GraphPressed"
					start.time = sw$observations$t[r]
					start.location = sw$observations$x[r]
					start.index = sw$observations$Index[r]
					last.time = start.time
					last.location = start.location
					last.index = start.index
				} else if (sw$observations$Action[r] == "Start"){
					behavior.num = behavior.num + 1
					action.started = "Start"
					start.time = sw$observations$t[r]
					start.location = sw$observations$x[r]
					start.index = sw$observations$Index[r]
				}
			}
			r = r + 1
		}
		### Play and Drag actions may be completed by ending table of values
		if (action.started == "Start"){
			Behavior.Num = c(Behavior.Num, behavior.num)
			Behavior.Name = c(Behavior.Name, "Play-Stop")
			Behavior.Time.Spent = c(Behavior.Time.Spent, (sw$observations$t[r] - start.time)/1000)
			Location.Old = c(Location.Old, paste("X:",start.location))
			Location.New = c(Location.New, paste("X:",sw$observations$x[r]))
			Value.Old = c(Value.Old, paste("T:",start.time))
			Value.New = c(Value.New, paste("T:",sw$observations$t[r]))
		} else if (action.started == "GraphPressed"){
			Behavior.Num = c(Behavior.Num, behavior.num)
			Behavior.Name = c(Behavior.Name, "Drag")
			Behavior.Time.Spent = c(Behavior.Time.Spent, (last.time - start.time)/1000)
			Location.Old = c(Location.Old, paste("X:",start.location))
			Location.New = c(Location.New, paste("X:",last.location))
			Value.Old = c(Value.Old, paste("T:",start.time))
			Value.New = c(Value.New, paste("T:",last.time))
		}
	} else {
		Behavior.Num = 1
		Behavior.Name = "Watch"
		Behavior.Time.Spent = row$Time.Spent.Seconds
		Location.Old = ""
		Location.New = ""
		Value.Old = ""
		Value.New = ""
	}
	behavior = subset(row, TRUE, Column.Names)
	behavior = behavior[rep(seq_len(nrow(behavior)), each=length(Behavior.Name)),]
	behavior$Behavior.Num= Behavior.Num
	behavior$Behavior.Time.Spent = Behavior.Time.Spent
	behavior$Behavior.Name = Behavior.Name
	behavior$Location.Old = Location.Old
	behavior$Location.New = Location.New
	behavior$Value.Old = Value.Old
	behavior$Value.New = Value.New
	return (behavior)
} 
behaviors.generic = function (obj, rownum, include.same = TRUE, Column.Names = c("Workgroup.Id", "Wise.Id.1", "Wise.Id.2", "Wise.Id.3", "Step.Num", "Step.Num.NoBranch", "Step.Title", "Step.Id", "Step.Work.Id", "Start.Time.Student.Clock", "Time.Spent.Seconds", "Step.Type"), ...){
	row = obj[rownum, ]
	behavior = subset(row, TRUE, Column.Names)
	behavior$Behavior.Num = 1
	behavior$Behavior.Time.Spent = row$Time.Spent.Seconds
	behavior$Behavior.Name = row$Step.Type
	behavior$Location.Old = ""
	behavior$Location.New = ""
	prevrows = subset(obj,obj$Workgroup.Id==row$Workgroup.Id & obj$Step.Num==row$Step.Num & Index<row$Index)
	if (nrow(prevrows) > 0){
		value.old = tail(prevrows$Student.Work.Part.1, 1)
	} else {
		value.old = ""
	}
	behavior$Value.Old = value.old
	behavior$Value.New = row$Student.Work.Part.1
	return (behavior)
}

## XLConnect
library(XLConnect)
library(ggplot2)
library(gridExtra)
library(gtable)
library(rjson)

write.behaviors = function (obj, dir.out, filename=NULL, b = behaviors(obj), sheet.by = "Workgroup.Id", parameters.by.step = NULL, append=FALSE){
	if (is.null(filename)) filename = paste("Behaviors", paste(unique(obj$Run.Id),collapse="-"),".xlsx",sep="")
	print(paste(dir.out,filename,sep=""))
	wb = loadWorkbook(paste(dir.out,filename,sep=""), create = !append)
	setStyleAction(wb, type = XLC$"STYLE_ACTION.NONE")
	imgcol = 20
	for (wgid in unique(b$Workgroup.Id)){
		sheetname = paste("s",wgid,"s",sep="")
		createSheet(wb, name = sheetname)
		writeWorksheet(wb, data = b, sheet = sheetname)
		setColumnWidth(wb, sheet = sheetname, column = imgcol, width = 256 * 40)
		#mergeCells(wb, sheet = "behaviors", reference = paste(LETTERS[imgcol],2,":",LETTERS[imgcol],4,sep=""))
		behs.total = max(b$Behavior.Num)
		r = 1 ## current row in behaviors
		for (beh.count in 1:behs.total){
			tally = length(which(b$Behavior.Num == beh.count))
			row = subset(obj, Step.Work.Id==b[r,"Step.Work.Id"])
			step.num = row$Step.Num
			#look for step parameters
			if (!is.null(parameters.by.step)){
				for (param in parameters.by.step){
					if (!is.null(param$Step.Num) && step.num %in% param$Step.Num){
						#use this param
						break
					} else {
						param = list()
					}
				}
			}
			### is there an image?
			if (row$Step.Type == "Grapher" || row$Step.Type == "Sensor"){
				sw = as.wiseSW(row)
				predictions = sw$predictions
				if (is.list(predictions) && length(predictions) > 0) predictions = tail(predictions, 1)[[1]]
				if (!is.null(param$expected)){expected = param$expected} else {expected = data.frame(id = character(), x = numeric(), y = numeric())}
				if (!is.null(sw$xMin) && !is.null(sw$xMax) && length(sw$xMin) > 0 && length(sw$xMax) > 0 && !is.na(tail(sw$xMin,1)) && !is.na(tail(sw$xMax,1))) {
					xlim = c(tail(sw$xMin,1), tail(sw$xMax,1))
				} else {
					# if scale does not match, then auto-adjust
					if (!is.null(param$xlim) && length(param$xlim) == 2){
						xlim = param$xlim
						if (!is.null(predictions$x) && length(predictions$x) > 1 && (max(predictions$x) <= 1 || max(predictions$x) > xlim[2])) {
							xlim[2] = max(c(1,predictions$x))
						}
					} else {
						xlim = NULL
					}
				}
				if (!is.null(sw$yMin) && !is.null(sw$yMax) && length(sw$yMin) > 0 && length(sw$yMax) > 0 && !is.na(tail(sw$yMin,1)) && !is.na(tail(sw$yMax,1))) {
					ylim = c(tail(sw$yMin,1), tail(sw$yMax,1))
				}  else {
					# if scale does not match, then auto-adjust
					if (!is.null(param$ylim) && length(param$ylim) == 2){
						ylim = param$ylim
						if (!is.null(predictions$y) && length(predictions$y) > 1 && (max(predictions$y) <= 1 || max(predictions$y) > ylim[2])) {
							ylim[2] = max(c(1,predictions$y))
						}
					} else {
						ylim = NULL
					}
				}
				
				if (!is.null(param$cols)){cols = param$cols} else {cols = NULL}
				
				plot.width = 40 * 7
				plot.height = 30 * 7
				png(paste(dir.plot,"tmp-image.png", sep=""), plot.width, plot.height)
				op = par(mar=c(2,2,0,0))
				plot(row, 'predictions', expected=expected, xlim=xlim,ylim=ylim, cols=cols)
				dev.off()
				par(op)
				## are there enough rows for this behavior for a single image?
				if (tally * 15 * 4/3 >= plot.height){
					createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
				} else {
					## need to adjust the rows to fit an image, just increase top row
					hdiff = (plot.height - tally * 15 * 4/3) * 3/4
					setRowHeight(wb, sheet = sheetname, r+1, height = 15 + hdiff)
					createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,":$",LETTERS[imgcol],"$",r+tally,sep=""), overwrite=TRUE)
				}
				addImage(wb, filename = paste(dir.plot,"tmp-image.png", sep=""), name = "pic", originalSize=TRUE)
			} else if (row$Step.Type == "CarGraph"){
				t1 = as.numeric(sub("T: ","",b[r, "Value.Old"]))
				t2 = as.numeric(sub("T: ","",b[r, "Value.New"]))
				plot.width = 40 * 7
				plot.height = (t2/1000-t1/1000) * 12
				plot.dims = NULL
				if (!is.null(param$expected)){expected = param$expected} else {expected = data.frame(id = character(), x = numeric(), y = numeric())}
				if (!is.null(param$xlim)){xlim = param$xlim} else {xlim = NULL}
				if (!is.null(param$ylim)){ylim = param$ylim} else {ylim = NULL}
				if (!is.na(plot.height) && plot.height > 20){
					png(paste(dir.plot,"tmp-image.png", sep=""), plot.width, plot.height)
					op = par(mar=c(2,2,0,0))
					plot.dims = tryCatch(plot(row, 'timedObservations',xlim=xlim, xlab="", ylab="", t.range = c(t1, t2), plot.width = plot.width, plot.height = plot.height, pointsize = 8), error = function(e) return (NULL)) 
					dev.off()
					par(op)	
				}
				if (!is.null(plot.dims)){
					## are there enough rows to fit this image
					if (tally * 15 * 4/3 >= plot.dims[2]){
						createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
					} else {
						### adjust first row to ensure that this image fits
						hdiff = (plot.dims[2] - tally * 15 * 4/3) * 3/4
						#print(paste(15+hdiff, plot.dims[2]))
						setRowHeight(wb, sheet = sheetname, r+1, height = 15 + hdiff)
						createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
					}
					addImage(wb, filename = paste(dir.plot,"tmp-image.png", sep=""), name = "pic", originalSize=TRUE)
				} else {
					## just write to field
					createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
					writeNamedRegion(wb, data = paste(b[r,'Behavior.Name'],b[r,'Behavior.Time.Spent']), name = "pic", header = FALSE, rownames = NULL)
				}
			} else if (row$Step.Type == "Table"){
				tbl = as.wiseSW(row)$table
				if (!is.null(tbl) && nrow(tbl) > 0){
					## first all columns of table need to be characters, not factors
					## and get widths
					col.max.nchars = numeric()
					for (c in 1:ncol(tbl)){
						tbl[,c] = as.character(tbl[,c])
						col.max.nchars = c(col.max.nchars, max(nchar(c(tbl[,c],names(tbl)[c]))))
					}
					plot.width = sum(col.max.nchars)/6 * 72
					plot.height = (nrow(tbl)+1)*2/3 * 72
					if (!is.na(plot.width) && !is.na(plot.height) && plot.width > 0 && plot.height > 0){
						# go through each behavior and update table with a prefix:
						# [A] added cell contents, [D] delete cell contents, [R] revised ..., no prefix means keep the same
						if (TRUE){
							for (t in 1:tally){
								beh = b[r+t-1,]
								cell.r = as.numeric(sub("\\,.*","",sub("\\(Row: ","",beh$Location.New)))
								cell.c = as.numeric(sub("\\)","",sub("\\(Row: [0-9]+\\, Col: ","",beh$Location.New)))
								if (!is.null(tbl[cell.r, cell.c])){
									## use behavior
									if (beh$Behavior.Name == "Add Cell"){
										tbl[cell.r, cell.c] = paste("[A]", tbl[cell.r, cell.c])
									} else if (beh$Behavior.Name == "Delete Cell"){
										tbl[cell.r, cell.c] = paste("[D]", beh$Value.Old)
									} else if (beh$Behavior.Name == "Revise Cell"){
										tbl[cell.r, cell.c] = paste("[R]", tbl[cell.r, cell.c])
									} 
								}
							}
						}
						# print new table as image
						tblgrob = tableGrob(tbl,gpar.coretext =gpar(fontsize=8),gpar.coltext=gpar(fontsize=8), gpar.rowtext=gpar(fontsize=8),gpar.corefill = gpar(fill = "#AAAAFF", col = "white"))
						gtbl = gtable(unit(rep(1,ncol(tbl)),"cm"),unit(rep(1,nrow(tbl)),"cm"),respect=TRUE,colnames=names(tbl))
						gtbl = gtable_add_grob(gtbl, tblgrob, 1, 1, nrow(tbl), ncol(tbl),clip="off")
						png(paste(dir.plot,"tmp-image.png", sep=""),plot.width/72, plot.height/72, unit="cm",res=72)	
						suppressWarnings(plot(gtbl))
						dev.off()

						## are there enough rows to fit this image
						if (tally * 15 * 4/3 >= plot.height){
							createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
						} else {
							### adjust first row to ensure that this image fits
							hdiff = (plot.height - tally * 15 * 4/3) * 3/4
							#print(paste(15+hdiff, plot.dims[2]))
							setRowHeight(wb, sheet = sheetname, r+1, height = 15 + hdiff)
							createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
						}
						addImage(wb, filename = paste(dir.plot,"tmp-image.png", sep=""), name = "pic", originalSize=TRUE)	
					} else {
						## just write to field
						createName(wb, name = "pic", formula = paste(sheetname,"!$",LETTERS[imgcol],"$",r+1,sep=""), overwrite=TRUE)
						writeNamedRegion(wb, data = paste(b[r,'Behavior.Name'],b[r,'Behavior.Time.Spent']), name = "pic", header = FALSE, rownames = NULL)
					}
				}
			}
			r = r + tally
		}
	}
	saveWorkbook(wb)
}

### the codeToNameMap should be a list of lists. Each sublist will have a codes vector
### and a name string. e.g. list(list(codes=c(1,2),name="Good")). If in a column a code of 1 or 2 is matched a column with .Good appended will be 1, not 0
transformBehaviorCodesToBinary = function (summary, codeToNameMap, ColNames){
	for (col in 1:length(ColNames)){
		ColName = ColNames[col]
		values = summary[,ColName]
		if (is.numeric(values)){
			# remove the column
			summary = summary[,names(summary)!=ColName]
			for (l in codeToNameMap){
				summary[,paste(ColName,l$name,sep=".")] = rep(0, nrow(summary))
				summary[values %in% l$codes,paste(ColName,l$name,sep=".")] = 1
			}
		}
	}
	return (summary)
}

### Creates a sum of each error across steps, and all errors on each step
addSummaryBehaviorCodes = function (summary, ErrorNames = character(), Step.Num = numeric(), BehaviorTypes = character(), postfix = ""){
	### Get behavior types 
	BehaviorTypes.real = unique(sub(".*[0-9]+\\.[0-9]+\\.","",grep("B\\.",names(summary),value=TRUE)))
	if (length(BehaviorTypes) > 0) BehaviorTypes.real = BehaviorTypes[BehaviorTypes %in% BehaviorTypes.real]
	BehaviorTypes.grep = paste(BehaviorTypes.real, collapse="|")

	### Get sum of each error across all steps
	ErrorNames.real = unique(sub("\\.[0-9].*","",sub("B\\.","",grep("B\\.",names(summary),value=TRUE))))
	if (length(ErrorNames) > 0) ErrorNames.real = ErrorNames[ErrorNames %in% ErrorNames.real]
	for (e in ErrorNames.real){
		summary[,paste("B",e,postfix,sep=".")] = apply(subset(summary,TRUE,grep(paste(e,".*",BehaviorTypes.grep,sep=""),names(summary))), 1, sum)
	}
	### Get sum ofe all errors for each step
	Step.Num.real = unique(as.numeric(substr(names(summary),regexpr("[0-9]+\\.[0-9]+",names(summary)),regexpr("[0-9]+\\.[0-9]+",names(summary))+attr(regexpr("[0-9]+\\.[0-9]+",names(summary)),"match.length")-1)))
	Step.Num.real = Step.Num.real[!is.na(Step.Num.real)]
	if (length(Step.Num) > 0) Step.Num.real = Step.Num[Step.Num %in% Step.Num.real]
	for (s in Step.Num.real){
		summary[,paste("B",s,postfix,sep=".")] = apply(subset(summary,TRUE,grep(paste(s,".*",BehaviorTypes.grep,sep=""), names(summary))), 1, sum)
	}
	return (summary)
}