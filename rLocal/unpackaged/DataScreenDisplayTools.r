################## DataScreenDisplayTools ##############
## The purpose of this set of functions is to give researchers
## an efficient means of looking at data within the R system.
## These functions are not intended to be used for exporting to external files
## although useful functions can and will be modified in the DataOutputDisplayTools.r
## file to allow printing of useful plots/tables/etc.

library(gplots)

################## DISPLAYING TEXT ########################
## Since WISE often requires students provide a great deal of text we useful
## these functions to give researchers an opportunity to read this text efficiently

## displayStudentWorkOnPlot will filter the given data frame by student work that has a minimum length.  
#  If there are multiple parts to this step partNum will specify which to retrieve.
#	If partNum is set to 0 (default) all parts will be displayed.
#  The  text is displayed to the plot area, you must press a key to get each new plot.
displayStudentWorkOnPlot = function (df, partNum=0, minlength=1){
	colnames = grep("Student.Work", names(df), value=TRUE);
	if (partNum > 0){
		colnames = colnames[partNum];
	}
	indices = seq(1:ncol(df))[names(df) %in% colnames];
	df.good = filterDF.Cols.minlength(df, colnames, minlength=minlength);
	print(nrow(df.good));
	if (nrow(df.good)==0) return;
	oldpar = par(ask=TRUE);
	for (i in 1:nrow(df.good)){
		sw = character();
		for (j in 1:length(indices)){
			sw = c(sw, "\n");
			sw = c(sw,paste("Part ", j, ":",sep=""));
			psw = df.good[i,indices[j]];
			sw = c(sw, psw);
		}
		sw = strwrap(sw, width=dev.size(units="px")[1]/8);
		textplot(sw, halign="left", valign="top", cex=1, fixed.width=FALSE);
	}
	par(oldpar);
}

textplot.wiseSW.Note = function(obj){args = as.list(substitute(list(...))); return(textplot.wiseSW(obj, ...))}
textplot.wiseSW.Html = function(obj){args = as.list(substitute(list(...))); return(textplot.wiseSW(obj, ...))}
textplot.wiseSW = function(obj, ...){
	args = as.list(substitute(list(...)))[-1L];
	sw = character();
	for (i in 1:length(obj)){
		val = obj[[i]];
		if (!is.na(val)){
			sw = c(sw, "\n");
			sw = c(sw,paste("Part ", i, ":",sep=""));
			sw = c(sw, val);
		}
		sw = c(sw, "\n");
	}
	sw = strwrap(sw, width=dev.size(units="px")[1]/8);
	if (sum(nchar(sw))> 0){
		newargs = list(object=sw, halign="left", valign="top", cex=1, fixed.width=FALSE);
		## add ellipticated arguments
		for (a in 1:length(args)){
			newargs[[names(args)[a]]] = args[[a]];
		}
		do.call(textplot, newargs);	
	}	
}
textplot(w, col="#0000FF", bg="#CCCCCC");