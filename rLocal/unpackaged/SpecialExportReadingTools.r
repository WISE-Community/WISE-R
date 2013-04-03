### Reads special export of idea manager, with public settings.  Excel files should be single tabs in a single directory
read.xlsx.ideaBasket.public = function (dir){
	files = list.files(dir);
	df = data.frame();
	#just open one file, top line even to get the names of columns
	for (f in files){
		if (substr(f,0,1) != "~" && grepl(".xl", f)[1]){
			dirf = paste(dir, f, sep="");
			head = read.xlsx2(dirf, sheetIndex = 1, startRow=1, endRow=10, stringsAsFactors = FALSE, colClasses=NA);
			break;
		}
	}
	#colClasses = apply(head,2,class)	
	colClasses = getColClasses.read(names(head));	
	
	# Iterate the first time through all the files to find the largest number of columns
	#  e.g. a questionarre could have multiple "student work" columns
	maxColumns = 0;
	for (f in files){
		if (substr(f,0,1) != "~" && grepl(".xl", f)[1]){
			dirf = paste(dir, f, sep="");
			sdf = read.xlsx2(dirf, sheetIndex = 1, colClasses=colClasses);
			df = rbind(df, sdf)
		}
	}
	### get final classes and update
	fcolClasses = getColClasses.final(df);
	for (c in 1:ncol(df)){
		if (fcolClasses[c] == "numeric"){
			df[,c] = as.numeric(df[,c]);
		} else if (fcolClasses[c] == "factor"){
			df[,c] = as.factor(df[,c]);
		} else if (fcolClasses[c] == "character"){
			df[,c] = as.character(df[,c]);
		}
	}

	return (df);
}