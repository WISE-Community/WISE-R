### Within the given directory find the "data.js" files and parse JSON file (two levels down)
read.xlsx.special <- function (dir){
	library(rjson)
	files = list.files(dir);
	df.out = data.frame(Workgroup.Id = vector(), Wise.Id.1 = vector(), Wise.Id.2 = vector(), Wise.Id.3 = vector(), Step.Work.Id = vector(), Student.Work = character());	
	for (f in files){
		dirdir = paste(dir,f,"\\",sep="");
		filesfiles = list.files(dirdir);
		dirf = "";
		### does dirf contain the data.js file or do we need to go deeper?
		if (length(which(filesfiles=="data.js")) == 0){
			dirdirdir = paste(dirdir,filesfiles,"\\",sep="");
			filesfilesfiles = list.files(dirdirdir);
			if (length(which(filesfilesfiles=="data.js")) == 1) {
				dirf = paste(dirdirdir,filesfilesfiles[which(filesfilesfiles=="data.js")[1]],sep="")
			}
		} else if (length(which(filesfiles=="data.js")) == 1){
			dirf = paste(dirdir,filesfiles[which(filesfiles=="data.js")[1]],sep="")
		}
		if (dirf != ""){
			d = readLines(dirf, warn=FALSE)
			d = sub("var data = ","", d);
			d = paste(d, collapse="");
			j = fromJSON(d);
			#return(j)
			### go through each student
			for (s in j[[which(names(j)=="students")]]){
				Workgroup.Id = as.factor(s$workgroupId);				
				if (length(s$wiseIds) > 0){
					Wise.Id.1 = as.factor(s$wiseIds[1])
					if (length(s$wiseIds) > 1){
						Wise.Id.2 = as.factor(s$wiseIds[2])
						if (length(s$wiseIds) > 2){
							Wise.Id.3 = as.factor(s$wiseIds[3])
						} else {
							Wise.Id.3 = as.factor("")
						}
					} else {
						Wise.Id.2 = as.factor("")
						Wise.Id.3 = as.factor("")
					}
				} else {
					Wise.Id.1 = as.factor("")
					Wise.Id.2 = as.factor("")
					Wise.Id.3 = as.factor("")
				}
				Step.Work.Id = vector();
				### go through each visit of this step
				for (visit in s$studentDataArray){
					Step.Work.Id = as.factor(visit$stepWorkId);
					Student.Work <- ""
					
					## go to each response within this step
					if (length(visit$data$nodeStates) > 0){
						for (ri in 1:length(visit$data$nodeStates)){
							response = visit$data$nodeStates[[ri]];
							Student.Work = paste(Student.Work, "Response #",ri,": ",toJSON(response),"\n ",sep="")
							#if (Step.Work.Id == 5233870) print(Student.Work)
						}
					}
					## Finally, populate the out data frame

					df.line = data.frame(Workgroup.Id = Workgroup.Id, Wise.Id.1 = Wise.Id.1, Wise.Id.2 = Wise.Id.2, Wise.Id.3 = Wise.Id.3, Step.Work.Id = Step.Work.Id, Student.Work = Student.Work);	
					df.out = rbind(df.out, df.line);
				}				
			}
		}
	}
	df.out$Student.Work <- as.character(df.out$Student.Work)
	return (df.out);
}

### Reads special export of idea manager, with public settings.  Excel files should be single tabs in a single directory
read.xlsx.ideaBasket.public <- function (dir){
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
	names(df) = gsub("..if.applicable.", "", names(df));
	names(df) = gsub("\\.\\.", "\\.", names(df));
	names(df) = gsub("\\.$", "", names(df));

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

### Reads special export of idea manager, with private settings.  Excel files should be single tabs in a single directory
read.xlsx.ideaBasket.private <- function (dir){
	files = list.files(dir);
	df = data.frame();

	# Iterate the first time through all the files to find the largest number of columns
	#  e.g. a questionarre could have multiple "student work" columns
	for (f in files){
		if (substr(f,0,1) != "~" && grepl(".xl", f)[1]){
			dirf = paste(dir, f, sep="");
			### get header
			head = read.xlsx2(dirf, sheetIndex = 1, startRow=1, endRow=2, stringsAsFactors = FALSE, colClasses=NA);
			colClasses = getColClasses.read(names(head));	
			head = read.xlsx2(dirf, sheetIndex = 1, startRow=1, endRow=2, colClasses=colClasses);
			### get body, but just part
			body = read.xlsx2(dirf, sheetIndex = 1, startRow=4, endRow=10, stringsAsFactors = FALSE, colClasses=NA);
			colClasses = getColClasses.read(names(body));	
			body = read.xlsx2(dirf, sheetIndex = 1, startRow=4, colClasses=colClasses);
			### merge header df with body
			head = head[rep(1,each=nrow(body)),];
			sdf = cbind(head,body);
			df = rbind(df, sdf)
		}
	}
	### get final classes and update
	names(df) = gsub("..if.applicable.", "", names(df));
	names(df) = gsub("\\.\\.", "\\.", names(df));
	names(df) = gsub("\\.$", "", names(df));
	
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

