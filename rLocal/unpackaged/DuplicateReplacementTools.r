### used for replacing 
read.csv.dupMap = function (filename){	
	df = read.csv(filename, header=TRUE);
	df$firstname = tolower(df$firstname)
	df$lastname = tolower(df$lastname)
	ndf = with(df,aggregate(df, by=list(firstname,lastname), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))
	iddf = with(df,aggregate(df, by=list(studentId), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))
	dupIds = iddf$studentId[!(tolower(iddf$studentId) %in% tolower(ndf$studentId))]	
	dupFirsts = tolower(iddf$firstname[!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	dupLasts = tolower(iddf$lastname[!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	replacements = numeric();
	## for each first,last combo find id in ndf
	for (i in 1:length(dupFirsts)){
		newId = subset(ndf, firstname==dupFirsts[i]&lastname==dupLasts[i])$studentId;
		if (length(newId) == 0) { print(paste(dupFirsts[i], dupLasts[i], "not found"))}
		else {
			if (length(newId) > 1) {print(paste(dupFirsts[i], dupLasts[i], "found more than once"))}
			newId = newId[1];			
		}
		
		replacements = c(replacements, newId)
	}
	dups = data.frame(from=dupIds, to=replacements)
	return (dups);
}

replaceDuplicateIds = function (wiseDF, dupMap){
	### make sure that the appropiate columns exist
	if (sum(names(wiseDF)=="Wise.Id.1"|names(wiseDF)=="Wise.Id.2"|names(wiseDF)=="Wise.Id.3") != 3){
		print("Are you sure this is a valid wise data object? Couldn't find Id columns");
		return (wiseDF);
	}
	# iterate through first column of duplicate match, look for duplicates and replace
	for (i in 1:nrow(dupMap)){
		d_id = dupMap[i,1]; 
		r_id = dupMap[i,2];
		wiseDF$Wise.Id.1[wiseDF$Wise.Id.1==d_id] = r_id;
		wiseDF$Wise.Id.2[wiseDF$Wise.Id.2==d_id] = r_id;
		wiseDF$Wise.Id.3[wiseDF$Wise.Id.3==d_id] = r_id;
	}
	return (wiseDF);
}
