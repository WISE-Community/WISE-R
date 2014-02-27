### used for replacing 
read.csv.dupMap = function (filename, ids.skip=numeric(), use.birth=TRUE){	
	df = read.csv(filename, header=TRUE);
	df$firstname = tolower(df$firstname)
	df$lastname = tolower(df$lastname)
	if(use.birth){ndf = with(df,aggregate(df, by=list(firstname,lastname,birthmonth,birthday), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))}
	else {ndf = with(df,aggregate(df, by=list(firstname,lastname), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))}
	iddf = with(df,aggregate(df, by=list(studentId), FUN=function(x){return(if (is.numeric(x)){min(x)}else{x[1]})}))
	dupIds = iddf$studentId[!(iddf$studentId %in% ids.skip) & !(iddf$studentId %in% ndf$studentId)]	
	dupFirsts = tolower(iddf$firstname[!(iddf$studentId %in% ids.skip) &!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	dupLasts = tolower(iddf$lastname[!(iddf$studentId %in% ids.skip) &!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	if (use.birth) dupBDay = tolower(iddf$birthday[!(iddf$studentId %in% ids.skip) &!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	if (use.birth) dupBMonth = tolower(iddf$birthmonth[!(iddf$studentId %in% ids.skip) &!(tolower(iddf$studentId) %in% tolower(ndf$studentId))])	
	
	replacements = numeric();
	## for each first,last combo find id in ndf
	for (i in 1:length(dupFirsts)){
		if (use.birth){newId = subset(ndf, firstname==dupFirsts[i]&lastname==dupLasts[i]&birthday==dupBDay[i]&birthmonth==dupBMonth[i])$studentId;}
		else {newId = subset(ndf, firstname==dupFirsts[i]&lastname==dupLasts[i])$studentId;}
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

### Looks for repeats in the from column and finds lowest number
reduceDupMap = function (dupMap){
	dupMap.out = dupMap[1,]
	for (r in 2:nrow(dupMap)){
		row = dupMap[r,]
		if (row$from %in% dupMap.out$from){
			ro = which(dupMap.out$from == row$from)
			dupMap.out$from[ro] = min(dupMap.out$from[ro], row$from)
		} else {
			dupMap.out = rbind(dupMap.out, row)
		}
	}
	return (dupMap.out)
}

replaceDuplicateIds = function (wiseDF, dupMap, alternativeIdName=""){
	### make sure that the appropiate columns exist
	if (sum(names(wiseDF)=="Wise.Id.1"|names(wiseDF)=="Wise.Id.2"|names(wiseDF)=="Wise.Id.3") < 1){
		print("Are you sure this is a valid wise data object? Couldn't find Id columns");
		return (wiseDF);
	}
	# iterate through first column of duplicate match, look for duplicates and replace
	for (i in 1:nrow(dupMap)){
		d_id = dupMap[i,1]; 
		r_id = dupMap[i,2];
		if("Wise.Id.1" %in% names(wiseDF)) wiseDF$Wise.Id.1[wiseDF$Wise.Id.1==d_id] = r_id;
		if("Wise.Id.2" %in% names(wiseDF)) wiseDF$Wise.Id.2[wiseDF$Wise.Id.2==d_id] = r_id;
		if("Wise.Id.3" %in% names(wiseDF)) wiseDF$Wise.Id.3[wiseDF$Wise.Id.3==d_id] = r_id;
	}
	return (wiseDF);
}
