### used for replacing 
### Three categories for replacing:
### 1) Same first name and last, same birth month/year
### 2) Same first name and last name, difffernt birth month/year (e.g. Jon Dough 01/25 and Jon Dough 01/01)
### 3) Same last name and birth month/year, same first initial of first name, but the rest is different(e.g. Jonathan Dough 1/25 and Jon Dough 1/25)
as.dupMap = function (df, ids.skip=numeric()){	
	df = read.csv(filename, header=TRUE);
	df = df[!duplicated(df$studentId),]
	df$firstname = tolower(df$firstname)
	df$lastname = tolower(df$lastname)
	df$firstinitial = substr(df$firstname,1,1)
	df = df[order(df$studentId,df$lastname,df$birthmonth,df$birthday,df$firstname),]
	ndf.1 = df[df$studentId%in%ids.skip|!duplicated(subset(df,TRUE,c(firstname,lastname,birthmonth,birthday))),]
	ndf.2 = df[df$studentId%in%ids.skip|!duplicated(subset(df,TRUE,c(firstname,lastname))),]
	ndf.3 = df[df$studentId%in%ids.skip|!duplicated(subset(df,TRUE,c(lastname,birthmonth,birthday,firstinitial))),]

	# since ndf.1 is most conservative, use it as a basis for the final output
	from = unique(df$studentId)
	to.1 = numeric()
	to.2 = numeric()
	to.3 = numeric()
	for (sid in from){
		first = subset(df,studentId==sid)$firstname[1]
		firsti = substr(first,1,1)
		last = subset(df,studentId==sid)$lastname[1]
		month = subset(df,studentId==sid)$birthmonth[1]
		day = subset(df,studentId==sid)$birthday[1]
		if (sid %in% ids.skip){
			to.1 = c(to.1, sid)
			to.2 = c(to.2, sid)
			to.3 = c(to.3, sid)
		} else {
			to.1 = c(to.1, subset(ndf.1,firstname==first&lastname==last&birthmonth==month&birthday==day)$studentId)
			to.2 = c(to.2, subset(ndf.2,firstname==first&lastname==last)$studentId)
			to.3 = c(to.3, subset(ndf.3,lastname==last&birthmonth==month&birthday==day&firstinitial==firsti)$studentId) 
		}
	}
	dups = data.frame(from=from, to = to.1, to.diffBirth = to.2, to.diffFirstName = to.3)
	return (dups);
}

findDuplicateDisprecpencies = function (dupMap){
	return (subset(dupMap, to!=to.diffBirth|to!=to.diffFirstName|to.diffBirth!=to.diffFirstName))
}

resolveDuplicateDiscrepencies = function(duplicates, dupMap=NULL, wiseDF=NULL, auto.resolveBirth=FALSE, start.row=1, resolve.NAs.only=FALSE){
	if (is.null(dupMap)) dupMap = as.dupMap(duplicates)
	if (is.null(dupMap$to.resolved)) dupMap$to.resolved  = dupMap$to
	discr = findDuplicateDisprecpencies(dupMap)
	if (resolve.NAs.only) discr = subset(discr,is.na(to.resolved))
	r.last = 1
	r = start.row
	val = 2
	while (r <= nrow(discr)){
		print(paste("row",r, "of",nrow(discr)))
		to.id = discr$to[r]
		# first check to make sure we haven't resolved this already, i.e. if the resolved column is different from to, then move along
		if (val == 5 || sum(is.na(subset(dupMap, to==to.id)$to.resolved)) > 0 || prod(subset(dupMap, to==to.id)$to == subset(dupMap, to==to.id)$to.resolved)){
			if (discr$to[r] != discr$to.diffBirth[r] && !auto.resolveBirth){
				print(rbind(subset(duplicates,studentId==discr$to[r])[1,],subset(duplicates,studentId==discr$to.diffBirth[r])[1,]))
				if (!is.null(wiseDF)) print(rbind(subset(wiseDF,Wise.Id.1==discr$to[r]|Wise.Id.2==discr$to[r]|Wise.Id.3==discr$to[r])[1,3:15],subset(wiseDF,Wise.Id.1==discr$to.diffBirth[r]|Wise.Id.2==discr$to.diffBirth[r]|Wise.Id.3==discr$to.diffBirth[r])[1,3:15]))
				cat("1:Same People \n2:Two different people \n0:I don't know \n9:exit \n5:I made a mistake on the last\n")
				val = scan(nmax=1)
				if (length(val) == 0){
					r = r - 1
				} else if (val == 1){
					resolveid = min(discr$to.diffBirth[r], discr$to[r])
					replaceid = max(discr$to.diffBirth[r], discr$to[r])
					dupMap[dupMap$to==replaceid|dupMap$to.diffBirth==replaceid,"to.resolved"] = resolveid
				} else if (val == 2){
					dupMap[dupMap$to==discr$to[r],"to.resolved"] = discr$to[r]
					dupMap[dupMap$to==discr$to.diffBirth[r],"to.resolved"] = discr$to.diffBirth[r]
				} else if (val == 0){
					dupMap[dupMap$to==discr$to[r]|dupMap$to.diffBirth==discr$to[r]|dupMap$to==discr$to.diffBirth[r]|dupMap$to.diffBirth==discr$to.diffBirth[r],"to.resolved"] = NA
				} else if (val == 5){
					r = r.last-1
				} else if (val == 9){
					print(paste("finished on row",r))
					return(dupMap)
				}
				r.last = r
			} else if (discr$to[r] != discr$to.diffFirstName[r]){
				print(rbind(subset(duplicates,studentId==discr$to[r])[1,],subset(duplicates,studentId==discr$to.diffFirstName[r])[1,]))
				if (!is.null(wiseDF)) print(rbind(subset(wiseDF,Wise.Id.1==discr$to[r]|Wise.Id.2==discr$to[r]|Wise.Id.3==discr$to[r])[1,3:15],subset(wiseDF,Wise.Id.1==discr$to.diffFirstName[r]|Wise.Id.2==discr$to.diffFirstName[r]|Wise.Id.3==discr$to.diffFirstName[r])[1,3:15]))
				cat("1:Same People \n2:Two different people \n0:I don't know \n9:exit \n5:I made a mistake on the last\n")
				val = scan(nmax=1)
				if (length(val) == 0){
					r = r - 1
				} else if (val == 1){
					resolveid = min(discr$to.diffFirstName[r], discr$to[r])
					replaceid = max(discr$to.diffFirstName[r], discr$to[r])
					dupMap[dupMap$to==replaceid|dupMap$to.diffFirstName==replaceid,"to.resolved"] = resolveid
				} else if (val == 2){
					dupMap[dupMap$to==discr$to[r],"to.resolved"] = discr$to[r]
					dupMap[dupMap$to==discr$to.diffFirstName[r],"to.resolved"] = discr$to.diffFirstName[r]
				} else if (val == 0){
					dupMap[dupMap$to==discr$to[r]|dupMap$to.diffFirstName==discr$to[r]|dupMap$to==discr$to.diffFirstName[r]|dupMap$to.diffFirstName==discr$to.diffFirstName[r],"to.resolved"] = NA
				} else if (val == 5){
					r = r.last-1
				} else if (val == 9){
					print(paste("finished on row",r))
					return(dupMap)
				}
				r.last = r
			}
		}
		r = r + 1
	}
	return(dupMap)
}

# Replace any Wise Id's in "from" found in Wise.Id.1, Wise.Id.2, or Wise.Id.3 to the specified "to" column
replaceDuplicateIds = function (wiseDF, dupMap, to.column="to.resolved"){
	### make sure that the appropiate columns exist
	if (sum(names(wiseDF)=="Wise.Id.1"|names(wiseDF)=="Wise.Id.2"|names(wiseDF)=="Wise.Id.3") < 1){
		print("Are you sure this is a valid wise data object? Couldn't find Id columns");
		return (wiseDF);
	}
	wiseDF$Wise.Id.1 = as.numeric(as.character(wiseDF$Wise.Id.1))
	wiseDF$Wise.Id.2 = as.numeric(as.character(wiseDF$Wise.Id.2))
	wiseDF$Wise.Id.3 = as.numeric(as.character(wiseDF$Wise.Id.3))
	# iterate through first column of duplicate match, look for duplicates and replace
	for (i in 1:nrow(dupMap)){
		d_id = dupMap[i,"from"]; 
		r_id = dupMap[i,to.column];
		wiseDF$Wise.Id.1[wiseDF$Wise.Id.1==d_id] = r_id;
		wiseDF$Wise.Id.2[wiseDF$Wise.Id.2==d_id] = r_id;
		wiseDF$Wise.Id.3[wiseDF$Wise.Id.3==d_id] = r_id;
	}
	wiseDF$Wise.Id.1 = as.factor(wiseDF$Wise.Id.1)
	wiseDF$Wise.Id.2 = as.factor(wiseDF$Wise.Id.2)
	wiseDF$Wise.Id.3 = as.factor(wiseDF$Wise.Id.3)
	return (wiseDF);
}

createStepSkipList = function (wiseDF, start.step = 1, auto.remove.regexp = "", auto.only = FALSE, step.title.rmlist = list()){
	step.title = unique(wiseDF$Step.Title)
	if (length(step.title.rmlist) > 0){
		l = unlist(step.title.rmlist)
		step.title.rm = l[which(names(l) == "Step.Title")]
		step.title = step.title[!(step.title %in% step.title.rm)]
	}
	
	s = start.step
	while (s <= length(step.title)){
		st = step.title[s]
		if (grepl(auto.remove.regexp, st)[1]){
			val = 2
		} else if (!auto.only){
			print(as.numeric(as.character(unique(subset(wiseDF,Step.Title==st)$Project.Id))))
			print(as.character(st))
			cat("1: keep step\n2: remove step\n5: I made a mistake\n9: exit\n")
			val = scan(nmax=1)
		} else {
			val = 1
		}
		
		if (length(val) == 0){

		} else if (val == 2){
			step.title.rmlist[[length(step.title.rmlist)+1]] = list(Project.Id=unique(subset(wiseDF,Step.Title==st)$Project.Id), Step.Title=st)
			s = s + 1
		} else if (val == 9){
			return (step.title.rmlist)
		} else if (val == 5){
			s = s - 1
		} else {
			s = s + 1
		}
	}
	return (step.title.rmlist)
}

removeStepsFromSkipList = function (wiseDF, skipList){
	for (l in skipList){
		wiseDF = subset(wiseDF, !(Project.Id %in% l$Project.Id & Step.Title %in% l$Step.Title))
	}
	return(wiseDF)
}
