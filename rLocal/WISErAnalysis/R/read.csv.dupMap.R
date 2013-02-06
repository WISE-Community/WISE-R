read.csv.dupMap <-
function (filename){
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
