replaceDuplicateIds <-
function (wiseDF, dupMap){
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
