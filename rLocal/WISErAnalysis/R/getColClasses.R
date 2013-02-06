getColClasses <-
function (colNames){
### clean up names a bit (remove .., . at end, "if applicable")
colNames= gsub("..if.applicable.", "", colNames);
colNames = gsub("\\.\\.", "\\.", colNames);
colNames = gsub("\\.$", "", colNames);

colClasses = character();
for (colName in colNames)
{
if (colName == "Workgroup.Id" ||
colName ==  "Wise.Id.1" ||
colName ==  "Wise.Id.2" ||
colName ==  "Wise.Id.3" ||
colName ==  "Class.Period" ||
colName ==  "Project.Id" ||
colName ==  "Parent.Project.Id" ||
colName ==  "Run.Id" ||
colName ==  "Time.Spent.Seconds" ||
colName ==  "Teacher.Score" ||
colName == "Rev.Num" ||
colName == "IRev.Num" ||
colName == "Step.Num"
){
colClasses = c(colClasses, "numeric");
} else {
colClasses = c(colClasses, "character");
}
}
return (colClasses);
}
