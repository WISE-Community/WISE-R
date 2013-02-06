transferColValues <-
function (targetDF, sourceDF, targetColName="Student.Work.Part.1", sourceColName="Student.Work.Part.1"){
for (r in 1:nrow(sourceDF)){
srow = sourceDF[r,];
## filter down to a single row of the targetDF (hopefully)
tindices = with(targetDF, which(Workgroup.Id==srow$Workgroup.Id[1]&Wise.Id.1==srow$Wise.Id.1[1]&Project.Id==srow$Project.Id[1]&Step.Num==srow$Step.Num[1]&Start.Time==srow$Start.Time[1]&End.Time==srow$End.Time[1], arr.ind=TRUE));
if (length(tindices) == 0){
print("No corresponding row in target data frame");
} else if (length(tindices)  > 1){
print(paste(length(tindices), "matching rows found, using first"));
print(targetDF[tindices,]);
tindices = tindices[1];
}
if (length(tindices) == 1){
targetDF[tindices[1],which(names(targetDF)==targetColName)] = srow[1,which(names(sourceDF)==sourceColName)];
}
}
return (targetDF);
}
