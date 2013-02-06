countExcelSheets2 <-
function(fileName, maxSheets=200){
count = 0;
for (i in 1:maxSheets){
completed = TRUE;
tryCatch({df=read.xlsx2(fileName,i,startRow=1,endRow=1); completed=FALSE}, 
error = function(err){}, 
finally={if (!completed) {count = count + 1;}}
)
}
return (count);
}
