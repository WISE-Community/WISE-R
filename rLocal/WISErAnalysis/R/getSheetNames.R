getSheetNames <-
function(fileName, maxSheets=100)
{
wb = loadWorkbook(fileName);
sheets = getSheets(wb);
return (names(sheets));
}
