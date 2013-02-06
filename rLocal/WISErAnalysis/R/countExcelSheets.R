countExcelSheets <-
function(fileName)
{
wb = loadWorkbook(fileName);
sheets = getSheets(wb);
return (length(sheets));
}
