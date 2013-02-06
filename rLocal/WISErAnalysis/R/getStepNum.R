getStepNum <-
function (df){
stepTitles = as.character(df$Step.Title);
matchNum = regexpr("[0-9]+\\.?[0-9]*", stepTitles);
# if any non-matches replace stepTitles with "0" and do again
stepTitles[matchNum==-1] = "0"
matchNum = regexpr("[0-9]+\\.?[0-9]*", stepTitles);
stepNums = as.numeric(regmatches(stepTitles,matchNum));
return (stepNums);
}
