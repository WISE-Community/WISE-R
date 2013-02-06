getRevisionNumber <-
function(df, inverse=FALSE, incrementIntegerOnChange=FALSE, betweenChangeIncrements=0.001){
# place the index in the first column of the df so we can find them again
df = cbind(Index=1:nrow(df), df);
Index = numeric();
Rev.Num = numeric();
## filter down to each step for each student in each run of each project
projectIds = unique(df$Project.Id);
for (pid in projectIds){
df.p = subset(df, Project.Id==pid);
runIds = unique(df.p$Run.Id);
for (rid in runIds){
df.p.r = subset(df.p, Run.Id==rid);
wgIds = unique(df.p.r$Workgroup.Id);
for (wgid in wgIds){
df.p.r.wg = subset(df.p.r, Workgroup.Id==wgid);
stepTitles = unique(df.p.r.wg$Step.Title);
for (st in stepTitles){
df.p.r.wg.st = subset(df.p.r.wg,Step.Title==st);
# after all that we finally have a single student's set of responses to a question
Index = c(Index, df.p.r.wg.st$Index);
if (incrementIntegerOnChange){
## between changes increments used when value of student work has not changed 
swindices = grep("Student.Work",names(df.p.r.wg.st));
sw = do.call(paste, c(df.p.r.wg.st[swindices],sep=""))
usw = unique(sw);
## remove non-answers from unique answers
usw = usw[!is.na(usw)&!grepl("^ *$",usw)&usw!="N/A"&!grepl("Response #[0-9]*: \\[\\]",usw)]
running_index = 0; running_value = 0;
rev = numeric();
for (val in sw){
index = which(val == usw);
if (length(index) > 0 && index != running_index){
running_value = floor(running_value) + 1;
running_index = index;
rev = c(rev, running_value);
} else {
running_value = running_value + betweenChangeIncrements;
if (floor(running_value) == 0) {
rev = c(rev, running_value-betweenChangeIncrements);
} else {
rev = c(rev, running_value);
}
}
}
} else {
rev = 1:length(df.p.r.wg.st$Index);
}

if (inverse){
Rev.Num = c(Rev.Num, rev - max(rev));
} else {
Rev.Num = c(Rev.Num, rev);
}

}
}
}
}
## combine Index and Rev.Num into a data.frame, order by index and return Rev.Num
tdf = data.frame(Index, Rev.Num);
tdf = tdf[order(tdf$Index),];
return (tdf$Rev.Num)
}
