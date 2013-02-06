aggregate.aggwisedata.frame <-
function (x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE){
df = x; class(df) = c("wisedata.frame", "data.frame");
by = eval(substitute(by),x,parent.frame());
aggregate(df,by, select.first, select.numerical, FUNS.numerical, simplify = TRUE);
}
