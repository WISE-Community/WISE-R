aggregate.wisedata.frame <-
function (x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE){
df = x;
class(df) = "data.frame";  # this way when we call aggregate it won't recursivley call this.
# create a new data frame with just by factor
first =  function(fdf){return(fdf[1])}
by_call = substitute(by);
b = deparse(by_call);
b = sub("^ *list *\\( *","", b); b = sub(" *\\) *$", "", b);
by_index = which(b == names(x));
by = eval(by_call,x,parent.frame());
odf = data.frame(aggregate(df, by, FUN = first, simplify=TRUE)[,c(1,by_index+1)]);
#names(odf)[1] = deparse(substitute(b));
### seb2lect.first are those items that we just want the top value
if (!missing(select.first)){
nl = as.list(seq_along(x));
names(nl) = names(x);
vars = eval(substitute(select.first), nl, parent.frame());
df.select.first = df[,c(by_index,vars)];
odf = aggregate(df.select.first, by, FUN = first, simplify=TRUE);
}
if (!missing(select.numerical)&& !missing(FUNS.numerical)){
# we need to aggregate by step first
nl = as.list(seq_along(x));
names(nl) = names(x);
vars = eval(substitute(select.numerical), nl, parent.frame());
index.step = which("Step.Num" == names(x));
index.wg = which("Workgroup.Id" == names(x));
agg_indices = union(by_index, union(vars, union(index.step, index.wg)));
df.agg = df[,agg_indices];
agg = aggregate(df.agg, by = list(Workgroup.Id, Step.Num), FUN="max", na.rm=TRUE, simplify = TRUE);
# there has to be a better way to do this (but apply seems to chnange to a matrix)
print(class(agg))
for (c in 1:ncol(agg)) {for(r in 1:nrow(agg)) if (is.infinite(agg[r, c])) agg[r,c] = NA; }  
#agg = apply(agg, 2, function(x) {x[is.infinite(x)] <- NA; x}) 
print(class(agg))
#return(list(by_call, agg, parent.frame()))
by = eval(by_call,agg,parent.frame());
for (fun in FUNS.numerical){

#df.select.numerical = agg[,c(by_index,vars)];
adf = aggregate(agg, by, FUN = fun, na.rm = TRUE, simplify=TRUE);
## if there is more than one numerical function attach the name of the funciton
if (length(FUNS.numerical) > 1){ names(adf) = paste(names(adf),".",fun,sep="") }
if (!missing(select.first) && ncol(odf) > 2){
odf = cbind(odf, adf[,2:ncol(adf)]);
} else {
odf = adf;
}
}

}
# remove repeats of by variable
reps = !grepl(b,names(odf)); #reps[1:length(by)] = FALSE; reps[length(by)+1] = TRUE;
odf = odf[,reps];
class(odf) = c("aggwisedata.frame", "data.frame");
return(odf);
}
