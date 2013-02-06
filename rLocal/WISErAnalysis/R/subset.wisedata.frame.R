subset.wisedata.frame <-
function(x, subset, select, drop = FALSE, ...){
if (missing(subset)) 
       r <- TRUE
else {
   e <- substitute(subset)
   # in case of Call Wise.Id (no .1, .2, etc) perform match against all three
   if (as.character(e)[2] == "Wise.Id"){
     id = as.numeric(as.character(e)[3]);
     c = call("|", call("|", call("==",quote(Wise.Id.1),id), call("==",quote(Wise.Id.2),id)),call("==",quote(Wise.Id.3),id))
     e = substitute(c);
   }
   r <- eval(e, x, parent.frame())
   if (!is.logical(r)) 
      stop("'subset' must evaluate to logical")
   r <- r & !is.na(r)
 }
if (missing(select)) 
    vars <- TRUE
else {
    nl <- as.list(seq_along(x))
    names(nl) <- names(x)
    vars <- eval(substitute(select), nl, parent.frame())
 }
x[r, vars, drop = drop]
}
