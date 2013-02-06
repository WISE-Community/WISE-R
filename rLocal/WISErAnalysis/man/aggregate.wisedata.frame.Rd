\name{aggregate.wisedata.frame}
\alias{aggregate.wisedata.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aggregate.wisedata.frame(x, by, select.first, select.numerical, FUNS.numerical, ..., simplify = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{select.first}{
%%     ~~Describe \code{select.first} here~~
}
  \item{select.numerical}{
%%     ~~Describe \code{select.numerical} here~~
}
  \item{FUNS.numerical}{
%%     ~~Describe \code{FUNS.numerical} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{simplify}{
%%     ~~Describe \code{simplify} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, by, select.first, select.numerical, FUNS.numerical, 
    ..., simplify = TRUE) 
{
    df = x
    class(df) = "data.frame"
    first = function(fdf) {
        return(fdf[1])
    }
    by_call = substitute(by)
    b = deparse(by_call)
    b = sub("^ *list *\\( *", "", b)
    b = sub(" *\\) *$", "", b)
    by_index = which(b == names(x))
    by = eval(by_call, x, parent.frame())
    odf = data.frame(aggregate(df, by, FUN = first, simplify = TRUE)[, 
        c(1, by_index + 1)])
    if (!missing(select.first)) {
        nl = as.list(seq_along(x))
        names(nl) = names(x)
        vars = eval(substitute(select.first), nl, parent.frame())
        df.select.first = df[, c(by_index, vars)]
        odf = aggregate(df.select.first, by, FUN = first, simplify = TRUE)
    }
    if (!missing(select.numerical) && !missing(FUNS.numerical)) {
        nl = as.list(seq_along(x))
        names(nl) = names(x)
        vars = eval(substitute(select.numerical), nl, parent.frame())
        index.step = which("Step.Num" == names(x))
        index.wg = which("Workgroup.Id" == names(x))
        agg_indices = union(by_index, union(vars, union(index.step, 
            index.wg)))
        df.agg = df[, agg_indices]
        agg = aggregate(df.agg, by = list(Workgroup.Id, Step.Num), 
            FUN = "max", na.rm = TRUE, simplify = TRUE)
        print(class(agg))
        for (c in 1:ncol(agg)) {
            for (r in 1:nrow(agg)) if (is.infinite(agg[r, c])) 
                agg[r, c] = NA
        }
        print(class(agg))
        by = eval(by_call, agg, parent.frame())
        for (fun in FUNS.numerical) {
            adf = aggregate(agg, by, FUN = fun, na.rm = TRUE, 
                simplify = TRUE)
            if (length(FUNS.numerical) > 1) {
                names(adf) = paste(names(adf), ".", fun, sep = "")
            }
            if (!missing(select.first) && ncol(odf) > 2) {
                odf = cbind(odf, adf[, 2:ncol(adf)])
            }
            else {
                odf = adf
            }
        }
    }
    reps = !grepl(b, names(odf))
    odf = odf[, reps]
    class(odf) = c("aggwisedata.frame", "data.frame")
    return(odf)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
