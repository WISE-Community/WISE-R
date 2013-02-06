\name{getColClasses}
\alias{getColClasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getColClasses(colNames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colNames}{
%%     ~~Describe \code{colNames} here~~
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
function (colNames) 
{
    colNames = gsub("..if.applicable.", "", colNames)
    colNames = gsub("\\.\\.", "\\.", colNames)
    colNames = gsub("\\.$", "", colNames)
    colClasses = character()
    for (colName in colNames) {
        if (colName == "Workgroup.Id" || colName == "Wise.Id.1" || 
            colName == "Wise.Id.2" || colName == "Wise.Id.3" || 
            colName == "Class.Period" || colName == "Project.Id" || 
            colName == "Parent.Project.Id" || colName == "Run.Id" || 
            colName == "Time.Spent.Seconds" || colName == "Teacher.Score" || 
            colName == "Rev.Num" || colName == "IRev.Num" || 
            colName == "Step.Num") {
            colClasses = c(colClasses, "numeric")
        }
        else {
            colClasses = c(colClasses, "character")
        }
    }
    return(colClasses)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
