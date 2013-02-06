\name{transferColValues}
\alias{transferColValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
transferColValues(targetDF, sourceDF, targetColName = "Student.Work.Part.1", sourceColName = "Student.Work.Part.1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{targetDF}{
%%     ~~Describe \code{targetDF} here~~
}
  \item{sourceDF}{
%%     ~~Describe \code{sourceDF} here~~
}
  \item{targetColName}{
%%     ~~Describe \code{targetColName} here~~
}
  \item{sourceColName}{
%%     ~~Describe \code{sourceColName} here~~
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
function (targetDF, sourceDF, targetColName = "Student.Work.Part.1", 
    sourceColName = "Student.Work.Part.1") 
{
    for (r in 1:nrow(sourceDF)) {
        srow = sourceDF[r, ]
        tindices = with(targetDF, which(Workgroup.Id == srow$Workgroup.Id[1] & 
            Wise.Id.1 == srow$Wise.Id.1[1] & Project.Id == srow$Project.Id[1] & 
            Step.Num == srow$Step.Num[1] & Start.Time == srow$Start.Time[1] & 
            End.Time == srow$End.Time[1], arr.ind = TRUE))
        if (length(tindices) == 0) {
            print("No corresponding row in target data frame")
        }
        else if (length(tindices) > 1) {
            print(paste(length(tindices), "matching rows found, using first"))
            print(targetDF[tindices, ])
            tindices = tindices[1]
        }
        if (length(tindices) == 1) {
            targetDF[tindices[1], which(names(targetDF) == targetColName)] = srow[1, 
                which(names(sourceDF) == sourceColName)]
        }
    }
    return(targetDF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
