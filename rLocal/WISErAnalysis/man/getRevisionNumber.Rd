\name{getRevisionNumber}
\alias{getRevisionNumber}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getRevisionNumber(df, inverse = FALSE, incrementIntegerOnChange = FALSE, betweenChangeIncrements = 0.001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{inverse}{
%%     ~~Describe \code{inverse} here~~
}
  \item{incrementIntegerOnChange}{
%%     ~~Describe \code{incrementIntegerOnChange} here~~
}
  \item{betweenChangeIncrements}{
%%     ~~Describe \code{betweenChangeIncrements} here~~
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
function (df, inverse = FALSE, incrementIntegerOnChange = FALSE, 
    betweenChangeIncrements = 0.001) 
{
    df = cbind(Index = 1:nrow(df), df)
    Index = numeric()
    Rev.Num = numeric()
    projectIds = unique(df$Project.Id)
    for (pid in projectIds) {
        df.p = subset(df, Project.Id == pid)
        runIds = unique(df.p$Run.Id)
        for (rid in runIds) {
            df.p.r = subset(df.p, Run.Id == rid)
            wgIds = unique(df.p.r$Workgroup.Id)
            for (wgid in wgIds) {
                df.p.r.wg = subset(df.p.r, Workgroup.Id == wgid)
                stepTitles = unique(df.p.r.wg$Step.Title)
                for (st in stepTitles) {
                  df.p.r.wg.st = subset(df.p.r.wg, Step.Title == 
                    st)
                  Index = c(Index, df.p.r.wg.st$Index)
                  if (incrementIntegerOnChange) {
                    swindices = grep("Student.Work", names(df.p.r.wg.st))
                    sw = do.call(paste, c(df.p.r.wg.st[swindices], 
                      sep = ""))
                    usw = unique(sw)
                    usw = usw[!is.na(usw) & !grepl("^ *$", usw) & 
                      usw != "N/A" & !grepl("Response #[0-9]*: \\[\\]", 
                      usw)]
                    running_index = 0
                    running_value = 0
                    rev = numeric()
                    for (val in sw) {
                      index = which(val == usw)
                      if (length(index) > 0 && index != running_index) {
                        running_value = floor(running_value) + 
                          1
                        running_index = index
                        rev = c(rev, running_value)
                      }
                      else {
                        running_value = running_value + betweenChangeIncrements
                        if (floor(running_value) == 0) {
                          rev = c(rev, running_value - betweenChangeIncrements)
                        }
                        else {
                          rev = c(rev, running_value)
                        }
                      }
                    }
                  }
                  else {
                    rev = 1:length(df.p.r.wg.st$Index)
                  }
                  if (inverse) {
                    Rev.Num = c(Rev.Num, rev - max(rev))
                  }
                  else {
                    Rev.Num = c(Rev.Num, rev)
                  }
                }
            }
        }
    }
    tdf = data.frame(Index, Rev.Num)
    tdf = tdf[order(tdf$Index), ]
    return(tdf$Rev.Num)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
