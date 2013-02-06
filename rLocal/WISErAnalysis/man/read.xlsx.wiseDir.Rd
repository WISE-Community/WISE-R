\name{read.xlsx.wiseDir}
\alias{read.xlsx.wiseDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.xlsx.wiseDir(dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
%%     ~~Describe \code{dir} here~~
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
function (dir) 
{
    files = list.files(dir)
    df = data.frame()
    maxColumns = 0
    sheetCounts = numeric()
    for (f in files) {
        if (substr(f, 0, 1) != "~") {
            dirf = paste(dir, f, sep = "")
            sheetCount = countExcelSheets(dirf)
            sheetCounts = c(sheetCounts, sheetCount)
            for (s in 1:sheetCount) {
                head = read.xlsx2(dirf, sheetIndex = s, startRow = 1, 
                  endRow = 1, stringsAsFactors = FALSE)
                pbody = read.xlsx2(dirf, sheetIndex = s, startRow = 4, 
                  endRow = 4, stringsAsFactors = FALSE)
                firstline = cbind(head, pbody)
                if (names(head)[1] == "Workgroup.Id") {
                  if (ncol(firstline) > maxColumns) {
                    maxColumns = ncol(firstline)
                    maxFile = f
                    maxSheetIndex = s
                  }
                }
            }
        }
    }
    if (length(which(names(df) == "Student.Work")) == 1) {
        names(df)[which(names(df) == "Student.Work")] = "Student.Work.Part.1"
    }
    if (maxColumns > 0) {
        dirf = paste(dir, maxFile, sep = "")
        head = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow = 1, 
            endRow = 1, stringsAsFactors = FALSE)
        colClassesHead = getColClasses(names(head))
        head = read.xlsx2(dirf, sheetIndex = maxSheetIndex, startRow = 1, 
            endRow = 1, colClasses = colClassesHead, stringsAsFactors = FALSE)
        pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, 
            startRow = 4, endRow = 4, stringsAsFactors = FALSE)
        colClassesBody = getColClasses(names(pbody))
        pbody = read.xlsx2(dirf, sheetIndex = maxSheetIndex, 
            startRow = 4, endRow = 4, colClasses = colClassesBody, 
            stringsAsFactors = FALSE)
        df = cbind(head, pbody)
    }
    else {
        return(df)
    }
    findex = 0
    for (f in files) {
        findex = findex + 1
        if (substr(f, 0, 1) != "~") {
            dirf = paste(dir, f, sep = "")
            sheetCount = sheetCounts[findex]
            for (s in 1:sheetCount) {
                head = read.xlsx2(dirf, sheetIndex = s, startRow = 1, 
                  endRow = 2, colClasses = colClassesHead, stringsAsFactors = FALSE)
                if (names(head)[1] == "Workgroup.Id") {
                  body = read.xlsx2(dirf, sheetIndex = s, startRow = 4, 
                    colClasses = colClassesBody, stringsAsFactors = FALSE)
                  full = cbind(head[rep(seq_len(nrow(head)), 
                    each = nrow(body)), ], body)
                  if (ncol(full) < ncol(df)) {
                    for (c in (ncol(full) + 1):ncol(df)) {
                      full = cbind(full, dum = rep("", nrow(full)))
                      names(full)[c] = names(df)[c]
                    }
                  }
                  if (length(which(names(full) == "Student.Work")) == 
                    1) 
                    `?`({
                      names(full)[which(names(full) == "Student.Work")] = "Student.Work.Part.1"
                    })
                  if (ncol(df) != 0 && ncol(full) != ncol(df)) {
                    print(names(full))
                    print(names(df))
                    print(f)
                    print(s)
                  }
                  if (length(names(df)) != length(intersect(names(df), 
                    names(full)))) {
                    print(names(full))
                    print(names(df))
                    print(f)
                    print(s)
                  }
                  df = rbind(df, full)
                }
            }
        }
    }
    df = cbind(Index = 1:nrow(df), df)
    df = cbind(df, Step.Num = getStepNum(df))
    df = cbind(df, Rev.Num = getRevisionNumber(df, FALSE))
    df = cbind(df, IRev.Num = getRevisionNumber(df, TRUE))
    df = cbind(df, URev.Num = getRevisionNumber(df, FALSE, TRUE))
    df = cbind(df, Research.Score = rep(NA, nrow(df)))
    df = cbind(df, Research.Notes = rep("", nrow(df)))
    names(df) = gsub("..if.applicable.", "", names(df))
    names(df) = gsub("\\.\\.", "\\.", names(df))
    names(df) = gsub("\\.$", "", names(df))
    class(df) = c("wisedata.frame", class(df))
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
