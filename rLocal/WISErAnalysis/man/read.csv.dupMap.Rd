\name{read.csv.dupMap}
\alias{read.csv.dupMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.csv.dupMap(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (filename) 
{
    df = read.csv(filename, header = TRUE)
    df$firstname = tolower(df$firstname)
    df$lastname = tolower(df$lastname)
    ndf = with(df, aggregate(df, by = list(firstname, lastname), 
        FUN = function(x) {
            return(if (is.numeric(x)) {
                min(x)
            } else {
                x[1]
            })
        }))
    iddf = with(df, aggregate(df, by = list(studentId), FUN = function(x) {
        return(if (is.numeric(x)) {
            min(x)
        } else {
            x[1]
        })
    }))
    dupIds = iddf$studentId[!(tolower(iddf$studentId) \%in\% tolower(ndf$studentId))]
    dupFirsts = tolower(iddf$firstname[!(tolower(iddf$studentId) \%in\% 
        tolower(ndf$studentId))])
    dupLasts = tolower(iddf$lastname[!(tolower(iddf$studentId) \%in\% 
        tolower(ndf$studentId))])
    replacements = numeric()
    for (i in 1:length(dupFirsts)) {
        newId = subset(ndf, firstname == dupFirsts[i] & lastname == 
            dupLasts[i])$studentId
        if (length(newId) == 0) {
            print(paste(dupFirsts[i], dupLasts[i], "not found"))
        }
        else {
            if (length(newId) > 1) {
                print(paste(dupFirsts[i], dupLasts[i], "found more than once"))
            }
            newId = newId[1]
        }
        replacements = c(replacements, newId)
    }
    dups = data.frame(from = dupIds, to = replacements)
    return(dups)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
