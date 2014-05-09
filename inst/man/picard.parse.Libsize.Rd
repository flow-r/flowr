\name{picard.parse.Libsize}
\alias{picard.parse.Libsize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picard.parse.Libsize(outFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outFile}{
%%     ~~Describe \code{outFile} here~~
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
function (outFile) 
{
    tmp = try(scan(outFile, what = "character", sep = "\n"))
    if (class(tmp) == "try-error") {
        return(list(libsize = "NA"))
    }
    tab = try(as.matrix(do.call(rbind, strsplit(tmp[-(1:grep("duplication_group_count\t", 
        tmp))], "\t"))))
    if (class(tab) == "try-error") {
        return(list(mrq = "NA"))
    }
    tab = apply(tab, 2, as.numeric)
    colnames(tab) = c("count", "freq")
    return(tab)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
