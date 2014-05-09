\name{picard.parse.InsSize}
\alias{picard.parse.InsSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picard.parse.InsSize(outFile)
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
    tmp <- try(scan(outFile, what = "character", sep = "\n"))
    if (class(tmp) == "try-error") {
        return(list(m = "NA", sd = "NA", sd2 = "NA"))
    }
    tab = as.matrix(do.call(rbind, strsplit(tmp[-(1:grep("insert_size", 
        tmp))], "\t")))
    tab = apply(tab, 2, as.numeric)
    tab2 <- tab[, 1:2]
    colnames(tab2) = c("iSize", "freq")
    m = tab[which(sum(tab2[, "freq"]) * 0.5 <= cumsum(tab2[, 
        "freq"]))[1], 1]
    mu <- fastmean(data.frame(freq = tab2[, "freq"], value = tab2[, 
        "iSize"]))
    s <- round(fastRMSE(data.frame(freq = tab2[, "freq"], value = tab2[, 
        "iSize"])), 2)
    n <- which(tab2[, "iSize"] <= 500)
    s1 <- round(fastRMSE(data.frame(freq = tab2[n, "freq"], value = tab2[n, 
        "iSize"])), 2)
    if (ncol(tab) > 2) {
        m <- paste(m, "E", sep = "")
    }
    return(list(m = m, sd = s, sd2 = s1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
