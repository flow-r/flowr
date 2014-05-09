\name{ngs.splitBamFileName}
\alias{ngs.splitBamFileName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ngs.splitBamFileName(bamFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bamFile}{
%%     ~~Describe \code{bamFile} here~~
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
function (bamFile) 
{
    tmp <- strsplit(gsub("(.*)_([0-9]{6}.*)_s_([1-8]{1})_([TCGA]*).(.*)", 
        "\\1,\\2,\\3,\\4,\\5", basename(bamFile)), ",")[[1]]
    sample <- tmp[1]
    runid <- tmp[2]
    lane <- tmp[3]
    index <- tmp[4]
    ext <- tmp[5]
    merge.version <- "NA"
    if (length(grep("merged", bamFile)) > 0) {
        tmp <- strsplit(gsub("(.*)_([0-9]{6}.*)_(.*)_(.*)", "\\1,\\2,\\3,\\4", 
            basename(bamFile)), ",")[[1]]
        sample <- tmp[1]
        runid <- tmp[2]
        merge.version <- tmp[3]
        ext <- tmp[4]
        lane <- "NA"
        index = "NA"
    }
    return(list(runid = runid, sample = sample, lane = lane, 
        index = index, merge.version = merge.version, ext = ext))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
