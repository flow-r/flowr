\name{picard.parse.MarkDup}
\alias{picard.parse.MarkDup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picard.parse.MarkDup(outFile, names.prefix = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outFile}{
%%     ~~Describe \code{outFile} here~~
}
  \item{names.prefix}{
%%     ~~Describe \code{names.prefix} here~~
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
function (outFile, names.prefix = "") 
{
    tmp <- try(scan(outFile, what = "character", sep = "\n"))
    names <- c("unpaired_reads_examined", "read_pairs_examined", 
        "unmapped_reads", "unpaired_read_duplicates", "read_pair_duplicates", 
        "read_pair_optical_duplicates", "percent_duplication", 
        "estimated_library_size")
    names <- paste(names.prefix, names, sep = "")
    if (class(tmp) == "try-error") {
        values <- rep("NA", length(names))
        names(values) <- names
        return(values)
    }
    len <- grep("LIBRARY\tUNPAIRED_READS_EXAMINED", tmp)
    cols = strsplit(tmp[len], "\t")[[1]]
    vals = strsplit(tmp[len + 1], "\t")[[1]][-1]
    pct.cols <- grep("percent", names)
    vals[pct.cols] <- as.numeric(vals[pct.cols]) * 100
    names(vals) <- names
    return(vals)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
