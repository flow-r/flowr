\name{runSamFlagStat}
\alias{runSamFlagStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runSamFlagStat(bamFile, samtools = file.path(getOption("ngs.samtoolPath"), "samtools"), outsuf = "flagstat", outPath = getOption("ngs.sampleQAPath"), insuf = ".bam|.sam", verbose = TRUE, force = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bamFile}{
%%     ~~Describe \code{bamFile} here~~
}
  \item{samtools}{
%%     ~~Describe \code{samtools} here~~
}
  \item{outsuf}{
%%     ~~Describe \code{outsuf} here~~
}
  \item{outPath}{
%%     ~~Describe \code{outPath} here~~
}
  \item{insuf}{
%%     ~~Describe \code{insuf} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{force}{
%%     ~~Describe \code{force} here~~
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
function (bamFile, samtools = file.path(getOption("ngs.samtoolPath"), 
    "samtools"), outsuf = "flagstat", outPath = getOption("ngs.sampleQAPath"), 
    insuf = ".bam|.sam", verbose = TRUE, force = FALSE) 
{
    dir.create(outPath, recursive = TRUE)
    outFile <- file.path(outPath, gsub(insuf, sprintf(".\%s", 
        outsuf), basename(bamFile)))
    logFile <- file.path(outPath, gsub(insuf, sprintf(".\%s.log", 
        outsuf), basename(bamFile)))
    log <- paste(as.character(Sys.time()), "\t", bamFile, "\t", 
        "BGN FLAGSTAT run\n")
    if (!force & file.exists(outFile)) 
        return(outFile)
    cat(log, file = logFile, append = TRUE)
    if (verbose) {
        cat(log)
    }
    cmd <- paste("time ", samtools, " flagstat ", bamFile, " > ", 
        outFile, sep = "")
    if (verbose) 
        cat(cmd, "\n")
    try(system(cmd))
    log <- paste(as.character(Sys.time()), "\t", bamFile, "\t", 
        "END FLAGSTAT run\n")
    cat(log, file = logFile, append = TRUE)
    if (verbose) {
        cat(log)
    }
    return(outFile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
