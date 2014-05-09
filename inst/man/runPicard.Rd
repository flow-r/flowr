\name{runPicard}
\alias{runPicard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runPicard(bamFile, tool, params = "", outsuf, picardPath = getOption("ngs.picardPath"), outPath = getOption("ngs.sampleQAPath"), java = getOption("ngs.java"), javaMem = getOption("ngs.javaMem"), insuf = ".bam|.sam", force = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bamFile}{
%%     ~~Describe \code{bamFile} here~~
}
  \item{tool}{
%%     ~~Describe \code{tool} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{outsuf}{
%%     ~~Describe \code{outsuf} here~~
}
  \item{picardPath}{
%%     ~~Describe \code{picardPath} here~~
}
  \item{outPath}{
%%     ~~Describe \code{outPath} here~~
}
  \item{java}{
%%     ~~Describe \code{java} here~~
}
  \item{javaMem}{
%%     ~~Describe \code{javaMem} here~~
}
  \item{insuf}{
%%     ~~Describe \code{insuf} here~~
}
  \item{force}{
%%     ~~Describe \code{force} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (bamFile, tool, params = "", outsuf, picardPath = getOption("ngs.picardPath"), 
    outPath = getOption("ngs.sampleQAPath"), java = getOption("ngs.java"), 
    javaMem = getOption("ngs.javaMem"), insuf = ".bam|.sam", 
    force = FALSE, verbose = TRUE) 
{
    outFile <- file.path(outPath, gsub(insuf, sprintf(".\%s.out", 
        outsuf), basename(bamFile)))
    logFile <- file.path(outPath, gsub(insuf, sprintf(".\%s.log", 
        outsuf), basename(bamFile)))
    log <- paste(as.character(Sys.time()), "\t", bamFile, "\t", 
        "BGN", tool, "run\n")
    cat(log, file = logFile, append = TRUE)
    if (verbose) {
        cat(log)
    }
    if (!force & file.exists(outFile)) 
        return(outFile)
    cmd <- sprintf("time \%s \%s -Djava.io.tmpdir=/IACS1/tmp -jar \%s/\%s INPUT=\%s OUTPUT=\%s VALIDATION_STRINGENCY=LENIENT \%s 2>>\%s", 
        java, javaMem, picardPath, tool, bamFile, outFile, params, 
        logFile)
    if (verbose) 
        cat(cmd, "\n")
    system(cmd)
    log <- paste(as.character(Sys.time()), "\t", bamFile, "\t", 
        "END", tool, "run\n")
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
