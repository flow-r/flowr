\name{samtools.parseFlagstat}
\alias{samtools.parseFlagstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
samtools.parseFlagstat(flagstatFile, genomeLen = as.numeric(getOption("ngs.genomeLen")), seqLen = as.numeric(getOption("ngs.seqLen")), names.prefix = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagstatFile}{
%%     ~~Describe \code{flagstatFile} here~~
}
  \item{genomeLen}{
%%     ~~Describe \code{genomeLen} here~~
}
  \item{seqLen}{
%%     ~~Describe \code{seqLen} here~~
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
function (flagstatFile, genomeLen = as.numeric(getOption("ngs.genomeLen")), 
    seqLen = as.numeric(getOption("ngs.seqLen")), names.prefix = "") 
{
    flagstat <- try(readLines(flagstatFile))
    as.n = as.numeric
    names <- c("cov", "totalreads", "mapped", "paired", "read1", 
        "read2", "properlypaired", "bothmapped", "singletons", 
        "matetodifferentchr")
    names <- paste(names.prefix, names, sep = "")
    if (class(flagstat) == "try-error") {
        values <- rep("NA", length(names))
        names(values) <- names
        return(values)
    }
    totalReads <- as.n(strsplit(flagstat[grep("total", flagstat)[1]], 
        " ")[[1]][1])
    cov <- round((totalReads * seqLen)/genomeLen, 2)
    mapped <- round(as.n(strsplit(flagstat[grep("mapped", flagstat)[1]], 
        "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 100, 
        2)
    paired <- round(as.n(strsplit(flagstat[grep("paired", flagstat)[1]], 
        "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 100, 
        2)
    read1 <- round(as.n(strsplit(flagstat[grep("read1", flagstat)[1]], 
        "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 100, 
        2)
    read2 <- round(as.n(strsplit(flagstat[grep("read2", flagstat)[1]], 
        "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 100, 
        2)
    pp <- round(as.n(strsplit(flagstat[grep("properly", flagstat)[1]], 
        "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 100, 
        2)
    bothmapped <- round(as.n(strsplit(flagstat[grep("itself", 
        flagstat)[1]], "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 
        100, 2)
    self <- round(as.numeric(strsplit(flagstat[grep("itself", 
        flagstat)[1]], " ")[[1]][1])/as.numeric(totalReads) * 
        100, 2)
    singletons <- round(as.n(strsplit(flagstat[grep("singletons", 
        flagstat)[1]], "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 
        100, 2)
    matetodifferentchr <- round(as.n(strsplit(flagstat[grep("with mate mapped to a different chr", 
        flagstat)[1]], "\\+| ", perl = TRUE)[[1]][1])/as.n(totalReads) * 
        100, 2)
    out <- c(cov = cov, totalreads = totalReads, mapped = mapped, 
        paired = paired, read1 = read1, read2 = read2, pp = pp, 
        bothmapped = bothmapped, singletons = singletons, matetodifferentchr = matetodifferentchr)
    names(out) <- names
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
