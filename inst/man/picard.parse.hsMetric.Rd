\name{picard.parse.hsMetric}
\alias{picard.parse.hsMetric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picard.parse.hsMetric(outFile, names.prefix = "")
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
    as.n = as.numeric
    tmp <- try(scan(outFile, what = "character", sep = "\n"))
    names <- c("genome_size", "bait_territory", "target_territory", 
        "bait_design_efficiency", "total_reads", "pf_reads", 
        "pf_unique_reads", "pct_pf_reads", "pct_pf_uq_reads", 
        "pf_uq_reads_aligned", "pct_pf_uq_reads_aligned", "pf_uq_bases_aligned", 
        "on_bait_bases", "near_bait_bases", "off_bait_bases", 
        "on_target_bases", "pct_selected_bases", "pct_off_bait", 
        "on_bait_vs_selected", "mean_bait_coverage", "mean_target_coverage", 
        "pct_usable_bases_on_bait", "pct_usable_bases_on_target", 
        "fold_enrichment", "zero_cvg_targets_pct", "fold_80_base_penalty", 
        "pct_target_bases_2x", "pct_target_bases_10x", "pct_target_bases_20x", 
        "pct_target_bases_30x", "hs_library_size", "hs_penalty_10x", 
        "hs_penalty_20x", "hs_penalty_30x", "at_dropout", "gc_dropout", 
        "sample", "library", "read_group", "pcttargetetX25", 
        "pcttargetetX50", "pcttargetetX75", "pcttargetetX100")
    names <- paste(names.prefix, names, sep = "")
    if (class(tmp) == "try-error") {
        values <- rep("NA", length(names))
        names(values) <- names
        return(values)
    }
    len <- grep("METRICS CLASS\tnet.sf.picard.analysis", tmp) + 
        1
    cols = strsplit(tmp[len], "\t")[[1]]
    vals = round(as.n(strsplit(tmp[len + 1], "\t")[[1]][2:(length(names) - 
        3)]), 4)
    outFile2 <- gsub("hsMetrics.out", "hsMetrics.perTarget.out", 
        outFile)
    mean.cov <- try(system(sprintf("awk '{if(NR==1){for(i=1;i<=NF;i++){if($i==\"mean_coverage\"){COL=i;}}}else{print $COL;}}' \%s", 
        outFile2), intern = TRUE), silent = TRUE)
    covs <- round(sapply(c(25, 50, 75, 100), function(x) sum(as.numeric(mean.cov) > 
        x)/length(mean.cov)), 4)
    vals <- c(vals, covs)
    names(vals) <- names
    pct.cols <- grep("pct", names)
    vals[pct.cols] <- vals[pct.cols] * 100
    cbind(as.character(vals), names)
    return(vals)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
