require(testthat)

flowdef = as.flowdef("~/Dropbox/public/github_ngsflows/inst/pipelines/fastq_haplotyper.def")
plot_flow(head(flowdef, 4))
plot_flow(head(flowdef, 7))
plot_flow(flowdef)
