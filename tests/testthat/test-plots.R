require(testthat)

if(FALSE){
	
	deffile = system.file("extdata/fastq_haplotyper.def", package = "flowr")
	
	flowdef = as.flowdef(deffile)
	plot_flow(head(flowdef, 4), pdf = TRUE)
	plot_flow(head(flowdef, 7), pdf = TRUE)
	plot_flow(flowdef, pdf = TRUE)
}