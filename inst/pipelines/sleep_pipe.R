## ---- define_modules  ----
#' @param x number of sleep commands
sleep <- function(x, samplename){
	cmd = list(sleep = sprintf("sleep %s && sleep %s;echo 'hello'",
		abs(round(rnorm(x)*10, 0)),
		abs(round(rnorm(x)*10, 0))))
	flowmat = to_flowmat(cmd, samplename)
	return(list(flowmat = flowmat))
}

#' @param x number of tmp commands
create_tmp <- function(x, samplename){
	## Create 100 temporary files
	tmp = sprintf("%s_tmp_%s", samplename, 1:x)
	cmd = list(create_tmp = sprintf("head -c 100000 /dev/urandom > %s", tmp))
	## --- convert the list into a data.frame
	flowmat = to_flowmat(cmd, samplename)
	return(list(flowmat = flowmat, outfiles = tmp))
}

#' @param x vector of files to merge
merge_size <- function(x, samplename){
	## Merge them according to samples, 10 each
	mergedfile = paste0(samplename, "_merged")
	cmd_merge <- sprintf("cat %s > %s",
		paste(x, collapse = " "), ## input files
		mergedfile)
	## get the size of merged files
	cmd_size = sprintf("du -sh %s; echo 'MY shell:' $SHELL", mergedfile)

	cmd = list(merge = cmd_merge, size = cmd_size)
	## --- convert the list into a data.frame
	flowmat = to_flowmat(cmd, samplename)
	return(list(flowmat = flowmat, outfiles = mergedfile))
}

## ---- define_pipeline  ----
#' @param x number of files to make
sleep_pipe <- function(x = 3, samplename = "samp1"){

	## call the modules one by one...
	out_sleep = sleep(x, samplename)
	out_create_tmp = create_tmp(x, samplename)
	out_merge_size = merge_size(out_create_tmp$outfiles, samplename)

	## row bind all the commands
	flowmat = rbind(out_sleep$flowmat,
		out_create_tmp$flowmat,
		out_merge_size$flowmat)

	return(list(flowmat = flowmat, outfiles = out_merge_size$outfiles))
}


## ---- run_sleep_example ----
main <- function(){
	#setwd("inst/examples"); ## cd to folder containing this script
	
	require(flowr)

	out = sleep_pipe(x = 3, "sample1")
	flowmat = out$flowmat
	def = to_flowdef(out$flowmat)
	
	## change submission and dependency types
	def$sub_type = c("scatter", "scatter", "serial", "serial")
	def$dep_type = c("none", "serial", "gather", "serial")

	## save a exmple plot
	plot_flow(to_flow(flowmat, def), pdffile = "sleep_pipe.pdf", pdf = TRUE)

	## save the data
	write_sheet(def, "sleep_pipe.def")

	## write a example output
	write_sheet(flowmat, "sleep_pipe.tsv")

}





