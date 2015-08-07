

##
## Rscript -e 'source("test-rerun.R")'
# source("~/Dropbox/public/github_flow/inst/extdata/test-rerun.R")


## test re run
source('~/Dropbox/public/github_flow/inst/pipelines/sleep_pipe.R')

require(flowr)


test_submit <- function(){
	## --- get sleep commands
	out = sleep_pipe(x = 3, "sample1")

	## --- specify the resource requirement
	def = to_flowdef(out$flowmat,
									 platform = "lsf", queue = "short" ,
									 memory_reserved = "16384", ## LSF wants more than 16GB
									 cpu_reserved = 1, walltime = "00:59"
	)

	## change submission and dependency types
	def$sub_type = c("scatter", "scatter", "serial", "serial")
	def$dep_type = c("none", "serial", "gather", "serial")

	fobj = to_flow(out$flowmat, def, platform = "lsf", flowname = "sleep_pipe")

	plot_flow(fobj)
	fobj = submit_flow(fobj, execute = FALSE)
	fobj = submit_flow(fobj, execute = TRUE)

}


test_rerun <- function(){

	fobj = test_submit()
	Sys.sleep(60)
	require(flowr)
	#debug(rerun)
	#wd = "/rsrch2/iacs/iacs_dep/sseth/flowr/runs/sleep_pipe-sample1-20150720-00-23-45-gWmrfIjR"
	wd = fobj@flow_path
	#debug(submit_flow)
	#debug(rerun)
	out = rerun(wd, start_from = "create_tmp")
}


if(FALSE){
	require(flowr)
	run("sleep_pipe", platform = "lsf", execute = TRUE)

}
# source("~/Dropbox/public/github_flow/inst/extdata/test-rerun.R");test_rerun()




