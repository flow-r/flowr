## ----libs_tut, echo = FALSE, message = FALSE-----------------------------
library(knitr)
library(flowr)

## ----example1, cache = FALSE, echo=FALSE---------------------------------
read_chunk(system.file('pipelines', 'sleep_pipe.R', package = 'flowr'))

## ----eval=FALSE----------------------------------------------------------
#  ## Single step submission:
#  fobj = run("sleep_pipe", execute = TRUE);
#  
#  ## Details of the above step:
#  setwd("~/flowr/pipelines")
#  ## behind the scenes, run does the following:
#  ## optionally, load default parameters
#  load_opts("sleep_pipe.conf")
#  
#  ## get sleep_pipe() function
#  source("sleep_pipe.R")
#  
#  ## create a flowmat
#  flowmat = sleep_pipe()
#  
#  ## read a flow definition.
#  flowdef = as.flowdef("sleep_pipe.def")
#  
#  ## create flow and submit to cluster
#  fobj = to_flow(flowmat, flowdef, execute = TRUE)

## ----define_modules, echo=FALSE------------------------------------------
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

## ----define_pipeline-----------------------------------------------------
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

## ------------------------------------------------------------------------
## create a flow matrix
out = sleep_pipe(x = 3, "sample1")
flowmat = out$flowmat

## ---- echo=FALSE---------------------------------------------------------
kable(flowmat)

## ----plot_skeleton_def, message=FALSE------------------------------------
def = to_flowdef(flowmat) ## create a skeleton flow definition
suppressMessages(plot_flow(def))

## ----message=FALSE-------------------------------------------------------
def$sub_type = c("scatter", "scatter", "serial", "serial")
def$dep_type = c("none", "serial", "gather", "serial")
kable(def)

## ----plot_tweaked_def, message=FALSE, echo = FALSE-----------------------
suppressMessages(plot_flow(def))

## ---- message=FALSE------------------------------------------------------
fobj = to_flow(flowmat, def, flowname = "sleep_pipe")

## ----eval=FALSE----------------------------------------------------------
#  plot_flow(fobj)
#  submit_flow(fobj) ## dry run
#  fobj2 = submit_flow(fobj, execute = TRUE) ## submission to LSF cluster
#  
#  ## after submission, we can use the following:
#  status(fobj2) ## check status
#  rerun(fobj2)  ## re-run from a intermediate step
#  kill(fobj2)   ## kill it!

