## ----libs, echo = FALSE, message = FALSE---------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
	fig.cap = "",
  dev = 'png'
)
library(flowr)


## ----eval=FALSE----------------------------------------------------------
#  install.packages('devtools')
#  devtools::install_github("sahilseth/flowr")

## ---- message=FALSE------------------------------------------------------
library(flowr)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  setup()

## ---- message=FALSE, echo=FALSE, fig.height=1.5, fig.width=5, eval=FALSE----
#  library(DiagrammeR)
#  mermaid("
#  graph LR
#  A(sleep)-->B(create_few_files)
#  B-->C{merge them}
#  C-->D[get size]
#  ")

## ----echo=FALSE, message=FALSE-------------------------------------------
ex = file.path(system.file(package = "flowr"), "pipelines")
flow_mat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
flow_def = as.flowdef(file.path(ex, "sleep_pipe.def"))

## ----echo=FALSE----------------------------------------------------------
kable(flow_mat)

## ---- message=FALSE, echo=FALSE------------------------------------------
kable(flow_def)

## ---- message=FALSE------------------------------------------------------
fobj <- to_flow(x = flow_mat, def = as.flowdef(flow_def), 
	flowname = "example1", platform = "lsf")

## ----plot_example1, fig.cap="Flow chart describing process for example 1", fig.height=5, fig.width=5, message=FALSE----
plot_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj, execute = TRUE)

## ----build_pipe_exdata, message=FALSE------------------------------------
## ------ load some example data
ex = file.path(system.file(package = "flowr"), "pipelines")
flow_mat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
flow_def = as.flowdef(file.path(ex, "sleep_pipe.def"))

## ----build_pipe_ex1, eval=FALSE, echo=FALSE------------------------------
#  #flow_def = read_sheet(file.path(exdata, "example1_flow_def2.txt"))
#  fobj = suppressMessages(to_flow(flow_mat, def = flow_def, platform = "torque"))
#  fobj@jobs[[1]]@nodes
#  #debug(submit_flow)
#  fobj = submit_flow(fobj)

## ----build_pipe_exdef, echo=FALSE----------------------------------------
kable(head(flow_def))

## ----build_pipe_exmat, echo=FALSE----------------------------------------
kable(subset(flow_mat, samplename == "sample1"))

## ----getqobj, eval=FALSE-------------------------------------------------
#  qobj <- queue(platform = "lsf", queue = "normal", verbose = FALSE)

## ----plot_simpleflow, eval=FALSE-----------------------------------------
#  job1 <- job(name = "myjob1", cmds = "sleep1", q_obj = qobj)
#  job2 <- job(name = "myjob2", cmds = "sleep2", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
#  job3 <- job(name = "myjob3", cmds = "sleep3", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
#  fobj <- flow(name = "myflow", jobs = list(job1, job2, job3), desc="description")
#  plot_flow(fobj)

## ---- eval=FALSE---------------------------------------------------------
#  dat <- flowr:::create_jobs_mat(fobj)
#  knitr:::kable(dat)

## ----build_pipe_plt_ab, echo=FALSE, message=FALSE, eval=FALSE------------
#  qobj <- queue(platform = "lsf", queue = "normal", verbose = FALSE)
#  A <- job(name = "A", cmds = "sleep1", q_obj = qobj,
#  				 submission_type = "scatter")
#  B <- job(name = "B", cmds = "sleep2", q_obj = qobj,
#  				 previous_job = "A",
#  				 dependency_type = "serial", submission_type = "scatter")
#  C <- job(name = "C", cmds = "sleep2", q_obj = qobj,
#  				 previous_job = "B",
#  				 dependency_type = "gather", submission_type = "serial")
#  D <- job(name = "D", cmds = "sleep2", q_obj = qobj,
#  				 previous_job = "C",
#  				 dependency_type = "burst", submission_type = "scatter")
#  
#  pab <- plot_flow(flow(jobs = list(A, B)))

## ----build_pipe_plt_bc, echo=FALSE, message=FALSE, eval=FALSE------------
#  pbc <- plot_flow(flow(jobs = list(B, C)))

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
#  jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter",
#               dependency_type = "gather", previous_job = "job1")
#  fobj <- flow(jobs = list(jobj1, jobj2))
#  plot_flow(fobj)

## ----build_pipe_plt_cd, echo=FALSE, message=FALSE, eval=FALSE------------
#  pcd <- plot_flow(flow(jobs = list(C, D)))

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial", name = "job1")
#  jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter",
#               dependency_type = "burst", previous_job = "job1")
#  fobj <- flow(jobs = list(jobj1, jobj2))
#  plot_flow(fobj)

## ----build_pipe_plt_abcd, message=FALSE----------------------------------
ex2def = as.flowdef(file.path(ex, "abcd.def"))
ex2mat = as.flowmat(file.path(ex, "abcd.tsv"))
fobj = suppressMessages(to_flow(x = ex2mat, def = ex2def))
kable(ex2def[, 1:4])
plot_flow(fobj)

## ----build_pipe_flow_def_cols, echo=FALSE, message=FALSE-----------------
#extdata = file.path(system.file(package = "flowr"), "extdata")
mat = params::read_sheet("imgs/flow_def_columns.txt")
kable(mat)

## ----echo=FALSE, results='asis'------------------------------------------
fetch_pipes()

## ----echo=FALSE----------------------------------------------------------
#exdata = file.path(system.file(package = "flowr"), "extdata")
plat <- params::read_sheet("imgs/platforms_supported.txt", id_column = "Platform")
kable(plat)

## ----example1, cache = FALSE---------------------------------------------
read_chunk(system.file('pipelines', 'sleep_pipe.R', package = 'flowr'))

## ----define_modules------------------------------------------------------
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
out = sleep_pipe(x = 3, "sample1")
flowmat = out$flowmat

kable(flowmat)

## ------------------------------------------------------------------------
def = to_flowdef(flowmat)
kable(def)

## ----message=FALSE-------------------------------------------------------
plot_flow(to_flow(flowmat, def))

## ----message=FALSE-------------------------------------------------------
def$sub_type = c("scatter", "scatter", "serial", "serial")
def$dep_type = c("none", "serial", "gather", "serial")
kable(def)

## ----message=FALSE-------------------------------------------------------
plot_flow(to_flow(flowmat, def))

