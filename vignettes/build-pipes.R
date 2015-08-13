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
extdata = file.path(system.file(package = "flowr"), "extdata")
mat = params::read_sheet(file.path(extdata, "flow_def_columns.txt"))
kable(mat)

