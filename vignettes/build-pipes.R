## ---- echo = FALSE, message = FALSE--------------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
	fig.cap = ""
)
library(flowr)


## ------------------------------------------------------------------------
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))
flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"))


## ------------------------------------------------------------------------
kable(head(flow_def))

## ------------------------------------------------------------------------
kable(subset(flow_mat, samplename == "sample1"))

## ----getqobj-------------------------------------------------------------
qobj <- queue(type = "lsf", queue = "normal", verbose = FALSE)

## ----plot_simpleflow-----------------------------------------------------
job1 <- job(name = "myjob1", cmds = "sleep1", q_obj = qobj)
job2 <- job(name = "myjob2", cmds = "sleep2", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
job3 <- job(name = "myjob3", cmds = "sleep3", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
fobj <- flow(name = "myflow", jobs = list(job1, job2, job3), desc="description")
plot_flow(fobj)

## ------------------------------------------------------------------------
dat <- flowr:::.create_jobs_mat(fobj)
knitr:::kable(dat)

## ------------------------------------------------------------------------
cmds = rep("sleep 5", 10)
jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
             dependency_type = "serial", previous_job = "job1")
fobj <- flow(jobs = list(jobj1, jobj2))
plot_flow(fobj)

## ------------------------------------------------------------------------
jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
             dependency_type = "gather", previous_job = "job1")
fobj <- flow(jobs = list(jobj1, jobj2))
plot_flow(fobj)

## ------------------------------------------------------------------------
jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial", name = "job1")
jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
             dependency_type = "burst", previous_job = "job1")
fobj <- flow(jobs = list(jobj1, jobj2))
plot_flow(fobj)

## ------------------------------------------------------------------------
queue(type = "lsf")@format

## ------------------------------------------------------------------------
queue(type = "torque")@format

