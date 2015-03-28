## ----, echo = FALSE, message = FALSE-------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE
)
library(flowr)

## ----getqobj-------------------------------------------------------------
qobj <- queue(type = "lsf", queue = "normal")

## ----plot_simpleflow-----------------------------------------------------
job1 <- job(name = "myjob1", cmds = "sleep1", q_obj = qobj)
job2 <- job(name = "myjob2", cmds = "sleep2", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
job3 <- job(name = "myjob3", cmds = "sleep3", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
fobj <- flow(name = "myflow", jobs = list(job1, job2, job3), desc="description")
#source('/DRIVES/Dropbox2/Dropbox/public/github_flow/R/plot-funcs.R')
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

