## ----load_libs, echo = FALSE, message = FALSE----------------------------
library(knitr)
library(flowr)

## ----load_flowr, message=FALSE-------------------------------------------
library(flowr)

## ----setup, eval=FALSE---------------------------------------------------
#  setup()

## ----echo=FALSE, eval=FALSE----------------------------------------------
#  #We have a quite handy command-line-interface for flowr, which exposes all functions of the package to terminal. Such that we dont have to open a interactive R session each time. To make this work, run a setup function which copies the 'flowr' helper script to your `~/bin` directory.
#  

## ----message=FALSE, echo=FALSE, fig.height=1.5, fig.width=5, eval=FALSE----
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

## ----message=FALSE, echo=FALSE-------------------------------------------
kable(flow_def)

## ----message=FALSE-------------------------------------------------------
ex = file.path(system.file(package = "flowr"), "pipelines")
flowmat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
flowdef = as.flowdef(file.path(ex, "sleep_pipe.def"))

fobj <- to_flow(x = flowmat, 
                 def = flowdef,
                 flowname = "example1", ## give it a name
                 platform = "lsf")      ## override platform mentioned in flow def

## ----plotit, fig.cap="Flow chart describing process for example 1", message=FALSE----
plot_flow(fobj)     # ?plot_flow for more information
plot_flow(flowdef) # plot_flow works on flow definition as well

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj, execute = TRUE)

## ----build_pipe_exdata, message=FALSE------------------------------------
ex = file.path(system.file(package = "flowr"), "pipelines")
flow_mat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
flow_def = as.flowdef(file.path(ex, "sleep_pipe.def"))

## ----build_pipe_ex1, eval=FALSE, echo=FALSE------------------------------
#  #flow_def = read_sheet(file.path(exdata, "example1_flow_def2.txt"))
#  fobj = suppressMessages(to_flow(flow_mat, def = flow_def, platform = "torque"))
#  fobj@jobs[[1]]@nodes
#  #debug(submit_flow)
#  fobj = submit_flow(fobj)

## ----build_pipe_exmat, echo=FALSE----------------------------------------
kable(subset(flow_mat, samplename == "sample1"))

## ----build_pipe_exdef, echo=FALSE----------------------------------------
kable(head(flow_def))

## ----plot_one_one, echo=FALSE, message=FALSE-----------------------------
qobj <- queue(platform = "lsf", queue = "normal", verbose = FALSE)
A <- job(name = "A", cmds = "sleep1", q_obj = qobj, 
				 submission_type = "scatter")
B <- job(name = "B", cmds = "sleep2", q_obj = qobj,
				 previous_job = "A", 
				 dependency_type = "serial", submission_type = "scatter")
C <- job(name = "C", cmds = "sleep2", q_obj = qobj,
				 previous_job = "B", 
				 dependency_type = "gather", submission_type = "serial")
D <- job(name = "D", cmds = "sleep2", q_obj = qobj,
				 previous_job = "C", 
				 dependency_type = "burst", submission_type = "scatter")

pab <- plot_flow(flow(jobs = list(A, B)))

## ----plot_many_one, echo=FALSE, message=FALSE, eval=FALSE----------------
#  pbc <- plot_flow(flow(jobs = list(B, C)))

## ----plot_one_many, echo=FALSE, message=FALSE, eval=FALSE----------------
#  pcd <- plot_flow(flow(jobs = list(C, D)))

## ----plot_abcd, message=FALSE--------------------------------------------
ex2def = as.flowdef(file.path(ex, "abcd.def"))
ex2mat = as.flowmat(file.path(ex, "abcd.tsv"))
kable(ex2def[, 1:4])
plot_flow(ex2def)

## ----echo=FALSE, results='asis', message=TRUE, eval=FALSE----------------
#  # Available Pipelines
#  
#  Here are some of the available pipelines along with their respective locations
#  
#  pipes = try(fetch_pipes(silent = TRUE))
#  #message(pipes)
#  if(class(pipes) != "try-error")
#  	if(nrow(pipes) > 0){
#  		pipes$pipe = basename(pipes$pipe)
#  		pipes$def = basename(pipes$def)
#  		pipes$conf = basename(pipes$conf)
#  		params::kable(pipes)
#  	}
#  

## ----echo=FALSE----------------------------------------------------------
#exdata = file.path(system.file(package = "flowr"), "extdata")
plat <- params::read_sheet("files/platforms_supported.txt", id_column = "Platform")
kable(plat)

