## ---- echo = FALSE, message = FALSE--------------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  dev = 'png',
	fig.cap = ""
)

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

