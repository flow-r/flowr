## ---- echo = FALSE, message = FALSE--------------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
	fig.cap = ""
)

## ----eval=FALSE----------------------------------------------------------
#  install.packages('devtools')
#  devtools::install_github("sahilseth/flowr")

## ---- message=FALSE------------------------------------------------------
library(flowr)
setup()

## ---- message=FALSE, echo=FALSE, fig.height=1.5, fig.width=5, eval=FALSE----
#  library(DiagrammeR)
#  mermaid("
#  graph LR
#  A(sleep)-->B(create_few_files)
#  B-->C{merge them}
#  C-->D[get size]
#  ")

## ----echo=FALSE, message=FALSE-------------------------------------------
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))
flow_mat = subset(flow_mat, samplename == "sample1")
flow_def = read_sheet(file.path(exdata,  "example1_flow_def.txt"))

## ----echo=FALSE----------------------------------------------------------
kable(flow_mat)

## ---- message=FALSE, echo=FALSE------------------------------------------
kable(flow_def)

## ---- eval=FALSE---------------------------------------------------------
#  ## load these files
#  exdata = file.path(system.file(package = "flowr"), "extdata")
#  flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))
#  flow_mat = subset(flow_mat, samplename == "sample1")
#  flow_def = read_sheet(file.path(exdata,  "example1_flow_def.txt"))

## ---- message=FALSE------------------------------------------------------
fobj <- to_flow(x = flow_mat, def = flow_def, 
	flowname = "example1", platform = "lsf")

## ----plot_example1, fig.cap="Flow chart describing process for example 1", fig.height=5, fig.width=5, message=FALSE----
plot_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj, execute = TRUE)

