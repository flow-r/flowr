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

## ------------------------------------------------------------------------
library(flowr)
setup()

## ------------------------------------------------------------------------
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"), id_column = "samplename")
## this has a bunch of samples, so let us subset one of them
flow_mat = subset(flow_mat, samplename == "sample1")
flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"), id_column = "jobname")

## ------------------------------------------------------------------------
kable(subset(flow_mat, samplename == 'sample1'))

## ------------------------------------------------------------------------
kable(flow_def)

## ------------------------------------------------------------------------
fobj <- to_flow(x = flow_mat, def = flow_def, 
	flowname = "example1", platform = "lsf")

## ----plot_example1, fig.cap="Flow chart describing process for example 1", fig.height=10, fig.width=8----
plot_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj, execute = TRUE)

