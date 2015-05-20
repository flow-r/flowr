## ----eval=FALSE----------------------------------------------------------
#  install.packages('devtools')
#  devtools::install_github("sahilseth/flow")

## ------------------------------------------------------------------------
library(flowr)
setup()

## ------------------------------------------------------------------------
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))
flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"))

flow_mat = subset(flow_mat, samplename == "sample1")

fobj <- to_flow(x = flow_mat, def = flow_def, 
	flowname = "example1",
	platform = "lsf")


## ----plot_example1, fig.cap="Flow chart describing flow for example 1"----
plot_flow(fobj)

## ----eval=FALSE----------------------------------------------------------
#  submit_flow(fobj)
#  submit_flow(fobj, execute = TRUE)

