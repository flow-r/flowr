## ----echo=FALSE, message=FALSE-------------------------------------------
library(flowr)

## ----eval=FALSE----------------------------------------------------------
#  ## for a latest stable version (updated every few days):
#  install.packages('devtools')
#  devtools::install_github("sahilseth/params")
#  devtools::install_github("sahilseth/flowr")

## ----eval=FALSE----------------------------------------------------------
#  library(flowr)
#  setup()

## ----build_pipe_flow_def_cols, echo=FALSE, message=FALSE-----------------
#extdata = file.path(system.file(package = "flowr"), "extdata")
mat = read_sheet("files/flow_def_columns.txt")
kable(mat, col.names = c("flowdef variable", "submission template variable"))

