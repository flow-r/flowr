## ----echo=FALSE, message=FALSE-------------------------------------------
library(flowr)

## ----eval=FALSE----------------------------------------------------------
#  ## for a latest official version (from CRAN)
#  install.packages("flowr", repos = "http://cran.rstudio.com")
#  
#  ## Latest stable release from DRAT (updated every other week)
#  install.packages("flowr", repos = "http://sahilseth.github.io/drat")
#  
#  ## OR cutting edge devel version
#  devtools::install_github("sahilseth/flowr", ref = "devel")
#  

## ----eval=FALSE----------------------------------------------------------
#  library(flowr)
#  setup()

## ----build_pipe_flow_def_cols, echo=FALSE, message=FALSE-----------------
#extdata = file.path(system.file(package = "flowr"), "extdata")
mat = read_sheet("files/flow_def_columns.txt")
kable(mat, col.names = c("flowdef variable", "submission template variable"))

