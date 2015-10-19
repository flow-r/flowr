## ----echo=FALSE, message=FALSE-------------------------------------------
library(params)
library(flowr)

## ----eval=FALSE----------------------------------------------------------
#  ## for a latest official version (from CRAN)
#  install.packages("flowr", repos = CRAN="http://cran.rstudio.com")
#  
#  ## Latest stable release from DRAT (updated every other week); CRAN for dependencies
#  install.packages("flowr", repos = c(CRAN="http://cran.rstudio.com", DRAT="http://sahilseth.github.io/drat"))
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

## ----eval=FALSE----------------------------------------------------------
#  install.packages("whisker")
#  install.packages("diagram")
#  install.packages("flowr", repos = "http://sahilseth.github.io/drat")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("flowr", repos = c(CRAN = "http://cran.rstudio.com", DRAT = "http://sahilseth.github.io/drat"))

