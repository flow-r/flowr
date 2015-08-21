## ---- echo = FALSE, message = FALSE--------------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
	fig.cap = ""
)
library(flowr)


## ----echo=FALSE----------------------------------------------------------
#exdata = file.path(system.file(package = "flowr"), "extdata")
plat <- params::read_sheet("imgs/platforms_supported.txt", id_column = "Platform")
kable(plat)

