
install.packages("drat", repos = "http://cran.rstudio.com")
library(drat)
addRepo("sahilseth")

install.packages("ngsflows", repos = "http://cran.rstudio.com")
#library(ngsflows)


library(readthedocs)
require(flowr)
require(knitr)



if(Sys.info()['sysname'] == "Darwin"){
	outwd = "../github_flowrdocs/source/rd"
}else{
	outwd = "flowrdocs/source/rd"
}

if(!file.exists(outwd))
	dir.create(outwd, recursive = TRUE)

#undebug(staticdocs:::as.sd_package)
pkg = staticdocs::as.sd_package(pkg = ".",
                                site_path = outwd,
                                templates_path = system.file("templates", package = "readthedocs"))

## https://github.com/tripit/slate

#debug(rtd:::to_rst)
#debug(rtd:::to_rst.character)
#build_site(pkg = "~/Dropbox/public/github_flow/", topics)
topics = c(
  "setup", 
  "to_flowmat",
  "to_flowdef",
  "to_flow",
  "plot_flow",
  "submit_flow",
  "kill_flow", 
  "rerun_flow",
  "queue", 
  "git",
  "flow"
)

#tmp <- build_topics(pkg = pkg, topics = topics)
tmp <- build_topics(pkg = pkg)
#undebug(build_vignettes)
require(tools)
tmp <- build_vignettes(pkg = pkg)
