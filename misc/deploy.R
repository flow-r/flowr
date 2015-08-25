library(readthedocs)
require(flowr)
require(knitr)

#pkg = "."

#undebug(staticdocs:::as.sd_package)
pkg = staticdocs::as.sd_package(pkg = ".",
                                site_path = "inst/staticdocs/source/rd",
                                templates_path = system.file("templates", package = "readthedocs"))


#debug(rtd:::to_rst)
#debug(rtd:::to_rst.character)
#build_site(pkg = "~/Dropbox/public/github_flow/", topics)
topics = topics = c(
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

tmp <- build_topics(pkg = pkg, topics = topics)
#undebug(build_vignettes)
require(tools)
tmp <- build_vignettes(pkg = pkg)
