
options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages("drat")
library(drat)
repo = addRepo("sahilseth")

install.packages("ngsflows")
#devtools::install_github("sahilseth/ngsflows")

library(ngsflows)

library(staticdocs)
library(packagedocs)
require(flowr)
require(knitr)



if(Sys.info()['sysname'] == "Darwin"){
	outwd = "~/Dropbox/public/github_flowrpages/flowr"
	code_path <- "~/Dropbox/public/github_flow"
}else{
	outwd = "gh-pages/flowr"
	code_path <- "../../"
}

if(!file.exists(outwd))
	dir.create(outwd, recursive = TRUE)

## ---------------------------

## load packages (and install if not on system)
# if(!require("staticdocs"))
#   devtools::install_github("hadley/staticdocs")
# if(!require("packagedocs"))
#   devtools::install_github("hafen/packagedocs")
# if(!require("rmarkdown"))
#   install.packages("rmarkdown")
# if(!require("flowr"))
#   devtools::install_github("sahilseth/flowr")

library(staticdocs)
library(packagedocs)
library(flowr)
library(rmarkdown)

# make sure your working directory is set to repo base directory
setwd(outwd)
#setwd("~/Dropbox/public/github_flowrdocs2/flowr")

## this make file uses several files, from flowr folder
# ├── README.Rmd
# ├── NEWS.md
# ├── man/*Rd
# └── vignettes/build-pipes.Rmd

# set some options
pd <- package_docs(lib_dir = "assets", toc = FALSE)
pd_collapsed <- package_docs(lib_dir = "assets", toc_collapse = TRUE)
pd_expand <- package_docs(lib_dir = "assets", toc_collapse = FALSE)
knitr::opts_knit$set(root.dir = normalizePath("."))

# generate index.html, get new from template !
unlink("assets", recursive = TRUE)

fls = c(
  "README.Rmd" = "index.Rmd",
  "NEWS.md" = "news.Rmd",
  "vignettes/build-pipes.Rmd" = "docs.Rmd",
  "vignettes/tutorial.Rmd" = "tutorial.Rmd"
)

unlink(fls)
file.copy(from = file.path(code_path, names(fls)), to = fls)
dir.create("files")
file.copy(file.path(code_path, "vignettes/files"), to = ".", recursive = TRUE)

render("index.Rmd", output_format = pd)
check_output("index.html")

render("docs.Rmd", output_format = pd_expand)
check_output("docs.html")
#system("open docs.html")
render("tutorial.Rmd", output_format = pd_expand)
check_output("tutorial.html")

render("news.Rmd", output_format = pd_expand)
check_output("news.html")

dir.create(file.path(code_path, "inst/staticdocs"))
#undebug(packagedocs:::get_rd_data)
#load_all("~/Dropbox/public/github_packagedocs/")
#debug(rd_template)
render_rd("rd_skeleton.Rmd", "flowr", code_path,
          rd_index = "rd_index.yaml", output_format = pd_expand)

check_output("rd.html")
if(Sys.info()['sysname'] == "Darwin"){
	system("open index.html")
}


## ---- create a PDF manual as well
if(FALSE){
	#setwd("~/Dropbox/public/github_flowrpages/flowr")
	rd = "../../github_flow/man/to_flow.Rd"
	require(tools)
	Rd2latex(rd, out = "rd/to_flow.tex")
	library(pander)
	system("pandoc -f html -t markdown rd.html > rd2.md")
	render("pdf.Rmd", pdf_document())
}

