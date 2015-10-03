
message("installing required packages....", getwd())
options(repos = c(CRAN = "http://cran.rstudio.com"))
if(!require(drat))
	install.packages("drat")

library(drat)
repo = addRepo("sahilseth")

#devtools::install_github("sahilseth/ngsflows")
if(!require(ngsflows))
	install.packages("ngsflows")
library(ngsflows)

library(staticdocs)
library(packagedocs)
require(flowr)
require(knitr)



if(Sys.info()['sysname'] == "Darwin"){
	outwd = "~/Dropbox/public/github_flowrpages"
	code_path <- "~/Dropbox/public/github_flow"
}else{
	outwd = "gh-pages"
	code_path <- "../"
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
pd_expand <- package_docs(lib_dir = "assets", toc_collapse = FALSE, toc_depth = 2)
knitr::opts_knit$set(root.dir = normalizePath("."))

# generate index.html, get new from template !
unlink("assets", recursive = TRUE)

message("Copying RMD files ....", getwd())
fls = c(
#  "README.Rmd" = "index.Rmd",
  "NEWS.md" = "news.Rmd",
  "vignettes/flowr_overview.Rmd" = "docs.Rmd",
  "vignettes/flowr_install.Rmd" = "install.Rmd",
  "vignettes/flowr_tutorial.Rmd" = "tutorial.Rmd"
)

unlink(fls)
file.copy(from = file.path(code_path, names(fls)), to = fls)
dir.create("files")
file.copy(file.path(code_path, "vignettes/files"), to = ".", recursive = TRUE)

message("rendering RMD files ....", getwd())

render("index.Rmd", output_format = pd)
check_output("index.html")
render("docs.Rmd", output_format = pd_expand)
check_output("docs.html")
render("tutorial.Rmd", output_format = pd_expand)
check_output("tutorial.html")
render("news.Rmd", output_format = pd_expand)
check_output("news.html")
render("install.Rmd", output_format = pd_expand)
check_output("install.html")

message("rendering RD files ....", getwd())
dir.create(file.path(code_path, "inst/staticdocs"))
#devtools::load_all("~/Dropbox/public/github_packagedocs/")
#debug(packagedocs:::get_rd_data)
#debug(rd_template)
#undebug(staticdocs:::to_html.Rd_content)
render_rd("rd_skeleton.Rmd", "flowr", code_path,
          rd_index = "rd_index.yaml", output_format = pd_expand)
check_output("rd.html")



## stuff for MAC ONLY
if(Sys.info()['sysname'] == "Darwin"){
	system("open index.html")
	setwd("~/Dropbox/public/github_flowrpages")
	system("rm manual.pdf;R CMD Rd2pdf --no-preview -o manual.pdf ~/Dropbox/public/github_flow")
	system("git commit -a -m 'update website'")
	system("git push")
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

setwd(code_path)

