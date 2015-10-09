#insert to drat
library(drat)

pkg = devtools::build("~/Dropbox/public/github_flow")
pkg = devtools::build("~/Dropbox/public/github_packagedocs")
pkg = devtools::build("~/Dropbox/public/github_staticdocs")
pkg = devtools::build("~/Dropbox/public/github_funr")
pkg = devtools::build("~/Dropbox/public/github_ngsflows")
pkg = devtools::build("~/Dropbox/public/github_params")

out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat", TRUE)
system("cd ~/Dropbox/public/github_drat;git push")



if(FALSE){
	## drat for user:
	#library(drat)
	
	#drat::addRepo("sahilseth")
	install.packages("params", repos = "http://sahilseth.github.io/drat")
	install.packages("flowr", repos = "http://sahilseth.github.io/drat")
	
	install.packages("ngsflows")
	
}