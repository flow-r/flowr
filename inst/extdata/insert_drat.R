#insert to drat
library(drat)

pkg = devtools::build("~/Dropbox/public/github_flow")
pkg = devtools::build("~/Dropbox/public/github_packagedocs")
pkg = devtools::build("~/Dropbox/public/github_staticdocs")
pkg = devtools::build("~/Dropbox/public/github_funr")
pkg = devtools::build("~/Dropbox/public/github_ngsflows")
pkg = devtools::build("~/Dropbox/public/github_params")

out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat", 
													c("add", "commit", "push"))



if(FALSE){
	## drat for user:
	#library(drat)
	system("git commit -a -m 'update website'")
	system("git push")
	
	#drat::addRepo("sahilseth")
	install.packages("params", repos = "http://sahilseth.github.io/drat")
	install.packages("flowr", repos = "http://sahilseth.github.io/drat")
	
	install.packages("ngsflows")
	
}