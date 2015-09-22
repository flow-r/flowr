#insert to drat
library(drat)

pkg = devtools::build("~/Dropbox/public/github_flow")
out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat")

pkg = devtools::build("~/Dropbox/public/github_funr")
out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat")

pkg = devtools::build("~/Dropbox/public/github_ngsflows")
out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat")

pkg = devtools::build("~/Dropbox/public/github_params")
out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat")



if(FALSE){
	## drat for user:
	library(drat)
	drat::addRepo("sahilseth")
	install.packages("flowr")
	install.packages("ngsflows")
	
}