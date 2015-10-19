#insert to drat
library(drat)

pkg1 = devtools::build("~/Dropbox/public/github_flow")
pkg2 = devtools::build("~/Dropbox/public/github_packagedocs")
pkg3 = devtools::build("~/Dropbox/public/github_staticdocs")
pkg4 = devtools::build("~/Dropbox/public/github_funr")
pkg5 = devtools::build("~/Dropbox/public/github_ngsflows")
pkg6 = devtools::build("~/Dropbox/public/github_params")

pkgs = c(pkg1, pkg2, pkg3, pkg4, pkg5, pkg6)

for(pkg in pkgs){
	out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat", TRUE)
}
system("cd ~/Dropbox/public/github_drat;git push")



if(FALSE){
	## drat for user:
	#library(drat)
	
	#drat::addRepo("sahilseth")
	install.packages("params", repos = "http://sahilseth.github.io/drat")
	install.packages("flowr", repos = "http://sahilseth.github.io/drat")
	
	install.packages("ngsflows")
	
}