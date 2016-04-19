

#insert to drat
library(drat)

#rmarkdown::render("~/Dropbox/public/github_flow/misc/insert_drat.R")

pkg1 = devtools::build("~/Dropbox/public/github_flow")
pkg4 = devtools::build("~/Dropbox/public/github_funr")
pkg6 = devtools::build("~/Dropbox/public/github_params")
pkg5 = devtools::build("~/Dropbox/public/flow-r/ultraseq")
pkg2 = devtools::build("~/Dropbox/public/github_packagedocs")
pkg3 = devtools::build("~/Dropbox/public/github_staticdocs")

pkgs = c(pkg1, pkg2, pkg3, pkg4, pkg5, pkg6)

for(pkg in pkgs){
	out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat", TRUE)
}

pkg = pkg4
pkg = pkg6
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




devtools::session_info()






# END