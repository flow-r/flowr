#insert to drat

pkg = devtools::build()

out = drat::insertPackage(pkg, "~/Dropbox/public/github_drat")



## drat for user:
drat::addRepo("sahilseth")
