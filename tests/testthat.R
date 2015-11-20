library(testthat)
library(flowr)

system("env")
system.file(package = "flowr")

#set_opts("verbose")
opts_flow$get()

test_check("flowr")


#test_package("flowr")
