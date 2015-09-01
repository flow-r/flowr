library(testthat)
library(flowr)

system("env")
system.file(package = "flowr")

#set_opts("verbose")
get_opts()

test_check("flowr")


#test_package("flowr")
