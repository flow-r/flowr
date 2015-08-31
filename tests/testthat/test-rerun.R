

library(flowr)

#system("env")

set_opts(verbose = TRUE)
get_opts()

verbose = FALSE

context("\n\nTesting running a pipeline from scratch")

pip = fetch_pipes("sleep_pipe", silent = FALSE)

if(verbose) print(pip)

test_that("check return of fetch_pipes", {
	expect_equal(length(pip), 4)
	expect_identical(class(pip), "data.frame")
})


message("class: ", class(pip$pipe))
message("file ", pip$pipe, " exists: ", file.exists(pip$pipe))
source(pip$pipe)
out = sleep_pipe(x = 3, "sample1")

test_that("test output of sleep_pipe", {
	expect_equal(length(out), 2)
	expect_identical(class(out), "list")
	expect_identical(is.flowmat(out$flowmat), TRUE)
})


#context("test creation of flowdef")
## --- specify the resource requirement
def = to_flowdef(out$flowmat,
								 platform = "lsf", queue = "short" ,
								 memory_reserved = "16384", ## LSF wants more than 16GB
								 cpu_reserved = 1, walltime = "00:59")

test_that("test creation of flowdef", {
	expect_identical(is.flowdef(def), TRUE)
})


#context("Test creation of flow")
fobj = to_flow(out$flowmat, def = def)

test_that("test creation of flow", {
	expect_is(fobj, "flow")
})





