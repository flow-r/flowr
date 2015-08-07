context("Test fetching of sleep pipe")

pip = fetch_pipes("sleep_pipe")

test_that("test fetching of sleep pipe", {
	expect_equal(length(pip), 4)
	expect_identical(class(pip), "data.frame")
})

source(pip$pipe)
out = sleep_pipe(x = 3, "sample1")

context("Test creation of flowmat")

test_that("test creation of flowmat", {
	expect_equal(length(out), 2)
	expect_identical(class(out), "list")
	expect_identical(is.flowmat(out$flowmat), TRUE)
})


context("Test creation of flowdef")
## --- specify the resource requirement
def = to_flowdef(out$flowmat,
								 platform = "lsf", queue = "short" ,
								 memory_reserved = "16384", ## LSF wants more than 16GB
								 cpu_reserved = 1, walltime = "00:59")

test_that("test creation of flowdef", {
	expect_identical(is.flowdef(def), TRUE)
})


context("Test creation of flow")
fobj = to_flow(out$flowmat, def = def)

test_that("test creation of flow", {
	expect_identical(class(fobj)[1], "flow")
})





