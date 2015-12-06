#library(flowr)

#system("env")

#set_opts(verbose = TRUE)
#opts_flow$get()

verbose = FALSE

set_opts(verbose = 0)

context("\n\nTesting running a pipeline from scratch")

pip = fetch_pipes("sleep_pipe", silent = TRUE)[1,]

if(verbose) print(pip)

test_that("check return of fetch_pipes", {
	expect_equal(length(pip), 4)
	expect_identical(class(pip), "data.frame")
})

if(verbose) message("class: ", class(pip$pipe))
if(verbose) message("file ", pip$pipe, " exists: ", file.exists(pip$pipe))

# ----- flowmat ---------
context("Test creation of flowmat")
source(pip$pipe)
out = sleep_pipe(x = 3, "sample1")

test_that("test output of sleep_pipe", {
	expect_equal(length(out), 2)
	expect_identical(class(out), "list")
	expect_is(out$flowmat, 'flowmat')
	expect_is(check(out$flowmat), 'flowmat')
})


# ----- flowdef ---------
context("Test creation of flowdef")
## --- specify the resource requirement
def = to_flowdef(out$flowmat,
								 platform = "lsf", queue = "short" ,
								 memory_reserved = "16384", ## LSF wants more than 16GB
								 cpu_reserved = 1, walltime = "00:59")

test_that("test creation of flowdef", {
	expect_is(def, "flowdef")
	expect_is(def, "data.frame")
	expect_equal(nrow(def), 4)
	expect_equal(ncol(def), 11)
})


# ----- test creation of flowdef ---------
context("Test creation of flow")
rm_col <- function(x, nm){
	col = which(colnames(x) %in% nm)
	x[, -col]
}


fobj = to_flow(out$flowmat, def = def, 
							 flowname = "sleep_pipe",
							 flow_run_path = getwd())

test_that("test creation of flow, works", {
	expect_is(fobj, "flow")
})

test_that("test creation of flow, fails; when it should", {
	## remove a row
	expect_error(to_flow(out$flowmat, def = def[-1,]), "extra jobs in prev_jobs")
	expect_error(to_flow(out$flowmat, def = def[,-1]), "flowdef needs these columns")
	expect_error(to_flow(out$flowmat, def = def[,-2]), "flowdef needs these columns")
	expect_error(to_flow(out$flowmat, def = def[,-3]), "flowdef needs these columns")
	
	expect_error(
		to_flow(out$flowmat, def = rm_col(def, "cpu_reserved"))
							 , "flowdef: missing resource columns")
	expect_error(to_flow(out$flowmat, def = rm_col(def, "memory_reserved"))
							 , "flowdef: missing resource columns")
	
	def2=def;def2$dep_type[1] = "NA"
	expect_error(to_flow(out$flowmat, def = def2), "dep_type: invalid")
	
	def2=def;def2$sub_type[1] = "NA"
	expect_error(to_flow(out$flowmat, def = def2), "sub_type: invalid")

})


context("Test flowr on multiple platform, including creation, status, rerun and kill")

plats = c("lsf", "sge", "torque")
for(plat in plats){
	
	qobj <- queue(platform = plat, submit_exe = "echo")
	
	#set_opts(verbose = 0)
	fobj = to_flow(out$flowmat, def = def, 
								 flowname = "sleep_pipe",
								 flow_run_path = getwd(), 
								 	qobj = qobj)

	expect_is(fobj, "flow")
	expect_equal(qobj@submit_exe, "echo")
	expect_equal(fobj@jobs[[1]]@submit_exe, "echo")
	
	fobj_dry = suppressMessages(submit_flow(fobj))
	#expect_warning(suppressMessages(submit_flow(fobj, execute = TRUE)), "Unable to parse JOB IDs")
	
	## fake submit
	fobj_sub = fobj_dry
	fobj_sub@status = "submitted"
	
	
	test_that("test fobj is correct", {
		
		expect_is(fobj, "flow")
		expect_is(fobj_dry, "flow")
		expect_equal(fobj_dry@status, "dry-run")
		
		## create flowdet
		expect_is(to_flowdet(fobj_dry), "data.frame")
		expect_error(to_flowdet(fobj), "fobj: no execution details")
	})
	
	test_that("flow is killable", {
		## killing
		expect_error(kill(fobj), "fobj: no execution details")
	})
	
	test_that("flow is re-runnable", {
		##  -----  re-running
		expect_error(rerun(fobj), "fobj: not submitted yet")
		expect_error(rerun(fobj, start_from = "create_tmp"), "fobj: not submitted yet")
		
		## rerun the fake submit
		expect_error(rerun(fobj_sub), "start_from, select, ignore: missing")
		## check rerun output
		fobj_re = suppressMessages(rerun(fobj_sub, start_from = "create_tmp", kill = FALSE, execute = FALSE))
		#suppressMessages(rerun(fobj2, start_from = "create_tmp", kill = TRUE, qobj = qobj)), "kill"
		fobj_re = suppressMessages(rerun(fobj_sub, start_from = "create_tmp", kill = FALSE, qobj = qobj))
		
		## check the flow
		expect_is(fobj_re, "flow")
		
	})
	
	
}

## add re-run test, specifying without start_from, with select and ignore: FAILS
## specify none: fails
## count jobs after select, ignore and start_from, and match them to expectation.






