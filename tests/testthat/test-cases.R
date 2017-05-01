#library(flowr)

#system("env")

#set_opts(verbose = TRUE)
#opts_flow$get()

verbose = FALSE

opts_flow$set(verbose = 0)

#context("\n\nTesting a few cases")

test_that("flow case 1", {
  skip("case 1")
  
  wd = system.file("extdata/test_cases", package = "flowr")
  #wd = "~/Dropbox2/Dropbox/public/github_flow/inst/extdata/test_cases"
  #wd = "~/Dropbox/public/github_flow/inst/extdata/test_cases"
  library(flowr)
  
  flowmat = as.flowmat(file.path(wd, "flow_mat.tsv"))
  flowdef = as.flowdef(file.path(wd, "flow_def.tsv"))
  
  # cat(tmp)
  
  
  tmp = tempdir()
  fobj = to_flow(flowmat, def = flowdef, platform = "test", flow_run_path = tmp)
  
  plot_flow(fobj)
  
  #undebug(flowr:::submit_flow.flow)
  submit_flow(fobj, execute = TRUE)
  
})