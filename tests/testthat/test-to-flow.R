require(testthat)

flowdef = as.flowdef("~/Dropbox/public/github_flow/inst/pipelines/abcd.def")
flowmat = as.flowmat("~/Dropbox/public/github_flow/inst/pipelines/abcd.tsv")

fobj_lsf = to_flow(flowmat, flowdef, flow_run_path = "", platform = "lsf")
fobj_torque = to_flow(flowmat, flowdef, flow_run_path = "", platform = "torque")
fobj_sge = to_flow(flowmat, flowdef, flow_run_path = "", platform = "sge")
#fobj_slurm = to_flow(flowmat, flowdef, flow_run_path = "", platform = "slurm")

expect_is(object = fobj_lsf, class = "flow")

