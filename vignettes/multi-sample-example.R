## ------------------------------------------------------------------------
require(flowr)
ex = file.path(system.file(package = "flowr"), "examples")
flow_mat = read_sheet(file.path(ex, "sleep_pipe.tsv"))
flow_def = read_sheet(file.path(ex, "sleep_pipe.def"))

## ------------------------------------------------------------------------
fobj <- to_flow(x = flow_mat, def = flow_def)

## ----eval=FALSE----------------------------------------------------------
#  fobj <- to_flow(x = flow_mat, def = flow_def, platform = "local")
#  fobj <- to_flow(x = flow_mat, def = flow_def, platform = "moab")
#  fobj <- to_flow(x = flow_mat, def = flow_def, platform = "lsf")

