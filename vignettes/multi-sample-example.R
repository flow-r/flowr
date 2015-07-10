## ------------------------------------------------------------------------
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))
flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"))

## ------------------------------------------------------------------------
fobj <- to_flow(x = flow_mat, def = flow_def)

## ----eval=FALSE----------------------------------------------------------
#  fobj <- to_flow(x = flow_mat, def = flow_def, platform = "local",
#  								execute = TRUE)
#  fobj <- to_flow(x = flow_mat, def = flow_def, platform = "moab")
#  fobj <- to_flow(x = flow_mat, def = flow_def, platform = "lsf")

