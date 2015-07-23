## ------------------------------------------------------------------------
require(flowr)
ex = file.path(system.file(package = "flowr"), "examples")
flow_mat = read_sheet(file.path(ex, "sleep_pipe.tsv"))
flow_def = read_sheet(file.path(ex, "sleep_pipe.def"))

