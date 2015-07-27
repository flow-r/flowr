
devtools::load_all("~/Dropbox/public/github_flow")
#debug(split_multi_dep)
#debug(display_mat)
#debug(.plot_flow_dat_type1)

def = "~/Dropbox/iacsSVN/RPacks/SaturnV/data/flow_def_stage1_hms_dna.txt"
plot_flow(def, detailed = FALSE)

def = "~/Dropbox/public/github_ngsflows/inst/pipelines/split_aln_merge.def"
plot_flow(def, detailed = FALSE)


def = "~/Dropbox/public/github_flow/inst/pipelines/abcd.def"
plot_flow(def, detailed = FALSE)

def = "~/Dropbox/public/github_flow/inst/pipelines/sleep_pipe.def"
plot_flow(def, detailed = FALSE)
