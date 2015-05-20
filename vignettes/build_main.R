## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE
)
library(flowr)

## ----example_flowmat-----------------------------------------------------
## 6 jobs with 10 each
cmds = sprintf("sleep %s", round(rchisq(6*10, df = 1), 1))
nms = sprintf("myjob%s", rep(1:6, each = 10))
mat = data.frame(cbind(jobname = nms, cmd = cmds), stringsAsFactors = FALSE)
head(mat)


