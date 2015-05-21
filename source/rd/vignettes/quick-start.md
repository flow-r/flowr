---
title: "Quick Start Example"
date: "2015-05-20"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Quick Start Example}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



Get me started
-------------



```r
install.packages('devtools')
devtools::install_github("sahilseth/flowr")
```

Run a setup function which copies 'flowr' Rscript to subsetquent steps easier.
More on this [here](https://github.com/sahilseth/rfun).


```r
library(flowr)
setup()
```

```
#> Consider adding ~/bin to your PATH variable in .bashrc.
#> export PATH=$PATH:$HOME/bin
#> You may now use all R functions using 'flowr' from shell.
```


# Create a flow using example data

**Let us say we want to do**:

- Have a few jobs which will just wait (sleep) for a few seconds (`sleep`)
- Then... (`tmp`) 
	- Create a few temporary files. But hey, as soon as a sleep completes start the corresponding `tmp` job.
	- If you are so inclined more on this [here](docs.flowr.space/build/html/rd/vignettes/build-pipes.html#serial-one-to-one-relationship).
	- Don't wait for all to complete
- When all `tmp` jobs are complete, `merge` them
- Then get the `size` of the resulting file

Now to do this we need two basic ingedients. A table with the commands, and a another which specifies the flow. We call them `flow_mat` and `flow_def`.

We already have examples so lets load them from the package and see how they look.



```r
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"), id_column = "samplename")
## this has a bunch of samples, so let us subset one of them
flow_mat = subset(flow_mat, samplename == "sample1")
flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"), id_column = "jobname")
```

# Ingredient 1: Commands to run (flow_mat)

```r
kable(subset(flow_mat, samplename == 'sample1'))
```



|samplename |jobname |cmd                                  |
|:----------|:-------|:------------------------------------|
|sample1    |sleep   |sleep 6                              |
|sample1    |sleep   |sleep 16                             |
|sample1    |sleep   |sleep 8                              |
|sample1    |tmp     |head -c 100000 /dev/urandom > tmp1_1 |
|sample1    |tmp     |head -c 100000 /dev/urandom > tmp1_2 |
|sample1    |tmp     |head -c 100000 /dev/urandom > tmp1_3 |
|sample1    |merge   |cat tmp1_1 tmp1_2 tmp1_3 > merge1    |
|sample1    |size    |du -sh merge1                        |

# Ingredient 2: Flow Definition (flow_def)
More on the format of this file [here].


```r
kable(flow_def)
```



|jobname |prev_jobs |dep_type |sub_type |queue  | memory_reserved|walltime | cpu_reserved|
|:-------|:---------|:--------|:--------|:------|---------------:|:--------|------------:|
|sleep   |none      |none     |scatter  |medium |          163185|23:00    |            1|
|tmp     |sleep     |serial   |scatter  |medium |          163185|23:00    |            1|
|merge   |tmp       |gather   |serial   |medium |          163185|23:00    |            1|
|size    |merge     |serial   |serial   |medium |          163185|23:00    |            1|

# Stich it
> into a flow


```r
fobj <- to_flow(x = flow_mat, def = flow_def, 
	flowname = "example1", platform = "lsf")
```

```
#> Using description default: type1
#> Using flow_base_path default: ~/flowr
#> ....
```

# Plot it

```r
plot_flow(fobj)
```

![Flow chart describing process for example 1](figure/plot_example1-1.pdf) 


# Test it
> Dry run (submit)


```r
submit_flow(fobj)
```

```
Test Successful!
You may check this folder for consistency. Also you may re-run submit with execute=TRUE
 ~/flowr/type1-20150520-15-18-27-5mSd32G0
```

# Submit it !

> Submit to the cluster


```r
submit_flow(fobj, execute = TRUE)
```

```
Flow has been submitted. Track it from terminal using:
flowr::status(x="~/flowr/type1-20150520-15-18-46-sySOzZnE")
OR
flowr status x=~/flowr/type1-20150520-15-18-46-sySOzZnE
```


# Check the status

```
flowr status x=~/flowr/type1-20150520-15-18-46-sySOzZnE
```

```
Loading required package: shape
Flowr: streamlining workflows
Showing status of: /rsrch2/iacs/iacs_dep/sseth/flowr/type1-20150520-15-18-46-sySOzZnE


|          | total| started| completed| exit_status|
|:---------|-----:|-------:|---------:|-----------:|
|001.sleep |    10|      10|        10|           0|
|002.tmp   |    10|      10|        10|           0|
|003.merge |     1|       1|         1|           0|
|004.size  |     1|       1|         1|           0|
```
