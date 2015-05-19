---
title: "Quick Start Example"
date: "2015-05-19"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Quick Start Example}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---



```r
install.packages('devtools')
devtools::install_github("sahilseth/flow")
```

Run a setup function which copies 'flowr' Rscript to subsetquent steps easier.
More on this [here](https://github.com/sahilseth/rfun).


```r
library(flowr)
setup()
```

```
## Consider adding ~/bin to your PATH variable in .bashrc.
## export PATH=$PATH:$HOME/bin
## You may now use all R functions using 'flowr' from shell.
```


# Create a flow using example data

```r
exdata = file.path(system.file(package = "flowr"), "extdata")
flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))
```

```
## Warning in file(file, "rt"): cannot open file '/DRIVES/Dropbox2/Dropbox/
## public/github_flow/extdata/example1_flow_mat.txt': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"))
```

```
## Warning in file(file, "rt"): cannot open file '/DRIVES/Dropbox2/Dropbox/
## public/github_flow/extdata/example1_flow_def.txt': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
flow_mat = subset(flow_mat, samplename == "sample1")
```

```
## Error in subset(flow_mat, samplename == "sample1"): object 'flow_mat' not found
```

```r
fobj <- to_flow(x = flow_mat, def = flow_def, 
	flowname = "example1",
	platform = "lsf")
```

```
## Error in to_flow(x = flow_mat, def = flow_def, flowname = "example1", : object 'flow_mat' not found
```



```r
plot_flow(fobj)
```

```
## Error in plot_flow(fobj): error in evaluating the argument 'x' in selecting a method for function 'plot_flow': Error: object 'fobj' not found
```

## For Figure1 the following process would be followed:

- `sleep`: Run all 10 sleep jobs for given sample
- `tmp`: Create 10 temporary files, after sleep jobs are complete
	- dependency is serial, tmp jobs does not wait for all sleep jobs to complete. 
	- This is a one-to-one relationship
- `merge`: When all `tmp` are complete, merge them
- `size`: get their size when merge is complete


```r
submit_flow(fobj)
submit_flow(fobj, execute = TRUE)
```

