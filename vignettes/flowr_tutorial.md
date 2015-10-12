# flowr
Sahil Seth  
`r Sys.Date()`  



# Tutorial: building a pipeline



Let us use the same example described in the overview section. We start by getting a set of commands we would like to run. 

```
## wait for a few seconds…
sleep 5
sleep 5

## create two small files
cat $RANDOM > tmp1
cat $RANDOM > tmp2

## merge the two files
cat tmp1 tmp2 > tmp

## check the size of the resulting file
du -sh tmp
```

**Wrap these commands into R**


```r
sleep=c('sleep 5', 'sleep 5')

tmp=c('cat $RANDOM > tmp1', 
			'cat $RANDOM > tmp2')
			
merge='cat tmp1 tmp2 > tmp'
			
size='du -sh tmp'
```

Next, we would create a table using the above commands:


```r
## create a table of all commands
library(flowr)
lst = list( sleep=sleep, 
           create_tmp=tmp, 
           merge=merge,
           size=size)

flowmat = to_flowmat(lst, "samp1")
kable(flowmat)
```



samplename   jobname      cmd                 
-----------  -----------  --------------------
samp1        sleep        sleep 5             
samp1        sleep        sleep 5             
samp1        create_tmp   cat $RANDOM > tmp1  
samp1        create_tmp   cat $RANDOM > tmp2  
samp1        merge        cat tmp1 tmp2 > tmp 
samp1        size         du -sh tmp          



## Creating Flow Definition

We have a few steps in a pipeline; we would use a flow definition to descibe their flow. Flowr enables us to quickly create a skeleton flow definition using a flowmat, which we can then alter to suit our needs. A handy function
`to_flowdef`, accepts a `flowmat` and creates a flow definition. 



```r
## create a skeleton flow definition
def = to_flowdef(flowmat) 
suppressMessages(plot_flow(def))
```

![](flowr_tutorial_files/figure-html/plot_skeleton_def-1.png) 

<div class="alert alert-info" role="alert">
The default skeleton takes a very conservative approach, creating all submissions as **serial** and all dependencies as **gather**. This ensures robustness, compromising efficiency. So customize this to make it super efficient.
</div>

We can make a few changes to make this pipeline a little more efficient. Briefly, we would run a few steps in a **scatter** fashion (in parallel).

A few points to note:

- Intial steps have no dependency, so their **previous_jobs** and **dependency_type** is **none**.
- Steps with multiple commands, which can be run in parallel are submitted as **scatter**.
- Steps with single commands are submitted as **serial**.
- Say two consective steps run on small pieces of data, we have a **serial**
**one to one** relationship. Example, both **sleep** and **create_tmp** 
are submitted as **scatter** and **create_tmp** has a dependency_type **serial**.
- Finally if a step needs all the small pieces from a previous step, we use a **gather** type dependency.

<!--
- multiple *sleep* commands would run as `scatter`/parallel, with `none` as the dependency.
- For each *sleep*, *create_tmp* creates a tmp file as `scatter`, using a `serial` type dependency. One `create_tmp` for one `sleep` (one-to-one relationship).
- Then all tmp files are *merged*. Intuitively, since this is a single step, we run it as `serial` and as all tmp files are required, we use a `gather` type dependency.
- Lastly, we need to check the *size* of the resulting merged file.
Again, since this is a single step, we run is as `serial`. More so since the previous step also had a single command, we use a `serial` type dependency.
-->


```r
##               sleep     create tmp   merge     size
def$sub_type = c("scatter", "scatter", "serial", "serial")
def$dep_type = c("none", "serial", "gather", "serial")
kable(def)
```



jobname      sub_type   prev_jobs    dep_type   queue   memory_reserved   walltime    cpu_reserved  platform    jobid
-----------  ---------  -----------  ---------  ------  ----------------  ---------  -------------  ---------  ------
sleep        scatter    none         none       short   2000              1:00                   1  torque          1
create_tmp   scatter    sleep        serial     short   2000              1:00                   1  torque          2
merge        serial     create_tmp   gather     short   2000              1:00                   1  torque          3
size         serial     merge        serial     short   2000              1:00                   1  torque          4

![](flowr_tutorial_files/figure-html/plot_tweaked_def-1.png) 


<div class="alert alert-info" role="alert">
**Tip:** Alternatively, one may write this to a file 
(**write_sheet(def, "sleep_pipe.def")**), make changes in a text editor and read it again (**as.flowdef("sleep_pipe.def")**.
</div>

## Create flow, submit to cluster

**Next, we create a flow object:**


```r
fobj = to_flow(flowmat, def, flowname = "sleep_pipe")
```

**Finally, we can submit this to the cluster:**

```r
plot_flow(fobj)
submit_flow(fobj) ## dry run
fobj2 = submit_flow(fobj, execute = TRUE) ## submission to LSF cluster

## after submission, we can use the following:
status(fobj2) ## check status
rerun(fobj2)  ## re-run from a intermediate step
kill(fobj2)   ## kill it!
```


## Creating modules
We used a simple example where a single function was creating all the commands.
This is easier, but a step (or module) is not re-usable in another pipeline.
Thus we may write a module for each step, such that one may mix and match to 
create their own pipeline.

**NOTE:**
A module, always returns a flowmat. A module may have one or several steps.
A module + flowdef, becomes a pipeline.


```
## to follow this tutorial, you may download them:
url=https://raw.githubusercontent.com/sahilseth/flowr/master/inst/pipelines
cd ~/flowr/pipelines
wget $url/sleep_pipe.R ## A R script, with sleep_pipe(), which creates a flowmat
wget $url/sleep_pipe.def ## A tab-delimited flow definition file
wget $url/sleep_pipe.conf ## An *optional* tab-delim conf file, defining default params
```

The `sleep_pipe` calls the three other functions (**modules**); fetches flowmat from each, then rbinds them,
creating a larger flowmat. You may refer to the [sleep_pipe.R](https://github.com/sahilseth/flowr/blob/master/inst/pipelines/sleep_pipe.R)
file for the source.


```r
#' @param x number of sleep commands
sleep <- function(x, samplename){
	cmd = list(sleep = sprintf("sleep %s && sleep %s;echo 'hello'",
		abs(round(rnorm(x)*10, 0)),
		abs(round(rnorm(x)*10, 0))))
	flowmat = to_flowmat(cmd, samplename)
	return(list(flowmat = flowmat))
}

#' @param x number of tmp commands
create_tmp <- function(x, samplename){
	## Create 100 temporary files
	tmp = sprintf("%s_tmp_%s", samplename, 1:x)
	cmd = list(create_tmp = sprintf("head -c 100000 /dev/urandom > %s", tmp))
	## --- convert the list into a data.frame
	flowmat = to_flowmat(cmd, samplename)
	return(list(flowmat = flowmat, outfiles = tmp))
}

#' @param x vector of files to merge
merge_size <- function(x, samplename){
	## Merge them according to samples, 10 each
	mergedfile = paste0(samplename, "_merged")
	cmd_merge <- sprintf("cat %s > %s",
		paste(x, collapse = " "), ## input files
		mergedfile)
	## get the size of merged files
	cmd_size = sprintf("du -sh %s; echo 'MY shell:' $SHELL", mergedfile)

	cmd = list(merge = cmd_merge, size = cmd_size)
	## --- convert the list into a data.frame
	flowmat = to_flowmat(cmd, samplename)
	return(list(flowmat = flowmat, outfiles = mergedfile))
}
```


```r
#' @param x number of files to make
sleep_pipe <- function(x = 3, samplename = "samp1"){

	## call the modules one by one...
	out_sleep = sleep(x, samplename)
	out_create_tmp = create_tmp(x, samplename)
	out_merge_size = merge_size(out_create_tmp$outfiles, samplename)

	## row bind all the commands
	flowmat = rbind(out_sleep$flowmat,
		out_create_tmp$flowmat,
		out_merge_size$flowmat)

	return(list(flowmat = flowmat, outfiles = out_merge_size$outfiles))
}
```



## Using run(), to run the entire pipeline

One may use `run` function to create the flowmat, fetch the flowdef and execute the pipeline in a single step. Here we would focus more on each of these steps in detail.



```r
## 1. Single step submission:
fobj = run("sleep_pipe", execute = TRUE); 

## 2
## change wd, so that we can source the files downloaded in the previous step
setwd("~/flowr/pipelines")

## 2a. optionally, load default parameters
load_opts("sleep_pipe.conf") 

## 2b. get sleep_pipe() function
source("sleep_pipe.R") 
## create a flowmat
flowmat = sleep_pipe()

## 2c. read a flow definition.
flowdef = as.flowdef("sleep_pipe.def")

## 2d. create flow and submit to cluster
fobj = to_flow(flowmat, flowdef, execute = TRUE)
```

<!---
The first argument to run is the name of the pipeline, 
one may use a custom flow definition etc. Further any extra arguments supplied are passed on to the pipeline function.

For example, the `sleep_pipe` function has a argument **x** which 
determines number of tmp files to create. Default is 3, let us try changing that.


```r
#run("sleep_pipe", x = )
```

--->

<!----


We then define another function `sleep_pipe` which calls the above defined **modules**; fetches flowmat from each, 
creating a larger flowmat. This time we will define a flowdef for the `sleep_pipe` function, elevating its status from
module to a pipeline.


This time we will define a flowdef for the `sleep_pipe` function, elevating its status from
module to a pipeline.




Here are a few examples of modules, three functions `sleep`, `create_tmp` and `merge_size` each returning a flowmat.

We believe pipeline and modules may be interchangeble, in the sense that a *smaller* pipeline may be 
included as part of a larger pipeline.
In flowr a module OR pipeline always returns a flowmat.
The only difference being, a pipeline also has a correspomding flow definition file. 


<div class="alert alert-info" role="alert">
As such, creating a flow definition for a module enables flowr
to run it, hence a module **elevates**, becoming a pipeline.
This lets the user mix and match several modules/pipelines to create a customized larger pipeline(s).
</div>
-->

<script src = "files/googl.js"></script>
