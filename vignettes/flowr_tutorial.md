# flowr
Sahil Seth  
`r Sys.Date()`  



# Tutorial: building a pipeline





A pipeline consists of several pieces, most essential of which is a function which generates a [flowmat](http://docs.flowr.space/docs.html#flow_mat). 
Once we get a flowmat, we need a [flow definition](http://docs.flowr.space/docs.html#flow_definition), which descibes how to stich various steps of the pipeline in a logical
flow. Optionally, one may use an additional file with default
paths to various tools and their parameters etc.


```
## to follow this tutorial, you may download them:
url=https://raw.githubusercontent.com/sahilseth/flowr/master/inst/pipelines
cd ~/flowr/pipelines
wget $url/sleep_pipe.R             ## A R script, with sleep_pipe(), which creates a flowmat
wget $url/sleep_pipe.def           ## A tab-delimited flow definition file
wget $url/sleep_pipe.conf          ## An *optional* tab-delim conf file, defining default params
```

These three files are available under the [pipelines](https://github.com/sahilseth/flowr/tree/master/inst/pipelines) folder on github.



Here is a pipeline and mentioned in the [overview](http://docs.flowr.space/docs) as well where
we sleep for a few seconds, create temporary files, merge them etc.

One may use a `run` function to create the flowmat, fetch the flowdef and execute the pipeline in a single step. Here we would focus more on each of these steps in detail.



```r
## 1. Single step submission:
fobj = run("sleep_pipe", execute = TRUE); 

## 2a. Details of the above step:
setwd("~/flowr/pipelines")
## behind the scenes, run does the following:
## optionally, load default parameters
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




## Creating Modules/Pipelines



The `sleep_pipe` calls the three other functions (**modules**); fetches flowmat from each, then rbinds them,
creating a larger flowmat. You may refer to the [sleep_pipe.R](https://github.com/sahilseth/flowr/blob/master/inst/pipelines/sleep_pipe.R)
file for the source.




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



```r
## create a flow matrix
out = sleep_pipe(x = 3, "sample1")
flowmat = out$flowmat
```


samplename   jobname      cmd                                                            
-----------  -----------  ---------------------------------------------------------------
sample1      sleep        sleep 2 && sleep 7;echo 'hello'                                
sample1      sleep        sleep 3 && sleep 14;echo 'hello'                               
sample1      sleep        sleep 5 && sleep 20;echo 'hello'                               
sample1      create_tmp   head -c 100000 /dev/urandom > sample1_tmp_1                    
sample1      create_tmp   head -c 100000 /dev/urandom > sample1_tmp_2                    
sample1      create_tmp   head -c 100000 /dev/urandom > sample1_tmp_3                    
sample1      merge        cat sample1_tmp_1 sample1_tmp_2 sample1_tmp_3 > sample1_merged 
sample1      size         du -sh sample1_merged; echo 'MY shell:' $SHELL                 


Next, we need a flow definition.


<div class="alert alert-info" role="alert">
**module:** A R function which creates a flow mat, is a module. Using **module + flowdef**, we can run a pipeline.
</div>

## Creating Flow Definition

Flowr enables us to quickly create a skeleton flow definition using a flowmat, which we can then alter to suit our needs. A handy function
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

- multiple *sleep* commands would run as `scatter`/parallel, with `none` as the dependency.
- For each *sleep*, *create_tmp* creates a tmp file as `scatter`, using a `serial` type dependency. One `create_tmp` for one `sleep` (one-to-one relationship).
- Then all tmp files are *merged*. Intuitively, since this is a single step, we run it as `serial` and as all tmp files are required, we use a `gather` type dependency.
- Lastly, we need to check the *size* of the resulting merged file.
Again, since this is a single step, we run is as `serial`. More so since the previous step also had a single command, we use a `serial` type dependency.



```r
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

