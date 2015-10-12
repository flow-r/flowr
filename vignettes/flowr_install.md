# flowr
Sahil Seth  
`r Sys.Date()`  



# Installation

Requirements:

- R version > 3.1, preferred 3.2



```r
## for a latest official version (from CRAN)
install.packages("flowr", repos = CRAN="http://cran.rstudio.com")

## Latest stable release from DRAT (updated every other week); CRAN for dependencies
install.packages("flowr", repos = c(CRAN="http://cran.rstudio.com", DRAT="http://sahilseth.github.io/drat"))

## OR cutting edge devel version
devtools::install_github("sahilseth/flowr", ref = "devel")
```

After installation run `setup()`, this will copy the flowr's helper script to `~/bin`. Please make sure that this folder is in your `$PATH` variable.


```r
library(flowr)
setup()
```


Running `flowr` from the terminal should now show the following:

```
Usage: flowr function [arguments]

status          Detailed status of a flow(s).
rerun           rerun a previously failed flow
kill            Kill the flow, upon providing working directory
fetch_pipes     Checking what modules and pipelines are available; flowr fetch_pipes

Please use 'flowr -h function' to obtain further information about the usage of a specific function.
```

If you interested, visit [funr's github page for more details](https://github.com/sahilseth/funr)

From this step on, one has the option of typing commands in a R console OR a bash shell (command line). 
For brevity we will show examples using the shell.

# Test

**Test a small pipeline on the cluster**

This will run a three step pipeline, testing several different relationships between jobs. 
Initially, we can test this locally, and later on a specific HPCC platform. 


```
## This may take about a minute or so.
flowr run x=sleep_pipe platform=local execute=TRUE
## corresponding R command:
run(x='sleep_pipe', platform='local', execute=TRUE)
```

If this completes successfully, we can try this on a computing cluster; where this would submit
a few interconnected jobs. 

Several platforms are supported out of the box (torque, moab, sge, slurm and lsf), you may use the platform variable
to switch between platforms.

```
flowr run x=sleep_pipe platform=lsf execute=TRUE
## other options for platform: torque, moab, sge, slurm, lsf
## this shows the folder being used as a working directory for this flow.
```

Once the submission is complete, we can test the status using `status()` by supplying it the full path
as recovered from the previous step.

```
flowr status x=~/flowr/runs/sleep_pipe-samp1-20150923-10-37-17-4WBiLgCm

## we expect to see a table like this when is completes successfully:

|               | total| started| completed| exit_status|status    |
|:--------------|-----:|-------:|---------:|-----------:|:---------|
|001.sleep      |     3|       3|         3|           0|completed |
|002.create_tmp |     3|       3|         3|           0|completed |
|003.merge      |     1|       1|         1|           0|completed |
|004.size       |     1|       1|         1|           0|completed |

## Also we expect a few files to be created:
ls ~/flowr/runs/sleep_pipe-samp1-20150923-10-37-17-4WBiLgCm/tmp
samp1_merged  samp1_tmp_1  samp1_tmp_2  samp1_tmp_3

## If both these checks are fine, we are all set !
```

There are a few places where things may go wrong, you may follow the advanced configuration guide for more details.
Feel free to post questions on [github issues page](https://github.com/sahilseth/flowr/issues).


# Advanced Configuration

## HPCC Support Overview

Support for several popular cluster platforms is built-in. There is a template, for each platform, which should work out of the box.
Further, one may copy and edit them (and save to `~/flowr/conf`) in case some changes are required. Templates from this folder (`~/flowr/conf`), would override defaults.

Here are links to latest templates on github:

- [torque](https://github.com/sahilseth/flowr/blob/master/inst/conf/torque.sh)
- [lsf](https://github.com/sahilseth/flowr/blob/master/inst/conf/lsf.sh)
- [moab](https://github.com/sahilseth/flowr/blob/master/inst/conf/moab.sh)
- [sge](https://github.com/sahilseth/flowr/blob/master/inst/conf/sge.sh)
- [slurm](https://github.com/sahilseth/flowr/blob/master/inst/conf/slurm.sh), needs testing


**Not sure what platform you have?**

You may check the version by running ONE of the following commands:

```
msub --version
## Version: **moab** client 8.1.1
man bsub
##Submits a job to **LSF** by running the specified
qsub --help
```



Here are some helpful guides and details on the platforms:

- PBS: [wiki](http://en.wikipedia.org/wiki/Portable_Batch_System)
- Torque: [wiki](http://en.wikipedia.org/wiki/TORQUE_Resource_Manager)
	- MD Anderson
	- [University of Houston](http://www.rcc.uh.edu/hpc-docs/49-using-torque-to-submit-and-monitor-jobs.html)
- LSF [wiki](http://en.wikipedia.org/wiki/Platform_LSF):
	- Harvard Medicla School uses: [LSF HPC 7](https://wiki.med.harvard.edu/Orchestra/IntroductionToLSF)
	- Also used at [Broad](https://www.broadinstitute.org/gatk/guide/article?id=1311)
- SGE [wiki](http://en.wikipedia.org/wiki/Sun_Grid_Engine)
	- A tutorial for [Sun Grid Engine](https://sites.google.com/site/anshulkundaje/inotes/programming/clustersubmit/sun-grid-engine)
	- Another from [JHSPH](http://www.biostat.jhsph.edu/bit/cluster-usage.html)
	- Dependecy info [here](https://wiki.duke.edu/display/SCSC/SGE+Job+Dependencies)

[Comparison_of_cluster_software](http://en.wikipedia.org/wiki/Comparison_of_cluster_software)

## FAQs and help on Solving Issues

#### Errors in job submission

<div class="alert alert-warning" role="alert">
Possible issue: Jobs are not getting submitted
</div>

<div class="alert alert-info" role="alert">

1. Check if the right platform was used for submission.
2. Confirm (with your system admin) that you have the privilege to submit jobs.
3. **Use a custom flowdef**: Many institutions have strict specification on the resource reservations. 
Make sure that the queue, memory, walltime, etc. requiremets are specified properly
4. **Use a custom submission template**: There are several parameters in the submission script
used to submit jobs to the cluster. You may customize this template to suit your needs.
</div>


**3. Use a custom flowdef**

We can copy an example flow definition and customize it to suit our needs. This a tab delimited text file, so make sure that the format is correct after you make any changes.

```
cd ~/flowr/pipelines
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/pipelines/sleep_pipe.def
## check the format
flowr as.flowdef x=~/flowr/pipelines/sleep_pipe.def
```

*Run the test with a custom flowdef*:

```
flowr run x=sleep_pipe execute=TRUE def=~/flowr/pipelines/sleep_pipe.def ## platform=lsf [optional, picked up from flowdef]
```

**4. Use a custom submission template**

If you need to customize the HPCC submission template, copy the [file for your platform](https://github.com/sahilseth/flowr/tree/master/inst/conf) and make your desired changes. 
For example the MOAB based cluster in our institution does **not** accept the `queue` argument, 
so we need to comment it out.


*Download the template for a specific HPCC platform* into `~/flowr/conf`

```
cd ~/flowr/conf ## flowr automatically picks up a template from this folder.
## for MOAB (msub)
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/conf/moab.sh
## for Torque (qsub)
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/conf/torque.sh
## for IBM LSF (bsub)
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/conf/lsf.sh
## for SGE (qsub)
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/conf/sge.sh
## for SLURM (sbatch) [untested]
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/conf/slurm.sh
```

Make the desired changes using your favourite editor and submit again.

<div class="alert alert-warning" role="alert">
Possible issue: Jobs for subsequent steps are not submitting (though first step works fine).
</div>

<div class="alert alert-info" role="alert">
1. **Confirm jobids are parsing fine**: Flowr parses the computing platform's output and 
extracts job IDs of submitted jobs.
2. **Check dependency string**: 
</div>


**1. Parsing job ids**

Flowr parses job IDs to keep a log of all submitted jobs, and also to pass them along as a dependency to subsequent jobs. This is taken care by the [parse_jobids()](https://github.com/sahilseth/flowr/blob/master/R/parse-jobids.R) function. Each job scheduler shows the jobs id, when you submit a job, but it may show it in a slightly different fashion. To accommodate this one can use regular expressions as described in the relevant section of the [flowr config](https://github.com/sahilseth/flowr/blob/master/inst/conf/flowr.conf).

For example LSF may show a string such as:

```
Job <335508> is submitted to queue <transfer>.
## test if it parses correctly
jobid="Job <335508> is submitted to queue <transfer>."
set_opts(flow_parse_lsf = ".*(\<[0-9]*\>).*")
parse_jobids(jobid, platform="lsf")
[1] "335508"
```

In this case *335508* was the job id and regex worked well !

Once we identify the correct regex for the platform you may update the configuration file with it.

```
cd ~/flowr/conf 
wget https://raw.githubusercontent.com/sahilseth/flowr/master/inst/conf/flowr.conf
## flowr automatically reads from this location, if you prefer to put it elsewhere, use
load_opts("flowr.conf") ## visit sahilseth.github.io/params for more details.
```

Update the regex pattern and submit again.


**2. Check dependency string**

After collecting job ids from previous jobs, flowr renders them as a dependency for subsequent
jobs. This is handled by [render_dependency.PLATFORM](https://github.com/sahilseth/flowr/blob/master/R/render-dependency.R) functions.

Confirm that the dependency parameter is specified correctly in the submission scripts:

```
wd=~/flowr/runs/sleep_pipe-samp1-20150923-11-20-39-dfvhp5CK ## path to the most recent submission
cat $wd/002.create_tmp/create_tmp_cmd_1.sh
```
#### Flowr Configuration file
<div class="alert alert-warning" role="alert">
Possible issue: Flowr shows too much OR too little information.
</div>

There are several [verbose levels](http://docs.flowr.space/rd.html#verbose) available (0, 1, 2, 3, ...)

One can change the verbose levels in this file (`~/flowr/conf/flowr.conf`) 
and check [verbosity](http://docs.flowr.space/rd.html#verbose) section in the help pages for more details.



#### Flowdef resource columns

<div class="alert alert-warning" role="alert">
Possible issue: 
What all resources are supported in the flow definition?
</div>

The resource requirement columns of flow definition are passed along to the final (cluster) submission script. 
For example values in `cpu_reserved` column would be populated as `{{{CPU}}}` in the submission template.

The following table provides a mapping between the flow definition columns and variables in the [submission templates](https://github.com/sahilseth/flowr/blob/master/inst/conf):



|flowdef variable |submission template variable |
|:----------------|:----------------------------|
|nodes            |NODES                        |
|cpu_reserved     |CPU                          |
|memory_reserved  |MEMORY                       |
|email            |EMAIL                        |
|walltime         |WALLTIME                     |
|extra_opts       |EXTRA_OPTS                   |
|*                |JOBNAME                      |
|*                |STDOUT                       |
|*                |CWD                          |
|*                |DEPENDENCY                   |
|*                |TRIGGER                      |
|**               |CMD                          |

`* These are generated on the fly` and `** This is gathered from flow mat`


#### Adding a new platform

<div class="alert alert-warning" role="alert">
Possible issue: Need to add a new platform
</div>

Adding a new platform involves [a few steps](https://github.com/sahilseth/flowr/issues/7), briefly we need to consider the following steps where changes would be necessary.

1. **job submission**: One needs to add a new template for the new platform. Several [examples](https://github.com/sahilseth/flowr/blob/master/inst/conf) are available as described in the previous section.
2. **parsing job ids**: flowr keeps a log of all submitted jobs, and also to pass them along as a dependency to subsequent jobs. This is taken care by the [parse_jobids()](https://github.com/sahilseth/flowr/blob/master/R/parse-jobids.R) function. Each job scheduler shows the jobs id, when you submit a job, but each shows it in a slightly different pattern. To accommodate this one can use regular expressions as described in the relevant section of the [flowr config](https://github.com/sahilseth/flowr/blob/master/inst/conf/flowr.conf).

3. **render dependency**: After collecting job ids from previous jobs, flowr renders them as a dependency for subsequent
jobs. This is handled by [render_dependency.PLATFORM](https://github.com/sahilseth/flowr/blob/master/R/render-dependency.R) functions.
4. **recognize new platform**: Flowr needs to be made aware of the new platform, for this we need to add a new class using the platform name. This is essentially a wrapper around the [job class](https://github.com/sahilseth/flowr/blob/master/R/class-def.R)

Essentially this requires us to add a new line like: `setClass("torque", contains = "job")`.


5. **killing jobs**: Just like submission flowr needs to know what command to use to kill jobs. This is defined in detect_kill_cmd function.



There are several [job scheduling](http://en.wikipedia.org/wiki/Job_scheduler) systems
available and we try to support the major players. Adding support is
quite easy if we have access to them. Your favourite not in the list?
re-open this issue, with details on the platform:
[adding platforms](https://github.com/sahilseth/flowr/issues/7)



<div class="alert alert-warning" role="alert">
Possible issue: For other issues upload the error shown in the out files to 
[github issues tracker](https://github.com/sahilseth/flowr/issues).
</div>

```
## outfiles end with .out, and are placed in a folder like 00X.<jobname>/
## here is one example:
cat $wd/002.create_tmp/create_tmp_cmd_1.out
## final script:
cat $wd/002.create_tmp/create_tmp_cmd_1.sh
```


<script src = "files/googl.js"></script>