---
title: "flowr"
subtitle: "Streamlining Workflows"
author: Sahil Seth
date: "`r Sys.Date()`"
output: html_document
packagedocs:
    toc: true
navpills: |
  <li><a href='overview.html'>Overview</a></li>
  <li><a href='install.html'>Install</a></li>
  <li><a href='tutorial.html'>Tutorial</a></li>
  <li><a href='rd.html'>Help</a></li>
  <li class="active"><a href='news.html'>News</a></li>
  <li><a href='https://github.com/flow-r/flowr'>Github <i class='fa fa-github'></i></a></li>
brand: |-
  <a href="http://flow-r.github.io/flowr">
  <img src='files/logo_red.png' alt='flowr icon' width='50px' height='40px' style='margin-top: -20px;margin-bottom: -20px'>
  </a>
copyright: Licence MIT
source: "github.com/flow-r/flowr/tree/devel/NEWS.md"
---

<br>


flowr 0.9.11
----------------------------------------------
> 2021-03-01

* added ability to toml files
* bugfix in for `moab` and `sge` schedulers (see #74 for details)
* bugfix in SBATCH params
* bugfix #91
* add ability to parse LSF resource output
* Several other bug fixes: from @sbamin fix #73, #74, #83


flowr 0.9.10.9022
----------------------------------------------
> 2017-04-30

* bugfix in for `moab` and `sge` schedulers (see #74 for details)


flowr 0.9.10 (dates)
----------------------------------------------
> 2016-04-18

tl;dr (**summary of changes**)

- Flowr Rscript gets further enhancements, taking advantage of improved [funr](https://github.com/sahilseth/funr)
- `run` function now accepts paths.

```
# 'cherries' version
cd <path to pipelines>
flowr x=mypipeline

# 'dates' version
flowr x=<path to pipelines>/mypipeline
```

- Previously, flowr expected a specific structure, now using `~/.flowr.conf`, 
one may specify their own structure - enabling flexibility.

```
# 'cherries' version: a fixed directory structure was recommended:
~/flowr
├── conf
│   ├── flowr.conf
├── pipelines
│   ├── sleep_pipe.R
├── runs

# 'dates' version: one may change the default paths to run, config files etc using 
~/.flowr.conf

# this file controls the location of these folders:
flow_base_path	~/flowr # flowr home
flow_conf_path	{{flow_base_path}}/conf  # path to configuration files, not required if using ~/.flowr.conf
flow_run_path	~/flowr/runs  # default home of all executed flows, you may change this
flow_pipe_paths	~/flowr/pipelines,<add new paths...> # multiple paths can be specified using ","
```

- a few bug fixes in [to_flow](https://github.com/flow-r/flowr/issues/66)
- several other minor changes to messages, errors and warnings.



flowr 0.9.9.5 (cherries)
----------------------------------------------
> 2015-12-03

tl;dr (**summary of changes**)

- Better handling of multiple flows in terms of running and re-running.
- Nicer and cleaner messages.
- Add two additional lines in `flowr.conf` (`modules_cmds` and `local_cores`), after upgrading the package.
Everything else is compatible with previous versions.

**additions/changes to `flowr.conf` file**

- **New**: option local_cores, which determines (max) number of cores to use when running local jobs.
- **New**: `flow_pipe_paths` now supports multiple paths, separated by comma. The `fetch_pipes()` would split the vector at commas.

- **IMP**: New version needs additional components in the `flowr.conf` file

- **New**: Now you can add a `module_cmds` variable to the config file, and this will be prefixed in all script of the pipeline. An example could be:

```diff
# version >= 0.9.8.9004
# max number of cores to use when running on a local server
local_cores	4

# default module of a pipeline
# version >= 0.9.8.9015
module_cmds	''

# examples: one may define all modules used in a pipeline here, 
# further one may specify any other command which should be run before 
# script executes.
#module_cmds	'module load samtools;export PATH=$PATH:/apps/bin'

```


**addition/changes to `status()`**

- **New**: status gets a new argument to turn off progress bar if needed.
- **New** enhanced `get_wds`/`status`, so that if current wd contains a flow_details file, status is shown for this folder and not sub-folder(s).

```
## now this works well !
flowr status x=.
```


**addition/changes to `run()` and `rerun()` functions**

- **New**: run function now accepts a custom configuration [`conf`], parameter. See `help(flowr::run)` for more details. This enables one, to specify custom parameters used for that pipeline.
- **New**: Now `rerun()` supports multiple folders. Basically, one may specify a parent folder which has multiple flowr runs and ask it to re-run **ALL** of them again, from a specific intermediate step.
- **New**: Flowr creates a new folder if there are multiple samples in the flowmat; basically containerizes the run, keeping the logs clean and debugging life easier.

- **New**: `run()` now supports, re-running as well. i.e. One would generate a new set of commands etc. but execute in the previous folder; possibly from a inter-mediate step (experimental feature).


**other changes**

- **New**: `to_flowdef()` can now guess submission and dependency types (experimental feature).
- **IMP**: `to_flowdef` now adds a parameter `nodes`, to enable specifying number of nodes required per-job.
- **IMP**: `opts_flow$get` replaces `get_opts`, for reliability etc. Also this closely follows how knitr options are set.
- fixed bugs in documentation (changed the formatting of output messages).





flowr 0.9.8 (blueberries)
----------------------------------------------
> 2015-10-02

- Modified the output of status function, to add a `status` column. Specifically, 
this uses information from other columns and summarizes whether a specific step is 
`pending`, `processing`, `completed` or `errored`.

```
================================================================================
|                | total| started| completed| exit_status|status     |
|:---------------|-----:|-------:|---------:|-----------:|:----------|
|001.alnCmd1     |   109|     109|       109|           0|completed  |
|007.markCmd     |     3|       3|         0|           0|processing |
```

- [0.9.7.11]
	- Switched default value of flow@status to "created". 
	- When using status on several folders, it used to be a little cluttered. 
	Have added spaces, so that this looks prettier now.
- **Introducing verbose levels**:

	One can set the level of verboseness using `opts_flow$set(verbose=2)`.
	Where the level may be 0, 1, 2.... 
	Level 1 is good for most purposes, where as, 
	level 0 is almost silent, producing messages 
	only when necessary.
	While level 2 is good when developing a new pipeline, additional details useful for debugging are 
	provided by level 3.
	
- Detailed checking of flowdef
```
checking if required columns are present...
checking if resources columns are present...
checking if dependency column has valid names...
checking if submission column has valid names...
checking for missing rows in def...
checking for extra rows in def...
checking submission and dependency types...
	jobname	prev.sub_type --> dep_type --> sub_type: relationship
	1: aln1_a	none --> none --> scatter 
	2: aln2_a	scatter --> none --> scatter 
	3: sampe_a	scatter --> serial --> scatter rel: complex one:one
	4: fixrg_a	scatter --> serial --> scatter rel: complex one:one
	5: merge_a	scatter --> gather --> serial rel: many:one
	6: markdup_a	serial --> serial --> serial rel: simple one:one
	7: target_a	serial --> serial --> serial rel: simple one:one
	8: realign_a	serial --> burst --> scatter rel: one:many
	9: baserecalib_a	scatter --> serial --> scatter rel: complex one:one
	10: printreads_a	scatter --> serial --> scatter rel: complex one:one
```
- rerun [0.9.7.9021]: 
	- Previously one could specify a starting point from where a re-run flow
	would initiate execution. Now one may also specify an arbitrary number of 
	of steps to re-run using select and ignore; which may need to run again.
- job killing and submission now sport a progress bar:
	- |============================================================      | 70%
	- This is especially useful flows with thousands of jobs
- Fixed 2 important bugs in moab.sh, lsf.sh template file, where it was missing the -n argument.
- Now the status function has a new argument use_cache. If enabled, it skips 
fetching statuses of previously completed jobs; this really speeding things up.
	- Also we have added a progress bar to show the status of this summarization.

- Several detailed changes to the documentation.

flowr 0.9.7 (apples)
----------------------------------------------
> 2015-08-22 (0.9.7.10)

- This release adds and changes functionality of several functions. 
- A new function run(), creates and submits a pipeline. Specifically it follows the following steps:
	- One supplies the name of the pipeline, which is used to fetch the pipeline using:
		`fetch_pipe()`
	- create flowmat by running a function called `mypipeline()`, `mypipeline` is the name of the pipeline.
	- load configuration file, with paths to tools etc using `load_conf()`
	- fetch the flow definition using `fetch_pipe`, and load it using `as.flowdef()`
	- Further, create a flow object using `to_flow()`
	- Finally, submit to the cluster, submit_flow()
- `kill()`: now a S3 functions, and operates on both a flow object
   and flow_wd folder
- `check()`: Now works on flowdef and flowmat
- as.flowmat(), as.flowdef(): easy ways to fetch and check these tables
- `fetch()` along with `fetch_pipes()` and `fetch_conf()` simplify finding files
- Reduce function overload, moving several functions a separate `params` pkg
	- moved `read_sheet()`, `write_sheet()`
	- moved `get_opts()`, `set_opts()`
	- moved `.load_conf()` `load_conf()`
	- Here is a link to [params](https://github.com/sahilseth/params) package
	- kable function is now a part of params, that removes the dependency to knitr package
- plot_flow: supports flowdef





flowr 0.9.6.13
----------------------------------------------
- [0.9.6.13] 2015-07-23
	- Using PBS/LSF script instead of one line commands
	- Format now a script, parsed using whisker
	- to_flowmat() and to_flowdef() introduced; creating of these tables simplified
	- Refactored code for plot_flow, handles multiple dependencies beautifully
- [0.9.6.10] 2015-07-08
	- add to_flow()
	- kill_flow()
	- supports moab
	- burst as dependency type
	- node can be character
	- final job names changed to: sprintf("%s_%s-%s", basename(fobj@flow_path), jobj@jobname, i)
	- Using PBS/LSF script instead of one line commands
	- Format now a script, parsed using whisker

flowr 0.9.6.7
----------------------------------------------
> 2015-04-07

- add read_sample_sheet
- clean package dependencies
- [0.9.6.6]
	- switch cat to messages
	- added to_flow
- [0.9.6.5]
	- Added more examples
	- Update Vignette with more examples
	- squash issues in DESCRIPTION
- [0.9.6.1]
	- Added some vignettes for a simple and complex example
	- satiate CRAN checks

flowr 0.9.5.1
----------------------------------------------
> 2015-03-16

- Added several new functions kill_flow(), which kills the flow.
- Added experimental rerun_flow which still needs some work
- updated documentation to satiate R CMD check
- now job() checks for a dependency_type is previous job is provided
- Added some slides inst
- Integrated documentation with Sphnix
- submit_flow, now generates flow_details.txt, flow.pdf,
flow_status.txt
- status() updates: flow_details.txt and flow_status.txt
- save flow.rda files with all details on the flow
- update flow details with job exit status, to be used in rerun

**TODO**

- add more examples
- update and cleanup documentation
- integrate with sqlite with status
- add example pipelines
- assign UUID to each job/flow. and use that to track them.
- Work on test_queue()
- Add recipes

flowr 0.85
----------------------------------------------
> 2014-12-05

- get\_flow_details():
 - Attach a job_number
 - Attach a flow level unique ID
- update flow details with exited jobs
- Create trigger upon start
- add started/pending column in details and summary
- plot_flow():
  make plot type 1 as default
- get_flow_status():
  -exit_status: reports number of exited jobs (not sum of exit codes)

<script src = "https://raw.githubusercontent.com/sahilseth/flowr/master/vignettes/files/googl.js"></script>

Versioning:

Loosely, following [fruits A-Z](https://www.wcrf-uk.org/eat-move-learn/learn-it/a-z-fruit-vegetables#E).
