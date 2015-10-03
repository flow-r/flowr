---
title: "flowr"
subtitle: "Streamlining Workflows"
author: Sahil Seth
date: "`r Sys.Date()`"
output: html_document
packagedocs:
    toc: true
navpills: |
  <li><a href='docs.html'>Overview</a></li>
  <li><a href='install.html'>Install</a></li>
  <li><a href='tutorial.html'>Tutorial</a></li>
  <li><a href='rd.html'>Help</a></li>
  <li class="active"><a href='news.html'>News</a></li>
  <li><a href='https://github.com/sahilseth/flowr'>Github <i class='fa fa-github'></i></a></li>
---

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
	only when neccessary.
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
	would initiate execution. Now one may also specify an arbitary number of 
	of steps to re-run using select and ignore; which may need to run again.
- job killing and submission now sport a progress bar:
	- |============================================================      | 70%
	- This is especially useful flows with thousands of jobs
- Fixed 2 important bugs in moab.sh, lsf.sh template file, where it was missing the -n argument.
- Now the status function has a new argument use_cache. If enabled, it skips 
fetching statuses of previously completed jobs; this really speeding things up.
	- Also we have added a progress bar to show the status of this summarization.

- Several detailed changes to the documentation.

flowr 0.9.7.10 (apples)
----------------------------------------------
> 2015-08-22

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
- Reduce function overload, moving several functions a seperate `params` pkg
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
	- Added some vignettes for a simple and comples example
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
- Add recipies

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
