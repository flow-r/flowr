---
output: html_document
packagedocs:
    toc: true
navpills: |
  <li><a href='docs.html'>Docs</a></li>
  <li><a href='rd.html'>Package Ref</a></li>
  <li class="active"><a href='news.html'>News</a></li>
  <li><a href='https://github.com/sahilseth/flowr'>Github <i class='fa fa-github'></i></a></li>
---

flowr latest
----------------------------------------------
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
	Where the level may be 0, 1 or 2. Level 2 is recommended when developing a new pipeline.
	Level 1 is good for most purposes, which level 0 is almost silent producing messages 
	only when neccessary. Would be great, getting feedback on this.

	



flowr 0.9.7.10 2015-09-02
----------------------------------------------
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





flowr 0.9.6.13 2015-09-02
----------------------------------------------
- Using PBS/LSF script instead of one line commands
- Format now a script, parsed using whisker
- to_flowmat() and to_flowdef() introduced; creating of these tables simplified
- Refactored code for plot_flow, handles multiple dependencies beautifully


flowr 0.9.6.10
----------------------------------------------
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
- add read_sample_sheet
- clean package dependencies


flowr 0.9.6.6
----------------------------------------------
- switch cat to messages
- added to_flow

flowr 0.9.6.5
----------------------------------------------
- Added more examples
- Update Vignette with more examples
- squash issues in DESCRIPTION

flowr 0.9.6.1
----------------------------------------------
- Added some vignettes for a simple and comples example
- satiate CRAN checks

flowr 0.9.5.1
----------------------------------------------

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

flow 0.85
----------------------------------------------

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
