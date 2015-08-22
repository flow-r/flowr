---
output: html_document
---



flowr 0.9.7 2015-08-22
----------------------------------------------
- run(): runs a complete pipeline:
	- create flowmat, my_pipeline()
	- load conf, load_conf()
	- load flowdef, as.flowdef()
	- created a flow object, to_flow()
	- Submit to the cluster, submit_flow()
- kill(): now a S3 functions, and operates on both a flow object
   and flow_path folder
- check(): Now works on flowdef and flowmat
- as.flowmat(), as.flowdef(): easy ways to fetch and check these tables
- fetch() along with fetch_pipes() and fetch_conf() simplifies finding files
- dependecies: brought back knitr dependency
- Reduce function overload, moving several functions a seperate `params` pkg
	- moved read_sheet(), write_sheet()
	- moved get_opts(), set_opts()
	- moved .load_conf() load_conf()
	- Here is a link to [params](https://github.com/sahilseth/params) package
	- includes kable as part of params
- [0.9.7.10] Now supports new column of status (completed/processing/errored...)
|                | total| started| completed| exit_status|status     |
|:---------------|-----:|-------:|---------:|-----------:|:----------|
|001.alnCmd1     |   109|     109|       109|           0|completed  |
|007.markCmd     |     3|       3|         0|           0|processing |





flowr 0.9.6.13
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
