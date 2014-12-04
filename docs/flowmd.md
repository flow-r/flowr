
Flow constructor
================

Usage
-----

```
flow(jobs = list(new("job")), name = "newflow", desc = "my_super_flow", 
	mode = c("scheduler", "trigger", "R"), 
	flow_base_path = "~/flows", trigger_path = "", flow_path = "", status = "")
```
Arguments
---------

**jobs**

`list` A list of jobs to be included in this flow

**name**:

   `character` Name of the flow. Defaults to `'newname'` Used in
    [submit\_flow](submit_flow-flow-method.html) to name the working
    directories.

desc
:   `character` Description of the flow This is used to name folders
    (when submitting jobs, see
    [submit\_flow](submit_flow-flow-method.html)). It is good practice
    to avoid spaces and other special characters. An underscore '\_'
    seems like a good word separator. Defaults to 'my\_super\_flow'. We
    usually use this to put sample names of the data.
mode
:   `character` Mode of submission of the flow.
flow\_base\_path
:   The base path of all the flows you would submit. Defaults to
    `~/flows`. Best practice to ignore it.
trigger\_path
:   `character` Defaults to `~/flows/trigger`. Best practice to ignore
    it.
flow\_path
:   `character`
status
:   `character` Not used at this time

Flow constructor
----------------

Description
-----------

Flow constructor

[Back to top](#)

Built by [staticdocs](https://github.com/hadley/staticdocs). Styled with
[bootstrap](http://twitter.github.com/bootstrap).
