  ------------- -----------------
  flow {flow}   R Documentation
  ------------- -----------------

Flow constructor
----------------

### Description

Flow constructor

### Usage

    flow(jobs = list(new("job")), name = "newflow", desc = "my_super_flow",
      mode = c("scheduler", "trigger", "R"), flow_base_path = "~/flows",
      trigger_path = "", flow_path = "", status = "")

### Arguments

+--------------------------------------+--------------------------------------+
| `jobs`                               | `list` A list of jobs to be included |
|                                      | in this flow                         |
+--------------------------------------+--------------------------------------+
| `name`                               | `character` Name of the flow.        |
|                                      | Defaults to `'newname'` Used in      |
|                                      | submit\_flow to name the working     |
|                                      | directories.                         |
+--------------------------------------+--------------------------------------+
| `desc`                               | `character` Description of the flow  |
|                                      | This is used to name folders (when   |
|                                      | submitting jobs, see submit\_flow).  |
|                                      | It is good practice to avoid spaces  |
|                                      | and other special characters. An     |
|                                      | underscore '\_' seems like a good    |
|                                      | word separator. Defaults to          |
|                                      | 'my\_super\_flow'. We usually use    |
|                                      | this to put sample names of the      |
|                                      | data.                                |
+--------------------------------------+--------------------------------------+
| `mode`                               | `character` Mode of submission of    |
|                                      | the flow.                            |
+--------------------------------------+--------------------------------------+
| `flow_base_path`                     | The base path of all the flows you   |
|                                      | would submit. Defaults to `~/flows`. |
|                                      | Best practice to ignore it.          |
+--------------------------------------+--------------------------------------+
| `trigger_path`                       | `character` Defaults to              |
|                                      | `~/flows/trigger`. Best practice to  |
|                                      | ignore it.                           |
+--------------------------------------+--------------------------------------+
| `flow_path`                          | `character`                          |
+--------------------------------------+--------------------------------------+
| `status`                             | `character` Not used at this time    |
+--------------------------------------+--------------------------------------+

* * * * *

[Package *flow* version 0.7 ]
