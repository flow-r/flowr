Flow constructor

    flow(jobs = list(new("job")), name = "newflow", desc = "my_super_flow",
      mode = c("scheduler", "trigger", "R"), flow_base_path = "~/flows",
      trigger_path = "", flow_path = "", status = "")

A list of jobs to be included in this flow

Name of the flow. Defaults to Used in to name the working directories.

Description of the flow This is used to name folders (when submitting
jobs, see ). It is good practice to avoid spaces and other special
characters. An underscore ’\_’ seems like a good word separator.
Defaults to ’my\_super\_flow’. We usually use this to put sample names
of the data.

Mode of submission of the flow.

The base path of all the flows you would submit. Defaults to . Best
practice to ignore it.

Defaults to . Best practice to ignore it.

Not used at this time
