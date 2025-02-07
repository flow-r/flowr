# How to contribute

Thanks for the support! If you could, please submit all pull requests in `devel` branch. We test things in `devel` and push then to `master` 
after a few tests.


## Adding a new platform

<div class="alert alert-warning" role="alert">
Possible issue: Need to add a new platform
</div>

Adding a new platform involves [a few steps](https://github.com/flow-r/flowr/issues/7), briefly we need to consider the following steps where changes would be necessary.

1. **job submission**: One needs to add a new template for the new platform. Several [examples](https://github.com/flow-r/flowr/tree/master/inst/conf) are available as described in the previous section.
2. **parsing job ids**: flowr keeps a log of all submitted jobs, and also to pass them along as a dependency to subsequent jobs. This is taken care by the [parse_jobids()](https://github.com/flow-r/flowr/blob/master/R/parse-jobids.R) function. Each job scheduler shows the jobs id, when you submit a job, but each shows it in a slightly different pattern. To accommodate this one can use regular expressions as described in the relevant section of the [flowr config](https://github.com/flow-r/flowr/blob/master/inst/conf/flowr.conf).

3. **render dependency**: After collecting job ids from previous jobs, flowr renders them as a dependency for subsequent
jobs. This is handled by [render_dependency.PLATFORM](https://github.com/flow-r/flowr/blob/master/R/render-dependency.R) functions.
4. **recognize new platform**: Flowr needs to be made aware of the new platform, for this we need to add a new class using the platform name. This is essentially a wrapper around the [job class](https://github.com/flow-r/flowr/blob/master/R/class-def.R)

Essentially this requires us to add a new line like: `setClass("torque", contains = "job")`.


5. **killing jobs**: Just like submission flowr needs to know what command to use to kill jobs. This is defined in detect_kill_cmd function.



There are several [job scheduling](http://en.wikipedia.org/wiki/Job_scheduler) systems
available and we try to support the major players. Adding support is
quite easy if we have access to them. Your favourite not in the list?
re-open this issue, with details on the platform:
[adding platforms](https://github.com/flow-r/flowr/issues/7)



<div class="alert alert-warning" role="alert">
Possible issue: For other issues upload the error shown in the out files to 
[github issues tracker](https://github.com/flow-r/flowr/issues).
</div>
