.. rflow documentation master file. Adapted from other sources.

flow (R-flow)
=============

flow
======

R pacakge to design (and submit) complex workflows, such that optimization/parallelization is easy.

Here is a shiny app, [flow_creator](https://sseth.shinyapps.io/flow_creator/) for you start building your `shiny` flow.
[`guest; guestpass`]

The process of submitting jobs uses the `dependency` feature of submitting jobs to a computing cluster.
This lets the user concentrate more on the type of analysis than its implmentation. Also the pipeline becomes really portable across platforms and institutions.

Just get me started...

```
install.packages('devtools')
devtools:::install_github("sahilseth/flow")
require(flow)
qobj <- queue(type="lsf", queue="normal")
jobj1 <- job()
jobj2 <- job()
fobj <- flow(jobs = list(jobj1, jobj2)))
```

As of now we have tested this on the following clusters:

|Platform|submission commnd|Works?|queue type *|
|:---|:---:|:---:|:---:|
|LSF HPC 7|bsub|Not tested|lsf
|LSF 9.1|bsub|Yes|lsf
|Torque|qsub|Yes|torque
|SGE|qsub|Not implemented|

*queue short-name used in [flow](https://github.com/sahilseth/flow)

There are several [job scheduling](http://en.wikipedia.org/wiki/Job_scheduler) systems available and we try to support the major players. Adding support is quite easy if we have access to them (otherwise not). Your favoroite not in the list? Send a [message](mailto:sahil.seth@me.com)

- [PBS wiki](http://en.wikipedia.org/wiki/Portable_Batch_System):
	- [Torque wiki](http://en.wikipedia.org/wiki/TORQUE_Resource_Manager)
	- MD Anderson, [University of Houston](http://www.rcc.uh.edu/hpc-docs/49-using-torque-to-submit-and-monitor-jobs.html)
- [LSF](http://en.wikipedia.org/wiki/Platform_LSF):
	- Harvard Medicla School uses: [LSF HPC 7](https://wiki.med.harvard.edu/Orchestra/IntroductionToLSF).
	- Also Used at [Broad](https://www.broadinstitute.org/gatk/guide/article?id=1311)
- [SGE](http://en.wikipedia.org/wiki/Sun_Grid_Engine)
	- A tutorial for [Sun Grid Engine](https://sites.google.com/site/anshulkundaje/inotes/programming/clustersubmit/sun-grid-engine), you may also see another at [JHSPH](http://www.biostat.jhsph.edu/bit/cluster-usage.html). Dependecy info [here](https://wiki.duke.edu/display/SCSC/SGE+Job+Dependencies)

[http://en.wikipedia.org/wiki/Comparison\_of\_cluster\_software](http://en.wikipedia.org/wiki/Comparison\_of\_cluster\_software)

Contents:

.. toctree::
   :maxdepth: 2

   contents/introduction
   contents/installation
   contents/testing
   contents/configuration
   contents/parallel
   contents/pipelines
   contents/outputs
   contents/code
   contents/sequencer
   contents/internals
   contents/presentations


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

