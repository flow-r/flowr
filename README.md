[![Build Status](https://travis-ci.org/sahilseth/flow.png)](https://travis-ci.org/sahilseth/flow)

flow
======

R pacakge to design (and submit) complex workflows, such that optimization/parallelization is easy.

Here is a shiny app, [flow_creator](https://sseth.shinyapps.io/flow_creator/) for you start building your `shiny` flow.
[`guest; guestpass`]

The process of submitting jobs uses the `dependency` feature of submitting jobs to a computing cluster.
This lets the user concentrate more on the type of analysis than its implmentation. Also the pipeline becomes really portable across platforms and institutions.

Just get me started...

```
install.packages(devtools)
devtools:::install_github("sahilseth/flow")
require(flow)
qobj <- queue(type="lsf", queue="normal")
jobj1 <- job()
jobj2 <- job()
fobj <- flow(jobs = list(jobj1, jobj2)))
```

As of now we have tested this on the following clusters:

Platform|submission commnd|Works?|queue type (name used in [flow](https://github.com/sahilseth/flow))
|-:|-:|-:|
LSF HPC 7|bsub|Not tested|lsf
LSF 9.1|bsub|Yes|lsf
Torque|qsub|Yes|torque
SGE|qsub|Not implemented|


There are several [job scheduling](http://en.wikipedia.org/wiki/Job_scheduler) systems available and we try to support the major players. Adding support is quite easy if we have access to them (otherwise not). Your favoroite not in the list? Send a [message](mailto:sahil.seth@me.com)

- [PBS wiki](http://en.wikipedia.org/wiki/Portable_Batch_System):
	- [Torque wiki](http://en.wikipedia.org/wiki/TORQUE_Resource_Manager)
	- MD Anderson, [University of Houston](http://www.rcc.uh.edu/hpc-docs/49-using-torque-to-submit-and-monitor-jobs.html)
- [LSF](http://en.wikipedia.org/wiki/Platform_LSF):
	- Harvard Medicla School uses: [LSF HPC 7](https://wiki.med.harvard.edu/Orchestra/IntroductionToLSF).
	- Also Used at [Broad](https://www.broadinstitute.org/gatk/guide/article?id=1311)
- [SGE](http://en.wikipedia.org/wiki/Sun_Grid_Engine)
	- A tutorial for [Sun Grid Engine](https://sites.google.com/site/anshulkundaje/inotes/programming/clustersubmit/sun-grid-engine), you may also see another at [JHSPH](http://www.biostat.jhsph.edu/bit/cluster-usage.html). Dependecy info [here](https://wiki.duke.edu/display/SCSC/SGE+Job+Dependencies)

<!---
```
qsub -hold_jid
## <comma separated list of job-ids, can also be a job id pattern such as 2722*> : will start the current job/job -array only after completion of all jobs in the comma separated list
## SGE arrary
qsub -t 1-10:1 -N arrayJob \ !
./my-arrayJobScript.sh
## LSF job arrarys:
Syntax
The bsub syntax used to create a job array follows:
bsub -J "arrayName[indexList, ...]" myJob 
Where:
-J "arrayName[indexList, ...]"
Names and creates the job array. The square brackets, [ ], around indexList must be entered exactly as shown and the job array name specification must be enclosed in quotes. Commas (,) are used to separate multiple indexList entries. The maximum length of this specification is 255 characters.
arrayName
User specified string used to identify the job array. Valid values are any combination of the following characters:
a-z | A-Z | 0-9 | . | - | _ 
indexList = start[-end[:step]]
Specifies the size and dimension of the job array, where:
start
Specifies the start of a range of indices. Can also be used to specify an individual index. Valid values are unique positive integers. For example, [1-5] and [1, 2, 3, 4, 5] specify 5 jobs with indices 1 through 5.
end
Specifies the end of a range of indices. Valid values are unique positive integers.
step
Specifies the value to increment the indices in a range. Indices begin at start, increment by the value of step, and do not increment past the value of end. The default value is 1. Valid values are positive integers. For example, [1-10:2] specifies a range of 1-10 with step value 2 creating indices 1, 3, 5, 7, and 9.
After the job array is created (submitted), individual jobs are referenced using the job array name or job ID and an index value. For example, both of the following series of job array statements refer to jobs submitted from a job array named myArray which is made up of 1000 jobs and has a job ID of 123:
myArray[1], myArray[2], myArray[3], ..., myArray[1000]
123[1], 123[2], 123[3], ..., 123[1000] 
http://ls11-www.informatik.uni-dortmund.de/people/hermes/manuals/LSF/users.pdf
lsinfo
## more info about lsf at:
```

### ## http://wiki.hpc.ufl.edu/doc/Torque_Job_Arrays
```
## run jobs 1 to 5
-t x-y
## a few of them:
qsub -t  4,8,15,16,23,42
## do 5 at a time
-t 1-100%5
qstat -t 40-80
```

Deleting job arrays and tasks

To delete some tasks use the following command format

```
qdel -t 4-8 479389284[]
```
Make sure to use the [] brackets after a job id.

Using Array index inside jobs

```
file=ls *.txt | head -n $PBS_ARRAYID | tail -n 1
myscript -in $file
https://ncisf.org/training/tutorials/job_arrays
For some systems it might be: #PBS -J 1-10
```	
A Very nice guide to [qsub](http://docs.adaptivecomputing.com/torque/4-1-4/Content/topics/commands/qsub.htm)
```
qsub -W depend=afterok:jobid,ok:jobid
qsub -W afterokarray:arrayid
```
[qstat](http://docs.adaptivecomputing.com/torque/4-1-4/Content/topics/commands/qstat.htm)

#PBS
[https://ncisf.org/training/tutorials/job_arrays](https://ncisf.org/training/tutorials/job_arrays)
Guide for PBS Pro [here](http://www.pbsworks.com/Product.aspx?id=1&AspxAutoDetectCookieSupport=1)
```
Uses -J for dependencies
#PBS -J 1-10
PBS_ARRAY_INDEX
```
<!---




















		
		
