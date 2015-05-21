Styles
------

.. code:: r

    exdata = file.path(system.file(package = "flowr"), "extdata")
    flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))

::

    #> Using 'samplename'' as id_column

.. code:: r

    flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"))

::

    #> Using 'jobname'' as id_column

Style 1
~~~~~~~

Ingredient 1: Flow Definition format
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each row of this table translate to a call to
(`job <http://docs.flowr.space/build/html/rd/topics/job.html>`__ or)
`queue <http://docs.flowr.space/build/html/rd/topics/queue.html>`__
function.

-  jobname: is passed as ``name`` argument to job().
-  prev\_jobs: passed as ``previous_job`` argument to job().
-  dep\_type: passed as ``dependency_type`` argument to job(). Possible
   values: gather, serial
-  sub\_type: passed as ``submission_type`` argument to job().
-  queue: name of the queue to be used for this particular job. Since
   each jobs can be submitted to a different queue, this makes your flow
   very flexible
-  memory\_reserved: Refer to your system admin guide on what values
   should go here. Some examples: 160000, 16g etc representing a 16GB
   reservation of RAM
-  walltime: How long would this job run. Again refer to your HPCC
   guide. Example: 24:00, 24:00:00
-  cpu\_reserved: Amount of CPU reserved.

Its best to have this as a tab seperated file (with no row.names).

.. code:: r

    kable(head(flow_def))

+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| jobname   | prev\_jobs   | dep\_type   | sub\_type   | queue    | memory\_reserved   | walltime   | cpu\_reserved   |
+===========+==============+=============+=============+==========+====================+============+=================+
| sleep     | none         | none        | scatter     | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| tmp       | sleep        | serial      | scatter     | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| merge     | tmp          | gather      | serial      | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| size      | merge        | serial      | serial      | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+

Ingredient 2: flow\_mat: A table with all the commands to run:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  samplename: The idea is to group commands based on this column and
   submit individual flows for each sample

   -  This column is not really used in subsequent steps. So one can put
      a dummy value like sample1, for all the rows if no subsetting is
      desired

-  jobname: This corresponds to the name of the job. This should match
   perfectly with the jobname column in flow\_def
-  cmd: the command to run

Its best to have this as a tab seperated file (with no row.names)

.. code:: r

    kable(subset(flow_mat, samplename == "sample1"))

+--------------+-----------+-----------------------------------------+
| samplename   | jobname   | cmd                                     |
+==============+===========+=========================================+
| sample1      | sleep     | sleep 6                                 |
+--------------+-----------+-----------------------------------------+
| sample1      | sleep     | sleep 16                                |
+--------------+-----------+-----------------------------------------+
| sample1      | sleep     | sleep 8                                 |
+--------------+-----------+-----------------------------------------+
| sample1      | tmp       | head -c 100000 /dev/urandom > tmp1\_1   |
+--------------+-----------+-----------------------------------------+
| sample1      | tmp       | head -c 100000 /dev/urandom > tmp1\_2   |
+--------------+-----------+-----------------------------------------+
| sample1      | tmp       | head -c 100000 /dev/urandom > tmp1\_3   |
+--------------+-----------+-----------------------------------------+
| sample1      | merge     | cat tmp1\_1 tmp1\_2 tmp1\_3 > merge1    |
+--------------+-----------+-----------------------------------------+
| sample1      | size      | du -sh merge1                           |
+--------------+-----------+-----------------------------------------+

Style 2
~~~~~~~

This style may be more suited for people who like to explore more
advanced usage and like to code in R. Also this one find this much
faster if jobs and their relationships changes a lot.

Here instead of seperating cmds and definitions one defines them step by
step incrementally.

-  Use: queue(), to define the computing cluster being used
-  Use: multiple calls job()
-  Use: flow() to stich the jobs into a flow.

Currently we support LSF, Torque and SGE. Let us use LSF for this
example.

.. code:: r

    qobj <- queue(type = "lsf", queue = "normal", verbose = FALSE)

Let us stitch a simple flow with three jobs, which are submitted one
after the other.

.. code:: r

    job1 <- job(name = "myjob1", cmds = "sleep1", q_obj = qobj)
    job2 <- job(name = "myjob2", cmds = "sleep2", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
    job3 <- job(name = "myjob3", cmds = "sleep3", q_obj = qobj, previous_job = "myjob1", dependency_type = "serial")
    fobj <- flow(name = "myflow", jobs = list(job1, job2, job3), desc="description")
    plot_flow(fobj)

.. figure:: figure/plot_simpleflow-1.pdf
   :alt: 

The above translates to a flow definition which looks like this:

.. code:: r

    dat <- flowr:::.create_jobs_mat(fobj)
    knitr:::kable(dat)

+----------+-----------+--------------+-------------+-------------+-----------------+---------+---------+---------------+
|          | jobname   | prev\_jobs   | dep\_type   | sub\_type   | cpu\_reserved   | nodes   | jobid   | prev\_jobid   |
+==========+===========+==============+=============+=============+=================+=========+=========+===============+
| myjob1   | myjob1    |              | none        | scatter     | 1               | 1       | 1       | NA            |
+----------+-----------+--------------+-------------+-------------+-----------------+---------+---------+---------------+
| myjob2   | myjob2    | myjob1       | serial      | scatter     | 1               | 1       | 2       | 1             |
+----------+-----------+--------------+-------------+-------------+-----------------+---------+---------+---------------+
| myjob3   | myjob3    | myjob1       | serial      | scatter     | 1               | 1       | 3       | 1             |
+----------+-----------+--------------+-------------+-------------+-----------------+---------+---------+---------------+

Submission types
----------------

-  scatter: submit all commands as parallel independent jobs
-  serial: run these commands sequentuially one after the other

Dependency types
----------------

-  none: independent job
-  serial: *one to one* relationship with previous job
-  gather: *many to one* wait for **all** commands in previous job to
   finish then start current
-  burst: *one to many* wait for one jobs and start several when it
   completes

Relationships
-------------

Serial: one to one relationship
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  All commads in 'job1' are submitted, and those is 'jobs2' *wait* for
   those in 'job1' to complete.
-  Commands in 'job2' are serially dependent on 'job1'
-  Both jobs are submitted as parallel (*scatter*), i.e. there is not
   **intra** dependency.
-  so previous job submission: ``scatter``, and current job's dependency
   type ``serial``

.. code:: r

    cmds = rep("sleep 5", 10)
    jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
    jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
                 dependency_type = "serial", previous_job = "job1")
    fobj <- flow(jobs = list(jobj1, jobj2))
    plot_flow(fobj)

.. figure:: figure/unnamed-chunk-6-1.pdf
   :alt: 

Gather: many to one relationship
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  makes sense when previous job had many commands running in parallel
   and current job would wait for all
-  so previous job submission: ``scatter``, and current job's dependency
   type ``gather``

.. code:: r

    jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "scatter", name = "job1")
    jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
                 dependency_type = "gather", previous_job = "job1")
    fobj <- flow(jobs = list(jobj1, jobj2))
    plot_flow(fobj)

.. figure:: figure/unnamed-chunk-7-1.pdf
   :alt: 

Burst: one to many relationship
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  makes sense when previous job had one command current job would split
   and submit several jobs in parallel
-  so previous job submission\_type: ``serial``, and current job's
   dependency type ``burst``, with a submission type: ``scatter``

.. code:: r

    jobj1 <- job(q_obj=qobj, cmd = cmds, submission_type = "serial", name = "job1")
    jobj2 <- job(q_obj=qobj, name = "job2", cmd = cmds, submission_type = "scatter", 
                 dependency_type = "burst", previous_job = "job1")
    fobj <- flow(jobs = list(jobj1, jobj2))
    plot_flow(fobj)

.. figure:: figure/unnamed-chunk-8-1.pdf
   :alt: 

HPCC submission formats
-----------------------

**LSF**

.. code:: r

    queue(type = "lsf")@format

::

    #> Setting default time to: 72:00. If this is more than queue max (/improper format), job will fail. You may change this in job()
    #> 
    #> Setting default memory to: 10000. If this is more than queue max (/improper format), job will fail.

::

    #> [1] "${SUBMIT_EXE} -q ${QUEUE} -J ${JOBNAME} -o ${STDOUT} -e ${STDERR} -n ${CPU} -cwd ${CWD} -M ${MEMORY} -R span[ptile=${CPU}] -W ${WALLTIME} -r ${EXTRA_OPTS} ${DEPENDENCY} '<' ${CMD} "

**torque**

.. code:: r

    queue(type = "torque")@format

::

    #> Setting default time to: 72:00:00. If this is more than queue max (/improper format), job will fail. You may change this in job()
    #> 
    #> Setting default memory to: 10g. If this is more than queue max (/improper format), job will fail.

::

    #> [1] "${SUBMIT_EXE} -N ${JOBNAME} -q ${QUEUE} -l nodes=${NODES}:ppn=${CPU} -l walltime=${WALLTIME} -l mem=${MEMORY} -S /bin/bash -d ${CWD} -V -o ${STDOUT} -m ae -M ${EMAIL} -j oe -r y -V ${EXTRA_OPTS} ${CMD} ${DEPENDENCY}"

My HPCC is not supported, how to make it work? send a message to:
sahil.seth [at] me.com
