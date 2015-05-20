Generate 100 commands each for sleep make div
---------------------------------------------

.. code:: r

    ## create a vector of sample names
    samp = sprintf("sample%s", 1:10)

    tmp <- lapply(1:length(samp), function(i){
        ## sleep for a few seconds (100 times)
        cmd_sleep = sprintf("sleep %s", abs(round(rnorm(10)*10, 0)))
        
        ## Create 100 temporary files
        tmp10 = sprintf("tmp%s_%s", i, 1:10)
        cmd_tmp = sprintf("head -c 100000 /dev/urandom > %s", tmp10)
        
        ## Merge them according to samples, 10 each
        cmd_merge <- sprintf("cat %s > merge%s", paste(tmp10, collapse = " "), i)

        ## get the size of merged files
        cmd_size = sprintf("du -sh merge%s", i)
        
        cmd <- c(cmd_sleep, cmd_tmp, cmd_merge, cmd_size)
        jobname <- c(rep("sleep", length(cmd_sleep)),
            rep("tmp", length(cmd_tmp)),
            rep("merge", length(cmd_merge)),
            rep("size", length(cmd_size)))
        ## create a DF
        df = data.frame(samplename = samp[i],
            jobname = jobname, 
            cmd = cmd)
    })

    flow_mat = do.call(rbind, tmp)
    kable(head(flow_mat))

+--------------+-----------+------------+
| samplename   | jobname   | cmd        |
+==============+===========+============+
| sample1      | sleep     | sleep 12   |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 6    |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 14   |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 5    |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 2    |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 11   |
+--------------+-----------+------------+

Make the flow definition
========================

Generate a skeleton flow definition
-----------------------------------

.. code:: r

    def = sample_flow_def(jobnames = unique(flow_mat$jobname))

::

    #> Creating a skeleton flow_def

.. code:: r

    kable(def)

+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| jobname   | prev\_jobs   | dep\_type   | sub\_type   | queue    | memory\_reserved   | walltime   | cpu\_reserved   |
+===========+==============+=============+=============+==========+====================+============+=================+
| sleep     | none         | none        | scatter     | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| tmp       | sleep        | serial      | scatter     | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| merge     | tmp          | serial      | scatter     | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+
| size      | merge        | serial      | scatter     | medium   | 163185             | 23:00      | 1               |
+-----------+--------------+-------------+-------------+----------+--------------------+------------+-----------------+

Change the dependency type for merge step into gather
-----------------------------------------------------

It might be easier to do such, by hand

.. code:: r

    def[def[, 'jobname'] == "merge","dep_type"] = "gather"
    def[def[, 'jobname'] == "merge","sub_type"] = "serial"
    def[def[, 'jobname'] == "size","sub_type"] = "serial"
    kable(def)

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

Write both into example data
============================

.. code:: r

    write.table(flow_mat, file = "inst/extdata/example1_flow_mat.txt", 
        row.names = FALSE, quote = FALSE, sep = "\t")
    write.table(def, file = "inst/extdata/example1_flow_def.txt", 
        row.names = FALSE, quote = FALSE, sep = "\t")
