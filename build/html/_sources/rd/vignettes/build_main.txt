Building pipelines
------------------

A working example
~~~~~~~~~~~~~~~~~

.. code:: r

    ## 6 jobs with 10 each
    cmds = sprintf("sleep %s", round(rchisq(6*10, df = 1), 1))
    nms = sprintf("myjob%s", rep(1:6, each = 10))
    mat = data.frame(cbind(jobname = nms, cmd = cmds), stringsAsFactors = FALSE)
    head(mat)

::

    #>   jobname       cmd
    #> 1  myjob1 sleep 0.1
    #> 2  myjob1 sleep 0.4
    #> 3  myjob1 sleep 4.8
    #> 4  myjob1   sleep 0
    #> 5  myjob1 sleep 0.1
    #> 6  myjob1 sleep 0.1
