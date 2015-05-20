Get me started
--------------

.. code:: r

    install.packages('devtools')
    devtools::install_github("sahilseth/flowr")

Run a setup function which copies 'flowr' Rscript to subsetquent steps
easier. More on this `here <https://github.com/sahilseth/rfun>`__.

.. code:: r

    library(flowr)
    setup()

::

    ## Consider adding ~/bin to your PATH variable in .bashrc.
    ## export PATH=$PATH:$HOME/bin
    ## You may now use all R functions using 'flowr' from shell.

Create a flow using example data
================================

.. code:: r

    exdata = file.path(system.file(package = "flowr"), "extdata")
    flow_mat = read_sheet(file.path(exdata, "example1_flow_mat.txt"))

::

    ## Using 'samplename'' as id_column

.. code:: r

    flow_def = read_sheet(file.path(exdata, "example1_flow_def.txt"))

::

    ## Using 'jobname'' as id_column

.. code:: r

    flow_mat = subset(flow_mat, samplename == "sample1")

    fobj <- to_flow(x = flow_mat, def = flow_def, 
        flowname = "example1",
        platform = "lsf")

::

    ## Using description default: type1
    ## Using flow_base_path default: ~/flowr
    ## ....

Ingredient 1: Commands to run (flow\_mat)
=========================================

.. code:: r

    kable(head(flow_mat))

+--------------+-----------+------------+
| samplename   | jobname   | cmd        |
+==============+===========+============+
| sample1      | sleep     | sleep 17   |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 7    |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 21   |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 1    |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 4    |
+--------------+-----------+------------+
| sample1      | sleep     | sleep 8    |
+--------------+-----------+------------+

Ingredient 2: Flow Definition (flow\_def)
=========================================

.. code:: r

    kable(flow_def)

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

**The above table basically translates to**:

-  ``sleep``: Run all 10 sleep jobs for given sample
-  ``tmp``: Create 10 temporary files, after sleep jobs are complete

   -  dependency is serial, tmp jobs does not wait for all sleep jobs to
      complete.
   -  This is a one-to-one relationship

-  ``merge``: When all ``tmp`` are complete, merge them
-  ``size``: get their size when merge is complete

Plot describing the definition
==============================

.. code:: r

    plot_flow(fobj)

.. figure:: figure/plot_example1-1.pdf
   :alt: Flow chart describing process for example 1

   Flow chart describing process for example 1

Dry run (submit)
================

.. code:: r

    submit_flow(fobj)

::

    Test Successful!
    You may check this folder for consistency. Also you may re-run submit with execute=TRUE
     ~/flowr/type1-20150520-15-18-27-5mSd32G0

Submit to the cluster
=====================

.. code:: r

    submit_flow(fobj, execute = TRUE)

::

    Flow has been submitted. Track it from terminal using:
    flowr::status(x="~/flowr/type1-20150520-15-18-46-sySOzZnE")
    OR
    flowr status x=~/flowr/type1-20150520-15-18-46-sySOzZnE

Check the status
================

::

    flowr status x=~/flowr/type1-20150520-15-18-46-sySOzZnE

::

    Loading required package: shape
    Flowr: streamlining workflows
    Showing status of: /rsrch2/iacs/iacs_dep/sseth/flowr/type1-20150520-15-18-46-sySOzZnE


    |          | total| started| completed| exit_status|
    |:---------|-----:|-------:|---------:|-----------:|
    |001.sleep |    10|      10|        10|           0|
    |002.tmp   |    10|      10|        10|           0|
    |003.merge |     1|       1|         1|           0|
    |004.size  |     1|       1|         1|           0|
