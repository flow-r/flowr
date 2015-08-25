.. flowr documentation master file, created by
   sphinx-quickstart on Mon Mar 23 11:44:18 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

|Build Status| |DOI|

flowr: Streamline your workflows
=================================

This framework allows you to design and implement complex pipelines, and deploy them on your institution's computing cluster. This has been built keeping in mind the needs of bioinformatics workflows. However, it is easily extendable to any field where a series of steps (shell commands) are to be executed in a (work)flow.

Why bother?
-------------
- Do you have a series of steps which **can** spawn across the whole computing cluster, but currently are not effectively utilizing those resources?
- Example: 
	- Think of a sample with multiple files (`alignment <http://en.wikipedia.org/wiki/Sequence_alignment>`_, with tens of `fastq <http://en.wikipedia.org/wiki/FASTQ_format>`_ files)
	- Each file processed using multiple cores
	- Say 50 files using 10 cores each --> 160 cores across 10 machines.
- Reproducible, with cleanly structured execution logs
- Track and re-run flows
- Lean and Portable, with easy installation
- Supports multiple platforms (torque, lsf, sge, ...)
	
.. Say the next step needs to wait for all these 10 jobs to complete, or say 

.. There are a few built-in pipelines which start from fastq files and do alignment and variant calling.


A few lines, to get started:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: r

	install.packages(devtools)
	devtools::install_github("sahilseth/flowr")
	## OR
	install.packages("flowr") ## coming up soon...
	library(flowr) ## load the library
	setup() ## copy flowr bash script
	run('sleep', execute=TRUE,  platform='moab')

- Here is a shiny app, `flow_creator <https://sseth.shinyapps.io/flow_creator>`_ which helps you build a flow.
- A few `slides <http://sahilseth.github.io/slides/flowrintro>`_ providing a quick overview.

.. The process of submitting jobs uses the `dependency` feature of submitting jobs to a computing cluster.
.. This lets the user concentrate more on the type of analysis than its implmentation. Also the pipeline becomes really portable across platforms and computing clusters.




Contents:
-----------------------

.. toctree::
	:maxdepth: 1
	
	rd/vignettes/quick-start
	
.. toctree::
	:glob:
	:maxdepth: 2
	
	build
	pipelines
	topics
	
.. toctree::
	hpcc-support
	faqs

Indices and tables
---------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

.. links in this page

.. _flow_creator: https://sseth.shinyapps.io/flow_creator
.. |Build Status| image:: https://travis-ci.org/sahilseth/flowr.png
   :target: https://travis-ci.org/sahilseth/flowr
.. |DOI| image:: https://zenodo.org/badge/11075/sahilseth/flowr.svg
   :target: http://dx.doi.org/10.5281/zenodo.16170

