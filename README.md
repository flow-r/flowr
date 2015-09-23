# flowr


[![Build Status](https://travis-ci.org/sahilseth/flowr.svg?branch=master)](https://travis-ci.org/sahilseth/flowr)
[![cran](http://www.r-pkg.org/badges/version/flowr)](http://cran.rstudio.com/web/packages/flowr/index.html)
[![codecov.io](http://codecov.io/github/sahilseth/flowr/coverage.svg?branch=devel)](http://codecov.io/github/sahilseth/flowr?branch=devel)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/flowr)


This framework allows you to design and implement complex pipelines, and
deploy them on your institution's computing cluster. This has been built
keeping in mind the needs of bioinformatics workflows. However, it is
easily extendable to any field where a series of steps (shell commands)
are to be executed in a (work)flow.

Highlights
----------

- Effectively process a **multi-step pipeline**, spawning it
across the computing cluster
- Example: 
	- A typical case in next-generation sequencing involves processing of tens of
   [fastqs](http://en.wikipedia.org/wiki/FASTQ_format) for a sample,
   [mapping](http://en.wikipedia.org/wiki/Sequence_alignment) them to a reference genome.
	- Each step requires a range resources in terms of CPU, RAM etc.
	- Consider step 1 uses 10 cores for each file; with 50 files it would use 500 cores in total.
	- Next step uses one core for each file, 50 cores in total.
	- Say step C merges them, and uses only 1 core.
	- Some pipelines may reserve the maximum, example say 500 cores throught steps 1 to 3, 
	flowr would handle the **surge**, reserving 500, 50 or 1; when needed.
	- Now consider the run has 10 samples, all of them would be procesed in
	 parallel, spawning **thousands of cores**.
-   **Reproducible** and **transparent**, with cleanly structured execution logs
-   **Track** and **re-run** flows
-   **Lean** and **Portable**, with easy installation
-   Supports **multiple cluster computing platforms** (torque, lsf, sge, slurm ...)

### A few lines, to get started:


```r
## From the official R repository (updated every month or so)
install.packages("flowr")

## OR for a latest stable version (updated every few days):
install.packages(devtools)
devtools::install_github("sahilseth/flowr")

## OR cutting edge devel version
devtools::install_github("sahilseth/flowr", ref = "devel")

library(flowr) ## load the library
setup() ## copy flowr bash script; and create a folder flowr under home.
```

## Resources
- For a quick overview, you may browse through,
 these [introductory slides](http://sahilseth.github.io/slides/flowrintro).
- The [documentation](http://sahilseth.github.io/flowr/flowr/docs.html) provides additional details regarding
the ideas and concepts used in flowr
- We have a [tutorial](http://sahilseth.github.io/flowr/flowr/tutorial.html) which can walk you through creating a
new pipeline
- Additionally, a subset of important functions are described in the [package reference](http://sahilseth.github.io/flowr/flowr/rd.html)
page
- You can use [flow creator](https://sseth.shinyapps.io/flow_creator), a shiny app to aid in
	designing a *shiny* new flow. This provides a good example of the concepts



Aknowledgements
---------------

-   Jianhua Zhang
-   Samir Amin
-   Kadir Akdemir
-   Ethan Mao
-   Henry Song
-   An excellent resource for writing your own R packages:
    [r-pkgs.had.co.nz](r-pkgs.had.co.nz)

<!--why this license http://kbroman.org/pkg_primer/pages/licenses.html -->
