
<!--brand: |-
  <a href="http://docs.flowr.space">
  <img src='https://raw.githubusercontent.com/sahilseth/flowr/devel/vignettes/files/logo.png' alt='flowr icon' width='40px' height='40px' style='margin-top: -10px;'>
  </a>
-->

  
<meta property="og:description" content="Easy, scalable big data pipelines using hpcc (high performance computing cluster)">
<meta property="og:title" content="flowr — Easy, scalable big data pipelines using hpcc">
<meta name="twitter:description" content="flowr - Easy, scalable big data pipelines using hpcc (high performance computing cluster)">
<meta name="twitter:title" content="flowr — Easy, scalable big data pipelines using hpcc (high performance computing cluster)">


[![Build Status](https://travis-ci.org/sahilseth/flowr.svg?branch=master)](https://travis-ci.org/sahilseth/flowr)
[![cran](http://www.r-pkg.org/badges/version/flowr)](https://cran.r-project.org/package=flowr)
[![codecov.io](http://codecov.io/github/sahilseth/flowr/coverage.svg?branch=devel)](http://codecov.io/github/sahilseth/flowr?branch=devel)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/flowr)

<!--
![license](https://img.shields.io/badge/license-MIT-blue.svg)
-->


## [![docs.flowr.space](https://raw.githubusercontent.com/sahilseth/flowr/devel/vignettes/files/logo.png) Streamlining Computing workflows](http://docs.flowr.space)

**Latest documentation: [docs.flowr.space](http://docs.flowr.space)**



Flowr framework allows you to design and implement complex pipelines, and
deploy them on your institution's computing cluster. This has been built
keeping in mind the needs of bioinformatics workflows. However, it is
easily extendable to any field where a series of steps (shell commands)
are to be executed in a (work)flow.

### Highlights

- No new **syntax or language**. Put all shell commands as a tsv file called [flow mat](http://docs.flowr.space/overview.html#flow_matrix).
- Define the [flow of steps](http://docs.flowr.space/overview.html#relationships) using a simple tsv file (serial, scatter, gather, burst...) called [flow def](http://docs.flowr.space/overview.html#flow_definition).
- Works on your laptop/server or cluster (/cloud).
- Supports **multiple cluster computing platforms** (torque, lsf, sge, slurm ...), cloud (star cluster) OR a local machine.
- One line installation (`install.packages("flowr")`)
- **Reproducible** and **transparent**, with cleanly structured execution logs
- **Track** and **re-run** flows
- **Lean** and **Portable**, with easy installation
- **Fine grain** control over resources (CPU, memory, walltime, etc.) of each step.

### Example
[![ex_fq_bam](http://docs.flowr.space/files/ex_fq_bam.png)](https://github.com/flow-r/fastq_bam)

<!---
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
--->


### A few lines, to get started


```r
## Official stable release from CRAN (updated every other month)
## visit docs.flowr.space/install for more details
install.packages("flowr",  repos = "http://cran.rstudio.com")

# or a latest version from DRAT, provide cran for dependencies
install.packages("flowr", repos = c(CRAN="http://cran.rstudio.com", DRAT="http://sahilseth.github.io/drat"))

library(flowr) ## load the library
setup() ## copy flowr bash script; and create a folder flowr under home.

## Run an example pipeline
flowr run x=sleep_pipe platform=local execute=TRUE
```

**Example pipelines** [inst/pipelines](https://github.com/sahilseth/flowr/blob/devel/inst/pipelines)

### Resources
- For a quick overview, you may browse through,
 these [introductory slides](http://sahilseth.github.io/slides/flowrintro).
- The [overview](http://docs.flowr.space/overview.html) provides additional details regarding
the ideas and concepts used in flowr
- We have a [tutorial](http://docs.flowr.space/tutorial.html) which can walk you through creating a
new pipeline
- Additionally, a subset of important functions are described in the [package reference](http://docs.flowr.space/rd.html)
page
- You may follow detailed instructions on [installing and configuring](http://docs.flowr.space/install.html)
- You can use [flow creator](https://sseth.shinyapps.io/flow_creator), a shiny app to aid in
	designing a *shiny* new flow. This provides a good example of the concepts

### Updates
This package is under active-development, 
you may watch for changes using
the [watch link above](https://help.github.com/articles/watching-repositories/).

### Feedback
Please feel free to raise a [github issue](https://github.com/sahilseth/flowr/issues) with questions and comments.

### Acknowledgements

-   Jianhua Zhang
-   Samir Amin
-   Roger Moye
-   Kadir Akdemir
-   Ethan Mao
-   Henry Song
-   An excellent resource for writing your own R packages:
    [r-pkgs.had.co.nz](r-pkgs.had.co.nz)

<!--why this license http://kbroman.org/pkg_primer/pages/licenses.html -->
<script src = "vignettes/files/googl.js"></script>
