[![Build
Status](https://travis-ci.org/sahilseth/flowr.svg?branch=master)](https://travis-ci.org/sahilseth/flowr)
[![cran](http://www.r-pkg.org/badges/version/flowr)](http://cran.rstudio.com/web/packages/flowr/index.html)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/flowr)

flowr: Streamline your workflows
================================

This framework allows you to design and implement complex pipelines, and
deploy them on your institution's computing cluster. This has been built
keeping in mind the needs of bioinformatics workflows. However, it is
easily extendable to any field where a series of steps (shell commands)
are to be executed in a (work)flow.

Highlights
----------

-   Effectively process a pipeline multi-step pipeline, spawning it
    across the computing cluster
- Example:

    -   A typical case with next-generation sequencing, a sample
        with tens of
        [fastq](http://en.wikipedia.org/wiki/FASTQ_format) files)
    -   Each file can be processed
        ([aligned](http://en.wikipedia.org/wiki/Sequence_alignment))
        individually, each using multiple cores
    -   Say 50 files using 10 cores each, totalling 500 cores across
        several machines, for one sample
    -   flowr further supports processing multiple samples in
        parrellel, spawning thousands of cores.
-   Reproducible, with cleanly structured execution logs
-   Track and re-run flows
-   Lean and Portable, with easy installation
-   Supports multiple platforms (**torque**, **lsf**, **sge**, **slurm** ...)

### A few lines, to get started:

```
## From the official R repository (may be a few versions behind)
install.packages("flowr")

## OR

install.packages(devtools)
devtools::install_github("sahilseth/flowr")

library(flowr) ## load the library
setup() ## copy flowr bash script; and create a folder flowr under home.
run('sleep', execute=TRUE,  platform='moab') ## submit a simple example
```

-   Here is a shiny app,
    [flow\_creator](https://sseth.shinyapps.io/flow_creator) which helps
    you build a flow.
-   A few [slides](http://sahilseth.github.io/slides/flowrintro)
    providing a quick overview.


Aknowledgements
---------------

-   Jianhua Zhang
-   Samir Amin
-   Kadir Akdemir
-   Ethan Mao
-   Henry Song
-   An excellent resource for writing your own R packages:
    r-pkgs.had.co.nz

