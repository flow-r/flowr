<!--[![DOI](https://zenodo.org/badge/11075/sahilseth/flowr.svg)](http://dx.doi.org/10.5281/zenodo.16170)-->
[![Build Status](https://travis-ci.org/sahilseth/flowr.png)](https://travis-ci.org/sahilseth/flowr)
[![](http://www.r-pkg.org/badges/version/flowr)](http://cran.rstudio.com/web/packages/flowr/index.html)
![](http://cranlogs.r-pkg.org/badges/grand-total/flowr)

<!--[![codecov.io](http://codecov.io/github/sahilseth/flowr/coverage.svg?branch=master)](http://codecov.io/github/sahilseth/flowr?branch=master)-->


flowr
======

R pacakge to design (and deploy) complex workflows, in a effective and efficient manner on High Perfomance computing clusters.

Please visit [docs.flowr.space](http://docs.flowr.space) for more information and here is a slide deck providing a quick overview: [sahilseth.github.io/slides/flowrintro](http://sahilseth.github.io/slides/flowrintro)

Here is a shiny app, [flow_creator](https://sseth.shinyapps.io/flow_creator/) for you to start building your flow.

We (re)-use some of the [github issues](https://github.com/sahilseth/flowr/issues?q=label%3Aquestion) as frequently asked questions.


```
## From the official R repository (may be a few versions behind)
install.packages("flowr")

## flowr previews, stable and recent
drat::addRepo("sahilseth")		
install.packages("flowr")

## for a latest and greatest, dev version.
install.packages('devtools')
devtools::install_github("sahilseth/flowr")
```

## Example:
```
library(flowr)
example(plot_flow)
```

As of now we have tested this on the following clusters:

|Platform|submission commnd|Works?|label*|
|:---|:---:|:---:|:---:|
|LSF 9|bsub|Yes|lsf
|LSF 7|bsub|should work|lsf
|Torque|qsub|Yes|torque
|moab|msub|Yes|moab
|SGE|qsub|should work|sge

*label: This serves as the type argument for [queue](http://docs.flowr.space/en/latest/rd/topics/queue.html) function.

### Aknowledgements:
- Jianhua Zhang
- Samir Amin
- Kadir Akdemir
- Ethan Mao
- Henry Song
- An excellent resource for writing your own R packages: r-pkgs.had.co.nz
