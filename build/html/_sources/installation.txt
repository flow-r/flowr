Installation
-------------

Just get me started…

```
install.packages('devtools')
devtools::install_github("sahilseth/flow")
require(flow)
qobj <- queue(type="lsf", queue="normal")
jobj1 <- job(q_obj=qobj)
jobj2 <- job(q_obj=qobj)
fobj <- flow(jobs = list(jobj1, jobj2))
As of now we have tested this on the following clusters:
```