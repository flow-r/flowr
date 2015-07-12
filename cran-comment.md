## Test env
local OSX install: 3.2
centos: 5
centos: 6


## R CMD CHECK

There was  previous issue, in which I was automatically copying a file to user's bin folder upon loading the package.

Now this has been seperated as independent function: setup(), which a user needs to call.

### Package in Depends field not imported from: ‘rmarkdown’
  These packages need to be imported from (in the NAMESPACE file)
  
## File ‘flowr/R/read-fobj.R’:
  attach(rda)
  
I have replaced attach, with readRDS, attach is now only for legacy purposes. This will go away in the next major release.

