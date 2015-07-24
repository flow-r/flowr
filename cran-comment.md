## Test env
local OSX install: 3.2
centos: 5
centos: 6


## R CMD CHECK

## Previous issue
 - Archived on 2015-05-18 as violated the CRAN policy on the use of the user's file space.
Now changing of user's file space has been seperated as independent function: setup(), instead of automatic call by .onAttach()


## File ‘flowr/R/read-fobj.R’:
  attach(rda)
I have replaced attach with readRDS, and this remains for legacy purposes. This will go away in the next major release.


