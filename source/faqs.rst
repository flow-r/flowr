Some helpful tips and questions
============================================

.. eventually in running the pipeline

:Q: What platforms are supported

========   ===============  ============    =======
|Platform|submission commnd|Works?       |queue type *|
========   ===============  ============    =======
LSF HPC 7|bsub             |Not tested   |lsf |
LSF 9.1  |bsub             |Yes          |lsf |
Torque   |qsub             |Yes          |torque |
SGE      |qsub             |Not implemented|
==================================================


:Q:  Am getting a error in `devtools:::install_github("sahilseth/flowr")`
	error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed

:A:	This is basically a issue with httr 
	(`link <http://stackoverflow.com/questions/24793863/devtoolsinstall-github-ignore-ssl-cert-verification-failure>`_)
	Try this:

::

	install.packages("RCurl")
	devtools:::install_github("sahilseth/flowr")
	
	If not then try this:

::
	
	install.packages("httr");
	library(httr);
	set_config( config( ssl.verifypeer = 0L ) )
	devtools:::install_github("sahilseth/flowr")
	

