FAQs
============================================

.. eventually in running the pipeline

:Q: What platforms are supported

:A: LSF, torque, SGE, ... more here.


:Q:  Am getting a error in `devtools:::install_github("sahilseth/flowr")`
	
::

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
	

