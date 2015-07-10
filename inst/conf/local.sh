

## ------------------------------ n o t e s -------------------------##
## All variables specified above are replaced on the fly. 
## Most of them come from the flow_definition file.
## This is a core component of how flowr interacts with the cluster.
## Please refer to the platform manual, before editing this file.
## ------------------------------------------------------------------##

## --- DO NOT EDIT from below here---- ##

touch {{{TRIGGER}}}
echo 'BGN at' `date`

## --- command to run comes here (flow_mat)
{{{CMD}}}

echo 'END at' `date`

exitstat=$?
echo $exitstat > {{{TRIGGER}}}
exit $exitstat
