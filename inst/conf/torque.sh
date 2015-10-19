#!/bin/bash
#PBS -N {{{JOBNAME}}}
#PBS -l nodes={{{NODES}}}:ppn={{{CPU}}}
#PBS -o {{{STDOUT}}}
#PBS -e {{{STDOUT}}}
#PBS -q {{{QUEUE}}}
#PBS -l walltime={{{WALLTIME}}}
#PBS -l mem={{{MEMORY}}}
#PBS -r y
#PBS -V
#PBS -j oe
#PBS -S /bin/bash
#PBS -d {{{CWD}}}
#PBS -M {{{EMAIL}}}
#PBS {{{DEPENDENCY}}}
#PBS {{{EXTRA_OPTS}}}

## -------   REMOVE one # to make QUEUE work
##PBS -q {{{QUEUE}}}                                    ## Job queue


## ------------------------------ n o t e s -------------------------##
## All variables specified above are replaced on the fly. 
## Most of them come from the flow_definition file.
## This is a core component of how flowr interacts with the cluster.
## Please refer to the platform manual, before editing this file.
## ------------------------------------------------------------------##

## --- DO NOT EDIT from below here---- ##

# following will always overwrite previous output file, if any. See https://github.com/sahilseth/flowr/issues/13
# credits: @dyndna
set +o noclobber

touch {{{TRIGGER}}}
echo 'BGN at' $(date)

##--- add some custom module commands
{{{MODULE_CMDS}}}

## --- command to run comes here (flow_mat)
{{{CMD}}}
exitstat=$?

echo 'END at' $(date)
echo $exitstat > {{{TRIGGER}}}
exit $exitstat
