#!/bin/bash
#MSUB -N {{{JOBNAME}}}                                  # name of the job
#MSUB -l nodes={{{NODES}}}:ppn={{{CPU}}}                          # specify number of nodes and cpu to reserve
#MSUB -o {{{STDOUT}}}                                   # output is sent to logfile, stdout + stderr by default
#MSUB -e {{{STDOUT}}}                                   # output is sent to logfile, stdout + stderr by default
#MSUB -q {{{QUEUE}}}                                    # Job queue
#MSUB -l walltime={{{WALLTIME}}}                        # Walltime in minutes
#MSUB -l mem={{{MEMORY}}}                               # Memory requirements in Kbytes
#MSUB -r y                                              # make the jobs re-runnable. 
#MSUB -V                                               # -V export all env of user to compute nodes
#MSUB -j oe                                           # merge output from stdout and stderr
#MSUB -S /bin/bash                                    # use bash shell
#MSUB -d {{{CWD}}}                                      # the workding dir for each job, this is <flow_path>/tmp
#MSUB -M {{{EMAIL}}}                                    # email address of the person
#MSUB -m n                                              # when to send a email
#MSUB {{{DEPENDENCY}}}                                  # Don't remove dependency args come here
#MSUB {{{EXTRA_OPTS}}}                                  # Any extra arguments passed onto queue(), don't change. Format handled by R

## -------   REMOVE one # to make QUEUE work


## ------------------------------ n o t e s -------------------------##
## All variables specified above are replaced on the fly. 
## Most of them come from the flow_definition file.
## This is a core component of how flowr interacts with the cluster.
## Please refer to the platform manual, before editing this file
## ------------------------------------------------------------------##

## --- DO NOT EDIT from below here---- ##
# following will always overwrite previous output file, if any. See https://github.com/sahilseth/flowr/issues/13
set +o noclobber

touch {{{TRIGGER}}}
echo 'BGN at' $(date)

##--- add some custom module commands
{{{MODULE_CMDS}}}

## --- command to run comes here (flow_mat)
{{{CMD}}}

exitstat=$?

echo 'END at' $(date)
echo ${exitstat} > {{{TRIGGER}}}
exit ${exitstat}
