#!/bin/bash
#BSUB -J {{{JOBNAME}}}                                  # name of the job
#BSUB -cwd {{{CWD}}}                                    # the workding dir for each job, this is <flow_run_path>/uniqueid/tmp
#BSUB -o {{{STDOUT}}}                                   # output is sent to logfile, stdout + stderr by default
#BSUB -e {{{STDERR}}}                                   # output is sent to logfile, stdout + stderr by default
#BSUB -q {{{QUEUE}}}                                    # Job queue
#BSUB -W {{{WALLTIME}}}                                 # Walltime in hours (usually for LSF). Please consult your sysadmin.
#BSUB -M {{{MEMORY}}}                                   # Memory requirements in Kbytes
#BSUB -n {{{CPU}}}                                      # CPU reserved
#BSUB -R span[ptile={{{CPU}}}]                          # CPU reserved, all reserved on same node
#BSUB -R rusage[mem={{{MEMORY}}}]                       # memory reserved
#BSUB -r                                                # make the jobs re-runnable
#BSUB {{{DEPENDENCY}}}                                  # Don't remove dependency args come here
#BSUB {{{EXTRA_OPTS}}}                                  # Any extra arguments passed onto queue()




## ------------------------------ n o t e s -------------------------##
## All variables specified above are replaced on the fly. 
## Most of them come from the flow_definition file.
## This is a core component of how flowr interacts with the cluster.
## Please refer to the platform manual, before editing this file
## ------------------------------------------------------------------##


## --- DO NOT EDIT from below here---- ##
target="default_target.txt"
redo=FALSE
# https://stackoverflow.com/questions/16826657/how-to-exit-a-shell-script-if-targeted-file-doesnt-exist

check_target()
{
  if [[ -f $target ]] ; then
    echo "file ${target} already exists."
    if [ $redo = "TRUE" ] ; then
      echo "  However, redo requested, proceeding ..."
    else
        exit 0
    fi
  fi
}

## following will always overwrite previous output file, if any. See https://github.com/sahilseth/flowr/issues/13
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
