#!/bin/bash
#SBATCH --job-name={{{JOBNAME}}}                        # name of the job
#SBATCH --chdir={{{CWD}}}                               # the workding dir for each job
#SBATCH --output={{{STDOUT}}}                           # output is sent to logfile, stdout + stderr by default
#SBATCH --error={{{STDOUT}}}                            # output is sent to logfile, stdout + stderr by default
#SBATCH --partition={{{PARTITION}}}                     # Job partition
#SBATCH --qos={{{QUEUE}}}                               # Job queue or qos, defined by column:queue in flowr.def
#SBATCH --time={{{WALLTIME}}}                      	    # Walltime, defined by column:walltime in flowr.def
#SBATCH --mem={{{MEMORY}}}                              # Memory, defined by column:memory_reserved in flowr.def
#SBATCH --nodes={{{NODES}}}                 		        # Nodes, defined by column:nodes in flowr.def
#SBATCH --ntasks={{{CPU}}}                 			        # CPU reserved, defined by column:cpu_reserved in flowr.def
#SBATCH --mail-type=FAIL                                # send email when job fails
#SBATCH --requeue --export=all                          # enable requeue and export current shell env
#SBATCH {{{DEPENDENCY}}}                                # slurm dependency, https://slurm.schedmd.com/sbatch.html
#SBATCH {{{EXTRA_OPTS}}}                                # extra options



## ------------------------------ n o t e s -------------------------##
## All variables specified above are replaced on the fly. 
## Most of them come from the flow_definition file.
## This is a core component of how flowr interacts with the cluster.
## Please refer to the platform manual, before editing this file
## ------------------------------------------------------------------##

## --- DO NOT EDIT from below here---- ##
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
