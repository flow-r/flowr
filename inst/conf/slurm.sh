#!/bin/bash
#SBATCH -J {{{JOBNAME}}}
#SBATCH -N {{{NODES}}}
#SBATCH -n {{{CPU}}}
#SBATCH -D {{{CWD}}}
#SBATCH --error={{{STDOUT}}}
#SBATCH --output={{{STDOUT}}}
#SBATCH --partition={{{QUEUE}}}
#SBATCH --time={{{WALLTIME}}} 
#SBATCH --mem={{{MEMORY}}}
#SBATCH {{{DEPENDENCY}}}
#SBATCH {{{EXTRA_OPTS}}}


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
