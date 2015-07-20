## example from
## https://www.hpc2n.umu.se/node/738



#!/bin/bash
# The name of the account you are running in. If none is given, you 
# will run in the default account, which will have lower priority
# and can take a long time to start. 
#SBATCH -A <account> 
# Asking for two nodes
#SBATCH -N 2
# and two processors
#SBATCH -n 2
# and two cpu's per task
#SBATCH -c 2

# This means the job cannot share nodes with any other running 
# jobs - it is the opposite of --share 
#SBATCH --exclusive 
# The job may take up to 5 minutes to complete 
#SBATCH --time=00:05:00 
# Set the names for the error and output files 
#SBATCH --error=job.%J.err 
#SBATCH --output=job.%J.out

srun ./my_program

touch {{{TRIGGER}}}
echo 'BGN at' $(date)

## --- command to run comes here (flow_mat)
{{{CMD}}}

exitstat=$?

echo 'END at' $(date)
echo ${exitstat} > {{{TRIGGER}}}
exit ${exitstat}

