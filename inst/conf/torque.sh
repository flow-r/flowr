#MSUB-N ${JOBNAME}                                  # name of the job
#MSUB-l nodes=1:ppn=${CPU}                          # specify number of nodes and cpu to reserve
#MSUB-o ${STDOUT}                                   # output is sent to logfile, stdout + stderr by default
#MSUB-e ${STDOUT}                                   # output is sent to logfile, stdout + stderr by default
#MSUB-q ${QUEUE}                                    # Job queue
#MSUB-l walltime=${WALLTIME}                        # Walltime in minutes
#MSUB-l mem=${MEMORY}                               # Memory requirements in Kbytes
#MSUB-r y -V                                        # make the jobs re-runnable. -V export all env of user to compute nodes
#MSUB-j oe                                          # merge output from stdout and stderr
#MSUB-S /bin/bash                                   # use bash shell
#MSUB-d {CWD}                                       # the workding dir for each job, this is ${flow_path}/tmp
#MSUB${DEPENDENCY}                                  # Don't remove dependency args come here
#MSUB${EXTRA_OPTS}                                  # Any extra arguments passed onto queue(), don't change. Format handled by R

## command to run comes here
${CMD}