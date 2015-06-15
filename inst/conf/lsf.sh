#BSUB-J {{JOBNAME}}                                  # name of the job
#BSUB-o {{STDOUT}}                                   # output is sent to logfile, stdout + stderr by default
#BSUB-e {{STDERR}}                                   # output is sent to logfile, stdout + stderr by default
#BSUB-q {{QUEUE}}                                    # Job queue
#BSUB-W {{WALLTIME}}                                 # Walltime in minutes
#BSUB-M {{MEMORY}}                                   # Memory requirements in Kbytes
#BSUB-R usage[mem={{MEMORY}}]                        # memory reserved
#BSUB-R span[ptile={{CPU}}]                          # CPU reserved
#BSUB-r                                             # make the jobs re-runnable
#BSUB{{EXTRA_OPTS}}                                  # Any extra arguments passed onto queue()
#BUSB{{DEPENDENCY}}                                  # Don't remove dependency args come here


## command to run comes here
{{CMD}}