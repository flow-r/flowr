#MSUB -N ex_sleep-sample1-20150709-20-19-27-JNavXg0f_001.sleep-1                                  # name of the job
#MSUB -l nodes=1:ppn=1                          # specify number of nodes and cpu to reserve
#MSUB -o ~/flowr/runs/ex_sleep-sample1-20150709-20-19-27-JNavXg0f/001.sleep/sleep_cmd_1.out                                   # output is sent to logfile, stdout + stderr by default
#MSUB -e ~/flowr/runs/ex_sleep-sample1-20150709-20-19-27-JNavXg0f/001.sleep/sleep_cmd_1.out                                   # output is sent to logfile, stdout + stderr by default
#MSUB -l walltime=23:00                        # Walltime in minutes
#MSUB -l mem=163185                               # Memory requirements in Kbytes
#MSUB -r y -V                                         # make the jobs re-runnable. -V export all env of user to compute nodes
#MSUB -j oe                                           # merge output from stdout and stderr
#MSUB -S /bin/bash                                    # use bash shell
#MSUB -d {{CWD}}                                       # the workding dir for each job, this is /tmp
#MSUB -M {{EMAIL}}                                      # email address of the person
#MSUB                                   # Don't remove dependency args come here
#MSUB                                   # Any extra arguments passed onto queue(), don't change. Format handled by R

## -------   REMOVE one # to make QUEUE work
##MSUB -q medium                                    # Job queue




## ------------------------------ n o t e s -------------------------##
## All variables specified above are replaced on the fly. 
## Most of them come from the flow_definition file.
## This is a core component of how flowr interacts with the cluster.
## Please refer to the platform manual, before editing this file
## ------------------------------------------------------------------##

## --- DO NOT EDIT from below here---- ##

touch {{TRIGGER}}
echo 'BGN at' `date`

## --- command to run comes here (flow_mat)
{{CMD}}

echo 'END at' `date`

exitstat=$?
echo $exitstat > {{TRIGGER}}
exit $exitstat
