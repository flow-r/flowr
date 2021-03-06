% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to-flowdef.R
\name{to_flowdef}
\alias{to_flowdef}
\alias{flowdef}
\alias{definition}
\alias{to_flowdef.flowmat}
\alias{to_flowdef.flow}
\alias{to_flowdef.character}
\alias{as.flowdef}
\alias{is.flowdef}
\title{Flow Definition defines how to stitch steps into a (work)flow.}
\format{
This is a tab separated file, with a minimum of 4 columns:<br>

\emph{required columns}:<br>
\itemize{

\item{\code{jobname}}: Name of the step

\item{\code{sub_type}}: Short for submission type, 
 refers to, how should multiple commands of this step be submitted. Possible values are `serial` or `scatter`. 
 
\item{\code{prev_jobs}}: Short for previous job, this would be the jobname of the previous job. 
This can be NA/./none if this is a independent/initial step, and no previous step is required for this to start. 
Additionally, one may use comma(s) to define multiple previous jobs (A,B).

\item{\code{dep_type}}: Short for dependency type, 
refers to the relationship of this job with the one defined in `prev_jobs`. 
This can take values `none`, `gather`, `serial` or `burst`.

}

\emph{resource columns} (recommended):<br>

Additionally, one may customize resource requirements used by each step.
The format used varies and depends to the computing platform. Thus its best to refer to 
your institutions guide to specify these.

\itemize{
	\item{\code{cpu_reserved}} integer, specifying number of cores to reserve [1]
\item{\code{memory_reserved}} Usually in KB [2000]
\item{\code{nodes}} number of server nodes to reserve, most tools can only use multiple cores on
a \strong{single} node [1]
\item{\code{walltime}} maximum time allowed for a step, usually in a HH:MM or HH:MM:SS format. [1:00]
\item{\code{queue}} the queue to use for job submission [short]
}
}
\usage{
to_flowdef(x, ...)

\method{to_flowdef}{flowmat}(
  x,
  sub_type,
  dep_type,
  prev_jobs,
  queue = "short",
  platform = "torque",
  memory_reserved = "2000",
  cpu_reserved = "1",
  nodes = "1",
  walltime = "1:00",
  guess = FALSE,
  verbose = opts_flow$get("verbose"),
  ...
)

\method{to_flowdef}{flow}(x, ...)

\method{to_flowdef}{character}(x, ...)

as.flowdef(x, ...)

is.flowdef(x)
}
\arguments{
\item{x}{can a path to a flowmat, flowmat or flow object.}

\item{...}{not used}

\item{sub_type}{submission type, one of: scatter, serial. Character, of length one or same as the number of jobnames}

\item{dep_type}{dependency type, one of: gather, serial or burst. Character, of length one or same as the number of jobnames}

\item{prev_jobs}{previous job name}

\item{queue}{Cluster queue to be used}

\item{platform}{platform of the cluster: lsf, sge, moab, torque, slurm etc.}

\item{memory_reserved}{amount of memory required.}

\item{cpu_reserved}{number of cpu's required. [1]}

\item{nodes}{if you tool can use multiple nodes, you may reserve multiple nodes for it. [1]}

\item{walltime}{amount of walltime required}

\item{guess}{should the function, guess submission and dependency types. See details.}

\item{verbose}{A numeric value indicating the amount of messages to produce.
 Values are integers varying from 0, 1, 2, 3, .... Please refer to the \link{verbose} page for more details.
\code{opts_flow$get("verbose")}}
}
\description{
This function enables creation of a skeleton flow definition with several default values, using a 
flowmat.
To customize the flowdef, one may supply parameters such as sub_type and dep_type upfront.
As such, these params must be of the same length as number of unique jobs using in the flowmat.

{
Each row in this table refers to one step of the pipeline. 
It describes the resources used by the step and also its relationship with other steps, 
especially, the step immediately prior to it.
} <br><br>

\strong{Submission types:} 
\emph{This refers to the sub_type column in flow definition.}<br>

Consider an example with three steps A, B and C. 
A has 10 commands from A1 to A10, similarly B has 10 commands B1 through B10 and 
C has a single command, C1.
Consider another step D (with D1-D3), which comes after C.

step (number of sub-processes)
A (10)   ----> B (10)  -----> C (1) -----> D (3)


 

\itemize{
\item \code{scatter}: submit all commands as parallel, independent jobs. 

	\emph{Submit A1 through A10 as independent jobs}
	\item \code{serial}: run these commands sequentially one after the other. 
	
	- \emph{Wrap A1 through A10, into a single job.}
}

\strong{Dependency types}

\emph{This refers to the dep_type column in flow definition.}

\itemize{
\item \code{none}: independent job.
		\itemize{\item \emph{Initial step A has no dependency}}
	\item \code{serial}: \emph{one to one} relationship with previous job. 
	\itemize{ \item \emph{B1 can start as soon as A1 completes, and B2 starts just after A2 and so on.}}
	\item \code{gather}: \emph{many to one}, wait for \strong{all} commands in the previous job to finish then start the  current step. 
	\itemize{\item \emph{All jobs of B (1-10), need to complete before C1 starts}}
	\item \code{burst}: \emph{one to many} wait for the previous step which has one job and start processing all cmds in the current step. 
	
	- \emph{D1 to D3 are started as soon as C1 finishes.}
}
}
\details{
\strong{NOTE:} Guessing is an experimental feature, please check the definition carefully. 
it is provided to help but not replace your best judgement. <br>

Optionally, one may provide the previous jobs and flowr can try guessing the appropriate 
submission and dependency types. If there are multiple commands, default is submitting them as 
scatter, else as serial. Further, if previous job has multiple commands and current job has single;
its assumed that all of the previous need to complete, suggesting a gather type dependency.
}
\examples{
# see ?to_flow for more examples

# read in a tsv; check and confirm format
ex = file.path(system.file(package = "flowr"), "pipelines")

# read in a flowdef from file
flowdef = as.flowdef(file.path(ex, "sleep_pipe.def"))

# check if this a flowdef
is.flowdef(flowdef)

# use a flowmat, to create a sample flowdef
flowmat = as.flowmat(file.path(ex, "sleep_pipe.tsv"))
to_flowdef(flowmat)

# change the platform
to_flowdef(flowmat, platform = "lsf")

# change the queue name
def = to_flowdef(flowmat, 
 platform = "lsf", 
 queue = "long")
plot_flow(def)

# guess submission and dependency types
def2 = to_flowdef(flowmat, 
 platform = "lsf", 
 queue = "long", 
 guess = TRUE)
plot_flow(def2)



}
