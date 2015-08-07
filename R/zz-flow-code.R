# nocov start

## some function to supplement the shiny GUI

if(FALSE){


  qobj <- queue(platform = "lsf", queue = "normal")
  job1 <- job(name = "myjob1", q_obj = qobj)
  job2 <- job(name = "myjob2", q_obj = qobj)
  job3 <- job(name = "myjob3", q_obj = qobj, previous_job = c("myjob2", "myjob1"))
  fobj <- flow(name = "myflow", jobs = list(job1, job2, job3), desc="description")
  plot_flow(fobj)

  x <- fobj

}

### generate code from dat
#' @title generate_flow_code
#' @description generate_flow_code
#' @param x flow object
#' @param ... currently ignored
#' @keywords internal
#' @examples
#' \dontrun{
#' generate_flow_code(x = x)
#' }
generate_flow_code <- function(x, ...){
  fobj <- x
  ## this would take in a flowmat and produce a code to generate it
  jobnames <- sapply(fobj@jobs, slot, "name")
  code_jobs <- sapply(jobnames, function(j){
    prev_jobs=fobj@jobs[[j]]@previous_job;prev_jobs <- ifelse(length(prev_jobs) > 1, prev_jobs, "none")
    cpu = fobj@jobs[[j]]@cpu;cmds=fobj@jobs[[j]]@cmds
    code_cmd <- sprintf("cmd_%s <- '%s'", j, cmds)
    code_job <- sprintf("jobj_%s <- job(name = '%s', q_obj = qobj, previous_job = '%s', cpu = '%s', cmd=cmd_%s)",
                        j, j, prev_jobs, cpu, j)
    return(c(code_cmd, code_job))
  })
  return(code_jobs)
}

# nocov end
