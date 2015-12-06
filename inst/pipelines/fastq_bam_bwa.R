## --------- f l o w r        r e c i p i e s ------------- ##
##
##
## --rg-id                        <string>    (read group ID)
## --rg-sample                    <string>    (sample ID)
## --rg-library                   <string>    (library ID)
## --rg-description               <string>    (descriptive string, no tabs allowed)
## --rg-platform-unit             <string>    (e.g Illumina lane ID)
## --rg-center                    <string>    (sequencing center name)
## --rg-date                      <string>    (ISO 8601 date of the sequencing run)
## --rg-platform                  <string>    (Sequencing platform descriptor)

#' get extention of fastq files
get_fq_ext <- function(x){
  ext = gsub(".*(fastq.gz$|fastq$|fq$)", "\\1", x)
  return(ext)
}


#' @title  chk fq:
#' @description 
#' Files should have same extension. If paired, both should be of the same length
#' @param fqs1 a vector of file paths
#' @param fqs2 a vector of file paths
chk_fq <- function(fqs1, fqs2){
  paired_end = FALSE
  
  if(!missing(fqs2)){
    paired_end = TRUE
    ext = unique(get_fq_ext(c(fqs1, fqs2)))
  }else{
    ext = unique(get_fq_ext(fqs1))
  }
  
  if(length(ext) > 1)
    stop("fastq with different extenstions found, this seems troublesome !", ext)
  
  if(paired_end)
    if(length(fqs1) != length(fqs2))
      stop("Length of fqs1 and fqs2 dont match ! Exiting...\nfqs1\n:", 
           fqs1, "\n\nfqs2\n:", fqs2)
  
  cat_cmd = ifelse(ext == "gz", "zcat", "cat")
  
  return(list(ext = ext, paired_end = paired_end, cat_cmd = cat_cmd))
}



#' @rdname bwa
#' @export
#' @importFrom flowr check_args opts_flow$get to_flowmat
bwa.backtrack <- function(
  
  fqs1, 
  fqs2,
  paired_end, ## auto detect it fastq2, is available
  samplename = opts_flow$get("samplename"),
  bwa_exe = opts_flow$get("bwa_exe"), 
  ref_bwa = opts_flow$get("ref_bwa"),
  bwa_aln_opts = opts_flow$get("bwa_aln_opts"),
  cpu_bwa_aln = opts_flow$get("cpu_bwa_aln"),
  bwa_sampe_opts = opts_flow$get("bwa_sampe_opts"),
  bwa_samse_opts = opts_flow$get("bwa_samse_opts"),
  samtools_exe = opts_flow$get("samtools_exe")){
  
  
  ## --- some generic steps which may be done in case of 
  ## --- all FQ inputs !
  chkfq <- chk_fq(fqs1 = fqs1, fqs2 = fqs2)
  if(missing(paired_end))
    paired_end = chkfq$paired_end
  
  
  
  #source('~/Dropbox/public/github_flow/R/checkmat_assert.R')
  ## no arguments should be NULL
  check_args(ignore = "fqs2")
  
  ##  ------- Set up all the files which would be used.
  sai_files1 = file.path(gsub(chkfq$ext, "sai", basename(fqs1)))
  if(paired_end)
    sai_files2 = file.path( gsub(chkfq$ext, "sai", basename(fqs2)))
  
  ## These would be out files !. ALWAYS USE basename
  bam_files = file.path(gsub(chkfq$ext, "bam", basename(fqs1)))
  bam_prefix = gsub(".bam", "", bam_files)
  
  ## --- BWA aln and sampe
  #bwa_method <- match.arg(bwa_method)
  
  if(!paired_end){
    cmd_aln1 = sprintf("%s aln -t %s %s %s > %s", 
                       bwa_exe, cpu_bwa_aln, bwa_aln_opts, ref_bwa, fqs1, sai_files1)
    cmd_samse = sprintf("%s sampe %s %s %s %s| %s view -Shu - > %s",
                        bwa_exe, bwa_samse_opts, ref_bwa, sai_files1, fqs1, samtools_exe, bam_prefix)
    cmds = list(aln1 = cmd_aln1, cmd_samse = cmd_samse)
  }
  
  if(paired_end){
    cmd_aln1 = sprintf("%s aln -t %s %s %s %s > %s", 
                       bwa_exe, cpu_bwa_aln, bwa_aln_opts, ref_bwa, fqs1, sai_files1)
    cmd_aln2 = sprintf("%s aln -t %s %s %s %s > %s",
                       bwa_exe, cpu_bwa_aln, bwa_aln_opts, ref_bwa, fqs2, sai_files2)
    cmd_sampe = sprintf("%s sampe %s %s %s %s %s %s | %s view -Shu - | %s sort - %s",
                        bwa_exe, bwa_sampe_opts, ref_bwa, sai_files1, sai_files2, fqs1, fqs2, samtools_exe, samtools_exe, bam_prefix)
    ## --- make a named list of commands
    cmds = list(aln1 = cmd_aln1, aln2 = cmd_aln2, sampe = cmd_sampe)
  }
  
  
  ## --- convert to flow_mat compatible DF.
  ## --- INPUT is a NAMED list
  flowmat = to_flowmat(cmds, samplename)
  return(list(outfiles = bam_files, flowmat = flowmat))
}



#' Use picard's MergeSamFiles tool to merge bam/sam files
#' 
#' @description 
#' The resulting file is sorted and index is created for it. 
#' Validation stringency of inputs is kept as lenient.
#' Multi-threading is turned on by default, though in our experience this does
#' not seem to use a lot of threads.
#' 
#' @param x a vectors of files to merge
#' @param mergedbam 
#' @param samplename 
#' @param java_exe 
#' @param java_mem 
#' @param java_tmp 
#' @param picard_jar 
#'
#' @export
picard_merge <- function(x, 
                         mergedbam,
                         samplename = opts_flow$get("samplename"),
                         java_exe = opts_flow$get("java_exe"),
                         java_mem = opts_flow$get("java_mem"),
                         java_tmp = opts_flow$get("java_tmp"),
                         picard_jar = opts_flow$get("picard_jar")){
  
  ## -----  check if any of the params are null
  #   params = lapply(names(formals()), function(zzz) get(zzz))
  #   names(params) = names(formals())
  #   check_params(params)
  
  check_args()  
  
  bam_list = paste("INPUT=", x, sep = "", collapse = " ")
  cmds = list(merge = sprintf("%s %s -Djava.io.tmpdir=%s -jar %s MergeSamFiles %s OUTPUT=%s ASSUME_SORTED=TRUE VALIDATION_STRINGENCY=LENIENT CREATE_INDEX=true USE_THREADING=true",
                              java_exe, java_mem, java_tmp, picard_jar, bam_list, mergedbam))
  
  ## --- INPUT is a NAMED list
  flowmat = to_flowmat(cmds, samplename)
  return(list(outfiles = mergedbam, flowmat = flowmat))
}

#' picard_rg
#' @export
#' @importFrom tools file_path_sans_ext
picard_rg <- function(x, 
                      samplename = opts_flow$get("samplename"),
                      lane = opts_flow$get("rg_lane"),
                      ## convert these into get option also, only for this flow
                      seq_platform = opts_flow$get("rg_platform"),
                      center = opts_flow$get("rg_center"),
                      java_exe = opts_flow$get("java_exe"),
                      java_mem = opts_flow$get("java_mem"),
                      java_tmp = opts_flow$get("java_tmp"),
											picard_jar = opts_flow$get("picard_jar")
){
  
  
  check_args()
  #     if(missing(samplename))
  #       stop("this function needs a samplename.")
  
  ## -----  check if any of the params are null
  #     params = lapply(names(formals()), function(zzz) get(zzz))
  #     names(params) = names(formals())
  #     check_params(params)
  
  ## make this editable later ....
  rgid = rglb = rgsm = samplename
  rgpu = lane
  
  ## add RG to the orignal bam name
  bamrg_files = sprintf("%s_rg.bam", tools::file_path_sans_ext(x))
  cmds = list(fixrg = sprintf("%s %s -Djava.io.tmpdir=%s -jar %s AddOrReplaceReadGroups INPUT=%s OUTPUT=%s SORT_ORDER=coordinate RGID='%s' RGLB='%s' RGPL='%s' RGPU='%s' RGSM='%s' RGCN='%s' VALIDATION_STRINGENCY=LENIENT",
                              java_exe, java_mem, java_tmp, picard_jar, 
                              x, bamrg_files, rgid, rglb, 
                              seq_platform, rgpu, rgsm, center))
  
  flowmat = to_flowmat(cmds, samplename)
  ret = list(outfiles = bamrg_files, flowmat = flowmat)
  return(ret)
  
}




#' @title 
#' fastq_bam_bwa
#'
#' @param fqs1 list of fastq files, may be a file with just the fastqs, one in each line.
#' @param fqs2 list of fastq files, may be a file with just the fastqs, one in each line. mate 2
#'
#' @details
#' If fqs2 is missing, automatically use single end
#' 
#' @export
fastq_bam_bwa <- function(fqs1, fqs2,
                          outfile,
                          samplename = opts_flow$get("samplename")){
  
  
  ## --- all subsequent steps would use this samplename
  check_args(ignore = c("outfile", "fqs2"))
  set_opts(samplename = samplename)
  pipename = "fastq_bam_bwa"
  message("Generating a ", pipename, " flowmat for sample: ", samplename)

  ## Calling modules, each returns
  ##   - a vector of outfiles
  ##   - a flowmat, which we need to rbind and are done !
  out_bwa = bwa.backtrack(fqs1 = fqs1, fqs2 = fqs2)
  out_rg = picard_rg(out_bwa$outfiles)
  
  if(missing(outfile))
    outfile = sprintf("%s.rg.sorted.bam", samplename) ## feel free to change this !
  
  out_merge = picard_merge(out_rg$outfiles, mergedbam = outfile)
  
  ##  merging three flowmats ---
  flowmat = rbind(out_bwa$flowmat, out_rg$flowmat, out_merge$flowmat)
  
  return(list(outfile = outfile, flowmat = flowmat))
}

## ---------------------- 

if(FALSE){
	## example
  require(flowr)
  load_opts(fetch_conf("fastq_bam_bwa.conf"))
  
  ## This fails, extension seems weird
  flow_mat = fastq_bam_bwa(fqs1 = rep("hello.fq.gz", 10),
                           fqs2 = rep("hello.fq", 10),
                           samplename = "smp")
  
  
  ## This fails, length is not the same, for paired end
  flow_mat = fastq_bam_bwa(fqs1 = rep("hello.fq", 10),
                           fqs2 = rep("hello.fq", 11),
                           samplename = "smp")
  
  ## this works
  out = fastq_bam_bwa(fqs1 = rep("hello.fq", 10),
                      fqs2 = rep("hello.fq", 10),
                      samplename = "smp")
  
  debug(bwa.backtrack)
  out = fastq_bam_bwa(fqs1 = rep("hello.fq", 10),
  										samplename = "smp")
  
  
  
}


