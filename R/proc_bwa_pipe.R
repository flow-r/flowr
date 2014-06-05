
samplematfile <- "~/projects/analysis_ss.mt.scc/files/MTSCC_NI56_mt_scc_sample_mat.csv"
se.or.pe <- "PE"
bampath <- "/scratch/iacs/iacs_dep/sseth/data/mt_scc/Project_VC_cSCC_Ni56-2/bams"
species <- "human"
runid <- "mt_scc"
## source("~/iacsSVN/RPacks/ibm.ppm.tools/R/submit.funcs.R")
## source("~/iacsSVN/RPacks/ibm.ppm.tools/R/submit_pipes.R")
## debug(start_wex_pipe);#
## out <- start_wex_pipe(samplematfile, bampath=bampath)

start_wex_pipe <- function(){
    if(missing(gccpath)) gccpath = "/scratch/iacs/gcc"
    mat <- read.csv(samplematfile, as.is=TRUE)
    samples <- unique(mat$samplename)
    len=length(samples)
                                        ##junk <- sapply(1:len, function(i){
    for(i in 1:len){
        ## -------------  DEFINE PATHS
        sample <- samples[i]
        sample.mat <- subset(mat,samplename==sample)
        s.runid <- sample.mat$runid[1];s.lane=sample.mat$lane[1]
        s.project=sample.mat$project[1];s.subproject=sample.mat$subproject[1]
        if(missing(bampath)) bampath = file.path(gccpath,"levelii",s.runid) ## folder for samplemat
        if(missing(qapath)) qapath = file.path(gccpath,"qa",runid,sample)
        if(missing(gatkpath)) gatkpath = file.path(gccpath,"leveliii",runid, "gatk.UniTyper") ## folder for samplemat
        if(missing(se.or.pe)) se.or.pe = ifelse(length(unique(sample.mat$read))==2, "PE", "SE")
        out_basename <- as.c(sample.mat$out_basename[1])
        sorted_bam <- sample.mat$sorted_bam[1]
        fqs1=as.c(subset(sample.mat,read==1)$files)
        fqs2=as.c(subset(sample.mat,read==2)$files)
        ## ------------------- prints
        cat("Sample:", sample,"\n")
        ## ------------------- START PIPELINES
         cmd <- sprintf("bash -c '%s/bwa sampe %s %s <(%s) <(%s) %s %s | %s view -Shu - | %s sort - %s 2>> %s'",
                       bwaPath,opt,refLib,aln.cmd1,aln.cmd2,fq1,fq2,samtools,samtools,bamName,logFile)
        aln.cmd1 <- paste(bwaPath, "/bwa aln ",alnOptions," ",refLib," ",fq1,sep = "")
        aln.cmd2 <- paste(bwaPath, "/bwa aln ",alnOptions," ",refLib," ",fq2,sep = "")
        merge.cmds <- mergeBAMs(bamList=bamListFile,outBam=outBam,javaXmx="-Xmx10g",post.merge.processing=FALSE,
                                assume.sorted=FALSE,execute=FALSE,verbose=FALSE)$cmds
        o.sorted <- submit.fastq.sorted.bam(fqs1=fqs1,fqs2=fqs2,outBam=out_basename,out.bampath=bampath,sample=sample,se.or.pe=se.or.pe,
                                            bwa.aln.opts="-l 50 -n 6 -k 4",runid=runid,lane=s.lane,platform='illumina',center='IACS',
                                            species=species,jrun=TRUE,verbose=FALSE,
                                            jobflow.name="/scratch/iacs/bin/ibm_pipe/workflows/fastq_sorted_bam.xml")
    }
}
