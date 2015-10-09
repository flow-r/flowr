## Example pipeline to run fastq_bam

For detailed instructions refer to: http://rpubs.com/sahiilseth/flowr_fq_bam


Assuming, you have three files in current wd OR `~/flowr/pipelines` and 
flowr has been installed and tested (http://docs.flowr.space/install.html)

```
cd ~/flowr/pipelines
base=https://raw.githubusercontent.com/sahilseth/flowr/devel/inst/pipelines
wget $base/fastq_bam_bwa.R
wget $base/fastq_bam_bwa.conf
wget $base/fastq_bam_bwa.def
```

Edit the conf file to provide relevent paths to tools

```
## customize parameters, including paths to samtools, bwa, reference genomes etc.
vi fastq_bam_bwa.conf
```

Confirm that the right platform and queue is listed in the flow definition:

```
## customize the resource requirements in flowdef:
- need to change: queue, platform
- may change: walltime, memory, CPUs etc.
vi fastq_bam_bwa.def
```

## submit to the cluster
flowr run x=fastq_bam_bwa fqs1=$fqs1 fqs2=$fqs2 samplename=samp execute=TRUE
