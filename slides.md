---
title: "flow: A leash to tame the elephant - HPCC"
author: Sahil Seth
output:
  beamer_presentation:
    colortheme: dolphin
    highlight: zenburn
    includes:
      in_header: /drives/Dropbox2/Dropbox/code/tex/beamerknitr.tex
    keep_tex: yes
    theme: Hannover
  ioslides_presentation:
    highlight: zenburn
    keep_md: yes
---




## Overview
\setcounter{tocdepth}{1}
\tableofcontents{}

## Vision
\section{vision}
- Huge amount of data, too little time
- Complex workflows, with inter-dependencies
- Flexible, but robust system to submit and manage jobs

## Overview
\section{overview}


## Example: a simple workflow
\section{simple}
- Let us say we have a 'Torque' cluster
- We have two jobs with 1 CPU each and second is dependent on the first


\begin{columns}[t]
\column{.5\textwidth}
\input{figure/simple.tex}
\column{.5\textwidth}
\includegraphics[page=1, type=pdf,ext=.pdf,read=.pdf, width=0.4\textwidth]{slides_files/figure-beamer/simple-1}
\end{columns}


## Example: a simple workflow
\section{simple}
- Let us say we have a 'Torque' cluster
- We have two jobs with 1 CPU each and second is dependent on the first
\begin{kframe}
\begin{alltt}
\hlstd{qobj} \hlkwb{<-} \hlkwd{queue}\hlstd{(}\hlkwc{type}\hlstd{=}\hlstr{"torque"}\hlstd{,} \hlkwc{verbose} \hlstd{=} \hlnum{FALSE}\hlstd{)}
\hlstd{jobj1} \hlkwb{<-} \hlkwd{job}\hlstd{(}\hlkwc{q_obj}\hlstd{=qobj,} \hlkwc{cmd} \hlstd{=} \hlstr{"sleep 2"}\hlstd{,} \hlkwc{cpu}\hlstd{=}\hlnum{1}\hlstd{,} \hlkwc{name} \hlstd{=} \hlstr{"job1"}\hlstd{,} \hlkwc{submission_type} \hlstd{=} \hlstr{"serial"}\hlstd{)}
\hlstd{jobj2} \hlkwb{<-} \hlkwd{job}\hlstd{(}\hlkwc{q_obj}\hlstd{=qobj,} \hlkwc{cmd} \hlstd{=} \hlstr{"sleep 2"}\hlstd{,} \hlkwc{cpu}\hlstd{=}\hlnum{1}\hlstd{,} \hlkwc{name} \hlstd{=} \hlstr{"job2"}\hlstd{,} \hlkwc{previous_job} \hlstd{=} \hlstr{"job1"}\hlstd{,} \hlkwc{submission_type} \hlstd{=} \hlstr{"serial"}\hlstd{)}
\hlstd{fobj} \hlkwb{<-} \hlkwd{flow}\hlstd{(}\hlkwc{jobs} \hlstd{=} \hlkwd{list}\hlstd{(jobj1, jobj2))}
\hlkwd{plot_flow}\hlstd{(fobj)}
\end{alltt}
\end{kframe}

\hfill{}\includegraphics[width=0.3\textwidth]{figure/simple2-1} 

















