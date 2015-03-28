#!/bin/bash

#file=trigger.txt 
wait_sec=5
#cmd="ls"

######### ---------- wait for the file
dir=$(dirname $1)
echo $dir

while [ ! -f $1 ] 
 	do
		sleep $wait_sec
	done
 $2
