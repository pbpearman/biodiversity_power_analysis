#!/bin/sh

#$ -N bird_power
#$ -S /bin/bash
#$ -cwd
#$ -j y

#$ -t 1-216:1

echo $SGE_TASK_ID

time R --vanilla --quiet --args $SGE_TASK_ID < run_pow_ann_birds3.r > bird_power$SGE_TASK_ID.log
