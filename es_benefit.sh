#!/bin/bash

#PBS -N es_benefit
#PBS -l walltime=00:60:00
#PBS -t 1-100

cd $PBS_O_WORKDIR

module load R/3.5.1
module load geos/3.4.2
module load gdal/2.2.2
module load proj/4.9.3
module load gcc/6.1.0

Rscript es_benefit.R
