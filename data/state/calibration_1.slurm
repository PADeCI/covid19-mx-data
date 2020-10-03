#!/bin/bash

#SBATCH -J calibration_1
#SBATCH -o calibration_1.%j
#SBATCH -N 1
#SBATCH --ntasks-per-node=7 #32 es el maximo para nodos FAT y standar
#SBATCH --qos=long #long, short, media
#SBATCH --partition=computes_standard
#SBATCH --exclusive

module load anaconda/3.7
source  /home/appls/hpc/anaconda/3.7/etc/profile.d/conda.sh 
conda activate r-environment

RUNDIR=/lustre/scratch/hpc-yadira-peralta/sccosmo-calibration-mx-states
cd $RUNDIR

#CPUS=8 #Cambiar dependiendo el numero

echo "########### START ###############"
Rscript ./analysis/03_IMIS_run.R

echo "############# END ###################"
date
echo "############ END ####################"
