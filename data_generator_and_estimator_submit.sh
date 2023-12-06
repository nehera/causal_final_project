#!/bin/bash -l

#SBATCH --job-name=data_generator_and_estimator
#SBATCH --output=data_generator_and_estimator_%A_%a.out
#SBATCH --error=data_generator_and_estimator_%A_%a.err

#SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --mem=16g
#SBATCH -t 5:00:00

#SBATCH --account=mfiecas
#SBATCH --mail-type=ALL
#SBATCH --mail-user=neher015@umn.edu

cd /home/mfiecas/neher015/causal_final_project

module load R

Rscript data_generator_and_estimator.R