#!/bin/bash

#SBATCH --job-name=c19
#SBATCH --account=CONDO
#SBATCH --partition=dis
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --output=hpc_output_bdwall_two.txt
#SBATCH --time=40:00:00
#SBATCH --array=0-5

set -eu

mkdir -p $SLURM_SUBMIT_DIR/02_future_runs/outputs

module load gcc/7.1.0-4bgguyp 
module load mvapich2/2.3a-avvw4kp 
. /lcrc/project/EMEWS/bebop/repos/spack/share/spack/setup-env.sh
# r 4.0.0
spack load /plchfp7

NUM_ROWS_PER_RUN=15000
STARTING_ROW=765001

RESULTS_DIR=$1
echo "RESULTS DIRECTORY: $RESULTS_DIR"

START=$(( $NUM_ROWS_PER_RUN * $SLURM_ARRAY_TASK_ID  + $STARTING_ROW))
END=$(($START + $NUM_ROWS_PER_RUN - 1))
echo "Task $SLURM_ARRAY_TASK_ID Running Rows $START - $END"
Rscript 02_future_runs/hpc_run_experiments.R $START $END 35 $RESULTS_DIR
