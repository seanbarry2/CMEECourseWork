#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
 
module load anaconda3/personal

echo "R is about to run"

cp $HOME/sb4524_HPC_2024_demographic_cluster.R $TMPDIR
cp $HOME/Demographic.R $TMPDIR
cp $HOME/sb4524_HPC_2024_main.R $TMPDIR

# Run the R script
R --vanilla < $HOME/sb4524_HPC_2024_demographic_cluster.R

echo "R has finished running"

# Move results files
mv output_* $HOME/Results

# Move output files
mv $HOME/run_script.sh.o* $HOME/Output

# Move error files
mv $HOME/run_script.sh.e* $HOME/Errors/
