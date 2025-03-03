#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
 
module load anaconda3/personal

echo "R is about to run"

cp $HOME/sb4524_HPC_2024_neutral_cluster.R $TMPDIR
cp $HOME/sb4524_HPC_2024_main.R $TMPDIR
cp $HOME/Demographic.R $TMPDIR

# Run the R script
R --vanilla < $HOME/sb4524_HPC_2024_neutral_cluster.R

echo "R has finished running"

# Move results files
mv neutral_run* $HOME/ResultsN

# Move output files
mv $HOME/run_neutral_script.sh.o* $HOME/OutputN

# Move error files
mv $HOME/run_neutral_script.sh.e* $HOME/ErrorsN
