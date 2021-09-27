#!/bin/bash                                                                                                    
#SBATCH -J TestCalib # A single job name for the array                                                         
#SBATCH -n 32 # Number of cores                                                                                
#SBATCH -N 1 # All cores on one machine 
                                                                       
#SBATCH -p batch # Default                                                                          
#SBATCH --mem-per-cpu=4G # Default is KB, not MB                                              
#SBATCH -t 0-12:00 # (D-HH:MM) 8 for model setting                  
                                                                                                
#SBATCH --mail-type=END                                                                                        
#SBATCH --mail-user=alyssa_bilinski@brown.edu                                                                   

module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018 R/3.5.2

R CMD BATCH --quiet --no-restore --no-save "--args test_q=$1 notify_val=$2 test_q_isolate=$3 vax_eff_val=$4 version=$5" Elementary.R test.out
