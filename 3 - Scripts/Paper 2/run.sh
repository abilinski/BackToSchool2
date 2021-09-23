#!/bin/bash                                                                                                    
#SBATCH -J TestCalib # A single job name for the array                                                         
#SBATCH -n 32 # Number of cores                                                                                
#SBATCH -N 1 # All cores on one machine                                                                        
#SBATCH -p serial_requeue # Partition                                                                          
#SBATCH --mem-per-cpu=4000 # Memory request 700 for model setting                                              
#SBATCH -t 0-12:00 # (D-HH:MM) 8 for model setting                                                             
#SBATCH -o /n/home00/abilinski/Schools/%a.out # Standard output                                                    
#SBATCH -e /n/home00/abilinski/Schools/%a.err # Standard error                                                     
#SBATCH --mail-type=END                                                                                        
#SBATCH --mail-user=abilinski@g.harvard.edu                                                                    

export R_LIBS_USER=$HOME/apps/R_3.5.1:$R_LIBS_USER
module load R/3.5.1-fasrc01

R CMD BATCH --quiet --no-restore --no-save "--args test_q=$1 notify_val=$2 version=$3" Elementary.R test.out
