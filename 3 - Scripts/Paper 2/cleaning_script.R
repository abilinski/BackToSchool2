# libraries
library(plyr)
library(tidyverse)
library(doMC)

# set working directory
wd = paste0("/n/home00/abilinski/Schools/")
setwd(wd)

# make binding function
bind_em = function(path){
  
  # set working directory
  setwd(paste0(wd, path))
  
  # bind files
  out = ldply(list.files(), function(a){print(a); load(a); return(out)})
  
  # reset working directory and save files
  setwd(paste0(wd, "Output"))
}

# set folders
folders = list.files()[grepl("Sep", list.files())]

# set up parallelization
doMC::registerDoMC(cores = 10)
foreach::getDoParWorkers()

# run parallelized loop
foreach(i%in%folders) %dopar% bind_em(i)
