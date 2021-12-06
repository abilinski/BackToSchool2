# libraries
library(plyr)
library(tidyverse)
library(doMC)

# set working directory
wd = "/users/abilinsk/data/abilinsk/Schools2/"
setwd(wd)

# make binding function
bind_em = function(path){
  
  # set working directory
  setwd(paste0(wd, path))
  
  # bind files
  out = ldply(list.files(), function(a){if(grepl("results", a)){load(a); return(out)}})
  
  # reset working directory and save files
  setwd(paste0(wd, "Output"))
  save(out, file = paste0(path, "_output.RData"))
  print(path)
}

# set folders
folders = list.files()[grepl("1_Dec", list.files())]
#folders = c("ES_29_Sep_3", "ES_29_Sep_4", "ES_29_Sep_2", "ES_29_Sep_1", "ES_29_Sep_0") 
print(folders)

# set up parallelization
doMC::registerDoMC(cores = 10)
foreach::getDoParWorkers()

# run parallelized loop
foreach(i=1:length(folders)) %dopar% bind_em(folders[i])
