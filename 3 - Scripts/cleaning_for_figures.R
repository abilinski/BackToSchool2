#********************************* Clean code from cluster ********************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

####*********************************** SETUP ******************************************####
# source global options
#source("~/Dropbox/Schools/Public code/global_options.R")
library(tidyverse)

# local
wd = "/Users/abilinski/Desktop/4 - Output/Main2/"
#wd = "/n/home00/abilinski/Schools/"

#### IMPORT ELEMENTARY SCHOOLS ####
read.files = function(path){
  setwd(paste0(wd, path))
  
  #df = data.frame(files = list.files()) %>% separate(files, sep = "_", remove = F, into = c("num", "date")) %>%
  #  group_by(num) %>% summarize(files = files[length(files)]) %>% mutate(id = as.numeric(sub("results", "", num)))
  #files = df$files
  files = list.files()

  load(files[1])
  sims = matrix(0, length(files), ncol(out))
  sims[1,] = unlist(out)
  
  for(i in 2:length(files)){
    load(files[i])
    sims[i,] = unlist(out)
    print(i)
  }
  
  sims = data.frame(sims)
  names(sims) = names(out)
  
  setwd(paste0(wd, "Figures"))
  save(sims, file =  paste0("fig_output_", path, ".RData"))
}

setwd(wd)
folders = c("Dynamic_Elem", "Dynamic_High")
for(i in folders) {
  read.files(i)
  print(i); gc()
}

