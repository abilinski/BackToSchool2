#********************************** LOADING R PACKAGE *************************************#
#                                                                                          #
#                                                                                          #
# This document implements a sample script to load and run the the BackToSchool R package. #
#******************************************************************************************#

# to update documentation
.rs.restartR()
library(roxygen2)
library(devtools)
wd = "~/Dropbox/Schools/Public code/1 - R Package/"
setwd(wd)

# make 
#create("BackToSchool")

# make documentation
setwd(paste0(wd, "BackToSchool"))
document()
build_manual()

# to load
setwd(wd)
install("BackToSchool")

