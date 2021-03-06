#********************************** LOADING R PACKAGE *************************************#
#                                                                                          #
#                                                                                          #
# This document loads the the BackToSchool R package.                                      #
#******************************************************************************************#

# setup
.rs.restartR()
library(roxygen2)
library(devtools)
wd = "~/Dropbox/Schools/Public code/1 - R Package/"

# to update documentation
setwd(paste0(wd, "BackToSchool"))
document()
build_manual()

# to load
setwd(wd)
install("BackToSchool")
library(BackToSchool)
