getwd()
setwd("~/Flight-Delay/Cleaning")
options(java.parameters = "-Xmx31g")
path="~/Flight-Delay/Cleaning/"
#######################

loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.fiocruz.br', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("dplyr")
loadlibrary("caret")
loadlibrary("dplyr")
loadlibrary("tidyr")
loadlibrary("stringr")
loadlibrary("ggplot2")
loadlibrary("corrplot")

################################
#CLEANING
source(file=paste(path,"code/cleaning_na_avaliation_imputation",sep=""))

###############################
#CLEANING WITH NA AVALIATION AND IMPUTATION

source(file=paste(path,"code/complete_cleaning.R",sep=""))

###############################
#TRANFORMATION
source(file=paste(path,"code/myPreprocessing.R",sep=""))

#analyzing with stratified
local="ok"

#Transformation/Normalization/Sampling/Reduction/Balancing
source(file=paste(path,"code/complete_transformation.R",sep=""))
###############################