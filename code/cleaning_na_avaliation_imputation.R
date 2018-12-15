
path="~/Flight-Delay/Cleaning/"
#path="E:/leonardosm/Documents/Mestrado/Monografia/novo_servidor/Brazilian-Flight-Delay/demonstracao/"

#######################

loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.fiocruz.br', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


#######################
loadlibrary("dplyr")
loadlibrary("caret")
loadlibrary("dplyr")
loadlibrary("tidyr")
loadlibrary("stringr")
loadlibrary("ggplot2")
loadlibrary("corrplot")
#############################

vra_wu_cl=readRDS(paste(path,"dataset/vra_wu_cl.RData",sep=""))


#VERIFYING MISSING DATA

vmissing_data <- as.data.frame(sort(sapply(vra_wu_cl, function(x) sum(is.na(x))),decreasing = T))

vmissing_data <- (vmissing_data/nrow(vra_wu_cl))*100
View(vmissing_data)
colnames(vmissing_data)[1] <- "missingvaluesPercentage"
vmissing_data$features <- rownames(vmissing_data)

ggplot(vmissing_data[vmissing_data$missingvaluesPercentage >0,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
  geom_bar(stat="identity") +theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
  xlab("Feature") + ggtitle("Understanding Missing Data")


#DETECTED THAT VISIBILITY HAS A LOT OF MISSING DATA, TO SOLVE THIS, WE IMPUTED DATA

#DATA IMPUTING
loadlibrary("VIM")

sum(is.na(vra_wu_cl$depart_visibility))   #2459553 NA
sum(is.na(vra_wu_cl$arrival_visibility))   #2473688 NA

#HOT-DECK IMPUTING
vra_wu_cl_imp=hotdeck(vra_wu_cl,variable="depart_visibility",domain_var="origin",ord_var=c("depart_temperature", "depart_dew_point","depart_humidity","depart_pressure"))
vra_wu_cl_imp=hotdeck(vra_wu_cl_imp,variable="arrival_visibility",domain_var="destiny",ord_var=c("arrival_temperature", "arrival_dew_point","arrival_humidity","arrival_pressure"))


################################################
#GRAPHIC REPRESENTATION OF IMPUTING DATA PROCESS
histMiss(vra_wu_cl_imp[,c("origin","depart_visibility","depart_visibility_imp")], delimiter="_imp", selection="all",only.miss=FALSE) 
histMiss(vra_wu_cl_imp[,c("destiny","arrival_visibility","arrival_visibility_imp")], delimiter="_imp", selection="all",only.miss=FALSE) 


#backup with imputed data
saveRDS(vra_wu_cl_imp, paste(path,"dataset/vra_wu_cl_imp.RData",sep="")) #7832675

#LISTWISE DELETION
vra_wu_cleaned=na.omit(vra_wu_cl)  #3113759
vra_wu_cl_imp_cleaned=na.omit(vra_wu_cl_imp) #5537361

#########################################
##SAVING

#WITH IMPUTED DATA
saveRDS(vra_wu_cl_imp_cleaned, paste(path,"dataset/VRA_WU_CLEANED_WITH_IMPUTED_DATA.RData",sep="")) 

#JUST CLEANED - LISTWISE DELETION
saveRDS(vra_wu_cleaned, paste(path,"dataset/VRA_WU_CLEANED.RData",sep="")) 