#LOADING DATA
vra_wu_cleaned=readRDS(paste(path,"dataset/VRA_WU_CLEANED.RData",sep=""))
vra_wu_cleaned_imp=readRDS(paste(path,"dataset/VRA_WU_CLEANED_WITH_IMPUTED_DATA.RData",sep=""))

##############################
#analyzing
View(head(vra_wu_cleaned))

sum(is.na(vra_wu_cleaned))  #0
sum(is.na(vra_wu_cleaned_imp))  #0
##############################

###STRATIFIED SAmPLE TO LOCAL TEST
if(local=="ok")
  {
  vra_wu_cleaned <- sample.stratified(data=vra_wu_cleaned, clabel="delayed", perc=0.01)[1]$sample
  vra_wu_cleaned_imp <- sample.stratified(data=vra_wu_cleaned_imp, clabel="delayed", perc=0.01)[1]$sample
  
  saveRDS(vra_wu_cleaned, paste(path,"dataset/vra_wu_cleaned_new_stratified.RData",sep=""))
  saveRDS(vra_wu_cleaned_imp, paste(path,"dataset/vra_wu_cleaned_imp_new_stratified.RData",sep="")) 
}
######################################################################################

######DATA TRANSFORMATION
source(file=paste(path,"code/myTransformation.R",sep=""))

vra_wu_cl_imp_tf=transformDataimp(vra_wu_cleaned_imp)
saveRDS(vra_wu_cl_imp_tf,paste(path,"dataset/vra_wu_cl_imp_tf.RData",sep=""))
rm(vra_wu_cl_imp_tf)
gc()

vra_wu_cl_tf=transformData(vra_wu_cleaned)
saveRDS(vra_wu_cl_tf,paste(path,"dataset/vra_wu_cl_tf.RData",sep="") ) 
rm(vra_wu_cl_tf)
gc()


#Transformation Generating NA =>binning problem
sum(is.na(vra_wu_cl_tf)) #80651
sum(is.na(vra_wu_cl_imp_tf)) #181562

gc()
#View(head(vra_wu_cl_tf))

#until correcting discretization problems
#vra_wu_cl_tf=na.omit(vra_wu_cl_tf)
#vra_wu_cl_imp_tf=na.omit(vra_wu_cl_imp_tf)

#####################################################################################
###DATA NORMALIZATION

##LOADING DATA
vra_wu_cl_imp_tf=readRDS(paste(path,"dataset/vra_wu_cl_imp_tf.RData",sep=""))
vra_wu_cl_tf=readRDS(paste(path,"dataset/vra_wu_cl_tf.RData",sep=""))

source(file=paste(path,"code/complete_data_normalization.R",sep=""))

#sum(is.na(vra_wu_cl_imp_tf_nminmax)) #0
#sum(is.na(vra_wu_cl_imp_tf_nzscore)) #0

#sum(is.na(vra_wu_cl_tf_nminmax)) #0
#sum(is.na(vra_wu_cl_tf_nzscore)) #0
#View(head(vra_wu_cl_tf_nzscore))

######################################################################################
##DATA SAMPLING (80/20)


###LOADING DATA
vra_wu_cl_imp_tf_nminmax=readRDS(paste(path,"dataset/vra_wu_cl_imp_tf_nminmax.RData",sep=""))
vra_wu_cl_imp_tf_nzscore=readRDS(paste(path,"dataset/vra_wu_cl_imp_tf_nzscore.RData",sep=""))
vra_wu_cl_tf_nminmax=readRDS(paste(path,"dataset/vra_wu_cl_tf_nminmax.RData",sep=""))
vra_wu_cl_tf_nzscore=readRDS(paste(path,"dataset/vra_wu_cl_tf_nzscore.RData",sep=""))

source(file=paste(path,"code/complete_data_sampling.R",sep=""))
gc()
#####################################################################################

##DATA REDUCTION

#sum(is.na(vra_wu_cl_imp_tf_nminmax_tr)) #145177
#sum(is.na(vra_wu_cl_tf_nzscore_tr)) #64528
#sum(is.na(vra_wu_cl_tf_nminmax_tr)) #64528
#sum(is.na(vra_wu_cl_imp_tf_nzscore_tr)) #145177

###LOADING DATA

vra_wu_cl_tf_nminmax_tr=readRDS(paste(path,"dataset/vra_wu_cl_tf_nminmax_tr.RData",sep=""))
#vra_wu_cl_tf_nminmax_tst=readRDS("~/New-Flight-Delay/Transformation_New_ord/dataset/vra_wu_cl_tf_nminmax_tst.RData")
vra_wu_cl_tf_nzscore_tr=readRDS(paste(path,"dataset/vra_wu_cl_tf_nzscore_tr.RData",sep=""))
#vra_wu_cl_tf_nzscore_tst=readRDS("~/New-Flight-Delay/Transformation_New_ord/dataset/vra_wu_cl_tf_nzscore_tst.RData")
vra_wu_cl_imp_tf_nminmax_tr=readRDS(paste(path,"dataset/vra_wu_cl_imp_tf_nminmax_tr.RData",sep=""))
#vra_wu_cl_imp_tf_nminmax_tst=readRDS("~/New-Flight-Delay/Transformation_New_ord/dataset/vra_wu_cl_imp_tf_nminmax_tst.RData")
vra_wu_cl_imp_tf_nzscore_tr=readRDS(paste(path,"dataset/vra_wu_cl_imp_tf_nzscore_tr.RData",sep=""))
#vra_wu_cl_imp_tf_nzscore_tst=readRDS("~/New-Flight-Delay/Transformation_New_ord/dataset/vra_wu_cl_imp_tf_nzscore_tst.RData")


source(file=paste(path,"code/complete_data_reduction.R",sep=""))
#rm(vra_wu_cl_tf_nminmax_tr)
#####################################################################################

##DATA BALANCING

##LOADING DATA

#LASSO
vra_wu_tr_minmax_ls=readRDS(paste(path,"dataset/vra_wu_tr_minmax_ls.RData",sep=""))
vra_wu_imp_minmax_tr_ls=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_ls.RData",sep=""))
vra_wu_tr_zscore_ls=readRDS(paste(path,"dataset/vra_wu_tr_zscore_ls.RData",sep=""))
vra_wu_imp_zscore_tr_ls=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_ls.RData",sep=""))

#IG
vra_wu_tr_minmax_ig=readRDS(paste(path,"dataset/vra_wu_tr_minmax_ig.RData",sep=""))
vra_wu_imp_minmax_tr_ig=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_ig.RData",sep=""))
vra_wu_tr_zscore_ig=readRDS(paste(path,"dataset/vra_wu_tr_zscore_ig.RData",sep=""))
vra_wu_imp_zscore_tr_ig=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_ig.RData",sep=""))

#CFS
vra_wu_tr_minmax_cfs=readRDS(paste(path,"dataset/vra_wu_tr_minmax_cfs.RData",sep=""))
vra_wu_imp_minmax_tr_cfs=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_cfs.RData",sep=""))
vra_wu_tr_zscore_cfs=readRDS(paste(path,"dataset/vra_wu_tr_zscore_cfs.RData",sep=""))
vra_wu_imp_zscore_tr_cfs=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_cfs.RData",sep=""))


#PCA
vra_wu_tr_minmax_pca=readRDS(paste(path,"dataset/vra_wu_tr_minmax_pca.RData",sep=""))
vra_wu_imp_minmax_tr_pca=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_pca.RData",sep=""))
vra_wu_tr_zscore_pca=readRDS(paste(path,"dataset/vra_wu_tr_zscore_pca.RData",sep=""))
vra_wu_imp_zscore_tr_pca=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_pca.RData",sep=""))

source(file=paste(path,"code/complete_data_balancing.R",sep=""))


