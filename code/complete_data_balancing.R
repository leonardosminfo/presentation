
source(file=paste(path,"code/myPreprocessing.R",sep=""))

##############################

#Balancing
#oversampling  balance.oversampling

#subsampling balance.subsampling

# DATA BALANCING
#######################
#LASSO

#SMOTE(over)###########
###MINMAX
vra_wu_tr_mm_ls.bo=balance.oversampling(vra_wu_tr_minmax_ls, "delayed")  #SMOTE
vra_wu_imp_mm_tr_ls.bo=balance.oversampling(vra_wu_imp_minmax_tr_ls, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_ls.bo,paste(path,"dataset/vra_wu_tr_mm_ls.bo.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_ls.bo,paste(path,"dataset/vra_wu_imp_mm_tr_ls.bo.RData",sep=""))
gc()
rm(vra_wu_tr_mm_ls.bo,vra_wu_imp_mm_tr_ls.bo)

###ZSCORE
vra_wu_tr_zs_ls.bo=balance.oversampling(vra_wu_tr_zscore_ls, "delayed")  #SMOTE
vra_wu_imp_zs_tr_ls.bo=balance.oversampling(vra_wu_imp_zscore_tr_ls, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_ls.bo,paste(path,"dataset/vra_wu_tr_zs_ls.bo.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_ls.bo, paste(path,"dataset/vra_wu_imp_zs_tr_ls.bo.RData",sep=""))
gc()
rm(vra_wu_tr_zs_ls.bo,vra_wu_imp_zs_tr_ls.bo)

#RS(sub)################
###MINMAX
vra_wu_tr_mm_ls.bs=balance.subsampling(vra_wu_tr_minmax_ls, "delayed")  #SMOTE
vra_wu_imp_mm_tr_ls.bs=balance.subsampling(vra_wu_imp_minmax_tr_ls, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_ls.bs,paste(path,"dataset/vra_wu_tr_mm_ls.bs.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_ls.bs,paste(path,"dataset/vra_wu_imp_mm_tr_ls.bs.RData",sep=""))
gc()
rm(vra_wu_tr_mm_ls.bs,vra_wu_imp_mm_tr_ls.bs)


###ZSCORE
vra_wu_tr_zs_ls.bs=balance.subsampling(vra_wu_tr_zscore_ls, "delayed")  #SMOTE
vra_wu_imp_zs_tr_ls.bs=balance.subsampling(vra_wu_imp_zscore_tr_ls, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_ls.bs,paste(path,"dataset/vra_wu_tr_zs_ls.bs.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_ls.bs, paste(path,"dataset/vra_wu_imp_zs_tr_ls.bs.RData",sep=""))
gc()
rm(vra_wu_tr_zs_ls.bs,vra_wu_imp_zs_tr_ls.bs)
#####################


#######################
#IG

#SMOTE(over)###########
###MINMAX
vra_wu_tr_mm_ig.bo=balance.oversampling(vra_wu_tr_minmax_ig, "delayed")  #SMOTE
vra_wu_imp_mm_tr_ig.bo=balance.oversampling(vra_wu_imp_minmax_tr_ig, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_ig.bo,paste(path,"dataset/vra_wu_tr_mm_ig.bo.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_ig.bo,paste(path,"dataset/vra_wu_imp_mm_tr_ig.bo.RData",sep=""))
gc()
rm(vra_wu_tr_mm_ig.bo,vra_wu_imp_mm_tr_ig.bo)

###ZSCORE
vra_wu_tr_zs_ig.bo=balance.oversampling(vra_wu_tr_zscore_ig, "delayed")  #SMOTE
vra_wu_imp_zs_tr_ig.bo=balance.oversampling(vra_wu_imp_zscore_tr_ig, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_ig.bo,paste(path,"dataset/vra_wu_tr_zs_ig.bo.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_ig.bo, paste(path,"dataset/vra_wu_imp_zs_tr_ig.bo.RData",sep=""))
gc()
rm(vra_wu_tr_zs_ig.bo,vra_wu_imp_zs_tr_ig.bo)

#RS(sub)################
###MINMAX
vra_wu_tr_mm_ig.bs=balance.subsampling(vra_wu_tr_minmax_ig, "delayed")  #SMOTE
vra_wu_imp_mm_tr_ig.bs=balance.subsampling(vra_wu_imp_minmax_tr_ig, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_ig.bs,paste(path,"dataset/vra_wu_tr_mm_ig.bs.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_ig.bs,paste(path,"dataset/vra_wu_imp_mm_tr_ig.bs.RData",sep=""))
gc()
rm(vra_wu_tr_mm_ig.bs,vra_wu_imp_mm_tr_ig.bs)


###ZSCORE
vra_wu_tr_zs_ig.bs=balance.subsampling(vra_wu_tr_zscore_ig, "delayed")  #SMOTE
vra_wu_imp_zs_tr_ig.bs=balance.subsampling(vra_wu_imp_zscore_tr_ig, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_ig.bs,paste(path,"dataset/vra_wu_tr_zs_ig.bs.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_ig.bs, paste(path,"dataset/vra_wu_imp_zs_tr_ig.bs.RData",sep=""))
gc()
rm(vra_wu_tr_zs_ig.bs,vra_wu_imp_zs_tr_ig.bs)
#####################




#######################
#cfs

#SMOTE(over)###########
###MINMAX
vra_wu_tr_mm_cfs.bo=balance.oversampling(vra_wu_tr_minmax_cfs, "delayed")  #SMOTE
vra_wu_imp_mm_tr_cfs.bo=balance.oversampling(vra_wu_imp_minmax_tr_cfs, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_cfs.bo,paste(path,"dataset/vra_wu_tr_mm_cfs.bo.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_cfs.bo,paste(path,"dataset/vra_wu_imp_mm_tr_cfs.bo.RData",sep=""))
gc()
rm(vra_wu_tr_mm_cfs.bo,vra_wu_imp_mm_tr_cfs.bo)

###ZSCORE
vra_wu_tr_zs_cfs.bo=balance.oversampling(vra_wu_tr_zscore_cfs, "delayed")  #SMOTE
vra_wu_imp_zs_tr_cfs.bo=balance.oversampling(vra_wu_imp_zscore_tr_cfs, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_cfs.bo,paste(path,"dataset/vra_wu_tr_zs_cfs.bo.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_cfs.bo, paste(path,"dataset/vra_wu_imp_zs_tr_cfs.bo.RData",sep=""))
gc()
rm(vra_wu_tr_zs_cfs.bo,vra_wu_imp_zs_tr_cfs.bo)

#RS(sub)################
###MINMAX
vra_wu_tr_mm_cfs.bs=balance.subsampling(vra_wu_tr_minmax_cfs, "delayed")  #SMOTE
vra_wu_imp_mm_tr_cfs.bs=balance.subsampling(vra_wu_imp_minmax_tr_cfs, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_cfs.bs,paste(path,"dataset/vra_wu_tr_mm_cfs.bs.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_cfs.bs,paste(path,"dataset/vra_wu_imp_mm_tr_cfs.bs.RData",sep=""))
gc()
rm(vra_wu_tr_mm_cfs.bs,vra_wu_imp_mm_tr_cfs.bs)


###ZSCORE
vra_wu_tr_zs_cfs.bs=balance.subsampling(vra_wu_tr_zscore_cfs, "delayed")  #SMOTE
vra_wu_imp_zs_tr_cfs.bs=balance.subsampling(vra_wu_imp_zscore_tr_cfs, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_cfs.bs,paste(path,"dataset/vra_wu_tr_zs_cfs.bs.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_cfs.bs, paste(path,"dataset/vra_wu_imp_zs_tr_cfs.bs.RData",sep=""))
gc()
rm(vra_wu_tr_zs_cfs.bs,vra_wu_imp_zs_tr_cfs.bs)
#####################





#######################
#pca

#View(head(vra_wu_tr_minmax_pca))
#SMOTE(over)###########
###MINMAX
vra_wu_tr_mm_pca.bo=balance.oversampling(vra_wu_tr_minmax_pca, "delayed")  #SMOTE
vra_wu_imp_mm_tr_pca.bo=balance.oversampling(vra_wu_imp_minmax_tr_pca, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_pca.bo,paste(path,"dataset/vra_wu_tr_mm_pca.bo.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_pca.bo,paste(path,"dataset/vra_wu_imp_mm_tr_pca.bo.RData",sep=""))
gc()
rm(vra_wu_tr_mm_pca.bo,vra_wu_imp_mm_tr_pca.bo)

###ZSCORE
vra_wu_tr_zs_pca.bo=balance.oversampling(vra_wu_tr_zscore_pca, "delayed")  #SMOTE
vra_wu_imp_zs_tr_pca.bo=balance.oversampling(vra_wu_imp_zscore_tr_pca, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_pca.bo,paste(path,"dataset/vra_wu_tr_zs_pca.bo.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_pca.bo, paste(path,"dataset/vra_wu_imp_zs_tr_pca.bo.RData",sep=""))
gc()
rm(vra_wu_tr_zs_pca.bo,vra_wu_imp_zs_tr_pca.bo)

#RS(sub)################
###MINMAX
vra_wu_tr_mm_pca.bs=balance.subsampling(vra_wu_tr_minmax_pca, "delayed")  #SMOTE
vra_wu_imp_mm_tr_pca.bs=balance.subsampling(vra_wu_imp_minmax_tr_pca, "delayed")  #SMOTE
saveRDS(vra_wu_tr_mm_pca.bs,paste(path,"dataset/vra_wu_tr_mm_pca.bs.RData",sep=""))
saveRDS(vra_wu_imp_mm_tr_pca.bs,paste(path,"dataset/vra_wu_imp_mm_tr_pca.bs.RData",sep=""))
gc()
rm(vra_wu_tr_mm_pca.bs,vra_wu_imp_mm_tr_pca.bs)


###ZSCORE
vra_wu_tr_zs_pca.bs=balance.subsampling(vra_wu_tr_zscore_pca, "delayed")  #SMOTE
vra_wu_imp_zs_tr_pca.bs=balance.subsampling(vra_wu_imp_zscore_tr_pca, "delayed") #SMOTE
saveRDS(vra_wu_tr_zs_pca.bs,paste(path,"dataset/vra_wu_tr_zs_pca.bs.RData",sep=""))
saveRDS(vra_wu_imp_zs_tr_pca.bs, paste(path,"dataset/vra_wu_imp_zs_tr_pca.bs.RData",sep=""))
gc()
rm(vra_wu_tr_zs_pca.bs,vra_wu_imp_zs_tr_pca.bs)
#####################





