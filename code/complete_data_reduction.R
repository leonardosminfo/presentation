#####################################################
source(file=paste(path,"code/myPreprocessing.R",sep=""))

#################################################################
#DATA REDUCTION

source(file=paste(path,"code/myFeature.R",sep=""))
########################################
#LASSO

vra_wu_tr_minmax_ls <- fs.lasso(vra_wu_cl_tf_nminmax_tr, "delayed") [[1]]
vra_wu_imp_minmax_tr_ls <- fs.lasso(vra_wu_cl_imp_tf_nminmax_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_minmax_ls, paste(path,"dataset/vra_wu_tr_minmax_ls.RData",sep=""))
saveRDS(vra_wu_imp_minmax_tr_ls, paste(path,"dataset/vra_wu_imp_minmax_tr_ls.RData",sep=""))
rm(vra_wu_tr_minmax_ls,vra_wu_imp_minmax_tr_ls)
gc()


#zscore
vra_wu_tr_zscore_ls <- fs.lasso(vra_wu_cl_tf_nzscore_tr, "delayed") [[1]]
vra_wu_imp_zscore_tr_ls <- fs.lasso(vra_wu_cl_imp_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_zscore_ls, paste(path,"dataset/vra_wu_tr_zscore_ls.RData",sep=""))
saveRDS(vra_wu_imp_zscore_tr_ls, paste(path,"dataset/vra_wu_imp_zscore_tr_ls.RData",sep=""))
rm(vra_wu_tr_zscore_ls,vra_wu_imp_zscore_tr_ls)
gc()
#####################################

#INFOGAIN
#minmax
vra_wu_tr_minmax_ig <- fs.ig(vra_wu_cl_tf_nminmax_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_minmax_ig, paste(path,"dataset/vra_wu_tr_minmax_ig.RData",sep=""))
rm(vra_wu_tr_minmax_ig)
gc(reset=TRUE)

vra_wu_imp_minmax_tr_ig <- fs.ig(vra_wu_cl_imp_tf_nminmax_tr, "delayed")[[1]]
saveRDS(vra_wu_imp_minmax_tr_ig, paste(path,"dataset/vra_wu_imp_minmax_tr_ig.RData",sep=""))
rm(vra_wu_imp_minmax_tr_ig)
gc()

#zscore
vra_wu_tr_zscore_ig <- fs.ig(vra_wu_cl_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_zscore_ig, paste(path,"dataset/vra_wu_tr_zscore_ig.RData",sep=""))
rm(vra_wu_tr_zscore_ig)
gc()

vra_wu_imp_zscore_tr_ig <- fs.ig(vra_wu_cl_imp_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_imp_zscore_tr_ig, paste(path,"dataset/vra_wu_imp_zscore_tr_ig.RData",sep=""))
rm(vra_wu_imp_zscore_tr_ig)
gc()


#####################################
#CFS
#minmax
vra_wu_tr_minmax_cfs <- fs.cfs(vra_wu_cl_tf_nminmax_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_minmax_cfs, paste(path,"dataset/vra_wu_tr_minmax_cfs.RData",sep=""))
gc()
rm(vra_wu_tr_minmax_cfs)

vra_wu_imp_minmax_tr_cfs <- fs.cfs(vra_wu_cl_imp_tf_nminmax_tr, "delayed") [[1]]
saveRDS(vra_wu_imp_minmax_tr_cfs, paste(path,"dataset/vra_wu_imp_minmax_tr_cfs.RData",sep=""))
rm(vra_wu_imp_minmax_tr_cfs)
gc()

#zscore
vra_wu_tr_zscore_cfs <- fs.cfs(vra_wu_cl_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_zscore_cfs, paste(path,"dataset/vra_wu_tr_zscore_cfs.RData",sep=""))
rm(vra_wu_tr_zscore_cfs)

vra_wu_imp_zscore_tr_cfs <- fs.cfs(vra_wu_cl_imp_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_imp_zscore_tr_cfs, paste(path,"dataset/vra_wu_imp_zscore_tr_cfs.RData",sep=""))
rm(vra_wu_imp_zscore_tr_cfs)
gc()

#####################################
#PCA
#minmax
vra_wu_tr_minmax_pca <- dt.pca(vra_wu_cl_tf_nminmax_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_minmax_pca, paste(path,"dataset/vra_wu_tr_minmax_pca.RData",sep=""))
#vra_wu_tr_minmax_pca <- dt.pca2(vra_wu_cl_tf_nminmax_tr, "delayed") [[1]]
#saveRDS(vra_wu_tr_minmax_pca, paste(path,"dataset/vra_wu_tr_minmax_pca2.RData",sep=""))

rm(vra_wu_tr_minmax_pca)
gc()

vra_wu_imp_minmax_tr_pca <- dt.pca(vra_wu_cl_imp_tf_nminmax_tr, "delayed") [[1]]
saveRDS(vra_wu_imp_minmax_tr_pca, paste(path,"dataset/vra_wu_imp_minmax_tr_pca.RData",sep=""))
rm(vra_wu_imp_minmax_tr_pca)
gc()

#zscore
vra_wu_tr_zscore_pca <- dt.pca(vra_wu_cl_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_tr_zscore_pca, paste(path,"dataset/vra_wu_tr_zscore_pca.RData",sep=""))
rm(vra_wu_tr_zscore_pca)
gc()

vra_wu_imp_zscore_tr_pca <- dt.pca(vra_wu_cl_imp_tf_nzscore_tr, "delayed") [[1]]
saveRDS(vra_wu_imp_zscore_tr_pca, paste(path,"dataset/vra_wu_imp_zscore_tr_pca.RData",sep=""))
rm(vra_wu_imp_zscore_tr_pca)
gc()

#print(vra_wu_tr_zscore_pca)
