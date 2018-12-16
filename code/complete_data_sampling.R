#####################################################

source(file=paste(path,"code/myPreprocessing.R",sep=""))


divideTrainAndTest <- function(data, percentual=0.8, seed=500) {
  set.seed(seed)
  x<- data
  ntrain <- round(nrow(x)*percentual)
  tindex <- sample(nrow(x), ntrain)
  xtrain <- x[tindex,]
  xtest <- x[-tindex,]
  
  return (list(xtrain, xtest))
}

#################################################################
#DATA SAMPLING 80/20

#CL_MINMAX
dtrtst <- divideTrainAndTest(data = vra_wu_cl_tf_nminmax, percentual = 0.8)
vra_wu_cl_tf_nminmax_tr=dtrtst[[1]]
vra_wu_cl_tf_nminmax_tst=dtrtst[[2]]
rm(dtrtst)
saveRDS(vra_wu_cl_tf_nminmax_tr, paste(path,"dataset/vra_wu_cl_tf_nminmax_tr.RData",sep=""))
saveRDS(vra_wu_cl_tf_nminmax_tst, paste(path,"dataset/vra_wu_cl_tf_nminmax_tst.RData",sep=""))


#CL_ZSCORE
dtrtst <- divideTrainAndTest(data = vra_wu_cl_tf_nzscore, percentual = 0.8)
vra_wu_cl_tf_nzscore_tr=dtrtst[[1]]
vra_wu_cl_tf_nzscore_tst=dtrtst[[2]]
rm(dtrtst)
saveRDS(vra_wu_cl_tf_nzscore_tr, paste(path,"dataset/vra_wu_cl_tf_nzscore_tr.RData",sep=""))
saveRDS(vra_wu_cl_tf_nzscore_tst, paste(path,"dataset/vra_wu_cl_tf_nzscore_tst.RData",sep=""))



##########

#CL_IMP_MINMAX
dtrtst <- divideTrainAndTest(data = vra_wu_cl_imp_tf_nminmax, percentual = 0.8)
vra_wu_cl_imp_tf_nminmax_tr=dtrtst[[1]]
vra_wu_cl_imp_tf_nminmax_tst=dtrtst[[2]]
rm(dtrtst)
saveRDS(vra_wu_cl_imp_tf_nminmax_tr, paste(path,"dataset/vra_wu_cl_imp_tf_nminmax_tr.RData",sep=""))
saveRDS(vra_wu_cl_imp_tf_nminmax_tst, paste(path,"dataset/vra_wu_cl_imp_tf_nminmax_tst.RData",sep=""))



#CL_IMP_ZSCORE
dtrtst <- divideTrainAndTest(data = vra_wu_cl_imp_tf_nzscore, percentual = 0.8)
vra_wu_cl_imp_tf_nzscore_tr=dtrtst[[1]]
vra_wu_cl_imp_tf_nzscore_tst=dtrtst[[2]]
rm(dtrtst)
saveRDS(vra_wu_cl_imp_tf_nzscore_tr, paste(path,"dataset/vra_wu_cl_imp_tf_nzscore_tr.RData",sep=""))
saveRDS(vra_wu_cl_imp_tf_nzscore_tst, paste(path,"dataset/vra_wu_cl_imp_tf_nzscore_tst.RData",sep=""))
gc()

