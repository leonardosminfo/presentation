source(file=paste(path,"code/myPreprocessing.R",sep=""))

############################
####WITH IMPUTATION

#mimax
vra_wu_cl_imp_tf_nminmax=vra_wu_cl_imp_tf
dtminmax=normalize.minmax(vra_wu_cl_imp_tf)$data
vra_wu_cl_imp_tf_nminmax[,colnames(dtminmax)]=dtminmax
View(head(vra_wu_cl_imp_tf_nminmax))

gc()

#zscore
vra_wu_cl_imp_tf_nzscore=vra_wu_cl_imp_tf
dtzscore=normalize.zscore(vra_wu_cl_imp_tf)$data
vra_wu_cl_imp_tf_nzscore[,colnames(dtzscore)]=dtzscore
View(head(vra_wu_cl_imp_tf_nzscore))


#SAVING
saveRDS(vra_wu_cl_imp_tf_nminmax,paste(path, "dataset/vra_wu_cl_imp_tf_nminmax.RData",sep=""))
saveRDS(vra_wu_cl_imp_tf_nzscore, paste(path, "dataset/vra_wu_cl_imp_tf_nzscore.RData",sep="")) 

rm(vra_wu_cl_imp_tf_nminmax,vra_wu_cl_imp_tf_nzscore,dtminmax,dtzscore)
gc()

###############################
#CLEANED WITHOUT IMPUTATION

#MINMAX
vra_wu_cl_tf_nminmax=vra_wu_cl_tf
dtminmax=normalize.minmax(vra_wu_cl_tf)$data
vra_wu_cl_tf_nminmax[,colnames(dtminmax)]=dtminmax
View(head(vra_wu_cl_tf_nminmax))
View(head(vra_wu_cl_tf))

gc()

#ZSCORE
vra_wu_cl_tf_nzscore=vra_wu_cl_tf
dtzscore=normalize.zscore(vra_wu_cl_tf)$data
vra_wu_cl_tf_nzscore[,colnames(dtzscore)]=dtzscore
View(head(vra_wu_cl_tf_nzscore))

#SAVING
saveRDS(vra_wu_cl_tf_nminmax, paste(path,"dataset/vra_wu_cl_tf_nminmax.RData",sep=""))
saveRDS(vra_wu_cl_tf_nzscore, paste(path,"dataset/vra_wu_cl_tf_nzscore.RData",sep="")) 


rm(vra_wu_cl_tf_nminmax,vra_wu_cl_tf_nzscore)
rm(dtminmax,dtzscore)
gc()

