loadlibrary("dplyr")

load.samples<-function(){
m_tr_tst=matrix(ncol=6,nrow=48)
colnames(m_tr_tst)<-c('balancingtype','normalization','reduction','imputed','nameoftrainingvariable','nameoftestingvariable')

#balancing type/normalization/reduction/name of training variable /name of testing variable
#1=> IM- IMBALANCED/BO- BALANCE OVERSAMPLING/BS- BALANCE SUBSAMPLING
#2=> MM- MINMAX/ZS- ZSCORE
#3=> LS/IG/CFS/PCA
#4=> IMP/WIMP


#IMABALANCED
#MINMAX
#LASSO
m_tr_tst[1,]=c('IM','MM','LS','WHIM','vra_wu_tr_minmax_ls','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[2,]=c('IM','MM','LS','IMP','vra_wu_imp_minmax_tr_ls','vra_wu_cl_imp_tf_nminmax_tst')
#IG
m_tr_tst[3,]=c('IM','MM','IG','WHIM','vra_wu_tr_minmax_ig','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[4,]=c('IM','MM','IG','IMP','vra_wu_imp_minmax_tr_ig','vra_wu_cl_imp_tf_nminmax_tst')
#CFS
m_tr_tst[5,]=c('IM','MM','CFS','WHIM','vra_wu_tr_minmax_cfs','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[6,]=c('IM','MM','CFS','IMP','vra_wu_imp_minmax_tr_cfs','vra_wu_cl_imp_tf_nminmax_tst')
#PCA
m_tr_tst[7,]=c('IM','MM','PCA','WHIM','vra_wu_tr_minmax_pca','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[8,]=c('IM','MM','PCA','IMP','vra_wu_imp_minmax_tr_pca','vra_wu_cl_imp_tf_nminmax_tst')


#zscore
#LASSO
m_tr_tst[9,]=c('IM','ZS','LS','WHIM','vra_wu_tr_zscore_ls','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[10,]=c('IM','ZS','LS','IMP','vra_wu_imp_zscore_tr_ls','vra_wu_cl_imp_tf_nzscore_tst')
#IG
m_tr_tst[11,]=c('IM','ZS','IG','WHIM','vra_wu_tr_zscore_ig','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[12,]=c('IM','ZS','IG','IMP','vra_wu_imp_zscore_tr_ig','vra_wu_cl_imp_tf_nzscore_tst')
#CFS
m_tr_tst[13,]=c('IM','ZS','CFS','WHIM','vra_wu_tr_zscore_cfs','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[14,]=c('IM','ZS','CFS','IMP','vra_wu_imp_zscore_tr_cfs','vra_wu_cl_imp_tf_nzscore_tst')
#PCA
m_tr_tst[15,]=c('IM','ZS','PCA','WHIM','vra_wu_tr_zscore_pca','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[16,]=c('IM','ZS','PCA','IMP','vra_wu_imp_zscore_tr_pca','vra_wu_cl_imp_tf_nzscore_tst')




#BALANCE OVERSAMPLING
#MINMAX
#LASSO
m_tr_tst[17,]=c('BO','MM','LS','WHIM','vra_wu_tr_mm_ls.bo','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[18,]=c('BO','MM','LS','IMP','vra_wu_imp_mm_tr_ls.bo','vra_wu_cl_imp_tf_nminmax_tst')
#IG
m_tr_tst[19,]=c('BO','MM','IG','WHIM','vra_wu_tr_mm_ls.bo','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[20,]=c('BO','MM','IG','IMP','vra_wu_imp_mm_tr_ls.bo','vra_wu_cl_imp_tf_nminmax_tst')
#CFS
m_tr_tst[21,]=c('BO','MM','CFS','WHIM','vra_wu_tr_mm_ls.bo','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[22,]=c('BO','MM','CFS','IMP','vra_wu_imp_mm_tr_ls.bo','vra_wu_cl_imp_tf_nminmax_tst')
#PCA
m_tr_tst[23,]=c('BO','MM','PCA','WHIM','vra_wu_tr_mm_ls.bo','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[24,]=c('BO','MM','PCA','IMP','vra_wu_imp_mm_tr_ls.bo','vra_wu_cl_imp_tf_nminmax_tst')

#ZSCORE
#LASSO
m_tr_tst[25,]=c('BO','ZS','LS','WHIM','vra_wu_tr_zs_ls.bo','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[26,]=c('BO','ZS','LS','IMP','vra_wu_imp_zs_tr_ls.bo','vra_wu_cl_imp_tf_nzscore_tst')
#IG
m_tr_tst[27,]=c('BO','ZS','IG','WHIM','vra_wu_tr_zs_ls.bo','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[28,]=c('BO','ZS','IG','IMP','vra_wu_imp_zs_tr_ls.bo','vra_wu_cl_imp_tf_nzscore_tst')
#CFS
m_tr_tst[29,]=c('BO','ZS','CFS','WHIM','vra_wu_tr_zs_ls.bo','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[30,]=c('BO','ZS','CFS','IMP','vra_wu_imp_zs_tr_ls.bo','vra_wu_cl_imp_tf_nzscore_tst')
#PCA
m_tr_tst[31,]=c('BO','ZS','PCA','WHIM','vra_wu_tr_zs_ls.bo','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[32,]=c('BO','ZS','PCA','IMP','vra_wu_imp_zs_tr_ls.bo','vra_wu_cl_imp_tf_nzscore_tst')



#BALANCE SUBSAMPLING
#MINMAX
#LASSO
m_tr_tst[33,]=c('BS','MM','LS','WHIM','vra_wu_tr_mm_ls.bs','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[34,]=c('BS','MM','LS','IMP','vra_wu_imp_mm_tr_ls.bs','vra_wu_cl_imp_tf_nminmax_tst')
#IG
m_tr_tst[35,]=c('BS','MM','IG','WHIM','vra_wu_tr_mm_ls.bs','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[36,]=c('BS','MM','IG','IMP','vra_wu_imp_mm_tr_ls.bs','vra_wu_cl_imp_tf_nminmax_tst')
#CFS
m_tr_tst[37,]=c('BS','MM','CFS','WHIM','vra_wu_tr_mm_ls.bs','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[38,]=c('BS','MM','CFS','IMP','vra_wu_imp_mm_tr_ls.bs','vra_wu_cl_imp_tf_nminmax_tst')
#PCA
m_tr_tst[39,]=c('BS','MM','PCA','WHIM','vra_wu_tr_mm_ls.bs','vra_wu_cl_tf_nminmax_tst')
m_tr_tst[40,]=c('BS','MM','PCA','IMP','vra_wu_imp_mm_tr_ls.bs','vra_wu_cl_imp_tf_nminmax_tst')

#ZSCORE
#LASSO
m_tr_tst[41,]=c('BS','ZS','LS','WHIM','vra_wu_tr_zs_ls.bs','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[42,]=c('BS','ZS','LS','IMP','vra_wu_imp_zs_tr_ls.bs','vra_wu_cl_imp_tf_nzscore_tst')
#IG
m_tr_tst[43,]=c('BS','ZS','IG','WHIM','vra_wu_tr_zs_ls.bs','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[44,]=c('BS','ZS','IG','IMP','vra_wu_imp_zs_tr_ls.bs','vra_wu_cl_imp_tf_nzscore_tst')
#CFS
m_tr_tst[45,]=c('BS','ZS','CFS','WHIM','vra_wu_tr_zs_ls.bs','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[46,]=c('BS','ZS','CFS','IMP','vra_wu_imp_zs_tr_ls.bs','vra_wu_cl_imp_tf_nzscore_tst')
#PCA
m_tr_tst[47,]=c('BS','ZS','PCA','WHIM','vra_wu_tr_zs_ls.bs','vra_wu_cl_tf_nzscore_tst')
m_tr_tst[48,]=c('BS','ZS','PCA','IMP','vra_wu_imp_zs_tr_ls.bs','vra_wu_cl_imp_tf_nzscore_tst')

return(m_tr_tst)
}

select.data.samples<-function(m_tr_tst,btype,norm,reduc,imputed){
  
    if(btype=='ALL')    btype=c('IM','BO','BS')
    if(norm=='ALL')     norm=c('MM','ZS')
    if(reduc=='ALL')    reduc=c('LS','IG','CFS','PCA')
    if(imputed=='ALL')  imputed=c('WHIM','IMP')
  
    
  #select
    
    selected=m_tr_tst[m_tr_tst[,'balancingtype'] %in% btype &
               m_tr_tst[,'normalization'] %in% norm &
               m_tr_tst[,'imputed'] %in% imputed &
               m_tr_tst[,'reduction'] %in% reduc,]
#selected=(as.data.frame(m_tr_tst)%>%
#  filter(m_tr_tst[,'balancingtype'] %in% btype &
#         m_tr_tst[,'normalization'] %in% norm &
#         m_tr_tst[,'imputed'] %in% imputed &
#         m_tr_tst[,'reduction'] %in% reduc))

return(selected)
}




file.data.samples<-function(data){
  
  #load all training and testing data passed
  
  #View(data)
  
  training_variable=as.data.frame(data[,'nameoftrainingvariable'])%>%
    distinct()
  
  
  testing_variable=as.data.frame(data[,'nameoftestingvariable'])%>%
    distinct()
  
  return(list(training_variable,testing_variable))
}  

load.data.samples<-function(training,testing){
  
  #load all training and testing data passed
  
  #training
 for(i in 1:nrow(as.data.frame(training)))
 {
      assign(as.character(as.data.frame(training)[i,1]),
          readRDS(paste(path,
         paste(paste("dataset/",as.character(as.data.frame(training)[1,1]),sep=""),
               ".RData",sep="")
         ,sep="")),envir = .GlobalEnv)
   
   print(as.character(as.data.frame(training)[i,1]))
   
 }
  
  #testing
  for(i in 1:nrow(as.data.frame(testing)))
  {
    assign(as.character(as.data.frame(testing)[i,1]),
           readRDS(paste(path,
                         paste(paste("dataset/",as.character(as.data.frame(testing)[1,1]),sep=""),
                               ".RData",sep="")
                         ,sep="")),envir = .GlobalEnv)
    
    print(as.character(as.data.frame(testing)[i,1]))
    
  }
  
}  





###IMBALANCED
#LASSO
#vra_wu_tr_minmax_ls=readRDS(paste(path,"dataset/vra_wu_tr_minmax_ls.RData",sep=""))
#vra_wu_imp_minmax_tr_ls=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_ls.RData",sep=""))
#vra_wu_tr_zscore_ls=readRDS(paste(path,"dataset/vra_wu_tr_zscore_ls.RData",sep=""))
#vra_wu_imp_zscore_tr_ls=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_ls.RData",sep=""))

#IG
#vra_wu_tr_minmax_ig=readRDS(paste(path,"dataset/vra_wu_tr_minmax_ig.RData",sep=""))
#vra_wu_imp_minmax_tr_ig=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_ig.RData",sep=""))
#vra_wu_tr_zscore_ig=readRDS(paste(path,"dataset/vra_wu_tr_zscore_ig.RData",sep=""))
#vra_wu_imp_zscore_tr_ig=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_ig.RData",sep=""))

#CFS
#vra_wu_tr_minmax_cfs=readRDS(paste(path,"dataset/vra_wu_tr_minmax_cfs.RData",sep=""))
#vra_wu_imp_minmax_tr_cfs=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_cfs.RData",sep=""))
#vra_wu_tr_zscore_cfs=readRDS(paste(path,"dataset/vra_wu_tr_zscore_cfs.RData",sep=""))
#vra_wu_imp_zscore_tr_cfs=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_cfs.RData",sep=""))

#PCA
#vra_wu_tr_minmax_pca=readRDS(paste(path,"dataset/vra_wu_tr_minmax_pca.RData",sep=""))
#vra_wu_imp_minmax_tr_pca=readRDS(paste(path,"dataset/vra_wu_imp_minmax_tr_pca.RData",sep=""))
#vra_wu_tr_zscore_pca=readRDS(paste(path,"dataset/vra_wu_tr_zscore_pca.RData",sep=""))
#vra_wu_imp_zscore_tr_pca=readRDS(paste(path,"dataset/vra_wu_imp_zscore_tr_pca.RData",sep=""))
########################


###BALANCED  BO/BS
#LASSO
#SMOTE(over)###########
###MINMAX
#vra_wu_tr_mm_ls.bo=readRDS(paste(path,"dataset/vra_wu_tr_mm_ls.bo.Rdata",sep=""))
#vra_wu_imp_mm_tr_ls.bo=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_ls.bo.RData",sep=""))
###ZSCORE#
#vra_wu_tr_zs_ls.bo=readRDS(paste(path,"dataset/vra_wu_tr_zs_ls.bo.RData",sep=""))
#vra_wu_imp_zs_tr_ls.bo=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_ls.bo.RData",sep=""))

#RS(sub)################
###MINMAX
#vra_wu_tr_mm_ls.bs=readRDS(paste(path,"dataset/vra_wu_tr_mm_ls.bs.RData",sep=""))
#vra_wu_imp_mm_tr_ls.bs=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_ls.bs.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_ls.bs=readRDS(paste(path,"dataset/vra_wu_tr_zs_ls.bs.RData",sep=""))
#vra_wu_imp_zs_tr_ls.bs=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_ls.bs.RData",sep=""))
#####################


#######################
#IG
#SMOTE(over)###########
###MINMAX
#vra_wu_tr_mm_ig.bo=readRDS(paste(path,"dataset/vra_wu_tr_mm_ig.bo.Rdata",sep=""))
#vra_wu_imp_mm_tr_ig.bo=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_ig.bo.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_ig.bo=readRDS(paste(path,"dataset/vra_wu_tr_zs_ig.bo.RData",sep=""))
#vra_wu_imp_zs_tr_ig.bo=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_ig.bo.RData",sep=""))

#RS(sub)################
###MINMAX
#vra_wu_tr_mm_ig.bs=readRDS(paste(path,"dataset/vra_wu_tr_mm_ig.bs.RData",sep=""))
#vra_wu_imp_mm_tr_ig.bs=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_ig.bs.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_ig.bs=readRDS(paste(path,"dataset/vra_wu_tr_zs_ig.bs.RData",sep=""))
#vra_wu_imp_zs_tr_ig.bs=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_ig.bs.RData",sep=""))
#####################

#######################
#cfs
#SMOTE(over)###########
###MINMAX
#vra_wu_tr_mm_cfs.bo=readRDS(paste(path,"dataset/vra_wu_tr_mm_cfs.bo.Rdata",sep=""))
#vra_wu_imp_mm_tr_cfs.bo=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_cfs.bo.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_cfs.bo=readRDS(paste(path,"dataset/vra_wu_tr_zs_cfs.bo.RData",sep=""))
#vra_wu_imp_zs_tr_cfs.bo=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_cfs.bo.RData",sep=""))

#RS(sub)################
###MINMAX
#vra_wu_tr_mm_cfs.bs=readRDS(paste(path,"dataset/vra_wu_tr_mm_cfs.bs.RData",sep=""))
#vra_wu_imp_mm_tr_cfs.bs=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_cfs.bs.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_cfs.bs=readRDS(paste(path,"dataset/vra_wu_tr_zs_cfs.bs.RData",sep=""))
#vra_wu_imp_zs_tr_cfs.bs=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_cfs.bs.RData",sep=""))
#####################

#######################
#pca
#SMOTE(over)###########
###MINMAX
#vra_wu_tr_mm_pca.bo=readRDS(paste(path,"dataset/vra_wu_tr_mm_pca.bo.Rdata",sep=""))
#vra_wu_imp_mm_tr_pca.bo=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_pca.bo.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_pca.bo=readRDS(paste(path,"dataset/vra_wu_tr_zs_pca.bo.RData",sep=""))
#vra_wu_imp_zs_tr_pca.bo=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_pca.bo.RData",sep=""))

#RS(sub)################
###MINMAX
#vra_wu_tr_mm_pca.bs=readRDS(paste(path,"dataset/vra_wu_tr_mm_pca.bs.RData",sep=""))
#vra_wu_imp_mm_tr_pca.bs=readRDS(paste(path,"dataset/vra_wu_imp_mm_tr_pca.bs.RData",sep=""))
###ZSCORE
#vra_wu_tr_zs_pca.bs=readRDS(paste(path,"dataset/vra_wu_tr_zs_pca.bs.RData",sep=""))
#vra_wu_imp_zs_tr_pca.bs=readRDS(paste(path,"dataset/vra_wu_imp_zs_tr_pca.bs.RData",sep=""))
#####################


###LOADING TESTING DATA
#LASSO MINMAX
#vra_wu_cl_tf_nminmax_tst=readRDS(paste(path,"dataset/vra_wu_cl_tf_nminmax_tst.RData",sep=""))

#For first test, loading only minmax data
#inbalanced
##LS
##IG
##CFS
##PCA

#balanced
#BO
##LS
##IG
##CFS
##PCA

#BS
##LS
##IG
##CFS
##PCA


