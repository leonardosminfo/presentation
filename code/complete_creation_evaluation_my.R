source(file=paste(path,"code/myPreprocessing.R",sep=""))
source(file=paste(path,"code/myPrediction.R",sep=""))
source(file=paste(path,"code/loading_training_test_data.R",sep=""))
##############
loadlibrary("dplyr")
loadlibrary("caret")
loadlibrary("dplyr")
loadlibrary("tidyr")
loadlibrary("stringr")
loadlibrary("ggplot2")
loadlibrary("corrplot")
loadlibrary("DMwR")

#MACHINE LEARNING PACKAGE
#loadlibrary("mlr")
##############
#CONFIGURING PARALLELISM
#set parallel backend (Windows)
loadlibrary("parallelMap")
loadlibrary("parallel")
set.seed(1991)
parallelStartSocket(cpus = (detectCores()-1))

#######################
#LOADING TRAINING DATA
#######################

#loading matrix with samples

#balancing type/normalization/reduction/name of training variable /name of testing variable
#1=> IM- IMBALANCED/BO- BALANCE OVERSAMPLING/BS- BALANCE SUBSAMPLING
#2=> MM- MINMAX/ZS- ZSCORE
#3=> LS/IG/CFS/PCA
#4=> IMP/WHIM

#load table with data to load
m_tr_tst=load.samples()

#select the data by parameter
#m_tr_tst_selected=select.data.samples(m_tr_tst,btype='ALL',norm='MM',reduc='PCA',imputed='ALL')
m_tr_tst_selected=select.data.samples(m_tr_tst,btype='ALL',norm='MM',reduc='LS',imputed='IMP')

#return the file names of data
files=file.data.samples(m_tr_tst_selected)


#load the data
load.data.samples(files[1],files[2])

files=as.data.frame(files)

#########HYPERPARAMETER OPTIMIZATION#########################

mynnet = class_mlp_nnet(vra_wu_tr_minmax_ls, "delayed")
head(mynnet$predictions)
mynnet$cmTrain
test <- class_test(mynnet$model, vra_wu_cl_tf_nminmax_tst[,names(vra_wu_tr_minmax_ls)], "delayed", predtype = mynnet$predtype)
head(test$predictions)
test$cmTest

mynnet$predictions$X0

my.pred <- mynnet$prediction$X1
#my.pred.class <- as.integer(mynnet$predictions$X1 > mynnet$predictions$X2 & mynnet$predictions$X1 > mynnet$predictions$X3)
my.pred.class <- as.integer(mynnet$predictions$X1>mynnet$predictions$X0)
my.true <- mynnet$predictions$X1.1

my.acc <- Accuracy(y_pred = my.pred.class, y_true = my.true)
my.f1 <- F1_Score(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.sens <- Sensitivity(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.spec <- Specificity(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.prec <- Precision(y_pred = my.pred.class, y_true = my.true, positive = "1")
my.rec <- Recall(y_pred = my.pred.class, y_true = my.true, positive = "1")

my.roc.pred <- prediction(my.pred, my.true)
my.roc.perf <- performance(my.roc.pred, "tpr", "fpr")
options(repr.plot.width=4, repr.plot.height=4)
plot(my.roc.perf)
my.auc <- performance(my.roc.pred, "auc")@y.values[[1]]

parallelStop()

