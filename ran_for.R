#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Exploring the various phases in data science with R.         #
#Motive of script: Classification using Random Forest and tuning                #
#--------------------------------------------------------------------------------

## @knitr InstallPackages
pkg <- c("naniar", "dplyr", "imputeTS", "rio", "lattice",
         "survival","ggplot2","Hmisc","mice", "colorspace",
         "grid","data.table","VIM","iterators","itertools",
         "missForest", "randomForest", "GGally", "purrr", "varhandle", "readxl",
         "csvy", "feather", "fst", "hexView", "rmatio", "Biocomb", "gtools", 
         "Rcpp", "FSelector", "caret", "mlbench", "corrplot", "MASS", 
         "party", "rpart", "rpart.plot", "randomForest", "pROC", "knitr", "Formula", "backports",
         "Boruta", "DataExplorer", "rfUtilities","parallel","doParallel")
new.pkg <- pkg[!(pkg %in% installed.packages())]
#if It is not previously installed, install it. 
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}


## @knitr LoadPackages
library(naniar)
library(dplyr)
library(imputeTS)
library(rio)
library(lattice)
library(survival)
library(ggplot2)
library(Hmisc)
library(mice)
library(colorspace)
library(grid)
library(data.table)
library(VIM)
library(iterators)
library(itertools)
library(missForest)
library(randomForest)
library(GGally)
library(purrr)
library(varhandle)
library(readxl)
library(csvy)
library(feather)
library(fst)
library(hexView)
library(rmatio)
library(Biocomb)
library(gtools)
library(Rcpp)
library(FSelector)
library(caret)
library(mlbench)
library(corrplot)
library(MASS)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(mice)
library(backports)
library(Boruta)
library(DataExplorer)
library(rfUtilities)
library(parallel)
library(doParallel)
print("hocche print.")
## @knitr LoadDataset
#importing dataset from local drive
dataset = readRDS('imputed.rds') 

df_data_rf<-data.frame(dataset)
#table(df_data_rf$liver_fat)

#Non-Rejected features selected from Boruta
w_non_rej<- c("age_ship_s0", "som_bmi_s0","som_tail_s0" , "som_huef_s0" , 
              "hrs_s_s0" , "asat_s_s0" , "alat_s_s0" , "ggt_s_s0" , "tg_s_s0" , "ferri_s0", 
              "igf1_s0" , "stea_alt75_s0" , "stea_s0" , "age_ship_s1" , "som_bmi_s1" , 
              "som_tail_s1" , "som_huef_s1" , "fib_cl_s1" , "hrs_s_s1" , "tg_s_s1" , 
              "hdl_s_s1" , "hs_crp_s1" , "igf1_s1" , "diabetes_s2" , "smoking_s2" , 
              "abstain_s2" , "hyperlipid_s2" , "earm_s2" , "stea_alt75_s2" , "stea_s2" , 
              "atc_c07a_s2" , "atc_c07ab_s2" , "atc_c08_s2" , "atc_c08ca01_s2" , 
              "age_ship0_s0" , "diabp_s0" , "avs_s0" , "mac_s0" , "metsyn_s0" , "waistc_s0",
              "waiidf_s0" , "whratc_s0" , "antihyp_s0" , "mort_time_birth_s0" , 
              "som_gew_s2")
#Confirmed features selected from Boruta
w_confirmed<- c("age_ship_s0" , "som_bmi_s0" , "som_tail_s0" , "som_huef_s0" , 
                "hrs_s_s0" , "alat_s_s0" , "ggt_s_s0" , "tg_s_s0" , "ferri_s0" , "stea_alt75_s0" ,
                "stea_s0" , "age_ship_s1" , "som_bmi_s1" , "som_tail_s1" , "som_huef_s1" , 
                "fib_cl_s1" , "hrs_s_s1" , "tg_s_s1","hs_crp_s1" , "igf1_s1" , "diabetes_s2" , 
                "smoking_s2" , "abstain_s2" , "hyperlipid_s2" , "earm_s2" , "stea_alt75_s2" , 
                "stea_s2" , "atc_c07a_s2" , "atc_c07ab_s2" , "atc_c08_s2" , "atc_c08ca01_s2" , 
                "age_ship0_s0" , "diabp_s0" , "avs_s0" , "mac_s0" , "metsyn_s0", "waistc_s0" , 
                "waiidf_s0" , "antihyp_s0",  "mort_time_birth_s0" , "som_gew_s2")

#1. We would run the random forest classification algorithm with dafault mtry and ntree
#   values.
#2. Run random forest 10 times with different values of ntree and observe OOB error
#3. Run random forest 10 times with different values of mtry and observe OOB errors
#4. Analyze th OOB missclassification error rate for every run and plot the optimum
#   ntree and mtry value where OOB error stabilizes and minimies

## @knitr ModelAlgoRandomForest
#Creating a list to store info of OOB error of different random forests 
nam<-c("Model_Name", "mtry","ntree","OOB_Err", "Acc_train", "Acc_test")
df_acc<-data.frame(matrix(ncol = 6, nrow = 0))
#colnames(df_acc)<-nam

#Data Partition for training and test set
set.seed(123)
#smpling data: 80%(training), 20%(test)
row.number <- sample(1:nrow(df_data_rf), 0.8*nrow(df_data_rf))
train_rf = df_data_rf[row.number,]
test_rf= df_data_rf[-row.number,]
#dim(train_rf)
#dim(test_rf)


## @knitr defMTreeNTreeVal
#1. Random Forest with default mtry and ntree value
rf1<- randomForest(liver_fat~.,data = train_rf)
#print(rf1)
#attributes(rf1)
#rf1$confusion
#Cross validation with 0 fold
rf1.cv <- rf.crossValidation(rf1, train_rf, p=0.10, n=10) 
#print(rf1.cv)
#attributes(rf1.cv)
#rf1.cv<-data.frame(rf1.cv)#convertig to data frame

# Plot cross validation verses model producers accuracy
#par(mfrow=c(1,2)) 
#print("Fig: Cross Validation Producers Accuracy.")
#plot(rf1.cv, type = "cv", main = "CV producers accuracy")
#print("Fig: Model Producers Accuracy.")
#plot(rf1.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model oob
#par(mfrow=c(1,2)) 
#print("Fig: Cross Validation vs Model OOB.")
#plot(rf1.cv, type = "cv", stat = "oob", main = "CV oob error")
#print("Fig: Model OOB Error.")
#plot(rf1.cv, type = "model", stat = "oob", main = "Model oob error")

## @knitr Predict
#prediction
p1_rf<-predict(rf1,test_rf)
p2_rf<-predict(rf1,train_rf)
#print(p1_rf)
#head(p1_rf)

library(e1071)
#confusion matrix
#confusionMatrix(p2_rf,train_rf$liver_fat)
#confusionMatrix(p1_rf,test_rf$liver_fat)

#error rate for random forest
#plot(rf1)
#Evaluate variable importance
#varImpPlot(rf1)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "DefaultRF", mtry = 18, ntree = 500,
                   OOB_Err = 16.67,Acc_train = 1, Acc_test = 0.8258 )
df_acc<-rbind(df_acc,newrow)


## @knitr defMTryNTreeVal2
#2. Tuning Random forest with different values of MTRY and observe OOB error


#Finding the optimal mtry value by tuning RF mtry value with minimum out of bag(OOB) error

mtry <- tuneRF(train_rf[-346],train_rf$liver_fat, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best_mtry <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
#print(mtry)
#print(best_mtry)

#Building model again using best mtry value found in tuning

rf1_mtry<- randomForest(liver_fat~.,data = train_rf, mtry = best_mtry,
                        importance = TRUE, ntree = 500)
#print(rf1_mtry)
#rf1_mtry$confusion
#Cross validation with 0 fold
rf1_mtry.cv <- rf.crossValidation(rf1_mtry, train_rf, p=0.10, n=10) 
#print(rf1_mtry.cv)
#rf1.cv<-data.frame(rf1.cv)#convertig to data frame

# Plot cross validation verses model producers accuracy
#par(mfrow=c(1,2)) 
#plot(rf1_mtry.cv, type = "cv", main = "CV producers accuracy")
#plot(rf1_mtry.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model oob
#par(mfrow=c(1,2)) 
#plot(rf1_mtry.cv, type = "cv", stat = "oob", main = "CV oob error")
#plot(rf1_mtry.cv, type = "model", stat = "oob", main = "Model oob error")

#prediction
p2_rf_mtry<-predict(rf1_mtry,train_rf)
p1_rf_mtry<-predict(rf1_mtry,test_rf)
#print(p1_rf_mtry)
#head(p1_rf_mtry)

library(e1071)
#confusion matrix
#confusionMatrix(p2_rf_mtry,train_rf$liver_fat)
#confusionMatrix(p1_rf_mtry,test_rf$liver_fat)

#error rate for random forest
#plot(rf1_mtry)
#Evaluate variable importance
#varImpPlot(rf1_mtry)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Tune_MTRY_RF", mtry = 40, ntree = 500,
                   OOB_Err = 14.97,Acc_train = 1, Acc_test = 0.8427 )
df_acc<-rbind(df_acc,newrow)

#3. Tuning Random forest with different values of NTREE and observe OOB error
## @knitr defMTryNTreeVal3
#Manual search by creating 10 folds and repeating 3 times
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')
#creating tunegrid
tunegrid <- expand.grid(.mtry = c(sqrt(ncol(train_rf))))
modellist <- list()

#train with different ntree parameters
for (ntree in c(700,1000,1500)){
  set.seed(123)
  fit <- train(liver_fat~.,
               data = train_rf,
               method = 'rf',
               metric = 'Accuracy',
               tuneGrid = tunegrid,
               trControl = control,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

#Compare results
results <- resamples(modellist)
#summary(results)

#Maximum Accuracy when NTREE = 700

#Building model again using best mtry value found in tuning

rf1_ntree<- randomForest(liver_fat~.,data = train_rf,
                         importance = TRUE, ntree = 700)
#print(rf1_ntree)
rf1_ntree$confusion
#Cross validation with 0 fold
rf1_ntree.cv <- rf.crossValidation(rf1_ntree, train_rf, p=0.10, n=10) 
#print(rf1_ntree.cv)
#rf1.cv<-data.frame(rf1.cv)#convertig to data frame

# Plot cross validation verses model producers accuracy
#par(mfrow=c(1,2)) 
#plot(rf1_ntree.cv, type = "cv", main = "CV producers accuracy")
#plot(rf1_ntree.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model oob
#par(mfrow=c(1,2)) 
#plot(rf1_ntree.cv, type = "cv", stat = "oob", main = "CV oob error")
#plot(rf1_ntree.cv, type = "model", stat = "oob", main = "Model oob error")

#prediction
p2_rf_ntree<-predict(rf1_ntree,train_rf)
p1_rf_ntree<-predict(rf1_ntree,test_rf)
#print(p1_rf_ntree)
#head(p1_rf_ntree)

library(e1071)
#confusion matrix
#confusionMatrix(p2_rf_ntree,train_rf$liver_fat)
#confusionMatrix(p1_rf_ntree,test_rf$liver_fat)

#error rate for random forest
#plot(rf1_ntree)
#Evaluate variable importance
#varImpPlot(rf1_ntree)

#Entering data in table df_acc
## @knitr defMTryNTreeVal4
newrow<-data.frame(Model_Name = "Tune_NTREE_RF", mtry = 18, ntree = 700,
                   OOB_Err = 16.53,Acc_train = 1, Acc_test = 0.8371 )
df_acc<-rbind(df_acc,newrow)

#4. Building Random forest with optimized values of MTRY and NTREE

rf1_op<- randomForest(liver_fat~.,data = train_rf, mtry = 40,
                      importance = TRUE, ntree = 700)
#print(rf1_op)
rf1_op$confusion
#Cross validation with 0 fold
rf1_op.cv <- rf.crossValidation(rf1_op, train_rf, p=0.10, n=10) 
#print(rf1_op.cv)
#rf1.cv<-data.frame(rf1.cv)#convertig to data frame

# Plot cross validation verses model producers accuracy
#par(mfrow=c(1,2)) 
#plot(rf1_op.cv, type = "cv", main = "CV producers accuracy")
#plot(rf1_op.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model oob
#par(mfrow=c(1,2)) 
#plot(rf1_op.cv, type = "cv", stat = "oob", main = "CV oob error")
#plot(rf1_op.cv, type = "model", stat = "oob", main = "Model oob error")

#prediction
p1_rf_op<-predict(rf1_op,test_rf)
p2_rf_op<-predict(rf1_op,train_rf)
#print(p1_rf_op)
#head(p1_rf_op)

library(e1071)
#confusion matrix
#confusionMatrix(p2_rf_op,train_rf$liver_fat)
#confusionMatrix(p1_rf_op,test_rf$liver_fat)

#error rate for random forest
#plot(rf1_op)
#Evaluate variable importance
#varImpPlot(rf1_op)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Tune_OP", mtry = 40, ntree = 700,
                   OOB_Err = 15.11,Acc_train = 1, Acc_test = 0.8427 )
df_acc<-rbind(df_acc,newrow)

#---------------------End of Modelling Algo1: Random Forest------------------
#---------------------Begin of Evaluation------------------------------------

#ggplot(df_acc, aes(x = Model_Name, y = OOB_Err, colour = variable)) + 
#  geom_line() + 
#  ylab(label="Model Name") + 
#  xlab("Out of Bag Error") + 
#  scale_colour_manual(values=c("grey", "blue"))
#---------------------End of Evaluation--------------------------------------