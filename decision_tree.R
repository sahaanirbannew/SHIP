#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Classification using decision tree, analysis and comparision #
#Motive of script: 1. We built 3 Decision trees:                                #
#                     1.1. DT with all features, growing and pruning and        #
#                          collecting base accuracy, preprune acc and prune acc #
#                     1.2  DT with Boruta features, growing and pruning and     #
#                          collecting base accuracy, preprune acc and prune acc # 
#                     1.3  DT with Random forest features, growing and pruning, #
#                          collecting base accuracy, preprune acc and prune acc #
#                  3. Comparision of models based on accuracies and plotting    #                 #
#--------------------------------------------------------------------------------
## @knitr InstallPackages
#Installing packages. 
pkg <- c("naniar", "dplyr", "imputeTS", "rio", "lattice",
         "survival","ggplot2","Hmisc","mice", "colorspace",
         "grid","data.table","VIM","iterators","itertools",
         "missForest", "randomForest", "GGally", "purrr", "varhandle", "readxl",
         "csvy", "feather", "fst", "hexView", "rmatio", "Biocomb", "gtools", 
         "Rcpp", "FSelector", "caret", "mlbench", "corrplot", "MASS", 
         "party", "rpart", "rpart.plot", "randomForest", "pROC", "knitr", "Formula", "backports",
         "Boruta", "DataExplorer", "rfUtilities","parallel", "doParallel")
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
#---------------------End of:import libraries--------------------------------

#---------------------Begin of:loading of Dataset----------------------------
## @knitr LoadDataset
#importing preprocessed dataset from local drive
dataset = readRDS('imputed.rds') 
#summary(dataset$liver_fat)#Checking distribution of labels of target in the preprocessed dataset
df_data_dt<-data.frame(dataset)#converting to data frame

#Creating a list to store info of decision trees for comparision
nam<-c("Model_Name", "Base_Acc","Pre_Prune_Acc","Post_Prune_Acc")
df_acc_2<-data.frame(matrix(ncol = 4, nrow = 0))
colnames(df_acc_2)<-nam

#---------------------End of:loading of Dataset------------------------------

#---------------------Begin of:Decision Tree---------------------------------
## @knitr BeginDecisionTree 
#partioning data into training and test set
row.number <- sample(1:nrow(df_data_dt), 0.8*nrow(df_data_dt))#sampling data to 80%- 20%
train_dt = df_data_dt[row.number,]#80% of the rowa to be ued as training data
test_dt = df_data_dt[-row.number,]#20% of rows to be used as testing data
#dim(train_dt)#to check what number of rows and columns in training data
#dim(test_dt)#to check what number of rows and columns in test data

#1. DT with rpart package
#------------------------
#par(mfrow=c(1,1))
#Base Model with all features and default value of controls
#-----------------------------------------------------------
## @knitr DTRpartPackage
dt1_rpart<-rpart(liver_fat~.,train_dt)

#printcp(dt1_rpart)#Cross Validation

#Plotting Decision trees
#Extra: Display extra information at the nodes
#rpart.plot(dt1_rpart)#No extra information.
#rpart.plot(dt1_rpart,extra = 1)#Display the number of observations that fall in the node
#rpart.plot(dt1_rpart,extra = 2)#Class models: display the classification rate at the node, 
#expressed as the number of correct classifications and 
#the number of observations in the node. 
#rpart.plot(dt1_rpart,extra = 3)#Class models: misclassification rate at the node, 
#expressed as the number of incorrect classifications 
#and the number of observations in the node.
#rpart.plot(dt1_rpart,extra = 4)#Class models: probability per class of observations
#in the node (conditioned on the node, sum across a node is 1).
#rpart.plot(dt1_rpart,extra = 5)# Class models: like 4 but don't display the fitted class.
#rpart.plot(dt1_rpart,extra = 8)#Class models: the probability of the fitted class.

#The summary gives the complete information about the tree,
#The number of nodes, information regarding each node,
#information regarding each split and split conditions,
#Class counts over the data as well as the probabilities of classes at eah node.
#It also gives info about primary split and surrogate splits
#A primary splitting rule : is always calculated by default, and it provides for the 
#                           assignment of observations when the predictor variable is
#                           missing, even when there are no missing values in the 
#                           training data. This allows for the possibility of missing 
#                           values when you are scoring new data
#A surrogate split : The purpose of a surrogate rule is to handle the assignment of 
#                   observations by using an alternative variable that has similar 
#                   predictive ability and has nonmissing values in observations where 
#                   the primary predictor is missing. 
## @knitr rpartSummary
summary(dt1_rpart)


# Examine the complexity plot(cp)
#----------------------------

## @knitr ExamineComplexity
#Printcp():This tells us how the tree performs
#The value of cp should be least, so that the cross-validated error rate is minimum.
#printcp(dt1_rpart)

#Plorcp(): This plots the various cp values.provides a graphical representation to the 
#cross validated error summary. The cp values are plotted against the geometric mean to 
#depict the deviation until the minimum value is reached.
#plotcp(dt1_rpart)

# Compute the accuracy of the rpart tree
test_dt$pred <- predict(dt1_rpart, test_dt, type = "class")
base_accuracy <- mean(test_dt$pred == test_dt$liver_fat)#calculating accuracy of this model
#base_accuracy

# Grow a tree with minsplit of 500 and max depth of 10
dt_rpart_preprun <- rpart(liver_fat ~ ., data = train_dt, method = "class", 
                          control = rpart.control(cp = 0, maxdepth = 10, minsplit = 500))
#printcp(dt_rpart_preprun)#Cross Validation
#rpart.plot(dt_rpart_preprun)
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt_rpart_preprun, test_dt, type = "class")
accuracy_preprun <- mean(test_dt$pred == test_dt$liver_fat)
#accuracy_preprun

#Postpruning
# Prune the base model based on the optimal cp value
dt_rpart_pruned <- prune(dt1_rpart, cp = 0.0084 )
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt_rpart_pruned, test_dt, type = "class")
accuracy_postprun <- mean(test_dt$pred == test_dt$liver_fat)
#data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model1", Base_Acc = base_accuracy, 
                   Pre_Prune_Acc = accuracy_preprun ,Post_Prune_Acc = accuracy_postprun )
df_acc_2<-rbind(df_acc_2,newrow)

#2. DT with rpart package with Boruta Features
#---------------------------------------------
#Base Model with all non rejected features from Boruta method
#------------------------------------------------------------
## @knitr DTBorutaFeatures
dt2_rpart<-rpart(liver_fat ~ age_ship_s0 + som_bmi_s0 + som_tail_s0 + som_huef_s0 + 
                   hrs_s_s0 + asat_s_s0 + alat_s_s0 + ggt_s_s0 + tg_s_s0 + ferri_s0 + 
                   igf1_s0 + stea_alt75_s0 + stea_s0 + age_ship_s1 + som_bmi_s1 + 
                   som_tail_s1 + som_huef_s1 + fib_cl_s1 + hrs_s_s1 + tg_s_s1 + 
                   hdl_s_s1 + hs_crp_s1 + igf1_s1 + diabetes_s2 + smoking_s2 + 
                   abstain_s2 + hyperlipid_s2 + earm_s2 + stea_alt75_s2 + stea_s2 + 
                   atc_c07a_s2 + atc_c07ab_s2 + atc_c08_s2 + atc_c08ca01_s2 + 
                   age_ship0_s0 + diabp_s0 + avs_s0 + mac_s0 + metsyn_s0 + waistc_s0 + 
                   waiidf_s0 + whratc_s0 + antihyp_s0 + mort_time_birth_s0 + 
                   som_gew_s2,
                 train_dt)
#printcp(dt2_rpart)#Cross Validation
#rpart.plot(dt2_rpart)
#rpart.plot(dt2_rpart,extra = 1)
#rpart.plot(dt2_rpart,extra = 2)
#rpart.plot(dt2_rpart,extra = 4)

#The summary gives the complete information about the tree,
#The number of nodes, information regarding each node,
#information regarding each split and split conditions,
#Class counts over the data as well as the probabilities of classes at eah node.
#It also gives info about primary split and surrogate splits
#A primary split and surrogate split 

## @knitr DTBorutaSummary
# Examine the complexity plot
# summary(dt2_rpart)

#Printcp():This tells us how the tree performs
#The value of cp should be least, so that the cross-validated error rate is minimum.
#printcp(dt2_rpart)

#Plorcp(): This plots the various cp values.provides a graphical representation to the 
#cross validated error summary. The cp values are plotted against the geometric mean to 
#depict the deviation until the minimum value is reached.
plotcp(dt2_rpart)

# Compute the accuracy of the rpart tree
test_dt$pred <- predict(dt2_rpart, test_dt, type = "class")
base_accuracy2 <- mean(test_dt$pred == test_dt$liver_fat)
base_accuracy2

# Grow a tree with minsplit of 200 and max depth of 10
## @knitr DTBoruta200d10
dt2_rpart <- rpart(liver_fat ~ age_ship_s0 + som_bmi_s0 + som_tail_s0 + som_huef_s0 + 
                     hrs_s_s0 + asat_s_s0 + alat_s_s0 + ggt_s_s0 + tg_s_s0 + ferri_s0 + 
                     igf1_s0 + stea_alt75_s0 + stea_s0 + age_ship_s1 + som_bmi_s1 + 
                     som_tail_s1 + som_huef_s1 + fib_cl_s1 + hrs_s_s1 + tg_s_s1 + 
                     hdl_s_s1 + hs_crp_s1 + igf1_s1 + diabetes_s2 + smoking_s2 + 
                     abstain_s2 + hyperlipid_s2 + earm_s2 + stea_alt75_s2 + stea_s2 + 
                     atc_c07a_s2 + atc_c07ab_s2 + atc_c08_s2 + atc_c08ca01_s2 + 
                     age_ship0_s0 + diabp_s0 + avs_s0 + mac_s0 + metsyn_s0 + waistc_s0 + 
                     waiidf_s0 + whratc_s0 + antihyp_s0 + mort_time_birth_s0 + 
                     som_gew_s2,
                   data = train_dt, method = "class", 
                   control = rpart.control(cp = 0, maxdepth = 10,minsplit = 200))
#printcp(dt2_rpart)#Cross Validation
#rpart.plot(dt2_rpart)#Plot
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt2_rpart, test_dt, type = "class")
accuracy2 <- mean(test_dt$pred == test_dt$liver_fat)
#accuracy2

# Grow a tree with minsplit of 100 and max depth of 10
## @knitr DTBoruta100d10
dt3_rpart <- rpart(liver_fat ~ age_ship_s0 + som_bmi_s0 + som_tail_s0 + som_huef_s0 + 
                     hrs_s_s0 + asat_s_s0 + alat_s_s0 + ggt_s_s0 + tg_s_s0 + ferri_s0 + 
                     igf1_s0 + stea_alt75_s0 + stea_s0 + age_ship_s1 + som_bmi_s1 + 
                     som_tail_s1 + som_huef_s1 + fib_cl_s1 + hrs_s_s1 + tg_s_s1 + 
                     hdl_s_s1 + hs_crp_s1 + igf1_s1 + diabetes_s2 + smoking_s2 + 
                     abstain_s2 + hyperlipid_s2 + earm_s2 + stea_alt75_s2 + stea_s2 + 
                     atc_c07a_s2 + atc_c07ab_s2 + atc_c08_s2 + atc_c08ca01_s2 + 
                     age_ship0_s0 + diabp_s0 + avs_s0 + mac_s0 + metsyn_s0 + waistc_s0 + 
                     waiidf_s0 + whratc_s0 + antihyp_s0 + mort_time_birth_s0 + 
                     som_gew_s2,
                   data = train_dt, method = "class", 
                   control = rpart.control(cp = 0, maxdepth = 10,minsplit = 100))
#printcp(dt3_rpart)#Cross Validation
#rpart.plot(dt3_rpart)#Plot
# Compute the accuracy of the pruned tree
## @knitr DTBorutaAccu
test_dt$pred <- predict(dt3_rpart, test_dt, type = "class")
accuracy3 <- mean(test_dt$pred == test_dt$liver_fat)
accuracy3

data.frame(base_accuracy2, accuracy2, accuracy3)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model1_Boruta", Base_Acc = base_accuracy2, 
                   Pre_Prune_Acc = accuracy2 ,Post_Prune_Acc = accuracy3 )
df_acc_2<-rbind(df_acc_2,newrow)

#3. DT with rpart package with Important Features from Random Forest
#-------------------------------------------------------------------

## @knitr DTRPartRanForP
print("print hocche.")

w_rf1 <- c("stea_alt75_s2" , "stea_s2" , "stea_s0" , "stea_alt75_s0" , "som_gew_s2" , "som_tail_s1",
           "som_tail_s0" , "som_bmi_s1" , "fib_cl_s1" , "som_huef_s0" , "tg_s_s1" , "som_huef_s1" ,
           "chol_hdl_s0" , "alat_s_s0" , "hrs_s_s0" , "ferri_s0" , "som_bmi_s0" , "crea_s_s0" ,
           "age_ship0_s0" , "tg_s_s0" , "hrs_s_s1" , "op_preg_w_s0" , "pillnow_w_s0" , "waiidf_s0" ,
           "sysbp_s0" , "stmean_s0" , "avmeanapp_s0" , "mac_s0" , "ffs_s0" , "ca_s_s0" , "liver_fat")

#train_rf_var<-train_dt[w_rf1]

#Base Model with all features
dt3_rpart<-rpart(liver_fat ~.,
                 train_dt[w_rf1])
printcp(dt3_rpart)#Cross Validation
#rpart.plot(dt3_rpart)
#rpart.plot(dt3_rpart,extra = 1)
#rpart.plot(dt3_rpart,extra = 2)
#rpart.plot(dt3_rpart,extra = 4)

#summary(dt3_rpart)
# Examine the complexity plot
## @knitr DTExamComplePlot
#printcp(dt3_rpart)
#plotcp(dt3_rpart)

# Compute the accuracy of the rpart tree
## @knitr DTAccuracyRpartTree 
test_dt$pred <- predict(dt3_rpart, test_dt[w_rf1], type = "class")
base_accuracy3 <- mean(test_dt$pred == test_dt$liver_fat)
#base_accuracy3

# Grow a tree with minsplit of 200 and max depth of 10
## @knitr DTMinSplit200D10
dt3_rpart_pre <- rpart(liver_fat ~.,
                       data = train_dt[w_rf1], method = "class", 
                       control = rpart.control(cp = 0, maxdepth = 10,minsplit = 200))
#printcp(dt3_rpart_pre)#Cross Validation
#rpart.plot(dt3_rpart_pre)#Plot
# Compute the accuracy of the pruned tree
## @knitr DTAccurPrunedTree
test_dt$pred <- predict(dt3_rpart_pre, test_dt[w_rf1], type = "class")
accuracy_pre3 <- mean(test_dt$pred == test_dt$liver_fat)
#accuracy_pre3

# Grow a tree with minsplit of 100 and max depth of 10
## @knitr DTRPartRanFor
dt3_rpart_post <- rpart(liver_fat ~.,
                        data = train_dt[w_rf1], method = "class", 
                        control = rpart.control(cp = 0, maxdepth = 10,minsplit = 100))
#printcp(dt3_rpart_post)#Cross Validation
#rpart.plot(dt3_rpart_post)#Plot
# Compute the accuracy of the pruned tree
## @knitr DTRPartRanForAccur
test_dt$pred <- predict(dt3_rpart_post, test_dt[w_rf1], type = "class")
accuracy_post3 <- mean(test_dt$pred == test_dt$liver_fat)
#accuracy_post3

#data.frame(base_accuracy3, accuracy_pre3, accuracy_post3)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model1_RanFor", Base_Acc = base_accuracy3, 
                   Pre_Prune_Acc = accuracy_pre3 ,Post_Prune_Acc = accuracy_post3 )
df_acc_2<-rbind(df_acc_2,newrow)

#df_acc_2
#---------------------End of:Decision Tree-----------------------------------

#---------------------Begin of:Evaluation------------------------------------
## @knitr BeginEval 
#Plot showing comparision of 3 models
#ggplot(df_acc_2) + geom_density(aes(Base_Acc, color = Model_Name)) + 
#  geom_jitter(aes(Base_Acc, 0, color = Model_Name), alpha = 0.5, height = 0.02 )

#hist(df_acc_2,main="Histogram Analysis of Models on OOB Error",
#    xlab="Model_Name", ylab="Accuracy",
#   border="blue", col="green"
#)
#Bar plot comparing 3 models on base accuracy
#DT with BORUTA gave good accuracy
## @knitr EndEval
#barplot(df_acc_2$Base_Acc, main="Decision Tree analysis", xlab="Model Names", ylab="Base Accuracy", 
#        names.arg=c("All","Bor","RF"),
#        border="red", density=c(0.8258, 0.8988, 0.8442))
#---------------------End of:Evaluation--------------------------------------
