#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Exploring the various phases in data science with R.         #
#Motive of script: Decision Tree Classification                                 #
#--------------------------------------------------------------------------------
#---------------------Begin of:import libraries----------------------------------
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

#importing dataset from local drive
dataset = readRDS('imputed.rds') 
summary(dataset$liver_fat)
df_data_dt<-data.frame(dataset)

#---------------------End of:loading of Dataset------------------------------

#---------------------Begin of:Decision Tree---------------------------------

#partioning data into training and test set
row.number <- sample(1:nrow(df_data_dt), 0.8*nrow(df_data_dt))
train_dt = df_data_dt[row.number,]
test_dt = df_data_dt[-row.number,]
dim(train_dt)
dim(test_dt)

#1. DT with rpart package
#------------------------
par(mfrow=c(1,1))
#Base Model with all features
dt1_rpart<-rpart(liver_fat~.,train_dt)
printcp(dt1_rpart)#Cross Validation
rpart.plot(dt1_rpart)
rpart.plot(dt1_rpart,extra = 1)
#rpart.plot(dt1_rpart,extra = 2) error extra=4 is legal only for "class" models (you have an "anova" model)
#rpart.plot(dt1_rpart,extra = 4)

summary(dt1_rpart)
# Examine the complexity plot
printcp(dt1_rpart)
plotcp(dt1_rpart)

# Compute the accuracy of the rpart tree
test_dt$pred <- predict(dt1_rpart, test_dt, type = "class")
base_accuracy <- mean(test_dt$pred == test_dt$liver_fat)
base_accuracy

#Misclassification error for train data for party package
#mis_err_train<-table(predict(dt1_rpart),train_dt$liver_fat)
#print(mis_err_train)
#1-sum(diag(mis_err_train))/sum(mis_err_train)#misclassification rate

#Misclassification error for test data rpart package
#test_pred_rpart<-predict(dt1_rpart,newdata=test_dt)
#mis_err_test<-table(test_pred_rpart, test_dt$liver_fat)
#print(mis_err_test)
#1-sum(diag(mis_err_test))/sum(mis_err_test)#misclassification rate
#------------------------------------------------------------------

# Grow a tree with minsplit of 500 and max depth of 8
dt_rpart_preprun <- rpart(liver_fat ~ ., data = train_dt, method = "class", 
                          control = rpart.control(cp = 0, maxdepth = 10,minsplit = 500))
printcp(dt_rpart_preprun)#Cross Validation
rpart.plot(dt_rpart_preprun)#Plot
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt_rpart_preprun, test_dt, type = "class")
accuracy_preprun <- mean(test_dt$pred == test_dt$liver_fat)
accuracy_preprun

#Postpruning
# Prune the base model based on the optimal cp value
dt_rpart_pruned <- prune(dt1_rpart, cp = 0.0084 )
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt_rpart_pruned, test_dt, type = "class")
accuracy_postprun <- mean(test_dt$pred == test_dt$liver_fat)
data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)

#1. DT with rpart package with Boruta Features
#---------------------------------------------
#Base Model with all features
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
printcp(dt2_rpart)#Cross Validation
rpart.plot(dt2_rpart)
rpart.plot(dt2_rpart,extra = 1)
rpart.plot(dt2_rpart,extra = 2)
rpart.plot(dt2_rpart,extra = 4)

summary(dt2_rpart)
# Examine the complexity plot
printcp(dt2_rpart)
plotcp(dt2_rpart)

# Compute the accuracy of the rpart tree
test_dt$pred <- predict(dt2_rpart, test_dt, type = "class")
base_accuracy2 <- mean(test_dt$pred == test_dt$liver_fat)
base_accuracy2

# Grow a tree with minsplit of 200 and max depth of 10
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
printcp(dt2_rpart)#Cross Validation
rpart.plot(dt2_rpart)#Plot
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt2_rpart, test_dt, type = "class")
accuracy2 <- mean(test_dt$pred == test_dt$liver_fat)
accuracy2

# Grow a tree with minsplit of 100 and max depth of 10
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
printcp(dt3_rpart)#Cross Validation
rpart.plot(dt3_rpart)#Plot
# Compute the accuracy of the pruned tree
test_dt$pred <- predict(dt3_rpart, test_dt, type = "class")
accuracy3 <- mean(test_dt$pred == test_dt$liver_fat)
accuracy3
#---------------------End of:Decision Tree-----------------------------------

#---------------------Begin of:Evaluation------------------------------------

#---------------------End of:Evaluation--------------------------------------

