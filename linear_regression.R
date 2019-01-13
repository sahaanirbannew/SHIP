#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Exploring the various phases in data science with R.         #
#Motive of script: Linear Regression and analysis                               #
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
#We need the target variable for regression in numeric
dataset1 = readRDS('C:/Users/Anirban/Documents/DataScience with R/181121_ship.rds')
dataset1<-data.frame(dataset1)
dataset1 <- dataset1[!is.na(dataset1$liver_fat),]
dataset$liver_fat<-as.character(dataset1$liver_fat)
dataset$liver_fat<-dataset1$liver_fat

df_data_lin<-data.frame(dataset)

#---------------------End of:loading of Dataset------------------------------

#---------------------Begin of:Selecting two sets of Features----------------
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
              "som_gew_s2","liver_fat")

#Confirmed features selected from Boruta
w_confirmed<- c("age_ship_s0" , "som_bmi_s0" , "som_tail_s0" , "som_huef_s0" , 
                "hrs_s_s0" , "alat_s_s0" , "ggt_s_s0" , "tg_s_s0" , "ferri_s0" , "stea_alt75_s0" ,
                "stea_s0" , "age_ship_s1" , "som_bmi_s1" , "som_tail_s1" , "som_huef_s1" , 
                "fib_cl_s1" , "hrs_s_s1" , "tg_s_s1","hs_crp_s1" , "igf1_s1" , "diabetes_s2" , 
                "smoking_s2" , "abstain_s2" , "hyperlipid_s2" , "earm_s2" , "stea_alt75_s2" , 
                "stea_s2" , "atc_c07a_s2" , "atc_c07ab_s2" , "atc_c08_s2" , "atc_c08ca01_s2" , 
                "age_ship0_s0" , "diabp_s0" , "avs_s0" , "mac_s0" , "metsyn_s0", "waistc_s0" , 
                "waiidf_s0" , "antihyp_s0",  "mort_time_birth_s0" , "som_gew_s2","liver_fat")

#---------------------End of:Selecting two sets of Features-------------------

#---------------------Begin of:Dividing data into training and test set-------
row.number <- sample(1:nrow(df_data_lin), 0.8*nrow(df_data_lin))
train_lin = df_data_lin[row.number,]
test_lin = df_data_lin[-row.number,]
dim(train_lin)
dim(test_lin)

#Creating a list to store info of linear models 
nam<-c("Model_Name", "p_Value","AIC","BIC", "RSq", "Adjust_RSq")
df_acc_l<-data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_acc_l)<-nam
#---------------------End of:Dividing data into training and test set---------

#---------------------Begin of : Linear Regression----------------------------
##Here we intedd to do multiple linear regression
#1. MLR with all attributes , check p-Value, AIC, BIC
#2. MLR with reduced attributes from step1, check p-Value, AIC, BIC(with ANOVA)
#3. MLR with Boruta features, check p-Value, AIC, BIC
#4. MLR with reduced Boruta attributes from step3, check p-Value, AIC, BIC(with ANOVA)
#5. Analysis of performance of all the above 4 models

#Explore the data.

#Scatter plots :To visualize linear relationship between dependant and independant vars

scatter.smooth(x=train_lin$som_bmi_s0, y = train_lin$liver_fat, main = "BMI~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin$som_tail_s0, y = train_lin$som_tail_s0, main = "som_tail_s0~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin$hrs_s_s0, y = train_lin$liver_fat, main = "Sleep~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin$stea_alt75_s0, y = train_lin$liver_fat, main = "stea_alt75_s0~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin$diabetes_s2, y = train_lin$liver_fat, main = "Diabetes~LiverFat",col=c("green","red"))

ggplot(train_lin, aes(liver_fat)) + geom_density(fill="blue")

#Density Graph: To check if the response var is close to normality

plot(density(train_lin$som_bmi_s0), main="Density Plot: BMI", ylab="Liver_fat", sub=paste("Skewness:", 
                                                                                           round(e1071::skewness(train_lin$som_bmi_s0), 2)))  # density plot for 'BMI'
polygon(density(train_lin$som_bmi_s0), col="pink")

plot(density(train_lin$ferri_s0), main="Density Plot: ferri_s0", ylab="Liver_Fat", sub=paste("Skewness:",
                                                                                              round(e1071::skewness(train_lin$ferri_s0), 2)))  # density plot for 'Diabetes'
polygon(density(train_lin$ferri_s0), col="yellow")

#1.MLR with all attributes , check p-Value, AIC, BIC

#anirban: throws error after this point.
model1 = lm(liver_fat~., data=train_lin)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
confint(model1)
AIC (model1)
BIC(model1)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model1", p_Value = 2.2, AIC = 4169.958,
                   BIC = 6159.183 ,RSq = 85.96, Adjust_RSq = 63.65 )
df_acc_l<-rbind(df_acc_l,newrow)

#2.MLR with reduced attributes from step1, check p-Value, AIC, BIC#Error

##For reduced features we would select features with both 2 stars and 1 star given my model1
mod1_red<-c("fruehgeb_w_s02", "gout_s0None", "asat_s_s0", "alat_s_s0", "chol_s_s0",
            "atc_c09aa02_s0", "age_ship_s1", "diabetes_s12", "som_huef_s1",
            "quick_s1", "fib_cl_s1", "hs_crp_s1", "atc_c09aa05_s12","liver_fat")

train_lin2<-data.frame(train_lin[mod1_red])

model2 = lm(liver_fat~atc_c09aa05_s12, data=train_lin)
summary(model2)
par(mfrow=c(2,2))
plot(model2)
confint(model2)
AIC (model2)
BIC(model2)


#3. MLR with Boruta features, check p-Value, AIC, BIC
#-------------------EDA+Linear regression with Boruta Features---------------------
train_lin3<-train_lin[w_non_rej]
ncol(train_lin1)

#Linear Model
model3 = lm(liver_fat ~., data = train_lin3)
print(model3)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
confint(model3)
AIC (model3)
BIC(model3)

#---------------------End of : Linear Regression------------------------------