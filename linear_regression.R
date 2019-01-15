#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Linear Regression and analysis                               #
#Motive of script: 1. We built 3 linear regression models                       #
#                  2. Individually analysed each models                         #
#                  3. Did comparision of models using ANOVA                     #
#                  4. Did some model interepretation usling ALE                 #
#--------------------------------------------------------------------------------
#---------------------Begin of:import libraries----------------------------------

## @knitr InstallPackages
pkg <- c("naniar", "dplyr", "imputeTS", "rio", "lattice",
         "survival","ggplot2","Hmisc","mice", "colorspace",
         "grid","data.table","VIM","iterators","itertools",
         "missForest", "randomForest", "GGally", "purrr", "varhandle", "readxl",
         "csvy", "feather", "fst", "hexView", "rmatio", "Biocomb", "gtools", 
         "Rcpp", "FSelector", "caret", "mlbench", "corrplot", "MASS", 
         "party", "rpart", "rpart.plot", "randomForest", "pROC", "knitr", "Formula", "backports",
         "Boruta", "DataExplorer", "rfUtilities","parallel","doParallel", "Metrics")
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
library(Metrics)

## @knitr LoadDataset
#importing preprocessed dataset from local drive
dataset = readRDS('imputed.rds') 
summary(dataset$liver_fat)#Checking distribution of labels of target in the preprocessed dataset

#We need the target variable for regression in numeric
dataset1 = readRDS('181121_ship.rds')
dataset1<-data.frame(dataset1)
dataset1 <- dataset1[!is.na(dataset1$liver_fat),]#keeping only labeled data
dataset$liver_fat<-as.character(dataset1$liver_fat)
dataset$liver_fat<-dataset1$liver_fat

df_data_lin<-data.frame(dataset) #converting to data frame

#Here we accumulate th various subsets of features that we are going to use for 
#creating our linear regression models for comparision

## @knitr ProcessData
#Non-Rejected features selected from Boruta method
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

#Confirmed features selected from Boruta method
w_confirmed<- c("age_ship_s0" , "som_bmi_s0" , "som_tail_s0" , "som_huef_s0" , 
                "hrs_s_s0" , "alat_s_s0" , "ggt_s_s0" , "tg_s_s0" , "ferri_s0" , "stea_alt75_s0" ,
                "stea_s0" , "age_ship_s1" , "som_bmi_s1" , "som_tail_s1" , "som_huef_s1" , 
                "fib_cl_s1" , "hrs_s_s1" , "tg_s_s1","hs_crp_s1" , "igf1_s1" , "diabetes_s2" , 
                "smoking_s2" , "abstain_s2" , "hyperlipid_s2" , "earm_s2" , "stea_alt75_s2" , 
                "stea_s2" , "atc_c07a_s2" , "atc_c07ab_s2" , "atc_c08_s2" , "atc_c08ca01_s2" , 
                "age_ship0_s0" , "diabp_s0" , "avs_s0" , "mac_s0" , "metsyn_s0", "waistc_s0" , 
                "waiidf_s0" , "antihyp_s0",  "mort_time_birth_s0" , "som_gew_s2","liver_fat")

#Important variables from Random forest
rf<- randomForest(liver_fat~.,data = dataset, mtry = 40,
                  importance = TRUE, ntree = 500)
#varImpPlot(rf) #plotting important variables computed by  Random Forest 

#collecting the variables
w_rf1 <- c("stea_alt75_s2" , "stea_s2" , "stea_s0" , "stea_alt75_s0" , "som_gew_s2" , "som_tail_s1",
           "som_tail_s0" , "som_bmi_s1" , "fib_cl_s1" , "som_huef_s0" , "tg_s_s1" , "som_huef_s1" ,
           "chol_hdl_s0" , "alat_s_s0" , "hrs_s_s0" , "ferri_s0" , "som_bmi_s0" , "crea_s_s0" ,
           "age_ship0_s0" , "tg_s_s0" , "hrs_s_s1" , "op_preg_w_s0" , "pillnow_w_s0" , "waiidf_s0" ,
           "sysbp_s0" , "stmean_s0" , "avmeanapp_s0" , "mac_s0" , "ffs_s0" , "ca_s_s0" , "liver_fat")


row.number <- sample(1:nrow(df_data_lin), 0.8*nrow(df_data_lin))#sampling data to 80%- 20%
train_lin = df_data_lin[row.number,]#80% of the rowa to be ued as training data
test_lin = df_data_lin[-row.number,]#20% of rows to be used as testing data
#dim(train_lin)#to check what number of rows and columns in training data
#dim(test_lin)#to check what number of rows and columns in test data

#Creating a data frame to store performance info of linear models to compare eventually
nam<-c("Model_Name", "p_Value","AIC","BIC", "RSq", "RMSE")
df_acc_l<-data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_acc_l)<-nam

## @knitr furtherSteps.
#Here we intedd to do multiple linear regression
#1. MLR with all attributes , check p-Value, AIC, BIC
#2. MLR with Important attributes from Random Forest 
#3. MLR with Boruta features

#Explore the data.

#Scatter plots :To visualize linear relationship between dependant and independant vars
#Here we tried to plot scatter plots to see the reltionship of various individual predictors
#with the target variable
train_lin1<-train_lin
scatter.smooth(x=train_lin1$som_bmi_s0, y = train_lin1$liver_fat, main = "BMI~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin1$som_tail_s0, y = train_lin1$som_tail_s0, main = "som_tail_s0~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin1$hrs_s_s0, y = train_lin1$liver_fat, main = "Sleep~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin1$stea_alt75_s0, y = train_lin1$liver_fat, main = "stea_alt75_s0~LiverFat",col=c("green","red"))
scatter.smooth(x=train_lin1$diabetes_s2, y = train_lin1$liver_fat, main = "Diabetes~LiverFat",col=c("green","red"))

#Here we plot a density curve to see the distribution od data of target variable(Liver_fat)
#Inference: Maximum people have a liver fat value les than 10 which is the threshhold limit
#Any one having liver fat value more than 10 is considered to have fatty liver.
#This curve helps to see the data districution
#ggplot(train_lin, aes(liver_fat)) + geom_density(fill="blue")

#Density Graph: To check how much is the response variable close to normality
#For som_bmi_s0, the graph is right skewed(positive skew)
#plot(density(train_lin1$som_bmi_s0), main="Density Plot: BMI", ylab="Liver_fat", sub=paste("Skewness:", 
#                                                                                           round(e1071::skewness(train_lin1$som_bmi_s0), 2)))  # density plot for 'BMI'
#polygon(density(train_lin1$som_bmi_s0), col="pink")

#For ferri_s0 the graph is right skewed(positive skew)
#plot(density(train_lin1$ferri_s0), main="Density Plot: ferri_s0", ylab="Liver_Fat", sub=paste("Skewness:",
#                                                                                              round(e1071::skewness(train_lin1$ferri_s0), 2)))  # density plot for 'Diabetes'
#polygon(density(train_lin1$ferri_s0), col="yellow")

## @knitr MLR1
#1.MLR with all attributes , check p-Value, AIC, BIC
#The linear regression model is created, taking into consideration all the features 
model1 = lm(liver_fat~., data=train_lin)
#The summary function gives the performance summary of the model
#such as:Residuals, coeficients(intercept and slope for each feature), t-Value,
#Residual standard error, Multiple R Squared value, F statistics
#summary(model1)

#par(mfrow=c(2,2))#Dividing plot screen into 2

#plot(model1)#plotting the linear regression model

#Inferences of above plot
#1. Residuals vs Fitted
#There could be a non-linear relationship between predictor variables and an 
#outcome variable and the pattern could show us in this plot.
#With the plot for model1 we could infer that the relationship is almost linear

#2. Normal Q-Q
#This plot shows if residuals are normally distributed and do residuals follow 
#a straight line well or do they deviate severely. 
#The residuals are lined well on the straight dashed line for model1 which is good.

#3. Scale-Location
#This plot shows if residuals are spread equally along the ranges of predictors.
#Used to check the assumption of equal variance
#It's good to have a horizontal line with equally (randomly) spread points.
#Model1 gives almost a horizontal line

#4. Residuals vs Leverage
#This plot helps us to find influential cases if any. i.e. see if there is any infuential 
#outlier in the data that affets the linear regression
#For model1 we dont see any specific infuential outlier

#Confidence intervals
#confint(model1)

#Model comparision parameters
#AIC is an estimate of a constant plus the relative distance between the 
#unknown true likelihood function of the data and the fitted likelihood function 
#of the model,so that a lower AIC means a model is considered to be closer to the truth.
#AIC (model1)
#BIC is an estimate of a function of the posterior probability of a model 
#being true, under a certain Bayesian setup, so that a lower BIC means that 
#a model is considered to be more likely to be the true model
#BIC(model1)

#Plots to see how the model has fitted the data for certain features
#Grey area is the confidence region
#1. age_ship_s0
#ggplot(data = train_lin, aes(x = age_ship_s0, y = liver_fat)) + geom_point()  +
#  stat_smooth(method = "lm", col = "dodgerblue3") +
#  theme(panel.background = element_rect(fill = "white"),
#        axis.line.x=element_line(),
#        axis.line.y=element_line()) +
#  ggtitle("Linear Model Fitted to Data")

#1.som_bmi_s0
#ggplot(data = train_lin, aes(x = som_bmi_s0, y = liver_fat)) + geom_point()  +
#  stat_smooth(method = "lm", col = "dodgerblue3") +
#  theme(panel.background = element_rect(fill = "white"),
#        axis.line.x=element_line(),
#        axis.line.y=element_line()) +
#  ggtitle("Linear Model Fitted to Data")

#Predictions using model1
prediction<-predict(model1, test_lin[5,])

#Calculating RMSE
rmse1<-rmse(test_lin$liver_fat, prediction)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model1", p_Value = 2.2, AIC = 4169.958,
                   BIC = 6159.183 ,RSq = 85.96, RMSE = rmse1 )
df_acc_l<-rbind(df_acc_l,newrow)
df_acc_l

## @knitr MLR2
#2.MLR with Important attributes from Random Forest, check p-Value, AIC, BIC#Error
train_lin2<-data.frame(train_lin1[w_rf1])

#The linear regression model is created, taking into consideration
#important features colleced from varImp function of Random Forest
model2 = lm(liver_fat~., data=train_lin2)

#The summary function gives the performance summary of the model
#such as:Residuals, coeficients(intercept and slope for each feature), t-Value,
#Residual standard error, Multiple R Squared value, F statistics
#summary(model2)

#par(mfrow=c(2,2))

#Plotting model2
#plot(model2)
#Inference
#Residual vs Fitter is almost linear
#Normal Q-Q has lined up well with the dashed line but not as perfect as model1
#Scale-Location line is almost horizontal
#Residual vs Leverage line touches some important outliers

#confidence interval
#confint(model2)

#Model comparision parameters
#AIC is an estimate of a constant plus the relative distance between the 
#unknown true likelihood function of the data and the fitted likelihood function 
#of the model,so that a lower AIC means a model is considered to be closer to the truth.
#AIC (model2)

#BIC is an estimate of a function of the posterior probability of a model 
#being true, under a certain Bayesian setup, so that a lower BIC means that 
#a model is considered to be more likely to be the true model
#BIC(model2)

#Predictions for model2
prediction<-predict(model2, test_lin[5,])

#Calculating RMSE
rmse2<-rmse(test_lin$liver_fat, prediction)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model2", p_Value = 2.2, AIC = 4200.606,
                   BIC = 4396.792 ,RSq = 56.95, RMSE = rmse2 )
df_acc_l<-rbind(df_acc_l,newrow)

## @knitr MLR3
#3. MLR with Boruta features
#EDA+Linear regression with Boruta Features 
train_lin3<-train_lin[w_non_rej]
#ncol(train_lin1)

#The linear regression model is created, taking into consideration
#all non rejected features from Boruta method
model3 = lm(liver_fat ~., data = train_lin3)
#print(model3)#Gives intercepts and coefficients

#The summary function gives the performance summary of the model
#such as:Residuals, coeficients(intercept and slope for each feature), t-Value,
#Residual standard error, Multiple R Squared value, F statistics
#summary(model3)

#par(mfrow=c(2,2))

#Plotting model3
#plot(model3)
#Inference
#Residual vs Fitter is almost linear
#Normal Q-Q has lined up well with the dashed line but not as perfect as model1
#Scale-Location line is almost horizontal
#Residual vs Leverage line touches some important outliers

#Confidence interval
#confint(model3)

#AIC value
#AIC (model3)

#BIC value
#BIC(model3)

#Predictions for model3
prediction<-predict(model3, test_lin[5,])

#Calculating RMSE
rmse3<-rmse(test_lin$liver_fat, prediction)

#Entering data in table df_acc
newrow<-data.frame(Model_Name = "Model3", p_Value = 2.2, AIC = 4237.193,
                   BIC = 4520.064 ,RSq = 57.04, RMSE = rmse3 )
df_acc_l<-rbind(df_acc_l,newrow)


## @knitr AnalyVariance
#Analysis of Variances

#Comparing model1 and model2

#anova(model1, model2)
#Model1 is proving better for its lower value fo RSS and other attributes

#Comparing model2 and model3

#anova(model2, model3)
#These two models are almost the same, but model3(with Boruta)
#is slightly better

#Comparing model1 and model3

#anova(model3, model1)
#Model is 1 is better in this case

#Plot1 showing comparision of 3 models
#The more the R square value, better the model
#ggplot(df_acc_l) + geom_density(aes(RSq, color = Model_Name)) + 
#  geom_jitter(aes(RSq, 0, color = Model_Name), alpha = 0.5, height = 0.02 )

#Plot2 showing comparision of 3 models
#The taller thr height of the bar ofr a model, the better it is
#barplot(df_acc_l$RSq, main="Linear Regression analysis with RSquared", xlab="Model Names", ylab="Accuracy", 
#        names.arg=c("M1","M2","M3"),
#        border="blue", density=c(85.96, 56.95, 57.04))

#Histogram to visualize model1
#If the plot is symmetrical around 0 that means tge model has fir the data well
#ggplot(data=train_lin, aes(model1$residuals)) + 
#  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
#  theme(panel.background = element_rect(fill = "white"),
#        axis.line.x=element_line(),
#        axis.line.y=element_line()) +
#  ggtitle("Histogram for Model Residuals")

#Histogram to visualize model2
#If the plot is symmetrical around 0 that means tge model has fir the data well
#ggplot(data=train_lin, aes(model2$residuals)) + 
#  geom_histogram(binwidth = 1, color = "black", fill = "green2") +
#  theme(panel.background = element_rect(fill = "white"),
#        axis.line.x=element_line(),
#        axis.line.y=element_line()) +
#  ggtitle("Histogram for Model Residuals")

#Histogram to visualize model3
#If the plot is symmetrical around 0 that means tge model has fir the data well
#ggplot(data=train_lin, aes(model3$residuals)) + 
#  geom_histogram(binwidth = 1, color = "black", fill = "blue 4") +
#  theme(panel.background = element_rect(fill = "white"),
#        axis.line.x=element_line(),
#        axis.line.y=element_line()) +
#  ggtitle("Histogram for Model Residuals")



#Scatter plot of BMI_s1 vs. Liver Fat
#HEre we are using training data that includes all the features
ggplot(data = train_lin, mapping = aes(x = som_bmi_s1, y = liver_fat)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "BMI_s1", y = "Liver_fat") +
  ggtitle(expression(atop("Scatterplot of BMI_s1 vs. liver_fat", atop(italic("With Fitted Regression Line", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

library(iml)
#ALE for the fields BMI and Liver Fat
#Accumulated Local Effects plot
#It describe how features influence the prediction of a machine learning model on average
(intrvls <- seq(min(df_data_lin$liver_fat), max(df_data_lin$liver_fat), length.out = 6))

#ggplot(df_data_lin, aes(som_bmi_s0, liver_fat)) +
#  geom_point() +
#  geom_segment(data = as_tibble(intrvls), aes(x = value, xend = value), 
#               y = min(df_data_lin$liver_fat) - 0.05 * (max(df_data_lin$liver_fat) - min(df_data_lin$liver_fat)), 
#               yend = max(df_data_lin$liver_fat) + 0.05 * (max(df_data_lin$liver_fat) - min(df_data_lin$liver_fat)), linetype = 2) +
#  geom_segment(data = df_data_lin, aes(x = intrvls[3], xend = intrvls[4], y = df_data_lin$liver_fat, yend = df_data_lin$liver_fat), 
#               arrow = arrow(length = unit(2, "mm"), type = "closed", ends = "both"), color = "blue") +
#  geom_text(data = tibble(id = 1:length(intrvls), int = intrvls), aes(x = int, y = 0.925, label = id))
