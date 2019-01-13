#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Exploring the various phases in data science with R.         #
#Motive of script: Feature Selection using Wrapper Method: Boruta               #
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

## @knitr LoadDataSet 
#importing dataset from local drive
dataset = readRDS('imputed.rds') 

df_bor<-data.frame(dataset)
#table(df_bor$liver_fat)

## @knitr WrapperBoruta
#applying wrapper method on dataset
set.seed(222)
boruta<- Boruta(liver_fat~., data = df_bor, doTrace = 2, maxRuns = 500)
#print(boruta)
#plot(boruta)
#plotImpHistory(boruta)
#getNonRejectedFormula(boruta)
#getConfirmedFormula(boruta)

#tentative fix

boruta_ten<-TentativeRoughFix(boruta)
#print(boruta_ten)
#attStats(boruta)
#plotImpHistory(boruta)