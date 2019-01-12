#------------------Cohort Analysis ~ Classification-Regression-------------------
#--------------------------------------------------------------------------------
#Dataset         : SHIP                                                         #
#Team members    : Mangaraj & Saha                                              #
#Description     : Exploring the various phases in data science with R.         #
#Motive of script: Data pre-processing and EDA                                  #
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
         "Boruta", "DataExplorer")
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

## @knitr LoadDataSet 

#importing dataset from local drive
dataset = readRDS('181121_ship.rds') 

df<-data.frame(dataset) #converting dataset into dataframe


## @knitr JustLabelledData
#We limit our scope to supervised learning only so keeping only labeled data
df <- df[!is.na(df$liver_fat),]

#temporary dataframe 
df_tmp<-data.frame(dataset) 
df_tmp <- df_tmp[!is.na(df_tmp$liver_fat),]

#---------------------End of:loading of Dataset---------------------------

#---------------------Begin of: Data Inspection---------------------------
## @knitr DataInspection

#str(df)#structure of the dataset

#summary(df)#summary of features of the dataset

#summary(df$liver_fat)#Exploring target variable

#dim(df)#number of rows and columns in dataset

#graphical representation of data types of the columns in the dataset
#plot_str(df[1:20])
#plot_str(df[21:50])
#plot_str(df[51:100])
#plot_str(df[101:150])
#plot_str(df[151:200])
#plot_str(df[201:250])
#plot_str(df[251:300])
#plot_str(df[301:350])
#plot_str(df[351:400])

#Histogram representation distribution of target variable over data
#hist(df$liver_fat,
#main = "Histogram~Liver fat",
#xlab = "Liver fat",
#col =  "orange")

#Box plot representing distribuyion of age over data
#boxplot(df$age_ship_s0,
#main = toupper("Boxplot of Age"),
#ylab = "Age in years",
#col = "magenta")
#---------------------End of: Data Inspection-----------------------------

#---------------------Begin of: Deleting irrelevant columns---------------

#some columns contain date, time and id which logically do not contribute
#to the analysis or prediction as we consider every feature as an independent
#contributor to the target variable, irrespective of the time of recording of 
#data

## @knitr DelIrrelevantColumns
df$blt_beg_s0<-NULL
df$blt_beg_s1<-NULL
df$blt_beg_s2<-NULL
df$exdate_ship_s0<-NULL
df$exdate_ship_s1<-NULL
df$exdate_ship_s2<-NULL
df$zz_nr<-NULL
df$exdate_ship0_s0<-NULL

#---------------------End of: Deleting irrelevant columns-----------------

#---------------------Begin of:Handling Missing Values--------------------

## @knitr HandlingMissingValues
#listing names of columns with missing values
list_na<-colnames(df)[apply(df, 2, anyNA)]
#list(list_na)

#Calculating the number of missing values in each column
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
#print(na_count)

#Calculating the percentage of missing values 
x <- NROW(df)
y <- (na_count/x) * 100
na_countP<-round(y,2)
#print(na_countP)

#Graphical representation of percentage of missing values in each column
#plot_missing(df[1:15])
#plot_missing(df[16:30])
#plot_missing(df[31:50])
#plot_missing(df[51:70])
#plot_missing(df[71:100])
#plot_missing(df[101:130])
#plot_missing(df[131:160])
#plot_missing(df[161:190])
#plot_missing(df[191:220])
#plot_missing(df[221:250])
#plot_missing(df[251:275])
#plot_missing(df[276:300])
#plot_missing(df[301:330])
#plot_missing(df[331:360])
#plot_missing(df[361:380])
#plot_missing(df[381:392])

#1. Check if column is factor or numerical
#2. If Factor and column gender specific then introduce new category
#3. If Factor and non gender specific with missing value then replace with "None"
#4. If Numeric and column is gender specific, convert it into categorical and intro new category
#5. If Numeric, non- gender specific and % of missing value > 10%, -> discard
#6. If Numeric, non- gender specific and % of missing valyes < 10% -> MICE imputation

# Handling missing values for Factor type #

#Names of factor type Columns with missing values 
w_fac<- which( sapply( df, class ) == 'factor' )
list_na_f<-colnames(df[w_fac])[apply(df, 2, anyNA)]
#list(list_na_f)

#Calculating the number of missing values in each of these columns
na_count_f <-sapply(df[w_fac], function(y) sum(length(which(is.na(y)))))
na_count_f <- data.frame(na_count_f)
#print(na_count_f)

#Calculating the percentage of missing values in each factor
x <- NROW(df)
y <- (na_count_f/x) * 100
na_count_fp<-round(y,2)
#View(na_count_fp)

#Handling columns with missing values ~ gender specific

#col names:parity_w_s0, birth_w_s0, fruehgeb_w_s0, menopaus_yn_w_s0, hormonrepl_w_s0,
#          pillever_w_s0, pillnow_w_s0, parity_w_s1, menopaus_yn_w_s1, hormonrepl_w_s1,
#          parity_w_s2, fruehgeb_w_s2, menopaus_yn_w_s2, hormonrepl_w_s2, pillever_w_s2,
#          pillnow_w_s2, premat_w_s0, stillbirth_w_s0, uterus_w_s0, hyst_w_s0,adnexe_w_s0,
#          op_preg_w_s0,breast_w_s0,vag_chir_w_s0,steri_w_s0, s2_frau_03_w_s2

#For all these columns we would introduce a new category "OG" ~ Other Gender

w_gen<-c("parity_w_s0", "birth_w_s0", "fruehgeb_w_s0", "menopaus_yn_w_s0", "hormonrepl_w_s0",
         "pillever_w_s0", "pillnow_w_s0", "parity_w_s1", "menopaus_yn_w_s1", "hormonrepl_w_s1",
         "parity_w_s2", "fruehgeb_w_s2", "menopaus_yn_w_s2", "hormonrepl_w_s2", "pillever_w_s2",
         "pillnow_w_s2", "premat_w_s0", "stillbirth_w_s0", "uterus_w_s0", "hyst_w_s0","adnexe_w_s0",
         "op_preg_w_s0","breast_w_s0","vag_chir_w_s0","steri_w_s0", "s2_frau_03_w_s2")

df[w_gen] <- lapply( df[w_gen], function(x) 
  ifelse(is.na(x),"OG",x))

#Handling columns with missing values ~ non-gender specific
#For all these columns we would introduce "None" for missing values

df[w_fac] <- lapply( df[w_fac], function(x) 
  ifelse(is.na(x),"None",x))
#View(df[w_fac])

#converting all modified character columns to factor data type
w_chr <- which( sapply( df, class ) == 'character' )
df[w_chr] <- lapply( df[w_chr], function(x) as.factor(x) )

# Handling missing values for Numeric type #

#conveting all integer type columns into numeric for uniformity
w_int <- which( sapply( df, class ) == 'integer' )
df[w_int] <- lapply( df[w_int], function(x) as.numeric(x) )

#Names of factor type Columns with missing values 
w_num<- which( sapply( df, class ) == 'numeric' )
list_count_n<-colnames(df[w_num])[apply(df, 2, anyNA)]
#list(list_count_n)

#Calculating the number of missing values in each of these columns
na_count_n <-sapply(df[w_num], function(y) sum(length(which(is.na(y)))))
na_count_n <- data.frame(na_count_n)

#Calculating the percentage of missing values in each factor
x <- NROW(df)
y <- (na_count_n/x) * 100
na_count_np<-round(y,2)
#View(na_count_np)

#Handling numeric columns with missing that are gender specific -> convert to categorical

#colnames:menopaus_w_s0,testo_m_s0,dheas_m_s0,testo_m_s1,menopaus_w_s2,menopause_w_s0,
#         menostat_w_s0,use_mht_w_s0

#for menopaus_w_s0: 40-50 -> level 1; 51-60 -> level 2; Rest -> 'OG'
df$menopaus_w_s0<-df_tmp$menopaus_w_s0 #df$menopaus_w_s0 is num[1:886] df_tmp$menopaus_w_s0 is int[1:886]
df$menopaus_w_s0[is.na(df$menopaus_w_s0)] <- 999 #putting temp var for missing values

for(i in 1:nrow(df)){ #dividing by range
  if(df$menopaus_w_s0[i]  >= 40 && df$menopaus_w_s0[i]  <= 50){
    df$menopaus_w_s0[i] <- 1
  }
  else if(df$menopaus_w_s0[i]  > 50 && df$menopaus_w_s0[i]  <= 100){
    df$menopaus_w_s0[i] <- 2
  }
}

df$menopaus_w_s0<-as.character(df$menopaus_w_s0) #converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$menopaus_w_s0[i]  == "999"){
    df$menopaus_w_s0[i] <- "OG"
  }
}

df$menopaus_w_s0<-as.factor(df$menopaus_w_s0) #converting to factor type

#for testo_m_s0: 0-16 -> level 1; 16-37 -> level 2; Rest -> 'OG'
#str(df$testo_m_s0)
#summary(df$testo_m_s0)

df$testo_m_s0[is.na(df$testo_m_s0)] <- 999 #putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$testo_m_s0[i]  >= 0 && df$testo_m_s0[i]  <= 16){
    df$testo_m_s0[i] <- 1
  }
  else if(df$testo_m_s0[i]  > 16 && df$testo_m_s0[i]  <= 37){
    df$testo_m_s0[i] <- 2
  }
}

df$testo_m_s0<-as.character(df$testo_m_s0)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$testo_m_s0[i]  == "999"){
    df$testo_m_s0[i] <- "OG"
  }
}

df$testo_m_s0<-as.factor(df$testo_m_s0)#converting to factor type

#for dheas_m_s0: 0-16 -> level 1; 16-37 -> level 2; Rest -> 'OG'

#str(df$dheas_m_s0)
#summary(df$dheas_m_s0)

df$dheas_m_s0[is.na(df$dheas_m_s0)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$dheas_m_s0[i]  >= 0 && df$dheas_m_s0[i]  <= 4){
    df$dheas_m_s0[i] <- 1
  }
  else if(df$dheas_m_s0[i]  > 4 && df$dheas_m_s0[i]  <= 9){
    df$dheas_m_s0[i] <- 2
  }
}

df$dheas_m_s0<-as.character(df$dheas_m_s0)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$dheas_m_s0[i]  == "999"){
    df$dheas_m_s0[i] <- "OG"
  }
}

df$dheas_m_s0<-as.factor(df$dheas_m_s0)#converting to factor type

#for testo_m_s1: 3-27 -> level 1; 28-55 -> level 2; Rest -> 'OG'
#str(df$testo_m_s1)
#summary(df$testo_m_s1)

df$testo_m_s1[is.na(df$testo_m_s1)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$testo_m_s1[i]  >= 3 && df$testo_m_s1[i]  <= 27){
    df$testo_m_s1[i] <- 1
  }
  else if(df$testo_m_s1[i]  > 27 && df$testo_m_s1[i]  <= 55){
    df$testo_m_s1[i] <- 2
  }
}

df$testo_m_s1<-as.character(df$testo_m_s1)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$testo_m_s1[i]  == "999"){
    df$testo_m_s1[i] <- "OG"
  }
}

df$testo_m_s1<-as.factor(df$testo_m_s1)#converting to factor type

#for menopaus_w_s2: 40-55 -> level 1; 56-70 -> level 2; Rest -> 'OG'
#str(df$menopaus_w_s2)
#summary(df$menopaus_w_s2)

df$menopaus_w_s2[is.na(df$menopaus_w_s2)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$menopaus_w_s2[i]  >= 40 && df$menopaus_w_s2[i]  <= 55){
    df$menopaus_w_s2[i] <- 1
  }
  else if(df$menopaus_w_s2[i]  > 55 && df$menopaus_w_s2[i]  <= 70){
    df$menopaus_w_s2[i] <- 2
  }
}

df$menopaus_w_s2<-as.character(df$menopaus_w_s2)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$menopaus_w_s2[i]  == "999"){
    df$menopaus_w_s2[i] <- "OG"
  }
}

df$menopaus_w_s2<-as.factor(df$menopaus_w_s2)#converting to factor type

#for menopause_w_s0: 0-50 -> level 1; 51-100 -> level 2; Rest -> 'OG'
#str(df$menopause_w_s0)
#summary(df$menopause_w_s0)

df$menopause_w_s0[is.na(df$menopause_w_s0)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$menopause_w_s0[i]  >= 0 && df$menopause_w_s0[i]  <= 50){
    df$menopause_w_s0[i] <- 1
  }
  else if(df$menopause_w_s0[i]  > 51 && df$menopause_w_s0[i]  <= 100){
    df$menopause_w_s0[i] <- 2
  }
}

df$menopause_w_s0<-as.character(df$menopause_w_s0)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$menopause_w_s0[i]  == "999"){
    df$menopause_w_s0[i] <- "OG"
  }
}

df$menopause_w_s0<-as.factor(df$menopause_w_s0)#converting to factor type

#for menostat_w_s0: 0-50 -> level 1; 51-100 -> level 2; Rest -> 'OG'
#str(df$menostat_w_s0)
#summary(df$menostat_w_s0)

df$menostat_w_s0[is.na(df$menostat_w_s0)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$menostat_w_s0[i]  >= 0 && df$menostat_w_s0[i]  <= 50){
    df$menostat_w_s0[i] <- 1
  }
  else if(df$menostat_w_s0[i]  > 51 && df$menostat_w_s0[i]  <= 100){
    df$menostat_w_s0[i] <- 2
  }
}

df$menostat_w_s0<-as.character(df$menostat_w_s0)#converting to character type

#summary(df$menostat_w_s0)

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$menostat_w_s0[i]  == "999"){
    df$menostat_w_s0[i] <- "OG"
  }
}

df$menostat_w_s0<-as.factor(df$menostat_w_s0)#converting to factor type

#for use_mht_w_s0: 0-50 -> level 1; 51-100 -> level 2; Rest -> 'OG'
#str(df$use_mht_w_s0)
#summary(df$use_mht_w_s0)

df$use_mht_w_s0[is.na(df$use_mht_w_s0)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$use_mht_w_s0[i]  >= 0 && df$use_mht_w_s0[i]  <= 0.5){
    df$use_mht_w_s0[i] <- 1
  }
  else if(df$use_mht_w_s0[i]  > 0.5 && df$use_mht_w_s0[i]  <= 1.0){
    df$use_mht_w_s0[i] <- 2
  }
}

df$use_mht_w_s0<-as.character(df$use_mht_w_s0)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$use_mht_w_s0[i]  == "999"){
    df$use_mht_w_s0[i] <- "OG"
  }
}

df$use_mht_w_s0<-as.factor(df$use_mht_w_s0)#converting to factor type

#for menopaus_w_s1: 33-47 -> level 1; 48-62 -> level 2; Rest -> 'OG'
#str(df$menopaus_w_s1)
#summary(df$menopaus_w_s1)

df$menopaus_w_s1[is.na(df$menopaus_w_s1)] <- 999#putting temp var for missing values

for(i in 1:nrow(df)){#dividing by range
  if(df$menopaus_w_s1[i]  >= 33 && df$menopaus_w_s1[i]  <= 47){
    df$menopaus_w_s1[i] <- 1
  }
  else if(df$menopaus_w_s1[i]  > 47 && df$menopaus_w_s1[i]  <= 62){
    df$menopaus_w_s1[i] <- 2
  }
}

df$menopaus_w_s1<-as.character(df$menopaus_w_s1)#converting to character type

for(i in 1:nrow(df)){#replacing 999 with OG
  if(df$menopaus_w_s1[i]  == "999"){
    df$menopaus_w_s1[i] <- "OG"
  }
}

df$menopaus_w_s1<-as.factor(df$menopaus_w_s1)#converting to factor type

#Deleting columns with more than 10% misiing values after processing till now

#colnames: "quick_s0","alb_u_s0","il6_s0","ige_s0","fs_s0","flag","asat_s_s1","ggt_s_s1",
#           "lip_s_s1","fs_s1","age_ship_s2","alkligt_s2","sleeph_s2","som_bmi_s2",
#           "som_tail_s2","som_huef_s2","hgb_s2","hba1c_s2","quick_s2","fib_cl_s2","crea_s_s2",
#           "hrs_s_s2","gluc_s_s2","asat_s_s2","ggt_s_s2","lip_s_s2","chol_s_s2","tg_s_s2",
#           "hdl_s_s2","ldl_s_s2","tsh_s2","jodid_u_s2","crea_u_s2","sd_volg_s2","lvm_s0",
#           "lvmi_s0","onsetsmok_s0","alb_crea_u_s0","imt_s0","crp_s_s0"

misscol5<- c("quick_s0","alb_u_s0","il6_s0","ige_s0","fs_s0","flag","asat_s_s1","ggt_s_s1",
             "lip_s_s1","fs_s1","age_ship_s2","alkligt_s2","sleeph_s2","som_bmi_s2",
             "som_tail_s2","som_huef_s2","hgb_s2","hba1c_s2","quick_s2","fib_cl_s2","crea_s_s2",
             "hrs_s_s2","gluc_s_s2","asat_s_s2","ggt_s_s2","lip_s_s2","chol_s_s2","tg_s_s2",
             "hdl_s_s2","ldl_s_s2","tsh_s2","jodid_u_s2","crea_u_s2","sd_volg_s2","lvm_s0",
             "lvmi_s0","onsetsmok_s0","alb_crea_u_s0","imt_s0","crp_s_s0")

#deleting the numerical columns with more than 5% missing values
df<-df[,-which(names(df) %in% misscol5)]
#checking the column size
#ncol(df)


## @knitr RunMice
#Imputing columns with MICE for columns having less than 10% misiing values 
imputed1<-mice(df, m = 2, method = "mean", seed = 500)

imputed <- complete(imputed1)
sapply(imputed, function(x) sum(is.na(x)))

#Calculating the number of NA values in a single column
na_count_imp <-sapply(imputed, function(y) sum(length(which(is.na(y)))))
na_count_imp <- data.frame(na_count_imp)

#Calculating the percentage of NA values 
x <- NROW(imputed)
y <- (na_count_imp/x) * 100
na_count_imp<-round(y,2)
View(na_count_imp)

#Deleting the column sc_sondercodes_s0
imputed$sc_sondercodes_s0<-NULL

#After mice imputation there are certain columns still having missing values
#We would run MICE for the second imputation phase

#checking what all columns still have missing values
w_colnames_miss<-colnames(imputed)[apply(is.na(imputed), 2, any)]
View(w_colnames_miss)
#creating a matrix with the above columns
df_mice<-imputed[w_colnames_miss]
View(df_mice)
imputed_m2<-mice(df_mice, m = 2, method = "mean", seed = 500)

#creating the imputed matrix
imputed2<-complete(imputed_m2)

#checking what all columns still have missing values
w_colnames_miss1<-colnames(imputed2)[apply(is.na(imputed2), 2, any)]
View(w_colnames_miss1)

#Calculating the number of NA values in a single column
na_count2 <-sapply(imputed2, function(y) sum(length(which(is.na(y)))))
na_count2 <- data.frame(na_count2)

#Calculating the percentage of NA values 
x <- NROW(imputed2)
y <- (na_count2/x) * 100
na_count2<-round(y,2)
View(na_count2)

View(imputed2)

#After 2nd phase of MICE imputation the 4 columns still have some missing values and they
#are missing at Random so for proceeding without rando values, we would discard those cols

#replacing columns of imputed dataset with imputed columns from imputed2 and
#deletinf the columns which still have missing values
imputed$alkligt_s0<-imputed2$alkligt_s0
imputed$hgb_s0<-imputed2$hgb_s0
imputed$hba1c_s0<-imputed2$hba1c_s0
imputed$fib_cl_s0<-imputed2$fib_cl_s0
imputed$crea_s_s0<-imputed2$crea_s_s0
imputed$hrs_s_s0<-imputed2$hrs_s_s0
imputed$gluc_s_s0<-imputed2$gluc_s_s0
imputed$asat_s_s0<-imputed2$asat_s_s0
imputed$alat_s_s0<-imputed2$alat_s_s0
imputed$ggt_s_s0<-imputed2$ggt_s_s0

imputed$lip_s_s0<-imputed2$lip_s_s0
imputed$chol_s_s0<-imputed2$chol_s_s0
imputed$tg_s_s0<-imputed2$tg_s_s0
imputed$hdl_s_s0<-imputed2$hdl_s_s0
imputed$ldl_s_s0<-imputed2$ldl_s_s0
imputed$lipo_a_s0<-imputed2$lipo_a_s0
imputed$apoa1_s0<-imputed2$apoa1_s0
imputed$apob_s0<-imputed2$apob_s0
imputed$tsh_s0<-imputed2$tsh_s0
imputed$ferri_s0<-imputed2$ferri_s0

imputed$cdt_s0<-imputed2$cdt_s0
imputed$jodid_u_s0<-imputed2$jodid_u_s0
imputed$crea_u_s0<-imputed2$crea_u_s0
imputed$hs_crp_s0<-imputed2$hs_crp_s0
imputed$prl_s0<-imputed2$prl_s0
imputed$igf1_s0<-imputed2$igf1_s0
imputed$igfbp3_s0<-imputed2$igfbp3_s0
imputed$udpdkrea_s0<-imputed2$udpdkrea_s0
imputed$sd_volg_s0<-imputed2$sd_volg_s0
imputed$avmean_s0<-imputed2$avmean_s0

imputed$avmeanapp_s0<-imputed2$avmeanapp_s0
imputed$stmean_s0<-imputed2$stmean_s0
imputed$stmeanapp_s0<-imputed2$stmeanapp_s0
imputed$zaehne_s0<-imputed2$zaehne_s0
imputed$age_ship_s1<-imputed2$age_ship_s1
imputed$alkligt_s1<-imputed2$alkligt_s1
imputed$som_bmi_s1<-imputed2$som_bmi_s1
imputed$som_tail_s1<-imputed2$som_tail_s1
imputed$som_huef_s1<-imputed2$som_huef_s1
imputed$hgb_s1<-imputed2$hgb_s1

imputed$hba1c_s1<-imputed2$hba1c_s1
imputed$quick_s1<-imputed2$quick_s1
imputed$fib_cl_s1<-imputed2$fib_cl_s1
imputed$crea_s_s1<-imputed2$crea_s_s1
imputed$hrs_s_s1<-imputed2$hrs_s_s1
imputed$gluc_s_s1<-imputed2$gluc_s_s1
imputed$chol_s_s1<-imputed2$chol_s_s1
imputed$tg_s_s1<-imputed2$tg_s_s1
imputed$hdl_s_s1<-imputed2$hdl_s_s1
imputed$ldl_s_s1<-imputed2$ldl_s_s1

imputed$tsh_s1<-imputed2$tsh_s1
imputed$jodid_u_s1<-imputed2$jodid_u_s1
imputed$crea_u_s1<-imputed2$crea_u_s1
imputed$hs_crp_s1<-imputed2$hs_crp_s1
imputed$igf1_s1<-imputed2$igf1_s1
imputed$sd_volg_s1<-imputed2$sd_volg_s1
imputed$edyrs_s0<-imputed2$edyrs_s0
imputed$kldb_75_s0<-imputed2$kldb_75_s0
imputed$hf_kldb_75_s0<-NULL
imputed$siops_s0<-imputed2$siops_s0

imputed$mps_s0<-imputed2$mps_s0
imputed$isei_s0<-imputed2$isei_s0
imputed$inceq_s0<-imputed2$inceq_s0
imputed$packyrs_s0<-imputed2$packyrs_s0
imputed$alcg30d_s0<-NULL
imputed$gfr_mdrd_s0<-imputed2$gfr_mdrd_s0
imputed$chol_hdl_s0<-imputed2$chol_hdl_s0
imputed$ffs_s0<-imputed2$ffs_s0
imputed$mcs_sf12_s0<-imputed2$mcs_sf12_s0
imputed$pcs_sf12_s0<-imputed2$pcs_sf12_s0

imputed$mort_cvd_s0<-NULL
imputed$mort_chd_s0<-NULL
imputed$mort_ca_s0<-NULL
imputed$na_s_s0<-imputed2$na_s_s0
imputed$k_s_s0<-imputed2$k_s_s0
imputed$ca_s_s0<-imputed2$ca_s_s0
imputed$mg_s_s0<-imputed2$mg_s_s0

#forming target variable
imputed$liver_fat<-df_tmp$liver_fat
#str(imputed$liver_fat)
for(i in 1:nrow(df)){
  if(imputed$liver_fat[i]  < 10){
    imputed$liver_fat[i] <- 0
  }
  else
    imputed$liver_fat[i] <- 1
}

#converting the numeric type of target variable to factor
imputed$liver_fat<-as.factor(imputed$liver_fat)

#We find 2 columns are still let with 1 missing value so imputing it again

#creating a matrix with the above columns
var = c("mps_s0","isei_s0")
df_mice1<-imputed[var]
imputed_m3<-mice(df_mice1, m = 2, method = "mean", seed = 500)

#creating the imputed matrix
imputed3<-complete(imputed_m3)

imputed$mps_s0<-imputed3$mps_s0
imputed$isei_s0<-imputed3$isei_s0

#---------------------End of:Handling Missing Values-----------------------

#---------------------Begin of:Exporting preprocessed file-----------------
export(imputed, "imputed.rds")
#---------------------End of:Exporting preprocessed file-------------------

#-------------------Descriptive Statistics---------------------------------
