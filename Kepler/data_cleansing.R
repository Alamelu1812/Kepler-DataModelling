install.packages("leaps")
library(leaps)
install.packages("glmnet")
library(glmnet)
library(e1071)
library(class)
install.packages("randomForest")
library(randomForest)
library(MASS)

setwd("/Users/alamelu/Desktop/SDM_Lab")
kepler <- read.csv("exoplanet.csv",header=T,na.strings = '')  

#Data Cleansing
colSums(is.na(kepler))
colnames(kepler)[colSums(is.na(kepler)) > 0]

#Verifying that columns koi_teq_err1, koi_teq_err2 only have NA values
sum(!is.na(kepler$koi_teq_err1)) # column has only NA values
sum(!is.na(kepler$koi_teq_err2)) # column has only NA values


# Deleting the columns unnecessary columns from dataset
kepler <- kepler[,!names(kepler) %in% c("rowid","kepid","kepoi_name","kepler_name","koi_score","koi_pdisposition",
                                        "koi_tce_plnt_num","koi_tce_delivname")]

#Remove err columns
kepler <- kepler[,!names(kepler) %in% c("koi_period_err1","koi_period_err2","koi_time0bk_err1",
                                        "koi_time0bk_err2","koi_impact_err1","koi_impact_err2",
                                        "koi_duration_err1","koi_duration_err1","koi_duration_err2",
                                        "koi_depth_err1","koi_depth_err2","koi_insol_err1","koi_insol_err2",
                                        "koi_steff_err1","koi_steff_err2","koi_slogg_err1","koi_slogg_err2",
                                        "koi_srad_err1","koi_srad_err2","koi_prad_err1","koi_prad_err2",
                                        "koi_teq_err1","koi_teq_err2")]



kepler.original <- kepler

#Remove CANDIDATE
kepler <- kepler [!(kepler$koi_disposition == "CANDIDATE"), ]
kepler$koi_disposition <- droplevels(kepler$koi_disposition)
kepler$koi_disposition <- as.factor(kepler$koi_disposition)
levels(kepler$koi_disposition)



#Remove columns based on domain knowledge
kepler.prop <-  kepler[,!names(kepler) %in% c("koi_time0bk","ra","dec","koi_kepmag","koi_fpflag_nt","koi_fpflag_ss",
                                              "koi_fpflag_co","koi_fpflag_ec")]
kepler.prop.ground <- kepler[,!names(kepler) %in% c("koi_time0bk","koi_fpflag_nt","koi_fpflag_ss",
                                                    "koi_fpflag_co","koi_fpflag_ec")]
kepler.flag.prop <- kepler[,!names(kepler) %in% c("koi_time0bk","ra","dec","koi_kepmag")]

names(kepler.prop)
names(kepler.prop.ground)
names(kepler.flag.prop)

#Check column type
#DS1
sapply(kepler.prop,class)

#DS2
sapply(kepler.prop.ground,class)

#DS3
sapply(kepler.flag.prop,class)

#convert flag to factors
kepler.flag.prop$koi_fpflag_nt <- as.factor(kepler.flag.prop$koi_fpflag_nt)
kepler.flag.prop$koi_fpflag_ss <- as.factor(kepler.flag.prop$koi_fpflag_ss)
kepler.flag.prop$koi_fpflag_co <- as.factor(kepler.flag.prop$koi_fpflag_co)
kepler.flag.prop$koi_fpflag_ec <- as.factor(kepler.flag.prop$koi_fpflag_ec)

sapply(kepler.flag.prop,class)

#Remove NA values
#for individual group
sum(is.na(kepler))

#DS1
dim(kepler.prop)
colSums(is.na(kepler.prop))
colnames(kepler.prop)[colSums(is.na(kepler.prop)) > 0]
kepler.prop <- kepler.prop[complete.cases(kepler.prop),]
dim(kepler.prop)

#DS2
dim(kepler.prop.ground)
colSums(is.na(kepler.prop.ground))
colnames(kepler.prop.ground)[colSums(is.na(kepler.prop.ground)) > 0]
kepler.prop.ground <- kepler.prop.ground[complete.cases(kepler.prop.ground),]
dim(kepler.prop.ground)

#DS3
dim(kepler.flag.prop)
colSums(is.na(kepler.flag.prop))
colnames(kepler.flag.prop)[colSums(is.na(kepler.flag.prop)) > 0]
kepler.flag.prop <- kepler.flag.prop[complete.cases(kepler.flag.prop),]
dim(kepler.flag.prop)

#Visualize mean and variance for predictors
#DS1
sapply(kepler.prop,mean)
sapply(kepler.prop,var)

#DS2
sapply(kepler.prop.ground,mean)
sapply(kepler.prop.ground,var)

#DS3
sapply(kepler.flag.prop,mean)
sapply(kepler.flag.prop,var)




#Outlier Analysis





#Validation split
#DS1
set.seed(1)
train.ind.ds1 = sample(1:nrow(kepler.prop),4/5*nrow(kepler.prop))

#DS2
set.seed(1)
train.ind.ds2 = sample(1:nrow(kepler.prop.ground),4/5*nrow(kepler.prop.ground))


#DS3
set.seed(1)
train.ind.ds3 = sample(1:nrow(kepler.flag.prop),4/5*nrow(kepler.flag.prop))

