#Best Subset Selection for DS 1
regfit.full <- regsubsets(koi_disposition~.,data=kepler.prop,nvmax=14)
reg.summary <- summary(regfit.full)
reg.summary

plot(reg.summary$bic, main = "BIC VS Number of Variables",xlab="Number of variables", ylab = "BIC", type="l")
which.min(reg.summary$bic)
reg.summary$which[10,]

kepler.prop <- kepler.prop[,!names(kepler.prop) %in% c("koi_srad")]

#-------------------------------------------------------------------------------------------------------------

#Best Subset Selection for DS 2
regfit.full <- regsubsets(koi_disposition~.,data=kepler.prop.ground,nvmax=14)
reg.summary <- summary(regfit.full)
reg.summary

plot(reg.summary$bic, main = "BIC VS Number of Variables",xlab="Number of variables", ylab = "BIC", type="l")
which.min(reg.summary$bic)
reg.summary$which[13,]

kepler.prop.ground <- kepler.prop.ground[,!names(kepler.prop.ground) %in% c("koi_srad")]

#-------------------------------------------------------------------------------------------------------------
kepler.flag.prop[1:10,]

#Best Subset Selection for DS 3
regfit.full <- regsubsets(koi_disposition~.,data=kepler.flag.prop,nvmax=14)
reg.summary <- summary(regfit.full)
reg.summary

plot(reg.summary$bic, main = "BIC VS Number of Variables",xlab="Number of variables", ylab = "BIC", type="l")
which.min(reg.summary$bic)
reg.summary$which[10,]

kepler.flag.prop <- kepler.flag.prop[,!names(kepler.flag.prop) %in% c("koi_impact","koi_duration","koi_prad","koi_slogg","koi_srad")]

#-------------------------------------------------------------------------------------------------------------


# Logistic Regression

# Logistic Fit & Summary for DS 1

logistic.model.prop <- glm(koi_disposition ~ .,data = kepler.prop[train.ind.ds1,],family=binomial)
summary(logistic.model.prop)


# Predicting
logistic.kepler.pred.prop <- predict(logistic.model.prop,kepler.prop[-train.ind.ds1,],type = 'response')
logistic.kepler.bin.prop <- as.numeric(logistic.kepler.pred.prop>=0.5)
logisitic.output.prop <- table(kepler.bin.prop,kepler.prop[-train.ind.ds1,]$koi_disposition)
logisitic.output.prop

# Accuracy Check
logistic.accu.prop <- (logisitic.output.prop[1,1] + logisitic.output.prop[2,2])/sum(logisitic.output.prop)
logistic.accu.prop


#-----------------------------------------------------------------------------------------------------------



# Logistic Fit & Summary for DS 2
logistic.model.prop.ground <- glm(koi_disposition ~ .,data = kepler.prop.ground[train.ind.ds2,],family=binomial)
summary(logistic.model.prop.ground)


# Predicting
logistic.kepler.pred.prop.ground <- predict(logistic.model.prop.ground,kepler.prop.ground[-train.ind.ds2,],type = 'response')
logistic.kepler.bin.prop.ground <- as.numeric(logistic.kepler.pred.prop.ground>=0.5)
logisitic.output.prop.ground <- table(logistic.kepler.bin.prop.ground,kepler.prop.ground[-train.ind.ds2,]$koi_disposition)
logisitic.output.prop.ground

# Accuracy Check
logistic.accu.prop.ground <- (logisitic.output.prop.ground[1,1] + logisitic.output.prop.ground[2,2])/sum(logisitic.output.prop.ground)
logistic.accu.prop.ground




#--------------------------------------------------------------------------------------------------


# Logistic Fit & Summary for DS 3
logistic.model.flag.prop <- glm(koi_disposition ~ .,data = kepler.flag.prop[train.ind.ds3,],family=binomial)
summary(logistic.model.flag.prop)


# Predicting
logistic.kepler.pred.flag.prop <- predict(logistic.model.flag.prop,kepler.flag.prop[-train.ind.ds3,],type = 'response')
logistic.kepler.bin.flag.prop <- as.numeric(logistic.kepler.pred.flag.prop>=0.5)
logisitic.output.flag.prop <- table(logistic.kepler.bin.flag.prop,kepler.flag.prop[-train.ind.ds3,]$koi_disposition)
logisitic.output.flag.prop

# Accuracy Check
logistic.accu.flag.prop <- (logisitic.output.flag.prop[1,1] + logisitic.output.flag.prop[2,2])/sum(logisitic.output.flag.prop)
logistic.accu.flag.prop



#-------------------------------------------------------------------------------------------------------------



# KNN for DS 1
knn.kepler.normal <-scale(kepler.prop[,!names(kepler.prop) %in% 'koi_disposition'])

knn.train.prop.x <- as.data.frame(knn.kepler.normal[train.ind.ds1,])
knn.test.prop.x  <- as.data.frame(knn.kepler.normal[-train.ind.ds1,])


#Store the outcome column separately
knn.train.prop.y=kepler.prop$koi_disposition[train.ind.ds1]
knn.test.prop.y=kepler.prop$koi_disposition[-train.ind.ds1]

#KNN Implementation

K = c(5,7,9,11,13,15,17,19,20,21,23,25,27,30,60,90,120)
cv.error <- rep(NA,length(K))
for (i in seq(length(K))){
  knn.pred.prop=knn(knn.train.prop.x,knn.test.prop.x, knn.train.prop.y, k=i)
  cv.error[i] = 1 - mean(knn.pred.prop == knn.test.prop.y)
}
cv.error
plot(K,cv.error,main="K-fold",type="b")

#-------------------------------------------------------------------------------------------------------------


# KNN for DS 2
knn.kepler.normal <-scale(kepler.prop.ground[,!names(kepler.prop) %in% 'koi_disposition'])

knn.train.prop.ground.x <- as.data.frame(knn.kepler.normal[train.ind.ds2,])
knn.test.prop.ground.x  <- as.data.frame(knn.kepler.normal[-train.ind.ds2,])


#Store the outcome column separately
knn.train.prop.ground.y=kepler.prop.ground$koi_disposition[train.ind.ds2]
knn.test.prop.ground.y=kepler.prop.ground$koi_disposition[-train.ind.ds2]

#KNN Implementation

K = c(5,7,9,11,13,15,17,19,20)
cv.error <- rep(NA,length(K))
for (i in seq(length(K))){
  knn.pred.prop.ground=knn(knn.train.prop.ground.x,knn.test.prop.ground.x, knn.train.prop.ground.y, k=i)
  cv.error[i] = 1 - mean(knn.pred.prop.ground == knn.test.prop.ground.y)
}
cv.error
plot(K,cv.error,main="K-fold",type="b")

#-------------------------------------------------------------------------------------------------------------


# KNN for DS 3

kepler.flag.prop$koi_fpflag_nt <- as.numeric(kepler.flag.prop$koi_fpflag_nt)
kepler.flag.prop$koi_fpflag_ss <- as.numeric(kepler.flag.prop$koi_fpflag_ss)
kepler.flag.prop$koi_fpflag_co <- as.numeric(kepler.flag.prop$koi_fpflag_co)
kepler.flag.prop$koi_fpflag_ec <- as.numeric(kepler.flag.prop$koi_fpflag_ec)

knn.kepler.normal <-scale(kepler.flag.prop[,!names(kepler.flag.prop) %in% 'koi_disposition'])

kepler.flag.prop$koi_fpflag_nt <- as.factor(kepler.flag.prop$koi_fpflag_nt)
kepler.flag.prop$koi_fpflag_ss <- as.factor(kepler.flag.prop$koi_fpflag_ss)
kepler.flag.prop$koi_fpflag_co <- as.factor(kepler.flag.prop$koi_fpflag_co)
kepler.flag.prop$koi_fpflag_ec <- as.factor(kepler.flag.prop$koi_fpflag_ec)

knn.train.flag.prop.x <- as.data.frame(knn.kepler.normal[train.ind.ds3,])
knn.test.flag.prop.x  <- as.data.frame(knn.kepler.normal[-train.ind.ds3,])


#Store the outcome column separately
knn.train.flag.prop.y=kepler.flag.prop$koi_disposition[train.ind.ds3]
knn.test.flag.prop.y=kepler.flag.prop$koi_disposition[-train.ind.ds3]

#KNN Implementation

K = c(5,7,9,11,13,15,17,19,20)
cv.error <- rep(NA,length(K))
for (i in seq(length(K))){
  knn.pred.flag.prop=knn(knn.train.flag.prop.x,knn.test.flag.prop.x, knn.train.flag.prop.y, k=i)
  cv.error[i] = 1 - mean(knn.pred.flag.prop == knn.test.flag.prop.y)
}
cv.error
plot(K,cv.error,main="K-fold",type="b")

#----------------------------------------------------------------------------------------------------------

#Random Forest for DS 1

set.seed(1)
randomForest.kepler.prop=randomForest(koi_disposition~.,data=kepler.prop,subset=train.ind.ds1,mtry=4, ntree =500,
                                      importance=TRUE)
randomForest.kepler.prop

set.seed(1)
randomForest.predict.prop = predict(randomForest.kepler.prop,newdata=kepler.prop[-train.ind.ds1,])
kepler.test.prop=kepler.prop[-train.ind.ds1,"koi_disposition"]
plot(randomForest.predict.prop, kepler.test.prop)
rF.output.prop <- table(randomForest.predict.prop, kepler.test.prop)
importance(randomForest.kepler.prop)
varImpPlot(randomForest.kepler.prop)

# Accuracy Check
randomForest.accu.prop <- (rF.output.prop[1,1] + rF.output.prop[2,2])/sum(rF.output.prop)
randomForest.accu.prop


#----------------------------------------------------------------------------------------------------------

#Random Forest for DS 2

set.seed(1)
randomForest.kepler.prop.ground=randomForest(koi_disposition~.,data=kepler.prop.ground,subset=train.ind.ds2,mtry=4, ntree =500,
                                             importance=TRUE)
randomForest.kepler.prop.ground

set.seed(1)
randomForest.predict.prop.ground = predict(randomForest.kepler.prop.ground,newdata=kepler.prop.ground[-train.ind.ds2,])
kepler.test.prop.ground=kepler.prop.ground[-train.ind.ds2,"koi_disposition"]
plot(randomForest.predict.prop.ground, kepler.test.prop.ground)
rF.output.prop.ground <- table(randomForest.predict.prop.ground, kepler.test.prop.ground)
importance(randomForest.kepler.prop.ground)
varImpPlot(randomForest.kepler.prop.ground)

# Accuracy Check
randomForest.accu.prop.ground <- (rF.output.prop.ground[1,1] + rF.output.prop.ground[2,2])/sum(rF.output.prop.ground)
randomForest.accu.prop.ground


#----------------------------------------------------------------------------------------------------------

#Random Forest for DS 3

set.seed(1)
randomForest.kepler.flag.prop=randomForest(koi_disposition~.,data=kepler.flag.prop,subset=train.ind.ds3,mtry=4, ntree =500,
                                           importance=TRUE)
randomForest.kepler.flag.prop

set.seed(1)
randomForest.predict.flag.prop = predict(randomForest.kepler.flag.prop,newdata=kepler.flag.prop[-train.ind.ds3,])
kepler.test.flag.prop=kepler.flag.prop[-train.ind.ds3,"koi_disposition"]
plot(randomForest.predict.flag.prop, kepler.test.flag.prop)
rF.output.flag.prop <- table(randomForest.predict.flag.prop, kepler.test.flag.prop)
importance(randomForest.kepler.flag.prop)
varImpPlot(randomForest.kepler.flag.prop)

# Accuracy Check
randomForest.accu.flag.prop <- (rF.output.flag.prop[1,1] + rF.output.flag.prop[2,2])/sum(rF.output.flag.prop)
randomForest.accu.flag.prop
