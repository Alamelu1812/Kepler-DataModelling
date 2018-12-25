# Logistic Regression

# Logistic Fit & Summary for DS 1

logistic.model.prop <- glm(koi_disposition ~ .,data = kepler.prop[train.ind.ds1,],family=binomial)
summary(logistic.model.prop)


# Predicting
logistic.kepler.pred.prop <- predict(logistic.model.prop,kepler.prop[-train.ind.ds1,],type = 'response')
logistic.kepler.bin.prop <- as.numeric(logistic.kepler.pred.prop>=0.5)
logisitic.output.prop <- table(logistic.kepler.bin.prop,kepler.prop[-train.ind.ds1,]$koi_disposition)
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

