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


