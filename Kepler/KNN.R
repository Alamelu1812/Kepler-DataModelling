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
