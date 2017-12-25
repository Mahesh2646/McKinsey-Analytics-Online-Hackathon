library(forecast)
library(xgboost)
library(lubridate)

setwd("C:/Users/MAHESH/Downloads/av mc")
getwd()

train=read.csv("train.csv")
test=read.csv("test.csv")

train$src = 1
test$src = 0
test$Vehicles <- 0
ds= rbind(train,test[,c(1,2,4,3,5)])

ds$Junction = as.factor(ds$Junction)
ds$dy_of_wk = as.factor(weekdays(as.Date(ds$DateTime)))
ds$day_of_month =as.factor(day(ds$DateTime))
ds$mnth = as.factor(month(ds$DateTime))
ds$yr = as.factor(year(ds$DateTime))
ds$hr = as.factor(hour(ds$DateTime))
ds$DateTime = as.Date(ds$DateTime)

train_dt1 <- subset(ds,src == 1 & Junction == 1)
test_dt1 <- subset(ds,src == 0 & Junction == 1)
train_dt2 <- subset(ds,src == 1 & Junction == 2)
test_dt2 <- subset(ds,src == 0 & Junction == 2)
train_dt3 <- subset(ds,src == 1 & Junction == 3)
test_dt3 <- subset(ds,src == 0 & Junction == 3)
train_dt4 <- subset(ds,src == 1 & Junction == 4)
test_dt4 <- subset(ds,src == 0 & Junction == 4)
test_dt1$Vehicles = NA
test_dt2$Vehicles = NA
test_dt3$Vehicles = NA
test_dt4$Vehicles = NA
summary(ds)


#jn1
xg_train1 = xgb.DMatrix( data.matrix(train_dt1[,c(-1,-2,-4-5)]), label=train_dt1$Vehicles)
xg_test1 = xgb.DMatrix( data.matrix(test_dt1[,c(-1,-2,-4-5)]))

param <- list("objective"   = "reg:linear",    # multiclass classification 
              "eval_metric" = "rmse",
              "nthread"     = 4,   # number of threads to be used 
              "max_depth"   = 15,    # maximum depth of tree 
              "eta"         = 0.03,    # step size shrinkage 
              "subsample"   = 0.5,    # part of data instances to grow tree 
              "colsample_bytree" = 0.5,  # subsample ratio of columns when constructing each tree 
              "silent"      = 1)



xgb <- xgboost(data = xg_train1, param = param, nrounds = 120, verbose = TRUE, prediction = TRUE,  maximize = TRUE)



pred1 <- predict(xgb, xg_test1)

pred1 = data.frame(pred1)
probs = data.frame(test_dt1, Vehilcles = pred1$pred1)
write.csv(probs, file = "J1a.csv", row.names = F)
write.csv(train_dt1, file = "J1_dt.csv", row.names = F)
#jn2

xg_train1 = xgb.DMatrix( data.matrix(train_dt2[,c(-2,-4-5)]), label=train_dt2$Vehicles)
xg_test1 = xgb.DMatrix( data.matrix(test_dt2[,c(-2,-4-5)]))

param <- list("objective"   = "reg:linear",    # multiclass classification 
              "eval_metric" = "rmse",
              "nthread"     = 4,   # number of threads to be used 
              "max_depth"   = 15,    # maximum depth of tree 
              "eta"         = 0.03,    # step size shrinkage 
              "subsample"   = 0.5,    # part of data instances to grow tree 
              "colsample_bytree" = 0.5,  # subsample ratio of columns when constructing each tree 
              "silent"      = 1)



xgb <- xgboost(data = xg_train1, param = param, nrounds = 120, verbose = TRUE, prediction = TRUE,  maximize = TRUE)


pred1 <- predict(xgb, xg_test1)

pred1 = data.frame(pred1)
probs2 = data.frame(test_dt2, Vehilcles = pred1$pred1)
write.csv(probs2, file = "J2.csv", row.names = F)


#jn3

xg_train1 = xgb.DMatrix( data.matrix(train_dt3[,c(-2,-4-5)]), label=train_dt3$Vehicles)
xg_test1 = xgb.DMatrix( data.matrix(test_dt3[,c(-2,-4-5)]))


param <- list("objective"   = "reg:linear",    # multiclass classification 
              "eval_metric" = "rmse",
              "nthread"     = 4,   # number of threads to be used 
              "max_depth"   = 15,    # maximum depth of tree 
              "eta"         = 0.03,    # step size shrinkage 
              "subsample"   = 0.5,    # part of data instances to grow tree 
              "colsample_bytree" = 0.5,  # subsample ratio of columns when constructing each tree 
              "silent"      = 1)



xgb <- xgboost(data = xg_train1, param = param, nrounds = 120, verbose = TRUE, prediction = TRUE,  maximize = TRUE)



pred1 <- predict(xgb, xg_test1)

pred1 = data.frame(pred1)
probs3 = data.frame(test_dt3, Vehilcles = pred1$pred1)
write.csv(probs3, file = "J3.csv", row.names = F)


#jn4

xg_train1 = xgb.DMatrix( data.matrix(train_dt4[,c(-2,-4-5)]), label=train_dt4$Vehicles)
xg_test1 = xgb.DMatrix( data.matrix(test_dt4[,c(-2,-4-5)]))

param <- list("objective"   = "reg:linear",    # multiclass classification 
              "eval_metric" = "rmse",
              "nthread"     = 4,   # number of threads to be used 
              "max_depth"   = 15,    # maximum depth of tree 
              "eta"         = 0.03,    # step size shrinkage 
              "subsample"   = 0.5,    # part of data instances to grow tree 
              "colsample_bytree" = 0.5,  # subsample ratio of columns when constructing each tree 
              "silent"      = 1)



xgb <- xgboost(data = xg_train1, param = param, nrounds = 80, verbose = TRUE, prediction = TRUE,  maximize = TRUE)



pred1 <- predict(xgb, xg_test1)

pred1 = data.frame(pred1)
probs4 = data.frame(test_dt4, Vehilcles = pred1$pred1)
write.csv(probs4, file = "J4.csv", row.names = F)

soln = rbind(probs,probs2,probs3,probs4)
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
soln$Vehilcles1 = specify_decimal(soln$Vehilcles * 2.9,0)
#summary(soln)
solnz <- soln[,c(4,12)]
solnzz = data.frame( ID= solnz$ID , Vehicles = solnz$Vehilcles)

write.csv(solnzz, file = "fnl.csv", row.names = F)

