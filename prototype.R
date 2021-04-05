library(tidyverse)
library(caret)
library(rpart)
library(xgboost)
library(ROSE)
library(MLmetrics)
library(caret)
library(doSNOW)
library(dplyr)
library(stringr)
library(rcompanion)


f1 <- function (data, lev = NULL, model = NULL) {
  experience<<-data
  precision <- Precision(data$pred, data$obs,positive ="1")
  recall  <- Recall(data$pred, data$obs,positive ="1")
  f1_val <- F1_Score(data$pred, data$obs,positive ="1")
  resu=c("precision"=precision,"rappel"=recall,"F1"=f1_val)
  print(resu)
  resu
}

test_model<-function(test_data,model,seuil=0.5){
  # Testing
  final <- data.frame(actual = test_data$match,predict(model, newdata = test_data, type = "prob"))
  final$predict <- ifelse(final$X0 > seuil, "0", "1")
  caret::confusionMatrix(as.factor(final$predict), test_data$match,positive="1")
}

df_mod=feature_eng()

#Split
index <- createDataPartition(df_mod$match, p = 0.7, list = FALSE)
train_data <- df_mod[index,]
test_data  <- df_mod[-index,]

#Validation croise
ctrl <- trainControl(method = "repeatedcv",number = 10,repeats = 3,summaryFunction = f1,search = "grid")


#KNN
k<- seq(1, 150, 2);k <- data.frame(k)
fit_knn<- train(match ~ ., data = train_data, method = "knn", trControl = ctrl , tuneGrid = k,preProcess = c("scale", "center"),metric = "F1")
# Testing
test_model(test_data,fit_knn)


#XGBOOST
tune_grid <- expand.grid(nrounds=c(100,200,300,400),max_depth = c(3:7),eta = c(0.05, 1),gamma = c(0.01),colsample_bytree = c(0.75),subsample = c(0.50),min_child_weight = c(0))
rf_fit <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1")
# Testing
test_model(test_data,rf_fit)


#random forest
model_rf <- train(match ~ .,data = train_data,method = "rf",preProcess = c("scale", "center"),trControl = ctrl,metric = "F1")
# Testing
test_model(test_data,model_rf)


#random forest + Undersampling
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE,sampling = "down",summaryFunction = f1,search = "grid")
model_rf_under <- train(match ~ .,data = train_data,method = "rf",preProcess = c("scale", "center"),trControl = ctrl,metric = "F1")
test_model(test_data,model_rf_under)


#random forest + Overrsampling
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE,sampling = "up",summaryFunction = f1,search = "grid")
model_rf_over <- train(match ~ .,data = train_data,method = "rf",preProcess = c("scale", "center"),trControl = ctrl,metric = "F1")
test_model(test_data,model_rf_over)


#random forest + rose sampling
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE,sampling = "rose",summaryFunction = f1,search = "grid")
model_rf_rose <- train(match ~ .,data = train_data,method = "rf",preProcess = c("scale", "center"),trControl = ctrl,metric = "F1")
test_model(test_data,model_rf_rose)
