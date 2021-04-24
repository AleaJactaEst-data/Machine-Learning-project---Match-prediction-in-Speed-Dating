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


#IMPORTATION JEU DE DONNEE
df_mod=feature_eng()
#var significatif 
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual)
#Split
resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest


#Validation croise
ctrl <- trainControl(method = "cv",number = 10,summaryFunction = f1,search = "grid")


#KNN
k <- data.frame(k=seq(1, 15, 2))
fit_knn<- train(match ~ ., data = train_data, method = "knn", trControl = ctrl , tuneGrid = k,preProcess = c("scale", "center"),metric = "F1")
# Testing
test_model(test_data,fit_knn)

