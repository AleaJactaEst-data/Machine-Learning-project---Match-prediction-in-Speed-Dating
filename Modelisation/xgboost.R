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
  precision <- Precision(data$pred, data$obs,positive ="1")
  recall  <- Recall(data$pred, data$obs,positive ="1")
  f1_val <- F1_Score(data$pred, data$obs,positive ="1")
  resu=c("precision"=precision,"rappel"=recall,"F1"=f1_val)
  resu
}

test_model<-function(test_data,model,seuil=0.5){
  # Testing
  final <- data.frame(actual = test_data$match,predict(model, newdata = test_data, type = "prob"))
  final$predict <- ifelse(final$X0 > seuil, "0", "1")
  caret::confusionMatrix(as.factor(final$predict), test_data$match,positive="1",mode="prec_recall")
}


#IMPORTATION JEU DE DONNEE
df_mod=feature_eng()
#var significatif 
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual)
#Split
resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest



# Classique ---------------------------------------------------------------

ctrl <- trainControl(method = "cv",number = 10,summaryFunction = f1,search = "grid")

#XGBOOST
tune_grid <- expand.grid(nrounds=c(1000,2000,3000,4000),max_depth = c(90),eta = c(1),gamma = c(0.01),colsample_bytree = c(0.75),subsample = c(0.50),min_child_weight = c(0))
rf_fit <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1")
plot(rf_fit)
# Testing
test_model(test_data,rf_fit)


# Methode re-echantillonnage ----------------------------------------------
ctrl <- trainControl(method = "cv",number = 10,summaryFunction = f1,search = "grid",sampling = "up")

#XGBOOST
tune_grid <- expand.grid(nrounds=c(1000,2000,3000,4000),max_depth = 1:5, eta = 1, gamma = 0.01, colsample_bytree = 0.75,subsample = 0.5,min_child_weight = c(0))
rf_fit_sample <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1")
plot(rf_fit_sample)
# Testing
test_model(test_data,rf_fit_sample,0.95)


# Méthode de ponderation --------------------------------------------------
ctrl <- trainControl(method = "cv",number = 10,search = "grid",summaryFunction = f1)
tune_grid <- expand.grid(nrounds = 200, max_depth = 3, eta = 1, gamma = 0.01, colsample_bytree = 0.75,subsample = 0.5,min_child_weight = c(0))

rf_fit_wheight <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1",weights = ifelse(train_data$match == "1",
                                                                                                                                                                                             12 ,
                                                                                                                                                                                             1))
test_model(test_data,rf_fit_wheight,0.009)








