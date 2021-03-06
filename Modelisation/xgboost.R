library(tidyverse)
library(caret)
library(rpart)
library(xgboost)
library(ROSE)
library(MLmetrics)
library(caret)
library(dplyr)
library(stringr)
library(rcompanion)

f1 <- function (data, lev = NULL, model = NULL) {
  setTxtProgressBar(pb, k/nb_tune);k<<-k+1;
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

best_f1<-function(model,test_data,min=0.01,max=0.99,step=0.001){
  seuil=seq(min,max,step)
  f1_seuil=sapply(seuil,function(x){
    print(x)
    # Testing
    final <- data.frame(actual = test_data$match,predict(model, newdata = test_data, type = "prob"))
    final$predict <- factor(ifelse(final$X0 > x, "0", "1"),levels = c("0","1"))
    F1_Score(final$predict, final$actual,positive ="1")
  })
  return(list(seuil=seuil,F1=f1_seuil))
}

#IMPORTATION JEU DE DONNEE
df_mod=feature_eng()
#var significatif 
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual)
#Split
resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest


tune_grid <- expand.grid(nrounds=c(500,1000,2000),max_depth = c(1:3), eta = c(0.1,0.01), gamma = c(0.1,0),colsample_bytree = c(0.75),subsample = c(0.50),min_child_weight = c(0))
nb_tune<<-nrow(tune_grid)*10
# Classique ---------------------------------------------------------------

ctrl <- trainControl(method = "cv",number = 10,summaryFunction = f1,search = "grid")

#XGBOOST
k<<-0;pb<<-txtProgressBar(style = 3,width = 50)
rf_fit <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1")
plot(rf_fit)
# Testing
f1_eval=best_f1(rf_fit,test_data)
plot(plot(f1_eval$seuil,f1_eval$F1))
test_model(test_data,rf_fit,f1_eval$seuil[which.max(f1_eval$F1)])

# Methode re-echantillonnage ----------------------------------------------
ctrl_up <- trainControl(method = "cv",number = 10,summaryFunction = f1,search = "grid",sampling = "up")
ctrl_rose <- trainControl(method = "cv",number = 10,summaryFunction = f1,search = "grid",sampling = "rose")

#XGBOOST
k<<-0;pb<<-txtProgressBar(style = 3,width = 50)
rf_fit_up <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl_up,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1")
plot(rf_fit_up)
# Testing
f1_eval2=best_f1(rf_fit_up,test_data)
plot(plot(f1_eval2$seuil,f1_eval2$F1))
test_model(test_data,rf_fit_up,f1_eval2$seuil[which.max(f1_eval2$F1)])

#XGBOOST
k<<-0;pb<<-txtProgressBar(style = 3,width = 50)
rf_fit_rose <- train(match ~., data = train_data, method = "xgbTree",trControl=ctrl_rose,tuneGrid = tune_grid,tuneLength = 10,preProcess = c("scale", "center"),metric = "F1")
plot(rf_fit_rose)
# Testing
f1_eval3=best_f1(rf_fit_rose,test_data)
plot(plot(f1_eval3$seuil,f1_eval3$F1))
test_model(test_data,rf_fit_rose,f1_eval3$seuil[which.max(f1_eval3$F1)])







