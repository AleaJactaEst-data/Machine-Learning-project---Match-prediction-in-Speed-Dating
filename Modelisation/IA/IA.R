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
library(tensorflow)
library(keras)

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




# Intelligence artificielle

# Creation des bases de test et d'apprentissage

resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest
df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

set.seed(1234)
perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
dapp <- df_mod2[perm,]
dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain = dapp$match


xtest = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
ytest = dtest$match


#Optim

# resultat pour gamma = 
#öres = optimisationNeuroneDeeplearning(0.1, 12, 0.3, 700, xtrain, xtest,ytrain, ytest);
#res = optimisationNeuroneDeeplearning(0.2, 4, 0.1, 700, xtrain, xtest,ytrain, ytest); # >> 0.2 et 3.5 
#res_0.2 = optimisationNeuroneDeeplearning(0.05, 0.3, 0.03, 700, xtrain, xtest,ytrain, ytest); # >> 0.05, 0.14 c'est allé jusqu'à 0.41
#res_3.5 = optimisationNeuroneDeeplearning(3.4, 3.6, 0.02, 700, xtrain, xtest,ytrain, ytest);


# loss function et metrics


focal_loss=function(y_true, y_pred){
  gamma = 5
  
  print("gamma")
  pt = y_pred * y_true + (1-y_pred) * (1-y_true)
  print("pt")
  pt = k_clip(pt, 0, 1)
  print("pt2")
  CE = -k_log(pt+k_epsilon())
  print("CE")
  FL = k_pow(1-pt, gamma) * CE
  print("FL")
  loss = k_sum(FL, axis=1)
  print(loss)
  return(loss)
}


# creation du modèle

model <- keras_model_sequential()
model %>% 
  
  layer_dense(units = round(ncol*0.8), input_shape = c(ncol), activation = "sigmoid") %>%
  layer_dropout(0.3)  %>%
  layer_dense(units = round(ncol*0.8),  activation = "relu") %>%
  layer_dropout(0.1)  %>%
  layer_dense(units = round(ncol*0.5),  activation = "sigmoid") %>%
  layer_dropout(0.1)  %>%
  layer_dense(units = round(ncol*0.4),  activation = "relu") %>%
  layer_dropout(0.3)  %>%
  layer_dense(units = round(ncol*0.15),  activation = "sigmoid") %>%
  layer_dropout(0.3)  %>%
  layer_dense(units = round(ncol*0.15), activation ="relu") %>%
  layer_dropout(0.3)  %>%
  
  layer_dense(units = 1, activation = "sigmoid")




# compilation, apprentissage et prédiction

model %>% compile(
  loss = focal_loss,
  optimizer = 'adam',#optimizer_rmsprop()
  metrics = f1_m
)
history <- model %>% fit(
  xtrain,  ytrain, 
  
  batch_size =0.05,epochs = 300,
  validation_split = 0.2
)


#prédiction sur l'échantillon test
predSimple <- model %>% predict_classes(xtest)
#print(table(predSimple))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest),positive="1",mode = "prec_recall"))



########################### poids

# utilisation de poids << la formule pour trouver les poids
# est nb classe laplus grande / nb classe i
# classe 0 << 1
#classe 1 << 6.0284857571

#res = optimisationPoidsClasse(2, 20, 0.3, 800, xtrain, xtest,ytrain, ytest) sert a rien


model <- keras_model_sequential()
model %>% 
  
  layer_dense(units = round(ncol*0.8), input_shape = c(ncol), activation = "sigmoid") %>%
  layer_dropout(0.3)  %>%
  #layer_dense(units = round(ncol*0.7),  activation = "sigmoid") %>%#a enlever
  #layer_dropout(0.3)  %>%#a enlever
  #layer_dense(units = round(ncol*0.5),  activation = "sigmoid") %>%#a enlever
  #layer_dropout(0.3)  %>%# a enlever
  layer_dense(units = round(ncol*0.15),  activation = "sigmoid") %>%
  layer_dropout(0.3)  %>%
  layer_dense(units = round(ncol*0.15), activation ="relu") %>%
  layer_dropout(0.3)  %>%
  
  layer_dense(units = 1, activation = "sigmoid")



model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
  
)
history <- model %>% fit(
  xtrain,  ytrain, 
  
  batch_size =0.05,epochs = 300,
  validation_split = 0.2,
  class_weight=list("0"=1,"1"=6.0284857571) #4.77 cool / 6 tres cool
)


#prédiction sur l'échantillon test
predSimple <- model %>% predict_classes(xtest)
#print(table(predSimple))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest),positive="1",mode = "prec_recall"))


############### ROSE
df_mod=feature_eng()
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual,0.1)
#Split
resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest

#base normale
df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

set.seed(1234)
perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
dapp <- df_mod2[perm,]
dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain = dapp$match


xtest = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
ytest = dtest$match

# base rose

df_mod <- ROSE(match ~ ., data = df_mod, seed = 1)$data
df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

set.seed(1234)
perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
dapp <- df_mod2[perm,]
dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain_rose = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain_rose = dapp$match
Xtest_rose = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
Ytest_rose = dtest$match


# loss function et metrics


recall_m=function(y_true, y_pred){
  true_positives = k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))
  possible_positives = k_sum(k_round(k_clip(y_true, 0, 1)))
  recall = true_positives / (possible_positives + k_epsilon())
  return(recall)
} 
precision_m=function(y_true, y_pred){
  true_positives = k_sum(k_round(k_clip(y_true * y_pred, 0, 1)))
  predicted_positives = k_sum(k_round(k_clip(y_pred, 0, 1)))
  precision = true_positives / (predicted_positives + k_epsilon())
  return(precision)
}
# parametre beta a changer
f1_m=function(y_true, y_pred){
  beta = 1
  precision = precision_m(y_true, y_pred)
  recall = recall_m(y_true, y_pred)
  return((1+beta)^2*((precision*recall)/(precision+beta * recall+k_epsilon())))
} 
focal_loss=function(y_true, y_pred){
  gamma = 7.3 # 3.8 a 7.5 fonctionne > test - amelioration surechantilonage
  print("gamma")
  pt = y_pred * y_true + (1-y_pred) * (1-y_true)
  print("pt")
  pt = k_clip(pt, 0, 1)
  print("pt2")
  CE = -k_log(pt+k_epsilon())
  print("CE")
  FL = k_pow(1-pt, gamma) * CE
  print("FL")
  loss = k_sum(FL, axis=1)
  print(loss)
  return(loss)
}


# creation du modèle

model <- keras_model_sequential()
model %>% 
  layer_dense(units = ncol, input_shape = c(ncol), activation = "sigmoid") %>%
  layer_dense(units = ncol,  activation = "relu") %>%
  layer_dropout(0.3)  %>%
  #   layer_dense(units = round(ncol*0.9), activation = "sigmoid") %>%
  #   layer_dense(units = round(ncol*0.9), activation = "relu") %>%
  # layer_dropout(0.3)  %>% # trouver l'influence des drop out
  #   layer_dense(units = round(ncol*0.8), activation = "relu") %>%
  # layer_dropout(0.25)  %>%
  #   layer_dense(units = round(ncol*0.7), activation = "sigmoid") %>%
  #   layer_dense(units = round(ncol*0.7), activation = "relu") %>%
  
  # layer_dropout(0.2)  %>%
  #   layer_dense(units = round(ncol*0.5), activation = "sigmoid") %>%
  #   layer_dense(units = round(ncol*0.5), activation ="relu") %>%
# layer_dropout(0.2)  %>%
layer_dense(units = round(ncol*0.5), activation = "sigmoid") %>%
  layer_dense(units = round(ncol*0.5), activation ="relu") %>%
  layer_dropout(0.2)  %>%
  layer_dense(units = round(ncol*0.3),  activation = "sigmoid") %>%
  layer_dense(units = round(ncol*0.3), activation ="relu") %>%
  layer_dropout(0.1)  %>%
  layer_dense(units = 1, activation = "sigmoid")



# compilation, apprentissage et prédiction

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',#optimizer_rmsprop()
  metrics = 'accuracy'
)
history <- model %>% fit(
  xtrain,  ytrain, 
  batch_size =0.1,epochs = 500,
  validation_split = 0.1
)
#prédiction sur l'échantillon test
predSimple <- model %>% predict_classes(Xtest_rose)
#print(table(predSimple))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(Ytest_rose),positive="1"))



