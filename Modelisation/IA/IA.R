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
#install_tensorflow()

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







# Intelligence artificielle

#IMPORTATION JEU DE DONNEE
df_mod=feature_eng()
#var significatif 
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual)

# Creation des bases de test et d'apprentissage

resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest
#df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

dapp = data.frame(lapply(train_data, function(x) as.numeric(as.character(x))))
dtest = data.frame(lapply(test_data, function(x) as.numeric(as.character(x))))


# set.seed(1234)
# perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
# dapp <- df_mod2[perm,]
# dtest <- df_mod2[-perm,]


ncol = ncol(dapp) - 1 
xtrain = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain = dapp$match


xtest = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
ytest = dtest$match


#Optim

# resultat pour gamma = 
#res = optimisationNeuroneDeeplearning(0.2, 10, 0.5, 700, xtrain, xtest,ytrain, ytest); # 1 et 6.7, 9.4


# loss function et metrics


focal_loss=function(y_true, y_pred){
  gamma = 6.4
  
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
  layer_dense(units = round(ncol*0.15),  activation = "sigmoid") %>%
  layer_dropout(0.3)  %>%
  layer_dense(units = round(ncol*0.15), activation ="relu") %>%
  layer_dropout(0.3)  %>%
  
  layer_dense(units = 1, activation = "sigmoid")




# compilation, apprentissage et prédiction

model %>% compile(
  loss = focal_loss,
  optimizer = 'adam',
  metrics = f1_m
)
history <- model %>% fit(
  xtrain,  ytrain, 
  
  batch_size =0.05,epochs = 200,
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

model <- keras_model_sequential()
model %>% 
  layer_dense(units = round(ncol*0.8), input_shape = c(ncol), activation = "sigmoid") %>%
  layer_dropout(0.3)  %>%
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
  
  batch_size =0.05,epochs = 200,
  validation_split = 0.2,
  class_weight=list("0"=1,"1"=6.0284857571)
)


#prédiction sur l'échantillon test
predSimple <- model %>% predict_classes(xtest)
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest),positive="1",mode = "prec_recall"))


############### ROSE #######################


df_mod=feature_eng()
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual,0.1)
#Split
resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest

#base normale
#df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

dapp = data.frame(lapply(train_data, function(x) as.numeric(as.character(x))))
dtest = data.frame(lapply(test_data, function(x) as.numeric(as.character(x))))

# set.seed(1234)
# perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
# dapp <- df_mod2[perm,]
# dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain_normal = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain_normal = dapp$match


xtest_normal = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
ytest_normal = dtest$match

# base rose

df_mod <- ROSE(match ~ ., data = df_mod, seed = 1)$data
resu=creationDesData(df_mod,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest
#df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

dapp = data.frame(lapply(train_data, function(x) as.numeric(as.character(x))))
dtest = data.frame(lapply(test_data, function(x) as.numeric(as.character(x))))

# set.seed(1234)
# perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
# dapp <- df_mod2[perm,]
# dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain_rose = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain_rose = dapp$match
Xtest_rose = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
Ytest_rose = dtest$match



# creation du modèle

model <- keras_model_sequential()
model %>% 
  layer_dense(units = ncol, input_shape = c(ncol), activation = "sigmoid") %>%
  layer_dense(units = ncol,  activation = "relu") %>%
  layer_dropout(0.3)  %>%
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
  xtrain_rose,  ytrain_rose, 
  batch_size =0.1,epochs = 100,
  validation_split = 0.1
)
#prédiction sur l'échantillon test
predSimple <- model %>% predict_classes(xtest_normal)
#print(table(predSimple))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1",mode = "prec_recall"))


################################# OverSampling  #######################


df_mod=feature_eng()
var_signif=feature_selection(df_mod$df,df_mod$var_num,df_mod$var_qual,0.1)
#Split
resu=creationDesData(df_mod$df,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest

#base normale
#df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

dapp = data.frame(lapply(train_data, function(x) as.numeric(as.character(x))))
dtest = data.frame(lapply(test_data, function(x) as.numeric(as.character(x))))

# set.seed(1234)
# perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
# dapp <- df_mod2[perm,]
# dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain_normal = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain_normal = dapp$match


xtest_normal = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
ytest_normal = dtest$match


# base oversampling

df_mod <- ovun.sample(match ~ ., data = df_mod, method = "over",N = 2*(nrow(df_mod) - sum(as.numeric(df_mod$match)-1)))$data

table(df_mod$match)


resu=creationDesData(df_mod,var_signif)
df_mod = resu$df_mod;train_data = resu$dapp;test_data = resu$dtest
#df_mod2 <- data.frame(lapply(df_mod, function(x) as.numeric(as.character(x))))

dapp = data.frame(lapply(train_data, function(x) as.numeric(as.character(x))))
dtest = data.frame(lapply(test_data, function(x) as.numeric(as.character(x))))

# set.seed(1234)
# perm <- sample(1:nrow(df_mod2),round(0.7*nrow(df_mod2)))
# dapp <- df_mod2[perm,]
# dtest <- df_mod2[-perm,]

ncol = ncol(dapp) - 1 
xtrain_over = as.matrix(dapp[, -which(names(dapp) %in% c("match"))])
ytrain_over = dapp$match
Xtest_over = as.matrix(dtest[, -which(names(dtest) %in% c("match"))])
Ytest_over = dtest$match



# creation du modèle

model <- keras_model_sequential()
model %>% 
  layer_dense(units = ncol, input_shape = c(ncol), activation = "sigmoid") %>%
  layer_dense(units = ncol,  activation = "relu") %>%
  layer_dropout(0.3)  %>%
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
  optimizer = 'adam',
  metrics = 'accuracy'
)
history <- model %>% fit(
  xtrain_over,  ytrain_over, 
  batch_size =0.1,epochs = 100,
  validation_split = 0.1
)
#prédiction sur l'échantillon test
predSimple <- model %>% predict_classes(xtest_normal)
#print(table(predSimple))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1",mode = "prec_recall"))


print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1"))







##################################

model_1 = load_model_hdf5("C:/Users/jacta/Desktop/4GM/Projet-SpeedDating/Modelisation/IA/model_f1_over_5752_2.hdf5")

l = seq(0.05,0.95,0.01)
f1_list = sapply(l, function(s) F1_Score(as.integer(predict_proba(model_1,xtrain_normal)>s), ytrain_normal, positive = "1"))
f1_list

meilleur_seuil_1 = l[which.max(f1_list)]
f1_1 = f1_list[which.max(f1_list)]
F1_Score(as.integer(predict_proba(model_1,xtest_normal)>meilleur_seuil_1), ytest_normal, positive = "1")

##################################

model_2 = load_model_hdf5("C:/Users/jacta/Desktop/4GM/Projet-SpeedDating/Modelisation/IA/model_f1_rose_41365_2.hdf5")

l = seq(0.05,0.95,0.01)
f1_list = sapply(l, function(s) F1_Score(as.integer(predict_proba(model_2,xtrain_normal)>s), ytrain_normal, positive = "1"))
f1_list

meilleur_seuil_2 = l[which.max(f1_list)]
f1_2 = f1_list[which.max(f1_list)]
F1_Score(as.integer(predict_proba(model_2,xtest_normal)>meilleur_seuil_2), ytest, positive = "1")

##################################

model_3 = load_model_hdf5("C:/Users/jacta/Desktop/4GM/Projet-SpeedDating/Modelisation/IA/model_f1_over_5008_2.hdf5")

l = seq(0.05,0.9,0.01)
f1_list = sapply(l, function(s) F1_Score(as.integer(predict_proba(model_3,xtrain_normal)>s), ytrain_normal, positive = "1"))
f1_list

meilleur_seuil_3 = l[which.max(f1_list)]
f1_3 = f1_list[which.max(f1_list)]
F1_Score(as.integer(predict_proba(model_3,xtest_normal)>meilleur_seuil_3), ytest, positive = "1")


######################### combinaison


predSimple = as.integer(as.integer(predict_proba(model_1,xtest_normal)>0.5)
                        +as.integer(predict_proba(model_2,xtest_normal)>0.5)
                        +as.integer(predict_proba(model_3,xtest_normal)>0.5)
                        >1.5)


print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1",mode = "prec_recall"))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1"))

# avec meilleur seuil : f1_score de 0.5928 >> meilleur modele a ce jour


predSimple = as.integer(as.integer(predict_proba(model_1,xtest_normal)>meilleur_seuil_1)
                        +as.integer(predict_proba(model_2,xtest_normal)>meilleur_seuil_2)
                        +as.integer(predict_proba(model_3,xtest_normal)>meilleur_seuil_3)
                        >1.5)


print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1",mode = "prec_recall"))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1"))

######################### avec pondération sur les probas

predSimple = as.integer(f1_1*predict_proba(model_1,xtest_normal)
                        + f1_2*predict_proba(model_2,xtest_normal)
                        + f1_3*predict_proba(model_3,xtest_normal)
                                   >(f1_1+f1_2+f1_3)/2)

print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1",mode = "prec_recall"))
print(caret::confusionMatrix(data=factor(predSimple),reference=factor(ytest_normal),positive="1"))
