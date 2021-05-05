
# Gestion Variables

# Undergra
gestionUndergra = function(df){
  df$undergra = as.character(df$undergra)
  df$undergra[which(df$undergra!="")] = 1
  df$undergra[which(df$undergra=="")] = 0
  df$undergra = factor(df$undergra)
  
  return(df)
}


# Career
gestionCareer = function(df){
  #Si career : lawyer alors career_c : 1
  df = df %>% mutate(career_c = ifelse(career=="lawyer" | career=="law", 1, career_c))
  
  #Si career : Economist alors career_c : 7 (with Finance...)
  df = df %>% mutate(career_c = ifelse(career=="Economist", 7, career_c))
  
  #Si career : tech professional  alors career_c : 7 (with Finance...)
  df = df %>% mutate(career_c = ifelse(career=="tech professional", 5, career_c))
  
  #Suppression des autres individus : quasiment toutes leurs variables sont nulles
  to_delete = df$career_c %>% is.na %>% which
  df = df[!seq_len(nrow(df)) %in% to_delete, ]
  
  df %>% dplyr::select(c("career_c","career"))
  
  return(df)
}



# Income
gestionIncome = function(df){
  income_ = as.character(factor(df$income)) # on convertit en character les valeurs
  
  for(i in 1:dim(df)[1]){
    string = income_[i] # on prend notre character
    id  = nchar(string) - 6 # l'indice où est la virgule
    if(id>0 & !is.na(id)){
      str_sub(string, id, id) <- "" # on remplace la virgule par rien
    }
    income_[i] = string
  }
  
  df$income = as.numeric(income_) # on change toutes les valeurs comme il faut dans le dataframe
  
  return(df)
}



# Essayer d'enlever les Na

# Agregation
agregationVariable = function(df){
  df2=df
  # CODE POUR GERER LE REGROUPEMENT DE VARIABLE
  attribut=c("attr","sinc","intel","fun","amb","shar")
  time=c(as.character(1:3),"s")
  catg=c(1:5,7)
  
  var_gen=outer(attribut,outer(catg,paste0("_",time),paste0),paste0)
  var_avg=outer(attribut,outer(catg,"_avg",paste0),paste0)
  
  #Etape 1 : harmoniser le systeme de point
  for(k in c(1,2,4)){#theme qui ont besoin d'être harmonisé
    for(i in 1:4){
      if(all(var_gen[,k,i]%in%names(df2))){
        df2[,var_gen[,k,i]]=sapply(var_gen[,k,i],function(v) 100*df2[,v]/apply(df2[,var_gen[,k,i]],1,sum,na.rm = T)) 
      }
    }
  }
  
  #ETAPE2 : regroupement
  for(k in 1:6){#catg/
    for(i in 1:6){#attribut
      v=var_gen[i,k,][which(var_gen[i,k,]%in%names(df2))]
      df2[,var_avg[i,k,1]]= apply(df2[,v],1,mean,na.rm = T)
    }
  }
  
  #ETAPE3 : Supression des anciennes variables
  v=as.vector(var_gen)
  v=v[which(v%in%names(df2))]
  df2=df2[,-which(names(df2)%in%v)]
  
  remove(attribut, catg, i, k, time, v, var_avg, var_gen)
  
  return(df2)
}


aggregateCarrer=function(df_individu){
  #carrer_cd
  law=c(1)
  science=c(4,5,12)
  huma=c(3,9,11,13,16)
  art=c(6,17)
  business=c(7,8)
  chercheur=c(2)
  sportif=c(14)
  autre=c(10,15)
  
  df_individu$career_c2=8
  df_individu$career_c2[df_individu$career_c%in%law]=1 #inutile mais bon
  df_individu$career_c2[df_individu$career_c%in%science]=2
  df_individu$career_c2[df_individu$career_c%in%huma]=3
  df_individu$career_c2[df_individu$career_c%in%art]=4
  df_individu$career_c2[df_individu$career_c%in%business]=5
  df_individu$career_c2[df_individu$career_c%in%chercheur]=6
  df_individu$career_c2[df_individu$career_c%in%sportif]=7
  
  return(df_individu)
}

aggregateField=function(df_individu){
  #field_cd
  law=c(1)
  science=c(2,4,5,10)
  huma=c(3,6,7,16,13,9)
  art=c(14,15,17)
  business=c(8)
  autre=c(12,17)
  
  df_individu$field_cd2=6
  df_individu$field_cd2[df_individu$field_cd%in%law]=1 #inutile mais bon
  df_individu$field_cd2[df_individu$field_cd%in%science]=2
  df_individu$field_cd2[df_individu$field_cd%in%huma]=3
  df_individu$field_cd2[df_individu$field_cd%in%art]=4
  df_individu$field_cd2[df_individu$field_cd%in%business]=5
  
  return(df_individu)
}

aggregateActivitie=function(df_individu){
  #activite1
  df_individu$act_sports=apply(df_individu,1,function(x) mean(x[c("sports","tvsports","exercise","hiking","yoga","clubbing")],na.rm=T))
  df_individu$act_art=apply(df_individu,1,function(x) mean(x[c("museums","art","reading","theater","movies","concerts","music")],na.rm=T))
  df_individu$act_autre=apply(df_individu,1,function(x) mean(x[c("dining","gaming","tv","shopping")],na.rm=T))
  
  #activite2
  df_individu$act_casanier=apply(df_individu,1,function(x) mean(x[c("tvsports","gaming","tv","yoga","art","reading","music","movies")],na.rm=T))
  df_individu$act_sortie=apply(df_individu,1,function(x) mean(x[c("sports","museums","theater","concerts","exercise","hiking","clubbing","dining","shopping")],na.rm=T))
  
  return(df_individu)
}

# Remplissage par la moyenne
gestionDesNASystematiques = function(df_individu){
  
  for(var in c("age","field_cd", "date", "amb2_avg", "shar2_avg")){
    id = which(is.na(df_individu[,var]))
    if(length(id)>0){
      df_individu = df_individu[-id,]
    }
  }
  
  for(var in c("attr4_avg", "sinc4_avg", "intel4_avg", "fun4_avg", "amb4_avg", "shar4_avg", "attr5_avg", "sinc5_avg", "intel5_avg", "fun5_avg", "amb5_avg")){
    id = which(is.na(df_individu[,var]))
    m = mean(df_individu[-id,var])
    df_individu[id,var] = m
  }
  
  return(df_individu)
}






creationDf_Couple = function(df,df_individu){
  df_couple_id = df[,c('iid','pid', 'match',"gender")]
  df_couple_id=df_couple_id[order(df_couple_id$gender),]
  
  coupleDated = c()
  ligneAenlever = c()
  
  for(i in 1:dim(df_couple_id)[1]){
    iid = df_couple_id$iid[i]
    pid = df_couple_id$pid[i]
    
    strCouple = paste(as.character(iid),as.character(pid))
    strCoupleInv = paste(as.character(pid),as.character(iid))
    
    if(!(strCoupleInv %in% coupleDated)){
      coupleDated = c(coupleDated,strCouple)
    }
    else{
      ligneAenlever = c(ligneAenlever,i)
    }
  }
  df_couple_id= df_couple_id[-ligneAenlever,]%>%select(-c("gender"))
  
  df_couple = merge(merge(df_couple_id, df_individu, by = 'iid'), df_individu, by.x = 'pid', by.y = 'iid')
  
  
  return(df_couple)
}

######################################################################♠

creation_avg_ect=function(df_couple2){
  var_num=c("act_sortie","act_casanier","act_autre","act_art","act_sports","age","imprace","imprelig","date","go_out",
            "sports", "tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga",
            paste0("attr",1:5,"_avg"),paste0("sinc",1:5,"_avg"),paste0("intel",1:5,"_avg"),paste0("fun",1:5,"_avg"),paste0("amb",1:5,"_avg"),paste0("shar",c(1,2,4),"_avg")
  )
  
  #Calcule moy et ect
  for(v in var_num){
    df_couple2[,paste0(v,"_moy")]=apply(df_couple2[,paste0(v,c(".x",".y"))],1,mean,na.rm=F)
    df_couple2[,paste0(v,"_ect")]=apply(df_couple2[,paste0(v,c(".x",".y"))],1,function(x) abs(x[1]-x[2]))
  }
  
  # Calcule de distance
  #activite
  activite=c("sports", "tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga")
  df_couple2$act_dist=apply(df_couple2[,paste0(activite,"_ect")]^2,1,function(x) sqrt(sum(x)))
  #activite1
  df_couple2$act1_dist=apply(df_couple2[,paste0(c("act_sortie","act_casanier"),"_ect")]^2,1,function(x) sqrt(sum(x)))
  #activite2
  df_couple2$act2_dist=apply(df_couple2[,paste0(c("act_autre","act_art","act_sports"),"_ect")]^2,1,function(x) sqrt(sum(x)))
  
  #Attribut
  attribut=c("attr","sinc","intel","fun","amb","shar")
  #Ce que tu cherches chez l'autre
  df_couple2$attribut1_dist=apply(df_couple2[,paste0(attribut,"1_avg_ect")]^2,1,function(x) sqrt(sum(x)))
  #Ce que tu penses que les gens de sexe opposé cherchent chez l'autre
  df_couple2$attribut2_dist=apply(df_couple2[,paste0(attribut,"2_avg_ect")]^2,1,function(x) sqrt(sum(x)))
  #Ce que tu penses de toi
  df_couple2$attribut3_dist=apply(df_couple2[,paste0(attribut[-6],"3_avg_ect")]^2,1,function(x) sqrt(sum(x)))
  #Ce que tu penses que ton sexe cherche chez l'autre
  df_couple2$attribut4_dist=apply(df_couple2[,paste0(attribut,"4_avg_ect")]^2,1,function(x) sqrt(sum(x)))
  #Ce que les autres pensent de toi
  df_couple2$attribut5_dist=apply(df_couple2[,paste0(attribut[-6],"5_avg_ect")]^2,1,function(x) sqrt(sum(x)))
  
  df_couple2$correspondance1_dist=apply((df_couple2[,paste0(attribut[-6],"1_avg.x")]-df_couple2[,paste0(attribut[-6],"3_avg.y")])^2,
                                        1,function(x) sqrt(sum(x)))
  
  df_couple2$correspondance2_dist=apply((df_couple2[,paste0(attribut[-6],"1_avg.y")]-df_couple2[,paste0(attribut[-6],"3_avg.x")])^2,
                                        1,function(x) sqrt(sum(x)))
  
  
  var_num=unlist(lapply(var_num,function(x) paste0(x,c(".x",".y","_moy","_ect"))))
  var_num=c(var_num,"act_dist","act1_dist","act2_dist","attribut1_dist","attribut2_dist","attribut3_dist","attribut4_dist","attribut5_dist","correspondance1_dist","correspondance2_dist")
  
  
  return(list("df"=df_couple2, "var_num" =  var_num))
}

creation_same_ind=function(df_couple2){
  
  var_qual=c("field_cd","race", "goal","career_c")
  var_qual2=unlist(lapply(var_qual,function(x) paste0(x,c(".x",".y"))))
  
  
  #Variable qualitative Solution1: varaible somme de modalite + indicatrice same modalitee
  for( v in var_qual){
    v_modalite=unique(c(df_couple2[,paste0(v,".x")],df_couple2[,paste0(v,".y")]))
    #indicatrice same
    var_qual2=c(var_qual2,paste0("same_",v))
    df_couple2[,paste0("same_",v)]= as.numeric(df_couple2[,paste0(v,".x")]==df_couple2[,paste0(v,".y")])
    #comptage
    for(mod in v_modalite){
      var_qual2=c(var_qual2,paste0(v,"_",mod))
      df_couple2[,paste0(v,"_",mod)]=apply(df_couple2[,paste0(v,c(".x",".y"))],1,function(x) sum(c(x[1]==mod,x[2]==mod),na.rm = T))
    }
  }
  for(var in c("match",var_qual2)){
    df_couple2[,var] = as.factor(df_couple2[,var])
  }
  
  return(list("df"=df_couple2, "var_qual" =  var_qual2))
}


feature_selection=function(df_couple2,var_num,var_qual2,seuil=0.05,nb_var_max=Inf){
  if(length(seuil)==1){
    seuil=c(seuil,seuil)
  }
  if(length(nb_var_max)==1){
    nb_var_max=c(nb_var_max,nb_var_max)
  }
  #Numerique
  var_num_sign=sapply(var_num,function(v)  t.test(df_couple2[,v]~df_couple2$match)$p.value)
  var_num_sign=var_num_sign[order(var_num_sign)]
  var_num_sign=var_num_sign[var_num_sign<=seuil[1]]
  var_num_sign=var_num_sign[1:min(nb_var_max[1],length(var_num_sign))]
  barplot(var_num_sign,main = "Test student variable numerique")
  
  
  #Qualitative
  #var_qual_sign=sapply(var_qual2,function(v) {cramerV(table(df_couple2[,c("match",v)]))})
  
  var_qual_sign=sapply(var_qual2,function(v)  chisq.test(df_couple2[,v],df_couple2$match)$p.value)
  var_qual_sign=var_qual_sign[order(var_qual_sign)]
  var_qual_sign=var_qual_sign[var_qual_sign<=seuil[2]]
  var_qual_sign=var_qual_sign[1:min(nb_var_max[2],length(var_qual_sign))]
  
  return(c("match",names(c(var_qual_sign,var_num_sign))))
}

# creation de df_mod, dapp et dtest

# noFactor sert pour le réseau de neurone, un réseaud de neurone ne peut pas prendre de factor
creationDesData = function(df_couple2, varSignificatifs = FALSE, varSupp = FALSE, noFactor = FALSE,p_split=0.7){
  if(length(varSignificatifs) > 1){
    df_mod = df_couple2[,which(names(df_couple2) %in% varSignificatifs)]
  }
  
  if(length(varSupp) > 1){
    if(length(which(names(df_mod) %in% varSupp))>0){
      df_mod =df_mod[,-which(names(df_mod) %in% varSupp)]
    }
  }
  
  if(noFactor){
    for(var in varFactor){
      if(var %in% names(df_mod)){
        df_mod[,var] = as.numeric(df_mod[,var]) -1
      }
    }
    remove(var)
    
    df_mod[,which(names(df_mod) %in% varFactor)] = df_mod[,which(names(df_mod) %in% varFactor)] -1
    df_mod$match = df_mod$match + 1
  }
  
  
  set.seed(1234)
  perm <- createDataPartition(df_mod$match, p = p_split, list = FALSE)
  dapp <- df_mod[perm,]
  dtest <- df_mod[-perm,]
  
  
  return(list(df_mod = df_mod,dapp = dapp,dtest = dtest))
  
}



optimisationNeuroneDeeplearning = function(gamma_min, gamma_max, gamma_pas, epoch, xtrain, xtest,ytrain, ytest){
  sol = as.data.frame(c(0,0,0))
  
  for( i in seq(gamma_min, gamma_max, gamma_pas)){
    
    focal_loss=function(y_true, y_pred){
      gamma = i 
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
      loss = focal_loss,
      optimizer = 'adam',
      metrics = f1_m
    )
    
    history <- model %>% fit(
      xtrain,  ytrain, 
      batch_size =0.05,epochs = epoch,
      validation_split = 0.15,
      view_metrics = FALSE
    );
    
    
    #prédiction sur l'échantillon test
    predSimple <- model %>% predict_classes(xtest)
    #print(table(predSimple))
    acc = sum(predSimple == ytest)/length(ytest)
    if(sum(ytest) != 0){
      sensitivity = sum(predSimple == ytest && ytest == 1)/sum(ytest)
    }else{
      sensitivity = 0
    }
    
    sol = cbind(sol, c(i,acc,sensitivity))
    print(c(i,acc,sensitivity))
    
    remove(model, history, predSimple, acc, sensitivity, focal_loss)
  }
  
  return(sol)
  
}








f1 <- function (data, lev = NULL, model = NULL) {
  experience<<-data
  precision <- Precision(data$pred, data$obs,positive ="1")
  recall  <- Recall(data$pred, data$obs,positive ="1")
  f1_val <- F1_Score(data$pred, data$obs,positive ="1")
  resu=c("precision"=precision,"rappel"=recall,"F1"=f1_val)
  print(resu)
  resu
}



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
