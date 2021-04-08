
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


# Remplissage par la moyenne
gestionDesNASystematiques = function(df_individu){
  
  for(var in c("attr4_avg", "sinc4_avg", "intel4_avg", "fun4_avg", "amb4_avg", "shar4_avg", "attr5_avg", "sinc5_avg", "intel5_avg", "fun5_avg", "amb5_avg")){
    id = which(is.na(df_individu[,var]))
    m = mean(df_individu[-id,var])
    df_individu[id,var] = m
  }
  
  return(df_individu)
}






creationDf_Couple_Id = function(df){
  df_couple_id = df[,c('iid','pid', 'match')] # samerace a garder ? non on le recrée après
  
  
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
  df_couple_id= df_couple_id[-ligneAenlever,]
  
  return(df_couple_id)
}

######################################################################♠

creation_avg_ect=function(df_couple2){

  
  # Var numerique -----------------------------------------------------------
  
  var_num=c("age","imprace","imprelig",
            "sports", "tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading","tv","theater","movies","concerts","music","shopping","yoga",
            paste0("attr",1:5,"_avg"),paste0("sinc",1:5,"_avg"),paste0("intel",1:5,"_avg"),paste0("fun",1:5,"_avg"),paste0("amb",1:5,"_avg"),paste0("shar",c(1,2,4),"_avg")
  )
  
  #Supression ordre variables numerique
  for(v in var_num){
      df_couple2[,paste0(v,"_moy")]=apply(df_couple2[,paste0(v,c(".x",".y"))],1,mean,na.rm=F)
      df_couple2[,paste0(v,"_ect")]=apply(df_couple2[,paste0(v,c(".x",".y"))],1,function(x) abs(x[1]-x[2]))
      #df_couple2 = df_couple2 %>% select(-paste0(v,c(".x",".y")))
  }
  
  # Var qualitative ---------------------------------------------------------
  
  var_qual=c("field_cd","race", "goal","date","go_out","career_c")
  var_qual2=unlist(lapply(var_qual,function(x) paste0(x,c(".x",".y"))))
  
  
  
  #Variable qualitative Solution1: varaible somme de modalite + indicatrice same modalitee
  for( v in var_qual){
    if(v %in% df_couple2){
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

  }
  for(var in c("match",var_qual2)){
    if(v %in% df_couple2){
      df_couple2[,var] = as.factor(df_couple2[,var])
    }
  }
  return(list("df"=df_couple2, "var_num" =  var_num, "var_qual" = var_qual2 ))
}





# creation de df_mod, dapp et dtest

# noFactor sert pour le réseau de neurone, un réseaud de neurone ne peut pas prendre de factor
creationDesData = function(df_couple2, varSignificatifs = FALSE, varSupp = FALSE, noFactor = FALSE){
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
  
  
  n = nrow(df_couple2)
  set.seed(1234)
  perm <- sample(1:n, 3000)
  
  dapp <- df_mod[perm,]
  dtest <- df_mod[-perm,]
  
  
  
  
  
  return(list(df_mod = df_mod,dapp = dapp,dtest = dtest))
  
}
