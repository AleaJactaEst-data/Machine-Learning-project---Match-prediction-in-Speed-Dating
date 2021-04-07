importation=function(){
  df = read.csv("../../DataBase/Speed Dating Data.csv")
  # titr1 -------------------------------------------------------------------
  
  df = df %>% select(-c("positin1","position", "expnum","exphappy"))
  #Gestion des non diplomés
  df$undergra = as.character(df$undergra)
  df$undergra[which(df$undergra=="")] = "other"
  df$undergra = factor(df$undergra)
  
  #Gestion des indices metiers
  #Si career : lawyer alors career_c : 1
  df = df %>% mutate(career_c = ifelse(career=="lawyer" | career=="law", 1, career_c))
  
  #Si career : Economist alors career_c : 7 (with Finance...)
  df = df %>% mutate(career_c = ifelse(career=="Economist", 7, career_c))
  
  #Si career : tech professional  alors career_c : 7 (with Finance...)
  df = df %>% mutate(career_c = ifelse(career=="tech professional", 5, career_c))
  
  #Suppression des autres individus : quasiment toutes leurs variables sont nulles
  to_delete = df$career_c %>% is.na %>% which
  df = df[!seq_len(nrow(df)) %in% to_delete, ]
  
  
  # code pour gérer l'écriture US des chiffres (virgule des milliers) ------------------------------------------------------------------
  
  income_ = as.character(factor(df$income)) # on convertit en charcter les valeurs
  
  for(i in 1:dim(df)[1]){
    string = income_[i] # on prend notre character
    id  = nchar(string) - 6 # l'indice où est la virgule
    if(id>0 & !is.na(id)){
      str_sub(string, id, id) <- "" # on remplace la virgule par rien
    }
    income_[i] = string
  }
  
  df$income = as.numeric(income_) # on change toutes les valeurs comme il faut dans le dataframe
  remove(income_)
  remove(string)
  remove(to_delete)
  remove(id)
  remove(i)
  
  
  # CODE POUR GERER LE REGROUPEMENT DE VARIABLE -------------------------------------------------------------------
  
  df2=df
  
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
  
  
  # Individus -------------------------------------------------------------------
  df_individu = df2[!duplicated(df2[,c('iid')]),]
  
  listeASupp = c('id','idg','round','wave','order','condtn','int_corr','partner','pid', 'match','samerace','age_o','race_o', 'pf_o_sin','pf_o_int','pf_o_fun','pf_o_amb','pf_o_sha','dec_o', 'sinc_o','attr_o', 'intel_o','fun_o','amb_o','shar_o','like_o','prob_o','met_o','match_es','pf_o_att','dec','attr','sinc','intel','fun','fun','amb','shar','like','prob','met','you_call','them_cal','length', 'date_3', 'numdat_3', 'num_in_3','numdat_2', 'satis_2','positin1','position', 'expnum','attr7_avg','sinc7_avg','intel7_avg','fun7_avg','amb7_avg','shar7_avg','shar3_avg','shar5_avg')
  
  listeASupp=c(listeASupp,c("field","undergra","mn_sat","tuition","from","zipcode","career","income"))
  
  df_individu = df_individu[,-which(names(df2) %in% listeASupp)]
  
  
  # GESTION DES NA A COMPLETER  -------------------------------------------------------------------
  
  for(var in c("attr4_avg", "sinc4_avg", "intel4_avg", "fun4_avg", "amb4_avg", "shar4_avg", "attr5_avg", "sinc5_avg", "intel5_avg", "fun5_avg", "amb5_avg")){
    id = which(is.na(df_individu[,var]))
    m = mean(df_individu[-id,var])
    df_individu[id,var] = m
  }
  
  for(var in c("age","field_cd", "date", "amb2_avg", "shar2_avg")){
    id = which(is.na(df_individu[,var]))
    if(length(id)>0){
      df_individu = df_individu[-id,]
    }
  }
  
  
  # Couple .x correspond au sexe 0 et .y correspond au sexe 1------------------------------------------------------------------
  df_couple_id = df2[,c('iid','pid', 'match',"gender")]
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
  
  
  remove(coupleDated, i, id, iid, ligneAenlever, listeASupp, m,pid,strCouple,strCoupleInv, var,df_couple_id)
  
  return(df_couple%>%select(-c("pid","iid")))
  
}


