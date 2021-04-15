importation=function(){
  pb=txtProgressBar(style = 3,width = 50)
  
  setTxtProgressBar(pb, 0.14)
  df = read.csv("DataBase/Speed Dating Data.csv")

  df = df %>% select(-c("positin1","position", "expnum","exphappy"))
  
  #Nettoyage
  setTxtProgressBar(pb, 0.28)
  df = df%>%gestionUndergra()%>%gestionCareer()%>%gestionIncome()

  
  #Homogénisation systeme de points + aggregation
  setTxtProgressBar(pb, 0.42)
  df=agregationVariable(df)
 
  
  # Individus
  setTxtProgressBar(pb, 0.6)
  df_individu = df[!duplicated(df[,c('iid')]),]
  
  listeASupp = c('id','idg','round','wave','order','condtn','int_corr','partner','pid', 'match','samerace','age_o','race_o', 'pf_o_sin','pf_o_int','pf_o_fun','pf_o_amb','pf_o_sha','dec_o', 'sinc_o','attr_o', 'intel_o','fun_o','amb_o','shar_o','like_o','prob_o','met_o','match_es','pf_o_att','dec','attr','sinc','intel','fun','fun','amb','shar','like','prob','met','you_call','them_cal','length', 'date_3', 'numdat_3', 'num_in_3','numdat_2', 'satis_2','positin1','position', 'expnum','attr7_avg','sinc7_avg','intel7_avg','fun7_avg','amb7_avg','shar7_avg','shar3_avg','shar5_avg')
  
  listeASupp=c(listeASupp,c("field","undergra","mn_sat","tuition","from","zipcode","career","income"))
  
  df_individu = df_individu[,-which(names(df) %in% listeASupp)]
  
  
  # Aggregation de variable
  setTxtProgressBar(pb, 0.7)
  df_individu=df_individu%>%aggregateField()%>%aggregateCarrer()%>%aggregateActivitie()
  
  
  # GESTION DES NA
  setTxtProgressBar(pb, 0.8)
  df_individu=gestionDesNASystematiques(df_individu)
  
  # Creation df_couple
  setTxtProgressBar(pb, 0.9)
  df_couple=creationDf_Couple(df,df_individu)
  
  return(df_couple%>%select(-c("pid","iid")))
  
}


