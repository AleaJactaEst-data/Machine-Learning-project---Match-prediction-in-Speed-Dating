library(rcompanion)
library(caret)

feature_eng=function(){
  df_couple2=importation()
  
  # Var numerique -----------------------------------------------------------
  
  resu=creation_avg_ect(df_couple2)
  df_couple2=resu$df
  var_num = resu$var_num
    
  # Var qualitative ---------------------------------------------------------
  
  resu=creation_same_ind(df_couple2)
  df_couple2=resu$df
  var_qual = resu$var_qual

  
  return(list("df"=df_couple2,"var_num"=var_num,"var_qual"=var_qual))
}
