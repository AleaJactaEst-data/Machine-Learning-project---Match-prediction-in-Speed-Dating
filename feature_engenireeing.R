library(rcompanion)
library(caret)

feature_eng=function(){
  df_couple2=importation()
  
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
  
  #Significativite
  var_num2=unlist(lapply(var_num,function(x) paste0(x,c(".x",".y","_moy","_ect"))))
  var_num_sign=sapply(var_num2,function(v)  t.test(df_couple2[,v]~df_couple2$match)$p.value)
  var_num_sign=var_num_sign[order(var_num_sign)]
  
  barplot(var_num_sign,main = "pvalue test student egalite moyenne")
  barplot(var_num_sign[var_num_sign<=0.05],main = "pvalue test student egalite moyenne des var significatives")
  
  
  
  featurePlot(x = df_couple2[, c("gender.x","gender.y",names(var_num_sign[order(var_num_sign)][1:5]))], 
              y = as.factor(df_couple2$match), 
              plot = "ellipse")
  
  # Var qualitative ---------------------------------------------------------
  
  var_qual=c("field_cd","race", "goal","date","go_out","career_c")
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
  #sigificativite cramer
  var_qual_sign=sapply(var_qual2,function(v) {cramerV(table(df_couple2[,c("match",v)]))})
  
  var_qual_sign=sapply(var_qual2,function(v)  chisq.test(df_couple2[,v],df_couple2$match)$p.value)
  var_qual_sign=var_qual_sign[order(var_qual_sign)]
  
  barplot(var_qual_sign,main = "pvalue test chi2 egalite moyenne")
  barplot(var_qual_sign[var_qual_sign<=0.05],
          main = "pvalue test chi2 egalite moyenne des var significatives")
  
  
  # feature selection -------------------------------------------------------
  df_mod=df_couple2[,c("match",names(c(var_qual_sign[var_qual_sign>0.05],var_num_sign[var_num_sign>0.05])))]
  
  return(df_mod)
}
