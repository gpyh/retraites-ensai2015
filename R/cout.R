#' Renvoit le taux de revalorisation à une date donnée.
#' Taux de revalorisation réelle du 01/01/2006 au 01/04/2011 
#' puis taux de 17,4% chaque premier janvier des années suivantes
#' 
#' 
tauxrevalo=function(date){
  r=c("2004-01-01"=0.015,
      "2005-01-01"=0.020,
      "2006-01-01"=0.018,
      "2007-01-01"=0.018,
      "2008-01-01"=0.011,
      "2008-09-01"=0.0191,
      "2009-04-01"=0.010,
      "2010-04-01"=0.009,
      "2011-04-01"=0.021,
      "2012-04-01"=0.021,
      "2013-04-01"=0.013
  )
  ifelse(
    day(date)!=1,
    0,
    ifelse(
      as.character(date)%in%names(r),
      r[as.character(date)],
      ifelse(
        month(date)==1&year(date)>2011,
        0.01740924,
        0)
    ))
}

#' Renvoit le coefficient de revalorisation
#' pour une période de liquidation et une date de décès données
coefrevalo=function(periode,deces){
  
  deces=floor_date(deces,"month")
  
  DT=unique(data.table(d=floor_date(deces),l=periode,key=c("d","l")),by=NULL)
  
  horner=function(...){
    Reduce(
      f=function(a,b){
        b*a + 1
      },right=TRUE,
      ...)
  }
  
  DT[,R:=tail(
    horner(
      x=tauxrevalo(l)+1,
      init=horner(x=tauxrevalo(l[.N]+months(0:diffm(l[.N],d[1])))+1),
      accumulate=TRUE),
    -1),
    by=d]
  
  DT[list(deces,periode),R]
}

#' A partir d'une data.table contenant deux colonnes :
#' - strate : une observation par strate
#' - variable (nom quelconque) : statistique calculée pour chaque strate
#' Renvoit une liste contenant :
#' - strates : un vecteur contenant la stat pour chaque strate
#' - total : un vecteur contenant la somme de la stat pour toutes les strates
creer_stat=function(DT){
  stat=list()
  vector=c(DT[,!"strate",with=FALSE][,1,with=FALSE])[[1]]
  names(vector)=DT$strate
  stat$strates=vector
  stat$total=sum(stat$strates)
  return(stat)
}
