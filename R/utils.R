#' Capture l'appel à une fonction de modélisation 
#' et remplace l'argument \code{data} par ses parties successives
#' selon la variable strate. 
#' Renvoit une liste de modèles : un pour chaque strate
stratifier=function(modelecall){
  modelecall=substitute(modelecall)
  DT=eval.parent(modelecall$data)
  strates=levels(DT$strate)
  calls=list()
  for(unestrate in strates){
    calls[[unestrate]]=modelecall
    calls[[unestrate]]$data=quote(DT[strate==unestrate])
  }
  
  modeles=foreach(unestrate=levels(m$strate),.combine="c") %dopar%{
    modele=list(eval(calls[[unestrate]],environment()))
    names(modele)=unestrate
    modele
  }
  
  return(modeles)
}

#' Renvoit un vecteur de proportions à partir d'un vecteur de valeurs
prop=function(x){
  x/sum(x,na.rm=TRUE)
}

#' Renommer facilement des colonnes dans une data.table à partir d'une liste
renamecols=function(DT,matching){
  setnames(
    DT,
    old=names(matching),
    new=unlist(matching))
}

#' Remplacer les valeurs manquantes dans un facteur par un nouveau label
replace.na=function(f,label){
  f=addNA(f)
  levels(f)[is.na(levels(f))]=label
  return(f)
}

#' Tableau de contingence avec valeurs manquantes
tb=function(...) table(...,useNA="ifany")

#' A partir d'une data.table DT et d'une liste de "motifs", crée une liste contenant :
#' - \code{data} : une data.table contenant la variable `id` de DT et identifiant les observations répondant aux conditions de chaque motif
#' - \code{tableau} : une `table` qui pour chaque motif identifie les effectifs pour chaque année de naissance
#' - \code{champ} : vecteur identifiant dans DT les observations ne répondant pas aux conditions des motifs
champ=function(DT,motifs) {
  motifs=eval(substitute(motifs), DT, enclos = parent.frame())
  champ=list()
  champ$champ=1*(!Reduce(`|`,motifs))
  champ$tableau=t(rbind(
    sapply(motifs,function(x) table(x,year(DT$naissance))["TRUE",])
  ))
  champ$data=cbind(id=b[,id],as.data.table(motifs))
  setkey(champ$data,id)
  return(champ)
}

#' Différence en mois entre deux dates
#' Le paramètre `millesime` détermine si la distance doit être millésimée.
#' Par exemple si on se sert de cette fonction pour calculer l'âge en mois le 01/01/2010 d'un individu né le 15/02/1946, le résultat sera 766 mois pour un âge révolu et 767 mois pour un âge millésimé.
diffm=function(debut,fin,millesime=FALSE) {
  (year(fin)-year(debut))*12 +
    (month(fin)-month(debut)) -
    (mday(debut)>mday(fin) & debut<fin & !millesime)*1 +
    (mday(debut)<=mday(fin) & fin<debut & !millesime)*1
}

#' Différence en jours entre deux dates
diffj=function(debut,fin) {
  as.double(fin-debut,units="days")
}

#' Convertit un nombre de jours depuis le 01/01/1960 dans un calendrier à 360 jours en une date réelle
c360todate=function(x) {
  ((x%/%360+1960)*10000 +
     ((x%/%30)%%12+1)*100 +
     x%%30+1) %>%
    ymd
}

#' Convertit une date réelle en nombre de jours depuis le 01/01/1960 dans un calendrier à 360 jours
datetoc360=function(x) {
  (year(x)-1960)*360 +
    (month(x)-1)*30 +
    mday(x)-1
}
