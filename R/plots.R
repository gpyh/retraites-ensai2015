# Fonctions pour graphiques

#' Courbes de survie Kaplan-Meier.
#'
#' Renvoit un objet de classe \code{ggplot} permetant de représenter des fonctions de survie.
#' estimées par les méthode de Kaplan-Meier.
#' Les courbe sont accompagnées de l'aire représentant un intervalle de confiance à 95%.
#'
#' @param data une data.table contenant les données ; doit renseigner la variable \code{jouini.agem} (entier).
#' @param by permet de représenter plusieurs courbes sur un même graphique en renseignant sous forme de chaine de caractère la variable d'aggrégation dans \code{data} ; NULL si une seule courbe.
#' @param censure booléen qui déterminer si les départs doivent être censurés à 779 mois.
#' @param color si une seule courbe, détermine la couleur.
#' @examples
#' kmbrut <- kmplot(b,censure=FALSE,color="red")
#' kmsante <- kmplot(b[as.logical(champ)],by="sante")
kmplot=function(data,by=NULL,censure=TRUE,color="black") {

  if (isTRUE(censure)){
    km=data[,rbind(
      c(1,1,1,0),
      as.data.frame(unclass(
        survfit(Surv(time,status)~1,data=data.frame(
          time=pmin(jouini.agem,779),
          status=1*(jouini.agem<=779)))
      )[c("upper","lower","surv","time")])
    ),by=by]
    agemin=km[time>=1,min(time)]
    agemax=779
  } else {
    km=data[,rbind(
      c(1,1,1,0),
      as.data.frame(unclass(
        survfit(Surv(time,status)~1,data=data.frame(
          time=jouini.agem,
          status=1))
      )[c("upper","lower","surv","time")])
    ),by=by]
    agemin=km[time>=1,min(time)]
    agemax=km[time>=1,max(time)]
  }

  if (is.null(by)) {
    g=ggplot(data=km) +
      geom_rect(mapping=aes(ymin=lower,ymax=upper,xmin=time,xmax=time+1),alpha=0.10,linetype="blank",fill=color) +
      geom_step(mapping=aes(x=time,y=surv),direction="hv",color=color) +
      coord_cartesian(xlim = c(agemin-2, agemax+2))
  } else {
    g=ggplot(data=km) + aes_string(fill=by,color=by) +
      geom_rect(mapping=aes(ymin=lower,ymax=upper,xmin=time,xmax=time+1),alpha=0.10,linetype="blank") +
      geom_step(mapping=aes(x=time,y=surv),direction="hv") +
      coord_cartesian(xlim = c(agemin-2, agemax+2))
  }

  return(g)

}

autoplot.retraitesregstrat=function(object){
  strata=attr(object,"strata")
  data=copy(attr(object,"eval"))[get(strata)=="Ensemble"]
  trange=range(data$start)
  g=ggplot(data=data,aes(x=start))+
    geom_step(mapping=aes(y=s),direction="hv",color="black")+
    geom_step(mapping=aes(y=schap),direction="hv",color="red")+
    coord_cartesian(xlim = trange+c(-2, 2))
  return(g)
}
