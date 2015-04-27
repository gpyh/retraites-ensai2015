retraitesreg=function(formula,data,type,strata="strate",repboot=1000,debug=FALSE,cuts=1:59){
  
  # Fonctions de fit
  if (type=="probit") {
    fitfn=function(data) {
      geepack::geeglm(
        formula=formula,
        id=id,
        data=data,
        family=binomial(link="probit")
      )
    }
  } else if (type=="weibull") {
    fitfn=function(data) {
      eha::phreg(
        formula=formula,
        data=data,
        dist="weibull"
      )
    }
  } else if (type=="pch") {
    fitfn=function(data){
      eha::phreg(
        formula=formula,
        data=data,
        dist="pch",
        cuts=cuts
      )
    }
  } else {
    stop("type de modèle non reconnu")
  }
  
  # Traduction des différentes fonctions caractéristiques des lois de modèle de durée
  h_to_cdf=function(h,cdf){
    h+(1-h)*cdf
  }
  
  cdf_to_p=function(cdf){
    cdf-c(0,head(cdf,-1))
  }
  
  data[,avantjouini:=c(head(cumprod(1-ijouini)+ijouini,-1),0),by=id]
  setkey(data,id,periode)

  # Fitting et prévision de h
  appr=list()
  eval=list()
  for (strate in levels(data[,get(strata)])) {
    
    indappr=data[as.logical(appr),get(strata)]==strate
    appr=data[as.logical(appr)][indappr]
    indeval=data[!as.logical(appr),get(strata)]==strate
    eval[[strate]]=data[!as.logical(appr)][indeval]

    model=fitfn(appr[as.logical(avantjouini)])
    if(isTRUE(debug)) message(paste0("fit de appr dans la strate ",strate," ok."))
    
    indnolimage=eval[[strate]][,list(i=.I[-.N]),by=id]$i
    if (type=="probit") {
      eval[[strate]][
        indnolimage,
        h:=suppressWarnings(predict(model,newdata=.SD,type="response"))]
    } else if (type=="pch") {
      eval[[strate]][
        indnolimage,
        h:=predictpch(model,newdata=.SD,cuts=cuts,hazards=model$hazards)]
    } else {
      eval[[strate]][
        indnolimage,
        h:=predictsurv(model,newdata=.SD,type=type)]
    }
    eval[[strate]][,s:=1-c(Reduce(h_to_cdf,h[-.N],accumulate=TRUE),1),by=id]
    eval[[strate]][,p:=cdf_to_p(1-s),by=id]
    eval[[strate]]=eval[[strate]][,list(strata=get(strata),id,periode,start,ijouini,plan,plan.valreel,s,p)]
    names(eval[[strate]])[1]=strata
    rm(indnolimage)
    rm(model)
    rm(appr)
    gc(FALSE)
  }
  
  eval=rbindlist(eval)
  
  # Stats
  plan=eval[as.logical(ijouini),list(plan=sum(plan)/length(plan),plan.valreel=sum(plan.valreel)/length(plan.valreel)),by=strata]
  names(plan)[names(plan)==strata]="strata"
  plan=rbind(plan,eval[as.logical(ijouini),list(strata="Ensemble",plan=sum(plan)/length(plan),plan.valreel=sum(plan.valreel)/length(plan.valreel))])
  
  idstats=eval[,list(tespplan=sum(p*plan.valreel),tvarplan=sum(p*(1.0-p)*plan.valreel^2),strata=get(strata)[1]),keyby=id]
  est=idstats[,list(espplan=sum(tespplan/length(tespplan)),stdplan=sqrt(sum(tvarplan/length(tvarplan)^2))),by=strata]
  est=rbind(est,idstats[,list(strata="Ensemble",espplan=sum(tespplan/length(tespplan)),stdplan=sqrt(sum(tvarplan/length(tvarplan)^2)))])
  
  # Bootstrap
  
  neval=length(idstats$id)
  boot=idstats[sample(id,neval*repboot,replace=TRUE),list(id,strata,tespplan,tvarplan,samp=rep(1:repboot,each=neval))]
  if(isTRUE(debug)) message(paste0("bootstrap ok"))
  
  rmse=boot[,list(espplan.boot=sum(tespplan/length(tespplan)),stdplan.boot=sqrt(sum(tvarplan/length(tvarplan)^2))),by=c("strata","samp")]
  rmse=rbind(rmse,boot[,list(strata="Ensemble",espplan.boot=sum(tespplan/length(tespplan)),stdplan.boot=sqrt(sum(tvarplan/length(tvarplan)^2))),by="samp"])
  rmse=merge(est,rmse,by="strata")
  rmse=rmse[,
      list(
        espplan=sqrt(sum((espplan.boot-espplan)^2)/repboot),
        stdplan=sqrt(sum((stdplan.boot-stdplan)^2)/repboot)
      )
    ,by="strata"]
  
  eval=eval[,list(id,get(strata),start,ijouini,s,p)]
  names(eval)[2]=strata
  
  eval[,start:=start+720]
  estim=copy(eval)
  estim[,schap:=1-cummax(ijouini),by=id]
  estim[,nids:=length(unique(id)),by=strata]
  n=length(unique(estim$id))
  tmin=min(estim$start)

  init=data.frame(strata=rep(c(levels(estim[,get(strata)]),"Ensemble"),3),start=tmin-(1:3),schap=rep(1,3),s=rep(1,3))
  names(init)[names(init)=="strata"]=strata
  
  predstrates=estim[,list(schap=sum(schap)/nids[1],s=sum(s)/nids[1]),by=c("start",strata)]
  pred=estim[,list(strata="Ensemble",schap=sum(schap)/n,s=sum(s)/n),by="start"]
  
  names(pred)[names(pred)=="strata"]=strata
  predstrates=rbind(init,pred,predstrates)
  setkeyv(predstrates,c(strata,"start"))
  
  err.survie.DT=predstrates[,list(err=sum(abs(s-schap))),by=strata]
  err.survie=err.survie.DT$err
  names(err.survie)=err.survie.DT[,get(strata)]
  
  if(isTRUE(debug)) message(paste0("stats ok"))
  
  # Fitting
  
  models=list()
  for (strate in c(levels(data[,get(strata)]),"Ensemble") ) {
    
    if (strate=="Ensemble") {
      model$type="empty"
    } else {
      ind=data[,get(strata)]==strate
      model=fitfn(data[ind&as.logical(avantjouini)])
      if(isTRUE(debug)) message(paste0("fit de la strate ",strate," ok."))
      model$type=type
      if (type=="probit") {
        quasiLik=MuMIn::quasiLik(model)
        QIC=MuMIn::QIC(model)
        summary=summary(model)
        rm(model)
        gc(FALSE)
        model=list()
        model$type=type
        model$quasiLik=quasiLik
        model$QIC=QIC
        model$dispersion=summary$dispersion$Estimate
        model$coef.names=rownames(summary$coefficients)
        model$coef=summary$geese$mean$estimate
        model$se=summary$geese$mean$san.se
        model$pvalues=summary$geese$mean$p
        rm(summary)
        gc(FALSE)
      }
    }
    
    model$err.survie=err.survie[strate]
    model$espplan.est=est[strata==strate,espplan]
    model$stdplan.est=est[strata==strate,stdplan]
    model$plan.valreel=plan[strata==strate,plan.valreel]
    model$rmse.espplan=rmse[strata==strate,espplan]
    model$rmse.stdplan=rmse[strata==strate,stdplan]
    
    class(model)=c("retraitesreg",class(model))
    
    models[[strate]]=model
    gc(FALSE)
  }
  
  class(models)="retraitesregstrat"
  
  attr(models,"strata")=strata
  attr(models,"eval")=predstrates
  
  return(models)
}

# Fonction de prédiction
predictsurv=function(model,newdata,type){
  scale=exp(model$coefficients["log(scale)"])
  shape=exp(model$coefficients["log(shape)"])
  if (type=="weibull") {
    baseh=function(t) {
      eha::hweibull(t,shape=shape,scale=scale)
    }
  } else if (type=="ev") {
    baseh=function(t) {
      eha::hEV(t,shape=shape,scale=scale)
    }
  } else {
    stop("type de modèle inconnu")
  }
  baseline=baseh(newdata[,end])
  prod=exp(model.matrix(object=model$terms,data=newdata)[,-1]%*%head(model$coefficients,-2))
  return(baseline*prod)
}

predictpch=function(model,newdata,hazards,cuts){
  baseline=eha::hpch(newdata[,end],cuts,hazards)
  prod=exp(model.matrix(object=model$terms,data=newdata)[,-1]%*%model$coefficients)
  return(baseline*prod)

}