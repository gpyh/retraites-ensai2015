setClass("retraitesreg",slots = list(type = "character", rmse.espplan = "numeric", rmse.stdplan = "numeric"))

extract.retraitesreg=function(model, include.lr = FALSE,...) {
  
  if (model$type=="probit") {
    
    coef.names=sub("(Intercept)","(Constante)",model$coef.names,fixed=TRUE)
    coef=model$coef
    se=model$se
    pvalues=model$pvalues
    gof.names=c(
      "Dispersion",
      "Quasi-log-vraisemblance",
      "QIC",
      "Somme des résidus de S",
      "Plan estimé par la moyenne $\\hat{\\mu}_{plan}$",
      "Ecart-type estimé $\\hat{\\sigma}_{plan}$",
      "Plan observé $-$ Plan estimé",
      "RMSE$(\\hat{\\mu}_{plan})$",
      "RMSE$(\\hat{\\sigma}_{plan})$")
    gof=c(
      model$dispersion,
      model$quasiLik,
      model$QIC,
      model$err.survie,
      model$espplan.est,
      model$stdplan.est,
      model$plan.valreel-model$espplan.est,
      model$rmse.espplan,
      model$rmse.stdplan)
    gof.decimal=c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE)
    
    tr=texreg::createTexreg(
      coef.names=coef.names,
      coef=coef,
      se=se,
      pvalues=pvalues,
      gof.names=gof.names,
      gof=gof,
      gof.decimal=gof.decimal
    )
    
  } else if (model$type %in% c("weibull","pch")) {
    
    coefs <- model$coefficients
    if (model$type=="weibull") {
      coef.names <- c(head(names(coefs),-2),"$log(\\lambda)$","$log(p)$")
    } else {
      coef.names=names(coefs)
    }
    se <- sqrt(diag(model$var))
    pval <- 1 - pchisq((coefs/se)^2, 1)
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()


      lik <- model$loglik[2]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "Log-vraisemblance")
      gof.decimal <- c(gof.decimal, TRUE)

    if (include.lr == TRUE) {
      lr <- -2 * (model$loglik[1] - model$loglik[2])
      gof <- c(gof, lr)
      gof.names <- c(gof.names, "Test du Log-rank")
      gof.decimal <- c(gof.decimal, TRUE)
    }

    gof=c(
      gof,
      model$err.survie,
      model$espplan.est,
      model$stdplan.est,
      model$plan.valreel-model$espplan.est,
      model$rmse.espplan,
      model$rmse.stdplan)
    gof.names=c(
      gof.names,
      "Somme des résidus de S",
      "Plan estimé par la moyenne $\\hat{\\mu}_{plan}$",
      "Ecart-type estimé $\\hat{\\sigma}_{plan}$",
      "Plan observé $-$ Plan estimé",
      "RMSE$(\\hat{\\mu}_{plan})$",
      "RMSE$(\\hat{\\sigma}_{plan})$")
    gof.decimal=c(gof.decimal,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
    tr <- createTexreg(coef.names = coef.names, coef = coefs, 
                       se = se, pvalues = pval, gof.names = gof.names, gof = gof, 
                       gof.decimal = gof.decimal)
    
  } else if (model$type=="empty") {
    
    coef.names="DO.NOT.WRITE"
    coef=0
    se=0
    pvalues=0
    gof.names=c(
      "Somme des résidus de S",
      "Plan estimé par la moyenne $\\hat{\\mu}_{plan}$",
      "Ecart-type estimé $\\hat{\\sigma}_{plan}$",
      "Plan observé $-$ Plan estimé",
      "RMSE$(\\hat{\\mu}_{plan})$",
      "RMSE$(\\hat{\\sigma}_{plan})$")
    gof=c(
      model$err.survie,
      model$espplan.est,
      model$stdplan.est,
      model$plan.valreel-model$espplan.est,
      model$rmse.espplan,
      model$rmse.stdplan)
    gof.decimal=c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE)
    
    tr=texreg::createTexreg(
      coef.names=coef.names,
      coef=coef,
      se=se,
      pvalues=pvalues,
      gof.names=gof.names,
      gof=gof,
      gof.decimal=gof.decimal
    )
    
  } else {
    stop("type de modèle inconnu")
  }
  
  return(tr)
  
}

setMethod("extract",signature=className("retraitesreg","retraites"),definition=extract.retraitesreg)