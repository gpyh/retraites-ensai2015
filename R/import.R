# Les fonctions pour importer les données

#' Importe la table simul_gen46 fournie par le tuteur.
#' La variable \code{pens_num} est automatiquement renommée en \code{id} et renvoyée en tant que chaîne de caractères.
#' Les variables à retenir et leur type sont renseignés dans le corps de la fonction.
#' On sépare des "variables de mesure" le véritable nom de la variable et le numéro de la période.
#' La table finale présente alors une observation pour un couple unique (\code{id},\code{periode360})
#'
#' @param path Le chemin vers la table.
#' @param format Le format de la table ; seul "csv" est implémenté.
#' @return Une data.table contenant les données simulées correctement transposées des retraités de la FPE. Les variables sont de types primitifs et aucune vérification n'est effectuée.
#' @examples
#' s46 <- import_s46("O:\\Annee2/stats/Groupe06/source/simul_gen46_new.csv")
import_s46=function(path,format="csv") {

  # Paramétrage
  supported_formats=c("csv")
  if(!(format %in% supported_formats))
    stop("Le format spécifié n'est pas supporté")

  id.vars=list(
    "pens_num"=readr::col_character(),
    "pens_regime"=readr::col_integer(),
    "cas_simul"=readr::col_integer(),
    "dod"=readr::col_integer(),
    "limage"=readr::col_integer())

  measure.vars=list(
    "indm"=readr::col_integer(),
    "salaire"=readr::col_integer(),
    "dureeassur"=readr::col_integer(),
    "montantdef"=readr::col_numeric(),
    #    "gainsur"=readr::col_numeric(),
    #     "gainmg"=readr::col_numeric(),
    #     "pounor"=readr::col_numeric(),
    #     "montantini"=readr::col_numeric(),
    #     "pertedec"=readr::col_numeric(),
    "OV"=readr::col_numeric())
  
  key="pens_num"

  # Execution
  if(format=="csv"){

    vars=names(readr::read_csv(path,n_max=1))
    
    key=grep(key,vars,fixed=TRUE,value=TRUE)[1]

    is.id.vars=
      vars %>%
      substr(.,nchar(.)-5,nchar(.)) %>% {
        substr(.,1,1)!='_' |
          suppressWarnings(is.na(as.numeric(substr(.,2,6))))
      }

    prefix=
      ifelse(!is.id.vars,vars,NA) %>%
      substr(.,1,nchar(.)-6)

    bined.vars=
      unique(na.omit(prefix)) %>%
      sapply(function(x)
        vars[prefix==x&!is.na(prefix)])

    bined.vars=bined.vars[names(measure.vars)]

    vars[!(vars %in% names(id.vars)) & is.id.vars] %>% {
      if(length(.)>0) {
        warning(paste(.,"n'est pas une variable d'identification attendue ; elle est ignorée.",collapse='\n'))
      }
    }

    unique(na.omit(prefix)) %>% subset(!(. %in% names(measure.vars))) %>% {
      if(length(.)>0) {
        warning(paste(.,"n'est pas une variable de mesure attendue ; elle est ignorée.",collapse='\n'))
      }
    }

    true.measure.vars=do.call(
      c,
      lapply(
        names(measure.vars),
        function(var)
          sapply(
            bined.vars[[var]],
            function(x)
              list(measure.vars[[var]])
          )
      )
    )

    collectors=sapply(vars,function(x) readr::col_skip())
    collectors[names(id.vars)]=id.vars
    collectors[names(true.measure.vars)]=true.measure.vars

    data=as.data.table(readr::read_csv(path,col_types=collectors))

    setnames(data,key,"id")
    names(id.vars)[names(id.vars)==key]="id"
    setkey(data,id)

    if(length(unique(data$id))!=dim(data)[1])
      stop(paste(key," n'est pas clé primaire !"))

    transposition=function(var){
      temp=melt(
        data[,c("id",bined.vars[[var]]),with=FALSE],
        id.vars="id",
        measure.vars=bined.vars[[var]],
        variable.factor=FALSE
      )

      temp %<>%
        subset(!is.na(value)) %>%
        `[`(,list(id,periode360=as.numeric(substr(variable,nchar(variable)-4,nchar(variable))),value)) %T>%
        setkeyv(c("id","periode360")) %T>%
        setnames("value",var)

      return(temp)
    }


    init.merge=merge(data[,names(id.vars),with=FALSE],transposition(names(measure.vars)[1]),by="id")
    setkeyv(init.merge,c("id","periode360"))

    return(
      Reduce(
        function(a,b)
          merge(a,transposition(b),all=TRUE),
        names(measure.vars)[-1],
        init.merge)
    )

  }
}

#' Importe la table base_gen465052 fournie par le tuteur.
#' La variable \code{pens_num} est automatiquement renommée en \code{id} et renvoyée en tant que chaîne de caractères. Hormis \code{pens_num}.
#' Aucune variable n'est particulièrement attendue et les types sont déduits automatiquement par \code{read.csv}.
#' Cela signifie que, hormis \code{id}, les chaînes de caractères sont converties en facteurs.
#'
#' @param path Le chemin vers la table.
#' @param format Le format de la table ; seul "csv" est implémenté.
#' @return Une data.table contenant les données des retraités de la FPE. Les variables sont de types primitifs et aucune vérification n'est effectuée.
#' @examples
#' b <- import_b("O:\\Annee2/stats/Groupe06/source/base_gen465052.csv")
import_b=function(path,format="csv") {

  data=as.data.table(read.csv(path,encoding = "UTF-8"))
  
  setnames(
    data,
    grep("pens_num",names(data),fixed=TRUE,value=TRUE)[1],
    "id"
  )
  data[,id:=as.character(id)]
  setkey(data,id)
  setcolorder(data,c("id",names(data) %>% subset(.!="id")))
  
  return(data)

}

