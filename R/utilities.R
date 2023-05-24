#' @import EdSurvey

buildDF <- function(x,cols=NULL){
  if(!is.null(cols)){
    vars <- cols
  }else{
    vars <- colnamesAttach(x)
    vars <- vars[vars!="version"]
  }

  vars2 <- vector(mode="character")
  for(v in vars){
    if(EdSurvey::hasPlausibleValue(v,x)){
      vars2 <- c(vars2,EdSurvey::getPlausibleValue(v,x))
    }else{
      vars2 <- c(vars2,v)
    }
  }
  # if the edsurvey.data.frame has been attached, we'll just bind
  # everything together
  try(z <- data.frame(sapply(vars2,FUN=function(v){as.name(eval(v))},
                             simplify = TRUE,USE.NAMES = TRUE)),
      silent=TRUE)

  if(exists("z")){
    return(z)
  }else{ # otherwise, we need to call getdata
    suppressWarnings(
      z <- EdSurvey::getData(x, varnames=vars2,
                   dropUnusedLevels=TRUE, omittedLevels=FALSE,
                   addAttributes=TRUE, returnJKreplicates=TRUE
      )
    )
    return(z)
  }
}

#temporary fix for HSTS attach
colnamesAttach <- function(esdf){
  svy <- getAttributes(esdf, "survey")
  cols <- colnames(esdf)
  ignoreVars <- c()

  if(svy == "HSTS"){
    #don't attach these
    ignoreLevels <- c("School_Catalog", "Test", "Transcript", "Transcript_Catalog")

    for(i in seq_along(ignoreLevels)){
      iL <- esdf$dataList[[ignoreLevels[i]]]
      tVars <- iL$fileFormat$variableName
      iVars <- iL$ignoreVars

      if(is.null(iVars)){
        iVars <- c()
      }

      ignoreVars <- c(ignoreVars, tVars[!tVars %in% iVars])
    }
    return(cols[!cols %in% ignoreVars])
  }

  return(cols)
}
