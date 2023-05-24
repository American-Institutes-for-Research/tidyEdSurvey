#' @importFrom ggplot2 fortify
#' @export
fortify.edsurvey.data.frame <- function(model,data,...){
  vars <- model$dataList$Student$lafObject@column_names
  vars <- vars[vars!="version"]
  # if the edsurvey.data.frame has been attached, we'll just bind
  # everything together
  try(z <- data.frame(sapply(vars,FUN=function(v){as.name(eval(v))},
                             simplify = TRUE,USE.NAMES = TRUE)),
      silent=TRUE)

  if(exists("z")){
    ggplot2::fortify(z,data,...)
  }else{ # otherwise, we need to call getdata
    suppressWarnings(
      z <- EdSurvey::getData(model, varnames=vars,
                   dropUnusedLevels=FALSE, omittedLevels=FALSE,
                   addAttributes=TRUE, returnJKreplicates=FALSE)
    )
    ggplot2::fortify(z,data,...)
  }
}
