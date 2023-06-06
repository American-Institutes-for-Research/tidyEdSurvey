#' this just implements attach for an edsurvey.data.frame and a light.edusrvey.data.frame
#' @param what equivalent to `what` in base::attach, but can also be an edsurvey.data.frame
#' @param pos equivalent to `pos` in base::attach
#' @param name equivalent to `name` in base::attach
#' @param warn.conflicts equivalent to `warn.conflicts` in base::attach
#' @author Blue Webb
#' @export
setGeneric('attach',
           def = function(what, pos = 2L, name = deparse1(substitute(what), backtick = FALSE),
                          warn.conflicts = TRUE) {

             if(!inherits(what,"edsurvey.data.frame")){
               standardGeneric("attach")
             }else {
               level <- what$cacheDataLevelName
               if(what$survey == "TIMSS"){
                 message(paste0("Attaching ",level, " level variables to search path.\nTo attach variables at a different level, set `cacheDataLevelName`."))
               }
               vars = colnamesAttach(what,level)
               suppressWarnings(
                 z <- getData(what, varnames=vars,
                              dropUnusedLevels=TRUE, omittedLevels=TRUE,
                              addAttributes=TRUE, returnJKreplicates=TRUE
                 )
               )
               base::attach(z, pos, name, warn.conflicts)
             }
           })


#' @method with edsurvey.data.frame
#' @export
with.edsurvey.data.frame <- function(data,expr,...){
  vars <- all.vars(substitute(expr))
  data_envr <- new.env()
  for(v in vars){
    tmp <- paste0("data$",v)
    data_envr[[v]] <- eval(parse(text = tmp))
  }
  data_envr <- as.data.frame(as.list(data_envr))
  eval(substitute(expr), data_envr, enclos=parent.frame())

}


