#' Require Study
#' 
#' @description throws an error if the study has not been set
#'
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' require_study()
#' }
require_study<-function(){
  if(is.null(pkg.globals$study)){
    stop("Can't get constructs without first calling set_data(TODO_STUDY, TODO_REDCAPKEY)")
  }
}


#' Require Automatic Cache
#' 
#' @description updates the automatic cache
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' require_automatic_cache()
#' }
require_automatic_cache<-function(force=FALSE){
  require_study()
  if(length(pkg.globals$auto_cache)<3 | force){
    calculate <- TRUE
    if(is.null(pkg.globals$data)){
      calculate <- FALSE
    }
    if(length(pkg.globals$auto_cache)==2 & calculate==FALSE & force==FALSE){
      return()
    }
    functions <- ls("package:AnalyticCodebase")
    auto_functions <- functions[str_detect(functions,paste0("^automatic_",pkg.globals$study))]
    constructs_list <- list()
    function_names <- list()
    for(f in auto_functions){
      if(calculate){
        results <- do.call(f,list(mode="data"))
      }
      fields <- do.call(f,list(mode="fields"))
      columns <- do.call(f,list(mode="columns"))
      for(col in columns){
        constructs_list[col] <- list(fields)
        function_names[col] <- f
        if(calculate){
          pkg.globals$cache[[col]] <- results %>% select(study_id, all_of(col))
        }
      }
    }
    pkg.globals$auto_cache <- list(constructs_list=constructs_list, function_names=function_names)
    if(calculate){
      pkg.globals$auto_cache['calculate']<- TRUE
    }
  }
}


