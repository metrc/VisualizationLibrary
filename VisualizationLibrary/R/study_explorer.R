#' Determine if a study is using the legacy matrix
#'
#'
#' @description returns TRUE if the study is using the legacy matrix
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#' study_uses_legacy()
#' }
study_uses_legacy<- function(){
  return(is.null(pkg.globals$function_sheet))
}


#' Find all constructs
#' 
#' @description returns vector of all the construct functions in this package
#'
#' @param study if study is passed then functions will return names of constructs only for that study
#'
#' @return vector of characters that are the names of the constructs
#' @export
#'
#' @examples
#' \dontrun{
#' find_all_constructs()
#' }
find_all_constructs<- function(within_study=TRUE, auto_cache_ignore=FALSE){
  if(study_uses_legacy()){
    functions <- ls("package:AnalyticCodebase")
    if (within_study){
      require_study()
      require_automatic_cache()
      functions <- str_remove(functions[str_detect(functions,paste0("^",pkg.globals$study))], paste0(pkg.globals$study,"_"))
      if(!is.null(pkg.globals$matrix)){
        functions <- c(str_remove(pkg.globals$matrix$Construct[!is.na(pkg.globals$matrix$function_name)], paste0(pkg.globals$study,"_")), functions)
      }
      if(!is.null(pkg.globals$auto_cache) & !auto_cache_ignore){
        functions <- c(names(pkg.globals$auto_cache$constructs_list), functions)
      }
      functions <- unique(functions)
    }
    return(functions)
  } else{
    return(pkg.globals$function_sheet$`Construct Item`)
  }
}


#' Find all fields
#' 
#' @description returns vector of all the fields used by this study in this package
#'
#' @return vector of characters that are the names of the fields
#' @export
#'
#' @examples
#' \dontrun{
#' find_all_fields()
#' }
find_all_fields<- function(){
  require_study()
  fields <- c("study_id","facilitycode","redcap_event_name")
  names <- find_all_constructs()
  for(name in names){
    fields <- c(fields, get_construct_fields(name))
  }
  fields <- unique(fields)
  return(fields)
}


#' Find all compatible functions
#' 
#' @description returns vector of all the construct functions in this package that can be used with this study
#' 
#' @param return_fields defaults to FALSE and when TRUE returns the fields in the compatible functions rather than function names
#' @param quiet defaults to FALSE and when TRUE hides added data from being printed
#' @param recursive check to see if the function could work if we use another function
#' @param everything include legacy functions
#' @param verify if not set to recursive determine compatibility by running the function
#'
#' @return vector of characters that are the names of the compatible functions
#' @export
#'
#' @examples
#' \dontrun{
#' find_all_compatible_functions()
#' }
find_all_compatible_functions<- function(return_fields=FALSE, quiet=FALSE, recursive=TRUE, everything=FALSE, verify=FALSE){
  require_study()
  functions <- ls("package:AnalyticCodebase")
  functions <- functions[!str_detect(functions,"^automatic")]
  if(!everything){
    if(!study_uses_legacy()){
      functions <- functions[str_detect(functions, pkg.globals$function_prefixes_regex)]
    }
  }
  if(verify & recursive){
    recursive <- FALSE
    warning("Verify turns off recursive search - you may get fewer results but they will work right now")
  }
  compatible_functions <- vector()
  required_fields <- vector()
  missing_fields <- vector()
  missing_functions <- vector()
  theoretical_functions <- vector()
  not_processed <- vector()
  for(func in functions){
    if(verify){
      output <- try(get(func)())
      if (!inherits(output, "try-error")) {
        fields <- get_function_fields(func, recursive=recursive)
        required_fields <- c(required_fields, fields)
        compatible_functions <- c(compatible_functions, func)
      }
    } else{
      theoretical <- FALSE
      compatible <- TRUE
      fields <- get_function_fields(func, recursive=recursive, injected_warning="theoretical_functions_USED")
      if(length(fields[str_detect(fields,"theoretical_functions_USED")])>0){
        reqs <- str_remove_all(fields[str_detect(fields,"theoretical_functions_USED")],"theoretical_functions_USED")
        req_func <- paste0(func," (",paste(reqs,collapse=", "),")")
        fields <- fields[!str_detect(fields,"theoretical_functions_USED")]
        theoretical<-TRUE
      }
      if(length(fields[!fields %in% pkg.globals$columns])==0){
        if(!quiet){
          if(!is.null(fields)){
            missing <- fields[!fields %in% colnames(pkg.globals$data)]
            if(length(missing)!=0){
              print(paste0("Note - ",func," has fields in data not currently loaded: ",
                           paste(missing, collapse=", ")))
              missing_fields <- c(missing_fields, missing)
              missing_functions <- c(missing_functions, func)
            }
          } else{
            not_processed <- c(not_processed, func)
          }
        }
        required_fields <- c(required_fields, fields)
        compatible_functions <- c(compatible_functions, func) 
      } else{
        compatible <- FALSE
      }
      if(compatible & theoretical){
        theoretical_functions <- c(theoretical_functions, req_func)
      }
    }
  }
  if(!quiet){
    print(paste("Functions not processed because they did not run correctly:", paste(not_processed,collapse=", ")))
    print(paste("Missing fields that could be loaded with all raw data from this study:", paste(missing_fields,collapse=", ")))
    print(paste("Missing Functions that could be loaded with all raw data from this study:", paste(missing_functions,collapse=", ")))
    print(paste("Functions that could be work if you used another function that isn't currently an construct for this study:", paste(theoretical_functions,collapse=", ")))
  }
  if(return_fields){
    return(unique(required_fields))
  }
  return(unique(compatible_functions))
}



#' Check all constructs
#' 
#' @description returns vector of all inappropriately named construct functions in this package
#'
#' @param studies if list of studies is passed than check will be perfect otherwise its aproximate
#'
#' @return vector of functions that do not comply with construct function naming standards
#' @export
#'
#' @examples
#' \dontrun{
#' check_all_constructs()
#' }
check_all_constructs<- function(studies=NULL){
  if(study_uses_legacy()){
    fxs <- ls("package:AnalyticCodebase")
    bad_functions <- fxs[str_remove_all(fxs,"[a-z]|_")!=""]
    if(is.null(studies)){
      return(bad_functions)
    }
    good_functions <- vector()
    studies <- paste0(tolower(str_replace_all(studies," ","_")),"_")
    study_regex <- paste(paste0("^",studies),collapse="|")
    for (fx in fxs){
      print(fx)
      print(good_functions)
      if(str_detect(fx, study_regex)){
        if(str_remove_all(fx,"[a-z]|_")==""){
          good_functions <- c(good_functions, fx)
        }
      }
    }
    return(fxs[!fxs %in% good_functions])
  } else{
    return(NULL)
  }
}





#' Bootstrap New Study
#' 
#' @description use this to bootstrap a new study
#' 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' bootstrap_new_study()
#' }
bootstrap_new_study <- function(){
  if(!study_uses_legacy()){
    stop("This function is only for legacy studies")
  }
  require_study()
  fs <- find_all_compatible_functions()
  study_names <- pkg.globals$study_names
  removal <- paste(paste0(study_names,"_"),collapse="|")
  theoretical_df <- tibble("Construct"=str_remove(fs,removal), "2"=fs)
  colnames(theoretical_df) <- c("Construct", pkg.globals$study)
  mdf <- pkg.globals$full_matrix
  order <- colnames(mdf)
  other_studies <- mdf %>% select(colnames(mdf)[colnames(mdf)!=pkg.globals$study])
  actual_df <- mdf %>% select(all_of(c("Construct", pkg.globals$study))) %>% filter(!is.na(.[[2]]))
  df <- rbind(actual_df, theoretical_df) %>% 
    group_by(Construct) %>% 
    slice(1) %>% 
    ungroup()
  output <- full_join(other_studies, df) %>% 
    select(all_of(order))
  googlesheets4::sheet_write(output, ss=pkg.globals$matrix_id, sheet="METRC_CONSTRUCT_MATRIX")
  print("Updated Google Drive METRC_CONSTRUCT_MATRIX!")
  return(output)
}

