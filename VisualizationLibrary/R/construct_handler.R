
#' Get Construct Output
#' 
#' @description returns output of the construct function matching the name input and set study
#'
#' @param name the name of a construct function such that paste(study, name, sep="_") is fx name
#'
#' @return standard construct output
#' @export
#'
#' @examples
#' \dontrun{
#' get_construct_output()
#' }
get_construct_output<- function(name){
  require_study()
  require_automatic_cache()
  if(name %in% names(pkg.globals$cache)){
    return(pkg.globals$cache[[name]])
  }
  print(paste("get_construct_output - calculating and then caching column for construct item:", name))
  if(study_uses_legacy()){ 
    pkg.globals$cache[[name]] <- eval(get_construct(name)())
  } else{
    temp <- eval(get_construct(name)())
    colnames(temp) <- c("study_id", name)
    pkg.globals$cache[[name]] <- temp
  }
  return(pkg.globals$cache[[name]])
}


#' Get Construct Outputs
#' 
#' @description returns joined output of the construct function matching the name input and set study
#'
#' @param names the vector of the names of the construct function such that paste(study, name, sep="_") is fx name
#'
#' @return standard construct output
#' @export
#'
#' @examples
#' \dontrun{
#' get_construct_outputs()
#' }
get_construct_outputs<- function(names){
  output <- NULL
  for(name in names){
    if(is.null(output)){
      output <- get_construct_output(name)
    } else{
      output <- left_join(output, get_construct_output(name))
    }
  }
  return(output)
}


#' Get Construct
#' 
#' @description returns the construct function matching the name input and set study
#'
#' @param name the name of a construct function such that paste(study, name, sep="_") is fx name
#'
#' @return a function
#' @export
#'
#' @examples
#' \dontrun{
#' get_construct()
#' }
get_construct<- function(name){
  require_study()
  return(get(get_construct_function_name(name)))
}


#' Get Construct Function Name
#'
#' @description returns the name of the construct function matching the name input and set study
#'
#' @param name the name of a construct function
#'
#' @return a character string
#' @export
#'
#' @examples
#' \dontrun{
#' get_construct_function_name()
#' }
get_construct_function_name<- function(name){
  require_study()
  if(!study_uses_legacy()){
    target <- pkg.globals$function_sheet[pkg.globals$function_sheet$`Construct Item` == name,]$`Function Name`
    if (length(target) == 0){
      stop(paste0("get_construct_function_name: no construct function found for ", name))
    }
    if (length(target) > 1){
      stop(paste0("get_construct_function_name: multiple construct functions found for ", name))
    }
    return(target)
  }
  if(name %in% pkg.globals$matrix$Construct){
    str <- pkg.globals$matrix[pkg.globals$matrix$Construct == name,]$function_name
    if(str_detect(str,"\\(")){
      str <- str_remove(str_extract(str,"^.+\\("),"\\(")
    }
  } else if(name %in% pkg.globals$auto_cache$constructs_list){
    return(get(pkg.globals$auto_cache$function_names[[name]]))
  } else{
    str <- paste(pkg.globals$study, name, sep="_")
  }
  return(str)
}


#' Get Construct Fields
#' 
#' @description returns the fields a construct function uses
#'
#' @param name the name of a construct function such that paste(study, name, sep="_") is fx name
#' @param recursive whether to recursively get the fields of the functions called by the construct
#'
#' @return fields character vector
#' @export
#'
#' @examples
#' \dontrun{
#' get_construct_fields()
#' }
get_construct_fields<- function(name, recursive=TRUE){
  require_automatic_cache()
  if(name %in% names(pkg.globals$auto_cache$constructs_list) && recursive){
    return(pkg.globals$auto_cache$constructs_list[[name]])
  }
  return(get_function_fields(get_construct(name), recursive=recursive))
}


#' Get Function Fields
#' 
#' @description returns the fields a function uses
#'
#' @param name the name of a function
#'
#' @return fields character vector
#' @export
#'
#' @examples
#' \dontrun{
#' get_function_fields()
#' }
get_function_fields<- function(name, recursive=TRUE, injected_warning=NULL){
  test <- name
  if(is.character(name)){
    test <- get(name)
  }
  str <- paste(unlist(strsplit(deparse(test),"\n")), collapse="")
  lines <- str_extract_all(str,"get_data\\(c\\([^\\)]+\\)\\)")
  if (!recursive){
    if (length(unlist(lines))==0){
      return(vector(mode="character"))
    }
    return(unique(unlist(strsplit(str_remove_all(lines,"get_data\\(c\\(|\\)|\"|'| |\\\\|c\\(|\\n"),","))))
  }
  recurs <- str_extract_all(str,"get_construct_output\\(\"[^\"]+\"\\)|get_construct_output\\(\'[^\\']+\'\\)")
  recurs_multi <- str_extract_all(str, "get_construct_outputs\\(c\\([^\\)]+\\)\\)")
  fields <- vector()
  if (length(unlist(lines))==0 & length(unlist(recurs))==0 & length(unlist(recurs_multi))==0){
    return()
  }
  if (length(unlist(lines))>0){
    fields <- c(fields, unlist(strsplit(str_remove_all(lines,"get_data\\(c\\(|\\)|\"|'| |\\\\|c\\(|\\n"),",")))
  }
  if (length(unlist(recurs))>0 | length(unlist(recurs_multi))>0){
    recur_targets <- unique(str_remove_all(unlist(recurs),"get_construct_output\\(|\\)|\"|'|"))
    if(length(unlist(recurs))==0){
      multi_recur_targets <- unlist(strsplit(str_remove_all(recurs_multi,"get_construct_outputs\\(c\\(|\\)|\"|'| |\\\\|c\\(|\\n"),","))
      recur_targets <- multi_recur_targets
    }else{
      if(length(unlist(recurs_multi))>0){
        multi_recur_targets <- unlist(strsplit(str_remove_all(recurs_multi,"get_construct_outputs\\(c\\(|\\)|\"|'| |\\\\|c\\(|\\n"),","))
        recur_targets <- c(recur_targets, multi_recur_targets)
      }
    }
    for(re in recur_targets){
      found <- try(get_construct_fields(re))
      if (!inherits(found, 'try-error')){
        fields <- c(fields, found)
      } else{
        if(recursive){
          sim_res <- try(fields_from_similar_function_different_study(re, preference=name, 
                                                                      recursive=recursive, injected_warning=injected_warning))
          if (!inherits(sim_res, 'try-error')){
            fields <- c(fields, sim_res)
          } else{
            # We failed to find a recursive combination that works so we add a field that will definitely trigger any systems to not import this
            fields <- c(fields, "This function wil not run because there are not combinations of constructs that exist currently that will work (and this field doesn't exist in redcap)")
          }
          if(!is.null(injected_warning)){
            fields <- c(fields, paste0(injected_warning,fields_from_similar_function_different_study(re, preference=name, return_similar_name = T, 
                                                                                                     recursive=recursive, injected_warning=injected_warning)))
          }
        } else{
          warning("IGNORE ERROR ABOVE - TRY FIELD SEARCH SUCCESSFULLY CAUGHT")
        }
      }
    }
  }
  return(unique(fields))
}


#' Fields From Similar Function Different Study
#' 
#' @description returns the fields a similar construct uses
#'
#' @param name the name of construct
#' @param preference a function name to use to extract preferred study
#'
#' @return fields character vector
#' @export
#'
#' @examples
#' \dontrun{
#' fields_from_similar_function_different_study()
#' }
fields_from_similar_function_different_study<- function(name, preference=NULL, return_similar_name=FALSE, recursive=FALSE, injected_warning=NULL){
  name <- str_remove(name,paste0(pkg.globals$study,"_"))
  require_study()
  study_names <- pkg.globals$study_names
  if(!is.null(preference)){
    # finding preference study
    pref_study <- study_names[str_detect(preference,study_names)]
    for (pref in pref_study){
      found <- try(get_function_fields(paste0(pref,"_",name), recursive=recursive, injected_warning=injected_warning))
      if (!inherits(found, 'try-error')){
        if(return_similar_name){
          return(paste0(pref,"_",name))
        }
        return(found)
      }
    }
    warning(paste("Could not find",paste0(pref,"_",name),"preferential function that worked, trying alternatives"))
  }
  # trying to use the matrix
  mdf <- pkg.globals$full_matrix
  trials_raw <- mdf %>% filter(Construct==name)  %>%  as.character()
  trails_raw <- trials_raw[which(trials_raw==name)[1]+1:length(trials_raw)]
  trials <- trials_raw[!is.na(trials_raw)&trials_raw!="NA"]
  for (trial in trials){
    found <- try(get_function_fields(trial, recursive=recursive, injected_warning=injected_warning))
    if (!inherits(found, 'try-error')){
      if(return_similar_name){
        return(trial)
      }
      return(found)
    }
  }
  for (study in study_names){
    found <- try(get_function_fields(paste0(study,"_",name), recursive=recursive, injected_warning=injected_warning))
    if (!inherits(found, 'try-error')){
      if(return_similar_name){
        return(paste0(study,"_",name))
      }
      return(found)
    }
  }
  return(paste("FAILED ATTEMPT TO FIND COMPATIBLE CONSTRUCT FOR",name))
}




#' Get Dependencies
#' 
#' @description finds what constructs a construct function relies on
#'
#' @return vector of dependencies
#' @export
#'
#' @examples
#' \dontrun{
#' get_dependencies()
#' }
get_dependencies <- function(name){
  require_automatic_cache()
  if(name %in% names(pkg.globals$auto_cache$constructs_list)){
    return(vector())
  }
  return(get_function_dependencies(get_construct_function_name(name)))
}



#' Get Function Dependencies
#'
#' @description finds what constructs a construct function relies on
#'
#' @return vector of dependencies
#' @export
#'
#' @examples
#' \dontrun{
#' get_function_dependencies()
#' }
get_function_dependencies <- function(name){
  return(unique(c(str_match_all(paste(deparse(get(name)),collapse = " "), "get_construct_output\\([\"|']([^\"|']+)[\"|']\\)")[[1]][,2],
                  unlist(strsplit(str_remove_all(unlist(str_match_all(paste(deparse(get(name)),collapse = " "),
                                                                      "get_construct_outputs\\(c\\(([^\\)]+)\\)")),"get_construct_outputs\\(c\\(|get_construct_outputs\\(|c\\(| |\"|'|\\)"),",")))))
}
