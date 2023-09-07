
#' Write Study
#' 
#' @description returns vector of all inappropriately named construct functions in this package
#'
#' @param study the study you want to run
#' @param key the Redcap api key to that study
#' @param column_names the column names you want in the dataset if you pass null all will be used
#' @param data_path path to write analytic set to
#' @param error_path path to write errors to
#' @param date set date of dataset requested
#' @param folder set the folder the raw data is located in defaults to STUDYNAME-cached_data
#' @param random scrambles the data frame - this is not going to return accurate results but can be used to check code
#' @param server sets cached data selector to server mode
#' @param dictionary includes a generated data dictionary
#' @param no_treat excludes treat field
#' @param anonymize creates random study_ids and date shifts all data
#' @param anon_allow allow anonymization despite issues
#' @param long_file_additions this is a string with comma separations that is used to add columns to long files of constructs
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' write_study()
#' }
write_study <- function(study, key, column_names, data_path, error_path, date=NULL, folder=NULL, server=FALSE, random=TRUE, dictionary=TRUE, no_treat=TRUE, anonymize=TRUE, anon_allow=FALSE, filter_column=NULL, long_file_additions=NULL){
  column_names <- unique(column_names)
  auto_column_mode <- "defined"
  treat_assign_fields <- c("treat_assign")
  treat_assign_columns <- c("treat", "treatment_arm")
  col_names <- vector()
  
  output <- try(set_data(study, key, date=date, folder=folder, server=server, columns=col_names, auto_column_mode = auto_column_mode))
  if (inherits(output, 'try-error')){
    warning("FAILING TO DETECT COLUMNS - FALLING BACK TO INCLDUING ALL COLUMNS")
    print("FAILING TO DETECT COLUMNS - FALLING BACK TO INCLDUING ALL COLUMNS")
    set_data(study, key, date=date, folder=folder, server=server)
  }
  if(!is.null(filter_column)){
    filter_redcap_data(filter_column)
  }
  if(anonymize){
    anonymize_redcap_data()
  }
  if(is.null(column_names)){
    column_names <- order_functions(find_all_constructs())
    display_order <- column_names
  } else{
    display_order <- column_names
    column_names <- order_functions(column_names)
  }
  analytic_data <- build_analytic_dataset(names=display_order, error_path = error_path)
  if(dictionary){
    if(no_treat){
      write_csv(generate_data_dictionary(analytic_data %>% select(-any_of(treat_assign_columns))),str_replace(data_path,"\\.csv","_dictionary.csv"))
    } else{
      write_csv(generate_data_dictionary(analytic_data),str_replace(data_path,"\\.csv","_dictionary.csv"))
    }
  }
  write_csv(analytic_data, data_path)
  data_path_test <- paste(getwd(),folder,"results.csv",sep="/")
  column_names <- column_names[column_names %in% colnames(analytic_data)]
  display_order <- display_order[display_order %in% colnames(analytic_data)]
  function_to_script(column_names, width=160, return_lines=FALSE, folder=folder, display_order=display_order)
  script_path <- paste(getwd(),folder,paste0(tolower(pkg.globals$study),'_analytic_codebase.R'),sep="/")
  if(anonymize){
    anon_status <- is_anonymizable(script_path)
    if(anon_status!=""){
      if(anon_allow){
        warning(anon_status)
      } else{
        warning(anon_status)
        if(file.exists(error_path)){
          er <- read_csv(error_path)
          er <- bind_rows(er, tibble(name="ERROR_NOT_ANONIMIZABLE",errors=anon_status))
          write_csv(er, error_path)
        } else{
          write_csv(tibble(name="ERROR_NOT_ANONIMIZABLE", documentation=NA, code=NA,  output=NA, errors=anon_status), error_path)
        }
      }
    }
  }
  if(server){
    pkg.globals$data <- NULL
  }
  system(paste("cd", paste0("'",folder,"'"),"&& Rscript --vanilla",paste0("'",script_path,"'")))
  if(no_treat){
    write_csv(analytic_data %>% select(-any_of(treat_assign_columns)), data_path)
  }
  if(no_treat){
    if(all_equal(read_csv(data_path, col_types = cols(.default = "c")) %>% select(-any_of(treat_assign_columns)),
                 read_csv(data_path_test, col_types = cols(.default = "c")) %>% select(-any_of(treat_assign_columns)))!=TRUE){
      if(file.exists(error_path)){
        er <- read_csv(error_path)
        er <- bind_rows(er, tibble(name="ERROR_NOT_REPRODUCED",errors="R script created to create analytic dataset is not recreating the analytic dataset created by build_analytic_dataset"))
        write_csv(er, error_path)
      } else{
        write_csv(tibble(name="ERROR_NOT_REPRODUCED", documentation=NA, code=NA,  output=NA, errors="R script created to create analytic dataset is not recreating the analytic dataset created by build_analytic_dataset"), error_path)
      }
      stop("R script created to create analytic dataset is not recreating the analytic dataset created by build_analytic_dataset")
    }
  } else{
    if(all_equal(read_csv(data_path, col_types = cols(.default = "c")),read_csv(data_path_test, col_types = cols(.default = "c")))!=TRUE){
      if(file.exists(error_path)){
        er <- read_csv(error_path)
        er <- bind_rows(er, tibble(name="ERROR_NOT_REPRODUCED",errors="R script created to create analytic dataset is not recreating the analytic dataset created by build_analytic_dataset"))
        write_csv(er, error_path)
      } else{
        write_csv(tibble(name="ERROR_NOT_REPRODUCED", documentation=NA, code=NA,  output=NA, errors="R script created to create analytic dataset is not recreating the analytic dataset created by build_analytic_dataset"), error_path)
      }
      stop("R script created to create analytic dataset is not recreating the analytic dataset created by build_analytic_dataset")
    } 
  }
  if(random){
    write_csv(randomize_tibble(read_csv(data_path_test)),data_path_test)
    write_csv(randomize_tibble(read_csv(data_path)),data_path)
  }
  if(no_treat){
    datacut_path <- paste0(dirname(data_path),"/",str_replace_all(str_to_lower(study)," ","_"),"_raw_dataset.csv")
    write_csv(read_csv(datacut_path, col_types = cols(.default = "c")) %>% select(-all_of(treat_assign_fields)),datacut_path)
  }else{
    if(!is.null(filter_column)|anonymize){
      datacut_path <- paste0(dirname(data_path),"/",str_replace_all(str_to_lower(study)," ","_"),"_raw_dataset.csv")
      write_csv(read_csv(datacut_path, col_types = cols(.default = "c")), datacut_path)
    }
  }
  write_all_zipped_constructs(dirname(data_path), name=long_file_additions, constructs=column_names)
  write_data_to_excel(data_path)
}


#' Legacy Generate Data Dictionary
#' 
#' @description generates simple
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' legacy_generate_data_dictionary()
#' }
legacy_generate_data_dictionary<-function(df){
  require_study()
  require_automatic_cache()
  cols <- colnames(df)[colnames(df) %in% find_all_constructs()]
  descs <- vector()
  values <- vector()
  mins <- vector()
  maxes <- vector()
  automatic_str_cache <- list() 
  for(col in cols){
    str <- get_construct_function_name(col)
    if(col %in% names(pkg.globals$auto_cache$function_names)){
      str <- pkg.globals$auto_cache$function_names[[col]]
    }
    if(!str_detect(str,"^automatic")){
      comment_lines <- gsub("_\b|^     ","",capture.output(tools:::Rd2txt(utils:::.getHelpFile(as.character(help(str, package = "AnalyticCodebase"))))))
      comment_lines <- comment_lines[comment_lines!=""]
      comment_str <- paste(comment_lines, collapse="\n")
      
      descs <- c(descs, str_remove(str_sub(comment_str, end=str_locate(comment_str, "Usage:")[1]-1, start=str_locate(comment_str, "Description:")[2]+1),"^\n"))
    } else{
      if(is.null(unname(unlist(automatic_str_cache[str])))){
        automatic_str_cache[str] <- list(get(str)(mode="docs"))
      }
      descs <- c(descs, unname(unlist(automatic_str_cache[[str]][col]))) 
    }
    unis <- unique(df[[col]])
    if(is.factor(df[[col]])) {
      ranges <- as.character(levels(df[[col]]))
      val <- paste("All levels/factors:",paste(sort(unis),collapse=", "))
    } else {
      ranges <- as.character(range(unis,na.rm=TRUE))
      if(length(unis)>10){
        val <- paste("Random sampling:",paste(sample(unis,size=3,replace=FALSE),collapse=", "))
      } else {
        val <- paste("All levels/factors:",paste(sort(unis),collapse=", "))
      }
    }
    mins <- c(mins, ifelse(str_length(ranges[1])>200, paste(str_sub(ranges[1],end=200),"..."), ranges[1]))
    maxes <- c(maxes, ifelse(str_length(ranges[2])>200, paste(str_sub(ranges[2],end=200),"..."), ranges[2]))
    values <- c(values, val)
  }
  return(tibble("Analytic Set Column"=cols, "Description"=descs, "Values"=values, "Minimum Value"=mins, "Maximum Value"=maxes))
}


#' Generate Data Dictionary
#' 
#' @description generates simple
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' generate_data_dictionary()
#' }
generate_data_dictionary <- function(df) {
  if(study_uses_legacy()){
    return(legacy_generate_data_dictionary(df))
  }
  freq_string <- function(data) {
    # Create a frequency table
    freq_table <- table(data)
    freq_table <- sort(freq_table, decreasing = TRUE)
    
    # Create a string for each item: "item: count"
    freq_strings <- paste(names(freq_table), ": ", freq_table, sep = "")
    
    # Combine all strings into one, separated by commas
    result <- paste(freq_strings, collapse = ", ")
    
    return(result)
  }
  require_study()
  require_automatic_cache()
  cols <- colnames(df)[colnames(df) %in% find_all_constructs()]
  domains <- vector()
  constructs <- vector()
  analysis <- vector()
  fields <- vector()
  cols_used <- vector()
  descs <- vector()
  values <- vector()
  data_info_mode <- vector()
  mins <- vector()
  maxes <- vector()
  means <- vector()
  medians <- vector()
  num_na <- vector()
  automatic_str_cache <- list() 
  for(col in cols){
    str <- get_construct_function_name(col)
    row_info <- pkg.globals$function_sheet %>% filter(`Function Name`==str)
    fields <- c(fields, row_info$`Redcap Field Dependencies`)
    cols_used <- c(cols_used, row_info$`Construct Item Dependecies`)
    domains <- c(domains, row_info$Domain)
    constructs <- c(constructs, row_info$Construct)
    analysis <- c(analysis, paste(row_info$`Analysis Approach Name`, row_info$`Analysis Approach Description`, sep = " -- "))
    if(col %in% names(pkg.globals$auto_cache$function_names)){
      str <- pkg.globals$auto_cache$function_names[[col]]
    }
    if(!str_detect(str,"^automatic")){
      comment_lines <- gsub("_\b|^     ","",capture.output(tools:::Rd2txt(utils:::.getHelpFile(as.character(help(str, package = "AnalyticCodebase"))))))
      comment_lines <- comment_lines[comment_lines!=""]
      comment_str <- paste(comment_lines, collapse="\n")
      descs <- c(descs, str_remove(str_sub(comment_str, end=str_locate(comment_str, "Usage:")[1]-1, start=str_locate(comment_str, "Description:")[2]+1),"^\n"))
    } else {
      if(is.null(unname(unlist(automatic_str_cache[str])))){
        automatic_str_cache[str] <- list(get(str)(mode="docs"))
      }
      descs <- c(descs, unname(unlist(automatic_str_cache[[str]][col]))) 
    }
    unis <- unique(df[[col]])
    if(length(unis)==1 & sum(is.na(unis))==1){
      mins <- c(mins, NA)
      maxes <- c(maxes, NA)
      values <- c(values, NA)
      means <- c(means, NA)
      medians <- c(medians, NA)
      num_na <- c(num_na, sum(is.na(df[[col]])))
      data_info_mode <- c(data_info_mode, "All NA")
    } else{
      if(is.factor(df[[col]])) {
        ranges <- as.character(levels(df[[col]]))
        val <- paste(paste(sort(unis),collapse=", "))
        data_info_mode <- c(data_info_mode, "All Levels/Factors")
      } else {
        ranges <- as.character(range(unis,na.rm=TRUE))
        if(length(unis)>10){
          val <- paste(paste(sample(unis,size=3,replace=FALSE),collapse=", "))
          data_info_mode <- c(data_info_mode, "Random Sampling")
        } else {
          val <- freq_string(df[[col]])
          data_info_mode <- c(data_info_mode, "All Levels/Factors")
        }
      }
      mins <- c(mins, ifelse(str_length(ranges[1])>200, paste(str_sub(ranges[1],end=200),"..."), ranges[1]))
      maxes <- c(maxes, ifelse(str_length(ranges[2])>200, paste(str_sub(ranges[2],end=200),"..."), ranges[2]))
      values <- c(values, val)
      
      # Calculate additional metrics
      na_omit_col <- na.omit(df[[col]])
      col_vals <- df[[col]]
      num_na <- c(num_na, sum(is.na(df[[col]])))
      if(sum(!is.na(as.numeric(na_omit_col))) == length(na_omit_col)){
        col_vals <- as.numeric(col_vals)
        means <- c(means, format(mean(col_vals, na.rm = TRUE), nsmall=2, scientific=FALSE))
        medians <- c(medians, median(col_vals, na.rm = TRUE))
      } else{
        means <- c(means, NA)
        medians <- c(medians, NA)
      }
    }
  }
  return(tibble("Domain"=domains, "Construct" = unlist(constructs), "Item (Analytic Set Column)"=cols, 
                "RedCap Fields"=fields, "Other Items Used"=cols_used, "Analysis Description"=analysis,
                "Code Description"=trimws(str_replace_all(descs,"\n"," ")), "Data Info Mode"=data_info_mode, 
                "Data Info"=values, "Min"=mins, "Max"=maxes, "Mean"=means, "Median"=medians, "Number Na"=num_na))
}



#' Order Functions
#' 
#' @description orders the names of a study
#'
#' @return vector of function names ordered
#' @export
#'
#' @examples
#' \dontrun{
#' order_functions()
#' }
order_functions <- function(names=NULL){
  if (is.null(names)){
    names <- find_all_constructs()
  } 
  ids <- seq(1,length(names))
  adj <- matrix(nrow=length(names),ncol=length(names), data=0)
  for(id in ids){
    deps_name <- get_dependencies(names[id])
    deps_ids <- which(names %in% deps_name)
    for(deps_id in deps_ids){
      adj[id,deps_id] <- 1
    }
  }
  return(rev(names[topo_sort(graph_from_adjacency_matrix(adj))]))
}






#' Check Order Functions
#' 
#' @description orders the names of a study
#'
#' @return vector of function names ordered
#' @export
#'
#' @examples
#' \dontrun{
#' order_functions()
#' }
check_order_functions <- function(names){
  dependencies <- vector()
  forbade_depends <- vector()
  has_depending <- vector()
  order <- names
  for(name in names){
    depends <- get_dependencies(name)
    dependencies <- c(dependencies, depends)
    for(depend in depends){
      forbade <- paste(depend,name)
      check_forbade <- paste(name,depend)
      if(sum(forbade_depends==check_forbade)>0){
        stop(paste("Circular Depenendency!",name,"can't depend on",depend,"because", depend,"already depends on",name))
      }
      forbade_depends <- c(forbade_depends, forbade)
      has_depending <- c(has_depending,name)
      if(which(order==depend) > which(order==name)){
        stop(paste("Column order violates dependency of",depend,"for column",name))
      }
    }
  }
}
