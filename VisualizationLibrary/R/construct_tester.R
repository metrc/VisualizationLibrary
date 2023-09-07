
#' Test Construct Function Documentation
#' 
#' @description checks whether the documentation of a construct function is compliant with standard output requirments
#' 
#' @param name the name of the construct excluding the starting study name
#' @return character string of issues
#' @export
#'
#' @examples
#' \dontrun{
#' test_documentation()
#' }
test_documentation<- function(name){
  require_study()
  require_automatic_cache()
  if(name %in% names(pkg.globals$auto_cache$constructs_list)){
    return("")
  }
  str <- get_construct_function_name(name)
  help_info <- try(help(str, package = "AnalyticCodebase"))
  if (inherits(help_info, 'try-error')){
    return(paste("Could not test documentation for name:",name,"function:",str,"because error was thrown when help() was run:",attr(help_info, 'condition')))
  }
  comment_lines <- gsub("_\b|^     ","",capture.output(tools:::Rd2txt(utils:::.getHelpFile(as.character(help_info)))))
  comment_lines <- comment_lines[comment_lines!=""]
  comment_str <- paste(comment_lines, collapse="\n")
  comment_issues <- vector()
  
  if(!str_detect(tolower(comment_lines[1]), name) & study_uses_legacy()){
    comment_issues <- c(comment_issues,"Name of function should be in the top line of documentation")
  }
  
  if(sum(str_detect(comment_lines,"Description:"))==0){
    comment_issues <- c(comment_issues,"Missing description")
  } else{
    if(str_locate(comment_str, "Usage:")[1]-str_locate(comment_str, "Description:")[2]<60){
      comment_issues <- c(comment_issues,"Missing a DETAILED description")
    }
  }
  return(paste(comment_issues, collapse=" | "))
}


#' Test Construct Function Code
#' 
#' @description checks whether the code of a construct function is compliant with standard output requirments
#' 
#' @param name the name of the construct excluding the starting study name
#' @return character string of issues
#' @export
#'
#' @examples
#' \dontrun{
#' test_code()
#' }
test_code<- function(name){
  require_study()
  require_automatic_cache()
  if(name %in% names(pkg.globals$auto_cache$constructs_list)){
    return("")
  }
  code <- unlist(strsplit(deparse(get_construct(name)),"\n"))
  return_line <- length(code)-1
  
  code_issues <- vector()
  
  if(name!=tolower(name)){
    code_issues <- c(code_issues,"Construct functions should have only lower case names")
  }
  
  if(str_remove_all(code[1], "function|\\)|\\(| ")!=""){
    code_issues <- c(code_issues,"Construct functions should not take any arguments!")
  }
  
  if(str_length(str_remove_all(code[return_line], "_|[a-z|0-9]|[A-Z|0-9]|\\)|\\(| "))){
    code_issues <- c(code_issues,"Final return doesn't return a single named variable!")
  }
  
  if(sum(str_detect(code,"get_data_developer"))>0){
    code_issues <- c(code_issues,"Construct functions should not use get_data_developer")
  } else{
    if(sum(str_detect(code,"get_data\\(c\\("))!=sum(str_detect(code,"get_data\\("))){
      code_issues <- c(code_issues,"Construct functions should pass a vector starting with 'c(' to get_data")
    }
    fields <- get_construct_fields(name)
    missing_fields <- fields[!fields %in% pkg.globals$columns]
    if(length(missing_fields)>0){
      code_issues <- c(code_issues,paste("Construct functions asks for fields in get_data not in study dataset:", paste(missing_fields, collapse = ', ')))
    }
  }
  return(paste(code_issues, collapse=" | "))
}



#' Test Construct Function Output
#' 
#' @description checks whether the output of a construct function is compliant with standard output requirments
#' 
#' @param name the name of the construct excluding the starting study name
#' @param ids the full list of study ids, can be used to increase performance when this is already known
#'
#' @return character string of issues
#' @export
#'
#' @examples
#' \dontrun{
#' test_output()
#' }
test_output<- function(name, ids=NULL){
  risky_test <- function(name, ids=NULL){
    require_study()
    if(is.null(ids)){
      ids <- get_data("study_id") %>% pull("study_id") %>% unique()
    }
    if(is_tibble(name)){
      output <- name
    } else{
      output <- try(get_construct_output(name))
      if (inherits(output, 'try-error')){
        return(paste("Could not test code because error was thrown:",attr(output, 'condition')))
      }
    }
    issues <- vector()
    if(!is_tibble(output)){
      issues <- c(issues, "The output of a construct function must be a tibble!")
      return(paste(issues, collapse=" | "))
    }
    if(length(colnames(output))!=2){
      issues <- c(issues, "The output of a construct function must be exactly two columns!")
    }
    if("study_id" != colnames(output)[1]){
      issues <- c(issues, "The output of a construct function must have the first column be 'study_id'!")
      return(paste(issues, collapse=" | "))
    }
    if(name != colnames(output)[2] & !is_tibble(name) & study_uses_legacy()){
      issues <- c(issues, "The output of a construct function's second column must match the name of the construct (excluding the starting study name)!")
    }
    duplicated_ids <- output %>% filter(duplicated(study_id)) %>% pull(study_id)
    if(!is_empty(duplicated_ids)){
      issues <- c(issues, paste("The output of this construct function has duplicated study_ids:", paste(unique(duplicated_ids),collapse=",")))
      return(paste(issues, collapse=" | "))
    }
    if(length(output %>% pull(study_id)) > length(ids)){
      extra_ids <- output %>% filter(!study_id %in% ids) %>% pull(study_id)
      issues <- c(issues,  paste("The output of this construct function has extra study ids:", paste(unique(extra_ids), collapse=",")))
    }
    if(length(output %>% pull(study_id)) < length(ids)){
      missing_ids <- ids[!ids %in% output$study_id]
      issues <- c(issues,  paste("The output of this construct function has missing study ids:", paste(unique(missing_ids), collapse=",")))
    }
    return(paste(issues, collapse=" | "))
  }
  output <- try(risky_test(name, ids))
  if (inherits(output, 'try-error')){
    return(paste("Construct output was so egregious that tester failed, potential system break.",
                 "Could not test code because error was thrown:",attr(output, 'condition')))
  }
  return(output)
}



#' Test Study Construct Functions
#' 
#' @description goes through each construct and tests documentation, code, and output for compliance with data standards
#'
#' @return table of test results
#' @export
#'
#' @examples
#' \dontrun{
#' test_study()
#' }
test_study<- function(){
  require_study()
  require_automatic_cache()
  full_ids <- get_data("study_id") %>% pull("study_id") %>% unique()
  print(paste("***** Testing Study:",pkg.globals$study))
  
  constructs <- vector()
  check_documentation <- vector()
  check_code <- vector() 
  check_output <- vector() 
  
  for (construct in find_all_constructs(auto_cache_ignore=TRUE)){
    constructs <- c(constructs, construct)
    check_documentation <- c(check_documentation, test_documentation(construct))
    check_code <- c(check_code, test_code(construct))
    check_output <- c(check_output, test_output(construct, ids=full_ids))
  }
  output <- as_tibble(list(name=constructs, documentation=check_documentation, code=check_code, output=check_output))
  
  unfill_vec <- function(x) {
    same <- x == dplyr::lag(x)
    ifelse(!is.na(same) & same, "", x)
  }
  
  output <- output %>% 
    separate_rows(-name,sep=" \\| ") %>% 
    group_by(name) %>% 
    summarise(documentation=unfill_vec(documentation), code=unfill_vec(code), output=unfill_vec(output)) %>% 
    ungroup() %>%
    distinct()
  
  return(output)
}
