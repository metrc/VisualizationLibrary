#' Build Analytic Data Set
#' 
#' @description integrates only test and error free contruct columns into full dataset
#' 
#' @param names you can specify a sublist of columns if you want
#' @param error_path you can specify a path to write the error dataframe to a csv
#'
#'
#' @return analytic_dataset dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' require_study()
#' }
build_analytic_dataset<-function(names=NULL, error_path=NULL){
  require_study()
  if(is.null(names)){
    names <- sort(find_all_constructs())
  }
  missing <- as_tibble(list("name"=names[!names %in% find_all_constructs()], "errors"="missing!", 
                            "documentation"="", "code"="", "output"=""))
  names <- names[names %in% find_all_constructs()]
  errors <- test_study() %>% mutate("errors"="")
  errors <- rbind(errors, missing) 
  working_columns <- errors %>% filter(documentation=="" & code=="" & output=="") %>% pull(name)
  names <- names[names %in% working_columns]
  
  first <- TRUE
  for(fname in names){
    new_column <- try(get_construct_output(fname))
    if (inherits(new_column, 'try-error')){
      errors <- errors %>% 
        mutate(errors=ifelse(name==fname, 
                             paste(fname,"was found but threw an error when it was run",attr(vis_result, 'condition')), 
                             errors))
    } else{
      if (first){
        analytic_dataset <- new_column
        first <- FALSE
      } else{
        analytic_dataset <- left_join(analytic_dataset, new_column)  
      }
    }
  }
  if(is.null(error_path)){
    print(errors %>% filter(!(documentation=="" & code=="" & output=="")))
  } else {
    write_csv(errors, error_path)
  }
  if(first){
    analytic_dataset <- tibble("failed_export"=TRUE)
  }  
  return(analytic_dataset)
}