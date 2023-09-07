pkg.globals <- new.env()
pkg.globals$matrix_id <- "1TeL4GKCYK-9TfRP--HKf_9hQapJiZ4bRBmBaG3tvlzg"
pkg.globals$function_prefixes <- c("snow_","nightingale_", "pasteur_", "hopkins_")
pkg.globals$function_prefixes_regex <- paste(pkg.globals$function_prefixes, collapse="|")
pkg.globals$function_sheet_id <- "1P0tiYxtPo3rvm7bUGBkwWjgCGTdp9XaropqsPgP409o"
pkg.globals$full_function_sheet <- NULL
pkg.globals$function_sheet <- NULL
pkg.globals$seed <- NULL
pkg.globals$data <- NULL
pkg.globals$anonymize_args <- list(date_shift=NULL,date_shift_each_id=NULL,hash_ids=NULL,hash_facility=NULL,anonimize_ids=NULL,anonimize_facility=NULL,hash_key=NULL)
pkg.globals$anonymize_key <- NULL
anonimized_associated <- vector()
pkg.globals$study <- NULL
pkg.globals$study_names <- NULL
pkg.globals$redcap_key <- NULL
pkg.globals$server <- NULL
pkg.globals$folder <- NULL
pkg.globals$matrix <- NULL
pkg.globals$full_matrix <- NULL
pkg.globals$columns <- NULL
pkg.globals$search_columns <- NULL
pkg.globals$auto_column_mode <- NULL
pkg.globals$codebook <- NULL
pkg.globals$cache <- list()
pkg.globals$auto_cache <- list()
pkg.globals$associated_data_calls <- vector()


#' Clear Cache
#' 
#' @description clears the cache of already processed get_construct calls
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' clear_cache()
#' }
clear_cache<-function(){
  pkg.globals$cache <- list()
}



#' Get Globals
#' 
#' @description get global data
#'
#' @return globals
#' @export
#'
#' @examples
#' \dontrun{
#' get_globals()
#' }
get_globals <- function(){
  return(pkg.globals)
}


#' Get Columns
#' 
#' @description get all columns from data file, though they may not all be loaded at that time
#'
#' @return vector of column names
#' @export
#'
#' @examples
#' \dontrun{
#' get_columns()
#' }
get_columns <- function(){
  return(pkg.globals$columns)
}

