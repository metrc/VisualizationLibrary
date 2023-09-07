#' Unzip a Construct
#' 
#' @description Helps unzip a zipped construct using info in documentation
#' 
#' @param name name of the construct
#'
#' @return tibble unzipped
#' @export
#'
#' @examples
#' \dontrun{
#' unzip_construct(name)
#' }
unzip_construct <- function(name){
  require_study()
  require_automatic_cache()
  if (name %in% names(pkg.globals$auto_cache$constructs_list)) {
    return("")
  }
  str <- get_construct_function_name(name)
  help_info <- try(help(str, package = "AnalyticCodebase"))
  if (inherits(help_info, "try-error")) {
    return(paste("Could not test documentation for name:", 
                 name, "function:", str, "because error was thrown when help() was run:", 
                 attr(help_info, "condition")))
  }
  comment_lines <- gsub("_\b|^     ", "", capture.output(tools:::Rd2txt(utils:::.getHelpFile(as.character(help_info)))))
  docs <- paste(comment_lines, collapse=" ")
  if(!str_detect(docs, "UNZIPPING INSTRUCTIONS:")){
    stop("This concept does not have 'UNZIPPING INSTRUCTIONS:'")
  }
  checks <- c('(row sep: ".").+','(row sep:".").+','(row sep: "[^"]+").+','(row sep:"[^"]+").+',
              "(row sep: '.').+","(row sep:'.').+","(row sep: '[^']+').+","(row sep:'[^']+').+")
  row_checks <- c('row sep: "."','row sep:"."','row sep: "[^"]+"','row sep:"[^"]+"',
                  "row sep: '.'","row sep:'.'","row sep: '[^']+'","row sep:'[^']+'")
  col_checks <- c('column sep: "."','column sep:"."','column sep: "[^"]+"','column sep:"[^"]+"',
                  "column sep: '.'","column sep:'.'","column sep: '[^']+'","column sep:'[^']+'")
  end_loc <- str_detect(docs,checks)
  if(sum(end_loc)<1){
    stop("Unzip failed! poorly formed end 'row sep:'")
  }
  col_loc <- str_detect(docs,col_checks)
  if(sum(col_loc)<1){
    stop("Unzip failed! poorly formed 'column sep:'")
  }
  row_loc <- str_detect(docs,row_checks)
  if(sum(row_loc)<1){
    stop("Unzip failed! poorly formed 'row sep:'")
  }
  col_reg <- col_checks[col_loc]
  if (length(col_reg)>1){
    col_reg<- col_reg[-1]
  }
  end_reg <- checks[end_loc]
  if (length(end_reg)>1){
    end_reg<- end_reg[-1]
  }
  instructions <- str_remove(str_replace(str_remove(docs, ".+UNZIPPING INSTRUCTIONS:"), end_reg,"\\1"), "^ +|^ ")
  columns <- str_remove_all(str_remove_all(str_remove(str_remove(unlist(strsplit(str_remove(str_remove(instructions, "^columns:"),"column sep:.+"),",")), "^ +|^ "), " +$| $"),"'"),'"')
  col_sep <- str_remove_all(str_remove_all(str_remove_all(str_remove(str_extract(instructions,col_reg)[1],"column sep:"), "^ +|^ | +$| $"),"'"),'"')
  row_sep <- str_remove_all(str_remove_all(str_remove_all(str_remove(str_extract(instructions,row_checks[row_loc])[1],"row sep:"), "^ +|^ | +$| $"),"'"),'"')
  if(col_sep=="|"){
    col_sep <- "\\|"
  }
  if(row_sep=="|"){
    row_sep <- "\\|"
  }
  return( get_construct_output(name) %>% 
            separate_rows(2, sep=row_sep) %>% 
            separate(-study_id, columns, sep=col_sep))
}


#' Write all zipped constructs
#' 
#' @description Helps unzip a zipped construct using info in documentation
#' 
#' @param dir_path the directory to save the files to
#' @param name name of the constructs to join, defaults to null and won't be included
#' @param constructs list of constructs to consider unzipping
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' write_all_zipped_constructs(name)
#' }
write_all_zipped_constructs <- function(dir_path, name=NULL, constructs=NULL){
  if(!is.null(name)){
    added <- get_construct_outputs(unname(unlist(strsplit(name,","))))
    a_cols <- colnames(added)
  }
  if (is.null(constructs)){
    constructs <- find_all_constructs()
  }
  for(c in constructs){
    res <- try(unzip_construct(c),silent=TRUE)
    if(!inherits(res,"try-error")){
      if(!is.null(name)){
        cols <- colnames(res)
        cols <- c(a_cols, cols[!cols %in% a_cols])
        res <- left_join(res,added,by="study_id") %>% select(cols)
      }
      write_csv(res,paste0(dir_path,'/',c,"_long_file.csv"))
    }
  }
}
