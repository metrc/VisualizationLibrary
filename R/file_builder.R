


#' METRC Basic Report
#'
#' @description This function gets the payments for a study using the stored method
#'
#' @param html_table_list list of named html tables (set the name in the list to be the table name)
#' @param file_name place to store html file
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' metrc_basic_report()
#' }
metrc_basic_report <- function(html_table_list, file_name){
  injected_tags <- '\n<meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />\n'
  style_text <- '// this is a simple bit of CSS that makes the tables and text in a report look refined\nbody {\n  font-family: "Times New Roman", Times, serif;\n}\ntable.table-basic {\n  font-family: "Times New Roman", Times, serif;\n  width: 100%;\n  text-align: center;\n}\ntable.table-basic td, table.table-basic th {\n  padding: 10px;\n}\ntable.table-basic tbody td {\n  font-size: 13px;\n}\ntable.table-basic tbody tr {\n    border-bottom: 1px solid black;\n    border-top: 1px solid black;\n    border-collapse: collapse;\n}\n'
  injected_style_text <- 'table{\n    border-collapse:collapse;\n}\ntr td{\n    page-break-inside: avoid;\n    white-space: nowrap;\n}'
  html_page <- paste('<!DOCTYPE html>\n<html>\n<head>',injected_tags,'<style>\n',style_text,injected_style_text,'</style></head>\n<br/>',sep='')
  
  first <- TRUE
  table_names <- names(html_table_list)
  for(i in seq(1:length(html_table_list))){
    table_html <- ""
    if(!first){
      table_html <- paste0(table_html, '\n<p style="page-break-after: always;">&nbsp;</p><br/>\n')
    }
    first <- FALSE
    table_html <- paste0(table_html, '<h3>Table ',i,'. ',table_names[i],'</h3>\n<br/>\n',html_table_list[i],'\n<br/>\n')
    html_page <- paste0(html_page,"\n",table_html)
  }
  
  html_page <- paste(html_page,'\n\n</body>\n</html>')
  write_file(html_page, file_name)
}