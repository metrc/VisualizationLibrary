#' Pretty prints a vector so it can be pasted into a vecort
#' 
#' @description you can pass it any vector and it will format it
#'
#' @param vec any vector
#'
#' @return string of vector
#' @export
#'
#' @examples
#' \dontrun{
#' pretty_print_cols()
#' }
pretty_print_cols <- function(vec){
  if(is.data.frame(vec)){
    vec <- colnames(vec)
  }
  return(paste0("'",paste(vec, collapse="', '"),"'"))
}



#' New Function Kickstarter
#' 
#' @description you can pass it any vector and it will format it
#'
#' @param function_name name of the function you want to build (sans the study name)
#' @param field name of a redcap field to start with
#' @param construct name of construct to start with
#' @param event event to filter in
#' @param instance event to filter in
#' @param instrument event to filter in
#'
#' @return string of function
#' @export
#'
#' @examples
#' \dontrun{
#' kickstart_function()
#' }
kickstart_function <- function(function_name, field, construct="screened", event=NA, instance=NA, instrument=NA){
  study <- pkg.globals$study
  filter <- ""
  if(!is.na(event)){
    filter <- paste(filter, paste0("redcap_event_name == '",event,"'"), sep=" & ")
  }
  if(!is.na(instance)){
    filter <- paste(filter, paste0("redcap_repeat_instance == ",instance), sep=" & ")
  }
  if(!is.na(instrument)){
    filter <- paste(filter, paste0("redcap_repeat_instrument == '",instrument,"'"), sep=" & ")
  }
  filter <- str_remove(filter, "^ & ")
  new_fields <- vector()
  codebooks <- get_codebook()
  for(f in field){
    codebook <- codebooks %>% filter(field_name==f)
    if(nrow(codebook)>0){
      if(codebook$field_type=="checkbox"){
        new_fields <- c(new_fields, paste0(f,"___",str_extract(unlist(strsplit(codebook$select_choices_or_calculations,"\\|")),"^[0-9]+|^[0-9]")))
      } else{
        new_fields <- c(new_fields, f)
      }
    } else{
      new_fields <- c(new_fields, f)
    }
  }
  field <- new_fields
  cat(paste0("#' ", study, " ", function_name, "\n#'\n#' @description ", function_name,
             " function imports the ",construct, 
             " construct and ...\n#' \n#' @return\n#' @export\n#'\n#' @examples\n#' \\dontrun{\n#' ",
             study, "_", function_name, "()\n#' }\n", study, "_", function_name,
             " <- function(){\n  data  <- get_data(c('study_id', 'redcap_event_name', '", paste(field,collapse="','"), "'))\n  ", 
             construct, " <- get_construct_output('", construct, "')\n  ", function_name, 
             " <- data %>%\n    filter(", filter, ")%>%\n    rename(",function_name,"=",paste(field,collapse="','"),")\n  ", function_name, 
             " <- full_join(", construct, ", ", function_name, 
             ") %>% select(study_id, ", function_name, ")\n  return(", function_name, ")\n}"))
}


#' Field Info
#' 
#' @description looks up info from the data dictionary for a study for a given field
#'
#' @return nothing but it prints info out
#' @export
#'
#' @examples
#' \dontrun{
#' field_info()
#' }
field_info <- function(fields, width=160, return_packet=FALSE){
  fields <- str_replace(fields, "___.|___.+","")
  codebook <- suppressMessages(suppressWarnings(get_codebook()))
  if(length(fields)>1){
    output <- list()
  }
  for(field in fields){
    label <- codebook[codebook$field_name==field,]$field_label
    form <- codebook[codebook$field_name==field,]$form_name
    check <- codebook[codebook$field_name==field,]$field_type
    label <- str_replace_all(label, "\n", " ")
    choices <- codebook[codebook$field_name==field,]$select_choices_or_calculations
    choices_vec <- str_wrap(unlist(strsplit(choices,"\\|")), width=width)
    choices_ves <- str_replace_all(choices_vec, "<.*>","")
    choices <- paste(choices_vec, collapse="\n")
    eqs <- max(str_length(choices_vec))
    eqs <- ifelse(is.na(eqs),1,eqs)
    llen <- str_length(label)
    llen <- ifelse(is.na(llen),1,llen)
    index_field <- case_when(is.na(check) ~ field,
                             check=='checkbox' ~ paste("CHECKBOX", field),
                             TRUE ~ field)
    packet <- list(label=label, form=form, checkbox=check=="checkbox", choices=choices_vec, values=str_remove(str_extract(choices_vec,"^[^,]+,"),","))
    if(length(fields)>1){
      output[[index_field]] <- packet
    }
    cat(str_replace_all(paste0(index_field,"\n",form,"\n",label,"\n",paste(rep("-",llen),collapse=""),"\n",choices,"\n", paste(rep("=",eqs),collapse=""),"\n","\n"),"\\n\\n","\n"))
  }
  if(return_packet){
    if(length(fields)>1){
      return(output)
    } else{
      return(packet)
    }
  }
}



#' Search Fields Info
#' 
#' @description looks up info from the data dictionary for a study for a given search term
#'
#' @return nothing but it prints info out
#' @export
#'
#' @examples
#' \dontrun{
#' search_fields()
#' }
search_fields <- function(terms, crf=NULL){
  fields <- vector()
  codebook <- suppressMessages(suppressWarnings(get_codebook()))
  if(!is.null(crf)){
    print(paste("Limiting to specific crfs:",paste(unique(codebook$form_name[str_detect(codebook$form_name,crf)]),collapse= ", ")))
    codebook <- codebook %>% filter(str_detect(form_name,crf))
  }
  
  if(is.null(pkg.globals$search_columns)){
    pkg.globals$search_columns <- colnames(get_data_developer())
  }
  missing_fields <- vector()
  for(term in terms){
    field <- unique(c(codebook %>% filter(str_detect(field_name,term)) %>% pull(field_name),codebook %>% filter(str_detect(tolower(field_name),tolower(term))) %>% pull(field_name)))
    labels <- unique(c(codebook %>% filter(str_detect(field_label,term)) %>% pull(field_name),codebook %>% filter(str_detect(tolower(field_label),tolower(term))) %>% pull(field_name)))
    selects <- unique(c(codebook %>% filter(str_detect(select_choices_or_calculations,term)) %>% pull(field_name),codebook %>% filter(str_detect(tolower(select_choices_or_calculations),tolower(term))) %>% pull(field_name)))
    notes <- unique(c(codebook %>% filter(str_detect(field_note,term)) %>% pull(field_name),codebook %>% filter(str_detect(tolower(field_note),tolower(term))) %>% pull(field_name)))
    fields <- c(fields, labels, selects, field, notes)
    missing_fields <- c(missing_fields, pkg.globals$search_columns[str_detect(pkg.globals$search_columns,term)])
  }
  fields <- unique(fields)
  missing_fields <- missing_fields[!missing_fields %in% fields]
  field_info(fields)
  if(length(missing_fields) > 0){
    print("Some fields in the data were not in the codebook:")
    print(missing_fields)
  }
  
  return(c(fields,missing_fields))
}


