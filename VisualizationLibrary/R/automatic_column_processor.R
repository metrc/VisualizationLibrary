#' Automatic Constructor
#' 
#' @description Helps automatically create simple redcap columns in the analytic set
#' 
#' @param fields a vector of fields to add to the analytic set
#' @param event a vector of length 1
#' @param instance a vector of length 1
#' @param instrument a vector of length 1
#' @param recode a list of recodings of all fields or a list of lists with length == fields length of recodings of each field
#' @param missing_val replace all na with this value
#' @param combine_checkbox unites checkboxes before recoding
#' @param mode either "data", "code", "columns", "count", or "fields"
#' @param recode_before_combine this will recode a checkbox before merging it with its other checkbox fields
#'
#' @return joined columns
#' @export
#'
#' @examples
#' \dontrun{
#' automatic_constructor()
#' }
automatic_constructor <- function(fields, event="baseline_arm_1", instrument=NA, instance=NA, recode = list(), missing_val=NA, combine_checkbox=TRUE, mode="data", recode_before_combine=FALSE){
  if(combine_checkbox & recode_before_combine){
    pre_recode<-recode
    names(pre_recode) <- fields
  }
  docs<-FALSE
  if(mode=="docs"){
    docs <- TRUE
    docs_fields <- fields
    mode <- "data"
  }
  dict <- get_codebook()
  if(length(event)!=1){
    stop("Only one event per constructor call - make multiple auto columns for multiple events")
  }
  if(mode!="data"){
    get_data <-  function(x){
      return(x %>% purrr::map_dfc(setNames, object = list(character())))
    }
  }
  checkbox_fields <- dict %>% 
    filter(field_name %in% fields & field_type=="checkbox") %>%
    pull(field_name)
  checkbox_fields_list <- list()
  for(f in checkbox_fields){
    fs <-  paste0(f,"___",str_extract(str_remove_all(unlist(strsplit(dict[dict$field_name==f,]$select_choices_or_calculations,"\\|")),"^ |^ +"),"^[0-9]+|^[0-9]"))
    checkbox_fields_list[f] <- list(fs)
    fields <- append(fields,fs,after=which(fields==f))
  }
  fields <- fields[!fields %in% checkbox_fields]
  if(combine_checkbox){
    code <- paste0("df <- AUTOMATIC_DATA %>%\nselect(",paste(c("study_id","redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance",fields),collapse=", "),") %>%",
                  "\nfilter(redcap_event_name=='",event,"')\n")
    df <- get_data(c("study_id","redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance",fields)) %>% 
      filter(redcap_event_name==event)
    for(check in names(checkbox_fields_list)){
      print(paste("Merging Checkbox",check))
      for(subcheck in checkbox_fields_list[[check]]){
        subval <- str_remove(subcheck,".+___")
        if(recode_before_combine){
          if(subval %in% names(pre_recode[[check]])){
            df <- df %>% mutate(!!subcheck:= ifelse(df[[subcheck]]==1, pre_recode[[check]][[subval]], NA))
            code <- c(code, paste0("df <- df %>% mutate(",subcheck,"=ifelse(",subcheck,"==1,'",pre_recode[[check]][[subval]],"', NA))\n"))
          } else{
            df <- df %>% mutate(!!subcheck:= ifelse(df[[subcheck]]==1, subval, NA))
            code <- c(code, paste0("df <- df %>% mutate(",subcheck,"=ifelse(",subcheck,"==1,",subval,", NA))\n"))
          }
        } else{
          df <- df %>% mutate(!!subcheck:= ifelse(df[[subcheck]]==1, subval, NA))
          code <- c(code, paste0("df <- df %>% mutate(",subcheck,"=ifelse(",subcheck,"==1,",subval,", NA))\n"))
        }
      }
      df <- df %>% unite("check", checkbox_fields_list[[check]], sep=",", na.rm=T) %>%
        rename(!!check := check)
      code <- c(code, paste0("df <- df %>% unite('",check,"',",paste(checkbox_fields_list[check],collapse="','"),",sep=',', na.rm=T)\n"))
    }
  } else{
    code <- paste0("df <- AUTOMATIC_DATA %>%\nselect(",
                   paste(c("study_id","redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance",fields),collapse=", "),")\n %>%",
                   "\nfilter(redcap_event_name=='",event,"')\n")
    df <- get_data(c("study_id","redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", fields)) %>% filter(redcap_event_name==event)
  }
  if(!"redcap_repeat_instance" %in% colnames(df)){
    df["redcap_repeat_instance"] <- NA
    code <- c(code,"df['redcap_repeat_instance'] <- NA\n")
  }
  if(!"redcap_repeat_instrument" %in% colnames(df)){
    df["redcap_repeat_instrument"] <- NA
    code <- c(code,"df['redcap_repeat_instrument'] <- NA\n")
  }
  if(is.na(instance)){
    code <- c(code,"df <- df %>% filter(is.na(redcap_repeat_instance))\n")
    df <- df %>% filter(is.na(redcap_repeat_instance))
  }
  if(!is.na(instance)){
    code <- c(code,paste0("df <- df %>% filter(redcap_repeat_instance=='",instance,"')\n"))
    df <- df %>% filter(redcap_repeat_instance==instance)
  }
  if(is.na(instrument)){
    code <- c(code,"df <- df %>% filter(is.na(redcap_repeat_instrument))\n")
    df <- df %>% filter(is.na(redcap_repeat_instrument))
  }
  if(!is.na(instrument)){
    code <- c(code,paste0("df <- df %>% filter(redcap_repeat_instrument=='",instrument,"')\n"))
    df <- df %>% filter(redcap_repeat_instrument==instrument)
  }
  
  if(length(recode)>1 & length(recode)!=length(colnames(df))-4){
    stop(paste("There are only",length(recode),"recode lists but there are", length(colnames(df))-2,
               "fields -",paste(colnames(df)[!colnames(df)%in%c("study_id","redcap_event_name")],collapse = ", "),
               "(If there are fewer fields than you expected make sure you understand the meaning of combine_checkbox=TRUE"))
  }
  columns <- colnames(df)[!colnames(df)%in%c("study_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")]
  if(length(recode)==1 & length(columns)>1 & !is.null(names(recode))){
    stop(paste0("There is a single named list for recode but there are multiple columns and the list names can't be blank",
               ", to recylce list make sure top level list is unnamed for recode (",
               "fields -",paste(columns,collapse = ", "),")"))
    
  }

  if(length(recode)==1){
    pos <- 0
    rec <- recode[[1]]
    for(i in seq(1:length(columns))){
      pos <- pos + 1
      old_col_name <- columns[pos]
      print(paste("recoding",old_col_name))
      new_col_name <- names(recode)
      col_name <- old_col_name
      if(!is.null(new_col_name)){
         if(new_col_name>""){
            col_name <- new_col_name
            columns[pos] <- new_col_name
         } 
      }
      if(length(rec)>0){
        if(old_col_name %in% checkbox_fields & recode_before_combine){
          code <- c(code, paste0("df <- df %>% mutate(",col_name,"=",old_col_name,")\n"))
          df <- df %>% rename(!!col_name := !!old_col_name)
        } else{
          if(is.logical(unname(unlist(rec))) | is.numeric(unname(unlist(rec)))){
            code <- c(code, paste0("new_codes <- list(",paste0(paste0("'",names(rec),"'=",unname(unlist(rec))),collapse=","),")\n"))
          } else {
            code <- c(code, paste0("new_codes <- list(",paste0(paste0("'",names(rec),"'='",unname(unlist(rec))),"'",collapse=","),")\n"))
          }
          code <- c(code, paste0("df <- df %>% mutate(",col_name,"=recode(",old_col_name,",!!!new_codes))\n"))
          df <- df %>% mutate(!!col_name:= recode(df[[old_col_name]], !!!rec))
        }
      } else{
        if(old_col_name!=col_name)
          code <- c(code, paste0("df <- df %>% mutate(",col_name,"=",old_col_name,"))\n"))
        df <- df %>% rename(!!col_name := !!old_col_name)
      }
    }
  }
  if(length(recode)>1){
      pos <- 0
      for(rec in recode){
        pos <- pos + 1
        old_col_name <- columns[pos]
        print(paste("Recoding",old_col_name))
        new_col_name <- names(recode)[pos]
        col_name <- old_col_name
        if(!is.null(new_col_name)){
          if(new_col_name>""){
            col_name <- new_col_name
            columns[pos] <- new_col_name
          } 
        }
        if(length(rec)>0){
          if(old_col_name %in% checkbox_fields & recode_before_combine){
            code <- c(code, paste0("df <- df %>% mutate(",col_name,"=",old_col_name,")\n"))
            df <- df %>% rename(!!col_name := !!old_col_name)
          } else{
            if(is.logical(unname(unlist(rec))) | is.numeric(unname(unlist(rec)))){
              code <- c(code, paste0("new_codes <- list(",paste0(paste0("'",names(rec),"'=",unname(unlist(rec))),collapse=","),")\n"))
            } else {
              code <- c(code, paste0("new_codes <- list(",paste0(paste0("'",names(rec),"'='",unname(unlist(rec))),"'",collapse=","),")\n"))
            }
            code <- c(code, paste0("df <- df %>% mutate(",col_name,"=recode(",old_col_name,",!!!new_codes))\n"))
            df <- df %>% mutate(!!col_name:= recode(df[[old_col_name]], !!!rec))
          }
        } else{
          if(old_col_name!=col_name)
          code <- c(code, paste0("df <- df %>% mutate(",col_name,"=",old_col_name,")\n"))
          df <- df %>% rename(!!col_name := !!old_col_name)
        }
      }
  }
  
  code <- c(code, paste0("df <- df %>% select(study_id,",paste(columns, collapse=","),")\n"))
  df <- df %>% select(study_id,all_of(columns))
  
  if(!is.na(missing_val)){
    code <- c(code, paste0("replace_missing <- function(x){replace_na(x,'",missing_val,"')}\n",
                           "df <- df %>% mutate_all(replace_missing)"))
    replace_missing <- function(x){replace_na(x,missing_val)}
    df <- df %>% mutate_all(replace_missing)
  }
  
  code <- c(code, "ids <- AUTOMATIC_DATA %>%\nselect(study_id) %>%\ndistinct()\ndf <- left_join(ids, df)\n")
  ids <- get_data("study_id") %>% distinct()
  df <- left_join(ids, df)
  
  if(mode=="data"){
    if(docs){
      fs <- unique(str_remove(docs_fields,"___.$|___.+$"))
      cs <- colnames(df)[colnames(df)!="study_id"]
      if(length(cs) != length(fs)){
        stop(paste("failed automatic documentation:",fs))
      }
      output <- list()
      for(i in seq(length(fs))){
        output[cs[i]] <- paste0("This column was automatically constructed from data from the field: ",fs[i]," event: ",event," instrument: ",instrument," instance: ",instance,"\nWith label: ",field_info(fs[i],return_packet = T)$label,"\nWith options:\n",paste(field_info(fs[i],return_packet = T)$choices,collapse="\n"))
      }
      return(output)
    }
    return(df)
  }
  if(mode=="code"){
    return(code)
  }
  if(mode=="columns"){
    return(columns)
  }
  if(mode=="count"){
    return(length(columns))
  }
  if(mode=="fields"){
    return(fields)
  }
}


