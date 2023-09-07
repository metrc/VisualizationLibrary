#' Randomize Tibble
#' 
#' @description randomizes each column of tibble with replacement
#' 
#' @param data Tibble
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' randomize_tibble()
#' }
randomize_tibble <- function(data){
  sample_replace<- function(x){
    return(sample(x, replace=TRUE))
  }
  if (is.null(pkg.globals$seed)){
    pre_sample <-  str_sub(intToUtf8(rev(utf8ToInt(str_replace(as.character(as.numeric(Sys.time())),"\\.","")))),end=-13)
    test <- 0
    for(i in seq(1:pre_sample)){
      test <<- test + sum(sample(1:100))
      se <-  str_sub(intToUtf8(rev(utf8ToInt(str_replace(as.character(as.numeric(Sys.time())),"\\.","")))),end=-13)
    }
    pkg.globals$seed <- se
  }
  set.seed(pkg.globals$seed)
  return(data %>% mutate_all(sample_replace))
}


#' Anonymize Redcap Data
#' 
#' @description Helps address phi concerns when distributing redcap data
#' 
#' @param data Tibble
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' anonymize_redcap_data()
#' }
anonymize_redcap_data <- function(date_shift=TRUE, date_shift_each_id=TRUE, hash_ids=TRUE, hash_facility=TRUE, anonimize_ids=FALSE, anonimize_facility=FALSE, hash_key="fullset", df=NULL){
  if(is.null(df)){
    data <- get_data_developer()
    pkg.globals$data <- NULL
    pkg.globals$anonymize_args <- list(date_shift=date_shift, date_shift_each_id=date_shift_each_id, hash_ids=hash_ids,
                                       hash_facility=hash_facility, anonimize_ids=anonimize_ids, anonimize_facility=anonimize_facility, hash_key=hash_key)
    key <- tibble("study_id"=unique(data$study_id))
    gc()
  } else{
    if(!sum(is.null(pkg.globals$anonymize_args))==0 | !"study_id" %in% colnames(df)){
      return(df)
    }
    key <- pkg.globals$anonymize_key
    if(sum(df$study_id %in% key$study_id)==0){
      warning("skipping this associated data anonymization because it appears already anonymized")
      return(df)
    }
    date_shift <- pkg.globals$anonymize_args$date_shift
    date_shift_each_id <- pkg.globals$anonymize_args$date_shift_each_id
    hash_ids <- pkg.globals$anonymize_args$hash_ids
    hash_facility <- pkg.globals$anonymize_args$hash_facility
    anonimize_ids <- pkg.globals$anonymize_args$anonimize_ids
    anonimize_facility <- pkg.globals$anonymize_args$anonimize_facility
    hash_key <- pkg.globals$anonymize_args$hash_key
    data <- df
    patch_numeric <- FALSE
    if(is.numeric(data$study_id)){
      patch_numeric <- TRUE
      data['study_id'] <- as.character(data$study_id)
    }
  }
  codes <- get_codebook() %>% filter(field_name %in% colnames(data))
  if(date_shift==FALSE & date_shift_each_id==TRUE){
    stop("date_shift_each_id can't be set to TRUE when date_shift==FALSE") 
  }
  if(hash_ids==TRUE & anonimize_ids==TRUE){
    stop("hash_ids==TRUE and anonimize_ids==TRUE - pick one and set the other to FALSE") 
  }
  if(hash_facility==TRUE & anonimize_facility==TRUE){
    stop("hash_facility==TRUE and anonimize_facility==TRUE - pick one and set the other to FALSE") 
  }
  shift <- rep(0,nrow(data))
  if(date_shift){
    if(!is.null(df)){
      shift <- left_join(data, key, by="study_id") %>% pull(shift)
    } else{
      shift <- rep(sample(seq(1:364),1),nrow(data))
      if(date_shift_each_id){
        date_shift_ids <- data %>%
          select(study_id) %>%
          group_by(study_id) %>%
          mutate(shift = rep(sample(seq(1:364),1), length(study_id)))
        shift <- date_shift_ids$shift
      }
      key <- left_join(key, date_shift_ids %>% distinct(), by="study_id")
    }
    date_ymd_converter <- function(x){
      as.Date(x,"%Y-%m-%d") + shift
    }
    date_ymd_cols <- codes %>%
      filter(text_validation_type_or_show_slider_number=="date_ymd") %>%
      pull(field_name)
    data <- data %>% 
      mutate_at(date_ymd_cols, date_ymd_converter)
    data <- data %>% 
      mutate_at(date_ymd_cols, as.character)
    
    datetime_ymd_seconds_converter <- function(x){
      strptime(x,format="%Y-%m-%d %H:%M:%S") + shift*60*60*24
    }
    datetime_ymd_seconds_character <- function(x){
      strftime(x,format="%Y-%m-%d %H:%M:%S")
    }
    datetime_ymd_seconds_cols <- codes %>%
      filter(text_validation_type_or_show_slider_number=="datetime_seconds_ymd") %>%
      pull(field_name)
    data <- data %>% 
      mutate_at(datetime_ymd_seconds_cols, datetime_ymd_seconds_converter)
    data <- data %>% 
      mutate_at(datetime_ymd_seconds_cols, datetime_ymd_seconds_character)
    
    date_ce_converter <- function(x){
      as.Date(x,"%Y-%m-%d") + shift
    }
    date_ce_cols <- codes %>%
      filter(text_validation_type_or_show_slider_number=="date_ce") %>%
      pull(field_name)
    data <- data %>% 
      mutate_at(date_ce_cols, date_ce_converter)
    data <- data %>% 
      mutate_at(date_ce_cols, as.character)
    
    datetime_ymd_converter <- function(x){
      strptime(x,format="%Y-%m-%d %H:%M") + shift*60*60*24
    }
    datetime_ymd_character <- function(x){
      strftime(x,format="%Y-%m-%d %H:%M")
    }
    datetime_ymd_cols <- codes %>%
      filter(text_validation_type_or_show_slider_number=="datetime_ymd") %>%
      pull(field_name)
    data <- data %>% 
      mutate_at(datetime_ymd_cols, datetime_ymd_converter)
    data <- data %>% 
      mutate_at(datetime_ymd_cols, datetime_ymd_character)
    
        # Apply the functions to the appropriate columns in the ASSOCIATED data
    if(!is.null(df)){
      
      shift_date <- function(x) {
        x_date <- as.Date(x, format = "%Y-%m-%d")
        x_shifted <- ifelse(is.na(x_date), x, as.character(x_date + shift))
        x_shifted
      }

      shift_posix <- function(x) {
        x_posix <- as.POSIXct(x)
        x_shifted <- ifelse(is.na(x_posix), x, format(x_posix + shift * 60 * 60 * 24, "%Y-%m-%d %H:%M:%S"))
        x_shifted
      }
      
      found_dates_col <- colnames(data)[str_detect(str_to_lower(colnames(data)),"_date|^date")]
      found_posix_col <- colnames(data)[str_detect(colnames(data),"POSIX")]

      data <- data %>%
        mutate_at(found_dates_col, shift_date) %>%
        mutate_at(found_posix_col, shift_posix)

    }
    
  }
  if(hash_ids){
    if(!is.null(df)){
      data <- left_join(data, key %>% select(study_id, hash_id), by="study_id") %>%
        mutate(study_id=hash_id) %>%
        select(-hash_id)
    } else{
      if(hash_key=="fullset"){
        hash_key <- as.character(sum(as.numeric(unique(data$study_id))))
      }
      duplicates<-TRUE
      modifier<-1
      while(duplicates){
        hashed_ids <- str_sub(str_remove_all(cli::hash_md5(paste0(hash_key,modifier,as.character(unique(data$study_id)))),"[a-z]"), end=12)
        if(sum(duplicated(hashed_ids))==0){
          duplicates = FALSE
        }
        modifier <- modifier*10
      }
      new_ids <- left_join(data %>% select(study_id),
                           tibble(study_id = unique(data$study_id), new_id=as.character(as.numeric(hashed_ids))))
      key <- left_join(key, data %>% 
                         mutate(hash_id = as.character(as.numeric(new_ids$new_id))) %>% 
                         select(study_id, hash_id) %>%
                         distinct(), by="study_id")
      data['study_id'] <- new_ids$new_id
      data  <- data %>% arrange(study_id)
    }
  }
  if(anonimize_ids){
    if(!is.null(df)){
      data <- left_join(data, key %>% select(study_id, anon_id), by="study_id") %>%
        mutate(study_id=anon_id) %>%
        select(-anon_id)
    } else{
      new_ids <- left_join(data %>% select(study_id),
                           tibble(study_id = unique(data$study_id), new_id=sample(seq(1000000:9000000), length(unique(data$study_id)))))
      key <- left_join(key, data %>% 
                         mutate(anon_id = as.character(as.numeric(new_ids$new_id))) %>% 
                         select(study_id, hash_id) %>%
                         distinct(), by="study_id")
      data['study_id'] <- as.character(as.numeric(new_ids$new_id))
      data  <- data %>% arrange(study_id)
    }
  }
  if(hash_facility & "facilitycode" %in% colnames(data) & is.null(df)){
    if(hash_key=="fullset"){
      hash_key <- paste0(unique(data$facilitycode))
    }
    duplicates<-TRUE
    modifier<-1
    while(duplicates){
      hashed_fa <- str_to_upper(str_sub(str_remove_all(cli::hash_md5(paste0(hash_key,modifier,as.character(unique(data$facilitycode)))),"[0-9]"), end=5))
      if(sum(duplicated(hashed_fa))==0){
        duplicates <- FALSE
      }
      modifier <- modifier*10
    }
    new_facility <- left_join(data %>% select(facilitycode),
                              tibble(facilitycode = unique(data$facilitycode), new_facility=hashed_fa) %>% filter(!is.na(facilitycode)))
    data['facilitycode'] <- new_facility$new_facility
    data  <- data %>% arrange(facilitycode)
  }
  if(anonimize_facility & "facilitycode" %in% colnames(data) & is.null(df)){
    fa <- unique(data$facilitycode)
    randString <- function(upperCase) {
      ASCII <- NULL
      if(upperCase>0)  ASCII <- c(ASCII, sample(65:90, upperCase))
      return( rawToChar(as.raw(sample(ASCII, length(ASCII)))) )
    }
    new_fa <- vector()
    for(f in fa){
      not_found <- TRUE
      while(not_found){
        new <- randString(3)
        if(!new %in% fa){
          new_fa <- c(new_fa, new)
          not_found <- FALSE
        }
      }
    }
    new_facility <- left_join(data %>% select(facilitycode),
                              tibble(facilitycode = fa, new_facility=new_fa) %>% filter(!is.na(facilitycode)))
    data['facilitycode'] <- new_facility$new_facility
    data  <- data %>% arrange(facilitycode)
  }
  if(!is.null(df)){
    if(patch_numeric){
      data['study_id'] <- as.numeric(data$study_id)
    }
    return(data)
  }
  pkg.globals$data <- data
  pkg.globals$anonymize_key <- key
}


#' is anonymizable
#' 
#' @description this helps you determine if the code created can really be used with anonymized data.
#' It checks for hardcoded dates and study_ids
#'
#' @param path path to script
#'
#' @return boolean
#' @export
#'
#' @examples
#' \dontrun{
#' is_anonymizable()
#' }
is_anonymizable <- function(path){
  code <- readLines(path)
  offset <- which(str_detect(code,"#.+olumn"))[1]
  code <- code[offset:length(code)]
  
  dates_used <- which(str_detect(code,"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))+offset-1
  
  id_check <- paste(paste0("[^0-9|\\.]",pkg.globals$anonymize_key$study_id,"[^0-9|\\.]"), collapse="|")
  id_compared <- which(str_detect(code,id_check))+offset-1
  if(length(id_compared)==0 & length(dates_used)==0){
    return("")
  }
  if(length(id_compared)!=0 & length(dates_used)==0){
    return(paste0("Anonymization Issue with IDs:\n---------------------------\n",
                  paste(paste("Line",id_compared," --> ",code[id_compared-offset+1]), collapse="\n")))
  }
  if(length(id_compared)==0 & length(dates_used)!=0){
    return(paste0("Anonymization Issue with Dates:\n---------------------------\n",
                  paste(paste("Line",dates_used," --> ",code[dates_used-offset+1]), collapse="\n")))
  }
  if(length(id_compared)!=0 & length(dates_used)!=0){
    return(paste0(paste0("Anonymization Issue with IDs:\n---------------------------\n",
                         paste(paste("Line",id_compared," --> ",code[id_compared-offset+1]), collapse="\n")),
                  "\n\n",
                  paste0("Anonymization Issue with Dates:\n---------------------------\n",
                         paste(paste("Line",dates_used," --> ",code[dates_used-offset+1]), collapse="\n"))))
  }
}



#' Filter Redcap Data
#' 
#' @description Helps address phi concerns when distributing redcap data by removing screened
#' 
#' @param filter_column name of column to filter by
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' Filter_redcap_data()
#' }
filter_redcap_data <- function(filter_column){
  require_study()
  limit_ids_df <- get_construct_output(filter_column)
  if(1 %in% limit_ids_df[[filter_column]]){
    limit_ids <- unique(limit_ids_df$study_id[limit_ids_df[[filter_column]]==1])
  } else{
    limit_ids <- unique(limit_ids_df$study_id[limit_ids_df[[filter_column]]])
  }
  data <- get_data_developer()
  pkg.globals$data <- data %>% 
    filter(study_id %in% limit_ids)
  clear_cache()
  gc()
}
