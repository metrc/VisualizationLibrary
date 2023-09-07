


#' Set Data
#' 
#' @description Redcap keeps a code book mapping the fields from the full dataset 
#' to the forms and questions they come from. This function downloads that codebook
#' provided you have already called set_data(TODO_STUDY, TODO_REDCAPKEY)
#' 
#' @param study this is the name of the study recognized by the metrc-reports server
#' @param key this is the redcap api key for that study
#' @param date if you want to select a specific day the data was cached input it in 0000-00-00 format
#' @param folder if you want to store your cached data in a custom folder
#' @param server this tells the package that it is within a server environment and to look for local files as data soruces
#' @param columns this allows you to prespecify the columns you want to use
#' @param auto_column_mode this only is utilized if columns are not NULL and if auto_column_mode is "defined"
#' it selects the columns in relevant construct functions, if it is set to "detect" it will include any fields
#' from any functions that use columns present in the data, and if it is set to something else or "" only the columns
#' in the columns argument will be selected
#' 
#' @import readr httr
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' set_data()
#' }
set_data <- function(study, key, date=NULL, folder=NULL, server=FALSE, columns=NULL, auto_column_mode="detect"){
  pkg.globals$cache <- list()
  study<- tolower(study)
  pkg.globals$study <- str_replace_all(study," ","_")
  pkg.globals$server <- server
  pkg.globals$redcap_key <- key
  pkg.globals$auto_column_mode <- ""
  if(pkg.globals$server){
    study_system = gsub(" ","\\ ",study, fixed=TRUE)
    main_folder_system <- paste(getwd(),study_system,sep="/")
    main_folder <- paste(getwd(),study,sep="/")
    folders = system(paste("ls",main_folder_system), intern = TRUE, ignore.stderr = TRUE)
    cds <- folders[str_detect(folders, "_chr_")]
    pkg.globals$folder <- folder
    print(paste("-------------------------------------------->   setting folder",folder))
    mdf <- read_csv(paste(getwd(), folder, "METRC_CONSTRUCT_MATRIX.csv", sep = "/"))
    pkg.globals$matrix <- mdf %>% 
      select(all_of(c("Construct",tolower(study)))) %>%
      rename(function_name=2) %>%
      filter(!is.na(function_name))
    study_name_raw <- colnames(mdf)[which(colnames(mdf)=="Construct")+1:length(colnames(mdf))]
    pkg.globals$study_names <- str_replace_all(str_to_lower(study_name_raw[!is.na(study_name_raw)])," ","_")
    pkg.globals$full_matrix <- mdf
    fdf <- read_csv(paste(getwd(), folder, "Analytic Function Sheet.csv", sep = "/"), col_types = cols(.default = "c"))
    set_function_sheet(fdf, study)
    if(!is.null(columns)){
      if(auto_column_mode=="defined" | auto_column_mode=="detect"){
        columns <- unique(c("study_id","facilitycode","redcap_event_name","redcap_repeat_instrument",
                            "redcap_repeat_instance",find_all_fields(),columns))
      }
    }
    if(length(cds) > 0){
      path_chr_rds <- paste(main_folder,cds[1], sep="/")
      if(!is.null(columns)){
        meta <- feather::feather_metadata(path_chr_rds)
        pkg.globals$columns <- names(meta$types)
        if(auto_column_mode=="detect"){
          columns <- unique(c(columns, find_all_compatible_functions(return_fields=TRUE, quiet=TRUE)))
        }
        print(paste("Loading certain columns from file with",auto_column_mode,"strategy:",paste(columns,collapse=", ")))
        pkg.globals$data <- feather::read_feather(path_chr_rds, columns = columns)
      } else{
        pkg.globals$data <- feather::read_feather(path_chr_rds)
        pkg.globals$columns <- colnames(pkg.globals$data)
      }
      pkg.globals$path <- path_chr_rds
      return(TRUE)
    }
    csv <- folders[str_detect(folders, "_csv_")]
    if(length(csv) > 0){
      path <- paste(main_folder,csv[1], sep="/")
      pkg.globals$data <- readr::read_csv(path, col_types = cols(.default = col_character()))
      pkg.globals$path <- path
      pkg.globals$columns <- colnames(pkg.globals$data)
      if(!is.null(columns)){
        if(auto_column_mode=="detect"){
          columns <- unique(c(columns, find_all_compatible_functions(return_fields=TRUE, quiet=TRUE)))
        }
        print(paste("CSV Loading certain columns from file with",auto_column_mode,"strategy:",paste(columns,collapse=", ")))
        pkg.globals$data <- pkg.globals$data %>% select(columns)
        gc()
      }
      return(TRUE)
    }
    stop("Could not find cached data on Server!")
  }
  if (is.null(folder)){
    folder <- paste0(study,"-cached_data")
  }
  folder_path <- paste(getwd(), folder, sep = "/")
  mdf <- googlesheets4::read_sheet(pkg.globals$matrix_id)
  pkg.globals$matrix <- mdf %>% 
    select(all_of(c("Construct",tolower(study)))) %>%
    rename(function_name=2) %>%
    filter(!is.na(function_name))
  study_name_raw <- colnames(mdf)[which(colnames(mdf)=="Construct")+1:length(colnames(mdf))]
  pkg.globals$study_names <- str_replace_all(str_to_lower(study_name_raw[!is.na(study_name_raw)])," ","_")
  pkg.globals$full_matrix <- mdf
  fdf <- googlesheets4::read_sheet(pkg.globals$function_sheet_id)
  set_function_sheet(fdf, study)
  if(!is.null(columns)){
    if(!is.null(columns)){
      if(auto_column_mode=="defined" | auto_column_mode=="detect"){
        columns <- unique(c(find_all_fields(),columns))
      }
    }
  }
  path <- paste(getwd(), folder, paste(study,'_data.feather', sep=''), sep = "/")
  if(file.exists(path)){
    if(!is.null(date)){
      print(paste("deleting Feather cache to get data from specific date:", path))
      file.remove(path)
    }else{
      print(paste("reading Feather - delete to cache fresh data from:", path))
      if(!is.null(columns)){
        meta <- feather::feather_metadata(path)
        pkg.globals$columns <- names(meta$types)
        if(auto_column_mode=="detect"){
          columns <- unique(c(columns, find_all_compatible_functions(return_fields=TRUE, quiet=TRUE)))
        }
        print(paste("Loading certain columns from file with",auto_column_mode,"strategy:",paste(columns,collapse=", ")))
        pkg.globals$data <- feather::read_feather(path, columns = columns)
      } else{
        pkg.globals$data <- feather::read_feather(path)
        pkg.globals$columns <- colnames(pkg.globals$data)
      }
      pkg.globals$path <- path
      return(TRUE)
    }
  }
  path <- paste(getwd(), folder, paste(study,'_data.csv', sep=''), sep = "/")
  if(file.exists(path)){
    if(!is.null(date)){
      print(paste("deleting CSV cache to get data from specific date:", path))
      file.remove(path)
    }else{
      print(paste("reading CSV - delete to cache fresh data from:", path))
      pkg.globals$data <- readr::read_csv(path, col_types = cols(.default = col_character()))
      pkg.globals$path <- path
      pkg.globals$columns <- colnames(pkg.globals$data)
      if(!is.null(columns)){
        if(auto_column_mode=="detect"){
          columns <- unique(c(columns, find_all_compatible_functions(return_fields=TRUE, quiet=TRUE)))
        }
        print(paste("CSV Loading certain columns from file with",auto_column_mode,"strategy:",paste(columns,collapse=", ")))
        pkg.globals$data <- pkg.globals$data %>% select(columns)
        gc()
      }
      return(TRUE)
    }
  }
  if(!dir.exists(folder_path)){
    dir.create(folder_path)
  }
  q <- list(study=study, filetype="chr")
  if (!is.null(date)){
    q['date']<-date
  }
  path <- paste(getwd(), folder, paste(study,'_data.feather', sep=''), sep = "/")
  r <- httr::GET("https://www.metrc-reports.org/cache", query=q,
                 httr::add_headers('token' = pkg.globals$redcap_key), httr::write_disk(path, overwrite=TRUE))
  rm(r)
  if(!is.null(columns)){
    meta <- tryCatch(feather::feather_metadata(path))
  } else{
    pkg.globals$data <- tryCatch(feather::read_feather(path))
  }
  if (inherits(pkg.globals$data,"error")){
    tryCatch(file.remove(path))
    q['type']<-NULL
    path <- paste(getwd(), folder, paste(study,'_data.csv', sep=''), sep = "/")
    r <- httr::GET("https://www.metrc-reports.org/cache", query=q,
                   httr::add_headers('token' = pkg.globals$redcap_key), httr::write_disk(path, overwrite=TRUE))
    rm(r)
    pkg.globals$data <-readr::read_csv(path, col_types = cols(.default = col_character()))
    pkg.globals$path <- path
    pkg.globals$columns <- colnames(pkg.globals$data)
    return(TRUE)
  }
  if(!is.null(columns)){
    pkg.globals$columns <- names(meta$types)
    if(auto_column_mode=="detect"){
      columns <- unique(c(columns, find_all_compatible_functions(return_fields=TRUE, quiet=TRUE)))
    }
    print(paste("Loading certain columns from file with",auto_column_mode,"strategy:",paste(columns,collapse=", ")))
    pkg.globals$data <- feather::read_feather(path, columns = columns)
  } else{
    pkg.globals$columns <- colnames(pkg.globals$data)
  }
  pkg.globals$path <- path
  return(TRUE)
}



#' Set Data Custom
#' 
#' @description This function loads data directly from an existing tibble
#' 
#' @param data this is and object with an existing tibble in it or and existing tibble
#' @param study this is the name of the study recognized by the metrc-reports server
#' @param folder this locates the data
#' @param server this tells the package that it is within a server environment and to look for local files as data soruces
#' 
#' @import readr httr
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' set_data_custom()
#' }
set_data_custom <- function(data_bundle, study, folder, server=FALSE){
  pkg.globals$cache <- list()
  study<- tolower(study)
  pkg.globals$study <- str_replace_all(study," ","_")
  pkg.globals$server <- server
  if(is_tibble(data_bundle)){
    pkg.globals$data <- data_bundle
  } else{
    pkg.globals$data <- data_bundle$data
  }
  pkg.globals$columns <- colnames(pkg.globals$data)
  if(server){
    study_system = gsub(" ","\\ ",study, fixed=TRUE)
    main_folder_system <- paste(getwd(),study_system,sep="/")
    main_folder <- paste(getwd(),study,sep="/")
    folders = system(paste("ls",main_folder_system), intern = TRUE, ignore.stderr = TRUE)
    cds <- folders[str_detect(folders, "_chr_")]
    pkg.globals$folder <- folder
    print(paste("-------------------------------------------->   setting folder",folder))
    mdf <- read_csv(paste(getwd(), folder, "METRC_CONSTRUCT_MATRIX.csv", sep = "/"))
    pkg.globals$matrix <- mdf %>% 
      select(all_of(c("Construct",tolower(study)))) %>%
      rename(function_name=2) %>%
      filter(!is.na(function_name))
    study_name_raw <- colnames(mdf)[which(colnames(mdf)=="Construct")+1:length(colnames(mdf))]
    pkg.globals$study_names <- str_replace_all(str_to_lower(study_name_raw[!is.na(study_name_raw)])," ","_")
    pkg.globals$full_matrix <- mdf
    fdf <- read_csv(paste(getwd(), folder, "Analytic Function Sheet.csv", sep = "/"), col_types = cols(.default = "c"))
    set_function_sheet(fdf, study)
  } else{
    pkg.globals$matrix <- googlesheets4::read_sheet(pkg.globals$matrix_id) %>% 
      select(all_of(c("Construct",tolower(study)))) %>%
      rename(function_name=2) %>%
      filter(!is.na(function_name))
    fdf <- googlesheets4::read_sheet(pkg.globals$function_sheet_id)
    set_function_sheet(fdf, study)
  }
  return(TRUE)
}


#' Set Function Sheet
#'
#'
#' @description This function loads the function sheet from a tibble
#'
#' @param fdf this is and object with an existing tibble in it
#' @param study this is the name of the study recognized by the metrc-reports server
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' set_function_sheet()
#' }
set_function_sheet <- function(fdf, study){
  target_study_name <- str_to_lower(str_replace_all(study,"_"," "))
  fxs <- ls("package:AnalyticCodebase")
  fdf <- fdf %>%
    filter(!is.na(`Function Name`) & (Status=="Deployed" | Status=="Deprecated") & `Function Name` %in% fxs)
  pkg.globals$full_function_sheet <- fdf
  fdf_targets <- c(paste(target_study_name,"Requested"), paste(target_study_name,"Status"))
  if (sum(fdf_targets %in% colnames(fdf))==length(fdf_targets)){
    temp_fdf <- fdf
    temp_fdf["requested"] <- temp_fdf[paste(target_study_name,"Requested")]
    pkg.globals$function_sheet <- temp_fdf %>%
      filter(requested=="Requested")
  } else{
    pkg.globals$function_sheet <- NULL
    warning("This study is using the legacy system.")
  }
}


#' Get Data Devleoper
#' 
#' @description provided you have already called set_data(TODO_STUDY, TODO_REDCAPKEY),
#' but remember set_data should NEVER be called inside a constructor function,
#' than this function can be used inside of constructor functions while they are in development,
#' but to improve performance after development get_data should be used with needed columns specified
#'
#' @import readr httr
#'
#' @return dataframe of all Redcap data
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_developer()
#' }
get_data_developer<-function(){
  return(pkg.globals$data)
}


#' Get Data
#' 
#' @description provided you have already called set_data(TODO_STUDY, TODO_REDCAPKEY),
#' but remember set_data should NEVER be called inside a constructor function,
#' than this function can be used inside of constructor functions by specifing the columns
#' you would like from the full dataset
#' 
#' @param selected_columns this is a vector of characters that can use to specifing desired columns 
#'
#' @import readr httr
#'
#' @return dataframe of some Redcap data
#' @export
#'
#' @examples
#' \dontrun{
#' get_data()
#' }
get_data<-function(selected_columns){
  return(pkg.globals$data %>% select(all_of(selected_columns)))
}




#' Download Custom Data
#' 
#' @description You can use this function to get arbitrary data from METRC studies
#' 
#' @param study The study name
#' @param token The redcap api token
#' @param path This is a default parameter that will auto name if given a directory 
#' path or will save to an arbitrary file if given a full path or will save to working directory
#' @param type This is a default parameter that will specify the type of file to download
#' it defaults to 'csv' but you can also request: a preparsed .rds with 'rds' an all character .rds with 'cds'
#' or a preparsed .feather with 'reg' an all character .feather with 'chr'
#' @param date This is a default parameter that you don't need to specify,
#' but if you do it will try to collect data from that date if its available.
#'
#' @import readr httr
#'
#' @return dataframe of Redcap Data
#' @export
#'
#' @examples
#' \dontrun{
#' download_custom_data()
#' }
download_custom_data<-function(study, token, path=NULL, type="csv", date=NULL){
  if(!type %in% c('csv','rds','cds','reg','chr')){
    stop("The type you requested is not available")
  }
  query = list(study=study)
  name <- paste(query,collapse = "_")
  if(!is.null(date)){
    query['date']<-date
    if(!str_detect(date,"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")){
      stop("Date must be a string formatted 0000-00-00")
    }
  }
  constructed_path <- FALSE
  if(is.null(path)){
    constructed_path <- TRUE
    path <- getwd()
    if(type=="csv"){
      path <- paste0(path,"/",name,".csv")
    }
    if(type=="rds" | type=="cds"){
      path <- paste0(path,"/",name,".rds")
    }
    if(type=="reg" | type=="chr"){
      path <- paste0(path,"/",name,".feather")
    }
  } else{
    if(!str_detect(path,".")){
      constructed_path <- TRUE
      path <- str_replace_all(paste0(path,"/",name),"//","/")
    }
    ext <- str_extract(path,"\\.[^\\.]+$")
    if(type=="csv" & ext != ".csv"){
      stop("Type 'csv' has to have path ending in .csv")
    }
    if((type=="rds" | type=="cds") & ext != ".rds"){
      stop("Type 'rds' or 'cds' has to have path ending in .rds")
    }
    if((type=="reg" | type=="chr")  & ext != ".feather"){
      stop("Type 'reg' or 'cgr' has to have path ending in .feather")
    } 
  }
  query['filetype'] <- type
  d <- httr::GET("https://www.metrc-reports.org/cache", query=query, 
                 httr::add_headers('token' = token), httr::write_disk(path, overwrite=TRUE))[c("headers","status_code")]
  
  if(d$status_code!=200){
    print(d)
    stop("Server could not fufill your request: if you are asking for a specific date or type it may be missing or check your access token by removing any date request and type")
  }
  creation <- d$headers['creation-time']
  if(constructed_path){
    old_path <- path
    path <- str_replace(old_path,"\\.",paste0("__",str_replace(creation," (..):(..):", "_\\1h\\2m"),"s."))
    file.rename(old_path,path)
  }
  
  if(type=="csv"){
    return(readr::read_csv(path, col_types = cols(.default = col_character())))
  }
  if(type=="rds" | type=="cds"){
    return(readr::read_rds(path))
  }
  if(type=="reg" | type=="chr"){
    return(feather::read_feather(path))
  }
  stop("BAD TYPE")
}


#' Get Associated Data
#' 
#' @description Downloads sheets from study folder on Google Drive by name
#' 
#' @param sheet_name the name of the Google Sheet
#' @param col_types set the col_types of the Google Sheet 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' get_associated_data()
#' }
get_associated_data <- function(sheet_name, col_types=NULL){
  require_study()
  if(!sheet_name %in% pkg.globals$associated_data_calls){
    pkg.globals$associated_data_calls <- c(pkg.globals$associated_data_calls, sheet_name)
  }
  if(pkg.globals$server){
    path <- paste(getwd(), pkg.globals$folder, paste0(sheet_name,".csv"), sep = "/")
    if(sheet_name %in% pkg.globals$anonimized_associated){
      if(!is.null(col_types)){
        return(read_csv(path, col_types = col_types))
      }
      return(read_csv(path))
    }
    if(!is.null(col_types)){
      out <- anonymize_redcap_data(df=read_csv(path, col_types = col_types))
    } else{
      out <- anonymize_redcap_data(df=read_csv(path))
    }
    write_csv(out, path)
    pkg.globals$anonimized_associated <- c(pkg.globals$anonimized_associated, sheet_name)
    return(out)
  } else{
    td <- googledrive::shared_drive_find()
    sid <- googledrive::drive_find(shared_drive = googledrive::as_id(td$id[td$name=="METRC"]), pattern = "Study Data", trashed=FALSE, type="folder")
    s_contents <- googledrive::drive_ls(googledrive::as_id(sid))
    fid <- s_contents %>% filter(tolower(name)==str_replace(tolower(pkg.globals$study),"_"," ")) %>% pull(id)
    contents <- googledrive::drive_ls(googledrive::as_id(fid))
    if (length(fid)!=1){
      stop("Could not find study folder in googledrive")
    }
    resource <- contents %>% 
      filter(name==sheet_name) %>% 
      pull(drive_resource)
    type <- resource[[1]]$mimeType
    if (type!="application/vnd.google-apps.spreadsheet"&type!="text/csv"){
      stop("specified file not a GoogleSheet not a .csv file")
    }
    if(!is.null(col_types)){
      return(anonymize_redcap_data(df=googlesheets4::read_sheet(contents %>% filter(name==sheet_name) %>% pull(id), col_types = col_types)))
    }
    return(anonymize_redcap_data(df=googlesheets4::read_sheet(contents %>% filter(name==sheet_name) %>% pull(id))))
  }
}


#' Get Associated Data Folder
#' 
#' @description Downloads all sheets from study folder on Google Drive by subdirectory name
#' 
#' @param folder_name the name of the Google Drive Folder
#' @param col_types et the col_types of every Google Sheet in specified folder
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' get_associated_data_folder()
#' }
get_associated_data_folder <- function(folder_name, col_types = NULL){
  require_study()
  
  if(pkg.globals$server){
    path <- file.path(getwd(), pkg.globals$folder, folder_name)
    files <- list.dirs(path, full.names = TRUE)
    output <- list()
    for (individual in files) {
      if(individual %in% pkg.globals$anonimized_associated){
        if(!is.null(col_types)){
          out <- read_csv(path, col_types = col_types)
        } else {
          out <- read_csv(path)
        }
      } else {
        if (!is.null(col_types)) {
          out <- anonymize_redcap_data(df=read_csv(path, col_types = col_types))
        } else {
          out <- anonymize_redcap_data(df=read_csv(path))
        }
        pkg.globals$anonimized_associated <- c(pkg.globals$anonimized_associated, individual)
        write_csv(out, path)
      }
      output[[basename(str_remove(individual, '.csv$'))]] <- out    
    }
    return(out)
  } else{
    td <- googledrive::shared_drive_find()
    sid <- googledrive::drive_find(shared_drive = googledrive::as_id(td$id[td$name=="METRC"]), pattern = "Study Data", trashed=FALSE, type="folder")
    s_contents <- googledrive::drive_ls(googledrive::as_id(sid))
    fid <- s_contents %>% filter(tolower(name)==str_replace(tolower(pkg.globals$study),"_"," ")) %>% pull(id)
    master_contents <- googledrive::drive_ls(googledrive::as_id(fid))
    sub_id <- master_contents %>% filter(name == folder_name) %>% pull(id)
    if (length(sub_id)==0){
      stop("Could not find given folder in study data folder")
    }
    sub_dir_contents <- googledrive::drive_ls(googledrive::as_id(sub_id))
    output <- list()
    for(number in 1:nrow(sub_dir_contents)) {
      sheet_name <- pull(sub_dir_contents, name)[number]
      sheet_id <- pull(sub_dir_contents, id)[number]
      if (sub_dir_contents[number,]$drive_resource[[1]]$mimeType=="application/vnd.google-apps.spreadsheet") {
        if (!is.null(col_types)) {
          sheet_data <- googlesheets4::read_sheet(sheet_id, col_types = col_types)
        } else {
          sheet_data <- googlesheets4::read_sheet(sheet_id)
        }
      } 
      if (sub_dir_contents[number,]$drive_resource[[1]]$mimeType=="text/csv") {
        temppath <- paste(sheet_id, '.csv')
        googledrive::drive_download(googledrive::as_id(sheet_id), path = temppath)
        if (!is.null(col_types)) {
          sheet_data <- read_csv(temppath, col_types = col_types)
        } else {
          sheet_data <- read_csv(temppath)
        }
        file.remove(temppath)
      }
      sheet_name <- str_remove(sheet_name, '.csv$')
      output[[sheet_name]] <- sheet_data
    }
    
    return(output)
  }
}


#' write data to excel
#' 
#' @description converts csv at path to exlsx at same path
#' 
#' @param dir_path the directory to file to convert to xlsx
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' write_data_to_excel(name)
#' }
write_data_to_excel <- function(dir_path){
  res <- try(write_xlsx(read_csv(dir_path),str_replace(dir_path,".csv$",".xlsx")))
  if(inherits(res,"try-error")){
    fix_long_string <- function(s){
      ifelse(str_length(s)>= 32767,
             paste0(str_sub(s,end=32000),"... (excel version truncates this cell - see csv for all data)"),
             s)
    }
    write_xlsx(read_csv(dir_path) %>% rowwise() %>% mutate_if(is.character, fix_long_string),str_replace(dir_path,".csv$",".xlsx"))
  }
}


#' Get Redcap Code Book
#' 
#' @description Redcap keeps a code book mapping the fields from the full dataset 
#' to the forms and questions they come from. This function downloads that codebook
#' provided you have already called set_data(TODO_STUDY, TODO_REDCAPKEY)
#'
#' @import readr httr
#'
#' @return dataframe of Redcap Code Book
#' @export
#'
#' @examples
#' \dontrun{
#' get_codebook()
#' }
get_codebook<-function(){
  if(is.null(pkg.globals$codebook)){
    if(is.null(pkg.globals$redcap_key)){
      cb <- try(ReportingSystem::get_study_dict(ReportingFunctions::get_globals()$study_name,ReportingFunctions::get_globals()$folder))
      if(!is.null(pkg.globals$data)){
        if(inherits(cb,"try-error")){
          if (as.character(attr(cb, "condition")) == "Error in loadNamespace(x): there is no package called ‘ReportingSystem’\n") {
            path <- paste(pkg.globals$folder, "/", str_replace_all(tolower(pkg.globals$study), 
              "_", " "), "_dict.csv", sep = "")
            cb <- readr::read_csv(path)
          }
        }
        missing_fields <- colnames(pkg.globals$data)
        missing_fields <- missing_fields[!str_detect(missing_fields,"___")&!missing_fields %in% cb$field_name]
        added <- tibble(field_name=missing_fields, field_label="Not in Codebook - Auto Added", form_name="MISSING")
        for(fn in unique(cb$form_name)){
          added <- added %>%
            mutate(form_name = ifelse(str_detect(field_name, fn), fn, form_name)) %>%
            mutate(form_name = as.character(form_name))
        }
        cb <- bind_rows(cb, added)
      }
      if(!inherits(cb,"try-error")){
        pkg.globals$codebook <- cb
        return(cb)
      }
      stop("Can't get codebook without first calling set_data(TODO_STUDY, TODO_REDCAPKEY)")
    }
    if(pkg.globals$server){
      path <- paste(folder,'/', str_replace_all(tolower(pkg.globals$study),"_"," "),'_dict.csv', sep='')
      return(readr::read_csv(path))
    }
    r <- httr::POST("https://www.metrcdata.org/redcap/api/", 
                    body = list("token" = pkg.globals$redcap_key, "content"= "metadata", "format"= "csv", "returnFormat"= "json"))
    data_raw <- readr::read_csv(httr::content(r, "text", encoding = "ISO-8859-1"))
    if(!is.null(pkg.globals$data)){
      missing_fields <- colnames(pkg.globals$data)
      missing_fields <- missing_fields[!str_detect(missing_fields,"___")&!missing_fields %in% data_raw$field_name]
      added <- tibble(field_name=missing_fields, field_label=missing_fields, select_choices_or_calculations="Not in Codebook - Auto Added", form_name="MISSING")
      for(fn in unique(data_raw$form_name)){
        added <- added %>%
          mutate(form_name = ifelse(str_detect(field_name, fn), fn, form_name))
      }
      data_raw <- bind_rows(data_raw, added)
    }
    pkg.globals$codebook <- data_raw
    return(data_raw)
  } else{
    return(pkg.globals$codebook)
  }
}
