#' Update Function Sheet Metadata
#'
#' @description updates the metadata for the function sheet by pulling the latest version from google sheets
#' then checking to see if the "Function Name" is in the package and if not setting the "Status" to "Error" and the
#' "System Message" to "Function not found in package". Then upload these changes to google sheets.
#'
#' @param retries number of times to retry the update if it fails
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' update_function_sheet_metadata()
#' }
update_function_sheet_metadata <- function(retries=5){
  if (pkg.globals$server) {
    function_sheet = read_csv("Analytic Function Sheet.csv", col_types = cols(.default = "c"))
  } else{
    # Get the initial modifiedTime
    file_info <- googledrive::drive_get(googledrive::as_id(pkg.globals$function_sheet_id))
    initial_timestamp <- file_info$drive_resource[[1]]$modifiedTime

    test = c("Deployed","Deprecated")

    # Read the sheet
    function_sheet <- googlesheets4::read_sheet(pkg.globals$function_sheet_id)
  }
  inconsistent_items <- tibble(`Function Name`="a", inconsistent_output="a") %>%
    slice(0)
  for (item in unique(function_sheet[["Construct Item"]])){
    items <- function_sheet %>%
      filter(`Construct Item`==item) %>%
      unite("Standard", all_of(c("Construct Scale/Unit","Construct Data Type",
                                 "Long File Required Columns","Long File Required Column Types")),
            sep=", ", remove=FALSE) %>%
      select(`Function Name`, Standard)
    if(length(unique(items$Standard))>1){
      # Create table of frequencies, preserving the original order
      freq_table <- table(factor(items$Standard, levels = unique(items$Standard)))
      # Find the most common item, breaking ties by order
      most_common <- as.character(names(freq_table)[which.max(freq_table)])
      inconsistent <- items %>%
        filter(Standard!=most_common) %>%
        select(`Function Name`) %>%
        mutate(`inconsistent_output`= paste(
          "The output of this function doesn't match other functions with the same construct item which have:",
          most_common))
      inconsistent_items <- inconsistent_items %>%
          bind_rows(inconsistent)
    }
  }

  updated_function_sheet <- left_join(function_sheet, inconsistent_items) %>%
    mutate(`System Message`= ifelse(!is.na(inconsistent_output) & inconsistent_output != "",
                                    inconsistent_output, "")) %>%
    select(-inconsistent_output) %>%
    mutate(`System Message`=ifelse(!`Function Name` %in% ls("package:AnalyticCodebase") & Status %in% c(test,"Errors"),
                                   ifelse(`System Message`=="",
                                          "Function not found in Analytic Codebase!",
                                        paste("Function not found in Analytic Codebase!", `System Message`)),
                                   "")) %>%
    mutate(Status=ifelse(is.na(`System Message`) | `System Message` == "",
                         ifelse(Status=="Errors", "Deployed", Status), "Errors")) %>%
    rowwise() %>%
    mutate(`Construct Item Dependecies` = ifelse(Status %in% test,
                                                 ifelse(`System Message`=="",
                                                 paste(get_function_dependencies(`Function Name`),collapse=", "),""),
                                                 "")) %>%
    rowwise() %>%
    mutate(`Redcap Field Dependencies` =  ifelse(Status %in% test,
                                                 ifelse(`System Message`=="",
                                                 paste(get_function_fields(`Function Name`, recursive=F),collapse=", "),""),
                                              "")) %>%
    ungroup() %>%
    mutate(`Function Dependencies` = ifelse(Status %in% test,
                                            ifelse(!is.na(`Construct Item Dependecies`) &
                                                    `Construct Item Dependecies`!="" &
                                                     !is.na(`Redcap Field Dependencies`) &
                                                      `Redcap Field Dependencies`!="",
                                            "Mixed",
                                            ifelse(!is.na(`Construct Item Dependecies`) &
                                                     `Construct Item Dependecies`!="", "Items",
                                                   ifelse(!is.na(`Redcap Field Dependencies`) &
                                                     `Redcap Field Dependencies`!="", "Redcap", "None"))),
                                            "")) %>%
    mutate(`Uses Redcap Event Dependencies` = ifelse(Status %in% test,
                                                     str_detect(`Redcap Field Dependencies`,"redcap_event_name"),
                                                     "")) %>%
    mutate(`Uses Redcap Instrument Dependencies` = ifelse(Status %in% test,
                                                          str_detect(`Redcap Field Dependencies`,"redcap_repeat_instrument"),
                                                          "")) %>%
    mutate(`Uses Redcap Instance Dependencies` = ifelse(Status %in% test,
                                                        str_detect(`Redcap Field Dependencies`,"redcap_repeat_instance"),
                                                        ""))
  if (pkg.globals$server) {
    write_csv(updated_function_sheet, "Analytic Function Sheet.csv")
  }

  # updated columns
  updated_cols <- c("Construct Item Dependecies", "Redcap Field Dependencies",
                    "Function Dependencies", "Uses Redcap Event Dependencies", "Uses Redcap Instrument Dependencies",
                    "Uses Redcap Instance Dependencies")
  # check if the updated columns are all next to each other by getting the position of each column first
  updated_cols_pos <- match(updated_cols, colnames(updated_function_sheet))
  # now sort them and see if the difference between the biggest and smallest is equal to the length of the vector
  # if it is, then they are all next to each other
  if (unique(diff(sort(updated_cols_pos)))[1] != 1 & length(unique(diff(sort(updated_cols_pos)))) != 1) {
    stop("The updated columns are not next to each other")
  }

  # Check the modifiedTime again before writing
  file_info <- googledrive::drive_get(googledrive::as_id(pkg.globals$function_sheet_id))
  current_timestamp <- file_info$drive_resource[[1]]$modifiedTime

  if (initial_timestamp == current_timestamp) {
    system_range <- googlesheets4::cell_cols(match("System Message",colnames(updated_function_sheet)))
    # write the range of the system message column
    googlesheets4::range_write(pkg.globals$function_sheet_id, updated_function_sheet %>% select(`System Message`),
                               sheet = "Analytic Function Sheet", reformat = FALSE,
                               range = system_range)
    status_range <- googlesheets4::cell_cols(match("Status",colnames(updated_function_sheet)))
    # write the range of the status column
    googlesheets4::range_write(pkg.globals$function_sheet_id, updated_function_sheet %>% select(`Status`),
                               sheet = "Analytic Function Sheet", reformat = FALSE,
                               range = status_range)
    # write the range of the rest of the updated columns starting with the first updated column
    googlesheets4::range_write(pkg.globals$function_sheet_id, updated_function_sheet %>% select(all_of(updated_cols)),
                               sheet = "Analytic Function Sheet", reformat = FALSE,
                               range = googlesheets4::cell_cols(updated_cols_pos))

  } else {
    if (retries > 0) {
      update_function_sheet_metadata(retries=(retries - 1))
    } else{
      stop("The sheet has been updated by another process. Please try again.")
    }
  }
}



#' Update Function Sheet Study Status
#'
#' @description This function updates the study status of the function sheet for the current study
#'
#' @param retries The number of times to retry updating the function sheet if it is locked by another process
#'
#' @return This function does not return anything
#' @export
#'
#' @examples
#' \dontrun{
#' update_function_sheet_for_study()
#' }
update_function_sheet_for_study <- function(retries=5){
  require_study()
  if(study_uses_legacy()){
    stop("This function is not supported for legacy studies")
  }
  test <- test_study()
  test <- test %>%
    mutate(status_simple = paste0(documentation,code,output)) %>%
    mutate(study_status = ifelse(status_simple=="","Added",
                                 paste(ifelse(documentation != "",paste("DOCUMENTATION:",documentation),""),
                                        ifelse(code != "",paste("CODE:",code),""),
                                        ifelse(output != "",paste("OUTPUT:",output),"")))) %>%
    rowwise() %>%
    mutate("Function Name"= get_construct_function_name(name)) %>%
    ungroup() %>%
    select("Function Name","study_status")

  if (pkg.globals$server) {
    function_sheet = read_csv("Analytic Function Sheet.csv", col_types = cols(.default = "c"))
  } else{
    # Get the initial modifiedTime
    file_info <- googledrive::drive_get(googledrive::as_id(pkg.globals$function_sheet_id))
    initial_timestamp <- file_info$drive_resource[[1]]$modifiedTime

    # Read the sheet
    function_sheet <- googlesheets4::read_sheet(pkg.globals$function_sheet_id)
  }

  target_study_name <- str_to_lower(str_replace_all(pkg.globals$study,"_"," "))
  target <- paste(target_study_name,"Status")

  updated_function_sheet <- function_sheet %>%
    left_join(test) %>%
    # copy the study_status column to the target study column
    mutate(!!target := study_status) %>%
    # remove the study_status column
    select(-study_status)

  if (pkg.globals$server) {
    write_csv(updated_function_sheet, "Analytic Function Sheet.csv")
  }


  # Check the modifiedTime again before writing
  file_info <- googledrive::drive_get(googledrive::as_id(pkg.globals$function_sheet_id))
  current_timestamp <- file_info$drive_resource[[1]]$modifiedTime

  if (initial_timestamp == current_timestamp) {
    status_range <- googlesheets4::cell_cols(match(target,colnames(updated_function_sheet)))
    # write the range of the status column
    googlesheets4::range_write(pkg.globals$function_sheet_id, updated_function_sheet %>% select(all_of(target)),
                               sheet = "Analytic Function Sheet", reformat = FALSE,
                               range = status_range)
  } else {
    if (retries > 0) {
      update_function_sheet_for_study(retries=(retries - 1))
    } else{
      stop("The sheet has been updated by another process. Please try again.")
    }
  }
}