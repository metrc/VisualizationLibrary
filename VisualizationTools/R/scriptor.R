

#' Function to Code Block
#' 
#' @description makes a block of code
#' 
#' @param names specific study function
#' @param data_name name of full redcap dataframe
#' @param analytic_name name of analytic dataset table
#' @param codebook pre downloaded data dictionary
#' @param width width to truncate comments
#'
#'
#' @return code block
#' @export
#'
#' @examples
#' \dontrun{
#' function_to_codeblock()
#' }
function_to_codeblock<-function(name, data_name="redcap_export", analytic_name="analytic_dataset", codebook=NULL, width=100, first=FALSE){
  if(name %in% names(pkg.globals$auto_cache$constructs_list)){
    return(list(code=paste("     ",c(str_replace_all(unlist(strsplit(do.call(pkg.globals$auto_cache$function_names[[name]],list(mode="code")),"\n")),"AUTOMATIC_DATA",data_name),
                                        paste(analytic_name, " <- left_join(",analytic_name,", df, by='study_id')"))),
                fields=do.call(pkg.globals$auto_cache$function_names[[name]],list(mode="fields"))))
  }
  ignore_fields <- c("study_id", "redcap_event_name", "redcap_repeat_instance", "redcap_repeat_instrument")
  if(is.null(codebook)){
    codebook <- get_codebook()
  }
  correct_cols <- colnames(get_construct_output(name))
  code <- unlist(strsplit(deparse(get_construct(name)),"\n"))
  code <- code[4:length(code)-1]
  output_code <- vector()
  expecting <- FALSE
  last <- ""
  all_fields <- vector()
  for(line in code){
    if(str_detect(line,"get_associated_data\\(")){
      line <- str_replace(line, "get_associated_data\\('([^']+)'\\)","\\1")
      line <- str_replace(line, "get_associated_data\\(\"([^']+)\"\\)","\\1")
    }
    if(str_detect(line,"get_construct_outputs\\(")){
      line <- str_replace(line, "get_construct_outputs\\(", paste(analytic_name,"%>%","select(study_id, "))
    }
    if(last!=""){
      line <- str_replace_all(paste(last, line, sep=","),",,|, ,|, +,",",")
      last <- ""
    }
    gap <- replace_na(str_extract(line,"^[\\s|\\t]+"),"")
    if(str_detect(line, "get_data\\(")){
      expecting <- TRUE
      if(!str_detect(line, "get_data\\(c\\([^\\)]+\\)\\)")){
        last <- line
        next
      } else{
        last <- ""
        expecting <- FALSE
      }
      fields <- unlist(strsplit(str_remove_all(line, ".+\\<\\-|get_data\\(c\\(|\\)\\).+|\\)\\)|\\s|\\t|\"|\\'"),","))
      all_fields <- c(all_fields,fields)
      line <- str_replace(line, "get_data\\(c\\([^\\)]+\\)\\)", paste0(data_name," %>% select(",paste(fields,collapse=", "),")"))
      fields <- unique(str_remove(fields[!fields %in% ignore_fields],"___.+$|___.$"))
      for(field in fields){
        label <- codebook[codebook$field_name==field,]$field_label
        label <- str_replace_all(label, "\n", " ")
        choices <- codebook[codebook$field_name==field,]$select_choices_or_calculations
        choices <- str_replace_all(choices, "<.*>","")
        choices <- str_replace_all(str_wrap(choices, width=width),"\n","\n# ")
        if(!is_empty(label)){
          output_code <- c(output_code, paste0(gap,"# Selecting field ",field," - ",label,": ",choices))
        }
      }
      new_line <- str_replace_all(line,'get_construct_output\\(["|\']([^["|\']]+)["|\']\\)',paste0(analytic_name,"%>% select(study_id, \\1)"))
      if(new_line != line){
        output_code <- c(output_code, paste0(gap,"# NOTE: We reuse the ", str_match(line,'get_construct_output\\(["|\']([^["|\']]+)["|\']\\)')[2]," column for calculating this column"))
        args <- unlist(strsplit(str_remove_all(new_line," |\t"),"<-"))
        if(length(args)==2){
          if(args[1]!=args[2]){
            output_code <- c(output_code, new_line)
          }
        } else{
          output_code <- c(output_code, new_line)
        }
      } else{
        output_code <- c(output_code, new_line)
      }
    }else if(str_detect(line, "return\\(")){
      line <- str_match(line, "return\\((.+)\\)$|return\\((.+)\\)[^\\)]$")[2]
      output_code <- c(output_code, paste0(gap,name," <- ",line))
      if(!study_uses_legacy()){
        output_code <- c(output_code, paste0(gap,"colnames(",name,") <- c('study_id','",correct_cols[2],"')"))
      }
      if(first){
        line <- paste0(gap, analytic_name, " <- ",name) 
      } else{
        line <- paste0(gap, analytic_name, " <- left_join(",analytic_name,", ",name,", by='study_id')") 
      }
      new_line <- str_replace_all(line,'get_construct_output\\(["|\']([^["|\']]+)["|\']\\)',paste0(analytic_name,"%>% select(study_id, \\1)"))
      if(new_line != line){
        output_code <- c(output_code, paste0(gap,"# NOTE: We reuse the ", str_match(line,'get_construct_output\\(["|\']([^["|\']]+)["|\']\\)')[2]," column for calculating this column"))
        args <- unlist(strsplit(str_remove_all(new_line," |\t"),"<-"))
        if(length(args)==2){
          if(args[1]!=args[2]){
            output_code <- c(output_code, new_line)
          }
        } else{
          output_code <- c(output_code, new_line)
        }
      } else{
        output_code <- c(output_code, new_line)
      }    
    } else{
        new_line <- str_replace_all(line,'get_construct_output\\(["|\']([^["|\']]+)["|\']\\)',paste0(analytic_name,"%>% select(study_id, \\1)"))
        if(new_line != line){
          output_code <- c(output_code, paste0(gap,"# NOTE: We reuse the ", str_match(line,'get_construct_output\\(["|\']([^["|\']]+)["|\']\\)')[2]," column for calculating this column"))
          args <- unlist(strsplit(str_remove_all(new_line," |\t"),"<-"))
          if(length(args)==2){
            if(args[1]!=args[2]){
              output_code <- c(output_code, new_line)
            }
          } else{
            output_code <- c(output_code, new_line)
          }
        } else{
          output_code <- c(output_code, new_line)
        }    
      }
  }
  if(expecting){
    stop("Failed to parse multiline get_data call")
  }
  return(list(code=output_code, fields=all_fields))
}


#' Functions to Script
#' 
#' @description makes a block of code
#' 
#' @param names specific study function
#' @param data_name name of full redcap dataframe
#' @param analytic_name name of analytic dataset table
#' @param width width to truncate comments
#' @param return_lines activates return of lines defaults to true
#'
#' @return code block
#' @export
#'
#' @examples
#' \dontrun{
#' function_to_codeblock()
#' }
function_to_script<-function(names, data_name="redcap_export", analytic_name="analytic_dataset", width=100, return_lines=TRUE, folder=NULL, display_order=NULL){
  require_study()
  codebook <- get_codebook()
  if(is.null(display_order)){
    display_order <- names
  }
  lines <- vector()
  first <- TRUE
  fields <- vector()
  already_run <-  vector()
  for(name in names){
    already <- pkg.globals$auto_cache$function_names[[name]]
    if(length(already)==1){
      if(already %in% already_run){
        next
      }
      already_run <-  c(already_run, pkg.globals$auto_cache$function_names[[name]])
    }
    output <- function_to_codeblock(name, data_name=data_name, analytic_name=analytic_name, width=width, codebook = codebook, first=first)
    results <- output$code
    fields <- c(fields, output$field)
    first <- FALSE
    min_gap <- "                            "
    for(line in results){
      gap <- replace_na(str_extract(line,"^[\\s|\\t]+"),"")
      if(str_length(gap)<str_length(min_gap)){
        min_gap <- gap
      }
    }
    str <- get_construct_function_name(name)
    if(name %in% names(pkg.globals$auto_cache$constructs_list)){
      str <- pkg.globals$auto_cache$function_names[[name]]
      full <- vector()
      for(n in names(pkg.globals$auto_cache$function_names)){
        if(pkg.globals$auto_cache$function_names[[n]]==str){
          full <- c(full,n)
        }
      }
      name <- paste(n, collapse=", ")
    }
    comment_lines <- gsub("_\b|^     ","",capture.output(tools:::Rd2txt(utils:::.getHelpFile(as.character(help(str))))))
    comment_lines <- comment_lines[comment_lines!=""]
    comment_str <- paste(comment_lines, collapse="\n")
    descritiption <- str_match(str_replace_all(comment_str,"\n", " "), "Description:(.+)Usage")[2]
    results <- c(paste0(min_gap,"### ",name," column ###"),
                 paste0(min_gap,"###"),
                 paste0(min_gap,"### Description: ",str_replace_all(str_wrap(descritiption, width=width),"\n","\n### ")),
                 min_gap,
                 min_gap,
                 results,
                 min_gap,
                 min_gap)
    lines <- c(lines, results)
  }
  min_gap <- "                            "
  for(line in lines){
    gap <- replace_na(str_extract(line,"^[\\s|\\t]+"),"")
    if(str_length(gap)<str_length(min_gap)){
      min_gap <- gap
    }
  }
  output_lines <- vector()
  for(line in lines){
    output_lines <- c(output_lines, str_remove(line,paste0("^",min_gap)))
  }
  ignore_fields <- c("study_id", "redcap_event_name", "redcap_repeat_instance", "redcap_repeat_instrument")
  new_data_path <- paste0(getwd(),'/',folder,'/',pkg.globals$study,"_raw_dataset.csv")
  print(unique(fields[!fields %in% ignore_fields & !is.na(fields)]))
  fields <- c(ignore_fields, unique(fields[!fields %in% ignore_fields & !is.na(fields)]))
  fields <- fields[fields %in% colnames(pkg.globals$data)]
  write_csv(get_data(fields), new_data_path)
  top_lines <- c(paste("#############", pkg.globals$study, "Analytic Dataset Script", "#############"), "#",
                 paste("# Script constructed on",Sys.Date()),"#","# TODO update path to data",
                 paste0("path <- '",new_data_path,"'"), "library(tidyverse)",
                 paste(data_name,"<-","read_csv(path, col_types = cols(.default = col_character()))"),"","")
  if(!is_empty(pkg.globals$associated_data_calls)){
    path_associated <- paste0(getwd(),'/',folder,'/',pkg.globals$associated_data_calls,".csv")
    top_lines <- c(top_lines, paste0(pkg.globals$associated_data_calls," <- ","read_csv('",path_associated,"', col_types = cols(.default = col_character()))"))
  }
  output_lines <- c(top_lines, output_lines, "# RESULTS", 
                    paste(analytic_name, "<-",analytic_name," %>%\n   select(study_id,",paste(display_order,collapse=","),")"), 
                    paste0("write_csv(",analytic_name,", 'results.csv')"))
  print("test")
  if(is.null(folder)){
    write_lines(output_lines,paste0(pkg.globals$study,"_analytic_codebase.R"))
  } else{
    write_lines(output_lines,paste0(getwd(),'/',folder,'/',pkg.globals$study,"_analytic_codebase.R"))
  }
  if(return_lines){
    return(output_lines) 
  }
}




