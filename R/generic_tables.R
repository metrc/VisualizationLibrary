#' Generic Long File Unzip
#'
#' @description 
#' Unzips a long file construct into one row per entry and visualizes count statistics 
#' for a target column within each group of a group column. Only enrolled participants 
#' are included in the calculations. Entries with no value in the target column are 
#' given the value "Missing."
#' 
#' Groups are shown in the order they first appear in the long file. Target values are 
#' sorted numerically when they are numbers, then alphabetically, with "Missing" kept at 
#' the bottom of each group.
#' 
#'
#' @param analytic This is the analytic data set 
#' @param construct The long file construct to unzip
#' @param column_names The names of the columns in the construct, in order (see the construct's UNZIPPING INSTRUCTIONS)
#' @param mode The type of statistics to calculate. Supports:
#' "n_percent", the count and percentage of each target value, with a "Missing" row for entries with no value.
#' Values are sorted numerically when they are numbers, then alphabetically, with "Missing" kept at the bottom.
#' "n_percent_of : 'value', 'value'", the same as "n_percent" but only shows the listed values, in the order listed.
#' The values must be written exactly as they appear in the long file, in single quotes and separated by commas.
#' Percentages are still out of every reading, missing included.
#' "mean_range", the mean (min-max) of the numeric target values, with an n (%) row for each text value.
#' "mean_sd", the same as "mean_range" but gives the mean (SD) of the numeric target values.
#' @param group_col One or two columns. The first column splits the table into sections, each with its own header. 
#' The optional second column names the items listed inside each section (for example c("event", "name"))
#' @param target_col One or two columns to calculate the statistics on. Two columns are calculated separately 
#' @param column_sep The character separating columns in the construct (see the construct's UNZIPPING INSTRUCTIONS). Defaults to "┘"
#' @param row_sep The character separating rows in the construct (see the construct's UNZIPPING INSTRUCTIONS).  Defaults to "¶"
#' @param return_df When TRUE, returns a data frame with one row per group and one column per target value instead of the html table. Defaults to FALSE.
#' @param filter_cols A vector of filter instructions based on the known column name, and the value you want all observations to have for that column. Defaults to NULL.
#' @param group_levels The groups to show and their order, using the exact names in the group column. 
#' Groups not listed are left out. Defaults to NULL, grouping is handled by tables natural order
#'
#' @return html table, or a data frame when return_df is TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' generic_long_file_unzip("Replace with Analytic Tibble", construct="peg_swm",
#'                         column_names=c("event", "name", "reading_side", "is_injured", "Radial", "Ulnar"),
#'                         group_col="event", target_col="Radial", filter_cols = c(is_injured = TRUE))
#' }
generic_long_file_unzip <- function(analytic, construct, column_names, group_col, target_col, 
                                    column_sep = "┘", row_sep = "¶", mode = "n_percent", return_df = FALSE, filter_cols = NULL, group_levels = NULL){
  
  #enrolled ids are an important part of knowing who to include in calculations
  enrolled_ids <- analytic %>%
    filter(enrolled %in% TRUE) %>%
    distinct(study_id)
  
  unzipped <- analytic %>%
    select(study_id, construct) %>%
    separate_rows(construct, sep = row_sep) %>%
    separate(construct, into = column_names, sep = column_sep)
  
  if(!is.null(filter_cols)){
    for(filter_col in names(filter_cols)){
      unzipped <- unzipped %>% filter(!!sym(filter_col) %in% filter_cols[[filter_col]])
    }
  }
  
  #n_percent_of is followed by the exact values to show, like "n_percent_of : '6.65', 'Unable to Perform'"
  if(str_starts(mode, "n_percent_of")){
    of_values <- mode %>%
      str_remove("n_percent_of") %>%
      str_remove(":") %>%
      str_split(",") %>%
      unlist() %>%
      str_trim() %>%
      str_remove_all("'")
    
    mode <- "n_percent_of"
  }
  
  #group_col[1] splits the table into sections (like event)
  #group_col[2] is optional and names the items listed inside each section (like the motion)
  #the columns get standard names here so each mode is only written once
  if(length(group_col) == 2){
    working <- unzipped %>%
      semi_join(enrolled_ids, by = "study_id") %>%
      select(study_id, section = all_of(group_col[1]), item = all_of(group_col[2]), all_of(target_col))
  } else {
    working <- unzipped %>%
      semi_join(enrolled_ids, by = "study_id") %>%
      select(study_id, section = all_of(group_col[1]), all_of(target_col)) %>%
      mutate(item = paste(target_col, collapse = " / "))
  }
  
  #each target column becomes its own set of rows, compare holds the name of the column it came from
  #with one target column, compare is the same on every row and every mode works as before
  working <- working %>%
    pivot_longer(all_of(target_col), names_to = "compare", values_to = "target") %>%
    filter(!is.na(section) & !is.na(item))
  
  #group levels choose which sections are shown and in what order,
  #otherwise sections are shown in the order they first appear in the long file
  if(!is.null(group_levels)){
    working <- working %>%
      filter(section %in% group_levels)
    
    group_order <- group_levels
  } else {
    group_order <- working %>%
      pull(section) %>%
      unique()
  }
  
  #every mode builds two data frames:
  #df_return (what return_df gives back) and
  #result for the renderer with the columns: group (section), item, compare (target column), temp (row label), 
  #value (cell text), sub (TRUE for indented rows)
  if(mode %in% c("n_percent", "n_percent_of")){
    #the denominator is every reading of that item in that section, missing included
    denoms <- working %>%
      group_by(section, item, compare) %>%
      summarize(denom = n(), missing = sum(is.na(target)), .groups = "drop")
    
    counts <- working %>%
      filter(!is.na(target)) %>%
      group_by(section, item, compare, target) %>%
      summarize(n = n(), .groups = "drop") %>%
      left_join(denoms, by = c("section", "item", "compare")) %>%
      mutate(result = format_count_percent(n, denom)) %>%
      select(section, item, compare, target, result)
    
    missing_rows <- denoms %>%
      mutate(target = "Missing") %>%
      mutate(result = format_count_percent(missing, denom)) %>%
      select(section, item, compare, target, result)
    
    if(mode == "n_percent_of"){
      #only the listed values are shown, in the order they were listed,
      #every item gets each of them with 0 (0%) where nobody has it
      custom_levels <- of_values
      
      combined <- denoms %>%
        select(section, item, compare) %>%
        crossing(target = of_values) %>%
        left_join(bind_rows(counts, missing_rows), by = c("section", "item", "compare", "target")) %>%
        mutate(result = replace_na(result, "0 (0%)"))
    } else {
      target_temps <- sort(unique(counts$target))
      numeric_temps <- suppressWarnings(as.numeric(target_temps))
      is_numeric <- !is.na(numeric_temps)
      custom_levels <- c(target_temps[is_numeric][order(numeric_temps[is_numeric])],
                         target_temps[!is_numeric], "Missing")
      
      #each item gets every value seen for that item in every target column, with 0 (0%) where nobody has it
      combined <- bind_rows(counts, missing_rows) %>%
        group_by(item) %>%
        complete(section, compare, target, fill = list(result = "0 (0%)")) %>%
        ungroup()
    }
    
    combined <- combined %>%
      arrange(factor(section, levels = group_order), item, factor(target, levels = custom_levels))
    
    df_return <- combined %>%
      pivot_wider(names_from = target, values_from = result) %>%
      select(section, item, compare, all_of(custom_levels))
    
    result <- combined %>%
      mutate(sub = length(group_col) == 2) %>%
      select(group = section, item, compare, temp = target, value = result, sub)
    
    #with an item column, each item gets a label row and its values sit indented under it
    if(length(group_col) == 2){
      item_rows <- combined %>%
        distinct(section, item, compare) %>%
        mutate(temp = item, value = "", sub = FALSE) %>%
        select(group = section, item, compare, temp, value, sub)
      
      result <- bind_rows(item_rows, result) %>%
        arrange(factor(group, levels = group_order), item, sub)
    }
    
  } else if(mode %in% c("mean_range", "mean_sd")){
    
    #the two modes only differ in how the numbers are summarized
    if(mode == "mean_sd"){
      mean_label <- "Mean (SD)"
    } else {
      mean_label <- "Mean (range)"
    }
    
    #everything comes in as text, values that convert to a number are numbers, the rest are text values
    readings <- working %>%
      filter(!is.na(target)) %>%
      mutate(number = suppressWarnings(as.numeric(target)))
    
    #numeric readings become the mean row for each item
    means <- readings %>%
      filter(!is.na(number)) %>%
      group_by(section, item, compare) %>%
      summarize(result = if(mode == "mean_sd") format_mean_sd(number) else format_mean_range(number), .groups = "drop") %>%
      mutate(row_type = "mean")
    
    #text readings (like "Unable to perform") become an n (%) row under each item
    texts <- readings %>%
      group_by(section, item, compare) %>%
      mutate(denom = n()) %>%
      ungroup() %>%
      filter(is.na(number)) %>%
      group_by(section, item, compare, target) %>%
      summarize(n = n(), denom = first(denom), .groups = "drop") %>%
      mutate(result = as.character(format_count_percent(n, denom))) %>%
      select(section, item, compare, row_type = target, result)
    
    #every item always gets a mean row, even if all of its readings are text
    combined <- bind_rows(means, texts) %>%
      complete(section, item, compare, row_type = c("mean", unique(texts$row_type))) %>%
      mutate(result = case_when(
        !is.na(result) ~ result,
        row_type == "mean" ~ "",
        TRUE ~ "0 (0%)"
      )) %>%
      arrange(factor(section, levels = group_order), item, row_type != "mean", row_type)
    
    df_return <- combined %>%
      mutate(row_type = if_else(row_type == "mean", mean_label, row_type)) %>%
      pivot_wider(names_from = row_type, values_from = result)
    
    result <- combined %>%
      mutate(temp = if_else(row_type == "mean", item, row_type)) %>%
      mutate(sub = row_type != "mean") %>%
      select(group = section, item, compare, temp, value = result, sub)
    
  } else {
    return(unzipped)
  }
  
  #restoring of original names
  if(return_df){
    df_return <- df_return %>%
      rename(!!group_col[1] := section)
    
    if(length(group_col) == 2){
      df_return <- df_return %>%
        rename(!!group_col[2] := item)
    } else {
      df_return <- df_return %>%
        select(-item)
    }
    
    if(length(target_col) == 1){
      df_return <- df_return %>%
        select(-compare)
    }
    
    return(df_return)
  }
  
  #general rendering of the mode's data frame
  #the header n is the number of readings in each section,
  #or the number of patients when items are listed inside each section
  if(length(group_col) == 2){
    group_totals <- working %>%
      group_by(section) %>%
      summarize(total = n_distinct(study_id), .groups = "drop")
  } else {
    group_totals <- working %>%
      count(section, name = "total")
  }
  
  #each target column becomes its own value column
  result <- result %>%
    pivot_wider(names_from = compare, values_from = value) %>%
    mutate(group = factor(group, levels = group_order))
  
  group_counts <- result %>%
    count(group)
  
  index_vec <- group_counts$n
  names(index_vec) <- paste0(as.character(group_counts$group), ' (n=',
                             group_totals$total[match(as.character(group_counts$group), group_totals$section)], ')')
  
  border_rows <- if(length(index_vec) > 1) c(1, cumsum(index_vec[1:(length(index_vec)-1)]) + 1) else c(1)
  
  sub_rows <- which(result$sub)
  
  #two target columns are headed by their names, one target column has no header
  if(length(target_col) == 2){
    header_names <- c('', target_col)
  } else {
    header_names <- c('', '')
  }
  
  vis <- result %>%
    select(temp, all_of(target_col)) %>%
    kable(format="html", align=c('l', rep('r', length(target_col))), col.names = header_names,
          table.attr = "style='width:100% !important;'") %>%
    add_indent(c(seq(nrow(result)))) %>%
    { if(length(sub_rows) > 0) add_indent(., sub_rows) else . } %>%
    row_spec(border_rows, extra_css = "border-top: 1px solid") %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left", escape = FALSE) %>%
    kable_styling("striped", full_width = T, position="left")
  
  return(vis)
}

#' Generic Characteristics
#'
#' @description 
#' Visualize basic count statistics for a number of constructs. Missing values are 
#' given the value "Missing."
#' 
#' For other relevant characteristics counting visualizations, please see: baseline_characteristics_percent, baseline_characteristics_percent_nm
#'
#' @param analytic This is the analytic data set 
#' @param constructs The constructs to run statistics from
#' @param names_vec The names of the constructs in the final visualization. Pass NA to attach a construct to the previous group's header without creating a new top border.
#' @param filter_cols The columns to filter the the data by (for totals and missing counts)
#' @param titlecase Changes construct values to Title Case
#' @param splits Splits the constructs if they are lists like "test_one,test_two" into two rows then counts them
#' @param subcategory_constructs This allows a characteristic to have a construct as a sub category, 
#' must be empty or specify a subcategory construct (or NA) for each construct (length of constructs == length of subcategory_constructs)
#' @param bottom_order_levels A vector of category names (e.g., "Missing", "Refused") to force to the bottom of the table, maintaining their order. Defaults to "Missing".
#' @param mean_sd A vector of construct names. If a construct is included here, it will be displayed as "Mean [SD]" with its calculated values, instead of categorical counts.
#' @param include_overall When TRUE and subcategory_constructs is used, an "All Sites" block computed over every subcategory together is shown before the per-subcategory blocks.
#' @param collapse_other_entries single flag or per-construct vector; TRUE collapses
#' free-text Other entries for that construct before counting - as a whole value for
#' an unsplit construct, and inside the list for a split one
#'
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' generic_characteristics("Replace with Analytic Tibble", constructs="stages", names_vec="Stages")
#' }
generic_characteristics <- function(analytic, constructs = c(), names_vec = c(), 
                                    filter_cols = c("enrolled"), titlecase = FALSE, splits=NULL,
                                    subcategory_constructs = c(), bottom_order_levels = c("Missing"),
                                    mean_sd = c(), include_overall = FALSE,
                                    collapse_other_entries = FALSE){
  
  out <- NULL
  index_vec <- c()
  sub_index_vec <- c()
  sub_bold_index_vec <- c()
  has_border <- c()
  
  if(is.null(splits)){
    splits <- rep(NA, length(constructs))
  } else{
    if(length(splits) == 1) {
      splits <- rep(splits, length(constructs))
    }
  }
  
  if(length(collapse_other_entries) == 1) {
    collapse_other_entries <- rep(collapse_other_entries, length(constructs))
  }
  for (coe_i in seq_along(constructs)) {
    # A construct absent from the data is left for the construct selection below,
    # which names the missing column in its error instead of a recycling failure.
    if (isTRUE(collapse_other_entries[coe_i]) && constructs[coe_i] %in% names(analytic)) {
      # An unsplit construct's Other free text may itself contain the split
      # character, so it is collapsed as a whole value; a split construct
      # collapses the Other terms inside its list.
      analytic[[constructs[coe_i]]] <- if (is.na(splits[coe_i])) {
        collapse_other(analytic[[constructs[coe_i]]])
      } else {
        collapse_other_multi(analytic[[constructs[coe_i]]])
      }
    }
  }
  
  if(is_empty(subcategory_constructs)){
    subcategory_constructs <- rep(NA, length(constructs))
  } else{
    if(length(subcategory_constructs) == 1) {
      subcategory_constructs <- rep(subcategory_constructs, length(constructs))
    }
  }
  
  for (i in seq(length(constructs))) {
    name_str <- names_vec[i]
    construct <- constructs[i]
    sub_construct <- subcategory_constructs[i]
    
    if (!is.null(filter_cols)){
      if(length(filter_cols) == 1) {
        inner_analytic <- analytic %>%
          filter(!!sym(filter_cols)) %>%
          select(study_id, all_of(c(constructs,subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]))
      } else {
        inner_analytic <- analytic %>%
          filter(!!sym(filter_cols[i])) %>%
          select(study_id, all_of(c(constructs,subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]))
      }
    }
    total <- nrow(inner_analytic)
    
    if (construct %in% mean_sd) {
      vec <- suppressWarnings(as.numeric(inner_analytic[[construct]]))
      inner <- tibble::tibble(temp = "Mean [SD]", percentage = format_mean_sd(vec), header = name_str)
      
      if (is.na(name_str) || name_str == "") {
        if (length(index_vec) > 0) {
          index_vec[length(index_vec)] <- index_vec[length(index_vec)] + 1
        } else {
          new <- 1; names(new) <- " "; index_vec <- c(index_vec, new); has_border <- c(has_border, FALSE)
        }
      } else {
        new <- 1; names(new) <- paste0(name_str, ' (n=', total, ')'); index_vec <- c(index_vec, new); has_border <- c(has_border, FALSE)
      }
      
      if (is.null(out)) out <- inner else out <- rbind(out, inner)
      next
    }
    
    inner <- inner_analytic %>%
      mutate(temp = !!sym(construct)) %>% 
      mutate(temp =  replace_na(as.character(temp), "Missing"))
    
    if(!is.na(sub_construct)){
      inner <- inner %>% 
        mutate(sub_temp = !!sym(sub_construct)) %>% 
        mutate(sub_temp =  replace_na(as.character(sub_temp), "Missing"))
    }
    
    inner_split <- splits[i]
    
    if(!is.na(inner_split)){
      inner <- inner %>% 
        separate_rows(temp,sep = inner_split)
    }
    
    non_bottom_temps <- sort(unique(inner$temp[!inner$temp %in% bottom_order_levels]))
    
    numeric_temps <- suppressWarnings(as.numeric(non_bottom_temps))
    is_numeric <- !is.na(numeric_temps)
    
    numeric_sort_list <- non_bottom_temps[is_numeric] %>% 
      as.numeric() %>% 
      sort() %>% 
      as.character()
    
    non_numeric_sort_list <- sort(non_bottom_temps[!is_numeric])
    
    custom_levels <- c(numeric_sort_list, non_numeric_sort_list, bottom_order_levels)
    
    if(!is.na(sub_construct)){
      if (include_overall) {
        inner <- bind_rows(inner %>% mutate(sub_temp = "All Sites"), inner)
      }
      sub_cats <- sort(unique(inner$sub_temp))
      sub_cats <- c(sub_cats[!sub_cats %in% bottom_order_levels], intersect(bottom_order_levels, sub_cats))
      if (include_overall) sub_cats <- c("All Sites", sub_cats[sub_cats != "All Sites"])
      row_count <- ifelse(is.null(out),0,nrow(out))
      new_row_count <- 0
      for(sub_cat in sub_cats){
        category_df <- inner %>% 
          filter(sub_temp==sub_cat) %>% 
          select(-sub_temp) %>% 
          group_by(temp) %>% 
          count(temp)
        
        category_tot <- sum(category_df$n)
        category_df <- category_df %>% 
          mutate(percentage = format_count_percent(n,category_tot))
        tot_df <- tibble(temp=sub_cat,percentage=format_count_percent(category_tot, total),header=name_str)
        
        category_df <- category_df  %>% 
          select(-n) %>%
          mutate(header = name_str) %>%
          mutate(temp = factor(temp, levels = custom_levels)) %>% 
          arrange(temp) %>%
          mutate(temp = as.character(temp))
        
        
        if (titlecase) {
          category_df <- category_df %>%
            mutate(temp = str_to_title(temp))
        }
        
        if (is.null(out)) {
          out <- bind_rows(tot_df, category_df)
        } else {
          out <- rbind(out, tot_df, category_df)
        }
        sub_bold_index_vec <- c(sub_bold_index_vec, row_count+1)
        sub_index_vec <- c(sub_index_vec, seq(nrow(category_df))+ row_count+1)
        row_count <- row_count + nrow(category_df) + 1
        new_row_count <- new_row_count + nrow(category_df) + 1
      }
      new <- new_row_count
      names(new) <- paste0(name_str, ' (n=', total, ')')
      index_vec <- c(index_vec, new)
      has_border <- c(has_border, TRUE)
    } else{
      inner <- inner %>% 
        group_by(temp) %>% 
        count(temp) %>% 
        mutate(percentage = format_count_percent(n, total)) %>% 
        select(-n) %>%
        mutate(header = name_str) %>%
        mutate(temp = factor(temp, levels = custom_levels)) %>% 
        arrange(temp) %>%
        mutate(temp = as.character(temp))
      
      if (titlecase) {
        inner <- inner %>%
          mutate(temp = str_to_title(temp))
      }
      
      new <- nrow(inner)
      names(new) <- paste0(name_str, ' (n=', total, ')')
      index_vec <- c(index_vec, new)
      has_border <- c(has_border, TRUE)
      
      if (is.null(out)) {
        out <- inner
      } else {
        out <- rbind(out, inner)
      }
    }
  }
  out <- out %>%
    select(-header)
  
  all_group_starts <- if(length(index_vec) > 1) c(1, cumsum(index_vec[1:(length(index_vec)-1)]) + 1) else c(1)
  border_rows <- all_group_starts[has_border]
  
  if(is_empty(sub_bold_index_vec)){
    vis <- kable(out, format="html", align='l', col.names = c('', '')) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      { if(length(border_rows) > 0) row_spec(., border_rows, extra_css = "border-top: 1px solid") else . } %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left", escape = FALSE) %>% 
      kable_styling("striped", full_width = F, position="left")
  } else{
    vis <- kable(out, format="html", align='l', col.names = c('', '')) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      add_indent(sub_index_vec) %>% 
      row_spec(sub_bold_index_vec, bold = TRUE) %>% 
      { if(length(border_rows) > 0) row_spec(., border_rows, extra_css = "border-top: 1px solid") else . } %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left", escape = FALSE) %>% 
      kable_styling("striped", full_width = F, position="left")
  }
  return(vis)
}




