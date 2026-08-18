#' Progress Tracker
#'
#' @description 
#' Generates a progress bar/visual to display a percentage and item based tracker of what things for a given study must be done.
#' 
#' Currently only rendering for the IVAC data set
#'
#' @param analytic analytic data set that must include study_id, and the associated data set called progress_tracker (dummy column with all values set to TRUE)
#' @param style integers 1-5 indicating the type of style preffered for the progress indicator
#' @param show_check_points boolean to render the names of the tasks next to the progress bar
#' @param trackers a comma separated string of particular tracker names, use "ALL" for all trackers, full name doesnt need to be given
#' @param numerator_construct the name of the construct you want to use as the numerator for your construct progress bar, 
#' if denominator is NA this construct must be only TRUE or FALSE so it can be its own denominator
#' @param denominator_construct optional denominator construct to allow the ratioing of one construct to another
#' @param construct_tracker_name name of the tracker for the given construct params
#' @param construct_units the "units" that are being mesured within the given tracker
#'
#' @return An HTML string that renders the progress bar with the associated settings
#' @export
#'
#' @examples
#' progress_tracker(1, TRUE, "ALL")
#' progress_tracker(5, FALSE, "FirstTracker, Sec")
#' 
progress_tracker <- function(analytic, style, showCheckPoints, trackers, numerator_construct = NA, denominator_construct = NA,
                             construct_tracker_name = NA, construct_units = NA) {
  
  master_list <- list()
  
  addProgressTracker <- function(master_list, name, tasks, statuses, done_values = "DONE", style = NA, units = NA) {
    if (length(tasks) != length(statuses)) {
      stop(paste0("addProgressTracker: 'tasks' and 'statuses' must be the same length (got ",
                  length(tasks), " tasks and ", length(statuses), " statuses) for tracker '", name, "'"))
    }
    
    if (name %in% names(master_list)) {
      stop(paste0("addProgressTracker: a tracker named '", name, "' already exists. ",
                  "Every tracker needs its own unique name."))
    }
    
    normalized_status <- ifelse(statuses %in% done_values, "DONE", "PENDING")
    
    tracker_df <- data.frame(task = tasks, status = normalized_status, raw_status = statuses,
                             stringsAsFactors = FALSE)
    attr(tracker_df, "style") <- style
    attr(tracker_df, "units") <- units
    
    master_list[[name]] <- tracker_df
    master_list
  }
  
  
  
  #if we have a progress tracker we can load for the analytic we load it and pass it through our new function to get the data the same way
  if (file.exists("progress_tracker.csv")) {
    csv_data <- read_csv("progress_tracker.csv")
    
    csv_total_cols <- ncol(csv_data)
    if (csv_total_cols %% 2 != 0) {
      stop("Must have an even number of colums, check to see if each tracker has a task column and status column")
    }
    csv_status_count <- sum(grepl("status", colnames(csv_data), ignore.case = TRUE))
    if (csv_status_count != csv_total_cols / 2) {
      stop("Not enough 'status' columns! Please check that for every task column there is a column that has status next to it")
    }
    
    for (col_i in seq(1, csv_total_cols, by = 2)) {
      master_list <- addProgressTracker(
        master_list,
        name        = names(csv_data)[col_i],
        tasks       = csv_data[[col_i]],
        statuses    = csv_data[[col_i + 1]],
        done_values = "DONE"
      )
    }
  }
  
  numerator_provided <- !(length(numerator_construct) == 1 && is.na(numerator_construct))
  
  if (numerator_provided) {
    
    #checks to make sure all arguments are supplied
    if (length(construct_tracker_name) == 1 && is.na(construct_tracker_name)) {
      stop("progress_tracker: numerator_construct was supplied, but construct_tracker_name was not.")
    }
    if (length(construct_units) == 1 && is.na(construct_units)) {
      stop("progress_tracker: numerator_construct was supplied, but construct_units was not.")
    }
    
    if (length(denominator_construct) == 1 && is.na(denominator_construct)) {
      #single mode
      
    }else{
      
    }
    
    
    
    resolveConstructTotals <- function(x, analytic) {
      looks_numeric_here <- function(v) !is.na(suppressWarnings(as.numeric(v)))
      
      totals <- vapply(x, function(one_value) {
        
        if (looks_numeric_here(one_value)) {
          return(as.numeric(one_value))
        }
        
        if (!(one_value %in% names(analytic))) {
          stop(paste0("resolveConstructTotals: '", one_value, "' is not a number and not a column found in `analytic`."))
        }
        
        col_vals <- analytic[[one_value]]
        
        if (all(is.na(col_vals))) {
          stop(paste0("resolveConstructTotals: analytic$", one_value, " has no non-NA values to sum."))
        }
        
        sum(as.numeric(col_vals), na.rm = TRUE)
        
      }, numeric(1))
      
      counts <- vapply(x, function(one_value) {
        if (looks_numeric_here(one_value)) {
          return(1)   # a literal number isn't backed by rows, need to treat it as a single unit
        }
        
        sum(!is.na(analytic[[one_value]]))
        
      }, numeric(1))
      
      list(total = totals, n = counts)
    }
    
    # resolveConstruct(): resolves denominator_construct to a per-construct
    # *multiplier* -- a literal number used as-is, or a column name whose
    # mean is used as the per-row multiplier.
    
    resolveConstruct <- function(x, analytic) {
      looks_numeric_here <- function(v) !is.na(suppressWarnings(as.numeric(v)))
      
      vapply(x, function(one_value) {
        
        if (looks_numeric_here(one_value)) {
          return(as.numeric(one_value))
        }
        
        if (!(one_value %in% names(analytic))) {
          stop(paste0("resolveConstruct: '", one_value, "' is not a number and not a column found in `analytic`."))
        }
        
        resolved_mean <- mean(analytic[[one_value]], na.rm = TRUE)
        
        if (is.nan(resolved_mean)) {
          stop(paste0("resolveConstruct: analytic$", one_value, " has no non-NA values to average."))
        }
        
        resolved_mean
        
      }, numeric(1))
    }
    
    resolved_numerator_info         <- resolveConstructTotals(numerator_construct, analytic)
    resolved_numerator              <- resolved_numerator_info$total
    numerator_row_counts            <- resolved_numerator_info$n
    resolved_denominator_multiplier <- resolveConstruct(denominator_construct, analytic)
    
    n_constructs <- length(resolved_numerator)
    
    check_construct_length <- function(x, param_name) {
      if (!(length(x) == 1 || length(x) == n_constructs)) {
        stop(paste0("progress_tracker: '", param_name, "' must have length 1 or length ",
                    n_constructs, " (to match numerator_construct), but has length ", length(x), "."))
      }
    }
    
    check_construct_length(resolved_denominator_multiplier, "denominator_construct")
    check_construct_length(construct_tracker_name, "construct_tracker_name")
    check_construct_length(construct_units, "construct_units")
    
    # Recycle any length-1 constant up to n_constructs so every construct
    # parameter lines up as n_constructs parallel vectors, uses rep to create a same size vector for each
    recycle_construct <- function(x) if (length(x) == 1) rep(x, n_constructs) else x
    
    construct_numerators       <- recycle_construct(resolved_numerator)
    denominator_multiplier_vec <- recycle_construct(resolved_denominator_multiplier)
    construct_denominators     <- numerator_row_counts * denominator_multiplier_vec
    construct_names            <- recycle_construct(construct_tracker_name)
    construct_units_vec        <- recycle_construct(construct_units)
    
    
    for (i in seq_len(n_constructs)) {
      master_list <- addProgressTracker(
        master_list,
        name     = construct_names[i],
        tasks    = construct_denominators[i],
        statuses = construct_numerators[i],
        units    = construct_units_vec[i]
      )
    }
    
  }
  
  target_trackers <- trimws(unlist(strsplit(trackers, ",")))
  global_style <- style   # the style this whole call was given; per-tracker overrides fall back to this
  revert_style <- FALSE
  
  
  for (curr_tracker_name in names(master_list)) {
    
    #keeps styles only changing and changing back for numberStyle rendering
    if(revert_style){
      style <- global_style
    }
    
    #create simple standardized dataframe for each tracker that splits it into tasks and statuses
    tracker_df  <- master_list[[curr_tracker_name]]
    tasks       <- tracker_df$task
    completions <- tracker_df$status
    raw_statuses <- tracker_df$raw_status   # pre-normalization values, used only by the ratio check below
    
    #trackers only works to sort trackers that arent constructs, only contructs have units so this is why this works
    if(is.na(attr(tracker_df, "units"))){
      if(trackers != "ALL"){
        if(!any(startsWith(curr_tracker_name, target_trackers))){
          next
        }
      }
    }
    
    ##this logic is for future specific styling, we fall back on global style if this value is null
    tracker_style <- attr(tracker_df, "style")
    style <- if (!is.null(tracker_style) && !is.na(tracker_style)) tracker_style else global_style
    
    tracker_units <- attr(tracker_df, "units")
    
    total_tasks <- sum(tasks != "" & !is.na(tasks)) # recalculated below once the empty rows are actually filtered out
    completed_tasks <- 0
    
    #data organizing and cleaning, marks all tasks as a locally defined completed or pending for consistency
    # Filter out empty rows
    valid_rows <- tasks != "" & !is.na(tasks)
    df <- data.frame(
      task = tasks[valid_rows],
      status = ifelse(completions[valid_rows] %in% "DONE", "Completed", "Pending"),
      stringsAsFactors = FALSE
    )
    
    total_tasks <- nrow(df)
    if(total_tasks == 0) next #skip if no tasks
    
    #sort so Completed tasks are always filled first, then Pending
    df$status <- factor(df$status, levels = c("Completed", "Pending"))
    df <- df[order(df$status), ]
    df$id <- 1:total_tasks
    
    #wrap text cleanly so it fits into the visual segments (approx 15 chars wide)
    df$task_wrap <- stringr::str_wrap(df$task, width = 15)
    
    completed_tasks <- sum(df$status == "Completed", na.rm = TRUE)
    percent_complete <- (completed_tasks / total_tasks) * 100
    
    #ratio format check (if a progress bar is just 2 numbers we know how to handle it), will always be a ratio of the left number to the right
    looks_numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))
    if (total_tasks == 1 && looks_numeric(df$task[1]) && looks_numeric(raw_statuses[valid_rows][1])) {
      ratio_total     <- round(as.numeric(df$task[1]))
      ratio_completed <- round(as.numeric(raw_statuses[valid_rows][1]))
      
      if (ratio_completed > ratio_total) {
        stop(paste0("progress_tracker: tracker '", curr_tracker_name, "' looks like a ratio (one numeric row), ",
                    "but its status value (", ratio_completed, ") is bigger than its task value (", ratio_total, "). ",
                    "Expected the task column to hold the total and the status column to hold the completed count."))
      }
      
      df <- data.frame(
        task   = paste0("Task ", seq_len(ratio_total)),
        status = factor(ifelse(seq_len(ratio_total) <= ratio_completed, "Completed", "Pending"),
                        levels = c("Completed", "Pending")),
        stringsAsFactors = FALSE
      )
      
      df <- df[order(df$status), ]
      df$id        <- 1:ratio_total
      df$task_wrap <- stringr::str_wrap(df$task, width = 15)
      
      total_tasks      <- ratio_total
      completed_tasks  <- ratio_completed
      percent_complete <- (completed_tasks / total_tasks) * 100
      
      if (!(style %in% c(5, 7, 8))) {
        style <- paste0("numberStyle", as.character(style))
        revert_style <- TRUE
      } else {
        stop(paste0("progress_tracker: tracker '", curr_tracker_name, "' looks like a ratio (one numeric row), ",
                    "but styles 5, 7, and 8 don't support ratio-style rendering. Progress trackers with ",
                    "number-only rendering can only use styles 1, 2, 3, 4, or 6."))
      }
      
    }
    
    #styling renders... 
    
    if(style == "numberStyle1"){
      #Subway bar converted
      track_color   <- "#e5e7eb"
      fill_color    <- "#10b981"
      fill_color_dk <- "#059669"
      
      BAR_LENGTH <- 10   # fixed bar length in data units, independent of total_tasks
      fraction <- completed_tasks / total_tasks
      fill_end <- fraction * BAR_LENGTH
      
      tick_frac   <- seq(0, 1, length.out = 10)   # exactly even positions, 0 to 1 inclusive
      tick_values <- round(tick_frac * total_tasks)  # nearest-whole-number label at each fixed position
      tick_x <- tick_frac * BAR_LENGTH
      
      p <- ggplot() +
        # Background empty track
        annotate("segment", x = 0, xend = BAR_LENGTH, y = 0, yend = 0,
                 color = track_color, linewidth = 14, lineend = "round")
      
      if (fraction > 0) {
        p <- p + annotate("segment", x = 0, xend = ifelse (fraction != 1, fill_end - .4, fill_end), y = 0, yend = 0,
                          color = fill_color, linewidth = 14, lineend = "round")
      }
      
      p <- p +
        # Tick marks + count labels along the bar
        annotate("segment", x = tick_x, xend = tick_x, y = -0.32, yend = -0.18,
                 color = "#9ca3af", linewidth = 0.7) +
        annotate("text", x = tick_x, y = -0.55, label = tick_values,
                 size = 3, fontface = "bold", color = "#6b7280") +
        
        # Percent badge, fixed just past the end of the (fixed-length) bar
        annotate("point", x = BAR_LENGTH + 1.8, y = 0, size = 24, color = fill_color, alpha = 0.12) +
        annotate("text", x = BAR_LENGTH + 1.8, y = 0, label = paste0(round(percent_complete), "%"),
                 fontface = "bold", size = 6, color = fill_color_dk)
      
      #units
      if (!is.na(tracker_units)) {
        p <- p + annotate("text", x = BAR_LENGTH / 2, y = -0.9, label = paste0("(", tracker_units, ")"),
                          fontface = "italic", size = 3.3, color = "#6b7280")
      }
      
      p <- p +
        coord_cartesian(ylim = c(-1.3, 1), xlim = c(-0.5, BAR_LENGTH + 2.7)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    }
    else if(style == "numberStyle2"){
      #Donut style
      ring_center <- 2.6
      ring_width  <- 1.15
      ring_outer  <- ring_center + ring_width / 2
      
      fraction <- completed_tasks / total_tasks
      
      #2 slices, filled and unfilled, ymin and ymax stay the same as before
      ring_df <- data.frame(
        status = factor(c("Completed", "Pending"), levels = c("Completed", "Pending")),
        ymin   = c(0, fraction),
        ymax   = c(fraction, 1)
      )
      
      tick_frac   <- seq(0, 1, length.out = 10)   # exactly even angular positions around the ring
      tick_values <- round(tick_frac * total_tasks)  # nearest-whole-number label at each fixed position
      
      tick_start <- ring_outer + 0.1
      tick_end   <- tick_start + 0.35
      
      #near far labels just to avoid any future problems with collisons
      label_near <- tick_end + 0.2
      label_far  <- label_near + 0.5
      label_radius <- ifelse(seq_along(tick_values) %% 2 == 1, label_near, label_far)
      
      tick_df <- data.frame(
        frac    = tick_frac,
        value   = tick_values,
        x_start = tick_start,
        x_end   = tick_end,
        label_x = label_radius,
        hjust   = ifelse(tick_frac < 0.5, 0, ifelse(tick_frac > 0.5, 1, 0.5))
      )
      
      # y = 0.5 lands at the bottom of the ring (6 o'clock), since
      # coord_polar's default start=0 puts y=0 at the top and sweeps
      # clockwise -- halfway around from the top is straight down.
      outer_radius <- max(tick_df$label_x) + 1 + ifelse(!is.na(tracker_units), 0.5, 0)
      
      p <- ggplot(ring_df) +
        geom_rect(aes(xmin = ring_center - ring_width / 2, xmax = ring_center + ring_width / 2,
                      ymin = ymin, ymax = ymax, fill = status), color = NA) +
        coord_polar(theta = "y", start = 0) +
        xlim(c(0, outer_radius)) +
        scale_fill_manual(values = c("Completed" = "#3b82f6", "Pending" = "#e5e7eb")) +
        geom_segment(data = tick_df, aes(x = x_start, xend = x_end, y = frac, yend = frac),
                     inherit.aes = FALSE, color = "#9ca3af", linewidth = 0.6) +
        geom_text(data = tick_df, aes(x = label_x, y = frac, label = value, hjust = hjust),
                  inherit.aes = FALSE, size = 2.8, fontface = "bold", color = "#6b7280") +
        annotate("text", x = 0, y = 0, label = paste0(round(percent_complete), "%"),
                 size = 9, fontface = "bold", color = "#1f2937")
      
      #units
      if (!is.na(tracker_units)) {
        p <- p + annotate("text", x = outer_radius - 0.3, y = 0.5, label = paste0("(", tracker_units, ")"),
                          fontface = "italic", size = 3.3, color = "#6b7280")
      }
      
      p <- p +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    }
    else if(style == "numberStyle3"){
      #Thermometer
      tube_x <- 1.6
      TUBE_HEIGHT <- 10   # fixed tube height in data units, independent of total_tasks
      
      fraction <- completed_tasks / total_tasks
      fill_y <- fraction * TUBE_HEIGHT
      
      tick_frac   <- seq(0, 1, length.out = 10)   # exactly even positions, 0 to 1 inclusive
      tick_values <- round(tick_frac * total_tasks)  # nearest-whole-number label at each fixed position
      tick_y <- tick_frac * TUBE_HEIGHT
      
      p <- ggplot() +
        annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = TUBE_HEIGHT,
                 color = "#cbd5e1", linewidth = 9, lineend = "round") +
        annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = TUBE_HEIGHT,
                 color = "#f8fafc", linewidth = 6.4, lineend = "round")
      
      if (fraction > 0) {
        p <- p + annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = fill_y,
                          color = "#ef4444", linewidth = 6.3, lineend = "round")
      }
      
      p <- p +
        annotate("segment", x = tube_x - 0.13, xend = tube_x - 0.13, y = 0.5, yend = TUBE_HEIGHT - 0.3,
                 color = "white", alpha = 0.55, linewidth = 1.2, lineend = "round") +
        annotate("point", x = tube_x, y = -0.5 , size = 13, color = "#b91c1c") +
        annotate("point", x = tube_x, y = -0.5, size = 10, color = "#ef4444") +
        
        # Tick marks + count labels along the tube's height
        annotate("segment", x = tube_x + 0.30, xend = tube_x + 0.60, y = tick_y, yend = tick_y,
                 color = "#6b7280", linewidth = 0.7) +
        annotate("text", x = tube_x + 0.75, y = tick_y, label = tick_values,
                 hjust = 0, size = 3.4, fontface = "bold", color = "#374151") +
        annotate("point", x = tube_x - 1.15, y = TUBE_HEIGHT / 2, size = 28, color = "#ef4444", alpha = 0.10) +
        annotate("text", x = tube_x - 1.15, y = TUBE_HEIGHT / 2,
                 label = paste0(round(percent_complete), "%"),
                 fontface = "bold", size = 6, color = "#b91c1c")
      
      #units
      if (!is.na(tracker_units)) {
        p <- p + annotate("text", x = tube_x + 2.8, y = TUBE_HEIGHT / 2, label = paste0("(", tracker_units, ")"),
                          hjust = 0, fontface = "italic", size = 3.3, color = "#6b7280")
      }
      
      p <- p +
        coord_cartesian(xlim = c(tube_x - 2.1, tube_x + 5.6), ylim = c(-0.8, TUBE_HEIGHT + 0.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    }
    else if(style == "numberStyle4"){
      #Fluid Battery
      fraction <- completed_tasks / total_tasks
      battery_fill <- if (percent_complete <= 33) "#ef4444" else if (percent_complete <= 66) "#f59e0b" else "#10b981"
      battery_text <- if (percent_complete <= 33) "#b91c1c" else if (percent_complete <= 66) "#b45309" else "#047857"
      
      CASE_LENGTH <- 10   # fixed case length in data units, independent of total_tasks
      fill_end <- fraction * CASE_LENGTH
      
      tick_frac   <- seq(0, 1, length.out = 10)   # exactly even positions, 0 to 1 inclusive
      tick_values <- round(tick_frac * total_tasks)  # nearest-whole-number label at each fixed position
      tick_x <- tick_frac * CASE_LENGTH
      
      p <- ggplot() +
        annotate("rect", xmin = 0, xmax = CASE_LENGTH, ymin = -0.6, ymax = 0.6,
                 fill = NA, color = "#374151", linewidth = 1.2) +
        annotate("rect", xmin = CASE_LENGTH, xmax = CASE_LENGTH + 0.3, ymin = -0.25, ymax = 0.25,
                 fill = "#374151", color = "#374151")
      
      if (fraction > 0) {
        p <- p + annotate("rect", xmin = 0.08, xmax = fill_end, ymin = -0.48, ymax = 0.48,
                          fill = battery_fill, color = NA)
      }
      
      p <- p +
        # Tick marks + count labels along the case
        annotate("segment", x = tick_x, xend = tick_x, y = 0.6, yend = 0.85,
                 color = "#9ca3af", linewidth = 0.7) +
        annotate("text", x = tick_x, y = 1.15, label = tick_values,
                 size = 3, fontface = "bold", color = "#6b7280") +
        annotate("text", x = CASE_LENGTH + 1.6, y = 0.55, label = "⚡", size = 5) +
        annotate("text", x = CASE_LENGTH + 1.6, y = 0, label = paste0(round(percent_complete), "%"),
                 size = 6, fontface = "bold", color = battery_text)
      
      #units
      if (!is.na(tracker_units)) {
        p <- p + annotate("text", x = CASE_LENGTH / 2, y = -1.1, label = paste0("(", tracker_units, ")"),
                          fontface = "italic", size = 3.3, color = "#6b7280")
      }
      
      p <- p +
        coord_cartesian(ylim = c(-1.6, 1.6), xlim = c(-0.2, CASE_LENGTH + 2.4)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
    }
    else if(style == "numberStyle6"){
      #Throttle
      fraction <- completed_tasks / total_tasks
      
      #2 slices again, filled and unfilled
      dial_df <- data.frame(
        status = factor(c("Completed", "Pending"), levels = c("Completed", "Pending")),
        ymin   = c(0, fraction),
        ymax   = c(fraction, 1)
      )
      
      #needle is moved fluidly around the edge continously
      needle_y  <- fraction
      percent_y <- 1.5
      
      tick_frac   <- seq(0, 1, length.out = 10)   # exactly even positions along the half-circle sweep
      tick_values <- round(tick_frac * total_tasks)  # nearest-whole-number label at each fixed position
      tick_df <- data.frame(
        frac  = tick_frac,
        value = tick_values,
        hjust = ifelse(tick_frac < 0.5, 1, ifelse(tick_frac > 0.5, 0, 0.5))
      )
      
      p <- ggplot(dial_df) +
        geom_rect(aes(xmin = 1.0, xmax = 3.0, ymin = ymin, ymax = ymax, fill = status), color = NA) +
        
        # Tick marks + count labels around the dial
        geom_segment(data = tick_df, aes(x = 3.0, xend = 3.2, y = frac, yend = frac),
                     inherit.aes = FALSE, color = "#9ca3af", linewidth = 0.8) +
        geom_text(data = tick_df, aes(x = 3.4, y = frac, label = value, hjust = hjust),
                  inherit.aes = FALSE, size = 2.8, fontface = "bold", color = "#6b7280") +
        
        # Needle hub + pointer
        annotate("point", x = 0, y = 0, size = 9, color = "#1f2937") +
        annotate("segment", x = 0.15, xend = 2.3, y = needle_y, yend = needle_y,
                 color = "#1f2937", linewidth = 1.4, lineend = "round") +
        
        # Percent readout
        annotate("text", x = 0.9, y = percent_y, label = paste0(round(percent_complete), "%"),
                 size = 8, fontface = "bold", color = "#c2410c")
      
      #unit renders
      if (!is.na(tracker_units)) {
        p <- p + annotate("text", x = 1.9, y = percent_y, label = paste0("(", tracker_units, ")"),
                          fontface = "italic", size = 3.3, color = "#6b7280")
      }
      
      p <- p +
        scale_fill_manual(values = c("Completed" = "#f97316", "Pending" = "#e5e7eb")) +
        coord_polar(theta = "y", start = -pi / 2) +
        ylim(c(0, 2)) +
        xlim(c(0, 4.7)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(2, 8, 0, 8),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
    }
    else if(style == 1){
      #Subway Style Bar
      track_color   <- "#e5e7eb"
      fill_color    <- "#10b981"
      fill_color_dk <- "#059669"
      
      p <- ggplot(df, aes(x = id, y = 0)) +
        # Background empty track
        annotate("segment", x = 0.5, xend = total_tasks + 0.5, y = 0, yend = 0,
                 color = track_color, linewidth = 14, lineend = "round")
      
      #rendering for empty bar
      if (completed_tasks > 0) {
        p <- p + annotate("segment", x = 0.5, xend = completed_tasks + 0.5, y = 0, yend = 0,
                          color = fill_color, linewidth = 14, lineend = "round")
      }
      
      p <- p +
        #rendered circles "checkpoints"
        geom_point(aes(fill = status), shape = 21, size = 7, color = "white", stroke = 2.2)
      
      if(showCheckPoints) {
        #grey pointer line
        p <- p + geom_segment(aes(x = id, xend = id, y = ifelse(id %% 2 == 1, 0.15, -0.15), yend = ifelse(id %% 2 == 1, 0.265, -0.265)),
                              color = "#9ca3af", linewidth = 0.6) +
          
          #using if else to stagger the text to be above and under bar 
          geom_text(aes(y = ifelse(id %% 2 == 1, 0.38, -0.38), label = task_wrap, color = status),
                    fontface = "bold", size = 4, vjust = 0.5, lineheight = 0.9)
      }
      
      
      #rendering percent based off total tasks to dynamically push it away from the bar itself
      p <- p + annotate("point", x = ifelse(total_tasks > 4, total_tasks * 1.25, total_tasks * 1.4), y = 0, size = 24, color = fill_color, alpha = 0.12) +
        annotate("text", x = ifelse(total_tasks > 4, total_tasks * 1.25, total_tasks * 1.4), y = 0, label = paste0(round(percent_complete), "%"),
                 fontface = "bold", size = 6, color = fill_color_dk) +
        
        scale_fill_manual(values = c("Completed" = fill_color, "Pending" = "white")) +
        scale_color_manual(values = c("Completed" = fill_color_dk, "Pending" = "#9ca3af")) +
        
        # xlim padding to keep things centered
        #consistent theme addition to keep all progress bars contained within their own consistent boxes
        coord_cartesian(ylim = c(-1, 1), xlim = c(-0.1, total_tasks + 2.7)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        
        labs(title = curr_tracker_name)
      
      
    } 
    else if(style == 2){
      #Segmented Donut Style
      
      df$val <- 1 # each task takes up an equal 1/n slice of the donut
      
      # Each task's slice spans [cum_start, cum_end] on a 0..total_tasks scale,
      # in the SAME order as the rows already appear in `df` (Completed tasks
      # first, then Pending -- this is what makes the ring's completed portion
      # one contiguous arc). Every layer below (ring, leader line, label) reads
      # its position from these two columns, so they can never disagree.
      df$cum_end   <- cumsum(df$val)
      df$cum_start <- df$cum_end - df$val
      df$mid_y     <- (df$cum_start + df$cum_end) / 2
      
      # frac_mid = how far around the donut (0 to 1) the MIDDLE of each task's
      # slice sits, purely to decide which side of the circle a label is on.
      df$frac_mid    <- df$mid_y / total_tasks
      
      # Labels on one half of the ring are left-aligned and on the other half
      # right-aligned, so the text always points outward away from the ring
      # instead of overlapping it.
      df$label_hjust <- ifelse(df$frac_mid < 0.5, 0, ifelse(df$frac_mid > 0.5, 1, 0.5))
      
      # --- Radius layout -----------------------------------------------------
      # Everything below is defined as a radius, working outward from the
      # center: ring -> small gap -> leader line -> label. When there are a
      # lot of tasks, adjacent slices can end up very close together in angle,
      # and if every label sat at the same radius those neighbors would
      # collide -- so in that case we alternate labels between a "near" and
      # "far" radius (see needs_stagger below), the same trick used for the
      # node labels in Style 1. With fewer tasks there's no crowding to solve,
      # so every label just uses the same consistent "near" radius.
      ring_center <- 2.6    # radius the ring itself is drawn at (bigger = bigger donut)
      ring_width  <- 1.15   # thickness of the ring
      ring_outer  <- ring_center + ring_width / 2
      
      leader_start <- ring_outer + 0.1    # connector line starts just outside the ring
      leader_end   <- leader_start + 0.4  # ...and points back to here
      
      label_near <- leader_end + 0.2   # "near" label radius
      label_far  <- label_near + 0.75  # "far" label radius (only used when staggering, see below)
      
      
      STAGGER_THRESHOLD <- 6
      needs_stagger <- total_tasks > STAGGER_THRESHOLD
      df$label_x <- if (needs_stagger) {
        ifelse(df$id %% 2 == 1, label_near, label_far)
      } else {
        label_near
      }
      
      outer_radius <- label_far + 1.3  # leaves room for the label text itself
      
      p <- ggplot(df) +
        # The ring itself, drawn with explicit ymin/ymax (= cum_start/cum_end)
        # rather than geom_col()+position_stack(), so every slice sits exactly
        # where mid_y says it does -- no separate stacking calculation that
        # could disagree with the leader lines or labels below.
        geom_rect(aes(xmin = ring_center - ring_width / 2, xmax = ring_center + ring_width / 2,
                      ymin = cum_start, ymax = cum_end, fill = status),
                  color = "white", linewidth = 2) +
        coord_polar(theta = "y", start = 0) +
        # This is a radius limit, not a left/right limit (it's a polar plot) --
        # sized to just clear the outermost ("far") labels.
        xlim(c(0, outer_radius)) +
        scale_fill_manual(values = c("Completed" = "#3b82f6", "Pending" = "#e5e7eb"))
      
      if(showCheckPoints){
        # Thin leader line pointing from the edge of the ring back out to each
        # label, drawn at that task's own mid_y -- guaranteed to point at the
        # correct slice since it's the same value used to draw the ring above.
        p <- p + geom_segment(aes(x = leader_start, xend = leader_end, y = mid_y, yend = mid_y),
                              color = "#9ca3af", linewidth = 0.6) +
          geom_text(aes(x = label_x, y = mid_y, label = task_wrap, color = status, hjust = label_hjust),
                    size = 3.3, fontface = "bold", lineheight = 0.9) +
          scale_color_manual(values = c("Completed" = "#1d4ed8", "Pending" = "#6b7280"))
      }
      
      
      
      # Center percentage text (x = 0 is the pole/center of the donut hole)
      p <- p + annotate("text", x = 0, y = 0, label = paste0(round(percent_complete), "%"),
                        size = 9, fontface = "bold", color = "#1f2937") +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
      
    } 
    else if(style == 3){
      #Thermometer Style
      tube_x <- 1.6
      
      p <- ggplot(df) +
        # Glass outline
        annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = total_tasks,
                 color = "#cbd5e1", linewidth = 9, lineend = "round") +
        # Empty tube interior
        annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = total_tasks,
                 color = "#f8fafc", linewidth = 6.4, lineend = "round")
      
      if (completed_tasks > 0) {
        p <- p + annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = completed_tasks,
                          color = "#ef4444", linewidth = 6.4, lineend = "round")
      }
      
      p <- p +
        # Glass shine streak
        annotate("segment", x = tube_x - 0.13, xend = tube_x - 0.13, y = 0.5, yend = total_tasks - 0.3,
                 color = "white", alpha = 0.55, linewidth = 1.2, lineend = "round") +
        # Bulb
        annotate("point", x = tube_x, y = 0, size = 13, color = "#b91c1c") +
        annotate("point", x = tube_x, y = 0, size = 10, color = "#ef4444")
      
      if(showCheckPoints){
        # Tick marks
        p <- p + geom_segment(aes(x = tube_x + 0.30, xend = tube_x + 0.60, y = id - 0.5, yend = id - 0.5),
                              color = "#6b7280", linewidth = 0.7) +
          # Task labels
          geom_text(aes(x = tube_x + .75, y = id - 0.5, label = task_wrap, color = status),
                    hjust = 0, size = 3.8, fontface = "bold", lineheight = 0.9)
      }
      
      # Percent readout
      p <- p + annotate("point", x = tube_x - 1.15, y = total_tasks / 2, size = 28, color = "#ef4444", alpha = 0.10) +
        annotate("text", x = tube_x - 1.15, y = total_tasks / 2,
                 label = paste0(round(percent_complete), "%"),
                 fontface = "bold", size = 6, color = "#b91c1c") +
        scale_color_manual(values = c("Completed" = "#b91c1c", "Pending" = "#9ca3af")) +
        
        #xlim used to keep things centered via left and right padding
        coord_cartesian(xlim = c(tube_x - 2.1, tube_x + 4.7), ylim = c(-0.8, total_tasks + 0.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
      
    } 
    else if(style == 4) {
      #Battery Progress bar
      
      #rennders the colors for better
      battery_fill <- if (percent_complete <= 33) "#ef4444" else if (percent_complete <= 66) "#f59e0b" else "#10b981"
      battery_text <- if (percent_complete <= 33) "#b91c1c" else if (percent_complete <= 66) "#b45309" else "#047857"
      
      p <- ggplot(df) +
        # Outer Battery Case
        geom_rect(aes(xmin = 0.2, xmax = total_tasks + 0.8, ymin = -0.6, ymax = 0.6), 
                  fill = NA, color = "#374151", linewidth = 1.2) +
        # Battery Terminal Nub
        geom_rect(aes(xmin = total_tasks + 0.8, xmax = total_tasks + 1.1, ymin = -0.25, ymax = 0.25), 
                  fill = "#374151", color = "#374151") +
        # Internal Charge Segments
        geom_rect(aes(xmin = id - 0.6, xmax = id + 0.4, ymin = -0.48, ymax = 0.48, fill = status), 
                  color = "white", linewidth = 1)
      
      if(showCheckPoints){
        # Staggered Connecting Lines (odd segments connect upward, even ones downward)
        p <- p + geom_segment(aes(x = id - 0.1, xend = id - 0.1, 
                                  y = ifelse(id %% 2 == 1, 0.7, -0.7), 
                                  yend = ifelse(id %% 2 == 1, 1.2, -1.2)), color = "#9ca3af") +
          # Staggered Task Labels
          geom_text(aes(x = id - 0.1, y = ifelse(id %% 2 == 1, 1.55, -1.55), 
                        label = task_wrap, color = status), 
                    size = 4, fontface = "bold", lineheight = 0.9)
      }
      
      # Charge readout
      p <- p + annotate("text", x = total_tasks + 2.0, y = 0.55, label = "⚡", size = 5) +
        annotate("text", x = total_tasks + 2.0, y = 0, label = paste0(round(percent_complete), "%"), 
                 size = 6, fontface = "bold", color = battery_text) +
        scale_fill_manual(values = c("Completed" = battery_fill, "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = battery_text, "Pending" = "#6b7280")) +
        
        #more centering logic for left right padding
        coord_cartesian(ylim = c(-2.2, 2.2), xlim = c(-0.2, total_tasks + 2.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    } 
    else if(style == 5){
      #Stepping Progress bar
      
      p <- ggplot(df) +
        # Pending Path
        geom_step(aes(x = id - 0.5, y = id), color = "#e5e7eb", linewidth = 2, direction = "vh") +
        # Completed Path
        geom_step(data = subset(df, id <= completed_tasks), 
                  aes(x = id - 0.5, y = id), color = "#f59e0b", linewidth = 2.5, direction = "vh") +
        # Step Nodes
        geom_point(aes(x = id - 0.5, y = id, fill = status), 
                   shape = 21, size = 4.5, color = "white", stroke = 1.5) +
        # Summit Flag
        annotate("text", x = total_tasks + 0.1, y = total_tasks + 0.6, label = "🚩", size = 6)
      
      if(showCheckPoints){
        # Dotted Line Connectors
        p <- p +geom_segment(aes(x = id - 0.5, xend = id + 0.45, y = id, yend = id), 
                             color = "#9ca3af", linewidth = 0.9, linetype = "dotted") +
          # Task Labels
          geom_text(aes(x = id + 0.55, y = id, label = task_wrap, color = status), 
                    hjust = 0, size = 6, fontface = "bold", lineheight = 0.9)
      }
      
      # Percent complete
      p <- p + annotate("point", x = 1, y = total_tasks , size = 26, color = "#f59e0b", alpha = 0.12) +
        annotate("text", x = 1, y = total_tasks, 
                 label = paste0(round(percent_complete), "%"),
                 size = 6, fontface = "bold", color = "#b45309") +
        scale_fill_manual(values = c("Completed" = "#f59e0b", "Pending" = "#d1d5db")) +
        scale_color_manual(values = c("Completed" = "#b45309", "Pending" = "#9ca3af")) +
        
        # Right-hand padding bumped slightly (3 -> 3.3) so it mirrors the 0.5-unit
        # gap left in front of the first step, instead of hugging the labels
        coord_cartesian(xlim = c(0, total_tasks + 3.3), ylim = c(0.5, total_tasks + 1.2)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    } 
    else if(style == 6) {
      #Style 6, Half Throttle
      df$seg_start   <- df$id - 1
      df$seg_end     <- df$id
      
      #same idea as the donut labels above: point each task's label outward
      # away from the dial instead of letting it overlap the ring.
      df$label_hjust <- ifelse(df$id / total_tasks < 0.5, 1, ifelse(df$id / total_tasks > 0.5, 0, 0.5))
      
      # The needle rests at the boundary between the last completed task and
      # the first pending one; if nothing is done yet it just sits at 0.
      needle_y  <- if (completed_tasks > 0) completed_tasks - 0.5 else 0
      percent_y <- 1.5 * total_tasks
      
      
      p <- ggplot(df) +
        # Task slices
        geom_rect(aes(xmin = 1.0, xmax = 3.0, ymin = seg_start, ymax = seg_end, fill = status), 
                  color = "white", linewidth = 1)
      
      if(showCheckPoints){
        # Task labels
        p <- p + geom_text(aes(x = 3.3, y = id - 0.5, label = task_wrap, color = status, hjust = label_hjust), 
                           size = 3, fontface = "bold", lineheight = 0.9)
      }
      
      # Needle hub + pointer
      p <- p + annotate("point", x = 0, y = 0, size = 9, color = "#1f2937") +
        annotate("segment", x = 0.15, xend = 2.3, y = needle_y, yend = needle_y, 
                 color = "#1f2937", linewidth = 1.4, lineend = "round") +
        
        # Percent readout
        annotate("text", x = 0.9, y = percent_y, label = paste0(round(percent_complete), "%"), 
                 size = 8, fontface = "bold", color = "#c2410c") +
        scale_fill_manual(values = c("Completed" = "#f97316", "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = "#c2410c", "Pending" = "#6b7280")) +
        coord_polar(theta = "y", start = -pi / 2) +
        ylim(c(0, 2 * total_tasks)) +
        
        #sizing options
        xlim(c(0, 4.7)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(2, 8, 0, 8),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
    }
    else if(style == 7) {
      #Spider spoke wheel (non linear style)
      
      set.seed(sum(utf8ToInt(curr_tracker_name))) #seeding to keep the same ordering of nodes for every run of the code for each tracker
      
      spoke_full  <- 2.9   # how far a Completed spoke reaches
      spoke_short <- 1.5   # how far a Pending spoke reaches
      
      
      cone_half_width_deg <- 18 #cone deadzone
      allowed_start_deg   <- 270 + cone_half_width_deg   # just past the cone's right edge, where to start rendering
      allowed_span_deg    <- 360 - 2 * cone_half_width_deg #allowed radius with cone deadzone
      
      # spoke_order[k] = which of the n evenly-spaced slots (1..n) row k's task
      # gets. Since slot numbers increase in angular order by construction, this
      # SAME vector doubles as an angular-adjacency index for the label
      # staggering below -- no separate column needed.
      spoke_order <- sample(seq_len(total_tasks))
      df$spoke_angle <- (allowed_start_deg + (spoke_order - 0.5) / total_tasks * allowed_span_deg) * pi / 180
      
      df$spoke_len <- ifelse(df$status == "Completed", spoke_full, spoke_short)
      df$node_x <- df$spoke_len * cos(df$spoke_angle)
      df$node_y <- df$spoke_len * sin(df$spoke_angle)
      df$label_hjust <- ifelse(df$node_x < 0, 1, 0)
      
      #dyanmic rendering for completed spokes being thicker
      df$spoke_width <- ifelse(df$status == "Completed", 1.9, 1)
      
      
      STAGGER_THRESHOLD <- 8   # past this many tasks, alternate near/far so tight angular neighbors don't collide
      label_gap_near <- 0.42 #displacment for near nodes
      label_gap_far  <- label_gap_near + 0.55 #displacement for far nodes
      
      #if we have more than stagger_trheshold nodes, we start the staggering process
      df$label_gap <- if (total_tasks > STAGGER_THRESHOLD) {
        ifelse(spoke_order %% 2 == 0, label_gap_far, label_gap_near)
      } else {
        label_gap_near
      }
      
      #spoke setting logic
      df$label_x <- df$node_x + cos(df$spoke_angle) * df$label_gap
      df$label_y <- df$node_y + sin(df$spoke_angle) * df$label_gap
      
      #adjustable distance for rendering the %complete graphic, percent_extra pushes it that amount below the last filled spoke for no chance of collision
      percent_extra <- 1.3
      percent_y <- -(spoke_full + percent_extra)
      
      p <- ggplot(df) +
        geom_segment(aes(x = 0, y = 0, xend = node_x, yend = node_y, color = status,
                         linewidth = I(spoke_width))) + 
        geom_point(aes(x = node_x, y = node_y, fill = status), shape = 21, size = 8,
                   color = "white", stroke = 1.6)
      
      if(showCheckPoints){
        p <- p  +  geom_text(aes(x = label_x, y = label_y, label = task_wrap,
                                 color = status, hjust = label_hjust),
                             size = 3.2, fontface = "bold", lineheight = 0.85)
      }
      
      #HUB
      p <- p + annotate("point", x = 0, y = 0, size = 13, color = "#1f2937") +
        annotate("text", x = 0, y = percent_y, label = paste0(round(percent_complete), "%"),
                 size = 8, fontface = "bold", color = "#111827") +
        scale_fill_manual(values = c("Completed" = "#6366f1", "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = "#4338ca", "Pending" = "#9ca3af")) +
        
        #bottom limited tied to percent_y so there will never be a chance of collision assuming regular length of task names
        coord_fixed(ratio = 1, xlim = c(-spoke_full - 2.3, spoke_full + 2.3),
                    ylim = c(percent_y - 0.6, spoke_full + 1.6)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        
        labs(title = curr_tracker_name)
      
    }
    else{
      #Orbit Ring Non-Linear Style
      
      set.seed(sum(utf8ToInt(curr_tracker_name))) #seeding again for consistency across different code runs
      
      ring_inner_r <- ifelse(total_tasks > 7, 2.6, 1.8)   #completed inner ring
      ring_outer_r <- ifelse(total_tasks > 7, 4.0, 2.7)   #pending outer ring
      
      #master function logic for placing the dots on the orbits, radius is either the inner ring our outer ring, and start angle is adjusted so there will be no inner
      #outer collision
      #logic is just reused with different variables to save code and allow for a more modifyable placment sequence
      place_on_ring <- function(sub_df, radius, start_angle) {
        if (nrow(sub_df) == 0) return(sub_df)
        sub_df <- sub_df[sample(nrow(sub_df)), ]
        n <- nrow(sub_df)
        sub_df$ring_radius <- radius
        sub_df$ring_order  <- seq_len(n)   # this ring's own 1..n index, used for label staggering below
        sub_df$angle        <- start_angle + seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
        sub_df$orbit_x <- radius * cos(sub_df$angle)
        sub_df$orbit_y <- radius * sin(sub_df$angle)
        sub_df
      }
      
      #actual placing using completed and pending constructs as previously mentione
      df <- rbind(
        place_on_ring(df[df$status == "Completed", ], ring_inner_r, start_angle = 0),
        place_on_ring(df[df$status == "Pending", ],   ring_outer_r, start_angle = pi / 2)
      )
      
      #each label rendered away from the center based on where it is located ring wise
      df$label_hjust <- ifelse(df$orbit_x < 0, 1, 0)
      
      #completes are bolded and pends are just regular text (small change)
      df$label_face <- ifelse(df$status == "Completed", "bold", "plain")
      
      #adding classic stagger threshold logic to provide collisons between nodes on the SAME radius, many variables here to allow for easy edits
      STAGGER_THRESHOLD <- 5
      tick_gap   <- 0.18   #gap from the ring out to where the leader line starts
      leader_len <- 0.35   #length of the leader line itself
      label_near <- tick_gap + leader_len + 0.15
      label_far  <- label_near + 0.55
      
      #actual logic of the labels to apply thresholding idea
      ring_counts <- table(df$status)
      df$ring_task_count <- as.integer(ring_counts[df$status])
      df$label_offset <- ifelse(df$ring_task_count > STAGGER_THRESHOLD & df$ring_order %% 2 == 0,
                                label_far, label_near)
      
      #logic to keep the nodes exactly on their rings by having an imaginary "spoke" going from the center
      df$leader_end_x <- (df$ring_radius + tick_gap) * cos(df$angle)
      df$leader_end_y <- (df$ring_radius + tick_gap) * sin(df$angle)
      df$label_x <- (df$ring_radius + df$label_offset) * cos(df$angle)
      df$label_y <- (df$ring_radius + df$label_offset) * sin(df$angle)
      
      #thin reference circle for each ring, traced out as a 100-point path because theres no way to draw a circle with one command in ggplot2
      ring_path <- function(radius) {
        a <- seq(0, 2 * pi, length.out = 100)
        data.frame(x = radius * cos(a), y = radius * sin(a))
      }
      
      p <- ggplot(df, aes(x = orbit_x, y = orbit_y)) +
        geom_path(data = ring_path(ring_inner_r), aes(x = x, y = y), inherit.aes = FALSE,
                  color = "#e5e7eb", linewidth = 0.6) +
        geom_path(data = ring_path(ring_outer_r), aes(x = x, y = y), inherit.aes = FALSE,
                  color = "#e5e7eb", linewidth = 0.6) +
        
        #leader line from each node out to its label + the ball itself
        geom_segment(aes(xend = leader_end_x, yend = leader_end_y), color = "#9ca3af", linewidth = 0.5)+ 
        geom_point(aes(fill = status), shape = 21, size = 6, color = "white", stroke = 1.6)
      
      if(showCheckPoints) {
        p <- p +  geom_text(aes(x = label_x, y = label_y, label = task_wrap, color = status,
                                hjust = label_hjust, fontface = I(label_face)),
                            size = 3.2, lineheight = 0.85)
      }
      
      # Percent readout
      p <- p + annotate("point", x = 0, y = 0, size = 22, color = "#f97316") +
        annotate("text", x = 0, y = 0, label = paste0(round(percent_complete), "%"),
                 size = 5.5, fontface = "bold", color = "white") +
        scale_fill_manual(values = c("Completed" = "#ec4899", "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = "#be185d", "Pending" = "#6b7280")) +
        
        #padding sized generously for the bigger label font + the "far" stagger distance, so long wrapped labels don't clip at the panel edge.
        coord_fixed(ratio = 1, xlim = c(-ring_outer_r - 2.6, ring_outer_r + 2.6),
                    ylim = c(-ring_outer_r - 1.8, ring_outer_r + 1.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        
        labs(title = curr_tracker_name)
    }
    
    #print and then use ggplot to draw a thin rectangle below to seperate
    print(p)
    grid::grid.rect(
      x = 0.5, y = 0.012, width = .94, height = 0.004,
      gp = grid::gpar(fill = "grey", col = NA)
    )
    
  }
}