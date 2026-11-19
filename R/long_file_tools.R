#' Unzip a Long File Column
#'
#' @description
#' Splits a zipped long-file column into one row per record and one column per
#' field. Detects the zipping format from the data:
#'
#' - Columns zipped with `auto_zip_dataframe()` (Elemental Study constructs) use
#'   "¶" between records and "┘" between fields, and carry their own column names.
#'   These are unzipped with those separators, "NA" strings become `NA`, and the
#'   escaped separators ("**paragraph**", "**corner**") are restored.
#' - Anything else is split with the legacy separators passed in `row_sep` and
#'   `col_sep`, exactly as `separate_rows()` and `separate()` would.
#'
#' For auto-zipped data the output columns are named to match `into`: when the
#' zipped header already contains every name in `into` those names are kept; when it
#' has the same number of fields but different names, the fields are renamed to
#' `into` by position (as the legacy `separate()` would); otherwise any name in `into`
#' that is missing is added as `NA`.
#'
#' @param df a data frame holding the zipped column
#' @param col the zipped column, bare or as a string
#' @param into names for the unzipped fields (legacy order)
#' @param row_sep legacy separator between records; `NULL` when each value holds one
#'   record
#' @param col_sep legacy separator between fields (a regular expression, as in
#'   `separate()`)
#' @param fill passed to `separate()` for legacy data
#'
#' @return the data frame with `col` replaced by the unzipped fields
#' @export
#'
#' @examples
#' \dontrun{
#' analytic %>%
#'   select(study_id, followup_data) %>%
#'   unzip_long_file(followup_data,
#'                   into = c("redcap_event_name", "followup_period", "form", "status", "form_dates"),
#'                   row_sep = ";", col_sep = ",")
#' }
unzip_long_file <- function(df, col, into, row_sep = NULL, col_sep, fill = "warn") {
  col <- rlang::as_name(rlang::ensym(col))
  auto_row_sep <- "¶"
  auto_col_sep <- "┘"

  values <- df[[col]]
  values <- values[!is.na(values)]
  is_auto <- any(grepl(auto_col_sep, values, fixed = TRUE) | grepl(auto_row_sep, values, fixed = TRUE))

  if (!is_auto) {
    if (!is.null(row_sep)) {
      df <- df %>%
        separate_rows(all_of(col), sep = row_sep)
    }
    df <- df %>%
      separate(all_of(col), into = into, sep = col_sep, fill = fill)
    return(df)
  }

  df <- df %>%
    separate_rows(all_of(col), sep = auto_row_sep)

  header_source <- df[[col]][!is.na(df[[col]]) & grepl(auto_col_sep, df[[col]], fixed = TRUE)][1]
  header_parts <- strsplit(header_source, auto_col_sep, fixed = TRUE)[[1]]
  zipped_cols <- strsplit(header_parts[length(header_parts)], "-", fixed = TRUE)[[1]]

  df <- df %>%
    separate(all_of(col), into = c(zipped_cols, ".zipped_col_names"), sep = auto_col_sep,
             fill = "right") %>%
    select(-.zipped_col_names) %>%
    mutate(across(all_of(zipped_cols), ~ na_if(.x, "NA"))) %>%
    mutate(across(all_of(zipped_cols), ~ str_replace_all(.x, stringr::fixed("**paragraph**"), auto_row_sep))) %>%
    mutate(across(all_of(zipped_cols), ~ str_replace_all(.x, stringr::fixed("**corner**"), auto_col_sep)))

  if (!all(into %in% zipped_cols)) {
    if (length(into) == length(zipped_cols)) {
      df <- df %>%
        rename_with(~ into, all_of(zipped_cols))
    } else {
      for (missing_col in setdiff(into, zipped_cols)) {
        df[[missing_col]] <- NA_character_
      }
    }
  }

  return(df)
}
