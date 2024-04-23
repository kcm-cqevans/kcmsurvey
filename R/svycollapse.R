svycollapse <- function(data, depvar = NULL, response = NULL, wgt = NULL, group_var = NULL) {
  # Check if columns exist in data, if not, use the provided parameters
  if (!all(c("depvar", "response", "wgt") %in% names(data))) {
    if(is.null(depvar) || is.null(response) || is.null(wgt)) {
      stop("missing depvar, response or wgt columns")
    }
  } else {
    depvar <- "depvar"
    response <- "response"
    wgt <- "wgt"
  }

  # Adjusted to directly use column names rather than data$column
  if (!is.null(group_var)) {
    data %>%
      filter(!is.na(.data[[group_var]]), !is.na(.data[[depvar]]), !is.na(.data[[response]])) %>%
      as_survey_design(ids = 1, weights = .data[[wgt]]) %>%
      group_by(.data[[group_var]], .data[[depvar]], .data[[response]]) %>%
      summarize(prop = 100 * survey_mean(variable = .data[[response]], na.rm = TRUE), .groups = 'drop') %>%
      filter(.data[[response]] == 100 | .data[[response]] == 1) %>%
      mutate(proplabel = paste0(round(prop, 1), "%"))
  } else {
    data %>%
      filter(!is.na(.data[[depvar]]), !is.na(.data[[response]])) %>%
      as_survey_design(ids = 1, weights = .data[[wgt]]) %>%
      group_by(.data[[depvar]], .data[[response]]) %>%
      summarize(prop = 100 * survey_mean(variable = .data[[response]], na.rm = TRUE), .groups = 'drop') %>%
      filter(.data[[response]] == 100 | .data[[response]] == 1) %>%
      mutate(proplabel = paste0(round(prop, 1), "%"))
  }
}
