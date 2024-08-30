#' Collapse survey data and compute proportions
#'
#' This function collapses survey data and computes proportions of responses for a given variable.
#'
#' @param data A data frame containing the survey data.
#' @param element_var The column name representing the dependent variable, if not present in data.
#' @param response_category The column name representing the response_category variable, if not present in data.
#' @param wgt The column name representing the weights, if not present in data.
#' @param group_var (Optional) The column name representing the grouping variable.
#'
#' @import survey
#' @import rlang
#' @importFrom srvyr as_survey_design
#' @importFrom srvyr survey_mean
#' @return A summarized data frame with proportions of responses.
#' @export
#'

svycollapse <- function(data, element_var = NULL, response_category = NULL, wgt = NULL, groupby_var = NULL,
                        data_shape=NULL) {
  # Check if columns exist in data, if not, use the provided parameters
  if (!all(c("response_category", "wgt") %in% names(data))) {
    if(is.null(response_category) || is.null(wgt)) {
      stop("missing response_category or wgt columns")
    }
  } else {
    element_var <- "element_var"
    response_category <- "response_category"
    wgt <- "wgt"
  }

  if(data_shape=="long" & !is.null(groupby_var)) {
    data %>%
      filter(!is.na(.data[[groupby_var]]), !is.na(.data[[element_var]]), !is.na(.data[[response_category]])) %>%
      as_survey_design(ids = 1, weights = .data[[wgt]]) %>%
      group_by(.data[[groupby_var]], .data[[element_var]], .data[[response_category]]) %>%
      summarize(prop = 100 * survey_mean(variable = .data[[response_category]], na.rm = TRUE, vartype="ci"), .groups = 'drop') %>%
      #filter(.data[[response_category]] == 100 | .data[[response_category]] == 1) %>%
      mutate(proplabel = paste0(round(prop, 1), "%")) %>%
      dplyr::rename(groupby_var:=!!groupby_var)
  }
  else if(data_shape=="long" & is.null(groupby_var)) {
    data %>%
      filter(!is.na(.data[[element_var]]), !is.na(.data[[response_category]])) %>%
      as_survey_design(ids = 1, weights = .data[[wgt]]) %>%
      group_by(.data[[element_var]], .data[[response_category]]) %>%
      summarize(prop = 100 * survey_mean(variable = .data[[response_category]], na.rm = TRUE, vartype="ci"), .groups = 'drop') %>%
      #filter(.data[[response_category]] == 100 | .data[[response_category]] == 1) %>%
      mutate(proplabel = paste0(round(prop, 1), "%"))
  }
  else if(data_shape=="wide" & !is.null(groupby_var)) {
    data %>%
      filter(!is.na(.data[[groupby_var]]), !is.na(.data[[response_category]])) %>%
      as_survey_design(ids = 1, weights = .data[[wgt]]) %>%
      group_by(.data[[groupby_var]], .data[[response_category]]) %>%
      summarize(prop = 100 * survey_mean(variable = .data[[response_category]], na.rm = TRUE, vartype="ci"), .groups = 'drop') %>%
      #filter(.data[[response_category]] == 100 | .data[[response_category]] == 1) %>%
      mutate(proplabel = paste0(round(prop, 1), "%")) %>%
      dplyr::rename(element_var:=!!response_category,
                    groupby_var:=!!groupby_var)
  }
  else if(data_shape=="wide" & is.null(groupby_var)) {
    data %>%
      filter(!is.na(.data[[response_category]])) %>%
      as_survey_design(ids = 1, weights = .data[[wgt]]) %>%
      group_by(.data[[response_category]]) %>%
      summarize(prop = 100 * survey_mean(variable = .data[[response_category]], na.rm = TRUE, vartype="ci"), .groups = 'drop') %>%
      #filter(.data[[response_category]] == 100 | .data[[response_category]] == 1) %>%
      mutate(proplabel = paste0(round(prop, 1), "%")) %>%
      dplyr::rename(element_var:=!!response_category)
  }
}

