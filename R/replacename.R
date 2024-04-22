replacename <- function(data, column, pattern_replacements, na_action = "keep", verbose = FALSE) {
  validate_df_cols(data, column)

  # Convert factor to character if necessary
  if (is.factor(data[[column]])) {
    data[[column]] <- as.character(data[[column]])
  }

  # Ensure pattern_replacements is a named vector
  if (!is.vector(pattern_replacements) || is.null(names(pattern_replacements))) {
    stop("pattern_replacements must be a named vector.", call. = FALSE)
  }

  original_data <- data[[column]]  # Define original_data
  transformed_column <- original_data
  patterns_matched <- logical(length = length(transformed_column))

  for (pattern in names(pattern_replacements)) {
    matches <- str_detect(transformed_column, pattern)
    patterns_matched <- patterns_matched | matches
    transformed_column[matches] <- pattern_replacements[[pattern]]
  }

  if (na_action == "remove") {
    data <- data[!is.na(transformed_column), , drop = FALSE]
  } else if (na_action != "keep") {
    transformed_column[is.na(transformed_column)] <- na_action
  }

  data[[column]] <- transformed_column

  if (verbose) {
    changed_indices <- which(original_data != transformed_column)
    if (length(changed_indices) > 0) {
      message("Changed values in column '", column, "' at indices: ", paste(changed_indices, collapse = ", "))
    } else {
      message("No changes were made in column '", column, "'.")
    }
  }

  return(data)
}
