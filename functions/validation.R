# ADVANCED R DEMONSTRATION: Higher-order functions for data validation
# These demonstrate the "advanced R" and "programming mindset" required by UHF

#' Create a data validator with custom checks
#' This is a HIGHER-ORDER FUNCTION that returns another function
#' @param checks List of validation functions
#' @return A validator function that can be applied to any dataframe
create_data_validator <- function(checks) {
  function(df) {
    results <- purrr::map(checks, ~{
      tryCatch({
        .x(df)  # Execute the check
        list(passed = TRUE, message = "OK")
      }, error = function(e) {
        list(passed = FALSE, message = e$message)
      })
    })
    
    # Name the results and provide summary
    names(results) <- names(checks)
    list(
      detailed_results = results,
      all_passed = all(purrr::map_lgl(results, "passed")),
      summary = paste0("Passed ", sum(purrr::map_lgl(results, "passed")), 
                       " of ", length(checks), " checks")
    )
  }
}

# Create healthcare-specific validator
healthcare_validator <- create_data_validator(list(
  "no_negative_ages" = function(df) {
    if(any(df$age < 0, na.rm = TRUE)) stop("Negative ages found in data")
  },
  "no_future_dates" = function(df) {
    date_cols <- df %>% select(where(lubridate::is.Date)) %>% names()
    for(col in date_cols) {
      if(any(df[[col]] > Sys.Date(), na.rm = TRUE)) 
        stop("Future dates found in ", col)
    }
  },
  "reasonable_numeric_ranges" = function(df) {
    # Domain-specific range validation
    if("blood_pressure" %in% names(df)) {
      if(any(df$blood_pressure > 300 | df$blood_pressure < 20, na.rm = TRUE))
        stop("Implausible blood pressure values")
    }
  }
))

#' Advanced data quality summary using functional programming
analyze_data_quality <- function(df) {
  quality_metrics <- list(
    total_rows = nrow(df),
    complete_cases = sum(complete.cases(df)),
    missing_by_column = purrr::map_df(df, ~sum(is.na(.))) %>%
      tidyr::pivot_longer(everything(), names_to = "column", values_to = "missing_count")
  )
  
  quality_metrics$completeness_rate <- round(
    quality_metrics$complete_cases / quality_metrics$total_rows * 100, 2
  )
  
  return(quality_metrics)
}