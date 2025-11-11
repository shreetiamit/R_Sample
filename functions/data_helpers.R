# Custom functions for healthcare data analysis

# Calculate robust summary statistics
calculate_robust_summary <- function(data, var) {
  # Extract the variable
  var_vector <- data[[deparse(substitute(var))]]
  
  # Simple bootstrapping
  boot_means <- replicate(100, {
    sample_data <- sample(var_vector, size = length(var_vector), replace = TRUE)
    mean(sample_data, na.rm = TRUE)
  })
  
  # Return results
  list(
    variable = deparse(substitute(var)),
    mean = mean(var_vector, na.rm = TRUE),
    bootstrapped_mean = mean(boot_means),
    ci_lower = quantile(boot_means, 0.025),
    ci_upper = quantile(boot_means, 0.975),
    n = length(var_vector)
  )
}

# Data quality analysis
analyze_data_quality <- function(df) {
  list(
    total_rows = nrow(df),
    total_columns = ncol(df),
    complete_cases = sum(complete.cases(df)),
    completeness_rate = round(sum(complete.cases(df)) / nrow(df) * 100, 1)
  )
}

# Missing data analysis
analyze_missing_patterns <- function(df) {
  missing_summary <- data.frame(
    column = names(df),
    missing_count = sapply(df, function(x) sum(is.na(x))),
    missing_percent = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 1)
  )
  missing_summary[order(-missing_summary$missing_count), ]
}