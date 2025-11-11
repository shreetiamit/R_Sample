# United Hospital Fund - Research Analyst Sample
# Statistical Analysis Demonstrating Healthcare Analytics Skills

library(tidyverse)
library(purrr)
library(broom)

cat("=== STATISTICAL ANALYSIS ===\n")

# Create sample data directly (since 01_data_cleaning.R may not have run)
set.seed(456) # Different seed from main analysis
analysis_data <- tibble(
  patient_id = 1:300,
  age = sample(18:75, 300, replace = TRUE),
  blood_pressure = rnorm(300, mean = 125, sd = 12),
  cholesterol = rnorm(300, mean = 195, sd = 28),
  treatment_group = sample(c("Control", "Intervention"), 300, replace = TRUE, prob = c(0.4, 0.6)),
  readmission_risk = rnorm(300, mean = 0.3, sd = 0.1)
) %>%
  mutate(
    # Create some realistic relationships
    blood_pressure = blood_pressure + age * 0.2,
    high_risk = ifelse(readmission_risk > 0.35, 1, 0),
    # Add missing data for demonstration
    cholesterol = ifelse(runif(300) > 0.95, NA, cholesterol)
  )

cat("Created analysis dataset with", nrow(analysis_data), "patient records\n")
cat("Columns:", paste(names(analysis_data), collapse = ", "), "\n\n")

# DEMONSTRATION 1: Advanced Summary Statistics
cat("1. ADVANCED DESCRIPTIVE STATISTICS:\n")

# Custom function for comprehensive summaries
comprehensive_summary <- function(data, var) {
  var_vector <- data %>% pull({{var}})
  list(
    variable = deparse(substitute(var)),
    n = length(var_vector),
    mean = mean(var_vector, na.rm = TRUE),
    median = median(var_vector, na.rm = TRUE),
    sd = sd(var_vector, na.rm = TRUE),
    min = min(var_vector, na.rm = TRUE),
    max = max(var_vector, na.rm = TRUE),
    missing = sum(is.na(var_vector)),
    missing_pct = round(sum(is.na(var_vector)) / length(var_vector) * 100, 1)
  )
}

# Apply to multiple variables using functional programming
variables_to_analyze <- c("age", "blood_pressure", "cholesterol", "readmission_risk")

cat("Summary statistics for key clinical variables:\n")
summary_results <- map(variables_to_analyze, ~{
  var_name <- .x
  result <- comprehensive_summary(analysis_data, !!sym(var_name))
  cat(paste0("\n• ", var_name, ":\n"))
  cat(paste0("  Mean: ", round(result$mean, 2), 
             ", SD: ", round(result$sd, 2),
             ", Missing: ", result$missing, " (", result$missing_pct, "%)"))
  result
})

# DEMONSTRATION 2: Statistical Hypothesis Testing
cat("\n\n2. STATISTICAL HYPOTHESIS TESTING:\n")

# T-test: Blood pressure by treatment group
cat("• T-test: Blood Pressure by Treatment Group\n")
ttest_bp <- t.test(blood_pressure ~ treatment_group, data = analysis_data)
print(ttest_bp)

# Chi-square test: High risk by treatment group
cat("\n• Chi-square Test: High Risk Status by Treatment Group\n")
risk_table <- table(analysis_data$treatment_group, analysis_data$high_risk)
chi_test <- chisq.test(risk_table)
print(chi_test)

# Correlation analysis
cat("\n• Correlation: Age vs Blood Pressure\n")
cor_test <- cor.test(analysis_data$age, analysis_data$blood_pressure, method = "pearson")
print(cor_test)

# DEMONSTRATION 3: Advanced Statistical Modeling
cat("\n3. PREDICTIVE MODELING:\n")

# Logistic regression for high risk prediction
cat("• Logistic Regression: Predicting High Readmission Risk\n")
logit_model <- glm(high_risk ~ age + blood_pressure + treatment_group,
                   data = analysis_data, family = binomial)

model_summary <- broom::tidy(logit_model) %>%
  mutate(
    odds_ratio = round(exp(estimate), 3),
    p_value = round(p.value, 4),
    significance = ifelse(p.value < 0.05, "**", ifelse(p.value < 0.1, "*", ""))
  )

print("Logistic Regression Results:")
print(model_summary %>% select(term, estimate, odds_ratio, p_value, significance))

# Linear regression for blood pressure
cat("\n• Linear Regression: Blood Pressure Predictors\n")
lm_model <- lm(blood_pressure ~ age + treatment_group, data = analysis_data)
lm_summary <- broom::tidy(lm_model) %>%
  mutate(p_value = round(p.value, 4),
         significance = ifelse(p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")))

print("Linear Regression Results:")
print(lm_summary %>% select(term, estimate, std.error, p_value, significance))

# DEMONSTRATION 4: Group-wise Analysis with Functional Programming
cat("\n4. GROUP-WISE ANALYSIS USING FUNCTIONAL PROGRAMMING:\n")

# Analyze multiple variables by treatment group
analyze_by_group <- function(data, group_var, analysis_vars) {
  group_var_sym <- sym(group_var)
  
  analysis_vars %>%
    map_df(~{
      var_sym <- sym(.x)
      data %>%
        group_by(!!group_var_sym) %>%
        summarise(
          variable = .x,
          mean = mean(!!var_sym, na.rm = TRUE),
          sd = sd(!!var_sym, na.rm = TRUE),
          n = n(),
          .groups = 'drop'
        )
    })
}

group_analysis <- analyze_by_group(analysis_data, "treatment_group", 
                                   c("age", "blood_pressure", "cholesterol", "readmission_risk"))

print("Group-wise Analysis Results:")
print(group_analysis)

# Save analysis results
if(!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)
write_csv(model_summary, "output/tables/logistic_model_results.csv")
write_csv(lm_summary, "output/tables/linear_model_results.csv")
write_csv(group_analysis, "output/tables/group_analysis_results.csv")

cat("\n✅ STATISTICAL ANALYSIS COMPLETE\n")
cat("• Advanced descriptive statistics\n")
cat("• Hypothesis testing (t-tests, chi-square, correlation)\n")
cat("• Predictive modeling (logistic & linear regression)\n")
cat("• Functional programming for group analysis\n")
cat("• Results saved to output/tables/\n")