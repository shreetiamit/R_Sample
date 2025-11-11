# United Hospital Fund - Research Analyst Sample
# Data Cleaning and Preparation Pipeline
# Demonstrates advanced R skills for healthcare data

library(tidyverse)
library(purrr)

cat("=== HEALTHCARE DATA CLEANING PIPELINE ===\n")

# Load custom functions
source("functions/validation.R")
source("functions/data_helpers.R")

cat("✅ Custom functions loaded successfully\n\n")

# SIMULATION: Create sample healthcare dataset (since we can't share real patient data)
set.seed(123) # For reproducibility
sample_health_data <- tibble(
  patient_id = 1:1000,
  age = sample(18:90, 1000, replace = TRUE),
  blood_pressure = rnorm(1000, mean = 120, sd = 15),
  cholesterol = rnorm(1000, mean = 200, sd = 30),  # Only ONE cholesterol column
  treatment_group = sample(c("Standard", "Experimental"), 1000, replace = TRUE),
  admission_date = sample(seq(as.Date('2020-01-01'), as.Date('2023-12-31'), by="day"), 1000),
  outcome = sample(0:1, 1000, replace = TRUE)
) %>%
  # Add missing data to existing cholesterol column (FIXED: no duplicate)
  mutate(cholesterol = ifelse(runif(1000) > 0.95, NA, cholesterol))

cat("Sample dataset created with", nrow(sample_health_data), "records\n")
cat("Columns:", paste(names(sample_health_data), collapse = ", "), "\n")

# DEMONSTRATION 1: Advanced Data Validation
cat("\n1. RUNNING DATA VALIDATION CHECKS...\n")
validation_results <- healthcare_validator(sample_health_data)
print(validation_results$summary)

# DEMONSTRATION 2: Data Quality Analysis
cat("\n2. DATA QUALITY ASSESSMENT...\n")
quality_metrics <- analyze_data_quality(sample_health_data)
print(paste("Data completeness rate:", quality_metrics$completeness_rate, "%"))

# DEMONSTRATION 3: Create Analysis-Ready Dataset
cat("\n3. CREATING ANALYSIS-READY DATASET...\n")
analysis_data <- sample_health_data %>%
  # Handle missing values
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  # Remove duplicate records
  distinct() %>%
  # Create derived variables (feature engineering)
  mutate(
    age_group = cut(age, breaks = c(0, 18, 35, 50, 65, 100),
                    labels = c("0-17", "18-34", "35-49", "50-64", "65+")),
    bp_category = case_when(
      blood_pressure < 120 ~ "Normal",
      blood_pressure < 130 ~ "Elevated", 
      blood_pressure < 140 ~ "Stage 1",
      TRUE ~ "Stage 2"
    )
  )

missing_analysis <- analyze_missing_patterns(analysis_data)
cat("Missing data analysis completed:\n")
print(missing_analysis)

cat("\n4. FINAL DATASET SUMMARY...\n")
print(paste("Final dataset dimensions:", nrow(analysis_data), "rows ×", ncol(analysis_data), "columns"))
cat("Variable types:\n")
print(map_chr(analysis_data, class))

# Create data directory if it doesn't exist
if(!dir.exists("data")) dir.create("data")

# Save cleaned data for next steps
write_csv(analysis_data, "data/cleaned_health_data.csv")

cat("\n✅ DATA CLEANING COMPLETE\n")
cat("Output saved to: data/cleaned_health_data.csv\n")
cat("Next: Run 02_analysis.R for statistical analysis\n")