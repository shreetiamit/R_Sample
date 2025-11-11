# United Hospital Fund - Data Visualization
library(tidyverse)
library(ggplot2)

cat("=== DATA VISUALIZATION ===\n")

# Load the cleaned data
analysis_data <- read_csv("data/cleaned_health_data.csv")

cat("Creating healthcare data visualizations...\n")

# Create output directory
if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# 1. Age distribution
p1 <- ggplot(analysis_data, aes(x = age)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30) +
  labs(title = "Patient Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

# 2. Blood pressure by treatment group
p2 <- ggplot(analysis_data, aes(x = treatment_group, y = blood_pressure, fill = treatment_group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Blood Pressure by Treatment Group", x = "Treatment Group", y = "Blood Pressure") +
  theme_minimal()

# 3. Outcome rates
p3 <- analysis_data %>%
  group_by(treatment_group) %>%
  summarise(outcome_rate = mean(outcome)) %>%
  ggplot(aes(x = treatment_group, y = outcome_rate, fill = treatment_group)) +
  geom_col(alpha = 0.7) +
  labs(title = "Outcome Rates by Treatment Group", x = "Treatment Group", y = "Outcome Rate") +
  theme_minimal()

# Save plots
ggsave("output/figures/age_distribution.png", p1, width = 8, height = 6)
ggsave("output/figures/bp_by_treatment.png", p2, width = 8, height = 6)
ggsave("output/figures/outcome_rates.png", p3, width = 8, height = 6)

cat("✅ VISUALIZATION COMPLETE\n")
cat("Plots saved to output/figures/\n")