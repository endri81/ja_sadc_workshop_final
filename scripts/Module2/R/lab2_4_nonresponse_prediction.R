################################################################################
#                                                                              #
#   SADC REGIONAL TRAINING WORKSHOP ON ADVANCED SAMPLING METHODS               #
#   Lab 2.4: AI/ML for Non-Response Prediction                                 #
#                                                                              #
#   R VERSION - Random Forest Model                                            #
#                                                                              #
#   Objective: Build a predictive model to identify households likely to       #
#              refuse participation, enabling targeted follow-up strategies    #
#                                                                              #
#   Data: Statistics South Africa General Household Survey 2024                #
#   Software: R 4.x with randomForest, caret, haven, dplyr, ggplot2           #
#                                                                              #
#   Author: SADC Workshop Materials                                            #
#   Date: March 2026                                                           #
#                                                                              #
################################################################################

# =============================================================================
# CONFIGURATION - MODIFY PATHS AS NEEDED
# =============================================================================

config <- list(
  # Data path - adjust to your local directory
  survey_path = "./data/GHS_2024/ghs-2024-hhold-v1.dta",
  
  # Output directory for results
  output_dir = "./outputs/",
  
  # Random seed for reproducibility
  seed = 42,
  
  # Train/test split ratio
  train_ratio = 0.7,
  
  # Number of trees for Random Forest
  n_trees = 500
)

# Create output directory if it doesn't exist
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

# =============================================================================
# PART 0: INSTALL AND LOAD REQUIRED PACKAGES
# =============================================================================

cat(strrep("=", 70), "\n")
cat("LAB 2.4: AI/ML FOR NON-RESPONSE PREDICTION (R - RANDOM FOREST)\n")
cat(strrep("=", 70), "\n\n")

# Function to install packages if not available
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, repos = "https://cran.r-project.org")
  }
}

# Required packages
required_packages <- c(
  "haven",         # Read STATA files
  "dplyr",         # Data manipulation
  "tidyr",         # Data tidying
  "randomForest",  # Random Forest algorithm
  "caret",         # Model training utilities
  "pROC",          # ROC curves
  "ggplot2",       # Visualization
  "scales"         # Formatting
)

install_if_missing(required_packages)

# Load packages
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(randomForest)
  library(caret)
  library(pROC)
  library(ggplot2)
  library(scales)
})

cat("All packages loaded successfully.\n\n")

# Set random seed for reproducibility
set.seed(config$seed)

# =============================================================================
# PART 1: LOAD AND EXPLORE DATA
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 1: Loading and Exploring GHS 2024 Data\n")
cat(strrep("-", 70), "\n\n")

# Load the household data
ghs_hh <- read_dta(config$survey_path)

cat("Dataset loaded successfully!\n")
cat("  Observations:", format(nrow(ghs_hh), big.mark = ","), "\n")
cat("  Variables:", ncol(ghs_hh), "\n\n")

# =============================================================================
# PART 2: SIMULATE NON-RESPONSE VARIABLE
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 2: Simulating Non-Response Patterns\n")
cat(strrep("-", 70), "\n\n")

# In real surveys, non-response is correlated with:
# - Income (higher income -> higher refusal rates - "too busy", privacy concerns)
# - Urban areas (more refusals than rural)
# - Household size (smaller households harder to contact)
# - Education (higher education -> more privacy conscious)
# - Age of household head (varies by survey type)

# We'll simulate realistic non-response based on these factors

# First, detect variable names (lowercase vs uppercase)
detect_varname <- function(data, possibilities) {
  for (var in possibilities) {
    if (var %in% names(data)) return(var)
  }
  return(NULL)
}

# Detect key variables
var_income <- detect_varname(ghs_hh, c("totmhinc", "TOTMHINC", "total_income"))
var_geotype <- detect_varname(ghs_hh, c("geotype", "GEOTYPE", "geo_type"))
var_hhsize <- detect_varname(ghs_hh, c("hholdsz", "HHOLDSZ", "hh_size", "hhsize"))
var_prov <- detect_varname(ghs_hh, c("prov", "PROV", "province"))

cat("Variables detected:\n")
cat("  Income:", var_income, "\n")
cat("  Geography type:", var_geotype, "\n")
cat("  Household size:", var_hhsize, "\n")
cat("  Province:", var_prov, "\n\n")

# Create standardized variable names
ghs_hh <- ghs_hh %>%
  rename(
    income = !!sym(var_income),
    geotype = !!sym(var_geotype),
    hhsize = !!sym(var_hhsize),
    province = !!sym(var_prov)
  )

# Handle missing income values
ghs_hh <- ghs_hh %>%
  mutate(
    # Replace missing/negative income with median
    income_clean = ifelse(is.na(income) | income < 0, 
                          median(income, na.rm = TRUE), 
                          income),
    # Log transform income (handle zeros)
    log_income = log1p(income_clean)
  )

# Create income quintiles
ghs_hh <- ghs_hh %>%
  mutate(
    income_quintile = ntile(income_clean, 5)
  )

# Simulate non-response probability
# Base refusal rate: 10%
# Higher income: +15% for Q5, +10% for Q4
# Urban areas: +5% compared to rural
# Small households (1-2): +3% (harder to contact)

ghs_hh <- ghs_hh %>%
  mutate(
    # Base probability
    prob_refuse = 0.10,
    
    # Income effect (higher income -> higher refusal)
    prob_refuse = prob_refuse + case_when(
      income_quintile == 5 ~ 0.15,  # Top quintile: +15%
      income_quintile == 4 ~ 0.10,  # 4th quintile: +10%
      income_quintile == 3 ~ 0.05,  # Middle: +5%
      income_quintile == 2 ~ 0.02,  # 2nd quintile: +2%
      TRUE ~ 0                       # Bottom quintile: no additional
    ),
    
    # Urban effect (geotype == 1 is typically urban)
    prob_refuse = prob_refuse + ifelse(geotype == 1, 0.05, 0),
    
    # Small household effect
    prob_refuse = prob_refuse + ifelse(hhsize <= 2, 0.03, 0),
    
    # Provincial variation (some provinces have historically higher refusal)
    # Province 7 (Gauteng) typically has higher refusal rates
    prob_refuse = prob_refuse + ifelse(province == 7, 0.05, 0),
    
    # Add random noise
    prob_refuse = prob_refuse + rnorm(n(), 0, 0.02),
    
    # Bound probabilities between 0.01 and 0.50
    prob_refuse = pmax(0.01, pmin(0.50, prob_refuse)),
    
    # Generate binary outcome (1 = refused, 0 = responded)
    refused = rbinom(n(), 1, prob_refuse)
  )

# Summary statistics
cat("Non-Response Simulation Summary:\n")
cat("  Total households:", format(nrow(ghs_hh), big.mark = ","), "\n")
cat("  Simulated refusals:", format(sum(ghs_hh$refused), big.mark = ","), "\n")
cat("  Overall refusal rate:", 
    sprintf("%.1f%%", 100 * mean(ghs_hh$refused)), "\n\n")

# Refusal rate by income quintile
refusal_by_income <- ghs_hh %>%
  group_by(income_quintile) %>%
  summarise(
    n = n(),
    n_refused = sum(refused),
    refusal_rate = mean(refused) * 100,
    .groups = "drop"
  )

cat("Refusal Rate by Income Quintile:\n")
print(as.data.frame(refusal_by_income), row.names = FALSE)
cat("\n")

# =============================================================================
# PART 3: PREPARE FEATURES FOR MODEL
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 3: Preparing Features for Random Forest Model\n")
cat(strrep("-", 70), "\n\n")

# Select and create features for the model
# We use variables that would be known BEFORE fieldwork (from frame)

model_data <- ghs_hh %>%
  select(
    # Target variable
    refused,
    
    # Frame-level variables (known before fieldwork)
    province,
    geotype,
    
    # From previous census or administrative data
    income_quintile,
    log_income,
    hhsize
  ) %>%
  # Remove any rows with missing values
  drop_na()

cat("Model dataset prepared:\n")
cat("  Observations:", format(nrow(model_data), big.mark = ","), "\n")
cat("  Features:", ncol(model_data) - 1, "\n\n")

# Convert categorical variables to factors
model_data <- model_data %>%
  mutate(
    refused = factor(refused, levels = c(0, 1), labels = c("Responded", "Refused")),
    province = factor(province),
    geotype = factor(geotype),
    income_quintile = factor(income_quintile)
  )

# Feature summary
cat("Feature Summary:\n")
cat("  Province levels:", nlevels(model_data$province), "\n")
cat("  Geotype levels:", nlevels(model_data$geotype), "\n")
cat("  Income quintile levels:", nlevels(model_data$income_quintile), "\n")
cat("  Log income range:", 
    sprintf("%.2f to %.2f", min(model_data$log_income), max(model_data$log_income)), "\n")
cat("  Household size range:", 
    min(model_data$hhsize), "to", max(model_data$hhsize), "\n\n")

# =============================================================================
# PART 4: TRAIN-TEST SPLIT
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 4: Creating Train-Test Split\n")
cat(strrep("-", 70), "\n\n")

# Create stratified train-test split (stratify by refused)
train_index <- createDataPartition(
  model_data$refused, 
  p = config$train_ratio, 
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("Train-Test Split:\n")
cat("  Training set:", format(nrow(train_data), big.mark = ","), 
    sprintf("(%.0f%%)", 100 * nrow(train_data) / nrow(model_data)), "\n")
cat("  Test set:", format(nrow(test_data), big.mark = ","),
    sprintf("(%.0f%%)", 100 * nrow(test_data) / nrow(model_data)), "\n\n")

# Check class balance
cat("Class Distribution:\n")
cat("  Training - Refused:", 
    sprintf("%.1f%%", 100 * sum(train_data$refused == "Refused") / nrow(train_data)), "\n")
cat("  Test - Refused:", 
    sprintf("%.1f%%", 100 * sum(test_data$refused == "Refused") / nrow(test_data)), "\n\n")

# =============================================================================
# PART 5: TRAIN RANDOM FOREST MODEL
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 5: Training Random Forest Model\n")
cat(strrep("-", 70), "\n\n")

cat("Training Random Forest with", config$n_trees, "trees...\n")
cat("This may take a minute...\n\n")

# Train Random Forest model
rf_model <- randomForest(
  refused ~ province + geotype + income_quintile + log_income + hhsize,
  data = train_data,
  ntree = config$n_trees,
  mtry = 2,                    # Number of variables tried at each split
  importance = TRUE,           # Calculate variable importance
  na.action = na.omit
)

cat("Model trained successfully!\n\n")

# Print model summary
print(rf_model)
cat("\n")

# =============================================================================
# PART 6: EXTRACT FEATURE IMPORTANCE
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 6: Feature Importance Analysis\n")
cat(strrep("-", 70), "\n\n")

# Extract importance measures
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>%
  arrange(desc(MeanDecreaseAccuracy))

cat("FEATURE IMPORTANCE (Ranked by Mean Decrease in Accuracy):\n")
cat(strrep("=", 60), "\n")
cat(sprintf("%-20s %20s %20s\n", "Feature", "Accuracy Impact", "Gini Impact"))
cat(strrep("-", 60), "\n")
for (i in 1:nrow(importance_df)) {
  cat(sprintf("%-20s %20.2f %20.2f\n", 
              importance_df$Feature[i],
              importance_df$MeanDecreaseAccuracy[i],
              importance_df$MeanDecreaseGini[i]))
}
cat(strrep("=", 60), "\n\n")

# Interpretation
cat("INTERPRETATION:\n")
cat(strrep("-", 60), "\n")
cat("Higher values indicate stronger predictive power.\n\n")

top_feature <- importance_df$Feature[1]
cat("Top Predictor:", top_feature, "\n")
cat("  This variable contributes most to predicting non-response.\n\n")

# Create importance plot
importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), 
                                              y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "#003366") +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "Variables Predicting Survey Non-Response",
    x = "Feature",
    y = "Mean Decrease in Accuracy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )

# Save plot
ggsave(
  filename = file.path(config$output_dir, "lab2_4_feature_importance.png"),
  plot = importance_plot,
  width = 8,
  height = 5,
  dpi = 150
)
cat("Feature importance plot saved to:", 
    file.path(config$output_dir, "lab2_4_feature_importance.png"), "\n\n")

# =============================================================================
# PART 7: MODEL EVALUATION
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 7: Model Evaluation on Test Set\n")
cat(strrep("-", 70), "\n\n")

# Predict on test set
test_predictions <- predict(rf_model, test_data, type = "response")
test_probabilities <- predict(rf_model, test_data, type = "prob")[, "Refused"]

# Confusion matrix
conf_matrix <- confusionMatrix(test_predictions, test_data$refused, positive = "Refused")

cat("CONFUSION MATRIX:\n")
print(conf_matrix$table)
cat("\n")

cat("MODEL PERFORMANCE METRICS:\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Accuracy:    %.1f%%\n", 100 * conf_matrix$overall["Accuracy"]))
cat(sprintf("  Sensitivity: %.1f%% (ability to detect refusals)\n", 
            100 * conf_matrix$byClass["Sensitivity"]))
cat(sprintf("  Specificity: %.1f%% (ability to identify respondents)\n", 
            100 * conf_matrix$byClass["Specificity"]))
cat(sprintf("  Precision:   %.1f%% (when we predict refusal, how often correct)\n", 
            100 * conf_matrix$byClass["Precision"]))
cat("\n")

# ROC Curve
roc_obj <- roc(test_data$refused, test_probabilities, levels = c("Responded", "Refused"))
auc_value <- auc(roc_obj)

cat(sprintf("  AUC (Area Under ROC Curve): %.3f\n", auc_value))
cat("    Interpretation: AUC > 0.7 indicates good discrimination\n\n")

# Create ROC plot
roc_df <- data.frame(
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities
)

roc_plot <- ggplot(roc_df, aes(x = 1 - Specificity, y = Sensitivity)) +
  geom_line(color = "#003366", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curve for Non-Response Prediction",
    subtitle = sprintf("AUC = %.3f", auc_value),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  ) +
  annotate("text", x = 0.6, y = 0.3, 
           label = sprintf("AUC = %.3f", auc_value),
           size = 5, fontface = "bold")

ggsave(
  filename = file.path(config$output_dir, "lab2_4_roc_curve.png"),
  plot = roc_plot,
  width = 7,
  height = 6,
  dpi = 150
)
cat("ROC curve saved to:", file.path(config$output_dir, "lab2_4_roc_curve.png"), "\n\n")

# =============================================================================
# PART 8: IDENTIFY HIGH-RISK HOUSEHOLDS
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 8: Identifying High-Risk Households for Follow-up\n")
cat(strrep("-", 70), "\n\n")

# Apply model to full dataset to get risk scores
full_probabilities <- predict(rf_model, model_data, type = "prob")[, "Refused"]

# Add risk scores to data
model_data$risk_score <- full_probabilities
model_data$risk_category <- cut(
  full_probabilities,
  breaks = c(0, 0.15, 0.25, 0.35, 1),
  labels = c("Low", "Medium", "High", "Very High"),
  include.lowest = TRUE
)

# Risk category distribution
risk_summary <- model_data %>%
  group_by(risk_category) %>%
  summarise(
    n_households = n(),
    pct_households = n() / nrow(model_data) * 100,
    actual_refusal_rate = sum(refused == "Refused") / n() * 100,
    .groups = "drop"
  )

cat("RISK STRATIFICATION RESULTS:\n")
cat(strrep("=", 70), "\n")
print(as.data.frame(risk_summary), row.names = FALSE)
cat("\n")

cat("OPERATIONAL RECOMMENDATIONS:\n")
cat(strrep("-", 70), "\n")
cat("Based on the model predictions:\n\n")

very_high_n <- sum(model_data$risk_category == "Very High", na.rm = TRUE)
high_n <- sum(model_data$risk_category == "High", na.rm = TRUE)

cat("1. VERY HIGH RISK households:", format(very_high_n, big.mark = ","), "\n")
cat("   -> Schedule senior interviewers\n")
cat("   -> Plan multiple contact attempts\n")
cat("   -> Prepare incentive protocols\n\n")

cat("2. HIGH RISK households:", format(high_n, big.mark = ","), "\n")
cat("   -> Assign experienced interviewers\n")
cat("   -> Allow flexible appointment times\n\n")

cat("3. MEDIUM/LOW RISK households:\n")
cat("   -> Standard interview protocols\n")
cat("   -> Regular follow-up procedures\n\n")

# =============================================================================
# PART 9: EXPORT RESULTS
# =============================================================================

cat(strrep("-", 70), "\n")
cat("PART 9: Exporting Results\n")
cat(strrep("-", 70), "\n\n")

# Export feature importance
write.csv(
  importance_df,
  file = file.path(config$output_dir, "lab2_4_feature_importance.csv"),
  row.names = FALSE
)
cat("Feature importance saved to:", 
    file.path(config$output_dir, "lab2_4_feature_importance.csv"), "\n")

# Export risk scores (sample)
risk_export <- model_data %>%
  select(province, geotype, income_quintile, hhsize, risk_score, risk_category) %>%
  head(1000)  # Export first 1000 for demonstration

write.csv(
  risk_export,
  file = file.path(config$output_dir, "lab2_4_risk_scores_sample.csv"),
  row.names = FALSE
)
cat("Risk scores (sample) saved to:", 
    file.path(config$output_dir, "lab2_4_risk_scores_sample.csv"), "\n")

# Export model summary
model_summary <- list(
  model_type = "Random Forest",
  n_trees = config$n_trees,
  n_observations = nrow(model_data),
  n_train = nrow(train_data),
  n_test = nrow(test_data),
  accuracy = conf_matrix$overall["Accuracy"],
  auc = as.numeric(auc_value),
  top_features = importance_df$Feature[1:3]
)

saveRDS(rf_model, file = file.path(config$output_dir, "lab2_4_rf_model.rds"))
cat("Model object saved to:", file.path(config$output_dir, "lab2_4_rf_model.rds"), "\n\n")

# =============================================================================
# PART 10: SUMMARY
# =============================================================================

cat(strrep("=", 70), "\n")
cat("LAB 2.4 COMPLETE: AI/ML Non-Response Prediction (R)\n")
cat(strrep("=", 70), "\n\n")

cat("SUMMARY:\n")
cat(strrep("-", 40), "\n")
cat("Model: Random Forest with", config$n_trees, "trees\n")
cat("Training observations:", format(nrow(train_data), big.mark = ","), "\n")
cat("Test observations:", format(nrow(test_data), big.mark = ","), "\n")
cat("Accuracy:", sprintf("%.1f%%", 100 * conf_matrix$overall["Accuracy"]), "\n")
cat("AUC:", sprintf("%.3f", auc_value), "\n\n")

cat("TOP 3 PREDICTORS OF NON-RESPONSE:\n")
for (i in 1:min(3, nrow(importance_df))) {
  cat("  ", i, ". ", importance_df$Feature[i], "\n", sep = "")
}
cat("\n")

cat("OUTPUT FILES:\n")
cat("  - lab2_4_feature_importance.png (importance plot)\n")
cat("  - lab2_4_feature_importance.csv (importance values)\n")
cat("  - lab2_4_roc_curve.png (ROC curve)\n")
cat("  - lab2_4_risk_scores_sample.csv (household risk scores)\n")
cat("  - lab2_4_rf_model.rds (saved model object)\n\n")

cat("KEY INSIGHT:\n")
cat(strrep("-", 40), "\n")
cat("The model can identify households with elevated refusal risk\n")
cat("BEFORE fieldwork begins, enabling:\n")
cat("  - Targeted interviewer assignment\n")
cat("  - Optimized contact strategies\n")
cat("  - Proactive non-response mitigation\n\n")

cat(strrep("=", 70), "\n")
cat("END OF LAB 2.4 (R VERSION)\n")
cat(strrep("=", 70), "\n")

################################################################################
# END OF SCRIPT
################################################################################
