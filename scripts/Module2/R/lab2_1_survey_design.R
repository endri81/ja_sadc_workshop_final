################################################################################
#                                                                              #
#   SADC REGIONAL TRAINING WORKSHOP ON ADVANCED SAMPLING METHODS               #
#   Day 2: Computational Tools and Spatial Integration                         #
#                                                                              #
#   LAB 2.1: DEFINING COMPLEX SURVEY DESIGN OBJECTS IN R                       #
#                                                                              #
#   Republic of Zambara — Household Living Conditions Survey                   #
#   Data Source: Zambara National Statistics Office (ZNSO)                     #
#   [Based on Statistics South Africa GHS 2024]                                #
#                                                                              #
################################################################################
#                                                                              #
#   Author:      Prof. Endri Raço (Consultant)                                 #
#   Workshop:    SADC Advanced Sampling Methods, March 2026                    #
#   Location:    Johannesburg, South Africa                                    #
#                                                                              #
#   Learning Objectives:                                                       #
#   LO2.1: Configure survey design objects in R                                #
#   LO2.2: Understand the consequences of ignoring complex design              #
#   LO2.3: Compare naive vs. design-based standard errors                      #
#                                                                              #
#   Narrative Context:                                                         #
#   Lindiwe Moyo, Chief Survey Methodologist at ZNSO, discovers that her       #
#   team has been analyzing household survey data using simple random          #
#   sampling assumptions. She tasks Thabo to demonstrate why this produces     #
#   incorrect standard errors and how to properly specify the complex          #
#   survey design in R.                                                        #
#                                                                              #
################################################################################

# ==============================================================================
# SECTION 0: ENVIRONMENT SETUP
# ==============================================================================

# Clear workspace
rm(list = ls())

# Set seed for reproducibility
set.seed(2026)

# Define output directory for results
output_dir <- "./lab2_1_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Install and load required packages
# ------------------------------------------------------------------------------

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, repos = "https://cran.r-project.org")
  }
}

# Required packages for this lab
required_packages <- c(
  "survey",      # Core survey analysis package (Lumley 2010)
  "srvyr",       # Tidyverse-compatible survey analysis
  "haven",       # Read STATA .dta files
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "knitr",       # Table formatting
  "kableExtra",  # Enhanced tables
  "ggplot2",     # Visualization
  "scales"       # Number formatting
)

# Install missing packages
install_if_missing(required_packages)

# Load packages
library(survey)
library(srvyr)
library(haven)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)

# Print session info for reproducibility
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SESSION INFORMATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n")
cat("Survey Package Version:", as.character(packageVersion("survey")), "\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ==============================================================================
# SECTION 1: DATA LOADING AND EXPLORATION
# ==============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: LOADING ZAMBARA HOUSEHOLD SURVEY DATA\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 1.1 Load the Zambara (GHS 2024) household data
# ------------------------------------------------------------------------------

# Define data path - adjust as needed for your environment
data_path <- "./data/GHS_2024/ghs-2024-hhold-v1.dta"

# Check if file exists
if (!file.exists(data_path)) {
  # Try alternative paths
  alt_paths <- c(
    "./data/ghs-2024-hhold-v1.dta",
    "../data/ghs-2024-hhold-v1.dta",
    "~/data/ghs-2024-hhold-v1.dta"
  )
  
  for (path in alt_paths) {
    if (file.exists(path)) {
      data_path <- path
      break
    }
  }
}

# Load the data
cat("Loading data from:", data_path, "\n\n")
zambara_hh <- read_dta(data_path)

# ------------------------------------------------------------------------------
# 1.2 Initial data exploration
# ------------------------------------------------------------------------------

cat("DATASET OVERVIEW\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("Number of households:", format(nrow(zambara_hh), big.mark = ","), "\n")
cat("Number of variables:", ncol(zambara_hh), "\n\n")

# ------------------------------------------------------------------------------
# 1.3 Examine key sampling design variables
# ------------------------------------------------------------------------------

# Key variables for complex survey design:
# - psu: Primary Sampling Unit (cluster identifier)
# - stratum: Stratification variable (province × geography type combinations)
# - house_wgt: Household sampling weight (inverse selection probability)
# - prov: Province (domain variable)
# - geotype: Geography type (Urban/Traditional/Farms)

cat("KEY SAMPLING DESIGN VARIABLES\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Check for presence of key variables
key_vars <- c("psu", "stratum", "house_wgt", "prov", "geotype")
vars_present <- key_vars %in% names(zambara_hh)
names(vars_present) <- key_vars

cat("\nVariable availability check:\n")
for (i in seq_along(key_vars)) {
  status <- ifelse(vars_present[i], "✓ Present", "✗ MISSING")
  cat(sprintf("  %-12s: %s\n", key_vars[i], status))
}

# ------------------------------------------------------------------------------
# 1.4 Summarize design variables
# ------------------------------------------------------------------------------

cat("\n\nDESIGN VARIABLE SUMMARY\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Number of PSUs (clusters)
n_psu <- length(unique(zambara_hh$psu))
cat("Number of PSUs (clusters):", format(n_psu, big.mark = ","), "\n")

# Number of strata
n_strata <- length(unique(zambara_hh$stratum))
cat("Number of strata:", format(n_strata, big.mark = ","), "\n")

# Weight distribution
cat("\nHousehold weight distribution:\n")
cat("  Min:    ", format(min(zambara_hh$house_wgt, na.rm = TRUE), nsmall = 2), "\n")
cat("  Median: ", format(median(zambara_hh$house_wgt, na.rm = TRUE), nsmall = 2), "\n")
cat("  Mean:   ", format(mean(zambara_hh$house_wgt, na.rm = TRUE), nsmall = 2), "\n")
cat("  Max:    ", format(max(zambara_hh$house_wgt, na.rm = TRUE), nsmall = 2), "\n")

# Households per PSU
hh_per_psu <- zambara_hh %>%
  group_by(psu) %>%
  summarise(n = n(), .groups = "drop")

cat("\nHouseholds per PSU:\n")
cat("  Min:    ", min(hh_per_psu$n), "\n")
cat("  Median: ", median(hh_per_psu$n), "\n")
cat("  Mean:   ", round(mean(hh_per_psu$n), 1), "\n")
cat("  Max:    ", max(hh_per_psu$n), "\n")

# Province distribution
cat("\nHouseholds by Province:\n")
prov_dist <- zambara_hh %>%
  group_by(prov) %>%
  summarise(
    n_households = n(),
    n_psu = n_distinct(psu),
    .groups = "drop"
  ) %>%
  arrange(prov)

print(prov_dist, n = Inf)

# ==============================================================================
# SECTION 2: THE PROBLEM — NAIVE ANALYSIS IGNORES DESIGN
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: THE PROBLEM WITH NAIVE ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 2.1 What Lindiwe's team was doing wrong
# ------------------------------------------------------------------------------

cat("SCENARIO: Lindiwe discovers her team's analysis mistake\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

cat("Thabo explains: 'We've been treating our complex survey data as if it\n")
cat("came from a simple random sample. This produces INCORRECT standard\n")
cat("errors because it ignores:\n\n")
cat("  1. CLUSTERING: Households in the same PSU are more similar to each\n")
cat("     other than to households in other PSUs (intra-cluster correlation)\n\n")
cat("  2. STRATIFICATION: We deliberately selected samples from each stratum\n")
cat("     to ensure representation, which affects variance calculation\n\n")
cat("  3. UNEQUAL WEIGHTS: Different households have different selection\n")
cat("     probabilities, affecting point estimates and variances'\n\n")

# ------------------------------------------------------------------------------
# 2.2 Calculate naive (SRS) estimates
# ------------------------------------------------------------------------------

cat("NAIVE ANALYSIS (Ignoring Complex Design)\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

# Examine the outcome variable: totmhinc (total monthly household income)
cat("Outcome variable: totmhinc (Total Monthly Household Income)\n\n")

# Check for missing values
n_missing <- sum(is.na(zambara_hh$totmhinc))
n_valid <- sum(!is.na(zambara_hh$totmhinc))
cat("Missing values:", format(n_missing, big.mark = ","), "\n")
cat("Valid observations:", format(n_valid, big.mark = ","), "\n\n")

# Filter to non-missing income observations
zambara_valid <- zambara_hh %>%
  filter(!is.na(totmhinc))

# ------------------------------------------------------------------------------
# Method 1: Completely unweighted mean (treats as SRS, ignores weights)
# ------------------------------------------------------------------------------

naive_unweighted <- zambara_valid %>%
  summarise(
    mean_income = mean(totmhinc),
    se_income = sd(totmhinc) / sqrt(n()),
    n = n()
  )

cat("Method 1: Unweighted mean (ignores weights entirely)\n")
cat("  Mean income:     R", format(round(naive_unweighted$mean_income, 2), 
                                    big.mark = ",", nsmall = 2), "\n")
cat("  Naive SE:        R", format(round(naive_unweighted$se_income, 2), 
                                    big.mark = ",", nsmall = 2), "\n")
cat("  Sample size:     ", format(naive_unweighted$n, big.mark = ","), "\n\n")

# ------------------------------------------------------------------------------
# Method 2: Weighted mean but naive SE (applies weights, ignores clustering)
# ------------------------------------------------------------------------------

# Calculate weighted mean
weighted_mean <- weighted.mean(zambara_valid$totmhinc, zambara_valid$house_wgt)

# Naive SE calculation (treating as if SRS with weights)
# This is WRONG but demonstrates what many analysts do
n_eff <- sum(zambara_valid$house_wgt)^2 / sum(zambara_valid$house_wgt^2)
weighted_var <- sum(zambara_valid$house_wgt * 
                      (zambara_valid$totmhinc - weighted_mean)^2) / 
  sum(zambara_valid$house_wgt)
naive_weighted_se <- sqrt(weighted_var / n_eff)

cat("Method 2: Weighted mean with naive SE (ignores clustering)\n")
cat("  Weighted mean:   R", format(round(weighted_mean, 2), 
                                    big.mark = ",", nsmall = 2), "\n")
cat("  Naive SE:        R", format(round(naive_weighted_se, 2), 
                                    big.mark = ",", nsmall = 2), "\n")
cat("  Effective n:     ", format(round(n_eff, 0), big.mark = ","), "\n\n")

# Store naive results for comparison
naive_results <- data.frame(
  Method = c("Unweighted (SRS)", "Weighted (Naive SE)"),
  Mean = c(naive_unweighted$mean_income, weighted_mean),
  SE = c(naive_unweighted$se_income, naive_weighted_se),
  stringsAsFactors = FALSE
)

# ==============================================================================
# SECTION 3: THE SOLUTION — PROPER SURVEY DESIGN SPECIFICATION
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: DEFINING THE COMPLEX SURVEY DESIGN\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 3.1 The svydesign() function
# ------------------------------------------------------------------------------

cat("CREATING THE SURVEY DESIGN OBJECT\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

cat("The svydesign() function from the 'survey' package (Lumley 2010) properly\n")
cat("accounts for the complex sampling design. Key parameters:\n\n")
cat("  id     = ~psu        # Cluster/PSU identifier (first stage sampling unit)\n")
cat("  strata = ~stratum    # Stratification variable\n")
cat("  weights = ~house_wgt # Sampling weights (inverse selection probabilities)\n")
cat("  data   = zambara_hh  # The survey dataset\n")
cat("  nest   = TRUE        # PSU IDs are nested within strata\n\n")

# ------------------------------------------------------------------------------
# 3.2 Create the survey design object
# ------------------------------------------------------------------------------

cat("Creating survey design object...\n\n")

# Define the survey design
# Note: nest = TRUE because PSU IDs may repeat across strata
zambara_design <- svydesign(
  id = ~psu,                    # Primary sampling unit (cluster)
  strata = ~stratum,            # Stratification variable
  weights = ~house_wgt,         # Household weights
  data = zambara_valid,         # Dataset with valid income values
  nest = TRUE                   # PSU IDs nested within strata
)

# Print design summary
cat("SURVEY DESIGN SUMMARY\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
print(summary(zambara_design))

# ------------------------------------------------------------------------------
# 3.3 Design object attributes
# ------------------------------------------------------------------------------

cat("\n\nDESIGN OBJECT ATTRIBUTES\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("Number of observations:", nrow(zambara_design), "\n")
cat("Number of PSUs:", length(unique(zambara_design$cluster)), "\n")
cat("Number of strata:", length(unique(zambara_design$strata)), "\n")
cat("Sum of weights:", format(sum(weights(zambara_design)), big.mark = ","), "\n")

# ==============================================================================
# SECTION 4: DESIGN-BASED ESTIMATION
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: DESIGN-BASED ESTIMATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 4.1 Mean estimation with proper standard errors
# ------------------------------------------------------------------------------

cat("ESTIMATING MEAN HOUSEHOLD INCOME (Design-Based)\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

# Calculate design-based mean and SE
design_mean <- svymean(~totmhinc, design = zambara_design)

cat("Design-based estimation results:\n\n")
cat("  Point estimate: R", format(round(coef(design_mean), 2), 
                                   big.mark = ",", nsmall = 2), "\n")
cat("  Standard error: R", format(round(SE(design_mean), 2), 
                                   big.mark = ",", nsmall = 2), "\n")

# Confidence interval
ci <- confint(design_mean)
cat("  95% CI:         [R", format(round(ci[1], 2), big.mark = ",", nsmall = 2),
    ", R", format(round(ci[2], 2), big.mark = ",", nsmall = 2), "]\n\n")

# Coefficient of variation
cv <- 100 * SE(design_mean) / coef(design_mean)
cat("  CV:             ", round(cv, 2), "%\n")

# Store design-based results
design_results <- data.frame(
  Method = "Design-Based (Correct)",
  Mean = as.numeric(coef(design_mean)),
  SE = as.numeric(SE(design_mean)),
  stringsAsFactors = FALSE
)

# ==============================================================================
# SECTION 5: COMPARISON — NAIVE vs. DESIGN-BASED
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: COMPARING NAIVE vs. DESIGN-BASED STANDARD ERRORS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 5.1 Build comparison table
# ------------------------------------------------------------------------------

comparison_table <- rbind(naive_results, design_results) %>%
  mutate(
    CI_Lower = Mean - 1.96 * SE,
    CI_Upper = Mean + 1.96 * SE,
    CI_Width = CI_Upper - CI_Lower,
    CV_Percent = 100 * SE / Mean
  )

# Calculate design effect (DEFF)
# DEFF = Var(design-based) / Var(naive weighted)
deff <- (SE(design_mean) / naive_weighted_se)^2

cat("DESIGN EFFECT (DEFF) CALCULATION\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")
cat("DEFF = Var(complex design) / Var(SRS with weights)\n")
cat("DEFF =", round(deff, 3), "\n\n")

cat("Interpretation:\n")
cat("  A DEFF of", round(deff, 2), "means the variance under the complex design\n")
cat("  is", round(deff, 2), "times larger than under SRS assumptions.\n\n")

if (deff > 1) {
  cat("  WARNING: DEFF > 1 indicates clustering INFLATES variance.\n")
  cat("  Ignoring the design leads to UNDERESTIMATED standard errors!\n")
} else {
  cat("  Note: DEFF < 1 indicates stratification reduces variance more than\n")
  cat("  clustering increases it (uncommon in household surveys).\n")
}

# ------------------------------------------------------------------------------
# 5.2 SE comparison ratio
# ------------------------------------------------------------------------------

se_ratio <- SE(design_mean) / naive_weighted_se

cat("\n\nSTANDARD ERROR COMPARISON\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

cat("SE Ratio (Design / Naive) =", round(se_ratio, 3), "\n\n")

cat("This means:\n")
cat("  - Naive SE:        R", format(round(naive_weighted_se, 2), 
                                      big.mark = ",", nsmall = 2), "\n")
cat("  - Design-based SE: R", format(round(SE(design_mean), 2), 
                                      big.mark = ",", nsmall = 2), "\n")
cat("  - Difference:      R", format(round(SE(design_mean) - naive_weighted_se, 2), 
                                      big.mark = ",", nsmall = 2), "\n\n")

underestimate_pct <- 100 * (SE(design_mean) - naive_weighted_se) / SE(design_mean)
cat("The naive approach UNDERESTIMATES the standard error by", 
    round(underestimate_pct, 1), "%\n\n")

cat("PRACTICAL CONSEQUENCES:\n")
cat("  - Confidence intervals too narrow\n")
cat("  - P-values too small\n")
cat("  - False positive rate inflated\n")
cat("  - Type I error rate exceeds nominal level\n")

# ------------------------------------------------------------------------------
# 5.3 Formatted comparison table
# ------------------------------------------------------------------------------

cat("\n\nCOMPREHENSIVE COMPARISON TABLE\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

comparison_display <- comparison_table %>%
  mutate(
    Mean = paste0("R ", format(round(Mean, 2), big.mark = ",", nsmall = 2)),
    SE = paste0("R ", format(round(SE, 2), big.mark = ",", nsmall = 2)),
    `95% CI` = paste0("[R ", format(round(CI_Lower, 0), big.mark = ","),
                      ", R ", format(round(CI_Upper, 0), big.mark = ","), "]"),
    `CI Width` = paste0("R ", format(round(CI_Width, 0), big.mark = ",")),
    `CV (%)` = round(CV_Percent, 2)
  ) %>%
  select(Method, Mean, SE, `95% CI`, `CI Width`, `CV (%)`)

print(comparison_display, row.names = FALSE)

# ==============================================================================
# SECTION 6: DOMAIN ESTIMATION BY PROVINCE
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 6: DOMAIN ESTIMATION BY PROVINCE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 6.1 Provincial estimates with design-based SEs
# ------------------------------------------------------------------------------

cat("MEAN HOUSEHOLD INCOME BY PROVINCE (Design-Based)\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

# Calculate provincial means
provincial_means <- svyby(
  formula = ~totmhinc,
  by = ~prov,
  design = zambara_design,
  FUN = svymean,
  vartype = c("se", "ci", "cv")
)

# Display results
print(provincial_means)

# ------------------------------------------------------------------------------
# 6.2 Compare with naive provincial estimates
# ------------------------------------------------------------------------------

cat("\n\nNAIVE vs. DESIGN-BASED SE BY PROVINCE\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

# Calculate naive SEs by province
naive_provincial <- zambara_valid %>%
  group_by(prov) %>%
  summarise(
    n = n(),
    mean_income = weighted.mean(totmhinc, house_wgt),
    naive_se = {
      wm <- weighted.mean(totmhinc, house_wgt)
      n_e <- sum(house_wgt)^2 / sum(house_wgt^2)
      wv <- sum(house_wgt * (totmhinc - wm)^2) / sum(house_wgt)
      sqrt(wv / n_e)
    },
    .groups = "drop"
  )

# Merge with design-based results
provincial_comparison <- provincial_means %>%
  as.data.frame() %>%
  rename(design_se = se) %>%
  left_join(naive_provincial, by = "prov") %>%
  mutate(
    se_ratio = design_se / naive_se,
    deff = se_ratio^2
  ) %>%
  select(prov, n, mean_income, naive_se, design_se, se_ratio, deff)

cat("Province-level comparison:\n\n")
print(provincial_comparison, row.names = FALSE)

# ==============================================================================
# SECTION 7: EXPORT RESULTS
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 7: EXPORTING RESULTS FOR PRESENTATION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ------------------------------------------------------------------------------
# 7.1 Export main comparison table
# ------------------------------------------------------------------------------

# Create export-ready table
export_comparison <- comparison_table %>%
  mutate(
    DEFF = c(NA, deff, 1),  # Only meaningful for design vs naive weighted
    SE_Ratio = c(NA, 1, se_ratio)
  )

# Save as CSV
write.csv(
  export_comparison,
  file = file.path(output_dir, "lab2_1_se_comparison.csv"),
  row.names = FALSE
)

cat("Exported: lab2_1_se_comparison.csv\n")

# ------------------------------------------------------------------------------
# 7.2 Export provincial comparison
# ------------------------------------------------------------------------------

write.csv(
  provincial_comparison,
  file = file.path(output_dir, "lab2_1_provincial_comparison.csv"),
  row.names = FALSE
)

cat("Exported: lab2_1_provincial_comparison.csv\n")

# ------------------------------------------------------------------------------
# 7.3 Export summary statistics
# ------------------------------------------------------------------------------

summary_stats <- data.frame(
  Metric = c(
    "Total Households",
    "Number of PSUs",
    "Number of Strata",
    "Mean HH Income (Design)",
    "SE (Design-Based)",
    "SE (Naive Weighted)",
    "Design Effect (DEFF)",
    "SE Ratio (Design/Naive)",
    "SE Underestimation (%)"
  ),
  Value = c(
    format(nrow(zambara_valid), big.mark = ","),
    format(n_psu, big.mark = ","),
    format(n_strata, big.mark = ","),
    paste0("R ", format(round(coef(design_mean), 2), big.mark = ",")),
    paste0("R ", format(round(SE(design_mean), 2), big.mark = ",")),
    paste0("R ", format(round(naive_weighted_se, 2), big.mark = ",")),
    round(deff, 3),
    round(se_ratio, 3),
    paste0(round(underestimate_pct, 1), "%")
  ),
  stringsAsFactors = FALSE
)

write.csv(
  summary_stats,
  file = file.path(output_dir, "lab2_1_summary_stats.csv"),
  row.names = FALSE
)

cat("Exported: lab2_1_summary_stats.csv\n")

# ------------------------------------------------------------------------------
# 7.4 Create visualization for slides
# ------------------------------------------------------------------------------

# SE comparison bar chart
se_plot_data <- data.frame(
  Method = c("Naive (Weighted)", "Design-Based"),
  SE = c(naive_weighted_se, SE(design_mean))
)

p_se_comparison <- ggplot(se_plot_data, aes(x = Method, y = SE, fill = Method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("R ", format(round(SE, 0), big.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Naive (Weighted)" = "#E74C3C", 
                                "Design-Based" = "#27AE60")) +
  labs(
    title = "Standard Error Comparison: Naive vs. Design-Based",
    subtitle = paste0("Design Effect (DEFF) = ", round(deff, 2),
                      " | SE underestimated by ", round(underestimate_pct, 1), "%"),
    x = "",
    y = "Standard Error (Rand)",
    caption = "Source: Zambara Household Living Conditions Survey (ZNSO)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

# Save plot
ggsave(
  filename = file.path(output_dir, "lab2_1_se_comparison_plot.png"),
  plot = p_se_comparison,
  width = 8, height = 6, dpi = 300
)

cat("Exported: lab2_1_se_comparison_plot.png\n")

# Provincial DEFF plot
p_provincial_deff <- ggplot(provincial_comparison, 
                             aes(x = reorder(as.factor(prov), deff), y = deff)) +
  geom_col(fill = "#3498DB", width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = round(deff, 2)), hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Design Effect by Province",
    subtitle = "DEFF > 1 indicates clustering inflates variance",
    x = "Province",
    y = "Design Effect (DEFF)",
    caption = "Red dashed line: DEFF = 1 (SRS equivalent)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave(
  filename = file.path(output_dir, "lab2_1_provincial_deff_plot.png"),
  plot = p_provincial_deff,
  width = 10, height = 7, dpi = 300
)

cat("Exported: lab2_1_provincial_deff_plot.png\n")

# ==============================================================================
# SECTION 8: KEY TAKEAWAYS
# ==============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 8: KEY TAKEAWAYS FOR LINDIWE'S TEAM\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("THABO'S SUMMARY FOR THE ZNSO TEAM:\n\n")

cat("1. ALWAYS SPECIFY THE SURVEY DESIGN\n")
cat("   - Use svydesign() with id, strata, and weights\n")
cat("   - Never analyze survey data with standard functions like mean(), lm()\n\n")

cat("2. THE DESIGN EFFECT MATTERS\n")
cat("   - Our DEFF =", round(deff, 2), "means variance is", round(deff, 2), 
    "times larger than SRS\n")
cat("   - Ignoring clustering underestimates SE by", round(underestimate_pct, 1), "%\n\n")

cat("3. CONSEQUENCES OF NAIVE ANALYSIS\n")
cat("   - Confidence intervals too narrow → false precision\n")
cat("   - P-values too small → inflated Type I error\n")
cat("   - Policy decisions based on overstated significance\n\n")

cat("4. PROVINCIAL VARIATION\n")
cat("   - DEFF varies by province (", 
    round(min(provincial_comparison$deff), 2), " to ",
    round(max(provincial_comparison$deff), 2), ")\n", sep = "")
cat("   - Design effects tend to be larger in more homogeneous areas\n\n")

cat("5. DOCUMENTATION IS ESSENTIAL\n")
cat("   - Record design specifications in all analysis scripts\n")
cat("   - Report DEFF alongside estimates in publications\n")
cat("   - Enable reproducibility by saving design objects\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("LAB 2.1 COMPLETED SUCCESSFULLY\n")
cat("Output files saved to:", output_dir, "\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ==============================================================================
# SESSION INFO FOR REPRODUCIBILITY
# ==============================================================================

cat("\nFULL SESSION INFORMATION:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
sessionInfo()

################################################################################
#                           END OF LAB 2.1 R SCRIPT                            #
################################################################################
