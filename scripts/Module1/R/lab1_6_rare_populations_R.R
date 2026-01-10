#===============================================================================
# LAB 1.6: SAMPLING RARE POPULATIONS
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 1, Module 1.7
#
# Objective: Compare sample size requirements for rare population estimation
#            using Simple Random Sampling vs. Stratified Sampling
#
# Data: GHS 2024 Household File (ghs-2024-hhold-v1.dta)
# Rare Population: High-income households (Top 5% of income distribution)
#
# Author: SADC Workshop Team
# Date: March 2026
#===============================================================================

#-------------------------------------------------------------------------------
# SECTION 1: SETUP AND DATA LOADING
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load required packages
required_packages <- c("haven", "dplyr", "survey", "ggplot2", "scales", "knitr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

cat("="
, rep("=", 70), "\n", sep = "")
cat("LAB 1.6: SAMPLING RARE POPULATIONS\n")
cat("Comparing SRS vs. Stratified Sampling for High-Income Households\n")
cat(rep("=", 71), "\n\n", sep = "")

# Set working directory (adjust as needed)
# setwd("path/to/your/data")

# Load the GHS 2024 household data
cat("Loading GHS 2024 household data...\n")
ghs_hh <- read_dta("ghs-2024-hhold-v1.dta")

cat("Dataset loaded successfully.\n")
cat("  - Observations:", nrow(ghs_hh), "\n")
cat("  - Variables:", ncol(ghs_hh), "\n\n")

#-------------------------------------------------------------------------------
# SECTION 2: EXPLORE INCOME DISTRIBUTION
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 2: INCOME DISTRIBUTION ANALYSIS\n")
cat(rep("-", 71), "\n\n", sep = "")

# Check available income variables
income_vars <- grep("inc|salary|earn|wage", names(ghs_hh), value = TRUE, ignore.case = TRUE)
cat("Available income-related variables:\n")
print(income_vars)
cat("\n")

# Use total monthly household income (totmhinc)
# If not available, we'll create a proxy or use another variable
if ("totmhinc" %in% names(ghs_hh)) {
  ghs_hh$income <- ghs_hh$totmhinc
  cat("Using 'totmhinc' (Total Monthly Household Income) as income measure.\n\n")
} else {
  # Check for alternative income variables
  cat("Note: 'totmhinc' not found. Checking alternatives...\n")
  # Try to find any income variable
  if (length(income_vars) > 0) {
    ghs_hh$income <- ghs_hh[[income_vars[1]]]
    cat("Using '", income_vars[1], "' as income measure.\n\n", sep = "")
  } else {
    stop("No income variable found in dataset. Please check variable names.")
  }
}

# Clean income data (remove missing/negative values for analysis)
ghs_hh_clean <- ghs_hh %>%
  filter(!is.na(income) & income >= 0)

cat("Income data cleaning:\n")
cat("  - Original observations:", nrow(ghs_hh), "\n")
cat("  - After removing missing/negative:", nrow(ghs_hh_clean), "\n")
cat("  - Records removed:", nrow(ghs_hh) - nrow(ghs_hh_clean), "\n\n")

# Summary statistics for income
cat("Income Distribution Summary:\n")
income_summary <- summary(ghs_hh_clean$income)
print(income_summary)
cat("\n")

# Calculate percentiles
percentiles <- quantile(ghs_hh_clean$income, 
                        probs = c(0.50, 0.75, 0.90, 0.95, 0.99), 
                        na.rm = TRUE)
cat("Income Percentiles:\n")
cat("  50th percentile (Median): R", format(percentiles[1], big.mark = ","), "\n")
cat("  75th percentile:          R", format(percentiles[2], big.mark = ","), "\n")
cat("  90th percentile:          R", format(percentiles[3], big.mark = ","), "\n")
cat("  95th percentile:          R", format(percentiles[4], big.mark = ","), "\n")
cat("  99th percentile:          R", format(percentiles[5], big.mark = ","), "\n\n")

#-------------------------------------------------------------------------------
# SECTION 3: DEFINE RARE POPULATION (TOP 5% INCOME)
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 3: DEFINING THE RARE POPULATION\n")
cat(rep("-", 71), "\n\n", sep = "")

# Define threshold for top 5% (95th percentile)
income_threshold <- percentiles["95%"]
cat("Rare Population Definition:\n")
cat("  High-income threshold (95th percentile): R", 
    format(income_threshold, big.mark = ","), "\n\n")

# Create indicator for rare population
ghs_hh_clean <- ghs_hh_clean %>%
  mutate(high_income = as.numeric(income >= income_threshold))

# Calculate prevalence
N_total <- nrow(ghs_hh_clean)
N_rare <- sum(ghs_hh_clean$high_income)
prevalence <- mean(ghs_hh_clean$high_income)

cat("Rare Population Characteristics:\n")
cat("  Total households in frame:      ", format(N_total, big.mark = ","), "\n")
cat("  High-income households:         ", format(N_rare, big.mark = ","), "\n")
cat("  Population prevalence (P):      ", sprintf("%.4f", prevalence), 
    " (", sprintf("%.2f%%", prevalence * 100), ")\n\n")

# Variance of proportion
var_p <- prevalence * (1 - prevalence)
cat("  Variance of proportion (P×Q):   ", sprintf("%.6f", var_p), "\n\n")

#-------------------------------------------------------------------------------
# SECTION 4: SAMPLE SIZE CALCULATION - SIMPLE RANDOM SAMPLING
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 4: SAMPLE SIZE FOR SIMPLE RANDOM SAMPLING (SRS)\n")
cat(rep("-", 71), "\n\n", sep = "")

# Parameters for sample size calculation
confidence_level <- 0.95
z_value <- qnorm(1 - (1 - confidence_level) / 2)  # 1.96 for 95% CI
relative_precision <- 0.10  # 10% relative precision (CV)

cat("Design Parameters:\n")
cat("  Confidence level:          ", sprintf("%.0f%%", confidence_level * 100), "\n")
cat("  Z-value:                   ", sprintf("%.4f", z_value), "\n")
cat("  Relative precision (CV):   ", sprintf("%.0f%%", relative_precision * 100), "\n\n")

# Calculate absolute margin of error
# Relative precision of 10% means: SE/P = 0.10, so SE = 0.10 * P
# Margin of error (e) = z * SE = z * 0.10 * P
margin_of_error_relative <- z_value * relative_precision * prevalence
cat("  Absolute margin of error:  ", sprintf("%.6f", margin_of_error_relative), "\n")
cat("  (This means estimate will be P ± ", sprintf("%.4f", margin_of_error_relative), ")\n\n")

#-------------------------------------------------------------------------------
# Formula for SRS sample size for proportion:
# n = (z² × P × (1-P)) / e²
# where e = relative_precision × P (for relative precision)
#
# Alternatively, for coefficient of variation (CV):
# n = (z² × (1-P)) / (CV² × P)
#-------------------------------------------------------------------------------

# Method 1: Using absolute margin of error derived from relative precision
e_absolute <- relative_precision * prevalence
n_srs_method1 <- (z_value^2 * prevalence * (1 - prevalence)) / (e_absolute^2)

# Method 2: Direct CV formula
# CV = SE/P, so SE = CV × P
# n = (z² × P × (1-P)) / (CV × P)² = (z² × (1-P)) / (CV² × P)
n_srs_method2 <- (z_value^2 * (1 - prevalence)) / (relative_precision^2 * prevalence)

cat("SRS Sample Size Calculation:\n")
cat("  Formula: n = z² × P × (1-P) / e²\n")
cat("  where e = CV × P (absolute error from relative precision)\n\n")

cat("  Substituting values:\n")
cat("    n = ", sprintf("%.4f", z_value^2), " × ", sprintf("%.4f", prevalence), 
    " × ", sprintf("%.4f", 1 - prevalence), " / ", sprintf("%.8f", e_absolute^2), "\n")
cat("    n = ", sprintf("%.2f", n_srs_method1), "\n\n")

# Round up to nearest integer
n_srs <- ceiling(n_srs_method1)

cat("  REQUIRED SRS SAMPLE SIZE: ", format(n_srs, big.mark = ","), " households\n\n")

# Finite population correction (FPC)
if (n_srs < N_total) {
  n_srs_fpc <- ceiling(n_srs / (1 + (n_srs - 1) / N_total))
  cat("  With finite population correction:\n")
  cat("    n_fpc = n / (1 + (n-1)/N)\n")
  cat("    n_fpc = ", format(n_srs_fpc, big.mark = ","), " households\n\n")
} else {
  n_srs_fpc <- n_srs
}

# Expected number of rare population units in SRS sample
expected_rare_srs <- n_srs * prevalence
cat("  Expected high-income HH in sample: ", sprintf("%.1f", expected_rare_srs), "\n")
cat("  (Only ~", sprintf("%.0f", expected_rare_srs), 
    " rare cases from ", format(n_srs, big.mark = ","), " interviews!)\n\n")

#-------------------------------------------------------------------------------
# SECTION 5: SAMPLE SIZE CALCULATION - STRATIFIED SAMPLING
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 5: SAMPLE SIZE FOR STRATIFIED SAMPLING\n")
cat(rep("-", 71), "\n\n", sep = "")

cat("Strategy: Create two strata based on proxy indicators of high income\n")
cat("          and oversample the stratum likely to contain rare population.\n\n")

#-------------------------------------------------------------------------------
# Create stratification variable using geographic proxy
# Urban formal areas typically have higher income concentrations
#-------------------------------------------------------------------------------

# Check for geotype or urban/rural variable
if ("geotype" %in% names(ghs_hh_clean)) {
  cat("Using 'geotype' for stratification (1=Urban, 2=Traditional, 3=Farms)\n\n")
  
  # Analyze high-income prevalence by geotype
  prevalence_by_geo <- ghs_hh_clean %>%
    group_by(geotype) %>%
    summarise(
      n_total = n(),
      n_high_income = sum(high_income),
      prevalence = mean(high_income),
      .groups = "drop"
    ) %>%
    mutate(
      pct_of_total = n_total / sum(n_total) * 100,
      pct_of_rare = n_high_income / sum(n_high_income) * 100
    )
  
  cat("High-Income Prevalence by Geographic Type:\n")
  print(as.data.frame(prevalence_by_geo))
  cat("\n")
  
  # Create binary stratum: Urban (high prevalence) vs Non-Urban (low prevalence)
  ghs_hh_clean <- ghs_hh_clean %>%
    mutate(stratum = ifelse(geotype == 1, "Urban", "Non-Urban"))
  
} else if ("metro" %in% names(ghs_hh_clean)) {
  cat("Using 'metro' for stratification\n\n")
  ghs_hh_clean <- ghs_hh_clean %>%
    mutate(stratum = ifelse(metro == 1, "Metro", "Non-Metro"))
} else {
  # Create income-based proxy stratum using other household characteristics
  cat("Creating proxy stratum based on province (economic hub vs other)\n\n")
  # Gauteng (7) and Western Cape (1) typically have higher incomes
  ghs_hh_clean <- ghs_hh_clean %>%
    mutate(stratum = ifelse(prov %in% c(1, 7), "High-Income Province", "Other Province"))
}

# Calculate stratum-specific statistics
stratum_stats <- ghs_hh_clean %>%
  group_by(stratum) %>%
  summarise(
    N_h = n(),
    N_rare_h = sum(high_income),
    P_h = mean(high_income),
    Var_h = P_h * (1 - P_h),
    .groups = "drop"
  ) %>%
  mutate(
    W_h = N_h / sum(N_h),  # Stratum weight
    contribution = W_h * P_h  # Contribution to overall prevalence
  )

cat("Stratum Statistics:\n")
print(as.data.frame(stratum_stats))
cat("\n")

# Verify overall prevalence
overall_check <- sum(stratum_stats$W_h * stratum_stats$P_h)
cat("Verification - Overall prevalence from strata: ", sprintf("%.4f", overall_check), "\n")
cat("                         Direct calculation: ", sprintf("%.4f", prevalence), "\n\n")

#-------------------------------------------------------------------------------
# Stratified Sample Size Calculations
#-------------------------------------------------------------------------------

# Extract stratum parameters
if (nrow(stratum_stats) == 2) {
  # High prevalence stratum
  stratum_high <- stratum_stats %>% filter(P_h == max(P_h))
  W_1 <- stratum_high$W_h
  P_1 <- stratum_high$P_h
  N_1 <- stratum_high$N_h
  
  # Low prevalence stratum
  stratum_low <- stratum_stats %>% filter(P_h == min(P_h))
  W_2 <- stratum_low$W_h
  P_2 <- stratum_low$P_h
  N_2 <- stratum_low$N_h
  
  cat("Stratum Parameters:\n")
  cat("  High-prevalence stratum (", stratum_high$stratum, "):\n", sep = "")
  cat("    Weight (W₁):      ", sprintf("%.4f", W_1), "\n")
  cat("    Prevalence (P₁):  ", sprintf("%.4f", P_1), "\n")
  cat("    Population (N₁):  ", format(N_1, big.mark = ","), "\n\n")
  
  cat("  Low-prevalence stratum (", stratum_low$stratum, "):\n", sep = "")
  cat("    Weight (W₂):      ", sprintf("%.4f", W_2), "\n")
  cat("    Prevalence (P₂):  ", sprintf("%.4f", P_2), "\n")
  cat("    Population (N₂):  ", format(N_2, big.mark = ","), "\n\n")
}

#-------------------------------------------------------------------------------
# Option A: Proportional Allocation
#-------------------------------------------------------------------------------

cat("OPTION A: PROPORTIONAL ALLOCATION\n")
cat("---------------------------------\n\n")

# For stratified sampling with proportional allocation:
# Var(p_st) = Σ(W_h² × P_h × (1-P_h) / n_h)
# With proportional allocation: n_h = n × W_h
# So: Var(p_st) = Σ(W_h × P_h × (1-P_h)) / n

# Required sample size for target CV
# CV² = Var(p_st) / P²
# n = Σ(W_h × P_h × (1-P_h)) / (CV² × P²)

variance_component_prop <- sum(stratum_stats$W_h * stratum_stats$P_h * (1 - stratum_stats$P_h))

n_prop <- (z_value^2 * variance_component_prop) / (relative_precision^2 * prevalence^2)
n_prop <- ceiling(n_prop)

cat("Formula: n = z² × Σ(Wₕ × Pₕ × (1-Pₕ)) / (CV² × P²)\n\n")
cat("Variance component Σ(Wₕ × Pₕ × Qₕ): ", sprintf("%.6f", variance_component_prop), "\n")
cat("Required total sample size: ", format(n_prop, big.mark = ","), "\n\n")

# Allocation to strata
stratum_stats$n_prop <- ceiling(n_prop * stratum_stats$W_h)
cat("Allocation by stratum:\n")
for (i in 1:nrow(stratum_stats)) {
  cat("  ", stratum_stats$stratum[i], ": ", 
      format(stratum_stats$n_prop[i], big.mark = ","), " households\n", sep = "")
}
cat("\n")

# Expected rare cases
expected_rare_prop <- sum(stratum_stats$n_prop * stratum_stats$P_h)
cat("Expected high-income HH in sample: ", sprintf("%.1f", expected_rare_prop), "\n\n")

#-------------------------------------------------------------------------------
# Option B: Optimal (Neyman) Allocation
#-------------------------------------------------------------------------------

cat("OPTION B: NEYMAN ALLOCATION\n")
cat("---------------------------\n\n")

# Neyman allocation minimizes variance for fixed n
# n_h ∝ N_h × S_h, where S_h = sqrt(P_h × (1-P_h))

stratum_stats <- stratum_stats %>%
  mutate(
    S_h = sqrt(P_h * (1 - P_h)),
    N_S = N_h * S_h,
    neyman_weight = N_S / sum(N_S)
  )

# For Neyman allocation, variance is:
# Var(p_st) = (Σ(W_h × S_h))² / n - Σ(W_h × S_h²) / N
# Simplified for large N: Var(p_st) ≈ (Σ(W_h × S_h))² / n

variance_component_neyman <- (sum(stratum_stats$W_h * stratum_stats$S_h))^2

n_neyman <- (z_value^2 * variance_component_neyman) / (relative_precision^2 * prevalence^2)
n_neyman <- ceiling(n_neyman)

cat("Formula: n = z² × (Σ(Wₕ × Sₕ))² / (CV² × P²)\n")
cat("         where Sₕ = √(Pₕ × (1-Pₕ))\n\n")

cat("Variance component (Σ(Wₕ × Sₕ))²: ", sprintf("%.6f", variance_component_neyman), "\n")
cat("Required total sample size: ", format(n_neyman, big.mark = ","), "\n\n")

# Allocation to strata
stratum_stats$n_neyman <- ceiling(n_neyman * stratum_stats$neyman_weight)
cat("Allocation by stratum:\n")
for (i in 1:nrow(stratum_stats)) {
  cat("  ", stratum_stats$stratum[i], ": ", 
      format(stratum_stats$n_neyman[i], big.mark = ","), " households\n", sep = "")
}
cat("\n")

# Expected rare cases
expected_rare_neyman <- sum(stratum_stats$n_neyman * stratum_stats$P_h)
cat("Expected high-income HH in sample: ", sprintf("%.1f", expected_rare_neyman), "\n\n")

#-------------------------------------------------------------------------------
# Option C: Oversampling the Rare Stratum
#-------------------------------------------------------------------------------

cat("OPTION C: OVERSAMPLING HIGH-PREVALENCE STRATUM\n")
cat("----------------------------------------------\n\n")

cat("Strategy: Allocate 50% of sample to high-prevalence stratum\n")
cat("          (vs. natural proportion of ", sprintf("%.1f%%", W_1 * 100), ")\n\n", sep = "")

# Set oversampling rate
oversample_rate <- 0.50  # 50% of sample to high-prevalence stratum

# Target: At least 100 rare cases for stable subgroup analysis
target_rare_cases <- 100

cat("Target minimum rare cases: ", target_rare_cases, "\n\n")

# Calculate required sample in each stratum to achieve target
# Expected rare in high stratum: n_1 × P_1
# Expected rare in low stratum: n_2 × P_2
# Total rare: n × (oversample_rate × P_1 + (1 - oversample_rate) × P_2)

expected_rare_rate <- oversample_rate * P_1 + (1 - oversample_rate) * P_2
n_oversample <- ceiling(target_rare_cases / expected_rare_rate)

cat("Expected rare case rate with oversampling: ", sprintf("%.4f", expected_rare_rate), "\n")
cat("Required total sample for ", target_rare_cases, " rare cases: ", 
    format(n_oversample, big.mark = ","), "\n\n")

# Allocation
n_1_over <- ceiling(n_oversample * oversample_rate)
n_2_over <- ceiling(n_oversample * (1 - oversample_rate))

cat("Allocation by stratum:\n")
cat("  ", stratum_high$stratum, ": ", format(n_1_over, big.mark = ","), 
    " households (", sprintf("%.1f%%", oversample_rate * 100), ")\n", sep = "")
cat("  ", stratum_low$stratum, ": ", format(n_2_over, big.mark = ","), 
    " households (", sprintf("%.1f%%", (1 - oversample_rate) * 100), ")\n\n", sep = "")

expected_rare_over <- n_1_over * P_1 + n_2_over * P_2
cat("Expected high-income HH in sample: ", sprintf("%.1f", expected_rare_over), "\n\n")

# Calculate variance for oversampled design
# Var(p_st) = W_1² × P_1 × Q_1 / n_1 + W_2² × P_2 × Q_2 / n_2
var_oversample <- (W_1^2 * P_1 * (1 - P_1) / n_1_over) + 
                  (W_2^2 * P_2 * (1 - P_2) / n_2_over)
se_oversample <- sqrt(var_oversample)
cv_oversample <- se_oversample / prevalence

cat("Achieved precision with oversampling:\n")
cat("  Standard Error: ", sprintf("%.6f", se_oversample), "\n")
cat("  CV: ", sprintf("%.2f%%", cv_oversample * 100), "\n")
cat("  95% CI width: ±", sprintf("%.4f", z_value * se_oversample), "\n\n")

#-------------------------------------------------------------------------------
# SECTION 6: COMPARISON SUMMARY
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 6: COMPARISON OF APPROACHES\n")
cat(rep("-", 71), "\n\n", sep = "")

# Create comparison table
comparison <- data.frame(
  Method = c("Simple Random Sampling",
             "Stratified - Proportional",
             "Stratified - Neyman",
             "Stratified - Oversampling"),
  Total_Sample = c(n_srs, n_prop, n_neyman, n_oversample),
  Expected_Rare = c(expected_rare_srs, expected_rare_prop, 
                    expected_rare_neyman, expected_rare_over),
  Efficiency_vs_SRS = c(1, n_srs / n_prop, n_srs / n_neyman, NA)
)

comparison$Efficiency_vs_SRS[4] <- "Target-based"

cat("Sample Size Comparison:\n")
cat("="
, rep("=", 69), "\n", sep = "")
cat(sprintf("%-30s %12s %15s %12s\n", 
            "Method", "Sample Size", "Expected Rare", "Efficiency"))
cat(rep("-", 70), "\n", sep = "")
for (i in 1:nrow(comparison)) {
  if (i < 4) {
    cat(sprintf("%-30s %12s %15.1f %12.2f\n",
                comparison$Method[i],
                format(comparison$Total_Sample[i], big.mark = ","),
                comparison$Expected_Rare[i],
                as.numeric(comparison$Efficiency_vs_SRS[i])))
  } else {
    cat(sprintf("%-30s %12s %15.1f %12s\n",
                comparison$Method[i],
                format(comparison$Total_Sample[i], big.mark = ","),
                comparison$Expected_Rare[i],
                comparison$Efficiency_vs_SRS[i]))
  }
}
cat(rep("=", 70), "\n\n", sep = "")

#-------------------------------------------------------------------------------
# SECTION 7: KEY INSIGHTS AND RECOMMENDATIONS
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 7: KEY INSIGHTS\n")
cat(rep("-", 71), "\n\n", sep = "")

cat("FINDINGS:\n\n")

cat("1. PROBLEM WITH SRS FOR RARE POPULATIONS:\n")
cat("   - Required sample size: ", format(n_srs, big.mark = ","), " households\n")
cat("   - Expected rare cases: only ", sprintf("%.0f", expected_rare_srs), "\n")
cat("   - Extremely inefficient: interviewing ", 
    sprintf("%.0f", n_srs / expected_rare_srs), 
    " HH for each rare case\n\n")

cat("2. STRATIFICATION BENEFIT:\n")
cat("   - Proportional allocation saves ", 
    sprintf("%.1f%%", (1 - n_prop/n_srs) * 100), " of sample\n")
cat("   - Neyman allocation saves ", 
    sprintf("%.1f%%", (1 - n_neyman/n_srs) * 100), " of sample\n")
cat("   - Both approaches maintain target precision\n\n")

cat("3. OVERSAMPLING FOR SUBGROUP ANALYSIS:\n")
cat("   - If analyzing the rare group itself (not just prevalence)\n")
cat("   - Need minimum ~100 cases for stable estimates\n")
cat("   - Oversampling achieves this with ", format(n_oversample, big.mark = ","), " HH\n")
cat("   - Must apply weighting adjustments in analysis\n\n")

cat("RECOMMENDATIONS:\n\n")
cat("• For prevalence estimation: Use Neyman allocation\n")
cat("• For detailed rare group analysis: Use oversampling\n")
cat("• Always document stratification and weighting in methodology\n")
cat("• Consider screening questions if rare population identifiable\n\n")

#-------------------------------------------------------------------------------
# SECTION 8: EXPORT RESULTS
#-------------------------------------------------------------------------------

cat(rep("-", 71), "\n", sep = "")
cat("SECTION 8: EXPORT RESULTS\n")
cat(rep("-", 71), "\n\n", sep = "")

# Create output directory
output_dir <- "lab1_6_outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Export stratum statistics
write.csv(stratum_stats, file.path(output_dir, "stratum_statistics.csv"), row.names = FALSE)

# Export comparison table
write.csv(comparison, file.path(output_dir, "sample_size_comparison.csv"), row.names = FALSE)

# Export parameters
params <- data.frame(
  Parameter = c("Confidence Level", "Z-value", "Relative Precision (CV)",
                "Population Size", "Rare Population Size", "Prevalence",
                "Income Threshold (95th pct)"),
  Value = c(sprintf("%.0f%%", confidence_level * 100),
            sprintf("%.4f", z_value),
            sprintf("%.0f%%", relative_precision * 100),
            format(N_total, big.mark = ","),
            format(N_rare, big.mark = ","),
            sprintf("%.4f", prevalence),
            paste0("R", format(round(income_threshold), big.mark = ",")))
)
write.csv(params, file.path(output_dir, "design_parameters.csv"), row.names = FALSE)

cat("Results exported to '", output_dir, "/' directory:\n", sep = "")
cat("  - stratum_statistics.csv\n")
cat("  - sample_size_comparison.csv\n")
cat("  - design_parameters.csv\n\n")

#-------------------------------------------------------------------------------
# VISUALIZATION
#-------------------------------------------------------------------------------

# Create comparison bar plot
if (require(ggplot2)) {
  
  comparison_plot <- data.frame(
    Method = factor(c("SRS", "Proportional", "Neyman", "Oversampling"),
                    levels = c("SRS", "Proportional", "Neyman", "Oversampling")),
    Sample_Size = c(n_srs, n_prop, n_neyman, n_oversample)
  )
  
  p <- ggplot(comparison_plot, aes(x = Method, y = Sample_Size, fill = Method)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = format(Sample_Size, big.mark = ",")), 
              vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("#8B0000", "#0051A2", "#228B22", "#DAA520")) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Sample Size Comparison for Rare Population Estimation",
         subtitle = paste0("Target: 10% CV for high-income households (", 
                          sprintf("%.1f%%", prevalence * 100), " prevalence)"),
         x = "Sampling Method",
         y = "Required Sample Size") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10))
  
  ggsave(file.path(output_dir, "sample_size_comparison.png"), p, 
         width = 8, height = 6, dpi = 150)
  
  cat("Visualization saved: sample_size_comparison.png\n\n")
}

cat(rep("=", 71), "\n", sep = "")
cat("LAB 1.6 COMPLETE\n")
cat(rep("=", 71), "\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
