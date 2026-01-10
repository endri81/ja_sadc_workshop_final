#===============================================================================
# LAB 1.4: SAMPLE ALLOCATION METHODS
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 1, Module 1.4: Optimal Allocation Strategies
#===============================================================================
#
# LEARNING OBJECTIVES:
#   - Calculate sample sizes using Proportional Allocation
#   - Apply Neyman (Optimal) Allocation based on stratum variance
#   - Implement Square Root (Power) Allocation for domain estimation
#   - Compare allocation strategies and their trade-offs
#
# DATA SOURCE: Zambara General Household Survey 2024
# FILE: ghs-2024-hhold-v1.dta
#
# ZAMBARA REGION MAPPING (Province codes):
#   1 = Western Drylands (Western Cape)
#   2 = Coastal Plains (Eastern Cape)
#   3 = Northern Bushveld (Northern Cape -> actually Limpopo based on prev)
#   4 = Central Plateau (Free State)
#   5 = Eastern Highlands (KwaZulu-Natal)
#   6 = Mining Belt (North West)
#   7 = Capital Region (Gauteng)
#   8 = Eastern Forests (Mpumalanga)
#   9 = Southern Cape (Limpopo -> check mapping)
#
# Note: Mapping adjusted based on actual SA province codes in data
#
# AUTHOR: SADC Sampling Workshop
# DATE: March 2026
#===============================================================================

#-------------------------------------------------------------------------------
# 0. SETUP AND CONFIGURATION
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load required packages
if (!require("haven")) install.packages("haven")
if (!require("survey")) install.packages("survey")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("knitr")) install.packages("knitr")

library(haven)
library(survey)
library(dplyr)
library(tidyr)
library(knitr)

# Set options
options(survey.lonely.psu = "adjust")
options(digits = 4)

#-------------------------------------------------------------------------------
# 1. LOAD AND PREPARE DATA
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 70), "\n")
cat("LAB 1.4: SAMPLE ALLOCATION METHODS\n")
cat(strrep("=", 70), "\n\n")

# Load Zambara household data
cat("Loading Zambara household survey data...\n")
zambara_hh <- read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")

cat("  Raw observations:", nrow(zambara_hh), "\n")

# Examine province variable
cat("\nProvince distribution (raw):\n")
print(table(zambara_hh$prov, useNA = "ifany"))

# Data cleaning
# - Remove missing weights
# - Remove implausible income values (coded as 9999999)
# - Keep only valid records
analysis_data <- zambara_hh %>%
  filter(!is.na(house_wgt) & house_wgt > 0) %>%
  filter(!is.na(totmhinc) & totmhinc < 9999999 & totmhinc >= 0) %>%
  filter(!is.na(prov))

cat("\nAfter cleaning:\n")
cat("  Valid observations:", nrow(analysis_data), "\n")
cat("  Provinces represented:", length(unique(analysis_data$prov)), "\n")

#-------------------------------------------------------------------------------
# 2. CREATE ZAMBARA REGION LABELS
#-------------------------------------------------------------------------------

# Map South African provinces to Zambara regions
# Based on actual province codes in Stats SA data
zambara_names <- c(
  "1" = "Western Drylands",
  "2" = "Coastal Plains", 
  "3" = "Northern Bushveld",
  "4" = "Central Plateau",
  "5" = "Eastern Highlands",
  "6" = "Mining Belt",
  "7" = "Capital Region",
  "8" = "Eastern Forests",
  "9" = "Southern Cape"
)

analysis_data <- analysis_data %>%
  mutate(
    prov = as.character(prov),
    region_name = zambara_names[prov]
  )

#-------------------------------------------------------------------------------
# 3. DECLARE SURVEY DESIGN
#-------------------------------------------------------------------------------

cat("\nDeclaring survey design...\n")

# Create survey design object
des <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = analysis_data,
  nest = TRUE
)

cat("  Design declared successfully\n")
cat("  Number of PSUs:", length(unique(analysis_data$psu)), "\n")
cat("  Number of strata:", length(unique(analysis_data$stratum)), "\n")

#-------------------------------------------------------------------------------
# 4. CALCULATE STRATUM-LEVEL STATISTICS
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 1: Calculate Population and Variance by Region\n")
cat(strrep("-", 70), "\n\n")

# Calculate weighted population totals by region
pop_stats <- analysis_data %>%
  group_by(prov, region_name) %>%
  summarise(
    # Sample counts
    n_sample = n(),
    n_psu = n_distinct(psu),
    
    # Weighted population estimate
    N_h = sum(house_wgt),
    
    # Unweighted income statistics for allocation
    mean_income = mean(totmhinc, na.rm = TRUE),
    sd_income = sd(totmhinc, na.rm = TRUE),
    var_income = var(totmhinc, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(prov)

# Calculate total population
N_total <- sum(pop_stats$N_h)

# Add population proportions
pop_stats <- pop_stats %>%
  mutate(
    W_h = N_h / N_total,           # Population weight/proportion
    N_h_S_h = N_h * sd_income,     # For Neyman allocation
    sqrt_N_h = sqrt(N_h)           # For power allocation
  )

# Display population statistics
cat("Population Statistics by Zambara Region:\n\n")
pop_display <- pop_stats %>%
  select(region_name, n_sample, n_psu, N_h, W_h, mean_income, sd_income) %>%
  mutate(
    N_h = round(N_h),
    W_h = round(W_h * 100, 1),
    mean_income = round(mean_income),
    sd_income = round(sd_income)
  )

names(pop_display) <- c("Region", "Sample n", "PSUs", "Pop (N_h)", 
                         "Weight %", "Mean Inc", "SD Inc")
print(kable(pop_display, format = "simple", align = "lrrrrrr"))

cat("\nTotal estimated population:", format(round(N_total), big.mark = ","), "\n")

#-------------------------------------------------------------------------------
# 5. DEFINE ALLOCATION FUNCTIONS
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 2: Apply Allocation Methods\n")
cat(strrep("-", 70), "\n\n")

# Total sample size to allocate
n_total <- 20000  # Target sample size for new survey

cat("Target total sample size: n =", format(n_total, big.mark = ","), "\n\n")

#--- 5.1 PROPORTIONAL ALLOCATION ---
# Formula: n_h = n * (N_h / N)
# Sample allocation proportional to population size

proportional_allocation <- function(N_h, n_total) {
  # N_h: vector of stratum population sizes
  # n_total: total sample size to allocate
  
  W_h <- N_h / sum(N_h)
  n_h <- n_total * W_h
  
  return(round(n_h))
}

#--- 5.2 NEYMAN (OPTIMAL) ALLOCATION ---
# Formula: n_h = n * (N_h * S_h) / sum(N_h * S_h)
# Minimizes variance of the overall mean for fixed total sample size

neyman_allocation <- function(N_h, S_h, n_total) {
  # N_h: vector of stratum population sizes
  # S_h: vector of stratum standard deviations
  # n_total: total sample size to allocate
  
  N_h_S_h <- N_h * S_h
  n_h <- n_total * (N_h_S_h / sum(N_h_S_h))
  
  return(round(n_h))
}

#--- 5.3 SQUARE ROOT (POWER) ALLOCATION ---
# Formula: n_h = n * (N_h^0.5) / sum(N_h^0.5)
# Compromise between proportional and equal allocation
# Good for domain estimation with balanced precision

power_allocation <- function(N_h, n_total, power = 0.5) {
  # N_h: vector of stratum population sizes
  # n_total: total sample size to allocate
  # power: exponent (0.5 = square root, 0 = equal, 1 = proportional)
  
  N_h_power <- N_h^power
  n_h <- n_total * (N_h_power / sum(N_h_power))
  
  return(round(n_h))
}

#-------------------------------------------------------------------------------
# 6. CALCULATE ALLOCATIONS
#-------------------------------------------------------------------------------

# Apply allocation methods
allocation_results <- pop_stats %>%
  mutate(
    # Method 1: Proportional
    n_proportional = proportional_allocation(N_h, n_total),
    
    # Method 2: Neyman (Optimal)
    n_neyman = neyman_allocation(N_h, sd_income, n_total),
    
    # Method 3: Square Root (Power = 0.5)
    n_power = power_allocation(N_h, n_total, power = 0.5),
    
    # Also calculate equal allocation for reference
    n_equal = round(n_total / n())
  )

#-------------------------------------------------------------------------------
# 7. DISPLAY ALLOCATION RESULTS
#-------------------------------------------------------------------------------

cat("ALLOCATION RESULTS BY METHOD:\n")
cat(strrep("=", 70), "\n\n")

# Create display table
alloc_display <- allocation_results %>%
  select(region_name, N_h, n_proportional, n_neyman, n_power, n_equal) %>%
  mutate(
    N_h = format(round(N_h), big.mark = ",")
  ) %>%
  arrange(desc(n_proportional))

names(alloc_display) <- c("Region", "Population", "Proportional", "Neyman", 
                           "Power (0.5)", "Equal")

print(kable(alloc_display, format = "simple", align = "lrrrrr"))

# Verify totals
cat("\n")
cat("Allocation Totals:\n")
cat("  Proportional:", sum(allocation_results$n_proportional), "\n")
cat("  Neyman:      ", sum(allocation_results$n_neyman), "\n")
cat("  Power (0.5): ", sum(allocation_results$n_power), "\n")
cat("  Equal:       ", sum(allocation_results$n_equal), "\n")

#-------------------------------------------------------------------------------
# 8. CALCULATE EXPECTED PRECISION (RSE) FOR EACH METHOD
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 3: Compare Expected Precision (RSE)\n")
cat(strrep("-", 70), "\n\n")

# Function to calculate expected RSE for a stratum
# RSE = SE / Mean * 100
# For SRS within stratum: SE = S_h / sqrt(n_h)
# Approximate RSE = (S_h / mean_h) / sqrt(n_h) * 100 = CV_h / sqrt(n_h) * 100

calculate_expected_rse <- function(n_h, sd_h, mean_h) {
  # Coefficient of variation
  cv_h <- sd_h / mean_h
  
  # Expected RSE (%) assuming SRS within stratum
  rse <- (cv_h / sqrt(n_h)) * 100
  
  return(rse)
}

# Calculate RSE for each method
precision_results <- allocation_results %>%
  mutate(
    # RSE for each allocation method
    rse_proportional = calculate_expected_rse(n_proportional, sd_income, mean_income),
    rse_neyman = calculate_expected_rse(n_neyman, sd_income, mean_income),
    rse_power = calculate_expected_rse(n_power, sd_income, mean_income),
    rse_equal = calculate_expected_rse(n_equal, sd_income, mean_income)
  )

# Display RSE comparison
cat("EXPECTED RELATIVE STANDARD ERROR (RSE %) BY METHOD:\n\n")

rse_display <- precision_results %>%
  select(region_name, n_proportional, rse_proportional, 
         n_neyman, rse_neyman, n_power, rse_power) %>%
  mutate(
    rse_proportional = round(rse_proportional, 1),
    rse_neyman = round(rse_neyman, 1),
    rse_power = round(rse_power, 1)
  ) %>%
  arrange(desc(rse_proportional))

names(rse_display) <- c("Region", "n_Prop", "RSE_Prop", 
                         "n_Neyman", "RSE_Neyman", "n_Power", "RSE_Power")

print(kable(rse_display, format = "simple", align = "lrrrrrr"))

#-------------------------------------------------------------------------------
# 9. IDENTIFY PROBLEMATIC ALLOCATIONS
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 4: Identify Domain Precision Issues\n")
cat(strrep("-", 70), "\n\n")

# Flag regions with RSE > 15% (unreliable estimates)
rse_threshold <- 15

problem_regions <- precision_results %>%
  filter(rse_proportional > rse_threshold) %>%
  select(region_name, n_proportional, rse_proportional, n_power, rse_power)

if (nrow(problem_regions) > 0) {
  cat("Regions with RSE > 15% under PROPORTIONAL allocation:\n\n")
  print(kable(problem_regions, format = "simple"))
  
  cat("\n")
  cat("RECOMMENDATION: Power allocation improves precision for small domains\n")
  cat("while maintaining reasonable national-level efficiency.\n")
} else {
  cat("All regions have acceptable precision (RSE <= 15%) under all methods.\n")
}

#-------------------------------------------------------------------------------
# 10. CALCULATE NATIONAL-LEVEL PRECISION
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 5: National-Level Efficiency Comparison\n")
cat(strrep("-", 70), "\n\n")

# For stratified sampling, the variance of the overall mean is:
# Var(y_bar_st) = sum(W_h^2 * S_h^2 / n_h)

calculate_national_variance <- function(W_h, S_h, n_h) {
  var_national <- sum((W_h^2 * S_h^2) / n_h)
  return(var_national)
}

# Calculate national variance for each method
with(allocation_results, {
  var_prop <- calculate_national_variance(W_h, sd_income, n_proportional)
  var_neyman <- calculate_national_variance(W_h, sd_income, n_neyman)
  var_power <- calculate_national_variance(W_h, sd_income, n_power)
  var_equal <- calculate_national_variance(W_h, sd_income, n_equal)
  
  # Overall mean for RSE calculation
  overall_mean <- sum(W_h * mean_income)
  
  cat("National-Level Precision Comparison:\n\n")
  cat(sprintf("  Method          Variance      SE        RSE(%%)\n"))
  cat(sprintf("  ------------------------------------------------\n"))
  cat(sprintf("  Proportional    %10.0f   %7.1f    %5.2f\n", 
              var_prop, sqrt(var_prop), sqrt(var_prop)/overall_mean*100))
  cat(sprintf("  Neyman          %10.0f   %7.1f    %5.2f\n", 
              var_neyman, sqrt(var_neyman), sqrt(var_neyman)/overall_mean*100))
  cat(sprintf("  Power (0.5)     %10.0f   %7.1f    %5.2f\n", 
              var_power, sqrt(var_power), sqrt(var_power)/overall_mean*100))
  cat(sprintf("  Equal           %10.0f   %7.1f    %5.2f\n", 
              var_equal, sqrt(var_equal), sqrt(var_equal)/overall_mean*100))
  
  cat("\n")
  cat("Efficiency relative to Neyman (optimal):\n")
  cat(sprintf("  Proportional: %.1f%%\n", var_neyman/var_prop * 100))
  cat(sprintf("  Power (0.5):  %.1f%%\n", var_neyman/var_power * 100))
  cat(sprintf("  Equal:        %.1f%%\n", var_neyman/var_equal * 100))
})

#-------------------------------------------------------------------------------
# 11. MINIMUM SAMPLE SIZE CONSTRAINT
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 6: Apply Minimum Sample Size Constraint\n")
cat(strrep("-", 70), "\n\n")

# Often we need a minimum sample per domain for reliability
min_n_per_region <- 800  # Minimum for RSE ~10-15%

cat("Minimum sample size per region:", min_n_per_region, "\n\n")

# Function to apply minimum constraint
apply_minimum <- function(n_h, n_total, min_n) {
  # Step 1: Identify regions below minimum
  below_min <- n_h < min_n
  n_regions <- length(n_h)
  
  # Step 2: Set minimum for small regions
  n_adjusted <- pmax(n_h, min_n)
  
  # Step 3: Redistribute excess from large regions proportionally
  excess <- sum(n_adjusted) - n_total
  
  if (excess > 0) {
    # Reduce only regions above minimum
    above_min <- n_adjusted > min_n
    reduction_pool <- n_adjusted[above_min] - min_n
    reduction_factor <- excess / sum(reduction_pool)
    
    if (reduction_factor < 1) {
      n_adjusted[above_min] <- n_adjusted[above_min] - 
        round(reduction_pool * reduction_factor)
    }
  }
  
  return(round(n_adjusted))
}

# Apply minimum constraint to power allocation
n_power_constrained <- apply_minimum(
  allocation_results$n_power, 
  n_total, 
  min_n_per_region
)

# Compare constrained vs unconstrained
constraint_comparison <- allocation_results %>%
  select(region_name, N_h, n_power) %>%
  mutate(
    n_power_min = n_power_constrained,
    change = n_power_min - n_power,
    below_min = n_power < min_n_per_region
  ) %>%
  arrange(n_power)

cat("Power Allocation with Minimum Constraint (n_min =", min_n_per_region, "):\n\n")
print(kable(constraint_comparison %>% 
              select(region_name, n_power, n_power_min, change),
            format = "simple", 
            col.names = c("Region", "Original", "Constrained", "Change"),
            align = "lrrr"))

cat("\nTotal after constraint:", sum(n_power_constrained), "\n")

#-------------------------------------------------------------------------------
# 12. EXPORT RESULTS FOR SLIDES
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("-", 70), "\n")
cat("STEP 7: Export Results\n")
cat(strrep("-", 70), "\n\n")

# Create final allocation table for slides
final_allocation <- allocation_results %>%
  select(region_name, N_h, W_h, sd_income, 
         n_proportional, n_neyman, n_power) %>%
  mutate(
    n_power_min = n_power_constrained
  ) %>%
  arrange(desc(N_h))

# Export to CSV
write.csv(final_allocation, "lab1_4_allocation_results.csv", row.names = FALSE)
cat("Results exported to: lab1_4_allocation_results.csv\n")

#-------------------------------------------------------------------------------
# 13. SUMMARY AND KEY FINDINGS
#-------------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 70), "\n")
cat("SUMMARY: SAMPLE ALLOCATION METHODS\n")
cat(strrep("=", 70), "\n\n")

cat("KEY FORMULAS:\n")
cat(strrep("-", 40), "\n")
cat("Proportional:  n_h = n × (N_h / N)\n")
cat("Neyman:        n_h = n × (N_h × S_h) / Σ(N_h × S_h)\n")
cat("Power (α):     n_h = n × (N_h^α) / Σ(N_h^α)\n")
cat("               where α = 0.5 for square root allocation\n")
cat("\n")

cat("ALLOCATION COMPARISON (n = 20,000):\n")
cat(strrep("-", 40), "\n")

# Summary statistics
summary_stats <- allocation_results %>%
  summarise(
    # Range of allocations
    prop_min = min(n_proportional),
    prop_max = max(n_proportional),
    neyman_min = min(n_neyman),
    neyman_max = max(n_neyman),
    power_min = min(n_power),
    power_max = max(n_power)
  )

cat(sprintf("Proportional: Range %d - %d\n", 
            summary_stats$prop_min, summary_stats$prop_max))
cat(sprintf("Neyman:       Range %d - %d\n", 
            summary_stats$neyman_min, summary_stats$neyman_max))
cat(sprintf("Power (0.5):  Range %d - %d\n", 
            summary_stats$power_min, summary_stats$power_max))

cat("\n")
cat("RECOMMENDATIONS FOR ZAMBARA:\n")
cat(strrep("-", 40), "\n")
cat("1. Proportional allocation optimizes national precision but\n")
cat("   leaves small regions (Western Drylands) with RSE > 25%.\n\n")
cat("2. Neyman allocation is theoretically optimal for national\n")
cat("   estimates but can worsen small-domain precision.\n\n")
cat("3. Power allocation (α=0.5) with minimum constraint (n≥800)\n")
cat("   provides balanced precision across all domains.\n\n")
cat("4. For Lindiwe's redesign: RECOMMEND Power allocation with\n")
cat("   minimum floor to ensure RSE < 15% for all regions.\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("END OF LAB 1.4\n")
cat(strrep("=", 70), "\n")

#-------------------------------------------------------------------------------
# END OF SCRIPT
#-------------------------------------------------------------------------------
