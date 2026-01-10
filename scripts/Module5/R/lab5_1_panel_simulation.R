###############################################################################
#                                                                             #
#   LAB 5.1: PANEL DATA SIMULATION AND LINKING                                #
#   SADC Regional Training Workshop on Advanced Sampling Methods              #
#   Day 5: Synthesis, Longitudinal Surveys, and Conclusion                    #
#                                                                             #
#   Republic of Zambara — Quarterly Labour Force Survey Pilot                 #
#                                                                             #
#   Author: SADC Statistical Training Programme                               #
#   Date: March 2026                                                          #
#   Software: R 4.x with survey, haven, dplyr packages                        #
#                                                                             #
###############################################################################

# =============================================================================
# NARRATIVE CONTEXT
# =============================================================================
#
# Lindiwe has convinced the Ministry to pilot a rotating panel design for the
# Zambara Quarterly Labour Force Survey (ZQLFS). The first wave was fielded in
# Q1 2026. Now, as Q2 approaches, she must understand what happens when 
# households drop out of the panel — the dreaded problem of ATTRITION.
#
# In this lab, we simulate what Wave 2 might look like if 20% of households
# fail to be re-interviewed. We explore:
#   1. How attrition creates gaps in our longitudinal data
#   2. How attrition rates vary by province (non-random attrition)
#   3. How to link records across waves using unique identifiers
#   4. The foundation for attrition weight adjustments (Lab 5.2)
#
# Key Learning Objectives:
#   - Understand panel data structure and linking mechanisms
#   - Simulate realistic non-random attrition patterns
#   - Calculate and interpret attrition rates by subgroup
#   - Prepare linked panel datasets for longitudinal analysis
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP AND DATA LOADING
# =============================================================================

# Clear workspace and set options
rm(list = ls())
options(scipen = 999)  # Disable scientific notation for IDs

# Load required packages
# Note: Install packages first if needed using install.packages()
library(haven)      # Reading STATA files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(survey)     # Complex survey analysis
library(ggplot2)    # Visualization

# Set seed for reproducibility
# This ensures all participants get identical "random" results
set.seed(20260306)  # Date of Day 5: 6 March 2026

# -----------------------------------------------------------------------------
# Define file paths
# Participants should modify this path to match their local setup
# -----------------------------------------------------------------------------
data_path <- "./data/GHS_2024/ghs-2024-hhold-v1.dta"

# Alternative paths for workshop setup:
# data_path <- "C:/SADC_Workshop/Data/ghs-2024-hhold-v1.dta"  # Windows
# data_path <- "~/SADC_Workshop/Data/ghs-2024-hhold-v1.dta"   # Mac/Linux

# -----------------------------------------------------------------------------
# Load Wave 1 data (GHS 2024 treated as ZQLFS Wave 1)
# -----------------------------------------------------------------------------
cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LOADING WAVE 1 DATA (GHS 2024 as ZQLFS Q1 2026)\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

wave1_raw <- read_dta(data_path)

# Examine the structure
cat("Wave 1 raw data dimensions:", nrow(wave1_raw), "households x", 
    ncol(wave1_raw), "variables\n\n")

# -----------------------------------------------------------------------------
# IMPORTANT: Explore variable names in the dataset
# Variable names may differ between GHS releases and from metadata documentation
# -----------------------------------------------------------------------------
cat("EXPLORING VARIABLE NAMES IN DATASET:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("All variable names:\n")
print(names(wave1_raw))
cat("\n")

# Helper function to find variables matching a pattern (case-insensitive)
find_var <- function(data, pattern) {
  matches <- grep(pattern, names(data), ignore.case = TRUE, value = TRUE)
  if(length(matches) > 0) {
    cat(sprintf("  Pattern '%s' matches: %s\n", pattern, paste(matches, collapse = ", ")))
  }
  return(matches)
}

cat("Searching for key variables:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

# Search for unique identifier (could be UqNr, UQNO, uqnr, unique_id, etc.)
id_vars <- find_var(wave1_raw, "uq|unique|hhid|hh_id")

# Search for PSU
psu_vars <- find_var(wave1_raw, "psu|cluster")

# Search for stratum
strat_vars <- find_var(wave1_raw, "strat|strata")

# Search for weight
weight_vars <- find_var(wave1_raw, "wgt|weight|wt")

# Search for province
prov_vars <- find_var(wave1_raw, "prov|region")

# Search for geography type
geo_vars <- find_var(wave1_raw, "geo|urban|type")

# Search for household size
size_vars <- find_var(wave1_raw, "hhold|hhsize|size|member")

# Search for income
income_vars <- find_var(wave1_raw, "inc|income|earn|salary")

# Search for head characteristics
head_vars <- find_var(wave1_raw, "head")

cat("\n")

# -----------------------------------------------------------------------------
# Define variable mapping based on actual data
# PARTICIPANTS: Update these mappings if your variable names differ
# -----------------------------------------------------------------------------
cat("VARIABLE MAPPING (update if needed):\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

# Auto-detect or set manually - using common GHS naming conventions
# These are the most likely variable names based on GHS documentation

# Function to get first match or use fallback
get_var_name <- function(data, patterns, fallback = NULL) {
  for(p in patterns) {
    matches <- grep(paste0("^", p, "$"), names(data), ignore.case = TRUE, value = TRUE)
    if(length(matches) > 0) return(matches[1])
  }
  return(fallback)
}

# Unique household identifier
var_id <- get_var_name(wave1_raw, c("UqNr", "UQNO", "uqnr", "unique_id", "hhid"))
cat(sprintf("  Unique ID variable: %s\n", ifelse(is.null(var_id), "NOT FOUND", var_id)))

# PSU
var_psu <- get_var_name(wave1_raw, c("PSU", "psu", "PSUNO", "cluster"))
cat(sprintf("  PSU variable: %s\n", ifelse(is.null(var_psu), "NOT FOUND", var_psu)))

# Stratum
var_stratum <- get_var_name(wave1_raw, c("Stratum", "stratum", "STRATUM", "strata"))
cat(sprintf("  Stratum variable: %s\n", ifelse(is.null(var_stratum), "NOT FOUND", var_stratum)))

# Household weight
var_weight <- get_var_name(wave1_raw, c("house_wgt", "HOUSE_WGT", "hh_wgt", "hhwgt", "weight"))
cat(sprintf("  Weight variable: %s\n", ifelse(is.null(var_weight), "NOT FOUND", var_weight)))

# Province
var_prov <- get_var_name(wave1_raw, c("Prov", "prov", "PROV", "province", "region"))
cat(sprintf("  Province variable: %s\n", ifelse(is.null(var_prov), "NOT FOUND", var_prov)))

# Geography type
var_geotype <- get_var_name(wave1_raw, c("GeoType", "geotype", "GEOTYPE", "geo_type", "urban"))
cat(sprintf("  GeoType variable: %s\n", ifelse(is.null(var_geotype), "NOT FOUND", var_geotype)))

# Household size
var_hhsize <- get_var_name(wave1_raw, c("hholdsz", "HHOLDSZ", "hhsize", "hh_size", "hhsz"))
cat(sprintf("  HH Size variable: %s\n", ifelse(is.null(var_hhsize), "NOT FOUND", var_hhsize)))

# Head age
var_head_age <- get_var_name(wave1_raw, c("head_age", "HEAD_AGE", "headage", "age_head"))
cat(sprintf("  Head Age variable: %s\n", ifelse(is.null(var_head_age), "NOT FOUND", var_head_age)))
 
# Head sex
var_head_sex <- get_var_name(wave1_raw, c("head_sex", "HEAD_SEX", "headsex", "sex_head"))
cat(sprintf("  Head Sex variable: %s\n", ifelse(is.null(var_head_sex), "NOT FOUND", var_head_sex)))

# Total monthly household income
var_income <- get_var_name(wave1_raw, c("totmhinc", "TOTMHINC", "hh_income", "income", "tot_inc"))
cat(sprintf("  Income variable: %s\n", ifelse(is.null(var_income), "NOT FOUND", var_income)))

# Economically active members
var_econact <- get_var_name(wave1_raw, c("econact_hh", "ECONACT_HH", "econact", "employed"))
cat(sprintf("  Econ Active variable: %s\n", ifelse(is.null(var_econact), "NOT FOUND", var_econact)))

cat("\n")

# Check for missing critical variables
missing_vars <- c()
if(is.null(var_id)) missing_vars <- c(missing_vars, "Unique ID")
if(is.null(var_prov)) missing_vars <- c(missing_vars, "Province")
if(is.null(var_income)) missing_vars <- c(missing_vars, "Income")

if(length(missing_vars) > 0) {
  cat("WARNING: The following critical variables were not found:\n")
  cat(paste("  -", missing_vars, collapse = "\n"), "\n")
  cat("\nPlease check the variable names above and update the mapping.\n")
  cat("You may need to manually set the variable names in the script.\n\n")
  
  # Show first few rows to help identify variables
  cat("First 3 rows of data (to help identify variables):\n")
  print(head(wave1_raw, 3))
  
  stop("Critical variables not found. Please update variable mapping.")
}

# =============================================================================
# SECTION 2: PREPARE WAVE 1 PANEL DATASET
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("PREPARING WAVE 1 PANEL DATASET\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# -----------------------------------------------------------------------------
# Select and rename key variables for panel analysis
# We use Zambara naming conventions for the fictional country
# Using dynamically detected variable names from Section 1
# -----------------------------------------------------------------------------

# Build the selection and transformation dynamically
wave1 <- wave1_raw

# Create standardized variable names using detected variables
wave1$hh_id <- as.character(wave1[[var_id]])
wave1$psu <- wave1[[var_psu]]
wave1$stratum <- if(!is.null(var_stratum)) wave1[[var_stratum]] else NA
wave1$hh_weight <- if(!is.null(var_weight)) wave1[[var_weight]] else 1
wave1$province <- wave1[[var_prov]]
wave1$geotype <- if(!is.null(var_geotype)) wave1[[var_geotype]] else NA
wave1$hh_size <- if(!is.null(var_hhsize)) wave1[[var_hhsize]] else NA
wave1$head_age <- if(!is.null(var_head_age)) wave1[[var_head_age]] else NA
wave1$head_sex <- if(!is.null(var_head_sex)) wave1[[var_head_sex]] else NA
wave1$income_w1 <- wave1[[var_income]]
wave1$employed_hh <- if(!is.null(var_econact)) wave1[[var_econact]] else NA

# Create province name mapping (Zambara regions)
wave1$province_name <- case_when(
  wave1$province == 1 ~ "Southern Cape",         # Western Cape
  wave1$province == 2 ~ "Coastal Plains",        # Eastern Cape
  wave1$province == 3 ~ "Western Drylands",      # Northern Cape
  wave1$province == 4 ~ "Central Plateau",       # Free State
  wave1$province == 5 ~ "Eastern Highlands",     # KwaZulu-Natal
  wave1$province == 6 ~ "Mining Belt",           # North West
  wave1$province == 7 ~ "Zambara Capital",       # Gauteng
  wave1$province == 8 ~ "Eastern Forests",       # Mpumalanga
  wave1$province == 9 ~ "Northern Bushveld",     # Limpopo
  TRUE ~ "Unknown"
)

# Create geotype name mapping
wave1$geotype_name <- case_when(
  wave1$geotype == 1 ~ "Urban",
  wave1$geotype == 2 ~ "Traditional",
  wave1$geotype == 3 ~ "Farms",
  TRUE ~ "Unknown"
)

# Add wave identifier
wave1$wave <- 1L

# Select only the standardized variables
wave1 <- wave1 %>%
  select(
    hh_id, psu, stratum, hh_weight,
    province, province_name, geotype, geotype_name,
    hh_size, head_age, head_sex,
    income_w1, employed_hh, wave
  ) %>%
  # Remove households with missing critical identifiers
  filter(!is.na(hh_id) & !is.na(income_w1))

cat("Wave 1 prepared dataset:\n")
cat("  Total households:", format(nrow(wave1), big.mark = ","), "\n")
cat("  Variables:", ncol(wave1), "\n\n")

# -----------------------------------------------------------------------------
# Wave 1 Summary Statistics by Province
# -----------------------------------------------------------------------------
wave1_summary <- wave1 %>%
  group_by(province, province_name) %>%
  summarise(
    n_households = n(),
    mean_income = mean(income_w1, na.rm = TRUE),
    median_income = median(income_w1, na.rm = TRUE),
    mean_hh_size = mean(hh_size, na.rm = TRUE),
    pct_urban = mean(geotype == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(province)

cat("Wave 1 Summary by Province (Zambara Regions):\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
print(as.data.frame(wave1_summary), row.names = FALSE)
cat("\n")

# =============================================================================
# SECTION 3: SIMULATE ATTRITION FOR WAVE 2
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SIMULATING WAVE 2 ATTRITION\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# -----------------------------------------------------------------------------
# Key Insight: Attrition is NEVER random!
# 
# In real panel surveys, households that drop out differ systematically from
# those that remain. Common patterns:
#   - Urban households have higher mobility → higher attrition
#   - Lower-income households may be harder to trace → higher attrition
#   - Younger household heads move more often → higher attrition
#   - Certain regions may have infrastructure challenges → higher attrition
#
# We simulate NON-RANDOM attrition to reflect these real-world patterns.
# -----------------------------------------------------------------------------

# Define base attrition probability (20% overall target)
base_attrition <- 0.20

# Define province-specific attrition multipliers
# These reflect differential fieldwork challenges across Zambara
province_attrition_multiplier <- c(
  "1" = 1.20,   # Southern Cape: High urban mobility
  "2" = 0.85,   # Coastal Plains: Stable communities
  "3" = 1.30,   # Western Drylands: Remote, sparse population
  "4" = 0.90,   # Central Plateau: Moderate
  "5" = 0.95,   # Eastern Highlands: Moderate
  "6" = 1.10,   # Mining Belt: Labor migration
  "7" = 1.40,   # Zambara Capital: Highest mobility
  "8" = 0.80,   # Eastern Forests: Stable rural
  "9" = 0.75    # Northern Bushveld: Most stable
)

# Calculate household-specific attrition probability
wave1 <- wave1 %>%
  mutate(
    # Province effect
    prov_mult = province_attrition_multiplier[as.character(province)],
    
    # Urban areas have 20% higher attrition
    urban_mult = ifelse(geotype == 1, 1.20, 1.00),
    
    # Young household heads (under 35) have 15% higher attrition
    age_mult = ifelse(head_age < 35 & !is.na(head_age), 1.15, 1.00),
    
    # Calculate individual attrition probability
    # Capped at 0.50 to avoid unrealistic rates
    p_attrit = pmin(0.50, base_attrition * prov_mult * urban_mult * age_mult),
    
    # Generate random attrition indicator
    # 1 = household drops out, 0 = household remains in panel
    attrit = rbinom(n(), 1, p_attrit)
  )

# -----------------------------------------------------------------------------
# Examine attrition simulation results
# -----------------------------------------------------------------------------
cat("Attrition Simulation Results:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

overall_attrition <- mean(wave1$attrit) * 100
cat(sprintf("Overall attrition rate: %.1f%%\n", overall_attrition))
cat(sprintf("Households retained: %s (%.1f%%)\n", 
            format(sum(wave1$attrit == 0), big.mark = ","),
            100 - overall_attrition))
cat(sprintf("Households lost: %s (%.1f%%)\n\n", 
            format(sum(wave1$attrit == 1), big.mark = ","),
            overall_attrition))

# =============================================================================
# SECTION 4: ATTRITION RATES BY PROVINCE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("ATTRITION RATES BY PROVINCE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

attrition_by_province <- wave1 %>%
  group_by(province, province_name) %>%
  summarise(
    n_wave1 = n(),
    n_attrited = sum(attrit),
    n_retained = sum(attrit == 0),
    attrition_rate = mean(attrit) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(attrition_rate))

cat("Attrition Rates by Province (Sorted by Attrition Rate):\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")

# Format for display
attrition_display <- attrition_by_province %>%
  mutate(
    attrition_rate = sprintf("%.1f%%", attrition_rate)
  )
print(as.data.frame(attrition_display), row.names = FALSE)
cat("\n")

# -----------------------------------------------------------------------------
# KEY INSIGHT BOX
# -----------------------------------------------------------------------------
cat("┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│  KEY INSIGHT: Non-Random Attrition                                 │\n")
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat("│  Notice that attrition rates vary substantially by province:       │\n")
cat(sprintf("│    - Highest: Zambara Capital (urban mobility)                     │\n"))
cat(sprintf("│    - Lowest: Northern Bushveld (stable rural communities)          │\n"))
cat("│                                                                     │\n")
cat("│  This NON-RANDOM pattern means that simple unweighted estimates    │\n")
cat("│  from Wave 2 will be BIASED. We will address this in Lab 5.2      │\n")
cat("│  using attrition weight adjustments.                               │\n")
cat("└─────────────────────────────────────────────────────────────────────┘\n\n")

# =============================================================================
# SECTION 5: CREATE WAVE 2 DATASET
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("CREATING WAVE 2 DATASET\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# -----------------------------------------------------------------------------
# Wave 2 includes only households that did NOT attrit
# We also simulate changes in income between waves
# -----------------------------------------------------------------------------

# Create Wave 2 dataset from retained households
wave2 <- wave1 %>%
  filter(attrit == 0) %>%
  mutate(
    # Update wave identifier
    wave = 2L,
    
    # Simulate income change between Wave 1 and Wave 2
    # Income changes are drawn from a mixture distribution:
    #   - Most households: small random change (normal, mean=0, sd=0.10)
    #   - Some households: larger positive change (job gained)
    #   - Some households: larger negative change (job lost)
    
    # Generate change type
    change_type = sample(c("stable", "increase", "decrease"), 
                         n(), replace = TRUE,
                         prob = c(0.70, 0.15, 0.15)),
    
    # Calculate income multiplier based on change type
    income_multiplier = case_when(
      change_type == "stable" ~ rnorm(n(), mean = 1.02, sd = 0.08),
      change_type == "increase" ~ rnorm(n(), mean = 1.25, sd = 0.15),
      change_type == "decrease" ~ rnorm(n(), mean = 0.80, sd = 0.12),
      TRUE ~ 1.0
    ),
    
    # Apply multiplier to get Wave 2 income
    # Ensure non-negative
    income_w2 = pmax(0, income_w1 * income_multiplier),
    
    # Calculate income change
    income_change = income_w2 - income_w1,
    income_pct_change = (income_change / income_w1) * 100
  ) %>%
  # Select variables for Wave 2
  select(
    hh_id, psu, stratum, hh_weight,
    province, province_name, geotype, geotype_name,
    hh_size, head_age, head_sex,
    income_w2,
    wave
  )

cat("Wave 2 dataset created:\n")
cat(sprintf("  Households in Wave 2: %s\n", format(nrow(wave2), big.mark = ",")))
cat(sprintf("  Retention rate: %.1f%%\n\n", nrow(wave2) / nrow(wave1) * 100))

# Wave 2 summary
wave2_summary <- wave2 %>%
  group_by(province, province_name) %>%
  summarise(
    n_households = n(),
    mean_income_w2 = mean(income_w2, na.rm = TRUE),
    .groups = "drop"
  )

cat("Wave 2 Summary by Province:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
print(as.data.frame(wave2_summary), row.names = FALSE)
cat("\n")

# =============================================================================
# SECTION 6: PANEL DATA LINKING
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("PANEL DATA LINKING: THE 'TIME TRAVEL' MERGE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# -----------------------------------------------------------------------------
# The unique household identifier (hh_id) is our "time travel" key
# It allows us to link the same household across different waves
# -----------------------------------------------------------------------------

# Prepare Wave 1 for merge (select key variables)
wave1_for_merge <- wave1 %>%
  select(
    hh_id, 
    income_w1,
    province, province_name, geotype, geotype_name,
    hh_size, head_age, head_sex,
    hh_weight, psu, stratum,
    attrit
  )

# Prepare Wave 2 for merge
wave2_for_merge <- wave2 %>%
  select(
    hh_id,
    income_w2
  )

# Perform the panel merge
# This is an INNER JOIN - only households present in BOTH waves
panel_linked <- wave1_for_merge %>%
  inner_join(wave2_for_merge, by = "hh_id")

cat("Panel Linking Results:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat(sprintf("  Wave 1 households: %s\n", format(nrow(wave1), big.mark = ",")))
cat(sprintf("  Wave 2 households: %s\n", format(nrow(wave2), big.mark = ",")))
cat(sprintf("  Linked panel: %s households (balanced panel)\n\n", 
            format(nrow(panel_linked), big.mark = ",")))

# -----------------------------------------------------------------------------
# Calculate income changes for linked panel
# -----------------------------------------------------------------------------
panel_linked <- panel_linked %>%
  mutate(
    income_change = income_w2 - income_w1,
    income_pct_change = ifelse(income_w1 > 0, 
                               (income_change / income_w1) * 100,
                               NA)
  )

# Summary of income changes
cat("Income Dynamics in Linked Panel:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat(sprintf("  Mean income Wave 1: R %.0f\n", mean(panel_linked$income_w1)))
cat(sprintf("  Mean income Wave 2: R %.0f\n", mean(panel_linked$income_w2)))
cat(sprintf("  Mean change: R %.0f (%.1f%%)\n", 
            mean(panel_linked$income_change),
            mean(panel_linked$income_pct_change, na.rm = TRUE)))
cat(sprintf("  Households with income increase: %.1f%%\n",
            mean(panel_linked$income_change > 0) * 100))
cat(sprintf("  Households with income decrease: %.1f%%\n",
            mean(panel_linked$income_change < 0) * 100))
cat("\n")

# =============================================================================
# SECTION 7: COMPARE SURVIVORS VS. ATTRITORS
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("COMPARING SURVIVORS VS. ATTRITORS (Wave 1 Characteristics)\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# This comparison reveals the selection bias introduced by attrition
comparison <- wave1 %>%
  group_by(attrit) %>%
  summarise(
    n = n(),
    mean_income = mean(income_w1, na.rm = TRUE),
    median_income = median(income_w1, na.rm = TRUE),
    mean_hh_size = mean(hh_size, na.rm = TRUE),
    mean_head_age = mean(head_age, na.rm = TRUE),
    pct_urban = mean(geotype == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(attrit == 0, "Survivors (Panel)", "Attritors (Lost)")
  ) %>%
  select(group, everything(), -attrit)

cat("Characteristics Comparison:\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
print(as.data.frame(comparison), row.names = FALSE)
cat("\n")

# Statistical test for difference
t_test_income <- t.test(income_w1 ~ attrit, data = wave1)
cat(sprintf("T-test for income difference: t = %.2f, p = %.4f\n",
            t_test_income$statistic, t_test_income$p.value))

if(t_test_income$p.value < 0.05) {
  cat(">>> SIGNIFICANT difference in income between groups!\n")
  cat(">>> This confirms NON-RANDOM attrition.\n\n")
}

# =============================================================================
# SECTION 8: FINAL ATTRITION REPORT TABLE
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("FINAL ATTRITION REPORT BY PROVINCE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

final_report <- wave1 %>%
  group_by(province, province_name) %>%
  summarise(
    `Wave 1 (N)` = n(),
    `Wave 2 (N)` = sum(attrit == 0),
    `Attrited (N)` = sum(attrit == 1),
    `Attrition Rate (%)` = sprintf("%.1f", mean(attrit) * 100),
    `Retention Rate (%)` = sprintf("%.1f", mean(attrit == 0) * 100),
    .groups = "drop"
  ) %>%
  rename(
    Province = province,
    `Province Name` = province_name
  )

cat("ZAMBARA QUARTERLY LABOUR FORCE SURVEY\n")
cat("Attrition Analysis: Wave 1 to Wave 2\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

print(as.data.frame(final_report), row.names = FALSE)

# Add totals row
total_row <- wave1 %>%
  summarise(
    Province = 99,
    `Province Name` = "TOTAL ZAMBARA",
    `Wave 1 (N)` = n(),
    `Wave 2 (N)` = sum(attrit == 0),
    `Attrited (N)` = sum(attrit == 1),
    `Attrition Rate (%)` = sprintf("%.1f", mean(attrit) * 100),
    `Retention Rate (%)` = sprintf("%.1f", mean(attrit == 0) * 100)
  )

cat("\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
print(as.data.frame(total_row), row.names = FALSE)
cat("\n")

# =============================================================================
# SECTION 9: SAVE OUTPUTS FOR LAB 5.2
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SAVING OUTPUTS FOR SUBSEQUENT LABS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Save datasets for Lab 5.2 (Attrition Weight Adjustment)
output_dir <- "./"

# Save Wave 1 with attrition indicators
saveRDS(wave1, file.path(output_dir, "zambara_wave1_with_attrition.rds"))
cat(sprintf("Saved: %s\n", file.path(output_dir, "zambara_wave1_with_attrition.rds")))

# Save Wave 2
saveRDS(wave2, file.path(output_dir, "zambara_wave2.rds"))
cat(sprintf("Saved: %s\n", file.path(output_dir, "zambara_wave2.rds")))

# Save linked panel
saveRDS(panel_linked, file.path(output_dir, "zambara_panel_linked.rds"))
cat(sprintf("Saved: %s\n", file.path(output_dir, "zambara_panel_linked.rds")))

# Save attrition report
write.csv(final_report, file.path(output_dir, "attrition_report_by_province.csv"),
          row.names = FALSE)
cat(sprintf("Saved: %s\n", file.path(output_dir, "attrition_report_by_province.csv")))

cat("\n")

# =============================================================================
# SECTION 10: VISUALIZATION
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("GENERATING VISUALIZATION\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Create attrition rate bar chart
attrition_plot <- attrition_by_province %>%
  ggplot(aes(x = reorder(province_name, attrition_rate), y = attrition_rate)) +
  geom_bar(stat = "identity", fill = "#003366", alpha = 0.8) +
  geom_hline(yintercept = overall_attrition, linetype = "dashed", 
             color = "#D4AF37", linewidth = 1) +
  annotate("text", x = 1.5, y = overall_attrition + 1.5, 
           label = sprintf("National Average: %.1f%%", overall_attrition),
           color = "#D4AF37", hjust = 0, size = 3.5) +
  coord_flip() +
  labs(
    title = "Attrition Rates by Province: Zambara QLFS Wave 1 to Wave 2",
    subtitle = "Non-random attrition threatens panel representativeness",
    x = "",
    y = "Attrition Rate (%)",
    caption = "Source: ZQLFS Pilot Simulation | SADC Training Workshop 2026"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text = element_text(size = 9),
    panel.grid.major.y = element_blank()
  )

# Save plot
ggsave(file.path(output_dir, "attrition_by_province.png"), 
       attrition_plot, width = 10, height = 6, dpi = 150)
cat(sprintf("Saved: %s\n\n", file.path(output_dir, "attrition_by_province.png")))

# =============================================================================
# SUMMARY AND NEXT STEPS
# =============================================================================

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LAB 5.1 COMPLETE: SUMMARY AND NEXT STEPS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("WHAT WE ACCOMPLISHED:\n")
cat("  1. Loaded GHS 2024 data as Zambara QLFS Wave 1\n")
cat("  2. Simulated non-random attrition for Wave 2\n")
cat("  3. Created linked panel dataset across waves\n")
cat("  4. Calculated attrition rates by province\n")
cat("  5. Demonstrated selection bias between survivors and attritors\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("  - Overall attrition rate: %.1f%%\n", overall_attrition))
cat("  - Attrition is NOT random — varies by province and household type\n")
cat("  - Urban areas and mobile households more likely to attrit\n")
cat("  - This creates BIAS if not addressed through weighting\n\n")

cat("NEXT STEPS (Lab 5.2):\n")
cat("  - Develop response propensity models\n")
cat("  - Calculate attrition weight adjustments\n")
cat("  - Compare weighted vs. unweighted estimates\n")
cat("  - Assess bias reduction from attrition weights\n\n")

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("END OF LAB 5.1\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

###############################################################################
#                           END OF SCRIPT                                     #
###############################################################################
