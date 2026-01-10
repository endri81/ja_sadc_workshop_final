#===============================================================================
# LAB 4.1: RESPONSE RATE ANALYSIS
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 4: Advanced Error Mitigation and Quality Assurance
#===============================================================================
#
# LEARNING OBJECTIVES:
# 1. Understand and simulate realistic non-response patterns
# 2. Calculate AAPOR Standard Response Rates (RR1)
# 3. Analyze response rates by domain (Province, Geotype)
# 4. Diagnose response bias using auxiliary variables
#
# DATA: Statistics South Africa General Household Survey 2024
#       Household file: ghs-2024-hhold-v1.dta
#
# ZAMBARA NARRATIVE:
# Lindiwe examines the field monitoring dashboard and discovers concerning
# non-response patterns. Before she can address the problem, she needs to
# quantify the extent and distribution of non-response across the country.
#
# REFERENCE:
# AAPOR (2023). Standard Definitions: Final Dispositions of Case Codes and 
#               Outcome Rates for Surveys. 10th edition.
# Groves, R.M. et al. (2009). Survey Methodology. 2nd ed. Wiley.
#
#===============================================================================

# Clear workspace
rm(list = ls())

# Set seed for reproducibility
set.seed(20260305)  # Day 4 date: 5 March 2026

#-------------------------------------------------------------------------------
# SECTION 1: LOAD REQUIRED PACKAGES
#-------------------------------------------------------------------------------

# Install packages if not available
required_packages <- c("haven", "survey", "dplyr", "tidyr", "ggplot2", 
                       "scales", "knitr", "kableExtra", "gridExtra")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("LAB 4.1: RESPONSE RATE ANALYSIS\n")
cat("SADC Advanced Sampling Workshop - Day 4\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

#-------------------------------------------------------------------------------
# SECTION 2: LOAD AND PREPARE DATA
#-------------------------------------------------------------------------------

cat("SECTION 2: Loading GHS 2024 Household Data\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Load the household data
# NOTE: Adjust path as needed for your environment
data_path <- "./data/GHS_2024/ghs-2024-hhold-v1.dta"

# Check if file exists, if not provide instructions
if (!file.exists(data_path)) {
  cat("\nNOTE: Data file not found at:", data_path, "\n")
  cat("Please ensure the GHS 2024 household file is in your working directory.\n")
  cat("Expected filename: ghs-2024-hhold-v1.dta\n\n")
  
  # For demonstration, create synthetic data with realistic structure
  cat("Creating synthetic demonstration data...\n\n")
  
  # Create synthetic data matching GHS 2024 structure
  n_hh <- 20940  # Approximate GHS 2024 household sample size
  
  # Province distribution (approximate from GHS 2024)
  prov_probs <- c(0.08, 0.12, 0.02, 0.05, 0.16, 0.06, 0.27, 0.07, 0.10)
  prov_names <- c("Western Cape", "Eastern Cape", "Northern Cape", 
                  "Free State", "KwaZulu-Natal", "North West",
                  "Gauteng", "Mpumalanga", "Limpopo")
  
  # Geotype distribution
  geo_probs <- c(0.65, 0.28, 0.07)  # Urban, Traditional, Farms
  
  ghs_hh <- data.frame(
    hhid = 1:n_hh,
    psu = sample(1:3218, n_hh, replace = TRUE),
    stratum = sample(10101:90401, n_hh, replace = TRUE),
    prov = sample(1:9, n_hh, replace = TRUE, prob = prov_probs),
    geotype = sample(1:3, n_hh, replace = TRUE, prob = geo_probs),
    house_wgt = runif(n_hh, 200, 5000),
    hholdsz = pmax(1, round(rnorm(n_hh, 3.2, 1.8))),
    totmhinc = pmax(0, rnorm(n_hh, 8500, 12000))
  )
  
  # Add province and geotype labels
  ghs_hh$prov_name <- factor(ghs_hh$prov, levels = 1:9, labels = prov_names)
  ghs_hh$geo_name <- factor(ghs_hh$geotype, levels = 1:3, 
                            labels = c("Urban", "Traditional", "Farms"))
  
} else {
  # Load actual GHS data
  ghs_hh <- haven::read_dta(data_path)
  
  # Standardize variable names (adjust based on actual variable names)
  # The GHS 2024 uses: psu, Stratum, prov, GeoType, house_wgt
  names(ghs_hh) <- tolower(names(ghs_hh))
  
  # Create province labels
  prov_names <- c("Western Cape", "Eastern Cape", "Northern Cape", 
                  "Free State", "KwaZulu-Natal", "North West",
                  "Gauteng", "Mpumalanga", "Limpopo")
  ghs_hh$prov_name <- factor(ghs_hh$prov, levels = 1:9, labels = prov_names)
  
  # Create geotype labels
  ghs_hh$geo_name <- factor(ghs_hh$geotype, levels = 1:3, 
                            labels = c("Urban", "Traditional", "Farms"))
}

# Display data structure
cat("\nData Structure:\n")
cat("  Observations:", nrow(ghs_hh), "\n")
cat("  Variables:", ncol(ghs_hh), "\n")
cat("  PSUs:", length(unique(ghs_hh$psu)), "\n")
cat("  Strata:", length(unique(ghs_hh$stratum)), "\n")

#-------------------------------------------------------------------------------
# SECTION 3: SIMULATE REALISTIC NON-RESPONSE PATTERNS
#-------------------------------------------------------------------------------

cat("\n\nSECTION 3: Simulating Non-Response Patterns\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# EXPLANATION OF NON-RESPONSE SIMULATION:
# We create realistic non-response that correlates with observable characteristics:
#
# 1. URBAN AREAS: Higher refusal rates (privacy concerns, busy schedules)
# 2. FARMS: Higher non-contact rates (dispersed, difficult access)
# 3. SMALL HOUSEHOLDS: Higher non-contact (fewer people home)
# 4. CERTAIN PROVINCES: Access challenges (e.g., Northern Cape - vast, sparse)
#
# This creates non-response that is NOT Missing Completely at Random (MCAR)
# but rather Missing at Random (MAR) conditional on observables.

# Base probabilities for response status
# 1 = Complete Interview, 2 = Refusal, 3 = Non-Contact

# Create propensity scores for non-response
ghs_hh <- ghs_hh %>%
  mutate(
    # Urban effect: higher refusal probability
    urban_effect = ifelse(geotype == 1, 0.08, 0),
    

    # Farm effect: higher non-contact probability  
    farm_effect = ifelse(geotype == 3, 0.12, 0),
    
    # Small household effect: harder to contact
    small_hh_effect = ifelse(hholdsz <= 2, 0.06, 0),
    
    # Province effects (based on realistic field challenges)
    # Northern Cape: vast distances, low density
    # Eastern Cape: rural access challenges
    # Limpopo: traditional areas with access issues
    prov_effect = case_when(
      prov == 3 ~ 0.05,  # Northern Cape
      prov == 2 ~ 0.04,  # Eastern Cape
      prov == 9 ~ 0.03,  # Limpopo
      prov == 8 ~ 0.02,  # Mpumalanga
      TRUE ~ 0
    ),
    
    # Combined non-response propensity
    nr_propensity = urban_effect + farm_effect + small_hh_effect + prov_effect,
    
    # Add random component
    random_component = runif(n())
  )

# Assign response status based on propensity
# Target overall response rate: approximately 85% (realistic for SA surveys)
ghs_hh <- ghs_hh %>%
  mutate(
    response_status = case_when(
      # Complete interview (base ~85%, reduced by propensity)
      random_component > (0.15 + nr_propensity) ~ 1L,
      # Refusal (more common in urban areas)
      random_component > (0.08 + nr_propensity * 0.3) & geotype == 1 ~ 2L,
      random_component > (0.05 + nr_propensity * 0.3) ~ 2L,
      # Non-contact (remainder)
      TRUE ~ 3L
    ),
    
    # Create labeled factor
    response_label = factor(response_status, 
                            levels = 1:3,
                            labels = c("Complete", "Refusal", "Non-Contact"))
  )

# Display response status distribution
cat("\nSimulated Response Status Distribution:\n")
response_dist <- table(ghs_hh$response_label)
response_pct <- prop.table(response_dist) * 100

for (i in seq_along(response_dist)) {
  cat(sprintf("  %s: %s (%.1f%%)\n", 
              names(response_dist)[i], 
              format(response_dist[i], big.mark = ","),
              response_pct[i]))
}

#-------------------------------------------------------------------------------
# SECTION 4: CALCULATE AAPOR RESPONSE RATE 1 (RR1)
#-------------------------------------------------------------------------------

cat("\n\nSECTION 4: AAPOR Response Rate Calculations\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# AAPOR RESPONSE RATE 1 (RR1) DEFINITION:
# RR1 = I / (I + P + R + NC + O + UH + UO)
#
# Where:
#   I  = Complete interviews
#   P  = Partial interviews (treating as 0 in this simulation)
#   R  = Refusals and break-offs
#   NC = Non-contacts
#   O  = Other non-response
#   UH = Unknown eligibility - household
#   UO = Unknown eligibility - other
#
# Simplified for this exercise:
# RR1 = Complete / (Complete + Refusal + Non-Contact)
#
# Reference: AAPOR Standard Definitions, 10th Edition (2023)

# Function to calculate RR1
calc_rr1 <- function(data) {
  counts <- table(data$response_status)
  complete <- ifelse("1" %in% names(counts), counts["1"], 0)
  total <- sum(counts)
  rr1 <- (complete / total) * 100
  return(rr1)
}

# Overall RR1
overall_rr1 <- calc_rr1(ghs_hh)
cat(sprintf("\nOverall AAPOR RR1: %.1f%%\n", overall_rr1))

#-------------------------------------------------------------------------------
# SECTION 5: RESPONSE RATES BY PROVINCE
#-------------------------------------------------------------------------------

cat("\n\nSECTION 5: Response Rates by Province\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Calculate response rates by province
rr_by_province <- ghs_hh %>%
  group_by(prov, prov_name) %>%
  summarise(
    n_total = n(),
    n_complete = sum(response_status == 1),
    n_refusal = sum(response_status == 2),
    n_noncontact = sum(response_status == 3),
    rr1 = (n_complete / n_total) * 100,
    refusal_rate = (n_refusal / n_total) * 100,
    noncontact_rate = (n_noncontact / n_total) * 100,
    .groups = "drop"
  ) %>%
  arrange(rr1)

# Display results
cat("\nResponse Rates by Province (sorted by RR1):\n\n")
cat(sprintf("%-15s %8s %8s %8s %8s %8s\n", 
            "Province", "Total", "Complete", "RR1%", "Refusal%", "NC%"))
cat(paste(rep("-", 60), collapse = ""), "\n")

for (i in 1:nrow(rr_by_province)) {
  cat(sprintf("%-15s %8d %8d %8.1f %8.1f %8.1f\n",
              as.character(rr_by_province$prov_name[i]),
              rr_by_province$n_total[i],
              rr_by_province$n_complete[i],
              rr_by_province$rr1[i],
              rr_by_province$refusal_rate[i],
              rr_by_province$noncontact_rate[i]))
}

# Identify provinces with lowest response rates
cat("\n⚠️  ALERT: Provinces with RR1 < 82%:\n")
low_rr_provinces <- rr_by_province %>% filter(rr1 < 82)
if (nrow(low_rr_provinces) > 0) {
  for (i in 1:nrow(low_rr_provinces)) {
    cat(sprintf("   - %s: %.1f%%\n", 
                low_rr_provinces$prov_name[i], 
                low_rr_provinces$rr1[i]))
  }
} else {
  cat("   None identified.\n")
}

#-------------------------------------------------------------------------------
# SECTION 6: RESPONSE RATES BY GEOTYPE
#-------------------------------------------------------------------------------

cat("\n\nSECTION 6: Response Rates by Geography Type\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Calculate response rates by geotype
rr_by_geotype <- ghs_hh %>%
  group_by(geotype, geo_name) %>%
  summarise(
    n_total = n(),
    n_complete = sum(response_status == 1),
    n_refusal = sum(response_status == 2),
    n_noncontact = sum(response_status == 3),
    rr1 = (n_complete / n_total) * 100,
    refusal_rate = (n_refusal / n_total) * 100,
    noncontact_rate = (n_noncontact / n_total) * 100,
    .groups = "drop"
  )

cat("\nResponse Rates by Geography Type:\n\n")
cat(sprintf("%-12s %8s %8s %8s %8s %8s\n", 
            "Geotype", "Total", "Complete", "RR1%", "Refusal%", "NC%"))
cat(paste(rep("-", 55), collapse = ""), "\n")

for (i in 1:nrow(rr_by_geotype)) {
  cat(sprintf("%-12s %8d %8d %8.1f %8.1f %8.1f\n",
              as.character(rr_by_geotype$geo_name[i]),
              rr_by_geotype$n_total[i],
              rr_by_geotype$n_complete[i],
              rr_by_geotype$rr1[i],
              rr_by_geotype$refusal_rate[i],
              rr_by_geotype$noncontact_rate[i]))
}

# Key insight
cat("\n📊 KEY INSIGHT:\n")
urban_rr <- rr_by_geotype$rr1[rr_by_geotype$geotype == 1]
trad_rr <- rr_by_geotype$rr1[rr_by_geotype$geotype == 2]
farm_rr <- rr_by_geotype$rr1[rr_by_geotype$geotype == 3]

cat(sprintf("   Urban refusal rate (%.1f%%) is higher than Traditional (%.1f%%)\n",
            rr_by_geotype$refusal_rate[rr_by_geotype$geotype == 1],
            rr_by_geotype$refusal_rate[rr_by_geotype$geotype == 2]))
cat(sprintf("   Farm non-contact rate (%.1f%%) is highest\n",
            rr_by_geotype$noncontact_rate[rr_by_geotype$geotype == 3]))

#-------------------------------------------------------------------------------
# SECTION 7: CROSS-TABULATION (PROVINCE × GEOTYPE)
#-------------------------------------------------------------------------------

cat("\n\nSECTION 7: Response Rates by Province × Geotype\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Calculate RR1 by Province and Geotype
rr_cross <- ghs_hh %>%
  group_by(prov_name, geo_name) %>%
  summarise(
    n = n(),
    rr1 = (sum(response_status == 1) / n()) * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = geo_name, values_from = c(n, rr1))

cat("\nRR1 (%) by Province and Geotype:\n\n")

# Create cross-tab of RR1
rr1_matrix <- ghs_hh %>%
  group_by(prov_name, geo_name) %>%
  summarise(rr1 = (sum(response_status == 1) / n()) * 100, .groups = "drop") %>%
  pivot_wider(names_from = geo_name, values_from = rr1)

print(rr1_matrix, n = 10)

# Identify problem cells (RR1 < 80%)
cat("\n⚠️  CELLS WITH RR1 < 80%:\n")
problem_cells <- ghs_hh %>%
  group_by(prov_name, geo_name) %>%
  summarise(
    n = n(),
    rr1 = (sum(response_status == 1) / n()) * 100,
    .groups = "drop"
  ) %>%
  filter(rr1 < 80, n >= 50)  # Only cells with sufficient sample

if (nrow(problem_cells) > 0) {
  for (i in 1:nrow(problem_cells)) {
    cat(sprintf("   - %s × %s: %.1f%% (n=%d)\n",
                problem_cells$prov_name[i],
                problem_cells$geo_name[i],
                problem_cells$rr1[i],
                problem_cells$n[i]))
  }
} else {
  cat("   None identified with n >= 50.\n")
}

#-------------------------------------------------------------------------------
# SECTION 8: RESPONSE BIAS ANALYSIS
#-------------------------------------------------------------------------------

cat("\n\nSECTION 8: Response Bias Analysis\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# RESPONSE BIAS CONCEPT:
# If respondents differ systematically from non-respondents on key variables,
# survey estimates will be biased. We compare:
#   - Mean household size of respondents vs. non-respondents
#
# Household size is available for all sampled units (from frame or observation)
# and serves as a proxy to detect potential bias in other variables.

# Separate respondents and non-respondents
respondents <- ghs_hh %>% filter(response_status == 1)
nonrespondents <- ghs_hh %>% filter(response_status != 1)

# Calculate mean household size
mean_hh_resp <- mean(respondents$hholdsz, na.rm = TRUE)
mean_hh_nonresp <- mean(nonrespondents$hholdsz, na.rm = TRUE)
mean_hh_all <- mean(ghs_hh$hholdsz, na.rm = TRUE)

# Standard errors
se_resp <- sd(respondents$hholdsz, na.rm = TRUE) / sqrt(nrow(respondents))
se_nonresp <- sd(nonrespondents$hholdsz, na.rm = TRUE) / sqrt(nrow(nonrespondents))

# T-test for difference
t_test <- t.test(respondents$hholdsz, nonrespondents$hholdsz)

cat("\nHousehold Size Comparison:\n\n")
cat(sprintf("\nRespondents (n=%s):\n", format(nrow(respondents), big.mark = ",")))
cat(sprintf("    Mean household size: %.3f (SE: %.4f)\n", mean_hh_resp, se_resp))

cat(sprintf("\nNon-Respondents (n=%s):\n", format(nrow(nonrespondents), big.mark = ",")))
cat(sprintf("    Mean household size: %.3f (SE: %.4f)\n", mean_hh_nonresp, se_nonresp))

cat(sprintf("\n  Difference: %.3f\n", mean_hh_resp - mean_hh_nonresp))
cat(sprintf("  t-statistic: %.3f\n", t_test$statistic))
cat(sprintf("  p-value: %.4f\n", t_test$p.value))

# Interpretation
cat("\n📊 INTERPRETATION:\n")
if (t_test$p.value < 0.05) {
  cat("   ⚠️  SIGNIFICANT DIFFERENCE DETECTED\n")
  cat("   Respondents and non-respondents differ significantly on household size.\n")
  if (mean_hh_resp > mean_hh_nonresp) {
    cat("   Non-respondents tend to be SMALLER households.\n")
    cat("   This suggests potential UNDERCOVERAGE of small households.\n")
  } else {
    cat("   Non-respondents tend to be LARGER households.\n")
    cat("   This suggests potential UNDERCOVERAGE of large households.\n")
  }
} else {
  cat("   ✓ No significant difference detected (p > 0.05)\n")
}

# Non-response bias formula
# Bias = (1 - RR) × (Y_R - Y_NR)
rr <- nrow(respondents) / nrow(ghs_hh)
bias_estimate <- (1 - rr) * (mean_hh_resp - mean_hh_nonresp)

cat(sprintf("\n  Estimated Non-Response Bias in Mean HH Size: %.4f\n", bias_estimate))
cat(sprintf("  Relative Bias: %.2f%%\n", (bias_estimate / mean_hh_all) * 100))

#-------------------------------------------------------------------------------
# SECTION 9: VISUALIZATION - RESPONSE BIAS
#-------------------------------------------------------------------------------

cat("\n\nSECTION 9: Creating Visualizations\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Plot 1: Household Size Distribution by Response Status
p1 <- ggplot(ghs_hh, aes(x = hholdsz, fill = response_label)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("Complete" = "#2E7D32", 
                                "Refusal" = "#D32F2F", 
                                "Non-Contact" = "#FFA000"),
                    name = "Response Status") +
  labs(title = "Household Size Distribution by Response Status",
       subtitle = "Lab 4.1: Response Bias Visualization",
       x = "Household Size (persons)",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0, 15, 2))

# Plot 2: Response Rate by Province
p2 <- ggplot(rr_by_province, aes(x = reorder(prov_name, rr1), y = rr1)) +
  geom_bar(stat = "identity", fill = "#1565C0", alpha = 0.8) +
  geom_hline(yintercept = 85, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f%%", rr1)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "AAPOR RR1 by Province",
       subtitle = "Dashed line = 85% target response rate",
       x = "",
       y = "Response Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, 100))

# Plot 3: Stacked bar of response outcomes by geotype
response_by_geo <- ghs_hh %>%
  group_by(geo_name, response_label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(geo_name) %>%
  mutate(pct = n / sum(n) * 100)

p3 <- ggplot(response_by_geo, aes(x = geo_name, y = pct, fill = response_label)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Complete" = "#2E7D32", 
                                "Refusal" = "#D32F2F", 
                                "Non-Contact" = "#FFA000"),
                    name = "Status") +
  labs(title = "Response Outcomes by Geography Type",
       x = "",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

# Plot 4: Mean household size comparison
bias_data <- data.frame(
  Group = c("Respondents", "Non-Respondents", "Total Sample"),
  Mean = c(mean_hh_resp, mean_hh_nonresp, mean_hh_all),
  SE = c(se_resp, se_nonresp, sd(ghs_hh$hholdsz) / sqrt(nrow(ghs_hh)))
)

p4 <- ggplot(bias_data, aes(x = Group, y = Mean)) +
  geom_bar(stat = "identity", fill = c("#1565C0", "#D32F2F", "#757575"), 
           alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE, ymax = Mean + 1.96*SE), 
                width = 0.2, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Mean)), vjust = -0.5, size = 4) +
  labs(title = "Mean Household Size: Respondents vs Non-Respondents",
       subtitle = "Error bars show 95% confidence intervals",
       x = "",
       y = "Mean Household Size") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, max(bias_data$Mean) * 1.3))

# Save plots
ggsave("lab4_1_response_bias_histogram.png", p1, width = 10, height = 6, dpi = 150)
ggsave("lab4_1_rr1_by_province.png", p2, width = 8, height = 6, dpi = 150)
ggsave("lab4_1_response_by_geotype.png", p3, width = 8, height = 6, dpi = 150)
ggsave("lab4_1_bias_comparison.png", p4, width = 8, height = 6, dpi = 150)

cat("\nPlots saved:\n")
cat("  - lab4_1_response_bias_histogram.png\n")
cat("  - lab4_1_rr1_by_province.png\n")
cat("  - lab4_1_response_by_geotype.png\n")
cat("  - lab4_1_bias_comparison.png\n")

#-------------------------------------------------------------------------------
# SECTION 10: SUMMARY OUTPUT TABLE FOR SLIDES
#-------------------------------------------------------------------------------

cat("\n\nSECTION 10: Summary Tables for Presentation\n")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Create summary table suitable for slides
summary_table <- data.frame(
  Domain = c("NATIONAL", rr_by_province$prov_name),
  N_Total = c(nrow(ghs_hh), rr_by_province$n_total),
  N_Complete = c(sum(ghs_hh$response_status == 1), rr_by_province$n_complete),
  RR1 = c(overall_rr1, rr_by_province$rr1),
  Refusal_Pct = c(sum(ghs_hh$response_status == 2)/nrow(ghs_hh)*100, 
                  rr_by_province$refusal_rate),
  NonContact_Pct = c(sum(ghs_hh$response_status == 3)/nrow(ghs_hh)*100,
                     rr_by_province$noncontact_rate)
)

cat("\n=== TABLE FOR SLIDES: Response Rates Summary ===\n\n")
print(summary_table, row.names = FALSE)

# Export for use in slides
write.csv(summary_table, "lab4_1_response_rates_summary.csv", row.names = FALSE)
cat("\nTable exported to: lab4_1_response_rates_summary.csv\n")

#-------------------------------------------------------------------------------
# SECTION 11: KEY FINDINGS FOR ZAMBARA NARRATIVE
#-------------------------------------------------------------------------------

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("KEY FINDINGS FOR LINDIWE'S REPORT\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

cat("\n📋 ZAMBARA HOUSEHOLD LIVING CONDITIONS SURVEY - RESPONSE ANALYSIS\n\n")

cat("1. OVERALL RESPONSE RATE:\n")
cat(sprintf("   AAPOR RR1 = %.1f%%\n", overall_rr1))
status <- ifelse(overall_rr1 >= 85, "ACCEPTABLE", 
                 ifelse(overall_rr1 >= 80, "MARGINAL", "CONCERNING"))
cat(sprintf("   Status: %s\n\n", status))

cat("2. GEOGRAPHIC PATTERNS:\n")
lowest_prov <- rr_by_province$prov_name[1]
lowest_rr <- rr_by_province$rr1[1]
cat(sprintf("   Lowest response: %s (%.1f%%)\n", lowest_prov, lowest_rr))
cat(sprintf("   Highest refusal: Urban areas (%.1f%%)\n", 
            rr_by_geotype$refusal_rate[rr_by_geotype$geotype == 1]))
cat(sprintf("   Highest non-contact: Farms (%.1f%%)\n\n",
            rr_by_geotype$noncontact_rate[rr_by_geotype$geotype == 3]))

cat("3. RESPONSE BIAS EVIDENCE:\n")
cat(sprintf("   Mean HH size (respondents): %.2f\n", mean_hh_resp))
cat(sprintf("   Mean HH size (non-respondents): %.2f\n", mean_hh_nonresp))
bias_sig <- ifelse(t_test$p.value < 0.05, "SIGNIFICANT", "Not significant")
cat(sprintf("   Difference: %s (p = %.4f)\n\n", bias_sig, t_test$p.value))

cat("4. RECOMMENDED ACTIONS:\n")
cat("   a) Increase callback attempts in Farm areas\n")
cat("   b) Deploy refusal conversion protocols in Urban areas\n")
cat("   c) Consider non-response weight adjustments\n")
cat("   d) Document bias direction for reporting\n")

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("END OF LAB 4.1\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
