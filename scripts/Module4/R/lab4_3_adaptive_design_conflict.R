#===============================================================================
# LAB 4.3: ADAPTIVE DESIGN IN CONFLICT ZONES
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 4, Module 4.2: Error Mitigation and Quality Assurance
#===============================================================================
#
# OBJECTIVE: Compare three strategies for handling inaccessible PSUs
#   Strategy A: Drop inaccessible PSUs (complete case analysis)
#   Strategy B: Re-weight remaining PSUs within affected strata
#   Strategy C: Substitute with "safe" neighbor PSUs
#
# DATA: GHS 2024 Household File
# CONFLICT ZONE: Eastern Highlands (KwaZulu-Natal, Province 5)
#                50% of PSUs made inaccessible
#
# ZAMBARA NARRATIVE: Eastern Forests region faces security crisis
#
#===============================================================================

#-------------------------------------------------------------------------------
# 0. SETUP AND PACKAGES
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Required packages
required_packages <- c("haven", "dplyr", "tidyr", "survey", "ggplot2", 
                       "knitr", "kableExtra")

# Install if missing
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set seed for reproducibility
set.seed(20260305)

# CRITICAL: Handle singleton PSUs (strata with only one PSU)
# This occurs when dropping PSUs leaves some strata with only one PSU
# Options: "certainty" (contribute 0 variance), "adjust", "average", "remove"
options(survey.lonely.psu = "certainty")

cat("\n")
cat("================================================================\n")
cat("  LAB 4.3: ADAPTIVE DESIGN IN CONFLICT ZONES\n")
cat("  SADC Advanced Sampling Workshop - Day 4\n")
cat("================================================================\n\n")

#-------------------------------------------------------------------------------
# 1. LOAD AND PREPARE DATA
#-------------------------------------------------------------------------------

cat("SECTION 1: Loading and Preparing Data\n")
cat("-------------------------------------\n\n")

# Load GHS 2024 household data
# NOTE: Adjust path as needed for your system
ghs_hh <- haven::read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")

cat("Dataset loaded:", nrow(ghs_hh), "households\n\n")

# Key variables for analysis:
# - uqnr: Unique household identifier
# - psu: Primary Sampling Unit
# - stratum: Sampling stratum (first digit = province)
# - prov: Province (1-9)
# - geotype: Geography type (1=Urban, 2=Traditional, 3=Farms)
# - house_wgt: Household weight
# - totmhinc: Total monthly household income
# - hholdsz: Household size

# Create province variable from stratum if prov doesn't exist
if (!"prov" %in% names(ghs_hh)) {
  ghs_hh <- ghs_hh %>%
    mutate(prov = as.numeric(substr(as.character(stratum), 1, 1)))
}

# Province labels (South African provinces)
prov_labels <- c(
  "1" = "Western Cape",
  "2" = "Eastern Cape", 
  "3" = "Northern Cape",
  "4" = "Free State",
  "5" = "KwaZulu-Natal",    # <- CONFLICT ZONE (Eastern Highlands in Zambara)
  "6" = "North West",
  "7" = "Gauteng",
  "8" = "Mpumalanga",
  "9" = "Limpopo"
)

ghs_hh <- ghs_hh %>%
  mutate(prov_name = prov_labels[as.character(prov)])

# Check data structure
cat("Province Distribution:\n")
print(table(ghs_hh$prov_name, useNA = "ifany"))
cat("\n")

# Check PSU counts by province
psu_by_prov <- ghs_hh %>%
  group_by(prov, prov_name) %>%
  summarise(
    n_hh = n(),
    n_psu = n_distinct(psu),
    .groups = "drop"
  )

cat("PSU Counts by Province:\n")
print(as.data.frame(psu_by_prov))
cat("\n")

#-------------------------------------------------------------------------------
# 2. SIMULATE CONFLICT ZONE: MAKE 50% OF KZN PSUs INACCESSIBLE
#-------------------------------------------------------------------------------

cat("SECTION 2: Simulating Conflict Zone\n")
cat("------------------------------------\n\n")

# Identify KwaZulu-Natal (Province 5) as conflict zone
CONFLICT_PROV <- 5
CONFLICT_NAME <- "KwaZulu-Natal (Eastern Highlands)"

cat("Conflict Zone Province:", CONFLICT_NAME, "\n\n")

# Get list of unique PSUs in KZN
kzn_psus <- ghs_hh %>%
  filter(prov == CONFLICT_PROV) %>%
  distinct(psu) %>%
  pull(psu)

n_kzn_psus <- length(kzn_psus)
cat("Total PSUs in conflict zone:", n_kzn_psus, "\n")

# Randomly select 50% of PSUs to be "inaccessible"
n_inaccessible <- round(n_kzn_psus * 0.50)
inaccessible_psus <- sample(kzn_psus, n_inaccessible)

cat("PSUs made inaccessible:", n_inaccessible, "(50%)\n")
cat("PSUs remaining accessible:", n_kzn_psus - n_inaccessible, "\n\n")

# Create accessibility flag
ghs_hh <- ghs_hh %>%
  mutate(
    accessible = case_when(
      prov != CONFLICT_PROV ~ TRUE,                    # All other provinces accessible
      psu %in% inaccessible_psus ~ FALSE,              # Inaccessible PSUs in KZN
      TRUE ~ TRUE                                       # Accessible PSUs in KZN
    )
  )

# Summary of accessibility
access_summary <- ghs_hh %>%
  group_by(prov_name, accessible) %>%
  summarise(
    n_hh = n(),
    n_psu = n_distinct(psu),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = accessible, values_from = c(n_hh, n_psu), 
              values_fill = 0)

cat("Accessibility Summary:\n")
print(as.data.frame(access_summary))
cat("\n")

# Households affected
n_hh_inaccessible <- sum(!ghs_hh$accessible)
pct_inaccessible <- round(100 * n_hh_inaccessible / nrow(ghs_hh), 1)
cat("Total households in inaccessible PSUs:", n_hh_inaccessible, 
    "(", pct_inaccessible, "% of sample)\n\n")

#-------------------------------------------------------------------------------
# 3. CALCULATE "TRUE" POPULATION PARAMETERS (Full Sample Benchmark)
#-------------------------------------------------------------------------------

cat("SECTION 3: Establishing True Population Parameters\n")
cat("--------------------------------------------------\n\n")

# Define survey design for FULL sample (our "truth")
des_full <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_hh,
  nest = TRUE
)

# Calculate "true" estimates from full sample
# Key indicators:
# 1. Mean household income (national)
# 2. Mean household income (KZN only)
# 3. Mean household size (national)
# 4. Proportion with electricity access

# Handle missing income
ghs_hh$income_valid <- !is.na(ghs_hh$totmhinc) & ghs_hh$totmhinc >= 0

# True estimates
true_income_national <- svymean(~totmhinc, design = des_full, na.rm = TRUE)
true_income_kzn <- svymean(~totmhinc, 
                           design = subset(des_full, prov == CONFLICT_PROV), 
                           na.rm = TRUE)
true_hhsize_national <- svymean(~hholdsz, design = des_full, na.rm = TRUE)

cat("TRUE POPULATION PARAMETERS (Full Sample):\n")
cat("-----------------------------------------\n")
cat("Mean HH Income (National):   R", format(coef(true_income_national), 
                                             big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Income (KZN):        R", format(coef(true_income_kzn), 
                                             big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Size (National):    ", round(coef(true_hhsize_national), 2), "\n\n")

# Store true values for comparison
TRUE_VALUES <- list(
  income_national = coef(true_income_national),
  income_kzn = coef(true_income_kzn),
  hhsize_national = coef(true_hhsize_national)
)

#-------------------------------------------------------------------------------
# 4. STRATEGY A: DROP INACCESSIBLE PSUs
#-------------------------------------------------------------------------------

cat("SECTION 4: STRATEGY A - Drop Inaccessible PSUs\n")
cat("----------------------------------------------\n\n")

# Create dataset with only accessible PSUs
ghs_strategyA <- ghs_hh %>%
  filter(accessible == TRUE)

cat("Strategy A Sample Size:", nrow(ghs_strategyA), "households\n")
cat("Households dropped:", nrow(ghs_hh) - nrow(ghs_strategyA), "\n\n")

# Check for singleton strata (strata with only one PSU after dropping)
singleton_check <- ghs_strategyA %>%
  group_by(stratum) %>%
  summarise(n_psu = n_distinct(psu), .groups = "drop") %>%
  filter(n_psu == 1)

if (nrow(singleton_check) > 0) {
  cat("WARNING: ", nrow(singleton_check), " strata have only 1 PSU after dropping.\n")
  cat("         Using survey.lonely.psu = 'certainty' to handle.\n\n")
}

# Define survey design for Strategy A (original weights, reduced sample)
des_strategyA <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_strategyA,
  nest = TRUE
)

# Calculate estimates
est_A_income_national <- svymean(~totmhinc, design = des_strategyA, na.rm = TRUE)
est_A_income_kzn <- svymean(~totmhinc, 
                            design = subset(des_strategyA, prov == CONFLICT_PROV),
                            na.rm = TRUE)
est_A_hhsize_national <- svymean(~hholdsz, design = des_strategyA, na.rm = TRUE)

# Calculate bias
bias_A_income_nat <- coef(est_A_income_national) - TRUE_VALUES$income_national
bias_A_income_kzn <- coef(est_A_income_kzn) - TRUE_VALUES$income_kzn
bias_A_hhsize <- coef(est_A_hhsize_national) - TRUE_VALUES$hhsize_national

cat("STRATEGY A ESTIMATES:\n")
cat("Mean HH Income (National):  R", format(coef(est_A_income_national), 
                                            big.mark = ",", nsmall = 0),
    " | Bias: R", format(bias_A_income_nat, big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Income (KZN):       R", format(coef(est_A_income_kzn), 
                                            big.mark = ",", nsmall = 0),
    " | Bias: R", format(bias_A_income_kzn, big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Size (National):   ", round(coef(est_A_hhsize_national), 2),
    " | Bias:", round(bias_A_hhsize, 3), "\n\n")

# Relative bias
relbias_A_income <- 100 * bias_A_income_nat / TRUE_VALUES$income_national
cat("Relative Bias (Income):", round(relbias_A_income, 2), "%\n\n")

#-------------------------------------------------------------------------------
# 5. STRATEGY B: RE-WEIGHT REMAINING PSUs IN AFFECTED STRATA
#-------------------------------------------------------------------------------

cat("SECTION 5: STRATEGY B - Re-weight Remaining PSUs\n")
cat("-------------------------------------------------\n\n")

# Calculate re-weighting factors within each stratum
# For strata with inaccessible PSUs: inflate weights by (n_total / n_accessible)

# First, calculate adjustment factors by stratum
weight_adjustment <- ghs_hh %>%
  group_by(stratum) %>%
  summarise(
    n_psu_total = n_distinct(psu),
    n_psu_accessible = n_distinct(psu[accessible]),
    n_hh_total = n(),
    n_hh_accessible = sum(accessible),
    sum_wgt_total = sum(house_wgt),
    sum_wgt_accessible = sum(house_wgt[accessible]),
    .groups = "drop"
  ) %>%
  mutate(
    # Adjustment factor: ratio of total to accessible weight sum
    wgt_factor = sum_wgt_total / sum_wgt_accessible,
    # Flag strata with inaccessible PSUs
    has_inaccessible = n_psu_total > n_psu_accessible
  )

cat("Strata with Inaccessible PSUs:\n")
affected_strata <- weight_adjustment %>%
  filter(has_inaccessible) %>%
  select(stratum, n_psu_total, n_psu_accessible, wgt_factor)
print(as.data.frame(affected_strata))
cat("\n")

# Create Strategy B dataset with adjusted weights
ghs_strategyB <- ghs_hh %>%
  filter(accessible == TRUE) %>%
  left_join(weight_adjustment %>% select(stratum, wgt_factor), by = "stratum") %>%
  mutate(
    house_wgt_adj = house_wgt * wgt_factor
  )

cat("Strategy B: Weights adjusted for", nrow(affected_strata), "strata\n")
cat("Original weight sum:", format(sum(ghs_hh$house_wgt), big.mark = ","), "\n")
cat("Adjusted weight sum:", format(sum(ghs_strategyB$house_wgt_adj), big.mark = ","), "\n\n")

# Define survey design for Strategy B (adjusted weights)
des_strategyB <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt_adj,
  data = ghs_strategyB,
  nest = TRUE
)

# Calculate estimates
est_B_income_national <- svymean(~totmhinc, design = des_strategyB, na.rm = TRUE)
est_B_income_kzn <- svymean(~totmhinc, 
                            design = subset(des_strategyB, prov == CONFLICT_PROV),
                            na.rm = TRUE)
est_B_hhsize_national <- svymean(~hholdsz, design = des_strategyB, na.rm = TRUE)

# Calculate bias
bias_B_income_nat <- coef(est_B_income_national) - TRUE_VALUES$income_national
bias_B_income_kzn <- coef(est_B_income_kzn) - TRUE_VALUES$income_kzn
bias_B_hhsize <- coef(est_B_hhsize_national) - TRUE_VALUES$hhsize_national

cat("STRATEGY B ESTIMATES:\n")
cat("Mean HH Income (National):  R", format(coef(est_B_income_national), 
                                            big.mark = ",", nsmall = 0),
    " | Bias: R", format(bias_B_income_nat, big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Income (KZN):       R", format(coef(est_B_income_kzn), 
                                            big.mark = ",", nsmall = 0),
    " | Bias: R", format(bias_B_income_kzn, big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Size (National):   ", round(coef(est_B_hhsize_national), 2),
    " | Bias:", round(bias_B_hhsize, 3), "\n\n")

# Relative bias
relbias_B_income <- 100 * bias_B_income_nat / TRUE_VALUES$income_national
cat("Relative Bias (Income):", round(relbias_B_income, 2), "%\n\n")

#-------------------------------------------------------------------------------
# 6. STRATEGY C: SUBSTITUTE WITH "SAFE" NEIGHBOR PSUs
#-------------------------------------------------------------------------------

cat("SECTION 6: STRATEGY C - Substitute with Neighbor PSUs\n")
cat("------------------------------------------------------\n\n")

# Substitution approach:
# For each inaccessible PSU, find a "similar" accessible PSU in same stratum
# and duplicate its data (simulating a substitute PSU)

# Get characteristics of inaccessible PSUs
inaccessible_psu_data <- ghs_hh %>%
  filter(!accessible) %>%
  group_by(psu, stratum) %>%
  summarise(
    n_hh = n(),
    mean_hhsize = mean(hholdsz, na.rm = TRUE),
    .groups = "drop"
  )

cat("Inaccessible PSUs to substitute:", nrow(inaccessible_psu_data), "\n\n")

# For each inaccessible PSU, find a substitute from same stratum
# (In practice, would use geographic proximity; here we use random within stratum)

substitute_assignments <- data.frame()

for (i in 1:nrow(inaccessible_psu_data)) {
  inac_psu <- inaccessible_psu_data$psu[i]
  inac_stratum <- inaccessible_psu_data$stratum[i]
  
  # Find accessible PSUs in same stratum
  potential_subs <- ghs_hh %>%
    filter(stratum == inac_stratum, accessible == TRUE) %>%
    distinct(psu) %>%
    pull(psu)
  
  if (length(potential_subs) > 0) {
    # Randomly select one substitute
    sub_psu <- sample(potential_subs, 1)
    
    substitute_assignments <- rbind(substitute_assignments,
                                    data.frame(
                                      inaccessible_psu = inac_psu,
                                      substitute_psu = sub_psu,
                                      stratum = inac_stratum
                                    ))
  }
}

cat("Substitution Assignments (first 10):\n")
print(head(substitute_assignments, 10))
cat("\n")

# Create Strategy C dataset
# Start with accessible PSUs
ghs_strategyC <- ghs_hh %>%
  filter(accessible == TRUE) %>%
  mutate(is_substitute = FALSE, original_psu = psu)

# Add substitute records for each inaccessible PSU
for (i in 1:nrow(substitute_assignments)) {
  sub_psu <- substitute_assignments$substitute_psu[i]
  inac_psu <- substitute_assignments$inaccessible_psu[i]
  
  # Get data from substitute PSU and mark as substitute
  sub_data <- ghs_hh %>%
    filter(psu == sub_psu) %>%
    mutate(
      is_substitute = TRUE,
      original_psu = psu,
      psu = inac_psu  # Assign to inaccessible PSU's position
    )
  
  ghs_strategyC <- bind_rows(ghs_strategyC, sub_data)
}

cat("Strategy C Sample Size:", nrow(ghs_strategyC), "households\n")
cat("Original accessible:", sum(!ghs_strategyC$is_substitute), "\n")
cat("Substituted:", sum(ghs_strategyC$is_substitute), "\n\n")

# Flag potential bias: substitutes may differ systematically
# Compare characteristics of original inaccessible vs substitutes
sub_comparison <- ghs_hh %>%
  filter(psu %in% inaccessible_psus) %>%
  summarise(
    source = "Inaccessible (Original)",
    mean_income = mean(totmhinc, na.rm = TRUE),
    mean_hhsize = mean(hholdsz, na.rm = TRUE),
    n = n()
  ) %>%
  bind_rows(
    ghs_strategyC %>%
      filter(is_substitute) %>%
      summarise(
        source = "Substitutes",
        mean_income = mean(totmhinc, na.rm = TRUE),
        mean_hhsize = mean(hholdsz, na.rm = TRUE),
        n = n()
      )
  )

cat("SUBSTITUTION BIAS FLAG:\n")
cat("Comparing inaccessible PSUs (true values) vs substitutes:\n")
print(as.data.frame(sub_comparison))
cat("\n")

# Define survey design for Strategy C
des_strategyC <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_strategyC,
  nest = TRUE
)

# Calculate estimates
est_C_income_national <- svymean(~totmhinc, design = des_strategyC, na.rm = TRUE)
est_C_income_kzn <- svymean(~totmhinc, 
                            design = subset(des_strategyC, prov == CONFLICT_PROV),
                            na.rm = TRUE)
est_C_hhsize_national <- svymean(~hholdsz, design = des_strategyC, na.rm = TRUE)

# Calculate bias
bias_C_income_nat <- coef(est_C_income_national) - TRUE_VALUES$income_national
bias_C_income_kzn <- coef(est_C_income_kzn) - TRUE_VALUES$income_kzn
bias_C_hhsize <- coef(est_C_hhsize_national) - TRUE_VALUES$hhsize_national

cat("STRATEGY C ESTIMATES:\n")
cat("Mean HH Income (National):  R", format(coef(est_C_income_national), 
                                            big.mark = ",", nsmall = 0),
    " | Bias: R", format(bias_C_income_nat, big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Income (KZN):       R", format(coef(est_C_income_kzn), 
                                            big.mark = ",", nsmall = 0),
    " | Bias: R", format(bias_C_income_kzn, big.mark = ",", nsmall = 0), "\n")
cat("Mean HH Size (National):   ", round(coef(est_C_hhsize_national), 2),
    " | Bias:", round(bias_C_hhsize, 3), "\n\n")

# Relative bias
relbias_C_income <- 100 * bias_C_income_nat / TRUE_VALUES$income_national
cat("Relative Bias (Income):", round(relbias_C_income, 2), "%\n\n")

#-------------------------------------------------------------------------------
# 7. COMPARISON OF ALL STRATEGIES
#-------------------------------------------------------------------------------

cat("================================================================\n")
cat("SECTION 7: COMPARISON OF ALL STRATEGIES\n")
cat("================================================================\n\n")

# Create comparison table
comparison_table <- data.frame(
  Strategy = c("Full Sample (True)", "A: Drop PSUs", "B: Re-weight", "C: Substitute"),
  Income_National = c(
    TRUE_VALUES$income_national,
    coef(est_A_income_national),
    coef(est_B_income_national),
    coef(est_C_income_national)
  ),
  Bias_Income = c(
    0,
    bias_A_income_nat,
    bias_B_income_nat,
    bias_C_income_nat
  ),
  RelBias_Income_Pct = c(
    0,
    relbias_A_income,
    relbias_B_income,
    relbias_C_income
  ),
  HHSize_National = c(
    TRUE_VALUES$hhsize_national,
    coef(est_A_hhsize_national),
    coef(est_B_hhsize_national),
    coef(est_C_hhsize_national)
  ),
  Bias_HHSize = c(
    0,
    bias_A_hhsize,
    bias_B_hhsize,
    bias_C_hhsize
  ),
  Sample_Size = c(
    nrow(ghs_hh),
    nrow(ghs_strategyA),
    nrow(ghs_strategyB),
    nrow(ghs_strategyC)
  )
)

# Format for display
comparison_display <- comparison_table %>%
  mutate(
    Income_National = paste0("R ", format(round(Income_National, 0), big.mark = ",")),
    Bias_Income = paste0("R ", format(round(Bias_Income, 0), big.mark = ",")),
    RelBias_Income_Pct = paste0(round(RelBias_Income_Pct, 2), "%"),
    HHSize_National = round(HHSize_National, 2),
    Bias_HHSize = round(Bias_HHSize, 3),
    Sample_Size = format(Sample_Size, big.mark = ",")
  )

cat("STRATEGY COMPARISON TABLE:\n")
cat("--------------------------\n\n")
print(comparison_display)
cat("\n")

#-------------------------------------------------------------------------------
# 8. VISUALIZATION
#-------------------------------------------------------------------------------

cat("SECTION 8: Creating Visualizations\n")
cat("-----------------------------------\n\n")

# Bar plot of bias comparison
bias_plot_data <- data.frame(
  Strategy = factor(c("A: Drop", "B: Re-weight", "C: Substitute"),
                    levels = c("A: Drop", "B: Re-weight", "C: Substitute")),
  RelBias = c(relbias_A_income, relbias_B_income, relbias_C_income)
)

p_bias <- ggplot(bias_plot_data, aes(x = Strategy, y = RelBias, fill = Strategy)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("#DC3545", "#FFC107", "#28A745")) +
  labs(
    title = "Relative Bias in Mean Household Income by Strategy",
    subtitle = "Conflict Zone: 50% of KwaZulu-Natal PSUs Inaccessible",
    y = "Relative Bias (%)",
    x = "Strategy"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(RelBias, 2), "%")), 
            vjust = ifelse(bias_plot_data$RelBias >= 0, -0.5, 1.5))

# Save plot
ggsave("lab4_3_bias_comparison.png", p_bias, width = 8, height = 6, dpi = 150)
cat("Saved: lab4_3_bias_comparison.png\n")

# Summary statistics plot
summary_plot_data <- data.frame(
  Strategy = factor(rep(c("True", "A: Drop", "B: Re-weight", "C: Substitute"), 2),
                    levels = c("True", "A: Drop", "B: Re-weight", "C: Substitute")),
  Indicator = rep(c("Income (R)", "HH Size"), each = 4),
  Value = c(
    TRUE_VALUES$income_national, coef(est_A_income_national),
    coef(est_B_income_national), coef(est_C_income_national),
    TRUE_VALUES$hhsize_national * 1000, coef(est_A_hhsize_national) * 1000,
    coef(est_B_hhsize_national) * 1000, coef(est_C_hhsize_national) * 1000
  )
)

#-------------------------------------------------------------------------------
# 9. SUMMARY AND RECOMMENDATIONS
#-------------------------------------------------------------------------------

cat("\n")
cat("================================================================\n")
cat("SECTION 9: SUMMARY AND RECOMMENDATIONS\n")
cat("================================================================\n\n")

cat("SCENARIO: 50% of PSUs in KwaZulu-Natal (Eastern Highlands) inaccessible\n\n")

cat("STRATEGY COMPARISON:\n")
cat("--------------------\n\n")

cat("Strategy A (Drop PSUs):\n")
cat("  - Simplest approach\n")
cat("  - Bias:", round(relbias_A_income, 2), "%\n")
cat("  - Reduces sample by", round(100 * n_hh_inaccessible / nrow(ghs_hh), 1), "%\n")
cat("  - Risk: Coverage bias if dropped PSUs differ systematically\n\n")

cat("Strategy B (Re-weight):\n")
cat("  - Maintains target population representation\n")
cat("  - Bias:", round(relbias_B_income, 2), "%\n")
cat("  - Assumption: Accessible PSUs represent inaccessible ones\n")
cat("  - Risk: Inflated variance, assumption may not hold\n\n")

cat("Strategy C (Substitute):\n")
cat("  - Maintains sample size\n")
cat("  - Bias:", round(relbias_C_income, 2), "%\n")
cat("  - Risk: Substitutes may differ from original PSUs\n")
cat("  - Requires careful matching and documentation\n\n")

# Determine best strategy
best_strategy <- which.min(abs(c(relbias_A_income, relbias_B_income, relbias_C_income)))
strategy_names <- c("A (Drop)", "B (Re-weight)", "C (Substitute)")

cat("RECOMMENDED APPROACH:\n")
cat("Based on lowest absolute bias, Strategy", strategy_names[best_strategy], 
    "performed best in this simulation.\n\n")

cat("IMPORTANT CAVEATS:\n")
cat("1. Results depend on how inaccessible PSUs differ from accessible ones\n")
cat("2. Always document the approach and conduct sensitivity analysis\n")
cat("3. Report both adjusted and unadjusted estimates when possible\n")
cat("4. Consider combining strategies (e.g., substitute + re-weight)\n\n")

#-------------------------------------------------------------------------------
# 10. EXPORT RESULTS
#-------------------------------------------------------------------------------

cat("SECTION 10: Exporting Results\n")
cat("-----------------------------\n\n")

# Save comparison table
write.csv(comparison_table, "lab4_3_strategy_comparison.csv", row.names = FALSE)
cat("Saved: lab4_3_strategy_comparison.csv\n")

# Save detailed results
results_list <- list(
  true_values = TRUE_VALUES,
  strategy_A = list(
    estimate_income = coef(est_A_income_national),
    se_income = SE(est_A_income_national),
    bias_income = bias_A_income_nat,
    relbias_income = relbias_A_income
  ),
  strategy_B = list(
    estimate_income = coef(est_B_income_national),
    se_income = SE(est_B_income_national),
    bias_income = bias_B_income_nat,
    relbias_income = relbias_B_income
  ),
  strategy_C = list(
    estimate_income = coef(est_C_income_national),
    se_income = SE(est_C_income_national),
    bias_income = bias_C_income_nat,
    relbias_income = relbias_C_income
  )
)

saveRDS(results_list, "lab4_3_results.rds")
cat("Saved: lab4_3_results.rds\n\n")

cat("================================================================\n")
cat("  LAB 4.3 COMPLETE\n")
cat("================================================================\n")

#===============================================================================
# END OF LAB 4.3
#===============================================================================
