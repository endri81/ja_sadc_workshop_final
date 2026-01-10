# ============================================================================
# LAB 3.2: CALIBRATION (RAKING)
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 3: Advanced Weighting and Variance Estimation
# ============================================================================
# 
# LEARNING OBJECTIVES:
#   1. Define population control totals for calibration
#   2. Perform raking (iterative proportional fitting) to calibrate weights
#   3. Compare NR-adjusted vs. calibrated weight estimates
#   4. Analyze g-weights (calibration adjustment factors)
#
# DATA: GHS 2024 Household File (ghs-2024-hhold-v1.dta)
# 
# REQUIRED PACKAGES: tidyverse, survey, haven, ggplot2
# ============================================================================

# Clear workspace
rm(list = ls())

# Load required packages
library(tidyverse)
library(survey)
library(haven)

# Set seed for reproducibility
set.seed(2024)

# ============================================================================
# SECTION 1: LOAD DATA AND PREPARE WEIGHTS
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 1: DATA LOADING AND PREPARATION\n")
cat(strrep("=", 70), "\n\n")

# Load GHS 2024 household data
ghs_hh <- read_dta("ghs-2024-hhold-v1.dta")

cat("Dataset dimensions:", nrow(ghs_hh), "observations,", ncol(ghs_hh), "variables\n\n")

# Check key variables exist
key_vars <- c("psu", "stratum", "prov", "geotype", "house_wgt", "totmhinc")
cat("Checking key variables:\n")
for (var in key_vars) {
  exists <- var %in% names(ghs_hh)
  cat(sprintf("  %-12s: %s\n", var, ifelse(exists, "Found", "MISSING")))
}

# Display province and geotype distributions
cat("\n--- Province Distribution ---\n")
print(table(ghs_hh$prov))

cat("\n--- GeoType Distribution ---\n")
print(table(ghs_hh$geotype))

# ============================================================================
# SECTION 2: SIMULATE NON-RESPONSE (from Lab 3.1)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 2: SIMULATE NON-RESPONSE (from Lab 3.1)\n")
cat(strrep("=", 70), "\n\n")

# Define NR probabilities by GeoType (differential non-response)
# These match Lab 3.1 parameters
nr_probs <- c("1" = 0.30, "2" = 0.18, "3" = 0.10)  # Urban, Traditional, Farms

# Create geotype key for matching
ghs_hh <- ghs_hh %>%
  mutate(
    geotype_char = as.character(geotype),
    nr_prob = nr_probs[geotype_char],
    response = rbinom(n(), 1, prob = 1 - nr_prob)
  )

# Response rates by GeoType
cat("Simulated Response Rates by GeoType:\n")
rr_table <- ghs_hh %>%
  group_by(geotype) %>%
  summarise(
    n_sampled = n(),
    n_responded = sum(response),
    response_rate = round(mean(response) * 100, 1)
  )
print(rr_table)

# ============================================================================
# SECTION 3: APPLY NON-RESPONSE ADJUSTMENT (from Lab 3.1)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 3: NON-RESPONSE ADJUSTMENT\n")
cat(strrep("=", 70), "\n\n")

# Create weighting classes (Province x GeoType)
ghs_hh <- ghs_hh %>%
  mutate(wgt_class = paste(prov, geotype, sep = "_"))

# Calculate NR adjustment factors by weighting class
nr_factors <- ghs_hh %>%
  group_by(wgt_class) %>%
  summarise(
    n_sampled = n(),
    n_responded = sum(response),
    nr_factor = n_sampled / n_responded,
    .groups = "drop"
  )

cat("NR Adjustment Factors (top 10 by factor):\n")
print(nr_factors %>% arrange(desc(nr_factor)) %>% head(10))

# Merge factors and calculate NR-adjusted weights
ghs_hh <- ghs_hh %>%
  left_join(nr_factors %>% select(wgt_class, nr_factor), by = "wgt_class") %>%
  mutate(
    base_wgt = house_wgt,
    wgt_nr = ifelse(response == 1, base_wgt * nr_factor, NA)
  )

# Keep only respondents for calibration
ghs_resp <- ghs_hh %>% filter(response == 1)

cat("\nRespondent dataset:", nrow(ghs_resp), "observations\n")
cat("Sum of NR-adjusted weights:", format(sum(ghs_resp$wgt_nr), big.mark = ","), "\n")

# ============================================================================
# SECTION 4: DEFINE POPULATION CONTROL TOTALS
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 4: POPULATION CONTROL TOTALS\n")
cat(strrep("=", 70), "\n\n")

# Population totals by Province (from Census/Admin data)
# These are hypothetical totals for the Republic of Zambara
# In practice, obtain from census or administrative records
pop_province <- data.frame(
  prov = 1:9,
  prov_name = c("Western Cape", "Eastern Cape", "Northern Cape", 
                "Free State", "KwaZulu-Natal", "North West",
                "Gauteng", "Mpumalanga", "Limpopo"),
  pop_total = c(2150000, 1950000, 420000, 
                890000, 3200000, 1180000, 
                5400000, 1450000, 1750000)
)

cat("Population Control Totals by Province:\n")
print(pop_province)
cat("\nTotal population:", format(sum(pop_province$pop_total), big.mark = ","), "\n")

# Population totals by GeoType
pop_geotype <- data.frame(
  geotype = 1:3,
  geo_name = c("Urban", "Traditional", "Farms"),
  pop_total = c(12500000, 5200000, 690000)
)

cat("\nPopulation Control Totals by GeoType:\n")
print(pop_geotype)
cat("\nTotal population:", format(sum(pop_geotype$pop_total), big.mark = ","), "\n")

# Verify totals match
if (sum(pop_province$pop_total) != sum(pop_geotype$pop_total)) {
  warning("Province and GeoType totals do not match! Adjusting GeoType...")
  # Scale GeoType totals to match Province total
  scale_factor <- sum(pop_province$pop_total) / sum(pop_geotype$pop_total)
  pop_geotype$pop_total <- round(pop_geotype$pop_total * scale_factor)
}

# ============================================================================
# SECTION 5: PREPARE DATA FOR CALIBRATION
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 5: PREPARE DATA FOR CALIBRATION\n")
cat(strrep("=", 70), "\n\n")

# Create factor variables for calibration
ghs_resp <- ghs_resp %>%
  mutate(
    prov_f = factor(prov, levels = 1:9, 
                    labels = pop_province$prov_name),
    geotype_f = factor(geotype, levels = 1:3,
                       labels = pop_geotype$geo_name)
  )

# Check current weighted totals before calibration
cat("Current weighted totals (NR-adjusted) by Province:\n")
current_prov <- ghs_resp %>%
  group_by(prov) %>%
  summarise(weighted_n = sum(wgt_nr), .groups = "drop")
print(current_prov)

cat("\nCurrent weighted totals (NR-adjusted) by GeoType:\n")
current_geo <- ghs_resp %>%
  group_by(geotype) %>%
  summarise(weighted_n = sum(wgt_nr), .groups = "drop")
print(current_geo)

# Define initial survey design with NR-adjusted weights
design_nr <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~wgt_nr,
  data = ghs_resp,
  nest = TRUE
)

cat("\nInitial survey design created with NR-adjusted weights.\n")

# ============================================================================
# SECTION 6: PERFORM RAKING (CALIBRATION)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 6: RAKING (ITERATIVE PROPORTIONAL FITTING)\n")
cat(strrep("=", 70), "\n\n")

# Create population totals in format required by rake()
# Province totals
pop_prov_list <- as.list(pop_province$pop_total)
names(pop_prov_list) <- pop_province$prov_name
pop_prov_df <- data.frame(prov_f = factor(pop_province$prov_name, 
                                           levels = pop_province$prov_name),
                          Freq = pop_province$pop_total)

# GeoType totals
pop_geo_list <- as.list(pop_geotype$pop_total)
names(pop_geo_list) <- pop_geotype$geo_name
pop_geo_df <- data.frame(geotype_f = factor(pop_geotype$geo_name,
                                             levels = pop_geotype$geo_name),
                         Freq = pop_geotype$pop_total)

cat("Performing raking on Province and GeoType margins...\n\n")

# Perform raking
design_raked <- rake(
  design = design_nr,
  sample.margins = list(~prov_f, ~geotype_f),
  population.margins = list(pop_prov_df, pop_geo_df),
  control = list(maxit = 50, epsilon = 1e-6, verbose = TRUE)
)

cat("\nRaking completed successfully!\n")

# Extract calibrated weights
ghs_resp$wgt_cal <- weights(design_raked)

# ============================================================================
# SECTION 7: CALCULATE G-WEIGHTS (CALIBRATION FACTORS)
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 7: G-WEIGHTS (CALIBRATION ADJUSTMENT FACTORS)\n")
cat(strrep("=", 70), "\n\n")

# G-weight = Calibrated weight / NR-adjusted weight
ghs_resp <- ghs_resp %>%
  mutate(g_weight = wgt_cal / wgt_nr)

# G-weight diagnostics
cat("G-Weight Diagnostics:\n")
cat(sprintf("  Mean:      %.4f\n", mean(ghs_resp$g_weight)))
cat(sprintf("  Std Dev:   %.4f\n", sd(ghs_resp$g_weight)))
cat(sprintf("  Min:       %.4f\n", min(ghs_resp$g_weight)))
cat(sprintf("  Max:       %.4f\n", max(ghs_resp$g_weight)))
cat(sprintf("  Range:     %.4f\n", max(ghs_resp$g_weight) - min(ghs_resp$g_weight)))

# G-weight by Province
cat("\nG-Weight Summary by Province:\n")
gw_prov <- ghs_resp %>%
  group_by(prov_f) %>%
  summarise(
    n = n(),
    mean_g = round(mean(g_weight), 4),
    min_g = round(min(g_weight), 4),
    max_g = round(max(g_weight), 4),
    .groups = "drop"
  )
print(gw_prov)

# G-weight by GeoType
cat("\nG-Weight Summary by GeoType:\n")
gw_geo <- ghs_resp %>%
  group_by(geotype_f) %>%
  summarise(
    n = n(),
    mean_g = round(mean(g_weight), 4),
    min_g = round(min(g_weight), 4),
    max_g = round(max(g_weight), 4),
    .groups = "drop"
  )
print(gw_geo)

# ============================================================================
# SECTION 8: VERIFY CALIBRATION
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 8: VERIFY CALIBRATION\n")
cat(strrep("=", 70), "\n\n")

# Check calibrated totals match population targets
cat("Verification: Calibrated Totals vs. Population Targets\n\n")

cat("--- Province ---\n")
cal_prov <- ghs_resp %>%
  group_by(prov) %>%
  summarise(cal_total = sum(wgt_cal), .groups = "drop") %>%
  left_join(pop_province %>% select(prov, pop_total), by = "prov") %>%
  mutate(
    diff = cal_total - pop_total,
    pct_diff = round((diff / pop_total) * 100, 4)
  )
print(cal_prov)

cat("\n--- GeoType ---\n")
cal_geo <- ghs_resp %>%
  group_by(geotype) %>%
  summarise(cal_total = sum(wgt_cal), .groups = "drop") %>%
  left_join(pop_geotype %>% select(geotype, pop_total), by = "geotype") %>%
  mutate(
    diff = cal_total - pop_total,
    pct_diff = round((diff / pop_total) * 100, 4)
  )
print(cal_geo)

# ============================================================================
# SECTION 9: COMPARE ESTIMATES - NR-ADJUSTED VS CALIBRATED
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 9: COMPARE ESTIMATES - totmhinc (Total Monthly Income)\n")
cat(strrep("=", 70), "\n\n")

# Create calibrated design object
design_cal <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~wgt_cal,
  data = ghs_resp,
  nest = TRUE
)

# Estimate mean total monthly household income
cat("Mean Total Monthly Household Income (totmhinc):\n\n")

# Check for missing values
n_miss <- sum(is.na(ghs_resp$totmhinc))
cat(sprintf("Missing values in totmhinc: %d (%.1f%%)\n\n", 
            n_miss, 100 * n_miss / nrow(ghs_resp)))

# NR-adjusted estimate
est_nr <- svymean(~totmhinc, design_nr, na.rm = TRUE)
cat("NR-Adjusted Weights:\n")
cat(sprintf("  Mean:  R %.2f\n", coef(est_nr)))
cat(sprintf("  SE:    R %.2f\n", SE(est_nr)))
cat(sprintf("  CV:    %.2f%%\n", 100 * SE(est_nr) / coef(est_nr)))

# Calibrated estimate
est_cal <- svymean(~totmhinc, design_cal, na.rm = TRUE)
cat("\nCalibrated Weights:\n")
cat(sprintf("  Mean:  R %.2f\n", coef(est_cal)))
cat(sprintf("  SE:    R %.2f\n", SE(est_cal)))
cat(sprintf("  CV:    %.2f%%\n", 100 * SE(est_cal) / coef(est_cal)))

# Difference
cat("\nDifference (Calibrated - NR-Adjusted):\n")
cat(sprintf("  Mean diff:  R %.2f (%.2f%%)\n", 
            coef(est_cal) - coef(est_nr),
            100 * (coef(est_cal) - coef(est_nr)) / coef(est_nr)))
cat(sprintf("  SE diff:    R %.2f (%.2f%%)\n",
            SE(est_cal) - SE(est_nr),
            100 * (SE(est_cal) - SE(est_nr)) / SE(est_nr)))

# Estimates by Province
cat("\n--- Mean Income by Province ---\n")
est_prov_nr <- svyby(~totmhinc, ~prov_f, design_nr, svymean, na.rm = TRUE)
est_prov_cal <- svyby(~totmhinc, ~prov_f, design_cal, svymean, na.rm = TRUE)

comparison_prov <- data.frame(
  Province = est_prov_nr$prov_f,
  NR_Mean = round(est_prov_nr$totmhinc, 0),
  NR_SE = round(est_prov_nr$se, 0),
  Cal_Mean = round(est_prov_cal$totmhinc, 0),
  Cal_SE = round(est_prov_cal$se, 0)
) %>%
  mutate(
    Diff = Cal_Mean - NR_Mean,
    Pct_Diff = round(100 * Diff / NR_Mean, 2)
  )
print(comparison_prov)

# Estimates by GeoType
cat("\n--- Mean Income by GeoType ---\n")
est_geo_nr <- svyby(~totmhinc, ~geotype_f, design_nr, svymean, na.rm = TRUE)
est_geo_cal <- svyby(~totmhinc, ~geotype_f, design_cal, svymean, na.rm = TRUE)

comparison_geo <- data.frame(
  GeoType = est_geo_nr$geotype_f,
  NR_Mean = round(est_geo_nr$totmhinc, 0),
  NR_SE = round(est_geo_nr$se, 0),
  Cal_Mean = round(est_geo_cal$totmhinc, 0),
  Cal_SE = round(est_geo_cal$se, 0)
) %>%
  mutate(
    Diff = Cal_Mean - NR_Mean,
    Pct_Diff = round(100 * Diff / NR_Mean, 2)
  )
print(comparison_geo)

# ============================================================================
# SECTION 10: WEIGHT DIAGNOSTICS COMPARISON
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 10: WEIGHT DIAGNOSTICS COMPARISON\n")
cat(strrep("=", 70), "\n\n")

# Compare weight distributions
weight_compare <- data.frame(
  Metric = c("Sum", "Mean", "SD", "CV", "Min", "Max", "Max/Min", "DEFF_w"),
  NR_Adjusted = c(
    sum(ghs_resp$wgt_nr),
    mean(ghs_resp$wgt_nr),
    sd(ghs_resp$wgt_nr),
    sd(ghs_resp$wgt_nr) / mean(ghs_resp$wgt_nr),
    min(ghs_resp$wgt_nr),
    max(ghs_resp$wgt_nr),
    max(ghs_resp$wgt_nr) / min(ghs_resp$wgt_nr),
    1 + (sd(ghs_resp$wgt_nr) / mean(ghs_resp$wgt_nr))^2
  ),
  Calibrated = c(
    sum(ghs_resp$wgt_cal),
    mean(ghs_resp$wgt_cal),
    sd(ghs_resp$wgt_cal),
    sd(ghs_resp$wgt_cal) / mean(ghs_resp$wgt_cal),
    min(ghs_resp$wgt_cal),
    max(ghs_resp$wgt_cal),
    max(ghs_resp$wgt_cal) / min(ghs_resp$wgt_cal),
    1 + (sd(ghs_resp$wgt_cal) / mean(ghs_resp$wgt_cal))^2
  )
)

weight_compare$NR_Adjusted <- round(weight_compare$NR_Adjusted, 2)
weight_compare$Calibrated <- round(weight_compare$Calibrated, 2)

print(weight_compare)

# ============================================================================
# SECTION 11: VISUALIZATIONS
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 11: VISUALIZATIONS\n")
cat(strrep("=", 70), "\n\n")

# Create output directory
if (!dir.exists("lab3_2_output")) {
  dir.create("lab3_2_output")
}

# --------------------------------------------------------------------------
# PLOT 1: G-Weight Distribution
# --------------------------------------------------------------------------
p1 <- ggplot(ghs_resp, aes(x = g_weight)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    title = "Distribution of G-Weights (Calibration Factors)",
    subtitle = "Raking on Province and GeoType margins",
    x = "G-Weight (Calibrated / NR-Adjusted)",
    y = "Density",
    caption = "Vertical line at g = 1.0 (no adjustment)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave("lab3_2_output/g_weight_distribution.png", p1, 
       width = 10, height = 6, dpi = 150)
cat("Saved: g_weight_distribution.png\n")

# --------------------------------------------------------------------------
# PLOT 2: G-Weight by Province
# --------------------------------------------------------------------------
p2 <- ggplot(ghs_resp, aes(x = prov_f, y = g_weight, fill = prov_f)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "G-Weight Distribution by Province",
    x = "Province",
    y = "G-Weight",
    caption = "Red line: g = 1.0 (no adjustment)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_fill_brewer(palette = "Set3")

ggsave("lab3_2_output/g_weight_by_province.png", p2, 
       width = 12, height = 6, dpi = 150)
cat("Saved: g_weight_by_province.png\n")

# --------------------------------------------------------------------------
# PLOT 3: G-Weight by GeoType
# --------------------------------------------------------------------------
p3 <- ggplot(ghs_resp, aes(x = geotype_f, y = g_weight, fill = geotype_f)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "G-Weight Distribution by GeoType",
    x = "Geographic Type",
    y = "G-Weight",
    caption = "Red line: g = 1.0 (no adjustment)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_fill_manual(values = c("Urban" = "#2E86AB", 
                                "Traditional" = "#A23B72", 
                                "Farms" = "#F18F01"))

ggsave("lab3_2_output/g_weight_by_geotype.png", p3, 
       width = 8, height = 6, dpi = 150)
cat("Saved: g_weight_by_geotype.png\n")

# --------------------------------------------------------------------------
# PLOT 4: Weight Comparison - NR-Adjusted vs Calibrated
# --------------------------------------------------------------------------
weight_long <- ghs_resp %>%
  select(wgt_nr, wgt_cal) %>%
  pivot_longer(cols = everything(), names_to = "Weight_Type", values_to = "Weight") %>%
  mutate(Weight_Type = factor(Weight_Type, 
                               levels = c("wgt_nr", "wgt_cal"),
                               labels = c("NR-Adjusted", "Calibrated")))

p4 <- ggplot(weight_long, aes(x = Weight, fill = Weight_Type)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Weight Distribution: NR-Adjusted vs. Calibrated",
    x = "Weight Value",
    y = "Density",
    fill = "Weight Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("NR-Adjusted" = "steelblue", 
                                "Calibrated" = "forestgreen"))

ggsave("lab3_2_output/weight_comparison_nr_vs_cal.png", p4, 
       width = 10, height = 6, dpi = 150)
cat("Saved: weight_comparison_nr_vs_cal.png\n")

# --------------------------------------------------------------------------
# PLOT 5: Income Estimates Comparison
# --------------------------------------------------------------------------
# Prepare data for plotting
est_compare <- rbind(
  data.frame(
    Domain = as.character(comparison_prov$Province),
    NR_Mean = comparison_prov$NR_Mean,
    Cal_Mean = comparison_prov$Cal_Mean,
    Type = "Province"
  ),
  data.frame(
    Domain = as.character(comparison_geo$GeoType),
    NR_Mean = comparison_geo$NR_Mean,
    Cal_Mean = comparison_geo$Cal_Mean,
    Type = "GeoType"
  )
)

est_long <- est_compare %>%
  pivot_longer(cols = c(NR_Mean, Cal_Mean), 
               names_to = "Weight_Type", values_to = "Mean_Income") %>%
  mutate(Weight_Type = factor(Weight_Type,
                               levels = c("NR_Mean", "Cal_Mean"),
                               labels = c("NR-Adjusted", "Calibrated")))

p5 <- ggplot(est_long %>% filter(Type == "Province"), 
             aes(x = Domain, y = Mean_Income, fill = Weight_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
  labs(
    title = "Mean Household Income by Province: NR-Adjusted vs. Calibrated",
    x = "Province",
    y = "Mean Monthly Income (Rand)",
    fill = "Weight Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("NR-Adjusted" = "steelblue", 
                                "Calibrated" = "forestgreen")) +
  scale_y_continuous(labels = scales::comma)

ggsave("lab3_2_output/income_comparison_by_province.png", p5, 
       width = 12, height = 7, dpi = 150)
cat("Saved: income_comparison_by_province.png\n")

# ============================================================================
# SECTION 12: EXPORT RESULTS
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 12: EXPORT RESULTS\n")
cat(strrep("=", 70), "\n\n")

# Export calibrated dataset
ghs_export <- ghs_resp %>%
  select(psu, stratum, prov, geotype, wgt_class,
         base_wgt, nr_factor, wgt_nr, wgt_cal, g_weight, totmhinc)

write_csv(ghs_export, "lab3_2_output/ghs2024_calibrated.csv")
saveRDS(ghs_export, "lab3_2_output/ghs2024_calibrated.rds")
cat("Exported: ghs2024_calibrated.csv and .rds\n")

# Export comparison tables
write_csv(comparison_prov, "lab3_2_output/income_comparison_province.csv")
write_csv(comparison_geo, "lab3_2_output/income_comparison_geotype.csv")
write_csv(weight_compare, "lab3_2_output/weight_diagnostics.csv")
cat("Exported: comparison tables\n")

# ============================================================================
# SECTION 13: SUMMARY
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("LAB 3.2 SUMMARY: CALIBRATION (RAKING)\n")
cat(strrep("=", 70), "\n\n")

cat("KEY FINDINGS:\n")
cat("-------------\n")
cat(sprintf("1. Sample size (respondents): %s\n", format(nrow(ghs_resp), big.mark = ",")))
cat(sprintf("2. Raking margins: Province (9 groups) x GeoType (3 groups)\n"))
cat(sprintf("3. G-weight range: %.3f to %.3f\n", 
            min(ghs_resp$g_weight), max(ghs_resp$g_weight)))
cat(sprintf("4. Mean g-weight: %.4f (should be ~1.0)\n", mean(ghs_resp$g_weight)))
cat(sprintf("5. Overall mean income change: %.2f%% after calibration\n",
            100 * (coef(est_cal) - coef(est_nr)) / coef(est_nr)))

cat("\nCALIBRATION ACHIEVED:\n")
cat("- Province margins: Matched to population totals\n")
cat("- GeoType margins: Matched to population totals\n")
cat("- Weight variability: ", 
    ifelse(sd(ghs_resp$wgt_cal) > sd(ghs_resp$wgt_nr), "Increased", "Decreased"),
    " after calibration\n")

cat("\nOUTPUT FILES:\n")
cat("- lab3_2_output/ghs2024_calibrated.csv\n")
cat("- lab3_2_output/g_weight_distribution.png\n")
cat("- lab3_2_output/g_weight_by_province.png\n")
cat("- lab3_2_output/g_weight_by_geotype.png\n")
cat("- lab3_2_output/weight_comparison_nr_vs_cal.png\n")
cat("- lab3_2_output/income_comparison_by_province.png\n")

cat("\n", strrep("=", 70), "\n")
cat("END OF LAB 3.2\n")
cat(strrep("=", 70), "\n")
