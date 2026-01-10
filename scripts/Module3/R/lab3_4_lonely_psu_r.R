# ============================================================================
# LAB 3.4: HANDLING LONELY PSUs (SINGLETON STRATA)
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 3, Module 3.4
# 
# Software: R with survey package
# Data: GHS 2024 Household File (ghs-2024-hhold-v1.dta)
# 
# Learning Objectives:
#   - Understand why singleton strata cause variance estimation problems
#   - Simulate singleton stratum scenarios
#   - Compare different correction methods
#   - Apply appropriate solutions in practice
#
# Author: SADC Workshop Team
# Date: January 2026
# ============================================================================

# ============================================================================
# SECTION 1: SETUP AND DATA PREPARATION
# ============================================================================

# Clear workspace
rm(list = ls())

# Load required packages
library(survey)
library(haven)
library(dplyr)
library(ggplot2)
library(knitr)

# Set output directory
output_dir <- "lab3_4_output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load GHS 2024 household data
cat("=" , rep("=", 70), "\n", sep = "")
cat("LAB 3.4: HANDLING LONELY PSUs (SINGLETON STRATA)\n")
cat("=", rep("=", 70), "\n\n", sep = "")

# Adjust path as needed
ghs_hh <- read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")

cat("Data loaded:", nrow(ghs_hh), "households\n\n")

# ============================================================================
# SECTION 2: UNDERSTAND THE PROBLEM
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 2: THE SINGLETON PSU PROBLEM\n")
cat("-", rep("-", 70), "\n\n", sep = "")

# Examine current stratification
psu_per_stratum <- ghs_hh %>%
  group_by(stratum) %>%
  summarise(
    n_psu = n_distinct(psu),
    n_hh = n(),
    .groups = "drop"
  )

cat("Current design summary:\n")
cat("  Total strata:", n_distinct(ghs_hh$stratum), "\n")
cat("  Total PSUs:", n_distinct(ghs_hh$psu), "\n")
cat("  PSUs per stratum range:", min(psu_per_stratum$n_psu), "to", 
    max(psu_per_stratum$n_psu), "\n\n")

# Check for existing singletons
existing_singletons <- psu_per_stratum %>% filter(n_psu == 1)
cat("Existing singleton strata:", nrow(existing_singletons), "\n\n")

# Explain the problem
cat("THE PROBLEM:\n")
cat("  Variance estimation formula requires at least 2 PSUs per stratum:\n")
cat("  \n")
cat("  Var_h = (n_h / (n_h - 1)) * sum[(z_hi - z_h_bar)^2]\n")
cat("  \n")
cat("  When n_h = 1:\n")
cat("    - Denominator (n_h - 1) = 0 → Division by zero!\n")
cat("    - Only one PSU → Cannot estimate within-stratum variance\n")
cat("    - No information about variability in that stratum\n\n")

# ============================================================================
# SECTION 3: SIMULATE SINGLETON STRATA
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 3: SIMULATING SINGLETON STRATA\n")
cat("-", rep("-", 70), "\n\n")

# Create a copy for simulation
ghs_sim <- ghs_hh

# Strategy: Collapse some strata to create artificial singletons
# We'll modify stratum codes to simulate the problem

# Find strata with exactly 2 PSUs (candidates for making singletons)
strata_with_2_psus <- psu_per_stratum %>% 
  filter(n_psu == 2) %>%
  pull(stratum)

cat("Strata with exactly 2 PSUs:", length(strata_with_2_psus), "\n")

if (length(strata_with_2_psus) >= 3) {
  # Select 3 strata to convert to singletons
  target_strata <- strata_with_2_psus[1:3]
  
  cat("Converting these strata to singletons:", paste(target_strata, collapse = ", "), "\n\n")
  
  # For each target stratum, keep only 1 PSU
  for (s in target_strata) {
    psus_in_stratum <- unique(ghs_sim$psu[ghs_sim$stratum == s])
    if (length(psus_in_stratum) >= 2) {
      # Keep only the first PSU, remove the second
      psu_to_remove <- psus_in_stratum[2]
      ghs_sim <- ghs_sim %>% filter(!(stratum == s & psu == psu_to_remove))
    }
  }
} else {
  # Alternative: Create singleton by assigning unique stratum to one PSU
  cat("Creating singleton by reassigning stratum codes...\n\n")
  
  # Pick a PSU and give it a unique stratum code
  sample_psu <- unique(ghs_sim$psu)[1]
  ghs_sim$stratum[ghs_sim$psu == sample_psu] <- 9999  # Unique singleton stratum
  target_strata <- 9999
}

# Verify we have singletons
psu_per_stratum_sim <- ghs_sim %>%
  group_by(stratum) %>%
  summarise(
    n_psu = n_distinct(psu),
    n_hh = n(),
    .groups = "drop"
  )

singleton_strata <- psu_per_stratum_sim %>% filter(n_psu == 1)
cat("Singleton strata created:", nrow(singleton_strata), "\n")
print(singleton_strata)
cat("\n")

# ============================================================================
# SECTION 4: DEMONSTRATE THE ERROR
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 4: DEMONSTRATING THE ERROR\n")
cat("-", rep("-", 70), "\n\n")

# Define outcome variable
income_var <- "totmhinc"

# Prepare data
ghs_sim_clean <- ghs_sim %>%
  filter(!is.na(.data[[income_var]]) & !is.na(psu) & !is.na(stratum) & !is.na(house_wgt))

cat("Analysis sample:", nrow(ghs_sim_clean), "households\n\n")

# Set option to FAIL on lonely PSUs (default behavior demonstration)
options(survey.lonely.psu = "fail")

cat("Setting: options(survey.lonely.psu = 'fail')\n")
cat("This is the DEFAULT behavior - throws an error\n\n")

# Try to create survey design and estimate mean
tryCatch({
  des_fail <- svydesign(
    id = ~psu,
    strata = ~stratum,
    weights = ~house_wgt,
    data = ghs_sim_clean,
    nest = TRUE
  )
  
  mean_result <- svymean(~totmhinc, des_fail, na.rm = TRUE)
  cat("Result:", as.numeric(coef(mean_result)), "\n")
  
}, error = function(e) {
  cat("ERROR ENCOUNTERED:\n")
  cat("  ", conditionMessage(e), "\n\n")
  cat("This error occurs because we cannot estimate variance with n_h = 1 PSU\n\n")
})

# ============================================================================
# SECTION 5: CORRECTION METHOD 1 - CERTAINTY
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 5: METHOD 1 - CERTAINTY\n")
cat("-", rep("-", 70), "\n\n")

cat("Setting: options(survey.lonely.psu = 'certainty')\n")
cat("Treatment: Singleton PSU contributes ZERO to variance\n")
cat("Rationale: Treat as if PSU was selected with certainty (probability = 1)\n")
cat("Effect: Variance may be UNDERESTIMATED\n\n")

options(survey.lonely.psu = "certainty")

des_certainty <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_sim_clean,
  nest = TRUE
)

# Calculate mean
mean_certainty <- svymean(~totmhinc, des_certainty, na.rm = TRUE)
mean_val_certainty <- as.numeric(coef(mean_certainty))
se_val_certainty <- as.numeric(SE(mean_certainty))
ci_certainty <- confint(mean_certainty)

cat("Results (Certainty Method):\n")
cat("  Mean: R", format(round(mean_val_certainty, 0), big.mark = ","), "\n")
cat("  SE:   R", format(round(se_val_certainty, 0), big.mark = ","), "\n")
cat("  95% CI: [R", format(round(ci_certainty[1,1], 0), big.mark = ","), 
    ", R", format(round(ci_certainty[1,2], 0), big.mark = ","), "]\n\n")

# ============================================================================
# SECTION 6: CORRECTION METHOD 2 - ADJUST (CENTERED)
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 6: METHOD 2 - ADJUST (CENTERED)\n")
cat("-", rep("-", 70), "\n\n")

cat("Setting: options(survey.lonely.psu = 'adjust')\n")
cat("Treatment: Center singleton around GRAND MEAN instead of stratum mean\n")
cat("Rationale: Use overall variability as proxy for missing stratum variance\n")
cat("Effect: Usually INCREASES variance estimate (conservative)\n\n")

options(survey.lonely.psu = "adjust")

des_adjust <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_sim_clean,
  nest = TRUE
)

# Calculate mean
mean_adjust <- svymean(~totmhinc, des_adjust, na.rm = TRUE)
mean_val_adjust <- as.numeric(coef(mean_adjust))
se_val_adjust <- as.numeric(SE(mean_adjust))
ci_adjust <- confint(mean_adjust)

cat("Results (Adjust/Centered Method):\n")
cat("  Mean: R", format(round(mean_val_adjust, 0), big.mark = ","), "\n")
cat("  SE:   R", format(round(se_val_adjust, 0), big.mark = ","), "\n")
cat("  95% CI: [R", format(round(ci_adjust[1,1], 0), big.mark = ","), 
    ", R", format(round(ci_adjust[1,2], 0), big.mark = ","), "]\n\n")

# ============================================================================
# SECTION 7: CORRECTION METHOD 3 - AVERAGE
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 7: METHOD 3 - AVERAGE\n")
cat("-", rep("-", 70), "\n\n")

cat("Setting: options(survey.lonely.psu = 'average')\n")
cat("Treatment: Use AVERAGE variance from other strata in same sampling stage\n")
cat("Rationale: Borrow information from similar strata\n")
cat("Effect: Moderate approach between certainty and adjust\n\n")

options(survey.lonely.psu = "average")

des_average <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_sim_clean,
  nest = TRUE
)

# Calculate mean
mean_average <- svymean(~totmhinc, des_average, na.rm = TRUE)
mean_val_average <- as.numeric(coef(mean_average))
se_val_average <- as.numeric(SE(mean_average))
ci_average <- confint(mean_average)

cat("Results (Average Method):\n")
cat("  Mean: R", format(round(mean_val_average, 0), big.mark = ","), "\n")
cat("  SE:   R", format(round(se_val_average, 0), big.mark = ","), "\n")
cat("  95% CI: [R", format(round(ci_average[1,1], 0), big.mark = ","), 
    ", R", format(round(ci_average[1,2], 0), big.mark = ","), "]\n\n")

# ============================================================================
# SECTION 8: CORRECTION METHOD 4 - REMOVE
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 8: METHOD 4 - REMOVE\n")
cat("-", rep("-", 70), "\n\n")

cat("Setting: options(survey.lonely.psu = 'remove')\n")
cat("Treatment: Remove singleton strata from variance calculation entirely\n")
cat("Rationale: Only use strata where we can estimate variance\n")
cat("Effect: May underestimate if singletons are systematically different\n\n")

options(survey.lonely.psu = "remove")

des_remove <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_sim_clean,
  nest = TRUE
)

# Calculate mean
mean_remove <- svymean(~totmhinc, des_remove, na.rm = TRUE)
mean_val_remove <- as.numeric(coef(mean_remove))
se_val_remove <- as.numeric(SE(mean_remove))
ci_remove <- confint(mean_remove)

cat("Results (Remove Method):\n")
cat("  Mean: R", format(round(mean_val_remove, 0), big.mark = ","), "\n")
cat("  SE:   R", format(round(se_val_remove, 0), big.mark = ","), "\n")
cat("  95% CI: [R", format(round(ci_remove[1,1], 0), big.mark = ","), 
    ", R", format(round(ci_remove[1,2], 0), big.mark = ","), "]\n\n")

# ============================================================================
# SECTION 9: COMPARISON TABLE
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 9: COMPARISON OF ALL METHODS\n")
cat("-", rep("-", 70), "\n\n")

# Create comparison table
comparison_df <- data.frame(
  Method = c("Certainty", "Adjust (Centered)", "Average", "Remove"),
  Mean = c(mean_val_certainty, mean_val_adjust, mean_val_average, mean_val_remove),
  SE = c(se_val_certainty, se_val_adjust, se_val_average, se_val_remove),
  stringsAsFactors = FALSE
)

comparison_df$CV <- (comparison_df$SE / comparison_df$Mean) * 100
comparison_df$CI_Lower <- comparison_df$Mean - 1.96 * comparison_df$SE
comparison_df$CI_Upper <- comparison_df$Mean + 1.96 * comparison_df$SE
comparison_df$CI_Width <- comparison_df$CI_Upper - comparison_df$CI_Lower
comparison_df$SE_Ratio <- comparison_df$SE / comparison_df$SE[1]  # Ratio to certainty

# Print comparison
cat("COMPARISON OF LONELY PSU HANDLING METHODS\n")
cat("=========================================\n\n")

print(kable(comparison_df, 
            digits = c(0, 0, 0, 2, 0, 0, 0, 4),
            format.args = list(big.mark = ","),
            col.names = c("Method", "Mean (R)", "SE (R)", "CV (%)", 
                          "CI Lower", "CI Upper", "CI Width", "SE Ratio")))

cat("\n")

# Interpretation
cat("INTERPRETATION:\n")
cat("--------------\n")
cat("1. Point estimates (Mean) are identical across all methods\n")
cat("2. Standard errors differ based on how singleton variance is handled:\n")
cat("   - Certainty: Lowest SE (singleton contributes 0)\n")
cat("   - Remove: Similar to certainty\n")
cat("   - Average: Moderate SE\n")
cat("   - Adjust: Typically highest SE (conservative)\n\n")

# ============================================================================
# SECTION 10: BASELINE COMPARISON (NO SINGLETONS)
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 10: BASELINE (ORIGINAL DATA WITHOUT SINGLETONS)\n")
cat("-", rep("-", 70), "\n\n")

# Use original data (no artificial singletons)
ghs_orig_clean <- ghs_hh %>%
  filter(!is.na(.data[[income_var]]) & !is.na(psu) & !is.na(stratum) & !is.na(house_wgt))

options(survey.lonely.psu = "fail")  # Reset to default

des_baseline <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_orig_clean,
  nest = TRUE
)

mean_baseline <- svymean(~totmhinc, des_baseline, na.rm = TRUE)
mean_val_baseline <- as.numeric(coef(mean_baseline))
se_val_baseline <- as.numeric(SE(mean_baseline))

cat("Baseline Results (Original data, no singletons):\n")
cat("  Mean: R", format(round(mean_val_baseline, 0), big.mark = ","), "\n")
cat("  SE:   R", format(round(se_val_baseline, 0), big.mark = ","), "\n\n")

cat("Impact of singleton strata:\n")
cat("  Baseline SE:   R", format(round(se_val_baseline, 0), big.mark = ","), "\n")
cat("  Certainty SE:  R", format(round(se_val_certainty, 0), big.mark = ","), 
    " (", round((se_val_certainty/se_val_baseline - 1)*100, 1), "% change)\n")
cat("  Adjust SE:     R", format(round(se_val_adjust, 0), big.mark = ","), 
    " (", round((se_val_adjust/se_val_baseline - 1)*100, 1), "% change)\n")
cat("  Average SE:    R", format(round(se_val_average, 0), big.mark = ","), 
    " (", round((se_val_average/se_val_baseline - 1)*100, 1), "% change)\n\n")

# ============================================================================
# SECTION 11: VISUALIZATION
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 11: VISUALIZATION\n")
cat("-", rep("-", 70), "\n\n")

# Add baseline to comparison
comparison_full <- rbind(
  data.frame(Method = "Baseline (No Singletons)", 
             Mean = mean_val_baseline, 
             SE = se_val_baseline,
             CV = (se_val_baseline/mean_val_baseline)*100,
             CI_Lower = mean_val_baseline - 1.96*se_val_baseline,
             CI_Upper = mean_val_baseline + 1.96*se_val_baseline,
             CI_Width = 2*1.96*se_val_baseline,
             SE_Ratio = se_val_baseline/se_val_certainty),
  comparison_df
)

# Order methods for plotting
comparison_full$Method <- factor(comparison_full$Method, 
                                  levels = c("Baseline (No Singletons)", 
                                             "Certainty", "Remove", 
                                             "Average", "Adjust (Centered)"))

# Plot 1: SE Comparison
p1 <- ggplot(comparison_full, aes(x = Method, y = SE, fill = Method)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = format(round(SE, 0), big.mark = ",")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("gray50", "#003366", "#003366", "#D4AF37", "#006633")) +
  labs(title = "Standard Error by Lonely PSU Handling Method",
       subtitle = "Mean Household Income (GHS 2024 with Simulated Singletons)",
       x = "", y = "Standard Error (Rands)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15)))

ggsave(file.path(output_dir, "lonely_psu_se_comparison.png"), p1, 
       width = 10, height = 6, dpi = 150)
cat("Saved: lonely_psu_se_comparison.png\n")

# Plot 2: CI Comparison
p2 <- ggplot(comparison_full, aes(x = Method, y = Mean, color = Method)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.3, linewidth = 1.2) +
  scale_color_manual(values = c("gray50", "#003366", "#003366", "#D4AF37", "#006633")) +
  labs(title = "95% Confidence Intervals by Method",
       subtitle = "Mean Household Income",
       x = "", y = "Income (Rands)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::comma)

ggsave(file.path(output_dir, "lonely_psu_ci_comparison.png"), p2, 
       width = 10, height = 6, dpi = 150)
cat("Saved: lonely_psu_ci_comparison.png\n\n")

# ============================================================================
# SECTION 12: EXPORT RESULTS
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 12: EXPORT RESULTS\n")
cat("-", rep("-", 70), "\n\n")

# Export comparison table
write.csv(comparison_full, file.path(output_dir, "lonely_psu_comparison.csv"), 
          row.names = FALSE)
cat("Saved: lonely_psu_comparison.csv\n")

# Export singleton stratum info
write.csv(singleton_strata, file.path(output_dir, "singleton_strata_info.csv"),
          row.names = FALSE)
cat("Saved: singleton_strata_info.csv\n\n")

# ============================================================================
# SECTION 13: PRACTICAL RECOMMENDATIONS
# ============================================================================

cat("-", rep("-", 70), "\n", sep = "")
cat("SECTION 13: PRACTICAL RECOMMENDATIONS\n")
cat("-", rep("-", 70), "\n\n")

cat("WHEN TO USE EACH METHOD:\n")
cat("========================\n\n")

cat("1. CERTAINTY (survey.lonely.psu = 'certainty')\n")
cat("   Use when: PSU was actually selected with certainty (self-representing)\n")
cat("   Example: A large metropolitan area that must be included\n")
cat("   Caution: Will UNDERESTIMATE variance if PSU wasn't really certainty\n\n")

cat("2. ADJUST/CENTERED (survey.lonely.psu = 'adjust')\n")
cat("   Use when: Want conservative (larger) variance estimates\n")
cat("   Example: Default choice when uncertain, for publication\n")
cat("   Advantage: Less likely to claim false precision\n\n")

cat("3. AVERAGE (survey.lonely.psu = 'average')\n")
cat("   Use when: Singleton strata are similar to other strata\n")
cat("   Example: When singletons resulted from random non-response\n")
cat("   Advantage: Uses actual variance information from data\n\n")

cat("4. REMOVE (survey.lonely.psu = 'remove')\n")
cat("   Use when: Singleton strata are small and negligible\n")
cat("   Caution: May bias results if singletons are systematically different\n\n")

cat("BEST PRACTICE:\n")
cat("--------------\n")
cat("1. PREVENT singletons in design stage:\n")
cat("   - Sample at least 2 PSUs per stratum\n")
cat("   - Collapse small strata during design\n")
cat("2. If singletons occur (e.g., non-response):\n")
cat("   - Document the issue\n")
cat("   - Use 'adjust' for conservative estimates\n")
cat("   - Report sensitivity analysis with multiple methods\n")
cat("3. For official statistics:\n")
cat("   - Follow agency guidelines\n")
cat("   - Common choice: 'adjust' or 'certainty' depending on context\n\n")

# Reset to default
options(survey.lonely.psu = "fail")

cat("=", rep("=", 70), "\n", sep = "")
cat("LAB 3.4 COMPLETE\n")
cat("=", rep("=", 70), "\n")
cat("\nOutput files saved to:", output_dir, "\n")
cat("Reset to default: options(survey.lonely.psu = 'fail')\n")
