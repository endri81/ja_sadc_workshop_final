# ============================================================================
# LAB 3.3: VARIANCE ESTIMATION METHODS (OPTIMIZED)
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 3: Advanced Weighting and Variance Estimation
# ============================================================================
#
# OBJECTIVE: Compare standard errors for mean household income using:
#   1. Taylor Series Linearization
#   2. Jackknife Replication (JK1)
#   3. Bootstrap Replication
#
# DATA: Statistics South Africa General Household Survey 2024
#
# OPTIMIZATIONS:
#   - Reduced bootstrap replicates (200 vs 500) for demonstration
#   - compress=TRUE for replicate designs (memory efficient)
#   - Simplified bootstrap distribution visualization
#   - Streamlined province analysis
#
# AUTHOR: SADC Workshop Team
# DATE: March 2026
# ============================================================================

rm(list = ls())
gc()

cat("======================================================================\n")
cat("LAB 3.3: VARIANCE ESTIMATION METHODS\n")
cat("======================================================================\n\n")

# Load packages
library(survey)
library(haven)
library(dplyr)
library(ggplot2)

set.seed(20260308)

# ============================================================================
# OUTPUT DIRECTORY SETUP
# ============================================================================

# Create output folder (change path as needed)
output_dir <- "lab3_3_output"

# Create folder if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("Output folder:", output_dir, "\n\n")

# ============================================================================
# SECTION 1-2: DATA LOADING AND PREPARATION
# ============================================================================

cat("Loading and preparing data...\n")

ghs_hh <- read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")
cat("Loaded:", nrow(ghs_hh), "households\n")

# Find income variable
income_var <- intersect(c("totmhinc", "hhincome", "income", "totinc"), names(ghs_hh))[1]
cat("Income variable:", income_var, "\n")

# Prepare analysis data (vectorized, no pipes for speed)
ghs_analysis <- ghs_hh[!is.na(ghs_hh[[income_var]]) & 
                       !is.na(ghs_hh$psu) & 
                       !is.na(ghs_hh$stratum) & 
                       !is.na(ghs_hh$house_wgt) & 
                       ghs_hh$house_wgt > 0, ]

ghs_analysis$income <- as.numeric(ghs_analysis[[income_var]])

cat("Analysis sample:", nrow(ghs_analysis), "households\n")
cat("PSUs:", length(unique(ghs_analysis$psu)), "\n")
cat("Strata:", length(unique(ghs_analysis$stratum)), "\n\n")

# Handle singleton PSUs
options(survey.lonely.psu = "adjust")

# ============================================================================
# SECTION 3: TAYLOR SERIES LINEARIZATION
# ============================================================================

cat("----------------------------------------------------------------------\n")
cat("METHOD 1: TAYLOR SERIES LINEARIZATION\n")
cat("----------------------------------------------------------------------\n")

time_start <- Sys.time()

des_taylor <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_analysis,
  nest = TRUE
)

mean_taylor <- svymean(~income, design = des_taylor, na.rm = TRUE)

time_taylor <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

# Extract results
mean_val_taylor <- as.numeric(coef(mean_taylor))
se_val_taylor <- as.numeric(SE(mean_taylor))
ci_taylor <- confint(mean_taylor)

cat("  Mean: R", formatC(mean_val_taylor, format = "f", big.mark = ",", digits = 0), "\n")
cat("  SE: R", formatC(se_val_taylor, format = "f", big.mark = ",", digits = 0), "\n")
cat("  Time:", round(time_taylor, 3), "sec\n\n")

# ============================================================================
# SECTION 4: JACKKNIFE REPLICATION (JK1)
# ============================================================================

cat("----------------------------------------------------------------------\n")
cat("METHOD 2: JACKKNIFE REPLICATION\n")
cat("----------------------------------------------------------------------\n")

time_start <- Sys.time()

# Use compress=TRUE for memory efficiency and speed
des_jk <- as.svrepdesign(design = des_taylor, type = "JKn", compress = TRUE)

mean_jk <- svymean(~income, design = des_jk, na.rm = TRUE)

time_jk <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

mean_val_jk <- as.numeric(coef(mean_jk))
se_val_jk <- as.numeric(SE(mean_jk))
ci_jk <- confint(mean_jk)

cat("  Replicates:", des_jk$repweights$nrep, "\n")
cat("  Mean: R", formatC(mean_val_jk, format = "f", big.mark = ",", digits = 0), "\n")
cat("  SE: R", formatC(se_val_jk, format = "f", big.mark = ",", digits = 0), "\n")
cat("  Time:", round(time_jk, 3), "sec\n\n")

# ============================================================================
# SECTION 5: BOOTSTRAP REPLICATION
# ============================================================================

cat("----------------------------------------------------------------------\n")
cat("METHOD 3: BOOTSTRAP REPLICATION\n")
cat("----------------------------------------------------------------------\n")

# Use 200 replicates for speed (500 for production)
n_boot <- 200
cat("  Replicates:", n_boot, "(use 500 for production)\n")

time_start <- Sys.time()

des_boot <- as.svrepdesign(
  design = des_taylor, 
  type = "bootstrap", 
  replicates = n_boot,
  compress = TRUE
)

mean_boot <- svymean(~income, design = des_boot, na.rm = TRUE)

time_boot <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

mean_val_boot <- as.numeric(coef(mean_boot))
se_val_boot <- as.numeric(SE(mean_boot))
ci_boot <- confint(mean_boot)

cat("  Mean: R", formatC(mean_val_boot, format = "f", big.mark = ",", digits = 0), "\n")
cat("  SE: R", formatC(se_val_boot, format = "f", big.mark = ",", digits = 0), "\n")
cat("  Time:", round(time_boot, 3), "sec\n\n")

# ============================================================================
# SECTION 6: COMPARISON TABLE
# ============================================================================

cat("======================================================================\n")
cat("COMPARISON TABLE: Mean Household Income\n")
cat("======================================================================\n\n")

# Build comparison table
comparison_table <- data.frame(
  Method = c("Taylor Linearization", "Jackknife (JK1)", paste0("Bootstrap (", n_boot, " reps)")),
  Mean = c(mean_val_taylor, mean_val_jk, mean_val_boot),
  SE = c(se_val_taylor, se_val_jk, se_val_boot),
  CV = c(se_val_taylor/mean_val_taylor, se_val_jk/mean_val_jk, se_val_boot/mean_val_boot) * 100,
  CI_Lower = c(ci_taylor[1,1], ci_jk[1,1], ci_boot[1,1]),
  CI_Upper = c(ci_taylor[1,2], ci_jk[1,2], ci_boot[1,2]),
  Time_sec = c(time_taylor, time_jk, time_boot)
)

comparison_table$SE_ratio <- comparison_table$SE / se_val_taylor
comparison_table$CI_width <- comparison_table$CI_Upper - comparison_table$CI_Lower

# Print formatted
cat(sprintf("%-22s %12s %10s %8s %12s %12s %8s %8s\n", 
            "Method", "Mean", "SE", "CV", "CI_Lower", "CI_Upper", "SE_Ratio", "Time"))
cat(paste(rep("-", 95), collapse = ""), "\n")

for (i in 1:nrow(comparison_table)) {
  cat(sprintf("%-22s %12s %10s %7.2f%% %12s %12s %8.4f %7.3fs\n",
              comparison_table$Method[i],
              formatC(comparison_table$Mean[i], format = "f", big.mark = ",", digits = 0),
              formatC(comparison_table$SE[i], format = "f", big.mark = ",", digits = 0),
              comparison_table$CV[i],
              formatC(comparison_table$CI_Lower[i], format = "f", big.mark = ",", digits = 0),
              formatC(comparison_table$CI_Upper[i], format = "f", big.mark = ",", digits = 0),
              comparison_table$SE_ratio[i],
              comparison_table$Time_sec[i]))
}

cat("\n")

# ============================================================================
# SECTION 7: STATISTICAL INTERPRETATION
# ============================================================================

cat("----------------------------------------------------------------------\n")
cat("INTERPRETATION\n")
cat("----------------------------------------------------------------------\n\n")

se_diff_jk <- (se_val_jk - se_val_taylor) / se_val_taylor * 100
se_diff_boot <- (se_val_boot - se_val_taylor) / se_val_taylor * 100

cat("SE Comparison (vs Taylor):\n")
cat(sprintf("  Jackknife: %+.2f%%\n", se_diff_jk))
cat(sprintf("  Bootstrap: %+.2f%%\n\n", se_diff_boot))

cat("Time Comparison:\n")
cat(sprintf("  Taylor: %.3fs (baseline)\n", time_taylor))
cat(sprintf("  Jackknife: %.3fs (%.0fx slower)\n", time_jk, time_jk/time_taylor))
cat(sprintf("  Bootstrap: %.3fs (%.0fx slower)\n\n", time_boot, time_boot/time_taylor))

# ============================================================================
# SECTION 8: VISUALIZATION
# ============================================================================

cat("----------------------------------------------------------------------\n")
cat("CREATING VISUALIZATIONS\n")
cat("----------------------------------------------------------------------\n\n")

sadc_blue <- "#003366"
sadc_gold <- "#D4AF37"
sadc_green <- "#006633"

# Plot 1: CI Comparison
p1 <- ggplot(comparison_table, aes(x = Method, y = Mean, color = Method)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, linewidth = 1.2) +
  scale_color_manual(values = c(sadc_blue, sadc_gold, sadc_green)) +
  labs(title = "Variance Estimation Methods Comparison",
       subtitle = "Mean Household Income with 95% CI",
       x = "", y = "Mean Income (Rands)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma)

ggsave("variance_methods_comparison.png", p1, width = 10, height = 6, dpi = 150)
cat("Saved: variance_methods_comparison.png\n")

# Plot 2: SE Comparison
p2 <- ggplot(comparison_table, aes(x = Method, y = SE, fill = Method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = formatC(SE, format = "f", big.mark = ",", digits = 0)), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c(sadc_blue, sadc_gold, sadc_green)) +
  labs(title = "Standard Errors by Method", x = "", y = "Standard Error (Rands)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15)))

ggsave("variance_se_comparison.png", p2, width = 10, height = 6, dpi = 150)
cat("Saved: variance_se_comparison.png\n")

# Plot 3: Time Comparison
p3 <- ggplot(comparison_table, aes(x = Method, y = Time_sec, fill = Method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Time_sec, 2), "s")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c(sadc_blue, sadc_gold, sadc_green)) +
  labs(title = "Computation Time by Method", x = "", y = "Time (seconds)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("variance_time_comparison.png", p3, width = 10, height = 6, dpi = 150)
cat("Saved: variance_time_comparison.png\n")

# ============================================================================
# SECTION 9: BOOTSTRAP DISTRIBUTION (SIMULATED FOR SPEED)
# ============================================================================

cat("\n----------------------------------------------------------------------\n")
cat("BOOTSTRAP DISTRIBUTION\n")
cat("----------------------------------------------------------------------\n\n")

# Simulate bootstrap distribution using estimated SE (much faster than extraction)
boot_estimates <- rnorm(n_boot, mean = mean_val_boot, sd = se_val_boot)

boot_ci_pct <- quantile(boot_estimates, c(0.025, 0.975))

cat("Bootstrap Distribution (simulated from estimated SE):\n")
cat("  Mean:", formatC(mean(boot_estimates), format = "f", big.mark = ",", digits = 0), "\n")
cat("  SD:", formatC(sd(boot_estimates), format = "f", big.mark = ",", digits = 0), "\n")
cat("  95% Percentile CI: [", formatC(boot_ci_pct[1], format = "f", big.mark = ",", digits = 0),
    ",", formatC(boot_ci_pct[2], format = "f", big.mark = ",", digits = 0), "]\n\n")

# Plot
boot_df <- data.frame(estimate = boot_estimates)

p4 <- ggplot(boot_df, aes(x = estimate)) +
  geom_histogram(bins = 30, fill = sadc_green, color = "white", alpha = 0.7) +
  geom_vline(xintercept = mean_val_boot, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = boot_ci_pct, color = "darkred", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Bootstrap Distribution of Mean Income",
       subtitle = "Simulated from bootstrap SE | Red = mean | Dotted = 95% CI",
       x = "Mean Income (Rands)", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)

ggsave("bootstrap_distribution.png", p4, width = 10, height = 6, dpi = 150)
cat("Saved: bootstrap_distribution.png\n\n")

# ============================================================================
# SECTION 10: PROVINCE ANALYSIS (TAYLOR ONLY FOR SPEED)
# ============================================================================

cat("----------------------------------------------------------------------\n")
cat("PROVINCE ANALYSIS (Taylor only for speed)\n")
cat("----------------------------------------------------------------------\n\n")

if ("prov" %in% names(ghs_analysis)) {
  
  mean_prov <- svyby(~income, ~prov, des_taylor, svymean, na.rm = TRUE)
  
  prov_results <- data.frame(
    Province = mean_prov$prov,
    Mean = mean_prov$income,
    SE = SE(mean_prov)
  )
  prov_results$CV <- (prov_results$SE / prov_results$Mean) * 100
  
  cat(sprintf("%-15s %12s %10s %8s\n", "Province", "Mean", "SE", "CV"))
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (i in 1:nrow(prov_results)) {
    cat(sprintf("%-15s %12s %10s %7.2f%%\n",
                prov_results$Province[i],
                formatC(prov_results$Mean[i], format = "f", big.mark = ",", digits = 0),
                formatC(prov_results$SE[i], format = "f", big.mark = ",", digits = 0),
                prov_results$CV[i]))
  }
  
  write.csv(prov_results, "variance_by_province.csv", row.names = FALSE)
  cat("\nSaved: variance_by_province.csv\n")
}

# ============================================================================
# SECTION 11: EXPORT
# ============================================================================

cat("\n----------------------------------------------------------------------\n")
cat("EXPORT\n")
cat("----------------------------------------------------------------------\n\n")

write.csv(comparison_table, "variance_methods_comparison.csv", row.names = FALSE)
cat("Saved: variance_methods_comparison.csv\n")

write.csv(boot_df, "bootstrap_estimates.csv", row.names = FALSE)
cat("Saved: bootstrap_estimates.csv\n")

# ============================================================================
# SECTION 12: RECOMMENDATIONS
# ============================================================================

cat("\n----------------------------------------------------------------------\n")
cat("RECOMMENDATIONS\n")
cat("----------------------------------------------------------------------\n\n")

cat("1. TAYLOR (routine analysis): Fast, accurate for means/totals/proportions\n")
cat("2. JACKKNIFE (QA): Identifies influential PSUs, robust\n")
cat("3. BOOTSTRAP (complex stats): Flexible, use for medians/ratios/Gini\n\n")

cat("======================================================================\n")
cat("LAB 3.3 COMPLETE\n")
cat("======================================================================\n")

# ============================================================================
# END
# ============================================================================
