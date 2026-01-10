#===============================================================================
# LAB 4.2: HOT-DECK IMPUTATION
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 4, Module 4.2: Error Mitigation and Quality Assurance
#===============================================================================
#
# OBJECTIVE: Compare imputation methods for handling missing income data
#   1. Introduce MAR missing values in income based on geotype
#   2. Mean Imputation (the "bad" way - distorts variance)
#   3. Hot-Deck Imputation (the "good" way - preserves distribution)
#   4. Compare distributions via density plots
#
# DATA: GHS 2024 Household File
# KEY VARIABLE: totmhinc (Total Monthly Household Income)
# DONOR CLASSES: Province × Household Size Group
#
# ZAMBARA NARRATIVE: Lindiwe addresses 24% missing income data
#
#===============================================================================

#-------------------------------------------------------------------------------
# 0. SETUP AND PACKAGES
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Required packages
required_packages <- c("haven", "dplyr", "tidyr", "ggplot2", "VIM", 
                       "gridExtra", "scales", "knitr")

# Install if missing
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set seed for reproducibility
set.seed(20260305)

cat("\n")
cat("================================================================\n")
cat("  LAB 4.2: HOT-DECK IMPUTATION\n")
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

# Key variables:
# - totmhinc: Total monthly household income
# - hholdsz: Household size
# - prov: Province (1-9)
# - geotype: Geography type (1=Urban, 2=Traditional, 3=Farms)
# - house_wgt: Household weight

# Create province variable from stratum if needed
if (!"prov" %in% names(ghs_hh)) {
  ghs_hh <- ghs_hh %>%
    mutate(prov = as.numeric(substr(as.character(stratum), 1, 1)))
}

# Province labels
prov_labels <- c(

"1" = "Western Cape", "2" = "Eastern Cape", "3" = "Northern Cape",
  "4" = "Free State", "5" = "KwaZulu-Natal", "6" = "North West",
  "7" = "Gauteng", "8" = "Mpumalanga", "9" = "Limpopo"
)

ghs_hh <- ghs_hh %>%
  mutate(
    prov_name = prov_labels[as.character(prov)],
    geotype_name = case_when(
      geotype == 1 ~ "Urban",
      geotype == 2 ~ "Traditional",
      geotype == 3 ~ "Farms",
      TRUE ~ "Unknown"
    )
  )

# Check original income distribution
cat("Original Income Distribution:\n")
cat("  N with valid income:", sum(!is.na(ghs_hh$totmhinc) & ghs_hh$totmhinc >= 0), "\n")
cat("  Mean:", format(mean(ghs_hh$totmhinc, na.rm = TRUE), big.mark = ",", nsmall = 0), "\n")
cat("  Median:", format(median(ghs_hh$totmhinc, na.rm = TRUE), big.mark = ",", nsmall = 0), "\n")
cat("  Std Dev:", format(sd(ghs_hh$totmhinc, na.rm = TRUE), big.mark = ",", nsmall = 0), "\n")
cat("  Min:", format(min(ghs_hh$totmhinc, na.rm = TRUE), big.mark = ","), "\n")
cat("  Max:", format(max(ghs_hh$totmhinc, na.rm = TRUE), big.mark = ","), "\n\n")

# Store original complete data for comparison
ghs_original <- ghs_hh %>%
  filter(!is.na(totmhinc) & totmhinc >= 0) %>%
  mutate(income_original = totmhinc)

#-------------------------------------------------------------------------------
# 2. INTRODUCE MISSING AT RANDOM (MAR) MECHANISM
#-------------------------------------------------------------------------------

cat("SECTION 2: Introducing Missing Values (MAR)\n")
cat("-------------------------------------------\n\n")

# MAR mechanism: Missing probability depends on geotype
# Urban areas: higher refusal (privacy concerns) -> 30% missing
# Traditional: moderate -> 20% missing  
# Farms: lower -> 15% missing

mar_probs <- c(
  "Urban" = 0.30,
  "Traditional" = 0.20,
  "Farms" = 0.15
)

cat("MAR Mechanism - Missing Probabilities by Geotype:\n")
for (g in names(mar_probs)) {
  cat("  ", g, ":", mar_probs[g] * 100, "%\n")
}
cat("\n")

# Create working dataset
ghs_work <- ghs_hh %>%
  filter(!is.na(totmhinc) & totmhinc >= 0) %>%
  mutate(
    # Generate uniform random for MAR selection
    rand = runif(n()),
    # Assign missing based on geotype-specific probability
    missing_prob = case_when(
      geotype_name == "Urban" ~ mar_probs["Urban"],
      geotype_name == "Traditional" ~ mar_probs["Traditional"],
      geotype_name == "Farms" ~ mar_probs["Farms"],
      TRUE ~ 0.20
    ),
    # Flag as missing if random < probability
    is_missing = rand < missing_prob,
    # Store true income for later comparison
    income_true = totmhinc,
    # Set to NA for "missing" cases
    income_observed = ifelse(is_missing, NA, totmhinc)
  )

# Summary of missing values
missing_summary <- ghs_work %>%
  group_by(geotype_name) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is_missing),
    pct_missing = round(100 * mean(is_missing), 1),
    .groups = "drop"
  )

cat("Missing Values by Geotype:\n")
print(as.data.frame(missing_summary))
cat("\n")

total_missing <- sum(ghs_work$is_missing)
total_n <- nrow(ghs_work)
pct_total_missing <- round(100 * total_missing / total_n, 1)

cat("Total Missing:", total_missing, "of", total_n, "(", pct_total_missing, "%)\n\n")

# Verify MAR: Compare observed vs missing on other characteristics
cat("Verifying MAR: Comparing Observed vs Missing Cases:\n")
mar_check <- ghs_work %>%
  group_by(is_missing) %>%
  summarise(
    n = n(),
    mean_hhsize = mean(hholdsz, na.rm = TRUE),
    pct_urban = 100 * mean(geotype_name == "Urban"),
    mean_income_true = mean(income_true, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(is_missing, "Missing", "Observed"))

print(as.data.frame(mar_check))
cat("\n")

#-------------------------------------------------------------------------------
# 3. STORE ORIGINAL STATISTICS (BENCHMARK)
#-------------------------------------------------------------------------------

cat("SECTION 3: Establishing Benchmark Statistics\n")
cat("--------------------------------------------\n\n")

# True statistics (before introducing missing)
true_stats <- ghs_work %>%
  summarise(
    n = n(),
    mean = mean(income_true),
    median = median(income_true),
    sd = sd(income_true),
    q25 = quantile(income_true, 0.25),
    q75 = quantile(income_true, 0.75),
    skewness = (mean(income_true) - median(income_true)) / sd(income_true)
  )

cat("TRUE INCOME STATISTICS (Full Data):\n")
cat("  N:", true_stats$n, "\n")
cat("  Mean: R", format(true_stats$mean, big.mark = ",", nsmall = 0), "\n")
cat("  Median: R", format(true_stats$median, big.mark = ",", nsmall = 0), "\n")
cat("  Std Dev: R", format(true_stats$sd, big.mark = ",", nsmall = 0), "\n")
cat("  IQR: R", format(true_stats$q25, big.mark = ","), "-", 
    format(true_stats$q75, big.mark = ","), "\n\n")

# Statistics using only observed (complete case)
observed_stats <- ghs_work %>%
  filter(!is_missing) %>%
  summarise(
    n = n(),
    mean = mean(income_observed),
    median = median(income_observed),
    sd = sd(income_observed)
  )

cat("OBSERVED CASE STATISTICS (Complete Cases Only):\n")
cat("  N:", observed_stats$n, "\n")
cat("  Mean: R", format(observed_stats$mean, big.mark = ",", nsmall = 0), "\n")
cat("  Median: R", format(observed_stats$median, big.mark = ",", nsmall = 0), "\n")
cat("  Std Dev: R", format(observed_stats$sd, big.mark = ",", nsmall = 0), "\n\n")

#-------------------------------------------------------------------------------
# 4. METHOD 1: MEAN IMPUTATION (THE BAD WAY)
#-------------------------------------------------------------------------------

cat("SECTION 4: Mean Imputation (The Bad Way)\n")
cat("----------------------------------------\n\n")

# Calculate overall mean from observed cases
mean_income <- mean(ghs_work$income_observed, na.rm = TRUE)

cat("Imputation Value (Overall Mean): R", format(mean_income, big.mark = ",", nsmall = 0), "\n\n")

# Perform mean imputation
ghs_work <- ghs_work %>%
  mutate(
    income_mean_imp = ifelse(is_missing, mean_income, income_observed)
  )

# Calculate post-imputation statistics
mean_imp_stats <- ghs_work %>%
  summarise(
    n = n(),
    mean = mean(income_mean_imp),
    median = median(income_mean_imp),
    sd = sd(income_mean_imp),
    q25 = quantile(income_mean_imp, 0.25),
    q75 = quantile(income_mean_imp, 0.75)
  )

cat("POST-MEAN IMPUTATION STATISTICS:\n")
cat("  N:", mean_imp_stats$n, "\n")
cat("  Mean: R", format(mean_imp_stats$mean, big.mark = ",", nsmall = 0), "\n")
cat("  Median: R", format(mean_imp_stats$median, big.mark = ",", nsmall = 0), "\n")
cat("  Std Dev: R", format(mean_imp_stats$sd, big.mark = ",", nsmall = 0), "\n\n")

# Calculate bias and distortion
cat("MEAN IMPUTATION PROBLEMS:\n")
cat("  Mean Bias: R", format(mean_imp_stats$mean - true_stats$mean, big.mark = ",", nsmall = 0), 
    "(", round(100 * (mean_imp_stats$mean - true_stats$mean) / true_stats$mean, 2), "%)\n")
cat("  Std Dev Change:", round(100 * (mean_imp_stats$sd - true_stats$sd) / true_stats$sd, 1), 
    "% (SHOULD BE ~0%)\n")
cat("  -> Variance is UNDERESTIMATED!\n\n")

#-------------------------------------------------------------------------------
# 5. METHOD 2: HOT-DECK IMPUTATION (THE GOOD WAY)
#-------------------------------------------------------------------------------

cat("SECTION 5: Hot-Deck Imputation (The Good Way)\n")
cat("---------------------------------------------\n\n")

# Define imputation classes: Province × Household Size Group
# HH Size groups: 1, 2-3, 4-5, 6+

ghs_work <- ghs_work %>%
  mutate(
    hhsize_grp = case_when(
      hholdsz == 1 ~ "1",
      hholdsz %in% 2:3 ~ "2-3",
      hholdsz %in% 4:5 ~ "4-5",
      hholdsz >= 6 ~ "6+",
      TRUE ~ "Unknown"
    ),
    # Create imputation class
    imp_class = paste(prov, hhsize_grp, sep = "_")
  )

# Check class sizes
class_summary <- ghs_work %>%
  group_by(imp_class) %>%
  summarise(
    n_total = n(),
    n_donors = sum(!is_missing),
    n_recipients = sum(is_missing),
    .groups = "drop"
  ) %>%
  arrange(n_donors)

cat("Imputation Classes (Province × HH Size):\n")
cat("  Total classes:", nrow(class_summary), "\n")
cat("  Classes with < 5 donors:", sum(class_summary$n_donors < 5), "\n")
cat("  Min donors in a class:", min(class_summary$n_donors), "\n")
cat("  Mean donors per class:", round(mean(class_summary$n_donors), 1), "\n\n")

# Show smallest classes (potential problem)
cat("Smallest Imputation Classes:\n")
print(head(class_summary, 10))
cat("\n")

# Perform Hot-Deck Imputation manually (for transparency)
# For each missing case, randomly select a donor from same class

ghs_work <- ghs_work %>%
  mutate(income_hotdeck = income_observed)  # Start with observed values

# Get indices of missing cases
missing_idx <- which(ghs_work$is_missing)

cat("Performing Hot-Deck Imputation for", length(missing_idx), "cases...\n")

# For each missing case
for (i in missing_idx) {
  # Get imputation class of this case
  this_class <- ghs_work$imp_class[i]
  
  # Find all donors (non-missing) in same class
  donors <- ghs_work %>%
    filter(imp_class == this_class, !is_missing) %>%
    pull(income_observed)
  
  # If no donors in exact class, use broader class (province only)
  if (length(donors) == 0) {
    this_prov <- ghs_work$prov[i]
    donors <- ghs_work %>%
      filter(prov == this_prov, !is_missing) %>%
      pull(income_observed)
  }
  
  # Randomly select one donor value
  if (length(donors) > 0) {
    ghs_work$income_hotdeck[i] <- sample(donors, 1)
  } else {
    # Fallback: use overall mean (rare)
    ghs_work$income_hotdeck[i] <- mean_income
  }
}

cat("Hot-Deck Imputation Complete.\n\n")

# Calculate post-imputation statistics
hotdeck_stats <- ghs_work %>%
  summarise(
    n = n(),
    mean = mean(income_hotdeck),
    median = median(income_hotdeck),
    sd = sd(income_hotdeck),
    q25 = quantile(income_hotdeck, 0.25),
    q75 = quantile(income_hotdeck, 0.75)
  )

cat("POST-HOT-DECK IMPUTATION STATISTICS:\n")
cat("  N:", hotdeck_stats$n, "\n")
cat("  Mean: R", format(hotdeck_stats$mean, big.mark = ",", nsmall = 0), "\n")
cat("  Median: R", format(hotdeck_stats$median, big.mark = ",", nsmall = 0), "\n")
cat("  Std Dev: R", format(hotdeck_stats$sd, big.mark = ",", nsmall = 0), "\n\n")

cat("HOT-DECK PERFORMANCE:\n")
cat("  Mean Bias: R", format(hotdeck_stats$mean - true_stats$mean, big.mark = ",", nsmall = 0),
    "(", round(100 * (hotdeck_stats$mean - true_stats$mean) / true_stats$mean, 2), "%)\n")
cat("  Std Dev Change:", round(100 * (hotdeck_stats$sd - true_stats$sd) / true_stats$sd, 1), 
    "% (SHOULD BE ~0%)\n")
cat("  -> Variance is PRESERVED!\n\n")

#-------------------------------------------------------------------------------
# 6. COMPARISON TABLE
#-------------------------------------------------------------------------------

cat("================================================================\n")
cat("SECTION 6: Comparison of Methods\n")
cat("================================================================\n\n")

comparison_table <- data.frame(
  Method = c("True (Full Data)", "Complete Cases Only", "Mean Imputation", "Hot-Deck Imputation"),
  N = c(true_stats$n, observed_stats$n, mean_imp_stats$n, hotdeck_stats$n),
  Mean = c(true_stats$mean, observed_stats$mean, mean_imp_stats$mean, hotdeck_stats$mean),
  Median = c(true_stats$median, observed_stats$median, mean_imp_stats$median, hotdeck_stats$median),
  StdDev = c(true_stats$sd, observed_stats$sd, mean_imp_stats$sd, hotdeck_stats$sd)
) %>%
  mutate(
    Mean_Bias = Mean - true_stats$mean,
    Mean_Bias_Pct = round(100 * (Mean - true_stats$mean) / true_stats$mean, 2),
    SD_Change_Pct = round(100 * (StdDev - true_stats$sd) / true_stats$sd, 1)
  )

cat("COMPARISON TABLE:\n")
print(comparison_table)
cat("\n")

#-------------------------------------------------------------------------------
# 7. DENSITY PLOTS - VISUAL COMPARISON
#-------------------------------------------------------------------------------

cat("SECTION 7: Creating Density Plots\n")
cat("---------------------------------\n\n")

# Prepare data for density plots
# Limit to reasonable income range for visualization (exclude extreme outliers)
income_cap <- quantile(ghs_work$income_true, 0.99, na.rm = TRUE)

plot_data <- ghs_work %>%
  filter(income_true <= income_cap) %>%
  select(income_true, income_mean_imp, income_hotdeck) %>%
  mutate(
    income_mean_imp = pmin(income_mean_imp, income_cap),
    income_hotdeck = pmin(income_hotdeck, income_cap)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "method",
    values_to = "income"
  ) %>%
  mutate(
    method = case_when(
      method == "income_true" ~ "Original (True)",
      method == "income_mean_imp" ~ "Mean Imputation",
      method == "income_hotdeck" ~ "Hot-Deck Imputation"
    ),
    method = factor(method, levels = c("Original (True)", "Mean Imputation", "Hot-Deck Imputation"))
  )

# Combined density plot
p_combined <- ggplot(plot_data, aes(x = income, color = method, linetype = method)) +
  geom_density(linewidth = 1.2, adjust = 1.5) +
  scale_color_manual(
    values = c("Original (True)" = "#003366", 
               "Mean Imputation" = "#DC3545", 
               "Hot-Deck Imputation" = "#28A745")
  ) +
  scale_linetype_manual(
    values = c("Original (True)" = "solid", 
               "Mean Imputation" = "dashed", 
               "Hot-Deck Imputation" = "dotdash")
  ) +
  scale_x_continuous(labels = scales::comma, limits = c(0, income_cap)) +
  labs(
    title = "Income Distribution: Original vs Imputation Methods",
    subtitle = paste0("MAR Missing Mechanism: ", pct_total_missing, "% Missing (N = ", 
                      format(total_n, big.mark = ","), ")"),
    x = "Monthly Household Income (R)",
    y = "Density",
    color = "Method",
    linetype = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold")
  )

# Save combined plot
ggsave("lab4_2_density_comparison.png", p_combined, width = 10, height = 6, dpi = 150)
cat("Saved: lab4_2_density_comparison.png\n")

# Faceted density plot
p_facet <- ggplot(plot_data, aes(x = income, fill = method)) +
  geom_density(alpha = 0.7, adjust = 1.5) +
  facet_wrap(~method, ncol = 1) +
  scale_fill_manual(
    values = c("Original (True)" = "#003366", 
               "Mean Imputation" = "#DC3545", 
               "Hot-Deck Imputation" = "#28A745")
  ) +
  scale_x_continuous(labels = scales::comma, limits = c(0, income_cap)) +
  labs(
    title = "Income Distributions by Imputation Method",
    x = "Monthly Household Income (R)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave("lab4_2_density_facet.png", p_facet, width = 10, height = 8, dpi = 150)
cat("Saved: lab4_2_density_facet.png\n")

# Zoomed plot showing mean imputation spike
mean_income_rounded <- round(mean_income, -2)  # Round to nearest 100

p_zoom <- ggplot(plot_data, aes(x = income, color = method, linetype = method)) +
  geom_density(linewidth = 1.2, adjust = 0.8) +
  geom_vline(xintercept = mean_income, color = "#DC3545", linetype = "dotted", linewidth = 1) +
  annotate("text", x = mean_income + 2000, y = 0.00008, 
           label = paste0("Mean = R ", format(round(mean_income), big.mark = ",")),
           color = "#DC3545", hjust = 0, size = 3.5) +
  scale_color_manual(
    values = c("Original (True)" = "#003366", 
               "Mean Imputation" = "#DC3545", 
               "Hot-Deck Imputation" = "#28A745")
  ) +
  scale_linetype_manual(
    values = c("Original (True)" = "solid", 
               "Mean Imputation" = "dashed", 
               "Hot-Deck Imputation" = "dotdash")
  ) +
  scale_x_continuous(labels = scales::comma, 
                     limits = c(mean_income - 15000, mean_income + 15000)) +
  labs(
    title = "Zoomed View: Mean Imputation Creates Artificial Spike",
    subtitle = "Notice the spike at the imputed mean value",
    x = "Monthly Household Income (R)",
    y = "Density",
    color = "Method",
    linetype = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("lab4_2_density_zoom.png", p_zoom, width = 10, height = 6, dpi = 150)
cat("Saved: lab4_2_density_zoom.png\n\n")

#-------------------------------------------------------------------------------
# 8. ADDITIONAL DIAGNOSTICS
#-------------------------------------------------------------------------------

cat("SECTION 8: Additional Diagnostics\n")
cat("----------------------------------\n\n")

# Check imputed values distribution
imputed_check <- ghs_work %>%
  filter(is_missing) %>%
  summarise(
    n = n(),
    true_mean = mean(income_true),
    mean_imp_value = mean(income_mean_imp),
    hotdeck_mean = mean(income_hotdeck),
    true_sd = sd(income_true),
    mean_imp_sd = sd(income_mean_imp),
    hotdeck_sd = sd(income_hotdeck)
  )

cat("IMPUTED VALUES ONLY (for missing cases):\n")
cat("  N imputed:", imputed_check$n, "\n")
cat("  True Mean (hidden): R", format(imputed_check$true_mean, big.mark = ",", nsmall = 0), "\n")
cat("  Mean Imp Value: R", format(imputed_check$mean_imp_value, big.mark = ",", nsmall = 0), "\n")
cat("  Hot-Deck Mean: R", format(imputed_check$hotdeck_mean, big.mark = ",", nsmall = 0), "\n")
cat("  True SD: R", format(imputed_check$true_sd, big.mark = ",", nsmall = 0), "\n")
cat("  Mean Imp SD: R", format(imputed_check$mean_imp_sd, big.mark = ",", nsmall = 0), 
    "(All same value!)\n")
cat("  Hot-Deck SD: R", format(imputed_check$hotdeck_sd, big.mark = ",", nsmall = 0), "\n\n")

# Correlation with true values
cat("CORRELATION WITH TRUE VALUES (for imputed cases):\n")
imputed_cases <- ghs_work %>% filter(is_missing)
cat("  Mean Imputation: ", round(cor(imputed_cases$income_true, 
                                     imputed_cases$income_mean_imp), 4), 
    "(always 0 since constant)\n")
cat("  Hot-Deck: ", round(cor(imputed_cases$income_true, 
                              imputed_cases$income_hotdeck), 4), "\n\n")

#-------------------------------------------------------------------------------
# 9. SUMMARY AND KEY TAKEAWAYS
#-------------------------------------------------------------------------------

cat("================================================================\n")
cat("SECTION 9: Summary and Key Takeaways\n")
cat("================================================================\n\n")

cat("KEY FINDINGS:\n")
cat("-------------\n\n")

cat("1. MEAN IMPUTATION PROBLEMS:\n")
cat("   - Underestimates variance by", 
    abs(round(100 * (mean_imp_stats$sd - true_stats$sd) / true_stats$sd, 1)), "%\n")
cat("   - Creates artificial spike at imputed value\n")
cat("   - All imputed values are identical\n")
cat("   - Distorts distribution shape\n\n")

cat("2. HOT-DECK ADVANTAGES:\n")
cat("   - Preserves variance (change:", 
    round(100 * (hotdeck_stats$sd - true_stats$sd) / true_stats$sd, 1), "%)\n")
cat("   - Uses real observed values as donors\n")
cat("   - Maintains distribution shape\n")
cat("   - Imputed values have natural variability\n\n")

cat("3. RECOMMENDATIONS:\n")
cat("   - NEVER use mean imputation for continuous variables\n")
cat("   - Hot-deck is appropriate for item non-response\n")
cat("   - Choose imputation classes based on predictive variables\n")
cat("   - Document imputation method in survey metadata\n")
cat("   - Consider multiple imputation for variance estimation\n\n")

#-------------------------------------------------------------------------------
# 10. EXPORT RESULTS
#-------------------------------------------------------------------------------

cat("SECTION 10: Exporting Results\n")
cat("-----------------------------\n\n")

# Save comparison table
write.csv(comparison_table, "lab4_2_imputation_comparison.csv", row.names = FALSE)
cat("Saved: lab4_2_imputation_comparison.csv\n")

# Save detailed results
results_list <- list(
  true_stats = true_stats,
  mean_imp_stats = mean_imp_stats,
  hotdeck_stats = hotdeck_stats,
  missing_summary = missing_summary,
  comparison_table = comparison_table
)

saveRDS(results_list, "lab4_2_results.rds")
cat("Saved: lab4_2_results.rds\n\n")

# Export key statistics for slides
slide_stats <- data.frame(
  Metric = c("N Total", "N Missing", "Pct Missing",
             "True Mean", "True SD",
             "Mean Imp Mean", "Mean Imp SD", "Mean Imp SD Change",
             "HotDeck Mean", "HotDeck SD", "HotDeck SD Change"),
  Value = c(total_n, total_missing, pct_total_missing,
            round(true_stats$mean), round(true_stats$sd),
            round(mean_imp_stats$mean), round(mean_imp_stats$sd),
            round(100 * (mean_imp_stats$sd - true_stats$sd) / true_stats$sd, 1),
            round(hotdeck_stats$mean), round(hotdeck_stats$sd),
            round(100 * (hotdeck_stats$sd - true_stats$sd) / true_stats$sd, 1))
)

write.csv(slide_stats, "lab4_2_slide_statistics.csv", row.names = FALSE)
cat("Saved: lab4_2_slide_statistics.csv\n\n")

cat("================================================================\n")
cat("  LAB 4.2 COMPLETE\n")
cat("================================================================\n")

#===============================================================================
# END OF LAB 4.2
#===============================================================================
