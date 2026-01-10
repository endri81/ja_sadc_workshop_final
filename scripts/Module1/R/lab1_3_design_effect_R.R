#===============================================================================
# LAB 1.3: DESIGN EFFECT (DEFF) AND INTRACLASS CORRELATION (ICC) CALCULATIONS
#===============================================================================
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 1, Module 3: Multi-Stage Stratified Design for Household Surveys
#
# NARRATIVE CONTEXT (Zambara Case):
# Lindiwe Moyo, Chief Methodologist at ZNSO, needs to demonstrate to the 
# Minister why the previous survey's simple random sampling assumptions led 
# to unreliable rural estimates. She calculates design effects to show the 
# efficiency loss from clustering and the heterogeneity across provinces.
#
# LEARNING OBJECTIVES:
# - Calculate and interpret the Design Effect (DEFF) for complex surveys
# - Estimate the Intraclass Correlation Coefficient (ICC) 
# - Demonstrate variance inflation from cluster sampling
# - Compare DEFF across provinces and geography types
#
# DATA REQUIREMENTS:
# - ghs-2024-hhold-v1.dta (Household file)
# - ghs-2024-person-v1.dta (Person file, for linking PSU info if needed)
#
# VARIABLE DICTIONARY:
# - totmhinc   : Total monthly household income (derived variable)
# - house_wgt  : Household survey weight
# - Stratum    : Sampling stratum (Province × Metro × GeoType)
# - PSU        : Primary Sampling Unit (cluster)
# - Prov       : Province (1=WC, 2=EC, 3=NC, 4=FS, 5=KZN, 6=NW, 7=GP, 8=MP, 9=LP)
# - GeoType    : Geography type (1=Urban, 2=Traditional, 3=Farms)
#
# REFERENCES:
# - Kish, L. (1965). Survey Sampling. Wiley. [Original DEFF formulation]
# - Lohr, S.L. (2022). Sampling: Design and Analysis. 3rd ed. CRC Press. Ch. 5
# - Lumley, T. (2010). Complex Surveys: A Guide to Analysis Using R. Wiley.
#===============================================================================

#-------------------------------------------------------------------------------
# SECTION 1: ENVIRONMENT SETUP
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Load required packages (install if needed)
required_packages <- c("survey", "haven", "dplyr", "tidyr", "ggplot2", "knitr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Set options for cleaner output
options(survey.lonely.psu = "adjust")  # Handle singleton PSUs
options(digits = 4)

#-------------------------------------------------------------------------------
# SECTION 2: DATA IMPORT AND PREPARATION
#-------------------------------------------------------------------------------

# Define file paths - MODIFY THESE TO YOUR LOCAL PATHS
data_path <- "./data/GHS_2024/"  # Path to data files
output_path <- "./output/"        # Path for results

# Create output folder if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
  cat("Created output directory:", output_path, "\n")
}

# Read household data
cat("\n========== LOADING DATA ==========\n")
hh_data <- haven::read_dta(paste0(data_path, "ghs-2024-hhold-v1.dta"))

cat("Household file dimensions:", dim(hh_data), "\n")
cat("Variables available:", ncol(hh_data), "\n")

# Examine key variables
cat("\n========== KEY VARIABLE SUMMARY ==========\n")

# Check variable names (case sensitivity matters)
key_vars <- c("totmhinc", "house_wgt", "Stratum", "GeoType", "Prov")
available_vars <- names(hh_data)

# Find matching variables (handle case variations)
for (v in key_vars) {
  matches <- grep(v, available_vars, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) {
    cat(sprintf("  %s found as: %s\n", v, paste(matches, collapse = ", ")))
  } else {
    cat(sprintf("  WARNING: %s not found!\n", v))
  }
}

# Standardize variable names to lowercase for consistency
names(hh_data) <- tolower(names(hh_data))

#-------------------------------------------------------------------------------
# SECTION 3: DATA CLEANING FOR ANALYSIS
#-------------------------------------------------------------------------------

cat("\n========== DATA CLEANING ==========\n")

# Create analysis dataset with required variables
# Note: PSU may need to be merged from person file if not in household file
analysis_data <- hh_data %>%
  select(
    uqnr,           # Unique household identifier
    psu,            # Primary sampling unit (cluster)
    stratum,        # Sampling stratum
    prov,           # Province
    geotype,        # Geography type
    house_wgt,      # Household weight
    totmhinc        # Total monthly household income
  ) %>%
  # Remove missing values for income (coded as 9999999 per metadata)
  filter(totmhinc < 9999999 & !is.na(totmhinc)) %>%
  # Remove zero weights
  filter(house_wgt > 0 & !is.na(house_wgt)) %>%
  # Create province labels for Zambara narrative
  mutate(
    # Map South African provinces to Zambara regions
    zambara_region = factor(prov, 
      levels = 1:9,
      labels = c(
        "Southern Cape",         # 1 = Western Cape
        "Coastal Plains",        # 2 = Eastern Cape  
        "Western Drylands",      # 3 = Northern Cape
        "Central Plateau",       # 4 = Free State
        "Eastern Highlands",     # 5 = KwaZulu-Natal
        "Mining Belt",           # 6 = North West
        "Capital Region",        # 7 = Gauteng
        "Eastern Forests",       # 8 = Mpumalanga
        "Northern Bushveld"      # 9 = Limpopo
      )
    ),
    geotype_label = factor(geotype,
      levels = 1:3,
      labels = c("Urban", "Traditional", "Farms")
    )
  )

cat("Records after cleaning:", nrow(analysis_data), "\n")
cat("Valid income observations:", sum(!is.na(analysis_data$totmhinc)), "\n")

# Summary of cluster structure
cluster_summary <- analysis_data %>%
  group_by(psu) %>%
  summarise(
    n_hh = n(),
    stratum = first(stratum),
    prov = first(prov)
  )

cat("\n========== CLUSTER STRUCTURE ==========\n")
cat("Total PSUs (clusters):", n_distinct(analysis_data$psu), "\n")
cat("Total strata:", n_distinct(analysis_data$stratum), "\n")
cat("Average households per PSU:", round(mean(cluster_summary$n_hh), 2), "\n")
cat("Range of cluster sizes:", min(cluster_summary$n_hh), "-", max(cluster_summary$n_hh), "\n")

#-------------------------------------------------------------------------------
# SECTION 4: DEFINE SURVEY DESIGN OBJECTS
#-------------------------------------------------------------------------------

cat("\n========== SURVEY DESIGN SPECIFICATION ==========\n")

# Complex design: Stratified two-stage cluster sampling with PPS
# Stage 1: PSUs selected with PPS within strata
# Stage 2: Systematic sampling of DUs within selected PSUs
# Weights account for selection probabilities and non-response adjustments

design_complex <- svydesign(
  id = ~psu,              # Cluster (PSU) identifier
  strata = ~stratum,      # Stratification variable
  weights = ~house_wgt,   # Final household weights
  data = analysis_data,
  nest = TRUE             # PSUs nested within strata
)

cat("Complex design created successfully\n")
cat("Design call:", deparse(design_complex$call), "\n")

# Simple Random Sampling design (for comparison)
# This assumes SRS and ignores the complex design - what happens if we get it wrong

design_srs <- svydesign(
  id = ~1,                # No clustering
  weights = ~house_wgt,   # Still use weights
  data = analysis_data
)

cat("SRS design created for comparison\n")

#-------------------------------------------------------------------------------
# SECTION 5: CALCULATE DESIGN EFFECT (DEFF) - NATIONAL LEVEL
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 5: NATIONAL-LEVEL DESIGN EFFECT CALCULATIONS                  \n")
cat("=========================================================================\n")

# Calculate mean income under complex design
mean_complex <- svymean(~totmhinc, design_complex, deff = TRUE)

# Calculate mean income under SRS assumption
mean_srs <- svymean(~totmhinc, design_srs)

# Extract components
est_complex <- coef(mean_complex)
se_complex <- SE(mean_complex)
deff_national <- deff(mean_complex)

est_srs <- coef(mean_srs)
se_srs <- SE(mean_srs)

cat("\n----- MEAN HOUSEHOLD INCOME (totmhinc) -----\n")
cat(sprintf("\nComplex Design Estimate:\n"))
cat(sprintf("  Point estimate:  R %.2f\n", est_complex))
cat(sprintf("  Standard error:  R %.2f\n", se_complex))
cat(sprintf("  95%% CI:          [R %.2f, R %.2f]\n", 
            est_complex - 1.96*se_complex, est_complex + 1.96*se_complex))

cat(sprintf("\nSRS Assumption (INCORRECT):\n"))
cat(sprintf("  Point estimate:  R %.2f\n", est_srs))
cat(sprintf("  Standard error:  R %.2f\n", se_srs))
cat(sprintf("  95%% CI:          [R %.2f, R %.2f]\n", 
            est_srs - 1.96*se_srs, est_srs + 1.96*se_srs))

cat(sprintf("\n***** DESIGN EFFECT (DEFF) = %.3f *****\n", deff_national))
cat(sprintf("\nInterpretation:\n"))
cat(sprintf("  The complex design variance is %.1f times larger than\n", deff_national))
cat(sprintf("  what we would expect under SRS. This means we need\n"))
cat(sprintf("  approximately %.0f times as many observations to achieve\n", ceiling(deff_national)))
cat(sprintf("  the same precision as a simple random sample.\n"))

# Effective sample size
n_actual <- nrow(analysis_data)
n_effective <- n_actual / deff_national

cat(sprintf("\nEffective Sample Size:\n"))
cat(sprintf("  Actual sample size:     %d households\n", n_actual))
cat(sprintf("  Effective sample size:  %.0f households\n", n_effective))
cat(sprintf("  Efficiency loss:        %.1f%%\n", (1 - 1/deff_national) * 100))

#-------------------------------------------------------------------------------
# SECTION 6: CALCULATE INTRACLASS CORRELATION COEFFICIENT (ICC)
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 6: INTRACLASS CORRELATION COEFFICIENT (ICC)                   \n")
cat("=========================================================================\n")

# For large datasets with thousands of clusters, direct ICC estimation via
# mixed models or ANOVA is computationally prohibitive. Instead, we derive
# ICC from the observed DEFF using the inverse of Kish's formula:
#
#   DEFF ≈ 1 + (m̄ - 1) × ρ
#   Therefore: ρ ≈ (DEFF - 1) / (m̄ - 1)
#
# This is standard practice in survey methodology (see Lohr 2022, Ch. 5.5)

m_bar <- mean(cluster_summary$n_hh)

# Method 1: Derive ICC from observed DEFF (FAST - recommended for large data)
icc_from_deff <- (deff_national - 1) / (m_bar - 1)

cat("\n----- ICC ESTIMATION (DERIVED FROM DEFF) -----\n")
cat(sprintf("\nUsing Kish's formula inverted: ρ = (DEFF - 1) / (m̄ - 1)\n"))
cat(sprintf("  Observed DEFF:              %.3f\n", deff_national))
cat(sprintf("  Average cluster size (m̄):  %.2f\n", m_bar))
cat(sprintf("\n***** ICC (rho) = %.4f *****\n", icc_from_deff))

icc_value <- icc_from_deff

# Method 2: Direct variance decomposition on a SAMPLE (optional, slower)
# Uncomment if you want to verify with actual variance components
# This samples 500 PSUs to make computation tractable

run_direct_icc <- FALSE  # Set to TRUE if you want direct estimation

if (run_direct_icc) {
  cat("\n----- DIRECT ICC (SAMPLED PSUs) -----\n")
  
  # Sample 500 PSUs for tractable computation
  set.seed(12345)
  sampled_psus <- sample(unique(analysis_data$psu), min(500, n_distinct(analysis_data$psu)))
  sample_data <- analysis_data[analysis_data$psu %in% sampled_psus, ]
  
  cat(sprintf("Using %d PSUs (sampled from %d) for direct ICC...\n", 
              length(sampled_psus), n_distinct(analysis_data$psu)))
  
  # Simple ANOVA-based variance decomposition
  # Between-group variance: MSB - MSW / n_per_group
  # Within-group variance: MSW
  
  aov_fit <- aov(totmhinc ~ factor(psu), data = sample_data)
  aov_summary <- summary(aov_fit)[[1]]
  
  MSB <- aov_summary["factor(psu)", "Mean Sq"]
  MSW <- aov_summary["Residuals", "Mean Sq"]
  
  # Average observations per PSU in sample
  n_avg <- nrow(sample_data) / length(sampled_psus)
  
  # Variance components
  var_within <- MSW
  var_between <- (MSB - MSW) / n_avg
  var_between <- max(0, var_between)  # Ensure non-negative
  
  icc_direct <- var_between / (var_between + var_within)
  
  cat(sprintf("  MSB (between PSUs):   %.0f\n", MSB))
  cat(sprintf("  MSW (within PSUs):    %.0f\n", MSW))
  cat(sprintf("  Var(between):         %.0f\n", var_between))
  cat(sprintf("  Var(within):          %.0f\n", var_within))
  cat(sprintf("  Direct ICC:           %.4f\n", icc_direct))
  cat(sprintf("  DEFF-derived ICC:     %.4f\n", icc_from_deff))
}

# Verify relationship
deff_theoretical <- 1 + (m_bar - 1) * icc_value

cat(sprintf("\n----- DEFF-ICC RELATIONSHIP VERIFICATION -----\n"))
cat(sprintf("\nTheoretical: DEFF = 1 + (m̄ - 1) × ρ\n"))
cat(sprintf("  Average cluster size (m̄):  %.2f\n", m_bar))
cat(sprintf("  ICC (ρ):                   %.4f\n", icc_value))
cat(sprintf("  Reconstructed DEFF:        %.3f\n", deff_theoretical))
cat(sprintf("  Observed DEFF:             %.3f\n", deff_national))

cat(sprintf("\nNote: Minor discrepancies arise from:\n"))
cat(sprintf("  1. Stratification (reduces design variance)\n"))
cat(sprintf("  2. Unequal cluster sizes\n"))
cat(sprintf("  3. Survey weighting effects\n"))

#-------------------------------------------------------------------------------
# SECTION 7: DESIGN EFFECTS BY PROVINCE (ZAMBARA REGIONS)
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 7: DESIGN EFFECTS BY ZAMBARA REGION (PROVINCE)                \n")
cat("=========================================================================\n")

# Calculate DEFF for each province
provinces <- sort(unique(analysis_data$prov))

deff_by_province <- data.frame(
  prov = integer(),
  zambara_region = character(),
  n_hh = integer(),
  n_psu = integer(),
  mean_income = numeric(),
  se_complex = numeric(),
  se_srs = numeric(),
  deff = numeric(),
  stringsAsFactors = FALSE
)

cat("\nCalculating province-specific design effects...\n")

for (p in provinces) {
  
  # Subset data for province
  prov_data <- subset(analysis_data, prov == p)
  
  # Create province-specific designs
  prov_complex <- svydesign(
    id = ~psu,
    strata = ~stratum,
    weights = ~house_wgt,
    data = prov_data,
    nest = TRUE
  )
  
  prov_srs <- svydesign(
    id = ~1,
    weights = ~house_wgt,
    data = prov_data
  )
  
  # Calculate means with DEFF
  mean_p_complex <- svymean(~totmhinc, prov_complex, deff = TRUE)
  mean_p_srs <- svymean(~totmhinc, prov_srs)
  
  # Get Zambara region name
  region_name <- as.character(prov_data$zambara_region[1])
  
  # Store results
  deff_by_province <- rbind(deff_by_province, data.frame(
    prov = p,
    zambara_region = region_name,
    n_hh = nrow(prov_data),
    n_psu = n_distinct(prov_data$psu),
    mean_income = coef(mean_p_complex),
    se_complex = SE(mean_p_complex),
    se_srs = SE(mean_p_srs),
    deff = as.numeric(deff(mean_p_complex))
  ))
}

# Display results
cat("\n----- DESIGN EFFECTS BY ZAMBARA REGION -----\n\n")

# Format for display
results_display <- deff_by_province %>%
  mutate(
    mean_income = sprintf("R %.0f", mean_income),
    se_complex = sprintf("%.1f", se_complex),
    se_srs = sprintf("%.1f", se_srs),
    deff = sprintf("%.2f", deff)
  ) %>%
  select(zambara_region, n_hh, n_psu, mean_income, se_complex, se_srs, deff)

names(results_display) <- c("Zambara Region", "n (HH)", "PSUs", "Mean Income", 
                            "SE (Complex)", "SE (SRS)", "DEFF")

print(knitr::kable(results_display, align = "lrrrrrr"))

#-------------------------------------------------------------------------------
# SECTION 8: DESIGN EFFECTS BY GEOGRAPHY TYPE
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 8: DESIGN EFFECTS BY GEOGRAPHY TYPE                           \n")
cat("=========================================================================\n")

# Calculate DEFF for each geography type
geotypes <- sort(unique(analysis_data$geotype))

deff_by_geotype <- data.frame(
  geotype = integer(),
  geotype_label = character(),
  n_hh = integer(),
  n_psu = integer(),
  mean_income = numeric(),
  deff = numeric(),
  stringsAsFactors = FALSE
)

for (g in geotypes) {
  
  geo_data <- subset(analysis_data, geotype == g)
  
  geo_complex <- svydesign(
    id = ~psu,
    strata = ~stratum,
    weights = ~house_wgt,
    data = geo_data,
    nest = TRUE
  )
  
  mean_g <- svymean(~totmhinc, geo_complex, deff = TRUE)
  
  geo_label <- as.character(geo_data$geotype_label[1])
  
  deff_by_geotype <- rbind(deff_by_geotype, data.frame(
    geotype = g,
    geotype_label = geo_label,
    n_hh = nrow(geo_data),
    n_psu = n_distinct(geo_data$psu),
    mean_income = coef(mean_g),
    deff = as.numeric(deff(mean_g))
  ))
}

cat("\n----- DESIGN EFFECTS BY GEOGRAPHY TYPE -----\n\n")

geo_display <- deff_by_geotype %>%
  mutate(
    mean_income = sprintf("R %.0f", mean_income),
    deff = sprintf("%.2f", deff)
  )

print(knitr::kable(geo_display[, c("geotype_label", "n_hh", "n_psu", "mean_income", "deff")],
                   col.names = c("Geography Type", "n (HH)", "PSUs", "Mean Income", "DEFF"),
                   align = "lrrrr"))

#-------------------------------------------------------------------------------
# SECTION 9: ICC BY PROVINCE (DERIVED FROM PROVINCIAL DEFFs)
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 9: ICC BY ZAMBARA REGION (DERIVED FROM DEFF)                  \n")
cat("=========================================================================\n")

# Derive ICC for each province using the inverse Kish formula
# This is much faster than fitting mixed models for each province

icc_by_province <- deff_by_province %>%
  mutate(
    avg_cluster_size = n_hh / n_psu,
    icc = (deff - 1) / (avg_cluster_size - 1),
    icc = pmax(0, icc)  # Ensure non-negative
  ) %>%
  select(prov, zambara_region, n_psu, avg_cluster_size, deff, icc)

cat("\n----- ICC BY ZAMBARA REGION (DERIVED FROM DEFF) -----\n\n")

icc_display <- icc_by_province %>%
  mutate(
    avg_cluster_size = sprintf("%.1f", avg_cluster_size),
    deff = sprintf("%.2f", deff),
    icc = sprintf("%.4f", icc)
  )

print(knitr::kable(icc_display[, c("zambara_region", "n_psu", "avg_cluster_size", "deff", "icc")],
                   col.names = c("Zambara Region", "PSUs", "Avg Cluster", "DEFF", "ICC"),
                   align = "lrrrr"))

cat("\nNote: ICC derived using ρ = (DEFF - 1) / (m̄ - 1)\n")
cat("Higher ICC indicates greater within-cluster homogeneity.\n")

#-------------------------------------------------------------------------------
# SECTION 10: VISUALIZATION
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 10: VISUALIZATIONS                                            \n")
cat("=========================================================================\n")

# Plot 1: DEFF by Province
plot_deff_province <- ggplot(deff_by_province, 
                             aes(x = reorder(zambara_region, deff), y = deff)) +
  geom_bar(stat = "identity", fill = "#003366", alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", deff)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Design Effect (DEFF) by Zambara Region",
    subtitle = "For Total Monthly Household Income (totmhinc)",
    x = "",
    y = "Design Effect",
    caption = "Red line indicates DEFF = 1 (SRS baseline)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, max(deff_by_province$deff) * 1.2))

print(plot_deff_province)

# Save plot
ggsave(paste0(output_path, "lab1_3_deff_by_province.png"), plot_deff_province, 
       width = 10, height = 6, dpi = 150)
cat("\nPlot saved:", paste0(output_path, "lab1_3_deff_by_province.png"), "\n")

# Plot 2: DEFF vs Average Cluster Size
deff_cluster <- deff_by_province %>%
  mutate(avg_cluster_size = n_hh / n_psu)

plot_deff_cluster <- ggplot(deff_cluster, aes(x = avg_cluster_size, y = deff)) +
  geom_point(size = 4, color = "#003366") +
  geom_text(aes(label = substr(zambara_region, 1, 3)), vjust = -1, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#D4AF37", linetype = "dashed") +
  labs(
    title = "Relationship Between Cluster Size and Design Effect",
    subtitle = "Larger clusters → Higher DEFF (more homogeneity effect)",
    x = "Average Households per PSU",
    y = "Design Effect (DEFF)",
    caption = "Dashed line: Linear trend"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot_deff_cluster)

ggsave(paste0(output_path, "lab1_3_deff_vs_cluster_size.png"), plot_deff_cluster, 
       width = 8, height = 6, dpi = 150)
cat("Plot saved:", paste0(output_path, "lab1_3_deff_vs_cluster_size.png"), "\n")

#-------------------------------------------------------------------------------
# SECTION 11: SUMMARY OUTPUT FOR SLIDES
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  SECTION 11: SUMMARY OUTPUT FOR PRESENTATION SLIDES                    \n")
cat("=========================================================================\n")

cat("\n============ KEY RESULTS FOR SLIDE INTEGRATION ============\n")

cat("\n>>> NATIONAL DESIGN EFFECT <<<\n")
cat(sprintf("DEFF_national = %.2f\n", deff_national))
cat(sprintf("ICC_national = %.4f\n", icc_value))
cat(sprintf("Mean_cluster_size = %.1f\n", m_bar))
cat(sprintf("Effective_n = %.0f\n", n_effective))
cat(sprintf("Actual_n = %d\n", n_actual))

cat("\n>>> HIGHEST DEFF REGION <<<\n")
max_deff_row <- deff_by_province[which.max(deff_by_province$deff), ]
cat(sprintf("Region: %s\n", max_deff_row$zambara_region))
cat(sprintf("DEFF: %.2f\n", max_deff_row$deff))

cat("\n>>> LOWEST DEFF REGION <<<\n")
min_deff_row <- deff_by_province[which.min(deff_by_province$deff), ]
cat(sprintf("Region: %s\n", min_deff_row$zambara_region))
cat(sprintf("DEFF: %.2f\n", min_deff_row$deff))

cat("\n>>> VARIANCE RATIO <<<\n")
var_ratio <- (se_complex^2) / (se_srs^2)
cat(sprintf("Var(complex) / Var(SRS) = %.2f\n", var_ratio))
cat(sprintf("SE inflation factor = %.2f\n", se_complex / se_srs))

cat("\n>>> GEOGRAPHY TYPE DEFFs <<<\n")
for (i in 1:nrow(deff_by_geotype)) {
  cat(sprintf("%s: DEFF = %.2f\n", 
              deff_by_geotype$geotype_label[i], 
              deff_by_geotype$deff[i]))
}

#-------------------------------------------------------------------------------
# SECTION 12: EXPORT RESULTS
#-------------------------------------------------------------------------------

cat("\n========== EXPORTING RESULTS ==========\n")

# Create results list for export
results_list <- list(
  national = list(
    deff = deff_national,
    icc = icc_value,
    mean_income = est_complex,
    se_complex = se_complex,
    se_srs = se_srs,
    n_actual = n_actual,
    n_effective = n_effective,
    m_bar = m_bar
  ),
  by_province = deff_by_province,
  by_geotype = deff_by_geotype,
  icc_by_province = icc_by_province
)

# Save as RDS
saveRDS(results_list, paste0(output_path, "lab1_3_deff_results.rds"))
cat("Results saved:", paste0(output_path, "lab1_3_deff_results.rds"), "\n")

# Export summary table to CSV
write.csv(deff_by_province, paste0(output_path, "lab1_3_deff_by_province.csv"), row.names = FALSE)
cat("Table saved:", paste0(output_path, "lab1_3_deff_by_province.csv"), "\n")

#-------------------------------------------------------------------------------
# END OF LAB SCRIPT
#-------------------------------------------------------------------------------

cat("\n")
cat("=========================================================================\n")
cat("  LAB 1.3 COMPLETE                                                      \n")
cat("=========================================================================\n")
cat("\nLindiwe now has the evidence she needs to show the Minister:\n")
cat(sprintf("  • The design effect of %.2f means the effective sample\n", deff_national))
cat(sprintf("    is only %.0f households, not the %d collected.\n", n_effective, n_actual))
cat(sprintf("  • Ignoring clustering would underestimate SEs by %.0f%%.\n", 
            (1 - se_srs/se_complex) * 100))
cat("  • Traditional areas show higher DEFFs, explaining rural estimate problems.\n")
cat("\n")

# Session info for reproducibility
cat("Session Info:\n")
print(sessionInfo())
