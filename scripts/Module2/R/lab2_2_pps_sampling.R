#===============================================================================
# LAB 2.2: PPS SAMPLING SELECTION
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 2: Computational Tools and Spatial Integration
#===============================================================================
#
# OBJECTIVE:
# Demonstrate Probability Proportional to Size (PPS) systematic sampling
# by creating a sampling frame from household data and selecting PSUs.
#
# LEARNING OUTCOMES:
# 1. Aggregate household data to create a PSU-level sampling frame
# 2. Implement PPS systematic sampling algorithm
# 3. Calculate and verify selection probabilities
# 4. Understand the relationship between MOS and selection probability
#
# DATA SOURCE:
# Statistics South Africa General Household Survey 2024 (Household file)
# Renamed to Zambara National Household Survey for training purposes
#
# REFERENCES:
# - Cochran, W.G. (1977). Sampling Techniques, 3rd ed. Wiley. Chapter 9.
# - Lohr, S.L. (2022). Sampling: Design and Analysis, 3rd ed. CRC Press. Ch 6.
# - Stats SA (2024). GHS 2024 Metadata: Randomised PPS Systematic Sampling.
#
# AUTHOR: SADC Workshop Team
# DATE: March 2026
#===============================================================================

# Clear workspace
rm(list = ls())

# Load required packages
library(haven)      # Read STATA .dta files
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(ggplot2)    # Visualization

cat(paste0(rep("=", 72), collapse = ""), "\n")
cat("LAB 2.2: PPS SAMPLING SELECTION\n")
cat(paste0(rep("=", 72), collapse = ""), "\n\n")

#===============================================================================
# PART 1: LOAD DATA AND CREATE SAMPLING FRAME
#===============================================================================

cat("PART 1: Creating the PSU Sampling Frame\n")
cat(paste0(rep("-", 52), collapse = ""), "\n\n")

# Load household data
# In practice, replace with your file path
zambara_hh <- read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")

cat("Loaded household data:\n")
cat("  Observations:", nrow(zambara_hh), "\n")
cat("  Variables:", ncol(zambara_hh), "\n\n")

# Examine PSU variable
cat("PSU (Primary Sampling Unit) summary:\n")
cat("  Total unique PSUs:", length(unique(zambara_hh$psu)), "\n")
cat("  PSU ID range:", min(zambara_hh$psu), "to", max(zambara_hh$psu), "\n\n")

#-------------------------------------------------------------------------------
# Step 1.1: Aggregate household counts by PSU to create Measure of Size (MOS)
#-------------------------------------------------------------------------------

# Create the sampling frame by aggregating to PSU level
# MOS = number of households in each PSU (this is the "size" for PPS)

psu_frame <- zambara_hh %>%
  group_by(psu, stratum, prov) %>%
  summarise(
    n_households = n(),                           # Measure of Size (MOS)
    mean_income = mean(totmhinc, na.rm = TRUE),   # Average income (auxiliary)
    .groups = "drop"
  ) %>%
  arrange(psu)

cat("PSU Sampling Frame created:\n")
cat("  Number of PSUs (frame units):", nrow(psu_frame), "\n")
cat("  Total households in frame:", sum(psu_frame$n_households), "\n\n")

# Summary statistics of MOS
cat("Measure of Size (MOS = household count) distribution:\n")
print(summary(psu_frame$n_households))
cat("\n")

# Visualize MOS distribution
cat("MOS Distribution by Province:\n")
mos_by_prov <- psu_frame %>%
  group_by(prov) %>%
  summarise(
    n_psus = n(),
    total_hh = sum(n_households),
    mean_mos = mean(n_households),
    min_mos = min(n_households),
    max_mos = max(n_households),
    .groups = "drop"
  )
print(mos_by_prov)
cat("\n")

#===============================================================================
# PART 2: IMPLEMENT PPS SYSTEMATIC SAMPLING
#===============================================================================

cat("PART 2: PPS Systematic Sampling Implementation\n")
cat(paste0(rep("-", 52), collapse = ""), "\n\n")

#-------------------------------------------------------------------------------
# Step 2.1: Set up the PPS sampling parameters
#-------------------------------------------------------------------------------

# Sample size: select 100 PSUs
n_sample <- 100

# Total measure of size (sum of all MOS in frame)
total_mos <- sum(psu_frame$n_households)

cat("Sampling parameters:\n")
cat("  Sample size (n):", n_sample, "PSUs\n")
cat("  Frame size (N):", nrow(psu_frame), "PSUs\n")
cat("  Total MOS:", total_mos, "households\n")
cat("  Sampling interval (I):", round(total_mos / n_sample, 2), "\n\n")

#-------------------------------------------------------------------------------
# Step 2.2: Calculate selection probabilities under PPS
#-------------------------------------------------------------------------------

# Under PPS, the selection probability for PSU i is:
# π_i = n × MOS_i / Total_MOS

psu_frame <- psu_frame %>%
  mutate(
    # Selection probability under PPS
    prob_selection = n_sample * n_households / total_mos,
    
    # Check for certainty selections (prob > 1)
    certainty = prob_selection > 1
  )

# Check for certainty PSUs (those with selection probability > 1)
n_certainty <- sum(psu_frame$certainty)
cat("Selection probability check:\n")
cat("  PSUs with π > 1 (certainty selections):", n_certainty, "\n")
cat("  Max selection probability:", round(max(psu_frame$prob_selection), 4), "\n")
cat("  Min selection probability:", round(min(psu_frame$prob_selection), 4), "\n\n")

#-------------------------------------------------------------------------------
# Step 2.3: Randomized PPS Systematic Sampling Algorithm
#-------------------------------------------------------------------------------

# Following Stats SA methodology:
# 1. Randomize the order of PSUs
# 2. Calculate cumulative MOS
# 3. Generate random start
# 4. Select PSUs at systematic intervals

cat("Implementing Randomized PPS Systematic Sampling...\n\n")

# Set seed for reproducibility (in practice, use different seed or none)
set.seed(42)

# Step 1: Randomize PSU order (this is the "Randomized" part of RPPS)
psu_frame_random <- psu_frame %>%
  mutate(random_order = runif(n())) %>%
  arrange(random_order) %>%
  mutate(seq_number = row_number())

# Step 2: Calculate cumulative MOS
psu_frame_random <- psu_frame_random %>%
  mutate(
    cumulative_mos = cumsum(n_households),
    cumulative_mos_prev = lag(cumulative_mos, default = 0)
  )

# Step 3: Calculate sampling interval
sampling_interval <- total_mos / n_sample
cat("Sampling interval (I):", round(sampling_interval, 2), "\n")

# Step 4: Generate random start between 0 and sampling_interval
random_start <- runif(1, min = 0, max = sampling_interval)
cat("Random start (r):", round(random_start, 2), "\n\n")

# Step 5: Generate selection points
selection_points <- random_start + (0:(n_sample - 1)) * sampling_interval
cat("First 5 selection points:", round(head(selection_points, 5), 2), "\n")
cat("Last 5 selection points:", round(tail(selection_points, 5), 2), "\n\n")

# Step 6: Select PSUs where selection point falls within cumulative range
# A PSU is selected if: cumulative_mos_prev < selection_point <= cumulative_mos

psu_frame_random <- psu_frame_random %>%
  mutate(selected = FALSE)

for (i in 1:length(selection_points)) {
  point <- selection_points[i]
  # Find PSU where this point falls
  idx <- which(psu_frame_random$cumulative_mos_prev < point & 
               psu_frame_random$cumulative_mos >= point)
  if (length(idx) > 0) {
    psu_frame_random$selected[idx[1]] <- TRUE
  }
}

# Count selected PSUs
n_selected <- sum(psu_frame_random$selected)
cat("PSUs selected:", n_selected, "\n\n")

# Extract selected sample
pps_sample <- psu_frame_random %>%
  filter(selected == TRUE) %>%
  select(psu, stratum, prov, n_households, prob_selection, seq_number)

#===============================================================================
# PART 3: VERIFY SELECTION PROBABILITIES
#===============================================================================

cat("PART 3: Verification of Selection Probabilities\n")
cat(paste0(rep("-", 52), collapse = ""), "\n\n")

#-------------------------------------------------------------------------------
# Step 3.1: Compare expected vs actual selection
#-------------------------------------------------------------------------------

cat("Sample composition:\n")
cat("  Expected sample size:", n_sample, "\n")
cat("  Actual sample size:", nrow(pps_sample), "\n\n")

# Summary of selected PSUs
cat("Selected PSUs - MOS distribution:\n")
print(summary(pps_sample$n_households))
cat("\n")

# Compare MOS distribution: Frame vs Sample
comparison <- data.frame(
  Statistic = c("Mean", "Median", "Min", "Max", "Sum"),
  Frame = c(
    mean(psu_frame$n_households),
    median(psu_frame$n_households),
    min(psu_frame$n_households),
    max(psu_frame$n_households),
    sum(psu_frame$n_households)
  ),
  Sample = c(
    mean(pps_sample$n_households),
    median(pps_sample$n_households),
    min(pps_sample$n_households),
    max(pps_sample$n_households),
    sum(pps_sample$n_households)
  )
)
comparison$Frame <- round(comparison$Frame, 1)
comparison$Sample <- round(comparison$Sample, 1)

cat("MOS Comparison (Frame vs PPS Sample):\n")
print(comparison, row.names = FALSE)
cat("\n")

#-------------------------------------------------------------------------------
# Step 3.2: Verify selection probabilities
#-------------------------------------------------------------------------------

# Under PPS, selection probability should be proportional to MOS
# π_i = n × MOS_i / Total_MOS

# Calculate theoretical vs empirical selection rate
cat("Selection Probability Verification:\n")
cat("  Sum of selection probabilities (should ≈ n):", 
    round(sum(psu_frame$prob_selection), 2), "\n")
cat("  Expected sum:", n_sample, "\n\n")

# For selected PSUs, show their selection probabilities
cat("Selection probabilities of first 10 selected PSUs:\n")
print(head(pps_sample[, c("psu", "n_households", "prob_selection")], 10))
cat("\n")

#-------------------------------------------------------------------------------
# Step 3.3: Calculate base weights (inverse of selection probability)
#-------------------------------------------------------------------------------

# Base weight = 1 / π_i
pps_sample <- pps_sample %>%
  mutate(
    base_weight = 1 / prob_selection
  )

cat("Base Weights (1/π) for selected PSUs:\n")
cat("  Mean base weight:", round(mean(pps_sample$base_weight), 2), "\n")
cat("  Range:", round(min(pps_sample$base_weight), 2), "to", 
    round(max(pps_sample$base_weight), 2), "\n\n")

# Verify: sum of weights should estimate frame size
cat("Weight verification:\n")
cat("  Sum of base weights:", round(sum(pps_sample$base_weight), 0), "\n")
cat("  Actual frame size:", nrow(psu_frame), "\n")
cat("  Ratio (should ≈ 1):", round(sum(pps_sample$base_weight) / nrow(psu_frame), 3), "\n\n")

#-------------------------------------------------------------------------------
# Step 3.4: Provincial distribution comparison
#-------------------------------------------------------------------------------

# Compare provincial representation
frame_by_prov <- psu_frame %>%
  group_by(prov) %>%
  summarise(
    n_psus_frame = n(),
    total_mos_frame = sum(n_households),
    pct_psus_frame = 100 * n() / nrow(psu_frame),
    pct_mos_frame = 100 * sum(n_households) / total_mos,
    .groups = "drop"
  )

sample_by_prov <- pps_sample %>%
  group_by(prov) %>%
  summarise(
    n_psus_sample = n(),
    total_mos_sample = sum(n_households),
    pct_psus_sample = 100 * n() / nrow(pps_sample),
    .groups = "drop"
  )

prov_comparison <- frame_by_prov %>%
  left_join(sample_by_prov, by = "prov") %>%
  mutate(
    n_psus_sample = ifelse(is.na(n_psus_sample), 0, n_psus_sample),
    pct_psus_sample = ifelse(is.na(pct_psus_sample), 0, pct_psus_sample)
  )

cat("Provincial Distribution (Frame vs PPS Sample):\n")
print(prov_comparison %>% 
        select(prov, pct_mos_frame, pct_psus_sample) %>%
        mutate(
          pct_mos_frame = round(pct_mos_frame, 1),
          pct_psus_sample = round(pct_psus_sample, 1)
        ))
cat("\n")
cat("Note: Under PPS, sample % should approximate frame MOS %\n\n")

#===============================================================================
# PART 4: SIMULATION TO VERIFY PPS PROPERTIES
#===============================================================================

cat("PART 4: Simulation Verification (Optional)\n")
cat(paste0(rep("-", 52), collapse = ""), "\n\n")

# Run multiple PPS samples to verify selection probabilities empirically
n_simulations <- 1000
cat("Running", n_simulations, "simulations to verify PPS properties...\n")

# Track selection counts for each PSU
selection_counts <- rep(0, nrow(psu_frame))

for (sim in 1:n_simulations) {
  # Randomize order
  random_order <- sample(nrow(psu_frame))
  temp_frame <- psu_frame[random_order, ]
  temp_frame$cumulative_mos <- cumsum(temp_frame$n_households)
  temp_frame$cumulative_mos_prev <- c(0, head(temp_frame$cumulative_mos, -1))
  
  # Random start
  r_start <- runif(1, 0, sampling_interval)
  
  # Selection points
  sel_points <- r_start + (0:(n_sample - 1)) * sampling_interval
  
  # Select PSUs
  for (point in sel_points) {
    idx <- which(temp_frame$cumulative_mos_prev < point & 
                 temp_frame$cumulative_mos >= point)
    if (length(idx) > 0) {
      original_idx <- which(psu_frame$psu == temp_frame$psu[idx[1]])
      selection_counts[original_idx] <- selection_counts[original_idx] + 1
    }
  }
}

# Calculate empirical selection probability
psu_frame$empirical_prob <- selection_counts / n_simulations

# Compare theoretical vs empirical
cat("\nTheoretical vs Empirical Selection Probabilities:\n")
cat("  Correlation:", round(cor(psu_frame$prob_selection, psu_frame$empirical_prob), 4), "\n")
cat("  Mean absolute difference:", 
    round(mean(abs(psu_frame$prob_selection - psu_frame$empirical_prob)), 4), "\n\n")

# Show comparison for a few PSUs
cat("Sample comparison (first 10 PSUs by MOS):\n")
print(psu_frame %>%
        arrange(desc(n_households)) %>%
        head(10) %>%
        select(psu, n_households, prob_selection, empirical_prob) %>%
        mutate(
          prob_selection = round(prob_selection, 4),
          empirical_prob = round(empirical_prob, 4),
          difference = round(empirical_prob - prob_selection, 4)
        ))

#===============================================================================
# PART 5: SAVE RESULTS
#===============================================================================

cat("\n")
cat("PART 5: Saving Results\n")
cat(paste0(rep("-", 52), collapse = ""), "\n\n")

# Save the sampling frame
write.csv(psu_frame, "zambara_psu_frame.csv", row.names = FALSE)
cat("Saved: zambara_psu_frame.csv\n")

# Save the selected sample
write.csv(pps_sample, "zambara_pps_sample.csv", row.names = FALSE)
cat("Saved: zambara_pps_sample.csv\n")

#===============================================================================
# SUMMARY
#===============================================================================

cat("\n")
cat(paste0(rep("=", 72), collapse = ""), "\n")
cat("LAB 2.2 SUMMARY\n")
cat(paste0(rep("=", 72), collapse = ""), "\n\n")

cat("KEY RESULTS:\n")
cat("  1. Created sampling frame with", nrow(psu_frame), "PSUs\n")
cat("  2. Total MOS (households):", total_mos, "\n")
cat("  3. Selected", nrow(pps_sample), "PSUs using PPS systematic sampling\n")
cat("  4. Sampling interval:", round(sampling_interval, 2), "\n")
cat("  5. Selection probabilities verified (correlation with empirical ≈ 1)\n\n")

cat("KEY FORMULAS:\n")
cat("  Selection probability: π_i = n × MOS_i / Σ MOS\n")
cat("  Sampling interval: I = Σ MOS / n\n")
cat("  Base weight: w_i = 1 / π_i\n\n")

cat("INTERPRETATION:\n")
cat("  - Larger PSUs (more households) have higher selection probability\n")
cat("  - PPS sampling is self-weighting for household-level estimates\n")
cat("  - Provincial representation proportional to total households\n\n")

cat(paste0(rep("=", 72), collapse = ""), "\n")
cat("END OF LAB 2.2 (R)\n")
cat(paste0(rep("=", 72), collapse = ""), "\n")
