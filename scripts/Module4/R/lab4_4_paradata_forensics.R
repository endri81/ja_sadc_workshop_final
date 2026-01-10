#===============================================================================
# LAB 4.4: PARADATA FORENSICS
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 4, Module 4.4: Quality Assurance Integration
#===============================================================================
#
# OBJECTIVE: Use paradata to detect data quality issues
#   1. Simulate paradata: Interview duration and timestamps
#   2. Flag "Curbstoning" (fake interviews): duration < 5 min or 2 AM
#   3. Generate Field Auditor Report listing suspicious enumerators
#
# DATA: GHS 2024 Household File
#
# ZAMBARA NARRATIVE: Lindiwe discovers suspicious patterns in field data
#
#===============================================================================

#-------------------------------------------------------------------------------
# 0. SETUP AND PACKAGES
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Required packages
required_packages <- c("haven", "dplyr", "tidyr", "ggplot2", "lubridate",
                       "knitr", "kableExtra", "scales")

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
cat("  LAB 4.4: PARADATA FORENSICS\n")
cat("  SADC Advanced Sampling Workshop - Day 4\n")
cat("================================================================\n\n")

#-------------------------------------------------------------------------------
# 1. LOAD AND PREPARE DATA
#-------------------------------------------------------------------------------

cat("SECTION 1: Loading and Preparing Data\n")
cat("-------------------------------------\n\n")

# Load GHS 2024 household data
ghs_hh <- haven::read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")

cat("Dataset loaded:", nrow(ghs_hh), "households\n\n")

# Create unique household ID if not present
if (!"hh_id" %in% names(ghs_hh)) {
  ghs_hh$hh_id <- 1:nrow(ghs_hh)
}

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
  mutate(prov_name = prov_labels[as.character(prov)])

#-------------------------------------------------------------------------------
# 2. SIMULATE PARADATA: ENUMERATOR IDs
#-------------------------------------------------------------------------------

cat("SECTION 2: Simulating Enumerator Assignments\n")
cat("--------------------------------------------\n\n")

# Simulate enumerator IDs (50 enumerators across the country)
n_enumerators <- 50
enumerator_ids <- paste0("ENUM_", sprintf("%03d", 1:n_enumerators))

# Assign enumerators to PSUs (each enumerator covers ~3-5 PSUs)
psu_list <- unique(ghs_hh$psu)
n_psus <- length(psu_list)

# Create PSU to enumerator mapping
psu_enum_map <- data.frame(
  psu = psu_list,
  enumerator_id = sample(enumerator_ids, n_psus, replace = TRUE)
)

# Merge enumerator IDs to household data
ghs_hh <- ghs_hh %>%
  left_join(psu_enum_map, by = "psu")

cat("Enumerators assigned:", n_enumerators, "\n")
cat("PSUs covered:", n_psus, "\n")
cat("Mean PSUs per enumerator:", round(n_psus / n_enumerators, 1), "\n\n")

# Check enumerator workload
enum_workload <- ghs_hh %>%
  group_by(enumerator_id) %>%
  summarise(
    n_interviews = n(),
    n_psus = n_distinct(psu),
    .groups = "drop"
  )

cat("Enumerator Workload Summary:\n")
cat("  Mean interviews:", round(mean(enum_workload$n_interviews), 1), "\n")
cat("  Min interviews:", min(enum_workload$n_interviews), "\n")
cat("  Max interviews:", max(enum_workload$n_interviews), "\n\n")

#-------------------------------------------------------------------------------
# 3. SIMULATE PARADATA: INTERVIEW DURATION
#-------------------------------------------------------------------------------

cat("SECTION 3: Simulating Interview Duration\n")
cat("----------------------------------------\n\n")

# GHS questionnaire is substantial - expect 25-45 minutes for good interview
# Distribution: Normal with mean 35, SD 8, but with some outliers

# Most interviews: legitimate (90%)
# Some very fast: curbstoning suspects (3%)
# Some very slow: complex households or problems (7%)

ghs_hh <- ghs_hh %>%
  mutate(
    # Interview type (for simulation)
    interview_type = sample(
      c("legitimate", "curbstoning", "complex"),
      n(),
      replace = TRUE,
      prob = c(0.90, 0.03, 0.07)
    ),
    # Duration based on type
    duration_minutes = case_when(
      interview_type == "curbstoning" ~ pmax(1, rnorm(n(), mean = 3, sd = 1.5)),
      interview_type == "complex" ~ rnorm(n(), mean = 55, sd = 10),
      TRUE ~ rnorm(n(), mean = 35, sd = 8)
    ),
    # Ensure minimum 1 minute
    duration_minutes = pmax(1, duration_minutes)
  )

cat("Interview Duration Summary:\n")
cat("  Mean:", round(mean(ghs_hh$duration_minutes), 1), "minutes\n")
cat("  Median:", round(median(ghs_hh$duration_minutes), 1), "minutes\n")
cat("  Min:", round(min(ghs_hh$duration_minutes), 1), "minutes\n")
cat("  Max:", round(max(ghs_hh$duration_minutes), 1), "minutes\n\n")

# Duration distribution
duration_dist <- ghs_hh %>%
  mutate(duration_cat = cut(duration_minutes, 
                            breaks = c(0, 5, 15, 30, 45, 60, Inf),
                            labels = c("<5 min", "5-15 min", "15-30 min", 
                                       "30-45 min", "45-60 min", ">60 min"))) %>%
  count(duration_cat) %>%
  mutate(pct = round(100 * n / sum(n), 1))

cat("Duration Distribution:\n")
print(as.data.frame(duration_dist))
cat("\n")

#-------------------------------------------------------------------------------
# 4. SIMULATE PARADATA: INTERVIEW TIMESTAMPS
#-------------------------------------------------------------------------------

cat("SECTION 4: Simulating Interview Timestamps\n")
cat("------------------------------------------\n\n")

# Field work period: March 1-31, 2026
# Normal working hours: 8 AM - 6 PM
# Some legitimate evening interviews: 6 PM - 8 PM
# Suspicious: Before 7 AM or after 9 PM

# Survey reference date
survey_start <- as.Date("2026-03-01")
survey_end <- as.Date("2026-03-31")
survey_days <- seq(survey_start, survey_end, by = "day")

ghs_hh <- ghs_hh %>%
  mutate(
    # Assign interview date (random within survey period)
    interview_date = sample(survey_days, n(), replace = TRUE),
    
    # Assign interview hour based on type
    interview_hour = case_when(
      # Curbstoning: some at suspicious hours (20% of curbstoners)
      interview_type == "curbstoning" & runif(n()) < 0.20 ~ sample(c(1, 2, 3, 23), n(), replace = TRUE),
      # Curbstoning: most during normal hours to avoid detection
      interview_type == "curbstoning" ~ sample(9:17, n(), replace = TRUE),
      # Complex: often later in day
      interview_type == "complex" ~ sample(10:19, n(), replace = TRUE),
      # Legitimate: normal distribution of working hours
      TRUE ~ sample(c(rep(8:18, 10), 19, 20), n(), replace = TRUE)
    ),
    
    # Random minutes
    interview_minute = sample(0:59, n(), replace = TRUE),
    
    # Create full timestamp
    interview_timestamp = as.POSIXct(
      paste(interview_date, sprintf("%02d:%02d:00", interview_hour, interview_minute)),
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

# Hour distribution
hour_dist <- ghs_hh %>%
  count(interview_hour) %>%
  mutate(pct = round(100 * n / sum(n), 2))

cat("Interview Hour Distribution:\n")
print(as.data.frame(hour_dist))
cat("\n")

#-------------------------------------------------------------------------------
# 5. FLAG CURBSTONING INDICATORS
#-------------------------------------------------------------------------------

cat("SECTION 5: Flagging Curbstoning Indicators\n")
cat("------------------------------------------\n\n")

# Define curbstoning criteria
DURATION_THRESHOLD <- 5  # minutes
SUSPICIOUS_HOURS <- c(0, 1, 2, 3, 4, 5, 22, 23)  # 10 PM - 5 AM

ghs_hh <- ghs_hh %>%
  mutate(
    # Flag 1: Very short interviews
    flag_short_duration = duration_minutes < DURATION_THRESHOLD,
    
    # Flag 2: Suspicious interview time
    flag_suspicious_time = interview_hour %in% SUSPICIOUS_HOURS,
    
    # Flag 3: Combined - either condition
    flag_any = flag_short_duration | flag_suspicious_time,
    
    # Flag 4: Severe - both conditions (extremely suspicious)
    flag_severe = flag_short_duration & flag_suspicious_time
  )

# Summary of flags
flag_summary <- ghs_hh %>%
  summarise(
    total_interviews = n(),
    short_duration = sum(flag_short_duration),
    suspicious_time = sum(flag_suspicious_time),
    any_flag = sum(flag_any),
    severe_flag = sum(flag_severe)
  ) %>%
  mutate(
    pct_short = round(100 * short_duration / total_interviews, 2),
    pct_time = round(100 * suspicious_time / total_interviews, 2),
    pct_any = round(100 * any_flag / total_interviews, 2),
    pct_severe = round(100 * severe_flag / total_interviews, 2)
  )

cat("CURBSTONING FLAGS SUMMARY:\n")
cat("-------------------------\n")
cat("Total interviews:", flag_summary$total_interviews, "\n\n")
cat("Flag 1 - Short duration (<", DURATION_THRESHOLD, "min):", 
    flag_summary$short_duration, "(", flag_summary$pct_short, "%)\n")
cat("Flag 2 - Suspicious time (", paste(SUSPICIOUS_HOURS, collapse=","), "):", 
    flag_summary$suspicious_time, "(", flag_summary$pct_time, "%)\n")
cat("Any flag:", flag_summary$any_flag, "(", flag_summary$pct_any, "%)\n")
cat("Severe (both flags):", flag_summary$severe_flag, "(", flag_summary$pct_severe, "%)\n\n")

#-------------------------------------------------------------------------------
# 6. ANALYZE BY ENUMERATOR
#-------------------------------------------------------------------------------

cat("SECTION 6: Enumerator-Level Analysis\n")
cat("------------------------------------\n\n")

# Calculate metrics by enumerator
enum_analysis <- ghs_hh %>%
  group_by(enumerator_id) %>%
  summarise(
    n_interviews = n(),
    n_psus = n_distinct(psu),
    
    # Duration metrics
    mean_duration = mean(duration_minutes),
    min_duration = min(duration_minutes),
    sd_duration = sd(duration_minutes),
    
    # Flag counts
    n_short = sum(flag_short_duration),
    n_suspicious_time = sum(flag_suspicious_time),
    n_any_flag = sum(flag_any),
    n_severe = sum(flag_severe),
    
    # Flag rates
    pct_short = 100 * mean(flag_short_duration),
    pct_suspicious_time = 100 * mean(flag_suspicious_time),
    pct_any_flag = 100 * mean(flag_any),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Risk score (weighted combination)
    risk_score = 0.4 * pct_short + 0.3 * pct_suspicious_time + 
                 0.2 * (100 - pmin(mean_duration, 35) / 35 * 100) +
                 0.1 * (100 - pmin(sd_duration, 10) / 10 * 100),
    # Risk category
    risk_category = case_when(
      risk_score > 30 ~ "HIGH",
      risk_score > 15 ~ "MEDIUM",
      risk_score > 5 ~ "LOW",
      TRUE ~ "NORMAL"
    )
  ) %>%
  arrange(desc(risk_score))

cat("Enumerator Risk Distribution:\n")
print(table(enum_analysis$risk_category))
cat("\n")

#-------------------------------------------------------------------------------
# 7. IDENTIFY HIGH-RISK ENUMERATORS
#-------------------------------------------------------------------------------

cat("SECTION 7: High-Risk Enumerators\n")
cat("--------------------------------\n\n")

# Filter high-risk enumerators
high_risk <- enum_analysis %>%
  filter(risk_category == "HIGH") %>%
  select(enumerator_id, n_interviews, mean_duration, n_any_flag, pct_any_flag, risk_score)

cat("HIGH-RISK ENUMERATORS (", nrow(high_risk), " identified):\n")
print(as.data.frame(high_risk))
cat("\n")

# Medium risk
medium_risk <- enum_analysis %>%
  filter(risk_category == "MEDIUM") %>%
  select(enumerator_id, n_interviews, mean_duration, n_any_flag, pct_any_flag, risk_score)

cat("MEDIUM-RISK ENUMERATORS (", nrow(medium_risk), " identified):\n")
print(as.data.frame(medium_risk))
cat("\n")

#-------------------------------------------------------------------------------
# 8. GENERATE FIELD AUDITOR REPORT
#-------------------------------------------------------------------------------

cat("================================================================\n")
cat("SECTION 8: FIELD AUDITOR REPORT\n")
cat("================================================================\n\n")

report_date <- format(Sys.Date(), "%Y-%m-%d")

cat("ZAMBARA NATIONAL STATISTICS OFFICE\n")
cat("FIELD AUDITOR REPORT: PARADATA QUALITY REVIEW\n")
cat("Report Generated:", report_date, "\n")
cat("Survey: General Household Survey 2026\n")
cat("Analysis Period: March 1-31, 2026\n")
cat("================================================================\n\n")

cat("EXECUTIVE SUMMARY\n")
cat("-----------------\n")
cat("Total interviews reviewed:", nrow(ghs_hh), "\n")
cat("Total enumerators:", n_enumerators, "\n")
cat("Interviews flagged:", flag_summary$any_flag, "(", flag_summary$pct_any, "%)\n")
cat("High-risk enumerators:", nrow(high_risk), "\n")
cat("Medium-risk enumerators:", nrow(medium_risk), "\n\n")

cat("FLAGGING CRITERIA\n")
cat("-----------------\n")
cat("1. Short Duration: Interview < 5 minutes\n")
cat("2. Suspicious Time: Interview conducted 10 PM - 5 AM\n")
cat("3. Risk Score: Weighted combination of duration and time flags\n\n")

cat("DETAILED FINDINGS\n")
cat("-----------------\n\n")

# High risk details
if (nrow(high_risk) > 0) {
  cat("HIGH-RISK ENUMERATORS (Immediate Review Required):\n\n")
  for (i in 1:nrow(high_risk)) {
    enum <- high_risk$enumerator_id[i]
    cat("  ", enum, "\n")
    cat("    - Interviews:", high_risk$n_interviews[i], "\n")
    cat("    - Mean duration:", round(high_risk$mean_duration[i], 1), "minutes\n")
    cat("    - Flagged interviews:", high_risk$n_any_flag[i], 
        "(", round(high_risk$pct_any_flag[i], 1), "%)\n")
    cat("    - Risk score:", round(high_risk$risk_score[i], 1), "\n")
    cat("    - RECOMMENDATION: Suspend and investigate\n\n")
  }
}

# Medium risk details
if (nrow(medium_risk) > 0) {
  cat("MEDIUM-RISK ENUMERATORS (Enhanced Monitoring Required):\n\n")
  for (i in 1:min(5, nrow(medium_risk))) {  # Show top 5
    enum <- medium_risk$enumerator_id[i]
    cat("  ", enum, "\n")
    cat("    - Interviews:", medium_risk$n_interviews[i], "\n")
    cat("    - Flagged interviews:", medium_risk$n_any_flag[i], 
        "(", round(medium_risk$pct_any_flag[i], 1), "%)\n")
    cat("    - Risk score:", round(medium_risk$risk_score[i], 1), "\n")
    cat("    - RECOMMENDATION: Spot-check interviews\n\n")
  }
  if (nrow(medium_risk) > 5) {
    cat("  ... and", nrow(medium_risk) - 5, "additional medium-risk enumerators\n\n")
  }
}

cat("RECOMMENDATIONS\n")
cat("---------------\n")
cat("1. HIGH-RISK: Immediately suspend", nrow(high_risk), "enumerators pending investigation\n")
cat("2. MEDIUM-RISK: Implement enhanced supervision for", nrow(medium_risk), "enumerators\n")
cat("3. Conduct callback verification on 10% of flagged interviews\n")
cat("4. Review training protocols for interview pacing\n")
cat("5. Implement real-time paradata monitoring in future surveys\n\n")

cat("================================================================\n")
cat("END OF REPORT\n")
cat("================================================================\n\n")

#-------------------------------------------------------------------------------
# 9. VISUALIZATIONS
#-------------------------------------------------------------------------------

cat("SECTION 9: Creating Visualizations\n")
cat("----------------------------------\n\n")

# Plot 1: Duration distribution with threshold
p_duration <- ggplot(ghs_hh, aes(x = duration_minutes)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = DURATION_THRESHOLD, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = DURATION_THRESHOLD + 2, y = Inf, label = "Curbstoning\nThreshold", 
           vjust = 2, hjust = 0, color = "red", size = 3) +
  labs(
    title = "Interview Duration Distribution",
    subtitle = paste0("Red line: ", DURATION_THRESHOLD, "-minute threshold for curbstoning flag"),
    x = "Duration (minutes)",
    y = "Count"
  ) +
  theme_minimal() +
  xlim(0, 80)

ggsave("lab4_4_duration_distribution.png", p_duration, width = 10, height = 6, dpi = 150)
cat("Saved: lab4_4_duration_distribution.png\n")

# Plot 2: Interview hours distribution
p_hours <- ggplot(ghs_hh, aes(x = factor(interview_hour), fill = flag_suspicious_time)) +
  geom_bar() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                    labels = c("Normal", "Suspicious"),
                    name = "Time Flag") +
  labs(
    title = "Interview Hour Distribution",
    subtitle = "Red bars indicate suspicious hours (10 PM - 5 AM)",
    x = "Hour of Day",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("lab4_4_interview_hours.png", p_hours, width = 10, height = 6, dpi = 150)
cat("Saved: lab4_4_interview_hours.png\n")

# Plot 3: Enumerator risk scores
p_risk <- ggplot(enum_analysis, aes(x = reorder(enumerator_id, -risk_score), 
                                    y = risk_score, fill = risk_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("NORMAL" = "green", "LOW" = "yellow", 
                               "MEDIUM" = "orange", "HIGH" = "red")) +
  labs(
    title = "Enumerator Risk Scores",
    subtitle = "Based on duration and timing anomalies",
    x = "Enumerator ID",
    y = "Risk Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        legend.position = "bottom")

ggsave("lab4_4_enumerator_risk.png", p_risk, width = 12, height = 6, dpi = 150)
cat("Saved: lab4_4_enumerator_risk.png\n")

# Plot 4: Scatterplot of duration vs hour by flag
p_scatter <- ggplot(ghs_hh, aes(x = interview_hour, y = duration_minutes, color = flag_any)) +
  geom_point(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                     labels = c("Normal", "Flagged"),
                     name = "Status") +
  geom_hline(yintercept = DURATION_THRESHOLD, linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = c(5, 22), linetype = "dashed", color = "darkred") +
  labs(
    title = "Interview Duration vs Hour",
    subtitle = "Dashed lines show flagging thresholds",
    x = "Hour of Day",
    y = "Duration (minutes)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("lab4_4_duration_vs_hour.png", p_scatter, width = 10, height = 6, dpi = 150)
cat("Saved: lab4_4_duration_vs_hour.png\n\n")

#-------------------------------------------------------------------------------
# 10. EXPORT RESULTS
#-------------------------------------------------------------------------------

cat("SECTION 10: Exporting Results\n")
cat("-----------------------------\n\n")

# Export enumerator analysis
write.csv(enum_analysis, "lab4_4_enumerator_analysis.csv", row.names = FALSE)
cat("Saved: lab4_4_enumerator_analysis.csv\n")

# Export flagged interviews
flagged_interviews <- ghs_hh %>%
  filter(flag_any) %>%
  select(hh_id, psu, enumerator_id, duration_minutes, interview_hour,
         flag_short_duration, flag_suspicious_time)

write.csv(flagged_interviews, "lab4_4_flagged_interviews.csv", row.names = FALSE)
cat("Saved: lab4_4_flagged_interviews.csv\n")

# Export high-risk report
write.csv(high_risk, "lab4_4_high_risk_enumerators.csv", row.names = FALSE)
cat("Saved: lab4_4_high_risk_enumerators.csv\n")

# Export summary statistics
summary_stats <- data.frame(
  Metric = c("Total Interviews", "Total Enumerators", "Flagged Short Duration",
             "Flagged Suspicious Time", "Any Flag", "High Risk Enumerators",
             "Medium Risk Enumerators"),
  Value = c(nrow(ghs_hh), n_enumerators, flag_summary$short_duration,
            flag_summary$suspicious_time, flag_summary$any_flag,
            nrow(high_risk), nrow(medium_risk))
)

write.csv(summary_stats, "lab4_4_summary_statistics.csv", row.names = FALSE)
cat("Saved: lab4_4_summary_statistics.csv\n\n")

cat("================================================================\n")
cat("  LAB 4.4 COMPLETE\n")
cat("================================================================\n")

#===============================================================================
# END OF LAB 4.4
#===============================================================================
