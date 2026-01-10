# =============================================================================
# LAB 5.3: AUTOMATED CODEBOOK GENERATION
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 5: Documentation Standards
# =============================================================================
#
# OBJECTIVES:
#   1. Generate a comprehensive codebook with variable metadata
#   2. Export metadata in standard formats (Markdown and Excel)
#   3. Visualize variable "health" (missingness patterns)
#
# DATA: Statistics South Africa General Household Survey 2024 (Household File)
# FILE: ghs-2024-hhold-v1.dta
#
# REQUIRED PACKAGES:
#   - haven: Read STATA .dta files
#   - tidyverse: Data manipulation and visualization
#   - writexl: Excel export
#   - knitr: Markdown table generation
#   - scales: Percentage formatting
#
# =============================================================================

# -----------------------------------------------------------------------------
# SECTION 0: SETUP AND PACKAGE INSTALLATION
# -----------------------------------------------------------------------------

# Install packages if needed
required_packages <- c("haven", "tidyverse", "writexl", "knitr", "scales", 
                       "labelled", "sjlabelled")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Load libraries
library(haven)
library(tidyverse)
library(writexl)
library(knitr)
library(scales)
library(labelled)

# Set encoding for proper handling of special characters
# Cross-platform locale setting
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_ALL", "English")
} else {
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
}
options(encoding = "UTF-8")

# Set working directory (adjust as needed)
# setwd("path/to/your/data")

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LAB 5.3: AUTOMATED CODEBOOK GENERATION\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# -----------------------------------------------------------------------------
# SECTION 1: LOAD DATA
# -----------------------------------------------------------------------------

cat("SECTION 1: Loading GHS 2024 Household Data\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n\n")

# Load the STATA dataset
# The haven package preserves variable labels and value labels from .dta files
ghs_hh <- read_dta("./data/GHS_2024/ghs-2024-hhold-v1.dta")

# Basic dataset information
n_obs <- nrow(ghs_hh)
n_vars <- ncol(ghs_hh)

cat("Dataset loaded successfully!\n")
cat(sprintf("  Observations: %s\n", format(n_obs, big.mark = ",")))
cat(sprintf("  Variables: %d\n\n", n_vars))

# -----------------------------------------------------------------------------
# SECTION 2: EXTRACT VARIABLE METADATA
# -----------------------------------------------------------------------------

cat("SECTION 2: Extracting Variable Metadata\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n\n")

# Function to determine variable type in a user-friendly way
get_var_type <- function(x) {
  if (is.labelled(x)) {
    base_type <- typeof(unclass(x))
    if (base_type %in% c("double", "integer")) {
      return("Numeric (Labelled)")
    } else {
      return("Character (Labelled)")
    }
  } else if (is.factor(x)) {
    return("Factor")
  } else if (is.numeric(x)) {
    if (is.integer(x)) {
      return("Integer")
    } else {
      return("Numeric")
    }
  } else if (is.character(x)) {
    return("Character")
  } else if (inherits(x, "Date")) {
    return("Date")
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("DateTime")
  } else {
    return(class(x)[1])
  }
}

# Function to get value labels as a formatted string (with encoding handling)
get_value_labels <- function(x) {
  if (!is.labelled(x)) return("")
  
  labs <- tryCatch(
    val_labels(x),
    error = function(e) NULL
  )
  
  if (is.null(labs) || length(labs) == 0) return("")
  
  # Clean encoding for label names
  lab_names <- tryCatch(
    iconv(names(labs), from = "", to = "UTF-8", sub = "?"),
    error = function(e) names(labs)
  )
  lab_names[is.na(lab_names)] <- "?"
  
  # Limit to first 5 labels if there are many
  if (length(labs) > 5) {
    lab_str <- paste(lab_names[1:5], "=", labs[1:5], collapse = "; ")
    lab_str <- paste0(lab_str, "; ... (", length(labs), " total)")
  } else {
    lab_str <- paste(lab_names, "=", labs, collapse = "; ")
  }
  return(lab_str)
}

# Function to get STATA storage type
get_stata_type <- function(x) {
  attr_format <- attr(x, "format.stata")
  if (!is.null(attr_format)) {
    return(attr_format)
  }
  return("")
}

# Helper function to safely extract and clean labels
safe_label <- function(x) {
  lab <- tryCatch(
    var_label(x),
    error = function(e) NULL
  )
  if (is.null(lab) || length(lab) == 0) return("")
  if (is.na(lab)) return("")
  
  # Convert encoding and replace problematic characters
  lab <- tryCatch(
    iconv(as.character(lab), from = "", to = "UTF-8", sub = "?"),
    error = function(e) as.character(lab)
  )
  if (is.na(lab)) return("")
  return(lab)
}

# Build the codebook dataframe
cat("Building codebook metadata...\n")

codebook <- tibble(
  position = 1:n_vars,
  variable = names(ghs_hh),
  label = map_chr(ghs_hh, safe_label),
  type = map_chr(ghs_hh, get_var_type),
  stata_format = map_chr(ghs_hh, get_stata_type),
  n_obs = n_obs,
  n_missing = map_int(ghs_hh, ~sum(is.na(.x))),
  n_valid = n_obs - map_int(ghs_hh, ~sum(is.na(.x))),
  pct_missing = map_dbl(ghs_hh, ~mean(is.na(.x)) * 100),
  n_unique = map_int(ghs_hh, ~n_distinct(.x, na.rm = TRUE)),
  value_labels = map_chr(ghs_hh, get_value_labels)
)

# Add summary statistics for numeric variables
codebook <- codebook %>%
  mutate(
    min_value = map_chr(ghs_hh, ~{
      if (is.numeric(.x)) {
        val <- min(.x, na.rm = TRUE)
        if (is.finite(val)) format(val, nsmall = 2) else "NA"
      } else ""
    }),
    max_value = map_chr(ghs_hh, ~{
      if (is.numeric(.x)) {
        val <- max(.x, na.rm = TRUE)
        if (is.finite(val)) format(val, nsmall = 2) else "NA"
      } else ""
    }),
    mean_value = map_chr(ghs_hh, ~{
      if (is.numeric(.x)) {
        val <- mean(.x, na.rm = TRUE)
        if (is.finite(val)) format(val, nsmall = 2) else "NA"
      } else ""
    })
  )

cat(sprintf("Codebook created with %d variables\n\n", nrow(codebook)))

# Display first 10 variables as preview
cat("Preview (first 10 variables):\n")
print(codebook %>% 
        select(position, variable, label, type, n_missing, pct_missing) %>% 
        head(10), 
      n = 10, width = 120)

# -----------------------------------------------------------------------------
# SECTION 3: VARIABLE HEALTH ASSESSMENT
# -----------------------------------------------------------------------------

cat("\n")
cat("SECTION 3: Variable Health Assessment\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n\n")

# Categorize variables by missingness level
codebook <- codebook %>%
  mutate(
    health_status = case_when(
      pct_missing == 0 ~ "Complete (0%)",
      pct_missing < 1 ~ "Excellent (<1%)",
      pct_missing < 5 ~ "Good (1-5%)",
      pct_missing < 10 ~ "Moderate (5-10%)",
      pct_missing < 25 ~ "Concerning (10-25%)",
      pct_missing < 50 ~ "Poor (25-50%)",
      TRUE ~ "Critical (>50%)"
    ),
    health_status = factor(health_status, levels = c(
      "Complete (0%)", "Excellent (<1%)", "Good (1-5%)", 
      "Moderate (5-10%)", "Concerning (10-25%)", 
      "Poor (25-50%)", "Critical (>50%)"
    ))
  )

# Summary statistics
health_summary <- codebook %>%
  group_by(health_status) %>%
  summarise(
    n_variables = n(),
    pct_of_total = n() / n_vars * 100,
    .groups = "drop"
  )

cat("Variable Health Summary:\n")
print(health_summary, n = 10)

cat("\n")
cat(sprintf("Overall dataset completeness: %.2f%%\n", 
            100 - mean(codebook$pct_missing)))

# Identify problematic variables (>10% missing)
problematic_vars <- codebook %>%
  filter(pct_missing > 10) %>%
  arrange(desc(pct_missing)) %>%
  select(variable, label, pct_missing, n_missing)

if (nrow(problematic_vars) > 0) {
  cat(sprintf("\nVariables with >10%% missing (%d variables):\n", 
              nrow(problematic_vars)))
  print(problematic_vars, n = 20, width = 100)
} else {
  cat("\nNo variables with >10% missing values.\n")
}

# -----------------------------------------------------------------------------
# SECTION 4: EXPORT TO EXCEL
# -----------------------------------------------------------------------------

cat("\n")
cat("SECTION 4: Export Codebook to Excel\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n\n")

# Prepare sheets for Excel workbook
excel_sheets <- list(
  "Codebook" = codebook %>%
    select(position, variable, label, type, stata_format, 
           n_obs, n_valid, n_missing, pct_missing, n_unique,
           min_value, max_value, mean_value, health_status, value_labels),
  
  "Health_Summary" = health_summary,
  
  "Problematic_Variables" = if (nrow(problematic_vars) > 0) {
    problematic_vars
  } else {
    tibble(message = "No variables with >10% missing")
  },
  
  "Dataset_Info" = tibble(
    attribute = c("Dataset Name", "File", "Observations", "Variables",
                  "Overall Completeness (%)", "Generation Date", "Software"),
    value = c("GHS 2024 Household File", "ghs-2024-hhold-v1.dta",
              format(n_obs, big.mark = ","), as.character(n_vars),
              sprintf("%.2f", 100 - mean(codebook$pct_missing)),
              as.character(Sys.time()), 
              paste(R.version$version.string))
  )
)

# Write to Excel
excel_filename <- "ghs2024_codebook.xlsx"
write_xlsx(excel_sheets, excel_filename)
cat(sprintf("Excel codebook saved: %s\n", excel_filename))

# -----------------------------------------------------------------------------
# SECTION 5: EXPORT TO MARKDOWN
# -----------------------------------------------------------------------------

cat("\n")
cat("SECTION 5: Export Codebook to Markdown\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n\n")

# Create markdown content
md_content <- c(
  "# GHS 2024 Household File - Codebook",
  "",
  paste0("**Generated:** ", Sys.time()),
  "",
  "## Dataset Overview",
  "",
  sprintf("- **File:** ghs-2024-hhold-v1.dta"),
  sprintf("- **Observations:** %s", format(n_obs, big.mark = ",")),
  sprintf("- **Variables:** %d", n_vars),
  sprintf("- **Overall Completeness:** %.2f%%", 100 - mean(codebook$pct_missing)),
  "",
  "## Variable Health Summary",
  "",
  kable(health_summary, format = "markdown"),
  "",
  "## Complete Variable List",
  "",
  "| # | Variable | Label | Type | Missing (%) | Unique |",
  "|---|----------|-------|------|-------------|--------|"
)

# Helper function to safely truncate strings with encoding issues
safe_truncate <- function(x, max_length = 50) {
  # Handle NULL, NA, and empty cases
  if (is.null(x) || length(x) == 0) return("")
  x <- as.character(x)
  if (is.na(x) || x == "") return("")
  
  # Convert to UTF-8 and handle encoding issues
  x <- tryCatch(
    iconv(x, from = "", to = "UTF-8", sub = "?"),
    error = function(e) x
  )
  if (is.na(x)) return("")
  
  # Use nchar with allowNA to handle problematic strings
  len <- tryCatch(
    nchar(x, allowNA = TRUE),
    error = function(e) NA
  )
  
  if (is.na(len) || len <= max_length) {
    return(x)
  } else {
    return(paste0(substr(x, 1, max_length - 3), "..."))
  }
}

# Add each variable row
for (i in 1:nrow(codebook)) {
  row <- codebook[i, ]
  # Truncate label if too long (with safe encoding handling)
  label_truncated <- safe_truncate(row$label, 50)
  
  md_content <- c(md_content, sprintf(
    "| %d | `%s` | %s | %s | %.1f%% | %d |",
    row$position, row$variable, label_truncated, 
    row$type, row$pct_missing, row$n_unique
  ))
}

# Add problematic variables section
if (nrow(problematic_vars) > 0) {
  md_content <- c(md_content, "",
    "## Variables Requiring Attention (>10% Missing)",
    "",
    "| Variable | Label | Missing (%) | N Missing |",
    "|----------|-------|-------------|-----------|"
  )
  
  for (j in 1:nrow(problematic_vars)) {
    pv <- problematic_vars[j, ]
    label_clean <- safe_truncate(pv$label, 40)
    md_content <- c(md_content, sprintf(
      "| `%s` | %s | %.1f%% | %s |",
      pv$variable, label_clean, pv$pct_missing, 
      format(pv$n_missing, big.mark = ",")
    ))
  }
}

# Write to markdown file
md_filename <- "ghs2024_codebook.md"
writeLines(md_content, md_filename)
cat(sprintf("Markdown codebook saved: %s\n", md_filename))

# -----------------------------------------------------------------------------
# SECTION 6: VISUALIZE VARIABLE HEALTH
# -----------------------------------------------------------------------------

cat("\n")
cat("SECTION 6: Variable Health Visualization\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n\n")

# Color palette for health status
health_colors <- c(
  "Complete (0%)" = "#2E7D32",      # Dark green
  "Excellent (<1%)" = "#4CAF50",    # Green
  "Good (1-5%)" = "#8BC34A",        # Light green
  "Moderate (5-10%)" = "#FFC107",   # Amber
  "Concerning (10-25%)" = "#FF9800", # Orange
  "Poor (25-50%)" = "#F44336",      # Red
  "Critical (>50%)" = "#B71C1C"     # Dark red
)

# --- PLOT 1: Missingness Bar Chart (All Variables) ---

# For datasets with many variables, show top 50 with highest missingness
if (n_vars > 50) {
  plot_data <- codebook %>%
    filter(pct_missing > 0) %>%
    arrange(desc(pct_missing)) %>%
    head(50)
  plot_title <- "Variable Missingness (Top 50 Variables with Missing Data)"
} else {
  plot_data <- codebook %>%
    arrange(desc(pct_missing))
  plot_title <- "Variable Missingness (All Variables)"
}

# Create bar chart
p1 <- ggplot(plot_data, aes(x = reorder(variable, pct_missing), 
                             y = pct_missing, 
                             fill = health_status)) +
  geom_col() +
  geom_hline(yintercept = c(5, 10, 25), linetype = "dashed", 
             color = "gray40", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = health_colors, name = "Health Status") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = plot_title,
    subtitle = "GHS 2024 Household File",
    x = NULL,
    y = "Percent Missing",
    caption = "Dashed lines at 5%, 10%, and 25% thresholds"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7, family = "mono")
  )

# Save plot 1
ggsave("variable_health_barchart.png", p1, width = 12, height = 10, dpi = 150)
cat("Saved: variable_health_barchart.png\n")

# --- PLOT 2: Health Status Distribution (Pie/Donut Chart) ---

p2 <- ggplot(health_summary, aes(x = 2, y = n_variables, fill = health_status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = health_colors, name = "Health Status") +
  geom_text(aes(label = ifelse(n_variables > 0, 
                                paste0(n_variables, "\n(", 
                                       round(pct_of_total, 1), "%)"), "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  labs(
    title = "Variable Health Distribution",
    subtitle = sprintf("GHS 2024 Household File (%d variables)", n_vars)
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save plot 2
ggsave("variable_health_distribution.png", p2, width = 10, height = 8, dpi = 150)
cat("Saved: variable_health_distribution.png\n")

# --- PLOT 3: Heatmap of Missingness (Variable Groups) ---

# Create variable position bins for heatmap
n_rows <- ceiling(n_vars / 10)  # 10 variables per column

heatmap_data <- codebook %>%
  mutate(
    col_group = ((position - 1) %/% n_rows) + 1,
    row_group = ((position - 1) %% n_rows) + 1
  )

p3 <- ggplot(heatmap_data, aes(x = col_group, y = row_group, fill = pct_missing)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f", pct_missing)), 
            size = 2, color = "white") +
  scale_fill_gradient2(
    low = "#2E7D32", mid = "#FFC107", high = "#B71C1C",
    midpoint = 25, 
    limits = c(0, 100),
    name = "% Missing"
  ) +
  scale_y_reverse() +
  labs(
    title = "Variable Missingness Heatmap",
    subtitle = "Each cell represents one variable (ordered by position)",
    x = "Variable Group",
    y = "Position within Group"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 8)
  )

# Save plot 3
ggsave("variable_health_heatmap.png", p3, width = 12, height = 8, dpi = 150)
cat("Saved: variable_health_heatmap.png\n")

# --- PLOT 4: Cumulative Distribution of Missingness ---

p4 <- codebook %>%
  arrange(pct_missing) %>%
  mutate(cumulative_pct = (row_number() / n()) * 100) %>%
  ggplot(aes(x = pct_missing, y = cumulative_pct)) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_area(fill = "#1976D2", alpha = 0.2) +
  geom_vline(xintercept = c(5, 10, 25), linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = c(25, 50, 75), linetype = "dotted", color = "gray60") +
  annotate("text", x = 5, y = 95, label = "5%", size = 3, hjust = -0.2) +
  annotate("text", x = 10, y = 95, label = "10%", size = 3, hjust = -0.2) +
  annotate("text", x = 25, y = 95, label = "25%", size = 3, hjust = -0.2) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(codebook$pct_missing) + 5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Cumulative Distribution of Variable Missingness",
    subtitle = "What percentage of variables have missingness below each threshold?",
    x = "Percent Missing (Threshold)",
    y = "Cumulative Percent of Variables"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

# Save plot 4
ggsave("variable_health_cumulative.png", p4, width = 10, height = 6, dpi = 150)
cat("Saved: variable_health_cumulative.png\n")

# -----------------------------------------------------------------------------
# SECTION 7: SUMMARY REPORT
# -----------------------------------------------------------------------------

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LAB 5.3 COMPLETE: CODEBOOK GENERATION SUMMARY\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("DATASET SUMMARY:\n")
cat(sprintf("  File: ghs-2024-hhold-v1.dta\n"))
cat(sprintf("  Observations: %s\n", format(n_obs, big.mark = ",")))
cat(sprintf("  Variables: %d\n", n_vars))
cat(sprintf("  Overall Completeness: %.2f%%\n\n", 100 - mean(codebook$pct_missing)))

cat("VARIABLE HEALTH BREAKDOWN:\n")
for (i in 1:nrow(health_summary)) {
  cat(sprintf("  %s: %d variables (%.1f%%)\n",
              health_summary$health_status[i],
              health_summary$n_variables[i],
              health_summary$pct_of_total[i]))
}

cat("\nOUTPUT FILES GENERATED:\n")
cat("  1. ghs2024_codebook.xlsx       - Complete codebook in Excel format\n")
cat("  2. ghs2024_codebook.md         - Codebook in Markdown format\n")
cat("  3. variable_health_barchart.png    - Bar chart of missingness\n")
cat("  4. variable_health_distribution.png - Donut chart of health status\n")
cat("  5. variable_health_heatmap.png     - Heatmap visualization\n")
cat("  6. variable_health_cumulative.png  - Cumulative distribution\n")

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("END OF LAB 5.3\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================
