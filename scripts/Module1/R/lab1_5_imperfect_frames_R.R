#===============================================================================
# LAB 1.5: IMPERFECT SAMPLING FRAMES
# Identifying Frame Mismatches Between Survey Data and Administrative Boundaries
#===============================================================================
#
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 1, Module 1.6: Handling Imperfect Frames
#
# Objective: Identify PSUs in GHS 2024 data that do not match administrative
#            boundary shapefiles, simulating a common frame quality issue.
#
# Data Sources:
#   - GHS 2024 Household Survey (Stats SA)
#   - GADM Administrative Boundaries (Provinces) - gadm41_ZAF_shp
#   - OCHA Administrative Boundaries (Districts) - zaf_adm_sadb_ocha_20201109_shp
#
# Author: SADC Workshop Team
# Date: March 2026
#===============================================================================

#-------------------------------------------------------------------------------
# 0. SETUP AND PACKAGE INSTALLATION
#-------------------------------------------------------------------------------

# Required packages
required_packages <- c(
  "sf",           # Simple features for spatial data

"haven",        # Read STATA files
  "dplyr",        # Data manipulation
  "tidyr",        # Data reshaping
  "stringr",      # String manipulation
  "readr",        # CSV writing
  "ggplot2",      # Visualization
  "knitr"         # Report generation
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LAB 1.5: IMPERFECT SAMPLING FRAMES - FRAME MISMATCH ANALYSIS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

#-------------------------------------------------------------------------------
# 1. DEFINE FILE PATHS
#-------------------------------------------------------------------------------

# Set your working directory and file paths
# UPDATE THESE PATHS to match your local setup

# Survey data
ghs_data_path <- "ghs-2024-hhold-v1.dta"

# Shapefiles (download from sources if not available)
# GADM: https://gadm.org/download_country.html (Select South Africa, Level 1)
# OCHA: https://data.humdata.org/dataset/south-africa-admin-level-1-boundaries

gadm_shapefile <- "gadm41_ZAF_shp/gadm41_ZAF_1.shp"      # Province level
ocha_shapefile <- "zaf_adm_sadb_ocha_20201109_shp/zaf_admbnda_adm2_sadb_ocha_20201109.shp"  # District level

# Output directory
output_dir <- "lab1_5_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir)

cat("File paths configured.\n")
cat("  Survey data:", ghs_data_path, "\n")
cat("  GADM shapefile:", gadm_shapefile, "\n")
cat("  OCHA shapefile:", ocha_shapefile, "\n\n")

#-------------------------------------------------------------------------------
# 2. LOAD SURVEY DATA
#-------------------------------------------------------------------------------

cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 2: Loading GHS 2024 Survey Data\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Load household data
if (file.exists(ghs_data_path)) {
  ghs_hh <- haven::read_dta(ghs_data_path)
  cat("Loaded GHS 2024 household data:", nrow(ghs_hh), "observations\n\n")
} else {
  # Create simulated data for demonstration
  cat("NOTE: GHS data file not found. Creating simulated data for demonstration.\n\n")
  
  set.seed(20260306)
  
  # Simulate PSU and geographic data based on GHS structure
  # Province codes: 1=WC, 2=EC, 3=NC, 4=FS, 5=KZN, 6=NW, 7=GP, 8=MP, 9=LP
  province_info <- data.frame(
    prov = 1:9,
    prov_name = c("Western Cape", "Eastern Cape", "Northern Cape", 
                  "Free State", "KwaZulu-Natal", "North West",
                  "Gauteng", "Mpumalanga", "Limpopo"),
    n_psu = c(380, 420, 95, 180, 650, 220, 890, 250, 340)
  )
  
  # Generate PSU data
  psu_list <- list()
  for (i in 1:nrow(province_info)) {
    prov <- province_info$prov[i]
    n_psu <- province_info$n_psu[i]
    
    # PSU codes: PPMMMNNN (Province, Metro, Number)
    psu_codes <- sprintf("%d%02d%04d", prov, 
                         sample(1:20, n_psu, replace = TRUE),
                         1:n_psu)
    
    # District codes (simulated municipality codes)
    district_codes <- sprintf("%d%02d", prov, sample(1:8, n_psu, replace = TRUE))
    
    psu_list[[i]] <- data.frame(
      psu = psu_codes,
      prov = prov,
      district = district_codes,
      geotype = sample(1:3, n_psu, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      stringsAsFactors = FALSE
    )
  }
  
  psu_frame <- do.call(rbind, psu_list)
  
  # Expand to household level (6-8 HH per PSU)
  hh_per_psu <- sample(5:8, nrow(psu_frame), replace = TRUE)
  
  ghs_hh <- psu_frame[rep(1:nrow(psu_frame), hh_per_psu), ]
  ghs_hh$uqnr <- 1:nrow(ghs_hh)  # Unique household ID
  
  # Introduce frame problems for demonstration
  # Problem 1: Some PSUs have invalid province codes
  problem_idx1 <- sample(1:nrow(ghs_hh), 50)
  ghs_hh$prov[problem_idx1] <- 99  # Invalid province
  
  # Problem 2: Some PSUs have mismatched province-district
  problem_idx2 <- sample(setdiff(1:nrow(ghs_hh), problem_idx1), 100)
  ghs_hh$district[problem_idx2] <- sprintf("%d%02d", 
                                            sample(1:9, 100, replace = TRUE),
                                            sample(1:8, 100, replace = TRUE))
  
  # Problem 3: Some PSU codes are malformed
  problem_idx3 <- sample(setdiff(1:nrow(ghs_hh), c(problem_idx1, problem_idx2)), 30)
  ghs_hh$psu[problem_idx3] <- paste0("X", substr(ghs_hh$psu[problem_idx3], 2, 8))
  
  cat("Created simulated data:", nrow(ghs_hh), "households in", 
      length(unique(ghs_hh$psu)), "PSUs\n\n")
}

# Extract unique PSUs and their geographic attributes
psu_geography <- ghs_hh %>%
  group_by(psu) %>%
  summarise(
    prov = first(prov),
    district = if ("district" %in% names(.)) first(district) else NA,
    geotype = if ("geotype" %in% names(.)) first(geotype) else NA,
    n_households = n(),
    .groups = "drop"
  )

cat("Extracted", nrow(psu_geography), "unique PSUs\n")
cat("Province distribution:\n")
print(table(psu_geography$prov, useNA = "ifany"))
cat("\n")

#-------------------------------------------------------------------------------
# 3. LOAD ADMINISTRATIVE BOUNDARY SHAPEFILES
#-------------------------------------------------------------------------------

cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 3: Loading Administrative Boundary Shapefiles\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Function to safely load shapefile
load_shapefile <- function(path, name) {
  if (file.exists(path)) {
    shp <- sf::st_read(path, quiet = TRUE)
    cat("Loaded", name, ":", nrow(shp), "features\n")
    return(shp)
  } else {
    cat("WARNING:", name, "not found at:", path, "\n")
    cat("         Creating simulated boundaries for demonstration.\n")
    return(NULL)
  }
}

# Load GADM provinces
gadm_provinces <- load_shapefile(gadm_shapefile, "GADM Provinces")

# Load OCHA districts
ocha_districts <- load_shapefile(ocha_shapefile, "OCHA Districts")

# If shapefiles not available, create reference data
if (is.null(gadm_provinces)) {
  cat("\nCreating reference province data from GADM structure...\n")
  
  gadm_provinces_ref <- data.frame(
    GID_1 = paste0("ZAF.", 1:9, "_1"),
    NAME_1 = c("Western Cape", "Eastern Cape", "Northern Cape",
               "Free State", "KwaZulu-Natal", "North West",
               "Gauteng", "Mpumalanga", "Limpopo"),
    VARNAME_1 = c("Wes-Kaap", "Oos-Kaap", "Noord-Kaap",
                  "Vrystaat", "KwaZulu-Natal", "Noordwes",
                  "Gauteng", "Mpumalanga", "Limpopo"),
    prov_code = 1:9,
    stringsAsFactors = FALSE
  )
  
  cat("Created reference data for", nrow(gadm_provinces_ref), "provinces\n")
} else {
  # Extract province reference from actual shapefile
  gadm_provinces_ref <- gadm_provinces %>%
    sf::st_drop_geometry() %>%
    select(any_of(c("GID_1", "NAME_1", "VARNAME_1"))) %>%
    mutate(prov_code = row_number())
}

if (is.null(ocha_districts)) {
  cat("\nCreating reference district data from OCHA structure...\n")
  
  # South Africa has 52 district municipalities
  # Distribution: WC=6, EC=8, NC=5, FS=5, KZN=11, NW=4, GP=5, MP=4, LP=4 = 52
  district_counts <- c(6, 8, 5, 5, 11, 4, 5, 4, 4)  # Sum = 52
  
  ocha_districts_ref <- data.frame(
    ADM2_PCODE = sprintf("ZA%02d", 1:52),
    ADM2_EN = paste("District", 1:52),
    ADM1_PCODE = sprintf("ZA%d", rep(1:9, district_counts)),
    ADM1_EN = rep(c("Western Cape", "Eastern Cape", "Northern Cape",
                    "Free State", "KwaZulu-Natal", "North West",
                    "Gauteng", "Mpumalanga", "Limpopo"),
                  district_counts),
    stringsAsFactors = FALSE
  )
  
  cat("Created reference data for", nrow(ocha_districts_ref), "districts\n")
} else {
  ocha_districts_ref <- ocha_districts %>%
    sf::st_drop_geometry() %>%
    select(any_of(c("ADM2_PCODE", "ADM2_EN", "ADM1_PCODE", "ADM1_EN")))
}

cat("\n")

#-------------------------------------------------------------------------------
# 4. FRAME MATCHING ANALYSIS
#-------------------------------------------------------------------------------

cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 4: Frame Matching Analysis\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Initialize problem tracking
frame_problems <- list()

#--- 4.1 Province Code Validation ---
cat("4.1 Validating Province Codes...\n")

valid_prov_codes <- gadm_provinces_ref$prov_code
invalid_prov_psu <- psu_geography %>%
  filter(!(prov %in% valid_prov_codes))

if (nrow(invalid_prov_psu) > 0) {
  cat("  PROBLEM FOUND:", nrow(invalid_prov_psu), 
      "PSUs with invalid province codes\n")
  cat("  Invalid codes found:", 
      paste(unique(invalid_prov_psu$prov), collapse = ", "), "\n")
  
  frame_problems$invalid_province <- invalid_prov_psu %>%
    mutate(
      problem_type = "Invalid Province Code",
      problem_detail = paste("Province code", prov, "not in GADM reference"),
      severity = "HIGH"
    )
} else {
  cat("  OK: All province codes are valid\n")
}

#--- 4.2 PSU Code Format Validation ---
cat("\n4.2 Validating PSU Code Format...\n")

# Expected format: numeric, 8+ digits, starting with province code
psu_format_check <- psu_geography %>%
  mutate(
    is_numeric = grepl("^[0-9]+$", psu),
    correct_length = nchar(psu) >= 7 & nchar(psu) <= 10,
    starts_with_prov = substr(psu, 1, 1) == as.character(prov)
  )

malformed_psu <- psu_format_check %>%
  filter(!is_numeric | !correct_length | !starts_with_prov)

if (nrow(malformed_psu) > 0) {
  cat("  PROBLEM FOUND:", nrow(malformed_psu), "PSUs with malformed codes\n")
  
  # Categorize malformation types
  not_numeric <- sum(!psu_format_check$is_numeric)
  wrong_length <- sum(!psu_format_check$correct_length)
  wrong_prefix <- sum(!psu_format_check$starts_with_prov)
  
  cat("    - Non-numeric codes:", not_numeric, "\n")
  cat("    - Wrong length:", wrong_length, "\n")
  cat("    - Province prefix mismatch:", wrong_prefix, "\n")
  
  frame_problems$malformed_psu <- malformed_psu %>%
    mutate(
      problem_type = "Malformed PSU Code",
      problem_detail = case_when(
        !is_numeric ~ "PSU code contains non-numeric characters",
        !correct_length ~ paste("PSU code length", nchar(psu), "outside 7-10 range"),
        !starts_with_prov ~ paste("PSU prefix", substr(psu, 1, 1), 
                                  "doesn't match province", prov),
        TRUE ~ "Multiple format issues"
      ),
      severity = "MEDIUM"
    ) %>%
    select(-is_numeric, -correct_length, -starts_with_prov)
} else {
  cat("  OK: All PSU codes are properly formatted\n")
}

#--- 4.3 Province-District Consistency ---
cat("\n4.3 Checking Province-District Consistency...\n")

if ("district" %in% names(psu_geography) && !all(is.na(psu_geography$district))) {
  
  # Extract province from district code (first digit)
  district_province_check <- psu_geography %>%
    filter(!is.na(district)) %>%
    mutate(
      district_prov = as.numeric(substr(district, 1, 1)),
      prov_match = (district_prov == prov)
    )
  
  mismatched_district <- district_province_check %>%
    filter(!prov_match | is.na(prov_match))
  
  if (nrow(mismatched_district) > 0) {
    cat("  PROBLEM FOUND:", nrow(mismatched_district), 
        "PSUs with province-district mismatch\n")
    
    frame_problems$district_mismatch <- mismatched_district %>%
      mutate(
        problem_type = "Province-District Mismatch",
        problem_detail = paste("PSU in province", prov, 
                               "but district code", district,
                               "implies province", district_prov),
        severity = "HIGH"
      ) %>%
      select(-district_prov, -prov_match)
  } else {
    cat("  OK: All district codes consistent with province\n")
  }
} else {
  cat("  SKIPPED: No district information available\n")
}

#--- 4.4 Geographic Type Validation ---
cat("\n4.4 Validating Geographic Type Codes...\n")

if ("geotype" %in% names(psu_geography)) {
  valid_geotypes <- c(1, 2, 3)  # 1=Urban, 2=Traditional, 3=Farms
  
  invalid_geotype <- psu_geography %>%
    filter(!(geotype %in% valid_geotypes) | is.na(geotype))
  
  if (nrow(invalid_geotype) > 0) {
    cat("  PROBLEM FOUND:", nrow(invalid_geotype), 
        "PSUs with invalid geotype codes\n")
    
    frame_problems$invalid_geotype <- invalid_geotype %>%
      mutate(
        problem_type = "Invalid Geography Type",
        problem_detail = paste("Geotype", geotype, "not in valid set {1,2,3}"),
        severity = "LOW"
      )
  } else {
    cat("  OK: All geography type codes are valid\n")
  }
} else {
  cat("  SKIPPED: No geotype information available\n")
}

#--- 4.5 Duplicate PSU Detection ---
cat("\n4.5 Checking for Duplicate PSU Codes...\n")

# Check if same PSU appears in multiple provinces
psu_province_combos <- ghs_hh %>%
  group_by(psu) %>%
  summarise(
    n_provinces = n_distinct(prov),
    provinces = paste(unique(prov), collapse = ","),
    .groups = "drop"
  ) %>%
  filter(n_provinces > 1)

if (nrow(psu_province_combos) > 0) {
  cat("  PROBLEM FOUND:", nrow(psu_province_combos), 
      "PSU codes appearing in multiple provinces\n")
  
  frame_problems$duplicate_psu <- psu_province_combos %>%
    mutate(
      problem_type = "Duplicate PSU Across Provinces",
      problem_detail = paste("PSU", psu, "found in provinces:", provinces),
      severity = "CRITICAL"
    )
} else {
  cat("  OK: No duplicate PSU codes across provinces\n")
}

cat("\n")

#-------------------------------------------------------------------------------
# 5. COMPILE PROBLEM REPORT
#-------------------------------------------------------------------------------

cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 5: Compiling Problem Report\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# Combine all problems
if (length(frame_problems) > 0) {
  
  problem_report <- bind_rows(frame_problems, .id = "problem_category") %>%
    select(
      problem_category,
      problem_type,
      severity,
      psu,
      prov,
      any_of(c("district", "geotype")),
      problem_detail,
      n_households
    ) %>%
    arrange(
      factor(severity, levels = c("CRITICAL", "HIGH", "MEDIUM", "LOW")),
      problem_category,
      psu
    )
  
  #--- Summary Statistics ---
  cat("FRAME QUALITY SUMMARY\n")
  cat("=" %>% rep(50) %>% paste(collapse = ""), "\n\n")
  
  summary_by_severity <- problem_report %>%
    group_by(severity) %>%
    summarise(
      n_psu = n_distinct(psu),
      n_households = sum(n_households, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_by_type <- problem_report %>%
    group_by(problem_type) %>%
    summarise(
      n_psu = n_distinct(psu),
      n_households = sum(n_households, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Problems by Severity:\n")
  print(as.data.frame(summary_by_severity), row.names = FALSE)
  cat("\n")
  
  cat("Problems by Type:\n")
  print(as.data.frame(summary_by_type), row.names = FALSE)
  cat("\n")
  
  # Calculate impact
  total_psu <- nrow(psu_geography)
  total_hh <- nrow(ghs_hh)
  affected_psu <- n_distinct(problem_report$psu)
  affected_hh <- sum(problem_report$n_households, na.rm = TRUE)
  
  cat("IMPACT ASSESSMENT:\n")
  cat("  Total PSUs in frame:", total_psu, "\n")
  cat("  PSUs with problems:", affected_psu, 
      sprintf("(%.1f%%)", 100 * affected_psu / total_psu), "\n")
  cat("  Total households:", total_hh, "\n")
  cat("  Households in problem PSUs:", affected_hh,
      sprintf("(%.1f%%)", 100 * affected_hh / total_hh), "\n\n")
  
  #--- Export Problem Report ---
  report_path <- file.path(output_dir, "frame_mismatch_report.csv")
  write_csv(problem_report, report_path)
  cat("Problem report exported to:", report_path, "\n")
  
  #--- Export Summary ---
  summary_path <- file.path(output_dir, "frame_quality_summary.csv")
  
  quality_summary <- data.frame(
    metric = c("Total PSUs", "Problem PSUs", "Problem Rate (%)",
               "Total Households", "Affected Households", "Affected Rate (%)",
               "Critical Issues", "High Issues", "Medium Issues", "Low Issues"),
    value = c(total_psu, affected_psu, round(100 * affected_psu / total_psu, 2),
              total_hh, affected_hh, round(100 * affected_hh / total_hh, 2),
              sum(problem_report$severity == "CRITICAL"),
              sum(problem_report$severity == "HIGH"),
              sum(problem_report$severity == "MEDIUM"),
              sum(problem_report$severity == "LOW"))
  )
  
  write_csv(quality_summary, summary_path)
  cat("Quality summary exported to:", summary_path, "\n\n")
  
} else {
  cat("NO FRAME PROBLEMS DETECTED\n")
  cat("All PSUs match administrative boundary references.\n\n")
  
  quality_summary <- data.frame(
    metric = c("Total PSUs", "Problem PSUs", "Problem Rate (%)"),
    value = c(nrow(psu_geography), 0, 0)
  )
}

#-------------------------------------------------------------------------------
# 6. VISUALIZATIONS
#-------------------------------------------------------------------------------

cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 6: Generating Visualizations\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

if (length(frame_problems) > 0) {
  
  #--- Problem Distribution by Province ---
  prov_problems <- problem_report %>%
    filter(prov %in% 1:9) %>%
    group_by(prov) %>%
    summarise(n_problems = n(), .groups = "drop") %>%
    right_join(
      data.frame(prov = 1:9, 
                 prov_name = c("WC", "EC", "NC", "FS", "KZN", 
                               "NW", "GP", "MP", "LP")),
      by = "prov"
    ) %>%
    mutate(n_problems = replace_na(n_problems, 0))
  
  p1 <- ggplot(prov_problems, aes(x = reorder(prov_name, -n_problems), 
                                   y = n_problems)) +
    geom_bar(stat = "identity", fill = "#003366", alpha = 0.8) +
    geom_text(aes(label = n_problems), vjust = -0.5, size = 3) +
    labs(
      title = "Frame Problems by Province",
      subtitle = "Number of PSUs with identified issues",
      x = "Province",
      y = "Number of Problem PSUs"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(file.path(output_dir, "problems_by_province.png"), p1, 
         width = 8, height = 5, dpi = 150)
  cat("Saved: problems_by_province.png\n")
  
  #--- Problem Distribution by Type ---
  p2 <- ggplot(summary_by_type, aes(x = reorder(problem_type, n_psu), 
                                     y = n_psu)) +
    geom_bar(stat = "identity", fill = "#D4AF37", alpha = 0.8) +
    geom_text(aes(label = n_psu), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(
      title = "Frame Problems by Type",
      subtitle = "Number of PSUs affected",
      x = "",
      y = "Number of PSUs"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12))
  
  ggsave(file.path(output_dir, "problems_by_type.png"), p2, 
         width = 8, height = 4, dpi = 150)
  cat("Saved: problems_by_type.png\n")
  
  #--- Severity Distribution ---
  severity_colors <- c("CRITICAL" = "#8B0000", "HIGH" = "#DC143C",
                       "MEDIUM" = "#FFA500", "LOW" = "#228B22")
  
  p3 <- ggplot(summary_by_severity, 
               aes(x = factor(severity, levels = c("CRITICAL", "HIGH", 
                                                    "MEDIUM", "LOW")),
                   y = n_psu, fill = severity)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = severity_colors) +
    geom_text(aes(label = n_psu), vjust = -0.5, size = 4) +
    labs(
      title = "Frame Problems by Severity",
      x = "Severity Level",
      y = "Number of PSUs"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "none"
    )
  
  ggsave(file.path(output_dir, "problems_by_severity.png"), p3, 
         width = 6, height = 5, dpi = 150)
  cat("Saved: problems_by_severity.png\n")
}

cat("\n")

#-------------------------------------------------------------------------------
# 7. RECOMMENDED REMEDIATION ACTIONS
#-------------------------------------------------------------------------------

cat("-" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("STEP 7: Recommended Remediation Actions\n")
cat("-" %>% rep(70) %>% paste(collapse = ""), "\n\n")

if (length(frame_problems) > 0) {
  
  cat("Based on the identified frame problems, we recommend:\n\n")
  
  if ("duplicate_psu" %in% names(frame_problems)) {
    cat("CRITICAL - Duplicate PSUs Across Provinces:\n")
    cat("  Action: Investigate data entry errors or PSU code reuse\n")
    cat("  Impact: May cause incorrect stratification and weighting\n\n")
  }
  
  if ("invalid_province" %in% names(frame_problems)) {
    cat("HIGH - Invalid Province Codes:\n")
    cat("  Action: Cross-reference with original sampling frame\n")
    cat("  Action: Contact field teams for correction\n")
    cat("  Impact: Cannot properly stratify or weight these PSUs\n\n")
  }
  
  if ("district_mismatch" %in% names(frame_problems)) {
    cat("HIGH - Province-District Mismatches:\n")
    cat("  Action: Verify district assignments against OCHA boundaries\n")
    cat("  Action: Check for boundary changes since frame construction\n")
    cat("  Impact: May affect domain estimation and variance calculation\n\n")
  }
  
  if ("malformed_psu" %in% names(frame_problems)) {
    cat("MEDIUM - Malformed PSU Codes:\n")
    cat("  Action: Apply standardized PSU code formatting rules\n")
    cat("  Action: Create crosswalk to correct codes\n")
    cat("  Impact: May cause matching failures in analysis\n\n")
  }
  
  if ("invalid_geotype" %in% names(frame_problems)) {
    cat("LOW - Invalid Geography Types:\n")
    cat("  Action: Assign based on centroid location or dominant land use\n")
    cat("  Impact: Minor effect on geotype-specific analyses\n\n")
  }
  
} else {
  cat("No remediation actions required - frame quality is good.\n\n")
}

#-------------------------------------------------------------------------------
# 8. SESSION INFO AND CLEANUP
#-------------------------------------------------------------------------------

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("LAB 1.5 COMPLETE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("Output files saved to:", output_dir, "\n")
cat("  - frame_mismatch_report.csv (detailed problem list)\n")
cat("  - frame_quality_summary.csv (summary statistics)\n")
cat("  - problems_by_province.png\n")
cat("  - problems_by_type.png\n")
cat("  - problems_by_severity.png\n\n")

cat("Session Info:\n")
cat("  R version:", R.version$version.string, "\n")
cat("  Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

#-------------------------------------------------------------------------------
# END OF SCRIPT
#-------------------------------------------------------------------------------
