#===============================================================================
# LAB 2.3: SPATIAL FRAME INTEGRATION
# SADC Regional Training Workshop on Advanced Sampling Methods
# Day 2: Computational Tools and Spatial Integration
#
# Objective: Integrate survey data with geographic shapefiles to:
#   1. Load and explore spatial boundaries
#   2. Merge survey estimates to district polygons
#   3. Create choropleth maps of mean income
#   4. Identify spatial gaps in sample coverage
#
# Data Sources:
#   - ghs-2024-hhold-v1.dta (GHS 2024 household data)
#   - gadm41_ZAF_shp (GADM Level 2 - Districts)
#   - zaf_adm_sadb_ocha_20201109_shp (OCHA Admin Boundaries)
#
# Author: SADC Workshop Team
# Date: March 2026
#===============================================================================

# Clear workspace
rm(list = ls())
cat("\014")

#===============================================================================
# USER CONFIGURATION - ADJUST PATHS AS NEEDED
#===============================================================================

# Set your working directory (uncomment and modify if needed)
# setwd("C:/Users/ENDRI/Dropbox/Work/ja_sadc_workshop_final")

# Data paths - adjust these if your folder structure differs
config <- list(
  # Survey data path
  survey_path = "./data/GHS_2024/ghs-2024-hhold-v1.dta",
  
  # GADM shapefile path (Level 2 = Districts)
  gadm_path = "./data/shapefiles/gadm41_ZAF_shp/gadm41_ZAF_2.shp",
  
  # OCHA shapefile path (Level 2 = Districts)
  ocha_path = "./data/shapefiles/zaf_adm_sadb_ocha_20201109_shp/zaf_admbnda_adm2_sadb_ocha_20201109.shp",
  
  # Output directory for maps and exports
  output_dir = "./outputs/"
)

# Create output directory if it doesn't exist
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

#===============================================================================

#-------------------------------------------------------------------------------
# PART 0: INSTALL AND LOAD REQUIRED PACKAGES
#-------------------------------------------------------------------------------

cat(paste0(rep("=", 72), collapse = ""), "\n")
cat("LAB 2.3: SPATIAL FRAME INTEGRATION\n")
cat(paste0(rep("=", 72), collapse = ""), "\n\n")

# Required packages
required_packages <- c(
  "haven",      # Read STATA files

"dplyr",      # Data manipulation
  "sf",         # Simple features for spatial data
  "tmap",       # Thematic maps
  "ggplot2",   # Plotting
  "viridis",    # Color palettes
  "RColorBrewer", # Additional palettes
  "survey",     # Survey statistics
  "readr"       # CSV export
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

cat("All packages loaded successfully!\n\n")

#-------------------------------------------------------------------------------
# PART 1: LOAD GEOGRAPHIC DATA (SHAPEFILES)
#-------------------------------------------------------------------------------

cat(paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 1: LOADING GEOGRAPHIC DATA\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# -----------------------------
# 1A: Load GADM Level 2 (Districts)
# -----------------------------

# GADM shapefiles provide administrative boundaries
# Level 0 = Country, Level 1 = Province, Level 2 = District

# Check if file exists, provide helpful message if not
if (file.exists(config$gadm_path)) {
  gadm_districts <- st_read(config$gadm_path, quiet = TRUE)
  cat("GADM Level 2 (Districts) loaded successfully!\n")
} else {
  cat("NOTE: GADM shapefile not found at:", config$gadm_path, "\n")
  cat("Please download from: https://gadm.org/download_country.html\n")
  cat("Select: South Africa, Shapefile format, Level 2\n\n")
  
  # Create placeholder for demonstration
  cat("Creating placeholder geometry for demonstration...\n")
  gadm_districts <- NULL
}

# Explore GADM structure
if (!is.null(gadm_districts)) {
  cat("\nGADM Districts Structure:\n")
  cat("  Number of districts:", nrow(gadm_districts), "\n")
  cat("  CRS:", st_crs(gadm_districts)$input, "\n")
  cat("  Variables:", paste(names(gadm_districts)[1:min(8, ncol(gadm_districts))], 
                            collapse = ", "), "\n")
  
  # Preview first few districts
  cat("\nFirst 5 districts:\n")
  print(head(gadm_districts[, c("NAME_1", "NAME_2", "TYPE_2")], 5))
}

# -----------------------------
# 1B: Load OCHA Admin Boundaries
# -----------------------------

if (file.exists(config$ocha_path)) {
  ocha_districts <- st_read(config$ocha_path, quiet = TRUE)
  cat("\nOCHA Admin Boundaries loaded successfully!\n")
} else {
  cat("\nNOTE: OCHA shapefile not found at:", config$ocha_path, "\n")
  cat("Please download from: https://data.humdata.org/dataset/south-africa-admin-level-0-to-3-boundaries\n")
  ocha_districts <- NULL
}

# Explore OCHA structure
if (!is.null(ocha_districts)) {
  cat("\nOCHA Districts Structure:\n")
  cat("  Number of districts:", nrow(ocha_districts), "\n")
  cat("  Variables:", paste(names(ocha_districts)[1:min(8, ncol(ocha_districts))], 
                            collapse = ", "), "\n")
}

#-------------------------------------------------------------------------------
# PART 2: LOAD AND PREPARE SURVEY DATA
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 2: LOADING AND PREPARING SURVEY DATA\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# Load GHS 2024 household data
if (file.exists(config$survey_path)) {
  ghs_hh <- read_dta(config$survey_path)
  cat("GHS 2024 household data loaded!\n")
  cat("  Observations:", nrow(ghs_hh), "\n")
  cat("  Variables:", ncol(ghs_hh), "\n")
} else {
  cat("ERROR: Survey data not found at:", config$survey_path, "\n")
  stop("Cannot proceed without survey data.")
}

# -----------------------------
# 2A: Detect Actual Variable Names (Case-Insensitive)
# -----------------------------

# STATA .dta files can have varying capitalization
# Find actual variable names in the data

cat("Detecting actual variable names in data...\n")
all_vars <- names(ghs_hh)

# Function to find variable (case-insensitive)
find_var <- function(pattern, vars) {
  matches <- grep(pattern, vars, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) return(matches[1])
  return(NULL)
}

# Find key variables
prov_var <- find_var("^prov$", all_vars)
metro_code_var <- find_var("metro_code|metrocode", all_vars)
metro_var <- find_var("^metro$", all_vars)
geotype_var <- find_var("geotype|geo_type", all_vars)
stratum_var <- find_var("^stratum$", all_vars)
psu_var <- find_var("^psu$", all_vars)
weight_var <- find_var("house_wgt|housewgt|hhwgt", all_vars)

cat("\nVariable mapping detected:\n")
cat("  Province:", ifelse(is.null(prov_var), "NOT FOUND", prov_var), "\n")
cat("  Metro_code:", ifelse(is.null(metro_code_var), "NOT FOUND", metro_code_var), "\n")
cat("  Metro:", ifelse(is.null(metro_var), "NOT FOUND", metro_var), "\n")
cat("  GeoType:", ifelse(is.null(geotype_var), "NOT FOUND", geotype_var), "\n")
cat("  Stratum:", ifelse(is.null(stratum_var), "NOT FOUND", stratum_var), "\n")
cat("  PSU:", ifelse(is.null(psu_var), "NOT FOUND", psu_var), "\n")
cat("  Weight:", ifelse(is.null(weight_var), "NOT FOUND", weight_var), "\n")

# Show all variable names for debugging if Metro_code not found
if (is.null(metro_code_var)) {
  cat("\nWARNING: Metro_code variable not found!\n")
  cat("Available variables containing 'metro' or 'geo':\n")
  geo_vars <- grep("metro|geo|prov|district", all_vars, ignore.case = TRUE, value = TRUE)
  print(geo_vars)
  
  cat("\nAll variable names in dataset:\n")
  print(all_vars)
}

# -----------------------------
# 2B: Create Province Labels
# -----------------------------

# Province codes in GHS (prov variable)
province_labels <- c(
  "1" = "Western Cape",
  "2" = "Eastern Cape",
  "3" = "Northern Cape",
  "4" = "Free State",
  "5" = "KwaZulu-Natal",
  "6" = "North West",
  "7" = "Gauteng",
  "8" = "Mpumalanga",
  "9" = "Limpopo"
)

# Standardize variable names for consistent use
# Rename detected variables to standard names
if (!is.null(prov_var) && prov_var != "prov") {
  ghs_hh <- ghs_hh %>% rename(prov = !!sym(prov_var))
}
if (!is.null(psu_var) && psu_var != "psu") {
  ghs_hh <- ghs_hh %>% rename(psu = !!sym(psu_var))
}
if (!is.null(stratum_var) && stratum_var != "stratum") {
  ghs_hh <- ghs_hh %>% rename(stratum = !!sym(stratum_var))
}
if (!is.null(weight_var) && weight_var != "house_wgt") {
  ghs_hh <- ghs_hh %>% rename(house_wgt = !!sym(weight_var))
}
if (!is.null(geotype_var) && geotype_var != "geotype") {
  ghs_hh <- ghs_hh %>% rename(geotype = !!sym(geotype_var))
}

# Add province name
ghs_hh <- ghs_hh %>%
  mutate(
    prov_name = province_labels[as.character(prov)]
  )

# -----------------------------
# 2C: Extract District Information from PSU/Stratum
# -----------------------------

# In GHS, district information may be embedded in PSU codes or stratum
# PSU codes often follow: PPDDDDSSSS format
# PP = Province, DDDD = District/Municipality, SSSS = EA

# Handle Metro_code - create if exists, use province-based fallback if not
if (!is.null(metro_code_var)) {
  # Rename to standard name if different
  if (metro_code_var != "Metro_code") {
    ghs_hh <- ghs_hh %>% rename(Metro_code = !!sym(metro_code_var))
  }
  
  # Create metro_district labels
  ghs_hh <- ghs_hh %>%
    mutate(
      psu_str = as.character(psu),
      psu_prov = substr(psu_str, 1, 1),
      metro_district = case_when(
        Metro_code == 1 ~ "WC Non-metro",
        Metro_code == 2 ~ "City of Cape Town",
        Metro_code == 3 ~ "EC Non-metro",
        Metro_code == 4 ~ "Buffalo City",
        Metro_code == 5 ~ "Nelson Mandela Bay",
        Metro_code == 6 ~ "NC Non-metro",
        Metro_code == 7 ~ "FS Non-metro",
        Metro_code == 8 ~ "Mangaung",
        Metro_code == 9 ~ "KZN Non-metro",
        Metro_code == 10 ~ "eThekwini",
        Metro_code == 11 ~ "NW Non-metro",
        Metro_code == 12 ~ "GP Non-metro",
        Metro_code == 13 ~ "Ekurhuleni",
        Metro_code == 14 ~ "City of Johannesburg",
        Metro_code == 15 ~ "City of Tshwane",
        Metro_code == 16 ~ "MP Non-metro",
        Metro_code == 17 ~ "LP Non-metro",
        TRUE ~ "Unknown"
      )
    )
  cat("\nDistrict/Metro breakdown in survey data:\n")
  print(table(ghs_hh$metro_district))
  
} else {
  # Metro_code not available - use province + Metro (binary) as fallback
  cat("\nUsing province + Metro (binary) as fallback for district classification...\n")
  
  if (!is.null(metro_var)) {
    if (metro_var != "Metro") {
      ghs_hh <- ghs_hh %>% rename(Metro = !!sym(metro_var))
    }
    
    ghs_hh <- ghs_hh %>%
      mutate(
        psu_str = as.character(psu),
        psu_prov = substr(psu_str, 1, 1),
        # Create Metro_code from province + metro status
        Metro_code = prov * 2 - (Metro == 1),  # Creates unique code per prov+metro
        metro_district = paste0(prov_name, " - ", 
                               ifelse(Metro == 1, "Metro", "Non-metro"))
      )
  } else {
    # Neither Metro_code nor Metro available - use province only
    cat("WARNING: No Metro variable found. Using province level only.\n")
    ghs_hh <- ghs_hh %>%
      mutate(
        psu_str = as.character(psu),
        psu_prov = substr(psu_str, 1, 1),
        Metro_code = prov,  # Use province as district proxy
        metro_district = prov_name
      )
  }
  
  cat("\nDistrict classification (fallback method):\n")
  print(table(ghs_hh$metro_district))
}

#-------------------------------------------------------------------------------
# PART 3: CALCULATE DISTRICT-LEVEL ESTIMATES
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 3: CALCULATING DISTRICT-LEVEL ESTIMATES\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# -----------------------------
# 3A: Set Up Survey Design
# -----------------------------

# Define survey design object
ghs_design <- svydesign(
  id = ~psu,
  strata = ~stratum,
  weights = ~house_wgt,
  data = ghs_hh,
  nest = TRUE
)

cat("Survey design object created.\n")

# -----------------------------
# 3B: Calculate Mean Income by Metro/District
# -----------------------------

# Check for income variable
if ("totmhinc" %in% names(ghs_hh)) {
  income_var <- "totmhinc"
} else {
  # Try alternative names
  income_vars <- grep("inc|income|salary", names(ghs_hh), 
                      value = TRUE, ignore.case = TRUE)
  if (length(income_vars) > 0) {
    income_var <- income_vars[1]
    cat("Using income variable:", income_var, "\n")
  } else {
    cat("WARNING: No income variable found. Using household size for demonstration.\n")
    income_var <- "hholdsz"
  }
}

# Calculate mean income by metro/district (using Metro_code)
district_estimates <- ghs_hh %>%
  filter(!is.na(get(income_var)) & get(income_var) < 9999999) %>%
  group_by(Metro_code, metro_district, prov, prov_name) %>%
  summarise(
    n_hh = n(),
    n_psu = n_distinct(psu),
    mean_income = weighted.mean(get(income_var), house_wgt, na.rm = TRUE),
    median_income = median(get(income_var), na.rm = TRUE),
    total_weight = sum(house_wgt),
    .groups = "drop"
  ) %>%
  arrange(prov, Metro_code)

cat("\nDistrict-level estimates calculated:\n")
print(district_estimates)

# -----------------------------
# 3C: Calculate Provincial Estimates (for comparison)
# -----------------------------

provincial_estimates <- ghs_hh %>%
  filter(!is.na(get(income_var)) & get(income_var) < 9999999) %>%
  group_by(prov, prov_name) %>%
  summarise(
    n_hh = n(),
    n_psu = n_distinct(psu),
    n_districts = n_distinct(Metro_code),
    mean_income = weighted.mean(get(income_var), house_wgt, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nProvincial estimates:\n")
print(provincial_estimates)

#-------------------------------------------------------------------------------
# PART 4: MERGE SURVEY DATA TO SHAPEFILES
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 4: MERGING SURVEY DATA TO SHAPEFILES\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# Create provincial-level spatial data for reliable merging
# (District-level requires exact name matching which can be tricky)

# -----------------------------
# 4A: Aggregate GADM to Province Level
# -----------------------------

if (!is.null(gadm_districts)) {
  # Dissolve districts to provinces
  gadm_provinces <- gadm_districts %>%
    group_by(NAME_1) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  cat("Created provincial boundaries from GADM:\n")
  print(gadm_provinces$NAME_1)
  
  # Create merge key
  gadm_provinces <- gadm_provinces %>%
    mutate(
      prov_name_clean = case_when(
        NAME_1 == "Western Cape" ~ "Western Cape",
        NAME_1 == "Eastern Cape" ~ "Eastern Cape",
        NAME_1 == "Northern Cape" ~ "Northern Cape",
        NAME_1 == "Free State" ~ "Free State",
        NAME_1 == "KwaZulu-Natal" ~ "KwaZulu-Natal",
        NAME_1 == "North West" ~ "North West",
        NAME_1 == "North-West" ~ "North West",
        NAME_1 == "Gauteng" ~ "Gauteng",
        NAME_1 == "Mpumalanga" ~ "Mpumalanga",
        NAME_1 == "Limpopo" ~ "Limpopo",
        TRUE ~ NAME_1
      )
    )
  
  # Merge with survey estimates
  provinces_with_data <- gadm_provinces %>%
    left_join(provincial_estimates, 
              by = c("prov_name_clean" = "prov_name"))
  
  cat("\nMerge successful! Provinces with survey data:\n")
  print(st_drop_geometry(provinces_with_data[, c("NAME_1", "n_hh", "mean_income")]))
}

# -----------------------------
# 4B: District-Level Merge (Using Metro_code mapping)
# -----------------------------

# For districts, we need to create a crosswalk between Metro_code and GADM district names
# This is approximate since GHS uses metro/non-metro rather than full district detail

if (!is.null(gadm_districts)) {
  # Create metro area polygons by identifying metro municipalities
  metro_names <- c(
    "City of Cape Town",
    "Buffalo City", 
    "Nelson Mandela Bay",
    "Mangaung",
    "eThekwini",
    "Ekurhuleni",
    "City of Johannesburg",
    "City of Tshwane"
  )
  
  # Try to identify metros in GADM data
  gadm_districts <- gadm_districts %>%
    mutate(
      is_metro = NAME_2 %in% metro_names | 
                 grepl("Metro|City of", NAME_2, ignore.case = TRUE)
    )
  
  cat("\nMetro municipalities identified in GADM:\n")
  metros_found <- gadm_districts %>%
    filter(is_metro) %>%
    st_drop_geometry() %>%
    select(NAME_1, NAME_2)
  print(metros_found)
}

#-------------------------------------------------------------------------------
# PART 5: CREATE CHOROPLETH MAPS
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 5: CREATING CHOROPLETH MAPS\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# -----------------------------
# 5A: Provincial Mean Income Map (tmap)
# -----------------------------

if (!is.null(gadm_districts) && exists("provinces_with_data")) {
  
  # Set tmap mode to plot (static)
  tmap_mode("plot")
  
  # Create provincial choropleth
  map_provincial <- tm_shape(provinces_with_data) +
    tm_polygons(
      col = "mean_income",
      palette = "YlOrRd",
      style = "quantile",
      n = 5,
      title = "Mean Monthly\nIncome (R)",
      border.col = "white",
      lwd = 1
    ) +
    tm_layout(
      main.title = "Zambara: Mean Household Income by Province",
      main.title.size = 1.2,
      main.title.position = "center",
      legend.position = c("left", "bottom"),
      legend.bg.color = "white",
      legend.bg.alpha = 0.8,
      frame = FALSE
    ) +
    tm_compass(position = c("right", "top"), size = 1.5) +
    tm_scale_bar(position = c("right", "bottom"))
  
  # Save map
  tmap_save(map_provincial, paste0(config$output_dir, "lab2_3_provincial_income_map.png"), 
            width = 10, height = 8, dpi = 300)
  cat("Provincial income map saved: lab2_3_provincial_income_map.png\n")
  
  # Display
  print(map_provincial)
}

# -----------------------------
# 5B: District-Level Map (with coverage indicators)
# -----------------------------

if (!is.null(gadm_districts)) {
  
  # For district-level, show sample coverage (n_psu per district)
  # First, calculate PSU counts by province (since we don't have full district mapping)
  
  psu_by_province_map <- ghs_hh %>%
    group_by(prov, prov_name) %>%
    summarise(
      n_psu = n_distinct(psu),
      n_hh = n(),
      .groups = "drop"
    )
  
  # Merge to spatial data
  if (exists("provinces_with_data")) {
    provinces_coverage <- provinces_with_data %>%
      left_join(psu_by_province_map %>% select(prov_name, n_psu_check = n_psu),
                by = c("prov_name_clean" = "prov_name"))
    
    # Sample coverage map
    map_coverage <- tm_shape(provinces_coverage) +
      tm_polygons(
        col = "n_psu",
        palette = "Blues",
        style = "jenks",
        n = 5,
        title = "Number of\nPSUs Sampled",
        border.col = "gray40",
        lwd = 0.5
      ) +
      tm_layout(
        main.title = "Zambara: Sample PSU Coverage by Province",
        main.title.size = 1.2,
        legend.position = c("left", "bottom"),
        frame = FALSE
      ) +
      tm_text("NAME_1", size = 0.6, col = "black")
    
    tmap_save(map_coverage, paste0(config$output_dir, "lab2_3_psu_coverage_map.png"),
              width = 10, height = 8, dpi = 300)
    cat("PSU coverage map saved: lab2_3_psu_coverage_map.png\n")
  }
}

# -----------------------------
# 5C: Alternative ggplot2 Map
# -----------------------------

if (!is.null(gadm_districts) && exists("provinces_with_data")) {
  
  gg_income_map <- ggplot(provinces_with_data) +
    geom_sf(aes(fill = mean_income), color = "white", size = 0.3) +
    scale_fill_viridis_c(
      name = "Mean Monthly\nIncome (R)",
      labels = scales::comma,
      option = "plasma"
    ) +
    labs(
      title = "Zambara: Mean Household Income by Province",
      subtitle = "Source: General Household Survey 2024",
      caption = "Note: Income in South African Rand"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  ggsave(paste0(config$output_dir, "lab2_3_income_map_ggplot.png"), gg_income_map, 
         width = 10, height = 8, dpi = 300)
  cat("ggplot income map saved: lab2_3_income_map_ggplot.png\n")
}

#-------------------------------------------------------------------------------
# PART 6: IDENTIFY SPATIAL GAPS IN SAMPLE COVERAGE
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 6: IDENTIFYING SPATIAL GAPS\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# -----------------------------
# 6A: Check District-Level Coverage
# -----------------------------

if (!is.null(gadm_districts)) {
  
  # Get all districts from GADM
  all_districts <- gadm_districts %>%
    st_drop_geometry() %>%
    select(NAME_1, NAME_2, TYPE_2) %>%
    distinct() %>%
    arrange(NAME_1, NAME_2)
  
  cat("Total districts in GADM shapefile:", nrow(all_districts), "\n\n")
  
  # Districts per province
  districts_per_province <- all_districts %>%
    group_by(NAME_1) %>%
    summarise(n_districts = n())
  
  cat("Districts per province:\n")
  print(districts_per_province)
}

# -----------------------------
# 6B: Identify Zero-Coverage Areas
# -----------------------------

# At Metro_code level, check for any codes with zero sample
all_metro_codes <- 1:17
sampled_metro_codes <- unique(ghs_hh$Metro_code)
missing_metro_codes <- setdiff(all_metro_codes, sampled_metro_codes)

cat("\n\nSample Coverage Analysis:\n")
cat("  Total Metro/District codes:", length(all_metro_codes), "\n")
cat("  Codes with sample:", length(sampled_metro_codes), "\n")
cat("  Codes WITHOUT sample:", length(missing_metro_codes), "\n")

if (length(missing_metro_codes) > 0) {
  cat("\nMISSING Metro codes:\n")
  print(missing_metro_codes)
} else {
  cat("\nAll Metro codes have sample coverage!\n")
}

# -----------------------------
# 6C: PSU Distribution Check
# -----------------------------

# Calculate PSU counts by province (needed for coverage analysis)
psu_by_province <- ghs_hh %>%
  group_by(prov, prov_name) %>%
  summarise(
    n_psu = n_distinct(psu),
    n_hh = n(),
    .groups = "drop"
  )

# Check for provinces with low PSU counts
psu_threshold <- 50  # Minimum recommended PSUs

low_coverage_provinces <- psu_by_province %>%
  filter(n_psu < psu_threshold)

if (nrow(low_coverage_provinces) > 0) {
  cat("\n\nWARNING: Provinces with < ", psu_threshold, " PSUs:\n")
  print(low_coverage_provinces)
} else {
  cat("\nAll provinces have adequate PSU coverage (>= ", psu_threshold, " PSUs)\n")
}

# -----------------------------
# 6D: Geographic Coverage Report
# -----------------------------

# Check if geotype variable exists
if ("geotype" %in% names(ghs_hh)) {
  coverage_report <- ghs_hh %>%
    group_by(prov, prov_name, geotype) %>%
    summarise(
      n_hh = n(),
      n_psu = n_distinct(psu),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = geotype,
      values_from = c(n_hh, n_psu),
      values_fill = 0
    )
  
  cat("\n\nCoverage by Province and Geography Type:\n")
  print(coverage_report)
  
  # Check for zero cells
  cat("\nNote: Zero values indicate potential spatial gaps in sampling frame.\n")
} else {
  # Create simpler coverage report without geotype
  coverage_report <- ghs_hh %>%
    group_by(prov, prov_name) %>%
    summarise(
      n_hh = n(),
      n_psu = n_distinct(psu),
      .groups = "drop"
    )
  
  cat("\n\nCoverage by Province (geotype variable not available):\n")
  print(coverage_report)
}

#-------------------------------------------------------------------------------
# PART 7: CREATE SPATIAL GAPS MAP
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 7: SPATIAL GAPS VISUALIZATION\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

if (!is.null(gadm_districts) && exists("provinces_with_data")) {
  
  # Add coverage flag
  provinces_with_data <- provinces_with_data %>%
    mutate(
      coverage_status = case_when(
        is.na(n_hh) | n_hh == 0 ~ "No Sample",
        n_psu < 50 ~ "Low Coverage",
        n_psu < 100 ~ "Moderate Coverage",
        TRUE ~ "Good Coverage"
      ),
      coverage_status = factor(coverage_status, 
                               levels = c("No Sample", "Low Coverage", 
                                          "Moderate Coverage", "Good Coverage"))
    )
  
  # Create coverage status map
  map_gaps <- tm_shape(provinces_with_data) +
    tm_polygons(
      col = "coverage_status",
      palette = c("No Sample" = "red", 
                  "Low Coverage" = "orange",
                  "Moderate Coverage" = "yellow",
                  "Good Coverage" = "green"),
      title = "Coverage Status",
      border.col = "gray30",
      lwd = 1
    ) +
    tm_layout(
      main.title = "Zambara: Sample Coverage Status",
      main.title.size = 1.2,
      legend.position = c("left", "bottom"),
      frame = FALSE
    ) +
    tm_text("NAME_1", size = 0.5)
  
  tmap_save(map_gaps, paste0(config$output_dir, "lab2_3_coverage_gaps_map.png"),
            width = 10, height = 8, dpi = 300)
  cat("Coverage gaps map saved: lab2_3_coverage_gaps_map.png\n")
}

#-------------------------------------------------------------------------------
# PART 8: EXPORT RESULTS
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("-", 72), collapse = ""), "\n")
cat("PART 8: EXPORTING RESULTS\n")
cat(paste0(rep("-", 72), collapse = ""), "\n\n")

# Export district estimates
write_csv(district_estimates, paste0(config$output_dir, "lab2_3_district_estimates.csv"))
cat("District estimates exported: lab2_3_district_estimates.csv\n")

# Export provincial estimates
write_csv(provincial_estimates, paste0(config$output_dir, "lab2_3_provincial_estimates.csv"))
cat("Provincial estimates exported: lab2_3_provincial_estimates.csv\n")

# Export coverage report
write_csv(coverage_report, paste0(config$output_dir, "lab2_3_coverage_report.csv"))
cat("Coverage report exported: lab2_3_coverage_report.csv\n")

# Summary statistics
summary_stats <- data.frame(
  Metric = c("Total Households", "Total PSUs", "Total Strata",
             "Provinces Covered", "Metro Areas Covered",
             "Min PSUs per Province", "Max PSUs per Province"),
  Value = c(
    nrow(ghs_hh),
    n_distinct(ghs_hh$psu),
    n_distinct(ghs_hh$stratum),
    n_distinct(ghs_hh$prov),
    n_distinct(ghs_hh$Metro_code),
    min(psu_by_province$n_psu),
    max(psu_by_province$n_psu)
  )
)

write_csv(summary_stats, paste0(config$output_dir, "lab2_3_summary_stats.csv"))
cat("Summary statistics exported: lab2_3_summary_stats.csv\n")

#-------------------------------------------------------------------------------
# SUMMARY
#-------------------------------------------------------------------------------

cat("\n", paste0(rep("=", 72), collapse = ""), "\n")
cat("LAB 2.3 COMPLETE: SPATIAL FRAME INTEGRATION\n")
cat(paste0(rep("=", 72), collapse = ""), "\n\n")

cat("Key Findings:\n")
cat("  1. Survey data successfully loaded:", nrow(ghs_hh), "households\n")
cat("  2. PSUs distributed across", n_distinct(ghs_hh$prov), "provinces\n")
cat("  3. Metro/District codes covered:", n_distinct(ghs_hh$Metro_code), "of 17\n")

if (length(missing_metro_codes) > 0) {
  cat("  4. SPATIAL GAPS IDENTIFIED:", length(missing_metro_codes), "metro codes missing\n")
} else {
  cat("  4. No major spatial gaps detected at metro/district level\n")
}

cat("\nOutputs Generated:\n")
cat("  - lab2_3_provincial_income_map.png\n")
cat("  - lab2_3_psu_coverage_map.png\n")
cat("  - lab2_3_income_map_ggplot.png\n")
cat("  - lab2_3_coverage_gaps_map.png\n")
cat("  - lab2_3_district_estimates.csv\n")
cat("  - lab2_3_provincial_estimates.csv\n")
cat("  - lab2_3_coverage_report.csv\n")
cat("  - lab2_3_summary_stats.csv\n")

cat("\n", paste0(rep("=", 72), collapse = ""), "\n")
cat("END OF LAB 2.3\n")
cat(paste0(rep("=", 72), collapse = ""), "\n")
