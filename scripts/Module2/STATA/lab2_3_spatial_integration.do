*===============================================================================
* LAB 2.3: SPATIAL FRAME INTEGRATION
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 2: Computational Tools and Spatial Integration
*
* Objective: Integrate survey data with geographic shapefiles to:
*   1. Load and explore spatial boundaries
*   2. Merge survey estimates to district polygons
*   3. Create choropleth maps of mean income
*   4. Identify spatial gaps in sample coverage
*
* Data Sources:
*   - ghs-2024-hhold-v1.dta (GHS 2024 household data)
*   - gadm41_ZAF_shp (GADM Level 2 - Districts)
*   - zaf_adm_sadb_ocha_20201109_shp (OCHA Admin Boundaries)
*
* Required User-Written Commands:
*   - shp2dta (from SSC)
*   - spmap (from SSC)
*   - geo2xy (from SSC) [optional]
*
* Author: SADC Workshop Team
* Date: March 2026
*===============================================================================

clear all
set more off
capture log close

*===============================================================================
* USER CONFIGURATION - ADJUST PATHS AS NEEDED
*===============================================================================

* Set your working directory (uncomment and modify if needed)
* cd "C:/Users/ENDRI/Dropbox/Work/ja_sadc_workshop_final"

* Define file paths - adjust these if your folder structure differs
global survey_path  "./data/GHS_2024/ghs-2024-hhold-v1.dta"
global gadm_shp     "./data/shapefiles/gadm41_ZAF_shp/gadm41_ZAF_2.shp"
global ocha_shp     "./data/shapefiles/zaf_adm_sadb_ocha_20201109_shp/zaf_admbnda_adm2_sadb_ocha_20201109.shp"
global output_dir   "./outputs/"

* Create output directory if it doesn't exist
capture mkdir "$output_dir"

*===============================================================================

* Start log
log using "${output_dir}lab2_3_spatial_integration.log", replace

display _n
display "=" * 72
display "LAB 2.3: SPATIAL FRAME INTEGRATION (STATA)"
display "=" * 72
display _n

*-------------------------------------------------------------------------------
* PART 0: INSTALL REQUIRED PACKAGES
*-------------------------------------------------------------------------------

display "-" * 72
display "PART 0: CHECKING/INSTALLING REQUIRED PACKAGES"
display "-" * 72
display _n

* Check and install required packages
capture which shp2dta
if _rc != 0 {
    display "Installing shp2dta..."
    ssc install shp2dta, replace
}

capture which spmap
if _rc != 0 {
    display "Installing spmap..."
    ssc install spmap, replace
}

capture which palettes
if _rc != 0 {
    display "Installing palettes..."
    ssc install palettes, replace
}

capture which colrspace
if _rc != 0 {
    display "Installing colrspace..."
    ssc install colrspace, replace
}

display "All required packages available."

*-------------------------------------------------------------------------------
* PART 1: CONVERT SHAPEFILES TO STATA FORMAT
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 1: CONVERTING SHAPEFILES TO STATA FORMAT"
display "-" * 72
display _n

* -----------------------------
* 1A: Convert GADM Level 2 Shapefile
* -----------------------------

* Check if shapefile exists
capture confirm file "$gadm_shp"
if _rc == 0 {
    display "Converting GADM shapefile..."
    
    * Convert shapefile to STATA format
    * This creates two files: _shp.dta (coordinates) and _db.dta (attributes)
    shp2dta using "$gadm_shp", ///
        database(gadm_districts_db) ///
        coordinates(gadm_districts_shp) ///
        genid(district_id) ///
        replace
    
    display "GADM shapefile converted successfully!"
    
    * Explore the database file
    use gadm_districts_db, clear
    describe, short
    
    display _n "First 10 districts:"
    list NAME_1 NAME_2 TYPE_2 in 1/10
    
    display _n "Number of districts: " _N
    
    * Create province summary
    tab NAME_1
}
else {
    display "NOTE: GADM shapefile not found at: $gadm_shp"
    display "Please download from: https://gadm.org/download_country.html"
    display "Select: South Africa, Shapefile format, Level 2"
}

* -----------------------------
* 1B: Convert OCHA Admin Boundaries
* -----------------------------

capture confirm file "$ocha_shp"
if _rc == 0 {
    display _n "Converting OCHA shapefile..."
    
    shp2dta using "$ocha_shp", ///
        database(ocha_districts_db) ///
        coordinates(ocha_districts_shp) ///
        genid(ocha_id) ///
        replace
    
    display "OCHA shapefile converted successfully!"
    
    use ocha_districts_db, clear
    describe, short
}
else {
    display _n "NOTE: OCHA shapefile not found at: $ocha_shp"
}

*-------------------------------------------------------------------------------
* PART 2: LOAD AND PREPARE SURVEY DATA
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 2: LOADING AND PREPARING SURVEY DATA"
display "-" * 72
display _n

* Load GHS 2024 household data
capture confirm file "$survey_path"
if _rc == 0 {
    use "$survey_path", clear
    display "GHS 2024 household data loaded!"
    display "  Observations: " _N
    describe, short
}
else {
    display "ERROR: Survey data not found at: $survey_path"
    exit 601
}

* -----------------------------
* 2A: Detect and Standardize Variable Names
* -----------------------------

* STATA variable names are case-sensitive
* Check what variables actually exist

display _n "Detecting variable names in dataset..."
describe, simple

* Try to find Metro_code (check various capitalizations)
capture confirm variable Metro_code
if _rc != 0 {
    capture confirm variable metro_code
    if _rc == 0 {
        rename metro_code Metro_code
        display "Renamed metro_code to Metro_code"
    }
    else {
        capture confirm variable METRO_CODE
        if _rc == 0 {
            rename METRO_CODE Metro_code
            display "Renamed METRO_CODE to Metro_code"
        }
        else {
            display "WARNING: Metro_code variable not found!"
            display "Will use province-level analysis only."
            gen Metro_code = prov  // Fallback: use province as district
        }
    }
}

* Standardize other key variables
capture confirm variable Prov
if _rc == 0 {
    rename Prov prov
    display "Renamed Prov to prov"
}

capture confirm variable PROV
if _rc == 0 {
    rename PROV prov
    display "Renamed PROV to prov"
}

capture confirm variable Stratum
if _rc == 0 {
    rename Stratum stratum
    display "Renamed Stratum to stratum"
}

capture confirm variable GeoType
if _rc == 0 {
    rename GeoType geotype
    display "Renamed GeoType to geotype"
}

capture confirm variable PSU
if _rc == 0 {
    rename PSU psu
    display "Renamed PSU to psu"
}

* -----------------------------
* 2B: Create Province Labels
* -----------------------------

* Province codes (1-9)
label define prov_lbl ///
    1 "Western Cape" ///
    2 "Eastern Cape" ///
    3 "Northern Cape" ///
    4 "Free State" ///
    5 "KwaZulu-Natal" ///
    6 "North West" ///
    7 "Gauteng" ///
    8 "Mpumalanga" ///
    9 "Limpopo"

label values prov prov_lbl

* -----------------------------
* 2B: Create Metro/District Labels
* -----------------------------

* Metro_code provides province + metro breakdown
label define metro_lbl ///
    1 "WC Non-metro" ///
    2 "City of Cape Town" ///
    3 "EC Non-metro" ///
    4 "Buffalo City" ///
    5 "Nelson Mandela Bay" ///
    6 "NC Non-metro" ///
    7 "FS Non-metro" ///
    8 "Mangaung" ///
    9 "KZN Non-metro" ///
    10 "eThekwini" ///
    11 "NW Non-metro" ///
    12 "GP Non-metro" ///
    13 "Ekurhuleni" ///
    14 "City of Johannesburg" ///
    15 "City of Tshwane" ///
    16 "MP Non-metro" ///
    17 "LP Non-metro"

label values Metro_code metro_lbl

display _n "Metro/District distribution in survey:"
tab Metro_code

*-------------------------------------------------------------------------------
* PART 3: CALCULATE DISTRICT-LEVEL ESTIMATES
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 3: CALCULATING DISTRICT-LEVEL ESTIMATES"
display "-" * 72
display _n

* -----------------------------
* 3A: Set Up Survey Design
* -----------------------------

svyset psu [pw=house_wgt], strata(stratum)
display "Survey design specified."

* -----------------------------
* 3B: Calculate Mean Income by Metro/District
* -----------------------------

* Check for income variable
capture confirm variable totmhinc
if _rc == 0 {
    local income_var "totmhinc"
}
else {
    * Try alternative
    capture confirm variable hholdsz
    if _rc == 0 {
        local income_var "hholdsz"
        display "WARNING: Using household size as demo variable"
    }
}

display "Using income variable: `income_var'"

* Clean income variable (remove missing codes)
gen income_clean = `income_var'
replace income_clean = . if income_clean >= 9999999

* Calculate weighted means by Metro_code
preserve

collapse (mean) mean_income = income_clean ///
         (median) median_income = income_clean ///
         (sum) total_weight = house_wgt ///
         (count) n_hh = income_clean, ///
         by(Metro_code prov)

* Count unique PSUs (need to do separately)
display _n "District-level estimates:"
list, sep(5)

* Save district estimates
save lab2_3_district_estimates, replace
export delimited using "${output_dir}lab2_3_district_estimates.csv", replace

restore

* -----------------------------
* 3C: Calculate Provincial Estimates
* -----------------------------

preserve

* Get PSU counts
bysort prov: egen n_psu = nvals(psu)

collapse (mean) mean_income = income_clean ///
         (count) n_hh = income_clean ///
         (first) n_psu, ///
         by(prov)

display _n "Provincial estimates:"
list

* Save provincial estimates
save lab2_3_provincial_estimates, replace
export delimited using "${output_dir}lab2_3_provincial_estimates.csv", replace

restore

*-------------------------------------------------------------------------------
* PART 4: MERGE SURVEY DATA TO SHAPEFILES
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 4: MERGING SURVEY DATA TO SHAPEFILES"
display "-" * 72
display _n

* -----------------------------
* 4A: Aggregate Districts to Province Level in Shapefile
* -----------------------------

capture confirm file "gadm_districts_db.dta"
if _rc == 0 {
    
    * Load GADM database
    use gadm_districts_db, clear
    
    * Create province ID for merging
    gen prov = .
    replace prov = 1 if NAME_1 == "Western Cape"
    replace prov = 2 if NAME_1 == "Eastern Cape"
    replace prov = 3 if NAME_1 == "Northern Cape"
    replace prov = 4 if NAME_1 == "Free State"
    replace prov = 5 if NAME_1 == "KwaZulu-Natal"
    replace prov = 6 if NAME_1 == "North West" | NAME_1 == "North-West"
    replace prov = 7 if NAME_1 == "Gauteng"
    replace prov = 8 if NAME_1 == "Mpumalanga"
    replace prov = 9 if NAME_1 == "Limpopo"
    
    * Save mapping
    keep district_id NAME_1 NAME_2 prov
    save gadm_province_mapping, replace
    
    display "Province mapping created."
    tab NAME_1 prov, missing
}

* -----------------------------
* 4B: Merge Provincial Estimates to Shapefile
* -----------------------------

capture confirm file "gadm_districts_db.dta"
if _rc == 0 {
    
    use gadm_districts_db, clear
    
    * Add province code
    gen prov = .
    replace prov = 1 if NAME_1 == "Western Cape"
    replace prov = 2 if NAME_1 == "Eastern Cape"
    replace prov = 3 if NAME_1 == "Northern Cape"
    replace prov = 4 if NAME_1 == "Free State"
    replace prov = 5 if NAME_1 == "KwaZulu-Natal"
    replace prov = 6 if NAME_1 == "North West" | NAME_1 == "North-West"
    replace prov = 7 if NAME_1 == "Gauteng"
    replace prov = 8 if NAME_1 == "Mpumalanga"
    replace prov = 9 if NAME_1 == "Limpopo"
    
    * Merge with provincial estimates
    merge m:1 prov using lab2_3_provincial_estimates, nogenerate
    
    * Check merge
    display _n "Merge results - districts with survey data:"
    tab NAME_1 if !missing(mean_income)
    
    * Save merged database
    save gadm_districts_merged, replace
    
    display "Merged database saved: gadm_districts_merged.dta"
}

*-------------------------------------------------------------------------------
* PART 5: CREATE CHOROPLETH MAPS
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 5: CREATING CHOROPLETH MAPS"
display "-" * 72
display _n

* -----------------------------
* 5A: Provincial Mean Income Map
* -----------------------------

capture confirm file "gadm_districts_merged.dta"
if _rc == 0 {
    
    use gadm_districts_merged, clear
    
    * Create income categories for mapping
    xtile income_cat = mean_income, nq(5)
    label define income_cat_lbl 1 "Lowest" 2 "Low" 3 "Medium" 4 "High" 5 "Highest"
    label values income_cat income_cat_lbl
    
    save gadm_districts_merged, replace
    
    * Create choropleth map
    spmap mean_income using gadm_districts_shp, ///
        id(district_id) ///
        clmethod(quantile) clnumber(5) ///
        fcolor(YlOrRd) ///
        ocolor(white ..) osize(thin ..) ///
        legend(pos(7) size(small)) ///
        legtitle("Mean Monthly" "Income (R)") ///
        title("Zambara: Mean Household Income by Province", size(medium)) ///
        subtitle("Source: General Household Survey 2024", size(small)) ///
        note("Note: Districts colored by provincial mean income", size(vsmall))
    
    graph export "${output_dir}lab2_3_provincial_income_map.png", replace width(2000)
    display "Provincial income map saved: lab2_3_provincial_income_map.png"
}
else {
    display "Shapefile database not available for mapping."
}

* -----------------------------
* 5B: PSU Coverage Map
* -----------------------------

capture confirm file "gadm_districts_merged.dta"
if _rc == 0 {
    
    use gadm_districts_merged, clear
    
    * Create PSU coverage map
    spmap n_psu using gadm_districts_shp, ///
        id(district_id) ///
        clmethod(custom) clbreaks(0 100 200 300 400 600) ///
        fcolor(Blues) ///
        ocolor(gray ..) osize(thin ..) ///
        legend(pos(7) size(small)) ///
        legtitle("Number of" "PSUs Sampled") ///
        title("Zambara: Sample PSU Coverage by Province", size(medium)) ///
        note("Note: Higher values indicate more sample points", size(vsmall))
    
    graph export "${output_dir}lab2_3_psu_coverage_map.png", replace width(2000)
    display "PSU coverage map saved: lab2_3_psu_coverage_map.png"
}

*-------------------------------------------------------------------------------
* PART 6: IDENTIFY SPATIAL GAPS IN SAMPLE COVERAGE
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 6: IDENTIFYING SPATIAL GAPS"
display "-" * 72
display _n

* Reload survey data
use "$survey_path", clear

* Clean income
gen income_clean = `income_var'
replace income_clean = . if income_clean >= 9999999

* -----------------------------
* 6A: Check Metro/District Coverage
* -----------------------------

display "Sample Coverage Analysis:"
display "-" * 40

* Expected metro codes (1-17)
forvalues i = 1/17 {
    quietly count if Metro_code == `i'
    if r(N) == 0 {
        display "  Metro code `i': NO SAMPLE (SPATIAL GAP!)"
    }
    else {
        display "  Metro code `i': " r(N) " households"
    }
}

* Summary
tab Metro_code, missing

* -----------------------------
* 6B: PSU Distribution by Province
* -----------------------------

display _n "PSU Distribution by Province:"
display "-" * 40

* Calculate PSU counts
preserve
bysort prov: egen n_psu = nvals(psu)
collapse (first) n_psu, by(prov)

* Check for low coverage
gen low_coverage = (n_psu < 50)
label define low_lbl 0 "Adequate" 1 "LOW COVERAGE"
label values low_coverage low_lbl

list prov n_psu low_coverage, sep(0)

* Flag provinces with low coverage
count if low_coverage == 1
if r(N) > 0 {
    display _n "WARNING: " r(N) " provinces have < 50 PSUs!"
}
else {
    display _n "All provinces have adequate PSU coverage (>= 50 PSUs)"
}

restore

* -----------------------------
* 6C: Coverage by Geography Type
* -----------------------------

display _n "Coverage by Province and Geography Type:"
display "-" * 40

tab prov geotype, missing

* Check for zero cells
preserve
collapse (count) n_hh = psu, by(prov geotype)

* Identify gaps
list if n_hh == 0, sep(0)

count if n_hh == 0
if r(N) > 0 {
    display _n "WARNING: " r(N) " province-geotype combinations have NO SAMPLE!"
}
restore

*-------------------------------------------------------------------------------
* PART 7: CREATE SPATIAL GAPS MAP
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 7: SPATIAL GAPS VISUALIZATION"
display "-" * 72
display _n

capture confirm file "gadm_districts_merged.dta"
if _rc == 0 {
    
    use gadm_districts_merged, clear
    
    * Create coverage status variable
    gen coverage_status = .
    replace coverage_status = 1 if missing(n_hh) | n_hh == 0
    replace coverage_status = 2 if n_psu < 50 & coverage_status == .
    replace coverage_status = 3 if n_psu >= 50 & n_psu < 100 & coverage_status == .
    replace coverage_status = 4 if n_psu >= 100 & coverage_status == .
    
    label define coverage_lbl ///
        1 "No Sample" ///
        2 "Low Coverage (<50 PSUs)" ///
        3 "Moderate (50-99 PSUs)" ///
        4 "Good Coverage (100+ PSUs)"
    label values coverage_status coverage_lbl
    
    save gadm_districts_merged, replace
    
    * Create coverage status map
    spmap coverage_status using gadm_districts_shp, ///
        id(district_id) ///
        clmethod(unique) ///
        fcolor(red orange yellow green) ///
        ocolor(gray ..) osize(thin ..) ///
        legend(pos(7) size(small)) ///
        legtitle("Coverage" "Status") ///
        title("Zambara: Sample Coverage Status", size(medium)) ///
        note("Red = No sample, Green = Good coverage", size(vsmall))
    
    graph export "${output_dir}lab2_3_coverage_gaps_map.png", replace width(2000)
    display "Coverage gaps map saved: lab2_3_coverage_gaps_map.png"
}

*-------------------------------------------------------------------------------
* PART 8: EXPORT SUMMARY REPORT
*-------------------------------------------------------------------------------

display _n
display "-" * 72
display "PART 8: EXPORTING SUMMARY REPORT"
display "-" * 72
display _n

* Reload survey data for final summary
use "$survey_path", clear

* Calculate summary statistics
quietly count
local total_hh = r(N)

quietly tab psu
local total_psu = r(r)

quietly tab stratum
local total_strata = r(r)

quietly tab prov
local total_prov = r(r)

quietly tab Metro_code
local total_metro = r(r)

* Create summary dataset
clear
set obs 7
gen str40 Metric = ""
gen Value = .

replace Metric = "Total Households" in 1
replace Value = `total_hh' in 1

replace Metric = "Total PSUs" in 2
replace Value = `total_psu' in 2

replace Metric = "Total Strata" in 3
replace Value = `total_strata' in 3

replace Metric = "Provinces Covered" in 4
replace Value = `total_prov' in 4

replace Metric = "Metro Areas Covered" in 5
replace Value = `total_metro' in 5

replace Metric = "Expected Metro Codes" in 6
replace Value = 17 in 6

replace Metric = "Metro Code Coverage Rate" in 7
replace Value = round(`total_metro'/17*100, 0.1) in 7

list, sep(0)

export delimited using "${output_dir}lab2_3_summary_stats.csv", replace
display "Summary statistics exported: lab2_3_summary_stats.csv"

*-------------------------------------------------------------------------------
* SUMMARY
*-------------------------------------------------------------------------------

display _n
display "=" * 72
display "LAB 2.3 COMPLETE: SPATIAL FRAME INTEGRATION"
display "=" * 72
display _n

display "Key Findings:"
display "  1. Survey data successfully loaded: `total_hh' households"
display "  2. PSUs distributed across `total_prov' provinces"
display "  3. Metro/District codes covered: `total_metro' of 17"
display _n

display "Outputs Generated:"
display "  - lab2_3_provincial_income_map.png"
display "  - lab2_3_psu_coverage_map.png"
display "  - lab2_3_coverage_gaps_map.png"
display "  - lab2_3_district_estimates.dta / .csv"
display "  - lab2_3_provincial_estimates.dta / .csv"
display "  - lab2_3_summary_stats.csv"
display "  - gadm_districts_merged.dta"
display _n

display "=" * 72
display "END OF LAB 2.3"
display "=" * 72

log close
