*===============================================================================
* LAB 1.5: IMPERFECT SAMPLING FRAMES
* Identifying Frame Mismatches Between Survey Data and Administrative Boundaries
*===============================================================================
*
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 1, Module 1.6: Handling Imperfect Frames
*
* Objective: Identify PSUs in GHS 2024 data that do not match administrative
*            boundary shapefiles, simulating a common frame quality issue.
*
* Data Sources:
*   - GHS 2024 Household Survey (Stats SA)
*   - GADM Administrative Boundaries (Provinces) - gadm41_ZAF_shp
*   - OCHA Administrative Boundaries (Districts) - zaf_adm_sadb_ocha_20201109_shp
*
* Requirements:
*   - STATA 15+ (for spshape commands) or shp2dta package
*   - spmap package for visualization (optional)
*
* Author: SADC Workshop Team
* Date: March 2026
*===============================================================================

clear all
set more off
capture log close

*-------------------------------------------------------------------------------
* 0. SETUP AND CONFIGURATION
*-------------------------------------------------------------------------------

* Set working directory (UPDATE THIS PATH)
* cd "C:/Users/YourName/SADC_Workshop/Lab1_5"

* Create output directory
capture mkdir "lab1_5_outputs"

* Start log file
log using "lab1_5_outputs/lab1_5_imperfect_frames.log", replace text

display as result "========================================================================"
display as result "LAB 1.5: IMPERFECT SAMPLING FRAMES - FRAME MISMATCH ANALYSIS"
display as result "========================================================================"
display ""

*-------------------------------------------------------------------------------
* 1. DEFINE FILE PATHS
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 1: Defining File Paths"
display as text "------------------------------------------------------------------------"
display ""

* Survey data path
global ghs_data "ghs-2024-hhold-v1.dta"

* Shapefile paths (convert to STATA format using shp2dta if needed)
global gadm_shp "gadm41_ZAF_shp/gadm41_ZAF_1"
global ocha_shp "zaf_adm_sadb_ocha_20201109_shp/zaf_admbnda_adm2_sadb_ocha_20201109"

* Output directory
global outdir "lab1_5_outputs"

display "File paths configured."
display "  Survey data: $ghs_data"
display "  GADM shapefile: $gadm_shp"
display "  OCHA shapefile: $ocha_shp"
display ""

*-------------------------------------------------------------------------------
* 2. CONVERT SHAPEFILES TO STATA FORMAT (if not already done)
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 2: Processing Shapefiles"
display as text "------------------------------------------------------------------------"
display ""

* Check if shp2dta is installed
capture which shp2dta
if _rc != 0 {
    display as error "Note: shp2dta not installed. Install with: ssc install shp2dta"
    display as text "Proceeding with reference data instead of actual shapefiles."
}

* Try to convert GADM shapefile
capture confirm file "${gadm_shp}.shp"
if _rc == 0 {
    display "Converting GADM shapefile to STATA format..."
    capture shp2dta using "${gadm_shp}", ///
        database("${outdir}/gadm_provinces_db") ///
        coordinates("${outdir}/gadm_provinces_xy") ///
        replace
    
    if _rc == 0 {
        display "  Successfully converted GADM provinces"
    }
    else {
        display "  Could not convert GADM shapefile"
    }
}
else {
    display "GADM shapefile not found. Creating reference data."
}

* Try to convert OCHA shapefile
capture confirm file "${ocha_shp}.shp"
if _rc == 0 {
    display "Converting OCHA shapefile to STATA format..."
    capture shp2dta using "${ocha_shp}", ///
        database("${outdir}/ocha_districts_db") ///
        coordinates("${outdir}/ocha_districts_xy") ///
        replace
    
    if _rc == 0 {
        display "  Successfully converted OCHA districts"
    }
    else {
        display "  Could not convert OCHA shapefile"
    }
}
else {
    display "OCHA shapefile not found. Creating reference data."
}

display ""

*-------------------------------------------------------------------------------
* 3. CREATE REFERENCE DATA FOR PROVINCES AND DISTRICTS
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 3: Creating Administrative Reference Data"
display as text "------------------------------------------------------------------------"
display ""

* Province reference (GADM structure)
clear
input byte prov_code str20 prov_name str15 prov_abbrev str30 gadm_gid
    1 "Western Cape"    "WC"  "ZAF.1_1"
    2 "Eastern Cape"    "EC"  "ZAF.2_1"
    3 "Northern Cape"   "NC"  "ZAF.3_1"
    4 "Free State"      "FS"  "ZAF.4_1"
    5 "KwaZulu-Natal"   "KZN" "ZAF.5_1"
    6 "North West"      "NW"  "ZAF.6_1"
    7 "Gauteng"         "GP"  "ZAF.7_1"
    8 "Mpumalanga"      "MP"  "ZAF.8_1"
    9 "Limpopo"         "LP"  "ZAF.9_1"
end

save "${outdir}/province_reference.dta", replace
display "Created province reference: 9 provinces"

* District reference (simplified OCHA structure)
* South Africa has 52 district municipalities across 9 provinces
* Distribution: WC=6, EC=8, NC=5, FS=5, KZN=11, NW=4, GP=5, MP=4, LP=4 = 52
clear
input byte prov_code byte district_num str5 district_code str30 district_name
    1  1  "DC1"   "West Coast"
    1  2  "DC2"   "Cape Winelands"
    1  3  "DC3"   "Overberg"
    1  4  "DC4"   "Eden"
    1  5  "DC5"   "Central Karoo"
    1  6  "CPT"   "City of Cape Town"
    2  1  "DC10"  "Cacadu"
    2  2  "DC12"  "Amathole"
    2  3  "DC13"  "Chris Hani"
    2  4  "DC14"  "Joe Gqabi"
    2  5  "DC15"  "OR Tambo"
    2  6  "DC44"  "Alfred Nzo"
    2  7  "BUF"   "Buffalo City"
    2  8  "NMA"   "Nelson Mandela Bay"
    3  1  "DC6"   "Namakwa"
    3  2  "DC7"   "Pixley ka Seme"
    3  3  "DC8"   "Siyanda"
    3  4  "DC9"   "Frances Baard"
    3  5  "DC45"  "John Taolo Gaetsewe"
    4  1  "DC16"  "Xhariep"
    4  2  "DC18"  "Lejweleputswa"
    4  3  "DC19"  "Thabo Mofutsanyana"
    4  4  "DC20"  "Fezile Dabi"
    4  5  "MAN"   "Mangaung"
    5  1  "DC21"  "Ugu"
    5  2  "DC22"  "uMgungundlovu"
    5  3  "DC23"  "Uthukela"
    5  4  "DC24"  "Umzinyathi"
    5  5  "DC25"  "Amajuba"
    5  6  "DC26"  "Zululand"
    5  7  "DC27"  "Umkhanyakude"
    5  8  "DC28"  "King Cetshwayo"
    5  9  "DC29"  "iLembe"
    5  10 "DC43"  "Harry Gwala"
    5  11 "ETH"   "eThekwini"
    6  1  "DC37"  "Bojanala Platinum"
    6  2  "DC38"  "Ngaka Modiri Molema"
    6  3  "DC39"  "Dr Ruth Segomotsi Mompati"
    6  4  "DC40"  "Dr Kenneth Kaunda"
    7  1  "DC42"  "Sedibeng"
    7  2  "DC48"  "West Rand"
    7  3  "EKU"   "Ekurhuleni"
    7  4  "JHB"   "City of Johannesburg"
    7  5  "TSH"   "City of Tshwane"
    8  1  "DC30"  "Gert Sibande"
    8  2  "DC31"  "Nkangala"
    8  3  "DC32"  "Ehlanzeni"
    8  4  "MBM"   "Mbombela"
    9  1  "DC33"  "Mopani"
    9  2  "DC34"  "Vhembe"
    9  3  "DC35"  "Capricorn"
    9  4  "DC47"  "Waterberg-Sekhukhune"
end

save "${outdir}/district_reference.dta", replace
display "Created district reference: `=_N' districts"
display ""

*-------------------------------------------------------------------------------
* 4. LOAD SURVEY DATA
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 4: Loading GHS 2024 Survey Data"
display as text "------------------------------------------------------------------------"
display ""

* Try to load actual GHS data
capture confirm file "$ghs_data"
if _rc == 0 {
    use "$ghs_data", clear
    display "Loaded GHS 2024 household data: `=_N' observations"
}
else {
    * Create simulated data for demonstration
    display "NOTE: GHS data file not found. Creating simulated data for demonstration."
    display ""
    
    clear
    set seed 20260306
    
    * Generate PSU-level data first
    set obs 3425  // Approximate number of PSUs
    
    gen psu_id = _n
    
    * Assign provinces (weighted by population)
    gen double u = runiform()
    gen byte prov = .
    replace prov = 1 if u <= 0.115          // WC 11.5%
    replace prov = 2 if u <= 0.220 & prov==. // EC 10.5%
    replace prov = 3 if u <= 0.250 & prov==. // NC 3%
    replace prov = 4 if u <= 0.305 & prov==. // FS 5.5%
    replace prov = 5 if u <= 0.505 & prov==. // KZN 20%
    replace prov = 6 if u <= 0.575 & prov==. // NW 7%
    replace prov = 7 if u <= 0.835 & prov==. // GP 26%
    replace prov = 8 if u <= 0.910 & prov==. // MP 7.5%
    replace prov = 9 if prov == .             // LP 9%
    drop u
    
    * Generate PSU codes (format: PPMMMNNN)
    gen metro_code = floor(runiform() * 20) + 1
    gen psu_num = floor(runiform() * 9999) + 1
    gen str10 psu = string(prov) + string(metro_code, "%02.0f") + string(psu_num, "%04.0f")
    
    * Generate district codes (format: PDD)
    gen district_num = floor(runiform() * 8) + 1
    gen str5 district = string(prov) + string(district_num, "%02.0f")
    
    * Geography type
    gen byte geotype = 1 + floor(runiform() * 3)
    replace geotype = 1 if runiform() < 0.6  // Urban bias
    
    * Expand to household level (6-8 HH per PSU)
    gen hh_count = 5 + floor(runiform() * 4)
    expand hh_count
    bysort psu_id: gen hh_num = _n
    gen uqnr = _n
    drop hh_count hh_num psu_id metro_code psu_num district_num
    
    * Introduce frame problems for demonstration
    * Problem 1: Invalid province codes (about 50 HH)
    gen double rand1 = runiform()
    replace prov = 99 if rand1 < 0.002
    
    * Problem 2: Province-district mismatch (about 100 HH)
    gen double rand2 = runiform()
    replace district = string(floor(runiform()*9)+1) + string(floor(runiform()*8)+1, "%02.0f") ///
        if rand2 < 0.004 & rand1 >= 0.002
    
    * Problem 3: Malformed PSU codes (about 30 HH)
    gen double rand3 = runiform()
    replace psu = "X" + substr(psu, 2, .) if rand3 < 0.001 & rand1 >= 0.002 & rand2 >= 0.004
    
    drop rand1 rand2 rand3
    
    display "Created simulated data: `=_N' households"
}

* Display data summary
display ""
display "Data Summary:"
tab prov, missing
display ""

*-------------------------------------------------------------------------------
* 5. EXTRACT UNIQUE PSUs AND GEOGRAPHIC ATTRIBUTES
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 5: Extracting PSU Geography"
display as text "------------------------------------------------------------------------"
display ""

* Save full household data
preserve

* Collapse to PSU level
collapse (first) prov district geotype (count) n_households=uqnr, by(psu)

display "Extracted `=_N' unique PSUs"
display ""

* Save PSU-level data
save "${outdir}/psu_geography.dta", replace

*-------------------------------------------------------------------------------
* 6. FRAME MATCHING ANALYSIS
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 6: Frame Matching Analysis"
display as text "------------------------------------------------------------------------"
display ""

* Initialize problem tracking variables
gen byte problem_invalid_prov = 0
gen byte problem_malformed_psu = 0
gen byte problem_district_mismatch = 0
gen byte problem_invalid_geotype = 0
gen str100 problem_detail = ""
gen str10 severity = ""

*--- 6.1 Province Code Validation ---
display as result "6.1 Validating Province Codes..."

* Valid province codes are 1-9
replace problem_invalid_prov = 1 if prov < 1 | prov > 9 | prov == .
replace problem_detail = "Province code " + string(prov) + " not in valid range 1-9" ///
    if problem_invalid_prov == 1
replace severity = "HIGH" if problem_invalid_prov == 1

count if problem_invalid_prov == 1
local n_invalid_prov = r(N)
if `n_invalid_prov' > 0 {
    display as error "  PROBLEM FOUND: `n_invalid_prov' PSUs with invalid province codes"
    tab prov if problem_invalid_prov == 1
}
else {
    display as text "  OK: All province codes are valid"
}
display ""

*--- 6.2 PSU Code Format Validation ---
display as result "6.2 Validating PSU Code Format..."

* Check for non-numeric characters
gen byte is_numeric = regexm(psu, "^[0-9]+$")

* Check length (should be 7-10 digits)
gen byte psu_len = strlen(psu)
gen byte correct_length = (psu_len >= 7 & psu_len <= 10)

* Check province prefix matches
gen byte psu_prefix = real(substr(psu, 1, 1))
gen byte prefix_matches = (psu_prefix == prov)

* Flag malformed PSUs
replace problem_malformed_psu = 1 if is_numeric == 0 | correct_length == 0 | prefix_matches == 0
replace problem_detail = "PSU code format error" if problem_malformed_psu == 1 & problem_detail == ""
replace severity = "MEDIUM" if problem_malformed_psu == 1 & severity == ""

count if problem_malformed_psu == 1
local n_malformed = r(N)
if `n_malformed' > 0 {
    display as error "  PROBLEM FOUND: `n_malformed' PSUs with malformed codes"
    count if is_numeric == 0
    display "    - Non-numeric codes: `r(N)'"
    count if correct_length == 0
    display "    - Wrong length: `r(N)'"
    count if prefix_matches == 0
    display "    - Province prefix mismatch: `r(N)'"
}
else {
    display as text "  OK: All PSU codes are properly formatted"
}

drop is_numeric psu_len correct_length psu_prefix prefix_matches
display ""

*--- 6.3 Province-District Consistency ---
display as result "6.3 Checking Province-District Consistency..."

* Extract province from district code (first digit)
gen byte district_prov = real(substr(district, 1, 1))
gen byte prov_district_match = (district_prov == prov)

replace problem_district_mismatch = 1 if prov_district_match == 0 & district != ""
replace problem_detail = "District " + district + " implies prov " + string(district_prov) + ///
    " but PSU in prov " + string(prov) if problem_district_mismatch == 1 & problem_detail == ""
replace severity = "HIGH" if problem_district_mismatch == 1 & severity == ""

count if problem_district_mismatch == 1
local n_district_mismatch = r(N)
if `n_district_mismatch' > 0 {
    display as error "  PROBLEM FOUND: `n_district_mismatch' PSUs with province-district mismatch"
}
else {
    display as text "  OK: All district codes consistent with province"
}

drop district_prov prov_district_match
display ""

*--- 6.4 Geographic Type Validation ---
display as result "6.4 Validating Geographic Type Codes..."

* Valid geotypes: 1=Urban, 2=Traditional, 3=Farms
replace problem_invalid_geotype = 1 if geotype < 1 | geotype > 3 | geotype == .
replace problem_detail = "Geotype " + string(geotype) + " not in valid set {1,2,3}" ///
    if problem_invalid_geotype == 1 & problem_detail == ""
replace severity = "LOW" if problem_invalid_geotype == 1 & severity == ""

count if problem_invalid_geotype == 1
local n_invalid_geo = r(N)
if `n_invalid_geo' > 0 {
    display as error "  PROBLEM FOUND: `n_invalid_geo' PSUs with invalid geotype codes"
}
else {
    display as text "  OK: All geography type codes are valid"
}
display ""

*--- 6.5 Create Combined Problem Flag ---
gen byte has_problem = (problem_invalid_prov == 1 | problem_malformed_psu == 1 | ///
                        problem_district_mismatch == 1 | problem_invalid_geotype == 1)

* Assign problem type
gen str50 problem_type = ""
replace problem_type = "Invalid Province Code" if problem_invalid_prov == 1
replace problem_type = "Malformed PSU Code" if problem_malformed_psu == 1 & problem_type == ""
replace problem_type = "Province-District Mismatch" if problem_district_mismatch == 1 & problem_type == ""
replace problem_type = "Invalid Geography Type" if problem_invalid_geotype == 1 & problem_type == ""

display ""

*-------------------------------------------------------------------------------
* 7. COMPILE PROBLEM REPORT
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 7: Compiling Problem Report"
display as text "------------------------------------------------------------------------"
display ""

* Calculate summary statistics
count
local total_psu = r(N)

count if has_problem == 1
local affected_psu = r(N)

egen total_hh_problem = total(n_households) if has_problem == 1
qui sum total_hh_problem
local affected_hh = r(max)
if missing(`affected_hh') local affected_hh = 0

egen total_hh_all = total(n_households)
qui sum total_hh_all
local total_hh = r(max)

display as result "FRAME QUALITY SUMMARY"
display as text "=================================================="
display ""

display "Problems by Severity:"
tab severity if has_problem == 1, missing

display ""
display "Problems by Type:"
tab problem_type if has_problem == 1, missing

display ""
display as result "IMPACT ASSESSMENT:"
display "  Total PSUs in frame: `total_psu'"
display "  PSUs with problems: `affected_psu' (" %4.1f 100*`affected_psu'/`total_psu' "%)"
display "  Total households: `total_hh'"
display "  Households in problem PSUs: `affected_hh' (" %4.1f 100*`affected_hh'/`total_hh' "%)"
display ""

* Export problem report
preserve
keep if has_problem == 1
keep psu prov district geotype n_households problem_type severity problem_detail ///
     problem_invalid_prov problem_malformed_psu problem_district_mismatch problem_invalid_geotype

order psu prov district geotype n_households problem_type severity problem_detail

export delimited using "${outdir}/frame_mismatch_report.csv", replace
display "Problem report exported to: ${outdir}/frame_mismatch_report.csv"
restore

* Export summary statistics
preserve
clear
input str30 metric value
    "Total_PSUs" `total_psu'
    "Problem_PSUs" `affected_psu'
    "Problem_Rate_Pct" `=100*`affected_psu'/`total_psu''
    "Total_Households" `total_hh'
    "Affected_Households" `affected_hh'
    "Affected_Rate_Pct" `=100*`affected_hh'/`total_hh''
end
export delimited using "${outdir}/frame_quality_summary.csv", replace
display "Quality summary exported to: ${outdir}/frame_quality_summary.csv"
restore

display ""

*-------------------------------------------------------------------------------
* 8. CROSS-REFERENCE WITH ADMINISTRATIVE BOUNDARIES
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 8: Cross-Reference with Administrative Boundaries"
display as text "------------------------------------------------------------------------"
display ""

* Merge with province reference
preserve
merge m:1 prov using "${outdir}/province_reference.dta", keep(master match) nogen

* Check for unmatched provinces
count if prov_name == ""
local unmatched_prov = r(N)
if `unmatched_prov' > 0 {
    display as error "WARNING: `unmatched_prov' PSUs could not be matched to province reference"
    list psu prov if prov_name == "" in 1/10
}
else {
    display as text "All PSUs successfully matched to province reference"
}
restore

display ""

*-------------------------------------------------------------------------------
* 9. GENERATE VISUALIZATIONS
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 9: Generating Visualizations"
display as text "------------------------------------------------------------------------"
display ""

* Problems by province
preserve
keep if has_problem == 1
collapse (count) n_problems=psu, by(prov)

* Merge with province names
merge 1:1 prov using "${outdir}/province_reference.dta", keep(match using) nogen
replace n_problems = 0 if n_problems == .

* Create bar chart
graph bar n_problems, over(prov_abbrev, sort(n_problems) descending) ///
    title("Frame Problems by Province") ///
    subtitle("Number of PSUs with identified issues") ///
    ytitle("Number of Problem PSUs") ///
    bar(1, color("0 51 102")) ///
    blabel(bar, format(%4.0f) size(small))

graph export "${outdir}/problems_by_province.png", replace width(800) height(500)
display "Saved: problems_by_province.png"
restore

* Problems by type
preserve
keep if has_problem == 1
collapse (count) n_psu=psu, by(problem_type)

graph hbar n_psu, over(problem_type, sort(n_psu)) ///
    title("Frame Problems by Type") ///
    subtitle("Number of PSUs affected") ///
    ytitle("Number of PSUs") ///
    bar(1, color("212 175 55")) ///
    blabel(bar, format(%4.0f) size(small))

graph export "${outdir}/problems_by_type.png", replace width(800) height(400)
display "Saved: problems_by_type.png"
restore

* Problems by severity
preserve
keep if has_problem == 1
collapse (count) n_psu=psu, by(severity)

* Define severity order
gen severity_order = .
replace severity_order = 1 if severity == "CRITICAL"
replace severity_order = 2 if severity == "HIGH"
replace severity_order = 3 if severity == "MEDIUM"
replace severity_order = 4 if severity == "LOW"

graph bar n_psu, over(severity, sort(severity_order)) ///
    title("Frame Problems by Severity") ///
    ytitle("Number of PSUs") ///
    bar(1, color("220 20 60")) ///
    blabel(bar, format(%4.0f) size(small))

graph export "${outdir}/problems_by_severity.png", replace width(600) height(500)
display "Saved: problems_by_severity.png"
restore

display ""

*-------------------------------------------------------------------------------
* 10. RECOMMENDED REMEDIATION ACTIONS
*-------------------------------------------------------------------------------

display as text "------------------------------------------------------------------------"
display as result "STEP 10: Recommended Remediation Actions"
display as text "------------------------------------------------------------------------"
display ""

display "Based on the identified frame problems, we recommend:"
display ""

if `n_invalid_prov' > 0 {
    display as result "HIGH - Invalid Province Codes (`n_invalid_prov' PSUs):"
    display as text "  Action: Cross-reference with original sampling frame"
    display as text "  Action: Contact field teams for correction"
    display as text "  Impact: Cannot properly stratify or weight these PSUs"
    display ""
}

if `n_district_mismatch' > 0 {
    display as result "HIGH - Province-District Mismatches (`n_district_mismatch' PSUs):"
    display as text "  Action: Verify district assignments against OCHA boundaries"
    display as text "  Action: Check for boundary changes since frame construction"
    display as text "  Impact: May affect domain estimation and variance calculation"
    display ""
}

if `n_malformed' > 0 {
    display as result "MEDIUM - Malformed PSU Codes (`n_malformed' PSUs):"
    display as text "  Action: Apply standardized PSU code formatting rules"
    display as text "  Action: Create crosswalk to correct codes"
    display as text "  Impact: May cause matching failures in analysis"
    display ""
}

if `n_invalid_geo' > 0 {
    display as result "LOW - Invalid Geography Types (`n_invalid_geo' PSUs):"
    display as text "  Action: Assign based on centroid location or dominant land use"
    display as text "  Impact: Minor effect on geotype-specific analyses"
    display ""
}

*-------------------------------------------------------------------------------
* 11. CLEANUP AND SUMMARY
*-------------------------------------------------------------------------------

display as text "========================================================================"
display as result "LAB 1.5 COMPLETE"
display as text "========================================================================"
display ""

display "Output files saved to: ${outdir}"
display "  - frame_mismatch_report.csv (detailed problem list)"
display "  - frame_quality_summary.csv (summary statistics)"
display "  - psu_geography.dta (PSU-level geographic data)"
display "  - province_reference.dta (province lookup table)"
display "  - district_reference.dta (district lookup table)"
display "  - problems_by_province.png"
display "  - problems_by_type.png"
display "  - problems_by_severity.png"
display ""

display "Session Info:"
display "  STATA version: `c(stata_version)'"
display "  Date: `c(current_date)' `c(current_time)'"

log close

*-------------------------------------------------------------------------------
* END OF SCRIPT
*-------------------------------------------------------------------------------
