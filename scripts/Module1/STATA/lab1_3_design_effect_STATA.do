*===============================================================================
* LAB 1.3: DESIGN EFFECT (DEFF) AND INTRACLASS CORRELATION (ICC) CALCULATIONS
*===============================================================================
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 1, Module 3: Multi-Stage Stratified Design for Household Surveys
*
* NARRATIVE CONTEXT (Zambara Case):
* Lindiwe Moyo, Chief Methodologist at ZNSO, needs to demonstrate to the 
* Minister why the previous survey's simple random sampling assumptions led 
* to unreliable rural estimates. She calculates design effects to show the 
* efficiency loss from clustering and the heterogeneity across provinces.
*
* LEARNING OBJECTIVES:
* - Calculate and interpret the Design Effect (DEFF) for complex surveys
* - Estimate the Intraclass Correlation Coefficient (ICC) 
* - Demonstrate variance inflation from cluster sampling
* - Compare DEFF across provinces and geography types
*
* DATA: ghs-2024-hhold-v1.dta (Household file)
*
* VARIABLE DICTIONARY:
* - totmhinc   : Total monthly household income (derived variable)
* - house_wgt  : Household survey weight
* - Stratum    : Sampling stratum (Province × Metro × GeoType)
* - psu        : Primary Sampling Unit (cluster)
* - Prov       : Province (1=WC, 2=EC, 3=NC, 4=FS, 5=KZN, 6=NW, 7=GP, 8=MP, 9=LP)
* - GeoType    : Geography type (1=Urban, 2=Traditional, 3=Farms)
*
* REFERENCES:
* - Kish, L. (1965). Survey Sampling. Wiley.
* - Lohr, S.L. (2022). Sampling: Design and Analysis. 3rd ed. CRC Press.
* - StataCorp. Stata Survey Data Reference Manual.
*===============================================================================

*-------------------------------------------------------------------------------
* SECTION 1: ENVIRONMENT SETUP
*-------------------------------------------------------------------------------

clear all
set more off
capture log close

* ============================================================================
* MODIFY THESE PATHS TO MATCH YOUR FOLDER STRUCTURE
* ============================================================================
global data_path "./data/GHS_2024/"
global output_path "./output/"
* ============================================================================

* Create output folder if it doesn't exist
capture mkdir "$output_path"

* Start log file in output folder
log using "${output_path}lab1_3_design_effect.log", replace text

display as text _n(3)
display as result "=========================================================="
display as result "  LAB 1.3: DESIGN EFFECT AND ICC CALCULATIONS             "
display as result "  SADC Advanced Sampling Workshop - Day 1                  "
display as result "=========================================================="
display as text _n(2)

*-------------------------------------------------------------------------------
* SECTION 2: DATA IMPORT AND PREPARATION
*-------------------------------------------------------------------------------

display as text "========== LOADING DATA =========="
display as text "Data path: $data_path"
display as text "Output path: $output_path"

* Load household data
use "${data_path}ghs-2024-hhold-v1.dta", clear

* Display basic info
describe, short
display as text "Observations: " _N
display as text "Variables: " c(k)

* Check for key variables
display as text _n "Checking key variables..."

foreach var in totmhinc house_wgt stratum geotype prov psu {
    capture confirm variable `var'
    if _rc == 0 {
        display as text "  `var': Found"
    }
    else {
        * Try different cases
        local uvar = upper("`var'")
        local pvar = proper("`var'")
        capture confirm variable `uvar'
        if _rc == 0 {
            rename `uvar' `var'
            display as text "  `var': Found as `uvar' (renamed)"
        }
        else {
            capture confirm variable `pvar'
            if _rc == 0 {
                rename `pvar' `var'
                display as text "  `var': Found as `pvar' (renamed)"
            }
            else {
                display as error "  `var': NOT FOUND"
            }
        }
    }
}

* Standardize to lowercase
rename *, lower

*-------------------------------------------------------------------------------
* SECTION 3: DATA CLEANING FOR ANALYSIS
*-------------------------------------------------------------------------------

display as text _n "========== DATA CLEANING =========="

* Check for missing/invalid income values (9999999 = unspecified per metadata)
count if totmhinc >= 9999999 | missing(totmhinc)
local n_missing = r(N)
display as text "Records with missing/invalid income: `n_missing'"

* Create analysis sample
gen byte sample_flag = 1
replace sample_flag = 0 if totmhinc >= 9999999 | missing(totmhinc)
replace sample_flag = 0 if house_wgt <= 0 | missing(house_wgt)

count if sample_flag == 1
local n_analysis = r(N)
display as text "Records in analysis sample: `n_analysis'"

* Keep only valid observations
keep if sample_flag == 1
drop sample_flag

* Create Zambara region labels (mapping SA provinces)
label define zambara_lbl ///
    1 "Southern Cape" ///
    2 "Coastal Plains" ///
    3 "Western Drylands" ///
    4 "Central Plateau" ///
    5 "Eastern Highlands" ///
    6 "Mining Belt" ///
    7 "Capital Region" ///
    8 "Eastern Forests" ///
    9 "Northern Bushveld"

gen zambara_region = prov
label values zambara_region zambara_lbl

* Create geography type labels
label define geotype_lbl 1 "Urban" 2 "Traditional" 3 "Farms"
label values geotype geotype_lbl

*-------------------------------------------------------------------------------
* SECTION 4: EXAMINE CLUSTER STRUCTURE
*-------------------------------------------------------------------------------

display as text _n "========== CLUSTER STRUCTURE =========="

* Count unique PSUs
quietly distinct psu
local n_psu = r(ndistinct)
display as text "Total PSUs (clusters): `n_psu'"

* Count unique strata
quietly distinct stratum
local n_strata = r(ndistinct)
display as text "Total strata: `n_strata'"

* Calculate cluster sizes
preserve
    collapse (count) n_hh = totmhinc, by(psu)
    summarize n_hh, detail
    local mean_cluster = r(mean)
    local min_cluster = r(min)
    local max_cluster = r(max)
restore

display as text "Average households per PSU: " %5.2f `mean_cluster'
display as text "Cluster size range: `min_cluster' - `max_cluster'"

* Store for later use
scalar m_bar = `mean_cluster'

*-------------------------------------------------------------------------------
* SECTION 5: DEFINE SURVEY DESIGN
*-------------------------------------------------------------------------------

display as text _n "========== SURVEY DESIGN SPECIFICATION =========="

* Set up complex survey design
svyset psu [pweight=house_wgt], strata(stratum) singleunit(scaled)

* Display design information
svydescribe

*-------------------------------------------------------------------------------
* SECTION 6: CALCULATE NATIONAL DESIGN EFFECT
*-------------------------------------------------------------------------------

display as text _n(3)
display as result "=========================================================="
display as result "  SECTION 6: NATIONAL-LEVEL DESIGN EFFECT                 "
display as result "=========================================================="

* Calculate mean under complex design with DEFF
svy: mean totmhinc

* Store results
matrix results_complex = r(table)
scalar est_complex = results_complex[1,1]
scalar se_complex = results_complex[2,1]

* Get DEFF
estat effects
matrix deff_matrix = r(deff)
scalar deff_national = deff_matrix[1,1]

* Calculate what SRS would give (ignoring design)
mean totmhinc [pweight=house_wgt]
matrix results_srs = r(table)
scalar est_srs = results_srs[1,1]
scalar se_srs = results_srs[2,1]

* Display results
display as text _n "----- MEAN HOUSEHOLD INCOME (totmhinc) -----"

display as text _n "Complex Design Estimate:"
display as text "  Point estimate:  R " %12.2fc est_complex
display as text "  Standard error:  R " %12.2fc se_complex
display as text "  95% CI:          [R " %10.2fc (est_complex - 1.96*se_complex) ///
                ", R " %10.2fc (est_complex + 1.96*se_complex) "]"

display as text _n "SRS Assumption (INCORRECT):"
display as text "  Point estimate:  R " %12.2fc est_srs
display as text "  Standard error:  R " %12.2fc se_srs

display as result _n "***** DESIGN EFFECT (DEFF) = " %6.3f deff_national " *****"

display as text _n "Interpretation:"
display as text "  The complex design variance is " %4.1f deff_national " times larger than"
display as text "  what we would expect under SRS."

* Effective sample size
local n_actual = _N
scalar n_effective = `n_actual' / deff_national

display as text _n "Effective Sample Size:"
display as text "  Actual sample size:     " %8.0fc `n_actual' " households"
display as text "  Effective sample size:  " %8.0fc n_effective " households"
display as text "  Efficiency loss:        " %5.1f ((1 - 1/deff_national) * 100) "%"

*-------------------------------------------------------------------------------
* SECTION 7: CALCULATE ICC (DERIVED FROM DEFF - FAST METHOD)
*-------------------------------------------------------------------------------

display as text _n(3)
display as result "=========================================================="
display as result "  SECTION 7: INTRACLASS CORRELATION (DERIVED FROM DEFF)   "
display as result "=========================================================="

* For large datasets with thousands of clusters, we derive ICC from DEFF
* using the inverse of Kish's formula:
*   DEFF ≈ 1 + (m̄ - 1) × ρ
*   Therefore: ρ = (DEFF - 1) / (m̄ - 1)
*
* This is standard practice (Lohr 2022, Ch. 5.5) and much faster than
* fitting variance components models with thousands of PSUs.

scalar icc_national = (deff_national - 1) / (m_bar - 1)

display as text _n "----- ICC ESTIMATION (DERIVED FROM DEFF) -----"
display as text _n "Using Kish's formula inverted: ρ = (DEFF - 1) / (m̄ - 1)"
display as text "  Observed DEFF:              " %6.3f deff_national
display as text "  Average cluster size (m̄):  " %6.2f m_bar
display as result _n "***** ICC (rho) = " %6.4f icc_national " *****"

* Verify relationship
scalar deff_check = 1 + (m_bar - 1) * icc_national

display as text _n "----- VERIFICATION -----"
display as text "  Reconstructed DEFF: " %6.3f deff_check
display as text "  Observed DEFF:      " %6.3f deff_national
display as text "  (Should match exactly when derived from same DEFF)"

*-------------------------------------------------------------------------------
* SECTION 8: DESIGN EFFECTS BY PROVINCE (ZAMBARA REGIONS)
*-------------------------------------------------------------------------------

display as text _n(3)
display as result "=========================================================="
display as result "  SECTION 8: DESIGN EFFECTS BY ZAMBARA REGION             "
display as result "=========================================================="

* Create temporary file to store results
tempfile deff_results
tempname memhold

postfile `memhold' prov str25 region n_hh n_psu mean_inc se_complex deff icc ///
    using `deff_results', replace

* Loop through provinces
forvalues p = 1/9 {
    
    * Get region label
    local region_name : label zambara_lbl `p'
    
    * Count observations and PSUs in province
    quietly count if prov == `p'
    local n_hh = r(N)
    
    if `n_hh' > 0 {
        
        quietly distinct psu if prov == `p'
        local n_psu_p = r(ndistinct)
        
        * Calculate average cluster size for this province
        local avg_cluster_p = `n_hh' / `n_psu_p'
        
        * Complex design estimate for province
        quietly svy, subpop(if prov == `p'): mean totmhinc
        matrix temp = r(table)
        local mean_p = temp[1,1]
        local se_p = temp[2,1]
        
        * Get DEFF for subpopulation
        quietly estat effects
        matrix deff_p = r(deff)
        local deff_prov = deff_p[1,1]
        
        * Derive ICC from DEFF for this province
        local icc_prov = (`deff_prov' - 1) / (`avg_cluster_p' - 1)
        if `icc_prov' < 0 {
            local icc_prov = 0
        }
        
        * Post results
        post `memhold' (`p') ("`region_name'") (`n_hh') (`n_psu_p') ///
            (`mean_p') (`se_p') (`deff_prov') (`icc_prov')
        
        display as text "  Province `p' (`region_name'): DEFF = " %5.2f `deff_prov' ///
            ", ICC = " %6.4f `icc_prov'
    }
}

postclose `memhold'

* Display results table
display as text _n "----- DESIGN EFFECTS BY ZAMBARA REGION -----"

use `deff_results', clear

* Format for display
format mean_inc %12.0fc
format se_complex %8.1f
format deff %5.2f
format icc %6.4f

list region n_hh n_psu mean_inc se_complex deff icc, ///
    separator(0) noobs abbreviate(15)

* Save results to output folder
export delimited using "${output_path}lab1_3_deff_by_province.csv", replace
display as text _n "Table saved: ${output_path}lab1_3_deff_by_province.csv"

* Find max and min DEFF
summarize deff
local max_deff = r(max)
local min_deff = r(min)

* Identify regions
gsort -deff
local max_region = region[1]
local max_deff_val = deff[1]
gsort deff  
local min_region = region[1]
local min_deff_val = deff[1]

* Save for later
scalar max_deff_s = `max_deff'
scalar min_deff_s = `min_deff'

*-------------------------------------------------------------------------------
* SECTION 9: DESIGN EFFECTS BY GEOGRAPHY TYPE
*-------------------------------------------------------------------------------

* Reload main data
use "${data_path}ghs-2024-hhold-v1.dta", clear
rename *, lower

* Apply same cleaning
keep if totmhinc < 9999999 & !missing(totmhinc)
keep if house_wgt > 0 & !missing(house_wgt)

* Redefine survey design
svyset psu [pweight=house_wgt], strata(stratum) singleunit(scaled)

display as text _n(3)
display as result "=========================================================="
display as result "  SECTION 9: DESIGN EFFECTS BY GEOGRAPHY TYPE             "
display as result "=========================================================="

* Create labels
label define geotype_lbl 1 "Urban" 2 "Traditional" 3 "Farms", replace
label values geotype geotype_lbl

tempfile geo_results
tempname geohold

postfile `geohold' geotype str15 geotype_label n_hh n_psu mean_inc deff icc ///
    using `geo_results', replace

forvalues g = 1/3 {
    
    local geo_name : label geotype_lbl `g'
    
    quietly count if geotype == `g'
    local n_hh_g = r(N)
    
    if `n_hh_g' > 0 {
        
        quietly distinct psu if geotype == `g'
        local n_psu_g = r(ndistinct)
        local avg_cluster_g = `n_hh_g' / `n_psu_g'
        
        quietly svy, subpop(if geotype == `g'): mean totmhinc
        matrix temp_g = r(table)
        local mean_g = temp_g[1,1]
        
        quietly estat effects
        matrix deff_g = r(deff)
        local deff_geo = deff_g[1,1]
        
        * Derive ICC
        local icc_geo = (`deff_geo' - 1) / (`avg_cluster_g' - 1)
        if `icc_geo' < 0 {
            local icc_geo = 0
        }
        
        post `geohold' (`g') ("`geo_name'") (`n_hh_g') (`n_psu_g') ///
            (`mean_g') (`deff_geo') (`icc_geo')
        
        display as text "  `geo_name': DEFF = " %5.2f `deff_geo' ", ICC = " %6.4f `icc_geo'
    }
}

postclose `geohold'

use `geo_results', clear
format mean_inc %12.0fc
format deff %5.2f
format icc %6.4f

display as text _n "----- DESIGN EFFECTS BY GEOGRAPHY TYPE -----"
list geotype_label n_hh n_psu mean_inc deff icc, separator(0) noobs

* Save to output
export delimited using "${output_path}lab1_3_deff_by_geotype.csv", replace
display as text "Table saved: ${output_path}lab1_3_deff_by_geotype.csv"

*-------------------------------------------------------------------------------
* SECTION 10: VISUALIZATION
*-------------------------------------------------------------------------------

display as text _n(3)
display as result "=========================================================="
display as result "  SECTION 10: VISUALIZATIONS                              "
display as result "=========================================================="

* Load province DEFF results
use `deff_results', clear

* Bar chart of DEFF by region
graph hbar deff, over(region, sort(deff) descending label(labsize(small))) ///
    ytitle("Design Effect (DEFF)") ///
    title("Design Effect by Zambara Region", size(medium)) ///
    subtitle("Total Monthly Household Income") ///
    yline(1, lpattern(dash) lcolor(red)) ///
    bar(1, color("0 51 102")) ///
    note("Red line indicates DEFF = 1 (SRS baseline)" ///
         "ICC derived using ρ = (DEFF-1)/(m̄-1)", size(vsmall))

graph export "${output_path}lab1_3_deff_by_province.png", replace width(1200)
display as text "Graph saved: ${output_path}lab1_3_deff_by_province.png"

* Bar chart of ICC by region
graph hbar icc, over(region, sort(icc) descending label(labsize(small))) ///
    ytitle("Intraclass Correlation (ICC)") ///
    title("ICC by Zambara Region", size(medium)) ///
    subtitle("Within-cluster homogeneity for household income") ///
    bar(1, color("0 102 51")) ///
    note("Higher ICC = greater homogeneity within clusters", size(vsmall))

graph export "${output_path}lab1_3_icc_by_province.png", replace width(1200)
display as text "Graph saved: ${output_path}lab1_3_icc_by_province.png"

*-------------------------------------------------------------------------------
* SECTION 11: SUMMARY OUTPUT FOR SLIDES
*-------------------------------------------------------------------------------

display as text _n(3)
display as result "=========================================================="
display as result "  SUMMARY OUTPUT FOR PRESENTATION SLIDES                  "
display as result "=========================================================="

display as text _n "============ KEY RESULTS FOR SLIDE INTEGRATION ============"

display as text _n ">>> NATIONAL DESIGN EFFECT <<<"
display as text "DEFF_national = " %5.2f deff_national
display as text "ICC_national = " %6.4f icc_national
display as text "Mean_cluster_size = " %5.1f m_bar
display as text "Effective_n = " %8.0f n_effective
display as text "Actual_n = " %8.0f `n_actual'
display as text "Mean_income = R " %10.0fc est_complex
display as text "SE_complex = R " %8.2f se_complex
display as text "SE_srs = R " %8.2f se_srs

display as text _n ">>> HIGHEST DEFF REGION <<<"
display as text "Region: `max_region'"
display as text "DEFF: " %5.2f max_deff_s

display as text _n ">>> LOWEST DEFF REGION <<<"
display as text "Region: `min_region'"
display as text "DEFF: " %5.2f min_deff_s

display as text _n ">>> VARIANCE RATIO <<<"
scalar var_ratio = (se_complex^2) / (se_srs^2)
display as text "Var(complex) / Var(SRS) = " %5.2f var_ratio
display as text "SE inflation factor = " %5.2f (se_complex / se_srs)

*-------------------------------------------------------------------------------
* SECTION 12: SAVE SCALAR RESULTS
*-------------------------------------------------------------------------------

* Save key results to a dataset
clear
set obs 1
gen deff_national = scalar(deff_national)
gen icc_national = scalar(icc_national)
gen m_bar = scalar(m_bar)
gen n_effective = scalar(n_effective)
gen n_actual = `n_actual'
gen mean_income = scalar(est_complex)
gen se_complex = scalar(se_complex)
gen se_srs = scalar(se_srs)

save "${output_path}lab1_3_national_results.dta", replace
export delimited using "${output_path}lab1_3_national_results.csv", replace
display as text _n "National results saved to: ${output_path}lab1_3_national_results.csv"

*-------------------------------------------------------------------------------
* END OF LAB SCRIPT
*-------------------------------------------------------------------------------

display as text _n(3)
display as result "=========================================================="
display as result "  LAB 1.3 COMPLETE                                        "
display as result "=========================================================="

display as text _n "Lindiwe now has the evidence she needs to show the Minister:"
display as text "  • The design effect of " %4.2f deff_national " means the effective sample"
display as text "    is only " %6.0f n_effective " households, not the " %6.0f `n_actual' " collected."
display as text "  • Ignoring clustering would underestimate SEs by " ///
    %4.1f ((1 - se_srs/se_complex) * 100) "%"
display as text "  • Traditional areas show higher DEFFs, explaining rural problems."
display as text _n "All results saved to: $output_path"

log close

*===============================================================================
* QUICK REFERENCE: KEY STATA COMMANDS
*===============================================================================
*
* svyset psu [pw=house_wgt], strata(stratum)  - Define survey design
* svy: mean totmhinc                          - Weighted mean with correct SEs
* estat effects                               - Get DEFF after svy estimation
* svy, subpop(if prov==1): mean totmhinc     - Subpopulation estimates
*
* ICC derived from DEFF: rho = (DEFF - 1) / (m_bar - 1)
*
*===============================================================================
