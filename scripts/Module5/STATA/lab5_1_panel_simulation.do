********************************************************************************
*                                                                              *
*   LAB 5.1: PANEL DATA SIMULATION AND LINKING                                 *
*   SADC Regional Training Workshop on Advanced Sampling Methods               *
*   Day 5: Synthesis, Longitudinal Surveys, and Conclusion                     *
*                                                                              *
*   Republic of Zambara — Quarterly Labour Force Survey Pilot                  *
*                                                                              *
*   Author: SADC Statistical Training Programme                                *
*   Date: March 2026                                                           *
*   Software: STATA 16+ (requires version 16 for frames)                       *
*                                                                              *
********************************************************************************

********************************************************************************
* NARRATIVE CONTEXT
********************************************************************************
*
* Lindiwe has convinced the Ministry to pilot a rotating panel design for the
* Zambara Quarterly Labour Force Survey (ZQLFS). The first wave was fielded in
* Q1 2026. Now, as Q2 approaches, she must understand what happens when 
* households drop out of the panel — the dreaded problem of ATTRITION.
*
* In this lab, we simulate what Wave 2 might look like if 20% of households
* fail to be re-interviewed. We explore:
*   1. How attrition creates gaps in our longitudinal data
*   2. How attrition rates vary by province (non-random attrition)
*   3. How to link records across waves using unique identifiers
*   4. The foundation for attrition weight adjustments (Lab 5.2)
*
* Key Learning Objectives:
*   - Understand panel data structure and linking mechanisms
*   - Simulate realistic non-random attrition patterns
*   - Calculate and interpret attrition rates by subgroup
*   - Prepare linked panel datasets for longitudinal analysis
*
********************************************************************************

********************************************************************************
* SECTION 1: SETUP AND DATA LOADING
********************************************************************************

clear all
set more off
set seed 20260306    // Date of Day 5: 6 March 2026 - ensures reproducibility

* Define file paths
* Participants should modify this path to match their local setup
global datapath "/mnt/user-data/uploads"
global outpath "/home/claude"

* Alternative paths for workshop setup:
* global datapath "C:/SADC_Workshop/Data"    // Windows
* global datapath "~/SADC_Workshop/Data"     // Mac/Linux

display ""
display as text "=" * 70
display as result "LOADING WAVE 1 DATA (GHS 2024 as ZQLFS Q1 2026)"
display as text "=" * 70
display ""

* Load Wave 1 data (GHS 2024 treated as ZQLFS Wave 1)
use "$datapath/ghs-2024-hhold-v1.dta", clear

* Examine the structure
describe, short
display ""
display as text "Wave 1 raw data dimensions: " as result _N as text " households"
display ""

* -----------------------------------------------------------------------------
* IMPORTANT: Explore variable names in the dataset
* Variable names may differ between GHS releases and from metadata documentation
* -----------------------------------------------------------------------------
display as text "EXPLORING VARIABLE NAMES IN DATASET:"
display as text "-" * 70
describe, simple
display ""

* Display variables matching key patterns to help identify correct names
display as text "Searching for key variables (case-insensitive patterns):"
display as text "-" * 70

* Look for unique ID variables
display as text "  Unique ID patterns (uq, unique, hhid):"
ds *uq* *unique* *hhid*, has(type numeric string)

* Look for PSU variables
display as text "  PSU patterns (psu, cluster):"
ds *psu* *cluster*, has(type numeric)

* Look for stratum variables
display as text "  Stratum patterns (strat):"
ds *strat*, has(type numeric)

* Look for weight variables
display as text "  Weight patterns (wgt, weight):"
ds *wgt* *weight*, has(type numeric)

* Look for province variables
display as text "  Province patterns (prov, region):"
ds *prov* *region*, has(type numeric)

* Look for income variables
display as text "  Income patterns (inc, income, earn):"
ds *inc* *income* *earn*, has(type numeric)

display ""

********************************************************************************
* VARIABLE MAPPING SECTION
* PARTICIPANTS: Update these local macros if your variable names differ
* Run the 'describe' and 'ds' commands above to identify correct names
********************************************************************************

display as text "VARIABLE MAPPING:"
display as text "-" * 70
display as text "Update these mappings if your variable names differ from defaults"
display ""

* -----------------------------------------------------------------------------
* AUTOMATIC VARIABLE DETECTION
* The script will try common variable name patterns
* If a variable is not found, it will display a warning
* -----------------------------------------------------------------------------

* Capture variable names - adjust these based on your actual data
* Common GHS 2024 naming conventions:

* Try to find unique ID (UqNr, UQNO, uqnr)
capture confirm variable UqNr
if _rc == 0 {
    local var_id "UqNr"
}
else {
    capture confirm variable UQNO
    if _rc == 0 {
        local var_id "UQNO"
    }
    else {
        capture confirm variable uqnr
        if _rc == 0 {
            local var_id "uqnr"
        }
        else {
            display as error "ERROR: Unique ID variable not found (tried UqNr, UQNO, uqnr)"
            display as text "Please set: local var_id ""your_variable_name"""
            local var_id ""
        }
    }
}
display as text "  Unique ID: " as result "`var_id'"

* Try to find PSU
capture confirm variable PSU
if _rc == 0 {
    local var_psu "PSU"
}
else {
    capture confirm variable psu
    if _rc == 0 {
        local var_psu "psu"
    }
    else {
        capture confirm variable PSUNO
        if _rc == 0 {
            local var_psu "PSUNO"
        }
        else {
            local var_psu ""
        }
    }
}
display as text "  PSU: " as result "`var_psu'"

* Try to find Stratum
capture confirm variable Stratum
if _rc == 0 {
    local var_stratum "Stratum"
}
else {
    capture confirm variable stratum
    if _rc == 0 {
        local var_stratum "stratum"
    }
    else {
        capture confirm variable STRATUM
        if _rc == 0 {
            local var_stratum "STRATUM"
        }
        else {
            local var_stratum ""
        }
    }
}
display as text "  Stratum: " as result "`var_stratum'"

* Try to find household weight
capture confirm variable house_wgt
if _rc == 0 {
    local var_weight "house_wgt"
}
else {
    capture confirm variable HOUSE_WGT
    if _rc == 0 {
        local var_weight "HOUSE_WGT"
    }
    else {
        capture confirm variable hh_wgt
        if _rc == 0 {
            local var_weight "hh_wgt"
        }
        else {
            local var_weight ""
        }
    }
}
display as text "  Weight: " as result "`var_weight'"

* Try to find Province
capture confirm variable Prov
if _rc == 0 {
    local var_prov "Prov"
}
else {
    capture confirm variable prov
    if _rc == 0 {
        local var_prov "prov"
    }
    else {
        capture confirm variable PROV
        if _rc == 0 {
            local var_prov "PROV"
        }
        else {
            local var_prov ""
        }
    }
}
display as text "  Province: " as result "`var_prov'"

* Try to find GeoType
capture confirm variable GeoType
if _rc == 0 {
    local var_geotype "GeoType"
}
else {
    capture confirm variable geotype
    if _rc == 0 {
        local var_geotype "geotype"
    }
    else {
        capture confirm variable GEOTYPE
        if _rc == 0 {
            local var_geotype "GEOTYPE"
        }
        else {
            local var_geotype ""
        }
    }
}
display as text "  GeoType: " as result "`var_geotype'"

* Try to find household size
capture confirm variable hholdsz
if _rc == 0 {
    local var_hhsize "hholdsz"
}
else {
    capture confirm variable HHOLDSZ
    if _rc == 0 {
        local var_hhsize "HHOLDSZ"
    }
    else {
        capture confirm variable hhsize
        if _rc == 0 {
            local var_hhsize "hhsize"
        }
        else {
            local var_hhsize ""
        }
    }
}
display as text "  HH Size: " as result "`var_hhsize'"

* Try to find head age
capture confirm variable head_age
if _rc == 0 {
    local var_head_age "head_age"
}
else {
    capture confirm variable HEAD_AGE
    if _rc == 0 {
        local var_head_age "HEAD_AGE"
    }
    else {
        local var_head_age ""
    }
}
display as text "  Head Age: " as result "`var_head_age'"

* Try to find head sex
capture confirm variable head_sex
if _rc == 0 {
    local var_head_sex "head_sex"
}
else {
    capture confirm variable HEAD_SEX
    if _rc == 0 {
        local var_head_sex "HEAD_SEX"
    }
    else {
        local var_head_sex ""
    }
}
display as text "  Head Sex: " as result "`var_head_sex'"

* Try to find total monthly household income
capture confirm variable totmhinc
if _rc == 0 {
    local var_income "totmhinc"
}
else {
    capture confirm variable TOTMHINC
    if _rc == 0 {
        local var_income "TOTMHINC"
    }
    else {
        capture confirm variable hh_income
        if _rc == 0 {
            local var_income "hh_income"
        }
        else {
            local var_income ""
        }
    }
}
display as text "  Income: " as result "`var_income'"

* Try to find economically active
capture confirm variable econact_hh
if _rc == 0 {
    local var_econact "econact_hh"
}
else {
    capture confirm variable ECONACT_HH
    if _rc == 0 {
        local var_econact "ECONACT_HH"
    }
    else {
        local var_econact ""
    }
}
display as text "  Econ Active: " as result "`var_econact'"

display ""

* Check for critical missing variables
if "`var_id'" == "" | "`var_prov'" == "" | "`var_income'" == "" {
    display as error "CRITICAL VARIABLES NOT FOUND!"
    display as error "Please manually set the variable names:"
    display as text "  local var_id ""your_unique_id_variable"""
    display as text "  local var_prov ""your_province_variable"""  
    display as text "  local var_income ""your_income_variable"""
    display ""
    display as text "Available variables in dataset:"
    describe, simple
    exit 198
}

********************************************************************************
* SECTION 2: PREPARE WAVE 1 PANEL DATASET
********************************************************************************

display as text "=" * 70
display as result "PREPARING WAVE 1 PANEL DATASET"
display as text "=" * 70
display ""

* -----------------------------------------------------------------------------
* Create unique household identifier as string (for panel linking)
* This is the "time travel" key that connects the same household across waves
* Using dynamically detected variable names from Section 1
* -----------------------------------------------------------------------------

* Generate standardized variable names from detected variables
tostring `var_id', generate(hh_id) format(%18.0f)

* Rename variables to standardized names (only if they exist)
if "`var_psu'" != "" {
    rename `var_psu' psu
}
else {
    generate psu = .
}

if "`var_stratum'" != "" {
    rename `var_stratum' stratum
}
else {
    generate stratum = .
}

if "`var_weight'" != "" {
    rename `var_weight' hh_weight
}
else {
    generate hh_weight = 1
}

rename `var_prov' province

if "`var_geotype'" != "" {
    rename `var_geotype' geotype
}
else {
    generate geotype = .
}

if "`var_hhsize'" != "" {
    rename `var_hhsize' hh_size
}
else {
    generate hh_size = .
}

if "`var_head_age'" != "" {
    rename `var_head_age' head_age
}
else {
    generate head_age = .
}

if "`var_head_sex'" != "" {
    rename `var_head_sex' head_sex
}
else {
    generate head_sex = .
}

rename `var_income' income_w1

if "`var_econact'" != "" {
    rename `var_econact' employed_hh
}
else {
    generate employed_hh = .
}

* Create province name labels (Zambara regions)
label define province_lbl ///
    1 "Southern Cape" ///
    2 "Coastal Plains" ///
    3 "Western Drylands" ///
    4 "Central Plateau" ///
    5 "Eastern Highlands" ///
    6 "Mining Belt" ///
    7 "Zambara Capital" ///
    8 "Eastern Forests" ///
    9 "Northern Bushveld"
label values province province_lbl

* Create geotype labels
label define geotype_lbl ///
    1 "Urban" ///
    2 "Traditional" ///
    3 "Farms"
label values geotype geotype_lbl

* Create wave identifier
generate wave = 1

* Keep only essential variables and remove missing critical identifiers
keep hh_id psu stratum hh_weight province geotype ///
     hh_size head_age head_sex income_w1 employed_hh wave

* Drop observations with missing key variables
drop if missing(hh_id) | missing(income_w1)

* Store Wave 1 count
local n_wave1 = _N

display as text "Wave 1 prepared dataset:"
display as text "  Total households: " as result %12.0fc `n_wave1'
display ""

* -----------------------------------------------------------------------------
* Wave 1 Summary Statistics by Province
* -----------------------------------------------------------------------------
display as text "Wave 1 Summary by Province (Zambara Regions):"
display as text "-" * 70

tabstat income_w1 hh_size, by(province) stat(n mean median) nototal format(%12.0fc)
display ""

********************************************************************************
* SECTION 3: SIMULATE ATTRITION FOR WAVE 2
********************************************************************************

display as text "=" * 70
display as result "SIMULATING WAVE 2 ATTRITION"
display as text "=" * 70
display ""

* -----------------------------------------------------------------------------
* Key Insight: Attrition is NEVER random!
* 
* In real panel surveys, households that drop out differ systematically from
* those that remain. We simulate NON-RANDOM attrition reflecting:
*   - Urban households have higher mobility → higher attrition
*   - Certain regions have infrastructure challenges → higher attrition
*   - Younger household heads move more often → higher attrition
* -----------------------------------------------------------------------------

* Define base attrition probability (20% overall target)
local base_attrition = 0.20

* Generate province-specific attrition multipliers
* These reflect differential fieldwork challenges across Zambara
generate prov_mult = .
replace prov_mult = 1.20 if province == 1   // Southern Cape: High urban mobility
replace prov_mult = 0.85 if province == 2   // Coastal Plains: Stable communities
replace prov_mult = 1.30 if province == 3   // Western Drylands: Remote, sparse
replace prov_mult = 0.90 if province == 4   // Central Plateau: Moderate
replace prov_mult = 0.95 if province == 5   // Eastern Highlands: Moderate
replace prov_mult = 1.10 if province == 6   // Mining Belt: Labor migration
replace prov_mult = 1.40 if province == 7   // Zambara Capital: Highest mobility
replace prov_mult = 0.80 if province == 8   // Eastern Forests: Stable rural
replace prov_mult = 0.75 if province == 9   // Northern Bushveld: Most stable

* Urban areas have 20% higher attrition
generate urban_mult = cond(geotype == 1, 1.20, 1.00)

* Young household heads (under 35) have 15% higher attrition
generate age_mult = cond(head_age < 35 & !missing(head_age), 1.15, 1.00)

* Calculate individual attrition probability (capped at 0.50)
generate p_attrit = min(0.50, `base_attrition' * prov_mult * urban_mult * age_mult)

* Generate random attrition indicator
* 1 = household drops out, 0 = household remains in panel
generate attrit = runiform() < p_attrit

* -----------------------------------------------------------------------------
* Examine attrition simulation results
* -----------------------------------------------------------------------------
display as text "Attrition Simulation Results:"
display as text "-" * 70
display ""

summarize attrit, meanonly
local overall_attrition = r(mean) * 100
local n_retained = _N - r(sum)
local n_lost = r(sum)

display as text "Overall attrition rate: " as result %5.1f `overall_attrition' "%"
display as text "Households retained: " as result %12.0fc `n_retained' ///
        as text " (" as result %5.1f (100 - `overall_attrition') as text "%)"
display as text "Households lost: " as result %12.0fc `n_lost' ///
        as text " (" as result %5.1f `overall_attrition' as text "%)"
display ""

********************************************************************************
* SECTION 4: ATTRITION RATES BY PROVINCE
********************************************************************************

display as text "=" * 70
display as result "ATTRITION RATES BY PROVINCE"
display as text "=" * 70
display ""

* Create attrition summary by province
preserve

collapse (count) n_wave1=hh_id ///
         (sum) n_attrited=attrit ///
         (mean) attrition_rate=attrit, by(province)

generate n_retained = n_wave1 - n_attrited
generate retention_rate = 1 - attrition_rate

* Sort by attrition rate (descending)
gsort -attrition_rate

display as text "Attrition Rates by Province (Sorted by Attrition Rate):"
display as text "-" * 70

list province n_wave1 n_retained n_attrited attrition_rate, ///
     noobs separator(0) ///
     table(province(Province) n_wave1(Wave_1_N) n_retained(Wave_2_N) ///
           n_attrited(Attrited_N) attrition_rate(Attrit_Rate))

restore

display ""
display as text "┌─────────────────────────────────────────────────────────────────────┐"
display as text "│  KEY INSIGHT: Non-Random Attrition                                 │"
display as text "├─────────────────────────────────────────────────────────────────────┤"
display as text "│  Notice that attrition rates vary substantially by province:       │"
display as text "│    - Highest: Zambara Capital (urban mobility)                     │"
display as text "│    - Lowest: Northern Bushveld (stable rural communities)          │"
display as text "│                                                                     │"
display as text "│  This NON-RANDOM pattern means that simple unweighted estimates    │"
display as text "│  from Wave 2 will be BIASED. We address this in Lab 5.2.          │"
display as text "└─────────────────────────────────────────────────────────────────────┘"
display ""

********************************************************************************
* SECTION 5: CREATE WAVE 2 DATASET
********************************************************************************

display as text "=" * 70
display as result "CREATING WAVE 2 DATASET"
display as text "=" * 70
display ""

* Save Wave 1 data with attrition indicators
save "$outpath/zambara_wave1_with_attrition.dta", replace
display as text "Saved Wave 1 with attrition indicators"

* Create Wave 2 dataset from retained households only
keep if attrit == 0

* Update wave identifier
replace wave = 2

* -----------------------------------------------------------------------------
* Simulate income change between Wave 1 and Wave 2
* Income changes follow a mixture distribution:
*   - Most households: small random change (normal, mean=0, sd=0.10)
*   - Some households: larger positive change (job gained)
*   - Some households: larger negative change (job lost)
* -----------------------------------------------------------------------------

* Generate change type (70% stable, 15% increase, 15% decrease)
generate double rand_change = runiform()
generate change_type = cond(rand_change < 0.70, 1, ///
                       cond(rand_change < 0.85, 2, 3))
label define change_lbl 1 "Stable" 2 "Increase" 3 "Decrease"
label values change_type change_lbl

* Calculate income multiplier based on change type
generate double income_multiplier = .
replace income_multiplier = 1.02 + 0.08 * invnormal(runiform()) if change_type == 1
replace income_multiplier = 1.25 + 0.15 * invnormal(runiform()) if change_type == 2
replace income_multiplier = 0.80 + 0.12 * invnormal(runiform()) if change_type == 3

* Apply multiplier to get Wave 2 income (ensure non-negative)
generate double income_w2 = max(0, income_w1 * income_multiplier)

* Calculate income change
generate double income_change = income_w2 - income_w1
generate double income_pct_change = (income_change / income_w1) * 100 if income_w1 > 0

local n_wave2 = _N
display as text "Wave 2 dataset created:"
display as text "  Households in Wave 2: " as result %12.0fc `n_wave2'
display as text "  Retention rate: " as result %5.1f (`n_wave2' / `n_wave1' * 100) "%"
display ""

* Wave 2 summary by province
display as text "Wave 2 Summary by Province:"
display as text "-" * 70
tabstat income_w2, by(province) stat(n mean) nototal format(%12.0fc)
display ""

* Save Wave 2 dataset
preserve
keep hh_id psu stratum hh_weight province geotype hh_size head_age head_sex ///
     income_w2 wave
save "$outpath/zambara_wave2.dta", replace
display as text "Saved Wave 2 dataset"
restore

********************************************************************************
* SECTION 6: PANEL DATA LINKING
********************************************************************************

display as text "=" * 70
display as result "PANEL DATA LINKING: THE 'TIME TRAVEL' MERGE"
display as text "=" * 70
display ""

* -----------------------------------------------------------------------------
* The unique household identifier (hh_id) is our "time travel" key
* It allows us to link the same household across different waves
* -----------------------------------------------------------------------------

* Reload Wave 1 for merge
use "$outpath/zambara_wave1_with_attrition.dta", clear

* Keep Wave 1 variables for merge
keep hh_id income_w1 province geotype hh_size head_age head_sex ///
     hh_weight psu stratum attrit

* Rename for clarity in merged dataset
rename income_w1 income_wave1

* Merge with Wave 2
merge 1:1 hh_id using "$outpath/zambara_wave2.dta", ///
      keepusing(income_w2) keep(match) nogenerate

* Rename for consistency
rename income_w2 income_wave2

local n_linked = _N

display as text "Panel Linking Results:"
display as text "-" * 70
display as text "  Wave 1 households: " as result %12.0fc `n_wave1'
display as text "  Wave 2 households: " as result %12.0fc `n_wave2'
display as text "  Linked panel: " as result %12.0fc `n_linked' ///
        as text " households (balanced panel)"
display ""

* Calculate income changes for linked panel
generate income_change = income_wave2 - income_wave1
generate income_pct_change = (income_change / income_wave1) * 100 if income_wave1 > 0

* Summary of income changes
display as text "Income Dynamics in Linked Panel:"
display as text "-" * 70

summarize income_wave1 income_wave2 income_change income_pct_change, format

* Calculate proportions with increase/decrease
count if income_change > 0
local pct_increase = r(N) / _N * 100
count if income_change < 0
local pct_decrease = r(N) / _N * 100

display ""
display as text "  Households with income increase: " as result %5.1f `pct_increase' "%"
display as text "  Households with income decrease: " as result %5.1f `pct_decrease' "%"
display ""

* Save linked panel
save "$outpath/zambara_panel_linked.dta", replace
display as text "Saved linked panel dataset"
display ""

********************************************************************************
* SECTION 7: COMPARE SURVIVORS VS. ATTRITORS
********************************************************************************

display as text "=" * 70
display as result "COMPARING SURVIVORS VS. ATTRITORS (Wave 1 Characteristics)"
display as text "=" * 70
display ""

* Reload Wave 1 with attrition indicators
use "$outpath/zambara_wave1_with_attrition.dta", clear

* This comparison reveals the selection bias introduced by attrition
display as text "Characteristics Comparison:"
display as text "-" * 70

* Create comparison table
tabstat income_w1 hh_size head_age, by(attrit) stat(n mean median) ///
        nototal format(%12.2fc)

* Urban percentage by attrition status
display ""
display as text "Percent Urban by Attrition Status:"
tabulate geotype attrit, col nofreq

* Statistical test for income difference
display ""
display as text "T-test for Income Difference Between Groups:"
ttest income_w1, by(attrit)

display ""
display as text ">>> If p-value < 0.05, this confirms NON-RANDOM attrition."
display as text ">>> Attritors differ systematically from survivors."
display ""

********************************************************************************
* SECTION 8: FINAL ATTRITION REPORT TABLE
********************************************************************************

display as text "=" * 70
display as result "FINAL ATTRITION REPORT BY PROVINCE"
display as text "=" * 70
display ""

display as text "ZAMBARA QUARTERLY LABOUR FORCE SURVEY"
display as text "Attrition Analysis: Wave 1 to Wave 2"
display as text "-" * 70
display ""

* Create final attrition report
preserve

collapse (count) wave1_n=hh_id ///
         (sum) attrited_n=attrit ///
         (mean) attrition_rate=attrit, by(province)

generate wave2_n = wave1_n - attrited_n
generate retention_rate = (1 - attrition_rate) * 100
replace attrition_rate = attrition_rate * 100

* Display report
list province wave1_n wave2_n attrited_n attrition_rate retention_rate, ///
     noobs separator(0) ///
     table(province("Province") wave1_n("Wave 1 (N)") wave2_n("Wave 2 (N)") ///
           attrited_n("Attrited (N)") attrition_rate("Attrit Rate %") ///
           retention_rate("Retain Rate %"))

display ""
display as text "-" * 70

* Calculate and display totals
summarize wave1_n, meanonly
local total_w1 = r(sum)
summarize wave2_n, meanonly
local total_w2 = r(sum)
summarize attrited_n, meanonly
local total_attrit = r(sum)
local total_attrit_rate = `total_attrit' / `total_w1' * 100
local total_retain_rate = 100 - `total_attrit_rate'

display as text "TOTAL ZAMBARA" _column(20) ///
        as result %8.0fc `total_w1' _column(32) ///
        as result %8.0fc `total_w2' _column(44) ///
        as result %8.0fc `total_attrit' _column(56) ///
        as result %5.1f `total_attrit_rate' _column(68) ///
        as result %5.1f `total_retain_rate'

* Export to CSV
export delimited province wave1_n wave2_n attrited_n attrition_rate retention_rate ///
       using "$outpath/attrition_report_by_province.csv", replace

restore

display ""
display as text "Exported: $outpath/attrition_report_by_province.csv"
display ""

********************************************************************************
* SECTION 9: VISUALIZATION
********************************************************************************

display as text "=" * 70
display as result "GENERATING VISUALIZATION"
display as text "=" * 70
display ""

* Create attrition rate bar chart
preserve

collapse (mean) attrition_rate=attrit, by(province)
replace attrition_rate = attrition_rate * 100

* Sort by attrition rate
gsort -attrition_rate
generate rank = _n

* Calculate overall average for reference line
summarize attrition_rate, meanonly
local avg_attrit = r(mean)

* Create bar chart
graph hbar attrition_rate, over(province, sort(rank) descending) ///
      bar(1, color(navy)) ///
      yline(`avg_attrit', lpattern(dash) lcolor(gold)) ///
      ytitle("Attrition Rate (%)") ///
      title("Attrition Rates by Province", size(medium)) ///
      subtitle("Zambara QLFS Wave 1 to Wave 2", size(small)) ///
      note("Dashed line = National average ({&cong}`=string(`avg_attrit', "%4.1f")'%)" ///
           "Source: ZQLFS Pilot Simulation | SADC Training Workshop 2026", size(vsmall)) ///
      scheme(s2color)

graph export "$outpath/attrition_by_province.png", replace width(1000) height(600)

restore

display as text "Saved: $outpath/attrition_by_province.png"
display ""

********************************************************************************
* SECTION 10: SUMMARY AND NEXT STEPS
********************************************************************************

display as text "=" * 70
display as result "LAB 5.1 COMPLETE: SUMMARY AND NEXT STEPS"
display as text "=" * 70
display ""

display as text "WHAT WE ACCOMPLISHED:"
display as text "  1. Loaded GHS 2024 data as Zambara QLFS Wave 1"
display as text "  2. Simulated non-random attrition for Wave 2"
display as text "  3. Created linked panel dataset across waves"
display as text "  4. Calculated attrition rates by province"
display as text "  5. Demonstrated selection bias between survivors and attritors"
display ""

display as text "KEY FINDINGS:"
display as text "  - Overall attrition rate: " as result %5.1f `overall_attrition' "%"
display as text "  - Attrition is NOT random — varies by province and household type"
display as text "  - Urban areas and mobile households more likely to attrit"
display as text "  - This creates BIAS if not addressed through weighting"
display ""

display as text "NEXT STEPS (Lab 5.2):"
display as text "  - Develop response propensity models"
display as text "  - Calculate attrition weight adjustments"
display as text "  - Compare weighted vs. unweighted estimates"
display as text "  - Assess bias reduction from attrition weights"
display ""

display as text "OUTPUT FILES SAVED:"
display as text "  - $outpath/zambara_wave1_with_attrition.dta"
display as text "  - $outpath/zambara_wave2.dta"
display as text "  - $outpath/zambara_panel_linked.dta"
display as text "  - $outpath/attrition_report_by_province.csv"
display as text "  - $outpath/attrition_by_province.png"
display ""

display as text "=" * 70
display as result "END OF LAB 5.1"
display as text "=" * 70

********************************************************************************
*                              END OF SCRIPT                                   *
********************************************************************************
