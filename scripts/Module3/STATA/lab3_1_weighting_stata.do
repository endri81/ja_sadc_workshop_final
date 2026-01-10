* ============================================================================
* LAB 3.2: CALIBRATION (RAKING)
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 3: Advanced Weighting and Variance Estimation
* ============================================================================
* 
* LEARNING OBJECTIVES:
*   1. Define population control totals for calibration
*   2. Perform raking (iterative proportional fitting) to calibrate weights
*   3. Compare NR-adjusted vs. calibrated weight estimates
*   4. Analyze g-weights (calibration adjustment factors)
*
* DATA: GHS 2024 Household File (ghs-2024-hhold-v1.dta)
* 
* REQUIRED: STATA 15+ with ipfraking package (ssc install ipfraking)
* ============================================================================

clear all
set more off
capture log close

* Create output directory
capture mkdir "lab3_2_output"

* Start log
log using "lab3_2_output/lab3_2_calibration.log", replace

display _n
display "=" * 70
display "LAB 3.2: CALIBRATION (RAKING)"
display "=" * 70
display _n

* ============================================================================
* SECTION 1: LOAD DATA AND EXAMINE STRUCTURE
* ============================================================================

display _n
display "=" * 70
display "SECTION 1: DATA LOADING AND PREPARATION"
display "=" * 70
display _n

* Load GHS 2024 household data
use "ghs-2024-hhold-v1.dta", clear

* Display dataset information
describe, short
display _n "Observations: " _N

* Check key variables
display _n "Checking key variables:"
describe psu stratum prov geotype house_wgt totmhinc

* Examine distributions
display _n "Province Distribution:"
tab prov

display _n "GeoType Distribution:"
tab geotype

* ============================================================================
* SECTION 2: SIMULATE NON-RESPONSE (from Lab 3.1)
* ============================================================================

display _n
display "=" * 70
display "SECTION 2: SIMULATE NON-RESPONSE (from Lab 3.1)"
display "=" * 70
display _n

* Set seed for reproducibility
set seed 2024

* Define NR probabilities by GeoType
* Urban = 0.30, Traditional = 0.18, Farms = 0.10
gen nr_prob = .
replace nr_prob = 0.30 if geotype == 1  // Urban
replace nr_prob = 0.18 if geotype == 2  // Traditional
replace nr_prob = 0.10 if geotype == 3  // Farms

* Simulate response indicator (1 = responded, 0 = non-response)
gen response = runiform() > nr_prob

* Check response rates
display _n "Simulated Response Rates by GeoType:"
tab geotype response, row

* ============================================================================
* SECTION 3: APPLY NON-RESPONSE ADJUSTMENT (from Lab 3.1)
* ============================================================================

display _n
display "=" * 70
display "SECTION 3: NON-RESPONSE ADJUSTMENT"
display "=" * 70
display _n

* Create weighting class (Province x GeoType)
egen wgt_class = group(prov geotype), label

* Calculate NR adjustment factors
bysort wgt_class: gen n_sampled = _N
bysort wgt_class: egen n_responded = total(response)
gen nr_factor = n_sampled / n_responded

* Display top NR factors
display _n "NR Adjustment Factors by Weighting Class:"
table wgt_class, stat(mean nr_factor) stat(freq)

* Calculate NR-adjusted weights
gen base_wgt = house_wgt
gen wgt_nr = base_wgt * nr_factor if response == 1

* Keep only respondents
keep if response == 1

display _n "Respondent sample size: " _N
summarize wgt_nr
display "Sum of NR-adjusted weights: " %15.0fc r(sum)

* ============================================================================
* SECTION 4: DEFINE POPULATION CONTROL TOTALS
* ============================================================================

display _n
display "=" * 70
display "SECTION 4: POPULATION CONTROL TOTALS"
display "=" * 70
display _n

* -----------------------------------------------------------------------------
* PROVINCE POPULATION TOTALS (from Census/Admin data)
* -----------------------------------------------------------------------------
* Note: These are hypothetical totals for training purposes
* In practice, obtain from census or administrative records

display "Population Control Totals by Province:"
display "  1. Western Cape:    2,150,000"
display "  2. Eastern Cape:    1,950,000"
display "  3. Northern Cape:     420,000"
display "  4. Free State:        890,000"
display "  5. KwaZulu-Natal:   3,200,000"
display "  6. North West:      1,180,000"
display "  7. Gauteng:         5,400,000"
display "  8. Mpumalanga:      1,450,000"
display "  9. Limpopo:         1,750,000"
display "  TOTAL:             18,390,000"

* Store province totals in matrix
matrix pop_prov = (2150000 \ 1950000 \ 420000 \ 890000 \ 3200000 \ ///
                   1180000 \ 5400000 \ 1450000 \ 1750000)

* -----------------------------------------------------------------------------
* GEOTYPE POPULATION TOTALS
* -----------------------------------------------------------------------------
display _n "Population Control Totals by GeoType:"
display "  1. Urban:       12,500,000"
display "  2. Traditional:  5,200,000"
display "  3. Farms:          690,000"
display "  TOTAL:          18,390,000"

* Store geotype totals
matrix pop_geo = (12500000 \ 5200000 \ 690000)

* Scale geotype to match province total (if needed)
scalar total_prov = 18390000
scalar total_geo = 12500000 + 5200000 + 690000
if total_prov != total_geo {
    display _n "Note: Scaling GeoType totals to match Province total"
    matrix pop_geo = pop_geo * (total_prov / total_geo)
}

* ============================================================================
* SECTION 5: CHECK CURRENT WEIGHTED TOTALS
* ============================================================================

display _n
display "=" * 70
display "SECTION 5: CURRENT WEIGHTED TOTALS (PRE-CALIBRATION)"
display "=" * 70
display _n

* Current weighted totals by Province
display "Current Weighted Totals by Province (NR-adjusted):"
table prov, stat(sum wgt_nr) stat(freq)

* Current weighted totals by GeoType
display _n "Current Weighted Totals by GeoType (NR-adjusted):"
table geotype, stat(sum wgt_nr) stat(freq)

* ============================================================================
* SECTION 6: PERFORM RAKING (CALIBRATION)
* ============================================================================

display _n
display "=" * 70
display "SECTION 6: RAKING (ITERATIVE PROPORTIONAL FITTING)"
display "=" * 70
display _n

* -----------------------------------------------------------------------------
* METHOD 1: Using ipfraking package (recommended)
* Install if needed: ssc install ipfraking
* -----------------------------------------------------------------------------

display "Performing raking using ipfraking..."
display _n

* Create province target variable
gen prov_target = .
replace prov_target = 2150000 if prov == 1
replace prov_target = 1950000 if prov == 2
replace prov_target = 420000 if prov == 3
replace prov_target = 890000 if prov == 4
replace prov_target = 3200000 if prov == 5
replace prov_target = 1180000 if prov == 6
replace prov_target = 5400000 if prov == 7
replace prov_target = 1450000 if prov == 8
replace prov_target = 1750000 if prov == 9

* Create geotype target variable (scaled to match province total)
gen geotype_target = .
replace geotype_target = 12500000 if geotype == 1
replace geotype_target = 5200000 if geotype == 2
replace geotype_target = 690000 if geotype == 3

* Check if ipfraking is installed
capture which ipfraking
if _rc != 0 {
    display as error "ipfraking not installed. Installing now..."
    ssc install ipfraking
}

* Perform raking
* Note: ipfraking performs iterative proportional fitting
ipfraking [pw=wgt_nr], generate(wgt_cal) ///
    ctotal(prov prov_target) ///
    ctotal(geotype geotype_target) ///
    iterate(50) tolerance(0.0001)

display _n "Raking completed successfully!"

* -----------------------------------------------------------------------------
* METHOD 2: Manual IPF (Alternative if ipfraking unavailable)
* -----------------------------------------------------------------------------
/*
* Initialize calibrated weight
gen wgt_cal = wgt_nr

* Iterative Proportional Fitting - Manual Implementation
forvalues iter = 1/20 {
    
    * Step 1: Adjust to Province margins
    forvalues p = 1/9 {
        quietly sum wgt_cal if prov == `p'
        local current_total = r(sum)
        local target = pop_prov[`p',1]
        replace wgt_cal = wgt_cal * (`target' / `current_total') if prov == `p'
    }
    
    * Step 2: Adjust to GeoType margins
    forvalues g = 1/3 {
        quietly sum wgt_cal if geotype == `g'
        local current_total = r(sum)
        local target = pop_geo[`g',1]
        replace wgt_cal = wgt_cal * (`target' / `current_total') if geotype == `g'
    }
    
    * Check convergence
    if `iter' == 20 {
        display "Manual IPF completed after 20 iterations"
    }
}
*/

* ============================================================================
* SECTION 7: CALCULATE G-WEIGHTS (CALIBRATION FACTORS)
* ============================================================================

display _n
display "=" * 70
display "SECTION 7: G-WEIGHTS (CALIBRATION ADJUSTMENT FACTORS)"
display "=" * 70
display _n

* G-weight = Calibrated weight / NR-adjusted weight
gen g_weight = wgt_cal / wgt_nr

* G-weight diagnostics
display "G-Weight Diagnostics:"
summarize g_weight, detail

* G-weight by Province
display _n "G-Weight Summary by Province:"
table prov, stat(mean g_weight) stat(min g_weight) stat(max g_weight) stat(freq)

* G-weight by GeoType
display _n "G-Weight Summary by GeoType:"
table geotype, stat(mean g_weight) stat(min g_weight) stat(max g_weight) stat(freq)

* ============================================================================
* SECTION 8: VERIFY CALIBRATION
* ============================================================================

display _n
display "=" * 70
display "SECTION 8: VERIFY CALIBRATION"
display "=" * 70
display _n

* Check calibrated totals by Province
display "Verification: Calibrated Totals vs. Population Targets"
display _n "--- Province ---"

forvalues p = 1/9 {
    quietly sum wgt_cal if prov == `p'
    local cal_total = r(sum)
    local target = pop_prov[`p',1]
    local diff = `cal_total' - `target'
    local pct_diff = 100 * `diff' / `target'
    display "Province `p': Cal=" %12.0fc `cal_total' " Target=" %12.0fc `target' ///
            " Diff=" %8.2f `pct_diff' "%"
}

* Check calibrated totals by GeoType
display _n "--- GeoType ---"

forvalues g = 1/3 {
    quietly sum wgt_cal if geotype == `g'
    local cal_total = r(sum)
    local target = pop_geo[`g',1]
    local diff = `cal_total' - `target'
    local pct_diff = 100 * `diff' / `target'
    display "GeoType `g': Cal=" %12.0fc `cal_total' " Target=" %12.0fc `target' ///
            " Diff=" %8.2f `pct_diff' "%"
}

* ============================================================================
* SECTION 9: COMPARE ESTIMATES - NR-ADJUSTED VS CALIBRATED
* ============================================================================

display _n
display "=" * 70
display "SECTION 9: COMPARE ESTIMATES - totmhinc (Total Monthly Income)"
display "=" * 70
display _n

* Set survey design
svyset psu [pw=wgt_nr], strata(stratum)

* Check missing values
count if missing(totmhinc)
display "Missing values in totmhinc: " r(N)

* -----------------------------------------------------------------------------
* Overall Mean Income
* -----------------------------------------------------------------------------
display _n "=== Mean Total Monthly Household Income ==="

* NR-adjusted estimate
display _n "NR-Adjusted Weights:"
svy: mean totmhinc
matrix nr_est = e(b)
matrix nr_se = e(V)
scalar nr_mean = nr_est[1,1]
scalar nr_se_val = sqrt(nr_se[1,1])
scalar nr_cv = 100 * nr_se_val / nr_mean
display "  Mean: R " %10.2f nr_mean
display "  SE:   R " %10.2f nr_se_val
display "  CV:      " %6.2f nr_cv "%"

* Calibrated estimate
svyset psu [pw=wgt_cal], strata(stratum)
display _n "Calibrated Weights:"
svy: mean totmhinc
matrix cal_est = e(b)
matrix cal_se = e(V)
scalar cal_mean = cal_est[1,1]
scalar cal_se_val = sqrt(cal_se[1,1])
scalar cal_cv = 100 * cal_se_val / cal_mean
display "  Mean: R " %10.2f cal_mean
display "  SE:   R " %10.2f cal_se_val
display "  CV:      " %6.2f cal_cv "%"

* Difference
display _n "Difference (Calibrated - NR-Adjusted):"
scalar mean_diff = cal_mean - nr_mean
scalar pct_diff = 100 * mean_diff / nr_mean
display "  Mean diff: R " %10.2f mean_diff " (" %5.2f pct_diff "%)"

* -----------------------------------------------------------------------------
* Mean Income by Province
* -----------------------------------------------------------------------------
display _n "=== Mean Income by Province ==="

display _n "NR-Adjusted Weights:"
svyset psu [pw=wgt_nr], strata(stratum)
svy: mean totmhinc, over(prov)

display _n "Calibrated Weights:"
svyset psu [pw=wgt_cal], strata(stratum)
svy: mean totmhinc, over(prov)

* -----------------------------------------------------------------------------
* Mean Income by GeoType
* -----------------------------------------------------------------------------
display _n "=== Mean Income by GeoType ==="

display _n "NR-Adjusted Weights:"
svyset psu [pw=wgt_nr], strata(stratum)
svy: mean totmhinc, over(geotype)

display _n "Calibrated Weights:"
svyset psu [pw=wgt_cal], strata(stratum)
svy: mean totmhinc, over(geotype)

* ============================================================================
* SECTION 10: WEIGHT DIAGNOSTICS COMPARISON
* ============================================================================

display _n
display "=" * 70
display "SECTION 10: WEIGHT DIAGNOSTICS COMPARISON"
display "=" * 70
display _n

* NR-Adjusted weight diagnostics
display "NR-Adjusted Weights:"
summarize wgt_nr, detail
scalar nr_sum = r(sum)
scalar nr_mean_wgt = r(mean)
scalar nr_sd = r(sd)
scalar nr_cv_wgt = nr_sd / nr_mean_wgt
scalar nr_min = r(min)
scalar nr_max = r(max)
scalar nr_ratio = nr_max / nr_min
scalar nr_deff = 1 + nr_cv_wgt^2

* Calibrated weight diagnostics
display _n "Calibrated Weights:"
summarize wgt_cal, detail
scalar cal_sum = r(sum)
scalar cal_mean_wgt = r(mean)
scalar cal_sd = r(sd)
scalar cal_cv_wgt = cal_sd / cal_mean_wgt
scalar cal_min = r(min)
scalar cal_max = r(max)
scalar cal_ratio = cal_max / cal_min
scalar cal_deff = 1 + cal_cv_wgt^2

* Display comparison table
display _n "Weight Diagnostics Comparison:"
display _n "Metric          NR-Adjusted    Calibrated"
display "-----------------------------------------------"
display "Sum             " %12.0fc nr_sum "  " %12.0fc cal_sum
display "Mean            " %12.2f nr_mean_wgt "  " %12.2f cal_mean_wgt
display "Std Dev         " %12.2f nr_sd "  " %12.2f cal_sd
display "CV              " %12.4f nr_cv_wgt "  " %12.4f cal_cv_wgt
display "Min             " %12.2f nr_min "  " %12.2f cal_min
display "Max             " %12.2f nr_max "  " %12.2f cal_max
display "Max/Min         " %12.2f nr_ratio "  " %12.2f cal_ratio
display "DEFF_w          " %12.4f nr_deff "  " %12.4f cal_deff

* ============================================================================
* SECTION 11: VISUALIZATIONS
* ============================================================================

display _n
display "=" * 70
display "SECTION 11: VISUALIZATIONS"
display "=" * 70
display _n

* -----------------------------------------------------------------------------
* PLOT 1: G-Weight Distribution
* -----------------------------------------------------------------------------
histogram g_weight, ///
    bin(50) ///
    frequency ///
    color(navy%70) ///
    xline(1, lcolor(red) lpattern(dash) lwidth(medium)) ///
    title("Distribution of G-Weights (Calibration Factors)") ///
    subtitle("Raking on Province and GeoType margins") ///
    xtitle("G-Weight (Calibrated / NR-Adjusted)") ///
    ytitle("Frequency") ///
    note("Vertical line at g = 1.0 (no adjustment)") ///
    scheme(s2color)

graph export "lab3_2_output/g_weight_distribution.png", replace width(1200)
display "Saved: g_weight_distribution.png"

* -----------------------------------------------------------------------------
* PLOT 2: G-Weight Boxplot by Province
* -----------------------------------------------------------------------------
label define prov_lbl 1 "W Cape" 2 "E Cape" 3 "N Cape" 4 "Free St" ///
    5 "KZN" 6 "N West" 7 "Gauteng" 8 "Mpum" 9 "Limpopo"
label values prov prov_lbl

graph box g_weight, over(prov) ///
    yline(1, lcolor(red) lpattern(dash)) ///
    title("G-Weight Distribution by Province") ///
    ytitle("G-Weight") ///
    note("Red line: g = 1.0 (no adjustment)") ///
    scheme(s2color)

graph export "lab3_2_output/g_weight_by_province.png", replace width(1400)
display "Saved: g_weight_by_province.png"

* -----------------------------------------------------------------------------
* PLOT 3: G-Weight Boxplot by GeoType
* -----------------------------------------------------------------------------
label define geo_lbl 1 "Urban" 2 "Traditional" 3 "Farms"
label values geotype geo_lbl

graph box g_weight, over(geotype) ///
    yline(1, lcolor(red) lpattern(dash)) ///
    title("G-Weight Distribution by GeoType") ///
    ytitle("G-Weight") ///
    note("Red line: g = 1.0 (no adjustment)") ///
    scheme(s2color)

graph export "lab3_2_output/g_weight_by_geotype.png", replace width(1000)
display "Saved: g_weight_by_geotype.png"

* -----------------------------------------------------------------------------
* PLOT 4: Weight Comparison - NR-Adjusted vs Calibrated
* -----------------------------------------------------------------------------
twoway (kdensity wgt_nr, color(navy%50) lwidth(medium)) ///
       (kdensity wgt_cal, color(forest_green%50) lwidth(medium)), ///
    title("Weight Distribution: NR-Adjusted vs. Calibrated") ///
    xtitle("Weight Value") ///
    ytitle("Density") ///
    legend(order(1 "NR-Adjusted" 2 "Calibrated") rows(1) pos(6)) ///
    scheme(s2color)

graph export "lab3_2_output/weight_comparison_nr_vs_cal.png", replace width(1200)
display "Saved: weight_comparison_nr_vs_cal.png"

* -----------------------------------------------------------------------------
* PLOT 5: Scatter - NR-Adjusted vs Calibrated Weights
* -----------------------------------------------------------------------------
twoway (scatter wgt_cal wgt_nr, msize(tiny) mcolor(navy%30)) ///
       (line wgt_nr wgt_nr, lcolor(red) lpattern(dash)), ///
    title("NR-Adjusted vs. Calibrated Weights") ///
    xtitle("NR-Adjusted Weight") ///
    ytitle("Calibrated Weight") ///
    legend(order(1 "Observations" 2 "45° line") rows(1) pos(6)) ///
    scheme(s2color)

graph export "lab3_2_output/weight_scatter_nr_vs_cal.png", replace width(1000)
display "Saved: weight_scatter_nr_vs_cal.png"

* ============================================================================
* SECTION 12: EXPORT RESULTS
* ============================================================================

display _n
display "=" * 70
display "SECTION 12: EXPORT RESULTS"
display "=" * 70
display _n

* Keep key variables and export
preserve
keep psu stratum prov geotype wgt_class base_wgt nr_factor wgt_nr wgt_cal g_weight totmhinc

* Export to Stata format
save "lab3_2_output/ghs2024_calibrated.dta", replace
display "Exported: ghs2024_calibrated.dta"

* Export to CSV
export delimited using "lab3_2_output/ghs2024_calibrated.csv", replace
display "Exported: ghs2024_calibrated.csv"
restore

* Export g-weight summary
preserve
collapse (mean) mean_g=g_weight (min) min_g=g_weight (max) max_g=g_weight ///
         (count) n=g_weight, by(prov)
export delimited using "lab3_2_output/gweight_by_province.csv", replace
display "Exported: gweight_by_province.csv"
restore

preserve
collapse (mean) mean_g=g_weight (min) min_g=g_weight (max) max_g=g_weight ///
         (count) n=g_weight, by(geotype)
export delimited using "lab3_2_output/gweight_by_geotype.csv", replace
display "Exported: gweight_by_geotype.csv"
restore

* ============================================================================
* SECTION 13: SUMMARY
* ============================================================================

display _n
display "=" * 70
display "LAB 3.2 SUMMARY: CALIBRATION (RAKING)"
display "=" * 70
display _n

display "KEY FINDINGS:"
display "-------------"
display "1. Sample size (respondents): " _N
display "2. Raking margins: Province (9 groups) x GeoType (3 groups)"
summarize g_weight, meanonly
display "3. G-weight range: " %6.4f r(min) " to " %6.4f r(max)
display "4. Mean g-weight: " %6.4f r(mean) " (should be ~1.0)"
display "5. Overall mean income change: " %6.2f pct_diff "% after calibration"

display _n "CALIBRATION ACHIEVED:"
display "- Province margins: Matched to population totals"
display "- GeoType margins: Matched to population totals"

display _n "OUTPUT FILES:"
display "- lab3_2_output/ghs2024_calibrated.dta"
display "- lab3_2_output/ghs2024_calibrated.csv"
display "- lab3_2_output/g_weight_distribution.png"
display "- lab3_2_output/g_weight_by_province.png"
display "- lab3_2_output/g_weight_by_geotype.png"
display "- lab3_2_output/weight_comparison_nr_vs_cal.png"
display "- lab3_2_output/weight_scatter_nr_vs_cal.png"

display _n
display "=" * 70
display "END OF LAB 3.2"
display "=" * 70

log close
