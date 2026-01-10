*===============================================================================
* LAB 1.6: SAMPLING RARE POPULATIONS
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 1, Module 1.7
*
* Objective: Compare sample size requirements for rare population estimation
*            using Simple Random Sampling vs. Stratified Sampling
*
* Data: GHS 2024 Household File (ghs-2024-hhold-v1.dta)
* Rare Population: High-income households (Top 5% of income distribution)
*
* Author: SADC Workshop Team
* Date: March 2026
*===============================================================================

clear all
set more off
cap log close

*-------------------------------------------------------------------------------
* SECTION 1: SETUP AND DATA LOADING
*-------------------------------------------------------------------------------

display _newline
display as text "=" * 71
display as result "LAB 1.6: SAMPLING RARE POPULATIONS"
display as text "Comparing SRS vs. Stratified Sampling for High-Income Households"
display as text "=" * 71
display _newline

* Set working directory (adjust as needed)
* cd "path/to/your/data"

* Start log file
cap mkdir lab1_6_outputs
log using "lab1_6_outputs/lab1_6_rare_populations.log", replace

* Load the GHS 2024 household data
display as text "Loading GHS 2024 household data..."
use "ghs-2024-hhold-v1.dta", clear

display as result "Dataset loaded successfully."
display as text "  - Observations: " _continue
display as result _N
display as text "  - Variables: " _continue
qui describe
display as result r(k)
display _newline

*-------------------------------------------------------------------------------
* SECTION 2: EXPLORE INCOME DISTRIBUTION
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 2: INCOME DISTRIBUTION ANALYSIS"
display as text "-" * 71
display _newline

* Check available income variables
display as text "Checking for income variables..."
describe *inc* *salary* *earn* *wage*

* Use total monthly household income (totmhinc)
* Generate income variable
cap confirm variable totmhinc
if _rc == 0 {
    gen income = totmhinc
    display as result "Using 'totmhinc' (Total Monthly Household Income) as income measure."
}
else {
    * Try alternative variables
    cap confirm variable hhincome
    if _rc == 0 {
        gen income = hhincome
        display as result "Using 'hhincome' as income measure."
    }
    else {
        display as error "No standard income variable found. Please check variable names."
        exit 198
    }
}
display _newline

* Count missing/negative values
count if missing(income) | income < 0
local n_missing = r(N)
local n_original = _N

display as text "Income data cleaning:"
display as text "  - Original observations: " _continue
display as result %12.0fc `n_original'

* Keep only valid income observations
drop if missing(income) | income < 0
local n_clean = _N
local n_dropped = `n_original' - `n_clean'

display as text "  - After removing missing/negative: " _continue
display as result %12.0fc `n_clean'
display as text "  - Records removed: " _continue
display as result %12.0fc `n_dropped'
display _newline

* Summary statistics for income
display as text "Income Distribution Summary:"
summarize income, detail
display _newline

* Calculate and store percentiles
_pctile income, percentiles(50 75 90 95 99)
local p50 = r(r1)
local p75 = r(r2)
local p90 = r(r3)
local p95 = r(r4)
local p99 = r(r5)

display as text "Income Percentiles:"
display as text "  50th percentile (Median): R" %12.0fc `p50'
display as text "  75th percentile:          R" %12.0fc `p75'
display as text "  90th percentile:          R" %12.0fc `p90'
display as text "  95th percentile:          R" %12.0fc `p95'
display as text "  99th percentile:          R" %12.0fc `p99'
display _newline

*-------------------------------------------------------------------------------
* SECTION 3: DEFINE RARE POPULATION (TOP 5% INCOME)
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 3: DEFINING THE RARE POPULATION"
display as text "-" * 71
display _newline

* Define threshold for top 5% (95th percentile)
local income_threshold = `p95'

display as text "Rare Population Definition:"
display as text "  High-income threshold (95th percentile): R" %12.0fc `income_threshold'
display _newline

* Create indicator for rare population
gen high_income = (income >= `income_threshold')
label variable high_income "High-income household (top 5%)"
label define high_income_lbl 0 "Not high-income" 1 "High-income"
label values high_income high_income_lbl

* Calculate prevalence
local N_total = _N
count if high_income == 1
local N_rare = r(N)

summarize high_income, meanonly
local prevalence = r(mean)
local Q = 1 - `prevalence'
local var_p = `prevalence' * `Q'

display as text "Rare Population Characteristics:"
display as text "  Total households in frame:      " %12.0fc `N_total'
display as text "  High-income households:         " %12.0fc `N_rare'
display as text "  Population prevalence (P):      " %9.4f `prevalence' ///
    " (" %5.2f `prevalence' * 100 "%)"
display _newline
display as text "  Variance of proportion (P×Q):   " %9.6f `var_p'
display _newline

*-------------------------------------------------------------------------------
* SECTION 4: SAMPLE SIZE CALCULATION - SIMPLE RANDOM SAMPLING
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 4: SAMPLE SIZE FOR SIMPLE RANDOM SAMPLING (SRS)"
display as text "-" * 71
display _newline

* Parameters for sample size calculation
local confidence_level = 0.95
local alpha = 1 - `confidence_level'
local z_value = invnormal(1 - `alpha'/2)  // 1.96 for 95% CI
local relative_precision = 0.10  // 10% relative precision (CV)

display as text "Design Parameters:"
display as text "  Confidence level:          " %5.0f `confidence_level' * 100 "%"
display as text "  Z-value:                   " %9.4f `z_value'
display as text "  Relative precision (CV):   " %5.0f `relative_precision' * 100 "%"
display _newline

* Calculate absolute margin of error
* Relative precision of 10% means: SE/P = 0.10, so SE = 0.10 * P
* Margin of error (e) = z * SE = z * 0.10 * P
local e_absolute = `relative_precision' * `prevalence'
local margin_of_error = `z_value' * `e_absolute'

display as text "  Absolute margin of error (e): " %9.6f `e_absolute'
display as text "  (This means estimate will be P ± " %7.4f `margin_of_error' ")"
display _newline

*-------------------------------------------------------------------------------
* Formula for SRS sample size for proportion:
* n = (z² × P × (1-P)) / e²
* where e = relative_precision × P (for relative precision)
*
* Alternatively, for coefficient of variation (CV):
* n = (z² × (1-P)) / (CV² × P)
*-------------------------------------------------------------------------------

* Calculate SRS sample size
local n_srs_raw = (`z_value'^2 * `prevalence' * `Q') / (`e_absolute'^2)
local n_srs = ceil(`n_srs_raw')

display as text "SRS Sample Size Calculation:"
display as text "  Formula: n = z² × P × (1-P) / e²"
display as text "  where e = CV × P (absolute error from relative precision)"
display _newline
display as text "  Substituting values:"
display as text "    n = " %7.4f `z_value'^2 " × " %6.4f `prevalence' ///
    " × " %6.4f `Q' " / " %12.8f `e_absolute'^2
display as text "    n = " %12.2f `n_srs_raw'
display _newline
display as result "  REQUIRED SRS SAMPLE SIZE: " %12.0fc `n_srs' " households"
display _newline

* Finite population correction (FPC)
if `n_srs' < `N_total' {
    local n_srs_fpc = ceil(`n_srs' / (1 + (`n_srs' - 1) / `N_total'))
    display as text "  With finite population correction:"
    display as text "    n_fpc = n / (1 + (n-1)/N)"
    display as text "    n_fpc = " %12.0fc `n_srs_fpc' " households"
    display _newline
}

* Expected number of rare population units in SRS sample
local expected_rare_srs = `n_srs' * `prevalence'
display as text "  Expected high-income HH in sample: " %8.1f `expected_rare_srs'
display as text "  (Only ~" %5.0f `expected_rare_srs' ///
    " rare cases from " %8.0fc `n_srs' " interviews!)"
display _newline

*-------------------------------------------------------------------------------
* SECTION 5: SAMPLE SIZE CALCULATION - STRATIFIED SAMPLING
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 5: SAMPLE SIZE FOR STRATIFIED SAMPLING"
display as text "-" * 71
display _newline

display as text "Strategy: Create two strata based on proxy indicators of high income"
display as text "          and oversample the stratum likely to contain rare population."
display _newline

*-------------------------------------------------------------------------------
* Create stratification variable using geographic proxy
* Urban formal areas typically have higher income concentrations
*-------------------------------------------------------------------------------

* Check for geotype variable
cap confirm variable geotype
if _rc == 0 {
    display as text "Using 'geotype' for stratification (1=Urban, 2=Traditional, 3=Farms)"
    display _newline
    
    * Analyze high-income prevalence by geotype
    display as text "High-Income Prevalence by Geographic Type:"
    tabulate geotype high_income, row
    
    * Create binary stratum: Urban (high prevalence) vs Non-Urban (low prevalence)
    gen stratum = cond(geotype == 1, 1, 2)
    label variable stratum "Stratification for rare population"
    label define stratum_lbl 1 "Urban" 2 "Non-Urban"
    label values stratum stratum_lbl
}
else {
    * Use province as proxy (Gauteng and Western Cape typically higher income)
    display as text "Using province for stratification (economic hubs vs other)"
    display _newline
    
    gen stratum = cond(prov == 1 | prov == 7, 1, 2)
    label variable stratum "Stratification for rare population"
    label define stratum_lbl 1 "High-Income Province" 2 "Other Province"
    label values stratum stratum_lbl
}

* Calculate stratum-specific statistics
display _newline
display as text "Stratum Statistics:"
display as text "-" * 60

* Stratum 1 (high prevalence expected)
summarize high_income if stratum == 1, meanonly
local N_1 = r(N)
local P_1 = r(mean)
local Q_1 = 1 - `P_1'
local Var_1 = `P_1' * `Q_1'
local S_1 = sqrt(`Var_1')
count if stratum == 1 & high_income == 1
local N_rare_1 = r(N)

* Stratum 2 (low prevalence expected)
summarize high_income if stratum == 2, meanonly
local N_2 = r(N)
local P_2 = r(mean)
local Q_2 = 1 - `P_2'
local Var_2 = `P_2' * `Q_2'
local S_2 = sqrt(`Var_2')
count if stratum == 2 & high_income == 1
local N_rare_2 = r(N)

* Calculate weights
local W_1 = `N_1' / `N_total'
local W_2 = `N_2' / `N_total'

* Display stratum statistics
display _newline
display as text "Stratum 1 (High-prevalence - Urban/Economic Hub):"
display as text "  Population (N₁):   " %12.0fc `N_1'
display as text "  Rare cases:        " %12.0fc `N_rare_1'
display as text "  Weight (W₁):       " %12.4f `W_1'
display as text "  Prevalence (P₁):   " %12.4f `P_1'
display as text "  Std Dev (S₁):      " %12.4f `S_1'
display _newline

display as text "Stratum 2 (Low-prevalence - Non-Urban/Other):"
display as text "  Population (N₂):   " %12.0fc `N_2'
display as text "  Rare cases:        " %12.0fc `N_rare_2'
display as text "  Weight (W₂):       " %12.4f `W_2'
display as text "  Prevalence (P₂):   " %12.4f `P_2'
display as text "  Std Dev (S₂):      " %12.4f `S_2'
display _newline

* Verify overall prevalence
local check_prevalence = `W_1' * `P_1' + `W_2' * `P_2'
display as text "Verification - Overall prevalence from strata: " %9.4f `check_prevalence'
display as text "                         Direct calculation:   " %9.4f `prevalence'
display _newline

*-------------------------------------------------------------------------------
* Option A: Proportional Allocation
*-------------------------------------------------------------------------------

display as text "OPTION A: PROPORTIONAL ALLOCATION"
display as text "-" * 40
display _newline

* For proportional allocation:
* Var(p_st) = Σ(W_h × P_h × (1-P_h)) / n
* Required n = z² × Σ(W_h × P_h × Q_h) / (CV² × P²)

local var_component_prop = `W_1' * `P_1' * `Q_1' + `W_2' * `P_2' * `Q_2'
local n_prop_raw = (`z_value'^2 * `var_component_prop') / (`relative_precision'^2 * `prevalence'^2)
local n_prop = ceil(`n_prop_raw')

display as text "Formula: n = z² × Σ(Wₕ × Pₕ × (1-Pₕ)) / (CV² × P²)"
display _newline
display as text "Variance component Σ(Wₕ × Pₕ × Qₕ): " %12.6f `var_component_prop'
display as result "Required total sample size: " %12.0fc `n_prop'
display _newline

* Allocation to strata
local n_1_prop = ceil(`n_prop' * `W_1')
local n_2_prop = ceil(`n_prop' * `W_2')

display as text "Allocation by stratum:"
display as text "  Stratum 1 (Urban): " %12.0fc `n_1_prop' " households"
display as text "  Stratum 2 (Other): " %12.0fc `n_2_prop' " households"
display _newline

* Expected rare cases
local expected_rare_prop = `n_1_prop' * `P_1' + `n_2_prop' * `P_2'
display as text "Expected high-income HH in sample: " %8.1f `expected_rare_prop'
display _newline

*-------------------------------------------------------------------------------
* Option B: Optimal (Neyman) Allocation
*-------------------------------------------------------------------------------

display as text "OPTION B: NEYMAN ALLOCATION"
display as text "-" * 40
display _newline

* Neyman allocation: n_h ∝ N_h × S_h
* Var(p_st) ≈ (Σ(W_h × S_h))² / n

local var_component_neyman = (`W_1' * `S_1' + `W_2' * `S_2')^2
local n_neyman_raw = (`z_value'^2 * `var_component_neyman') / (`relative_precision'^2 * `prevalence'^2)
local n_neyman = ceil(`n_neyman_raw')

display as text "Formula: n = z² × (Σ(Wₕ × Sₕ))² / (CV² × P²)"
display as text "         where Sₕ = √(Pₕ × (1-Pₕ))"
display _newline
display as text "Variance component (Σ(Wₕ × Sₕ))²: " %12.6f `var_component_neyman'
display as result "Required total sample size: " %12.0fc `n_neyman'
display _newline

* Neyman weights
local N_S_1 = `N_1' * `S_1'
local N_S_2 = `N_2' * `S_2'
local neyman_total = `N_S_1' + `N_S_2'
local neyman_w1 = `N_S_1' / `neyman_total'
local neyman_w2 = `N_S_2' / `neyman_total'

local n_1_neyman = ceil(`n_neyman' * `neyman_w1')
local n_2_neyman = ceil(`n_neyman' * `neyman_w2')

display as text "Allocation by stratum:"
display as text "  Stratum 1 (Urban): " %12.0fc `n_1_neyman' " households"
display as text "  Stratum 2 (Other): " %12.0fc `n_2_neyman' " households"
display _newline

* Expected rare cases
local expected_rare_neyman = `n_1_neyman' * `P_1' + `n_2_neyman' * `P_2'
display as text "Expected high-income HH in sample: " %8.1f `expected_rare_neyman'
display _newline

*-------------------------------------------------------------------------------
* Option C: Oversampling the Rare Stratum
*-------------------------------------------------------------------------------

display as text "OPTION C: OVERSAMPLING HIGH-PREVALENCE STRATUM"
display as text "-" * 50
display _newline

display as text "Strategy: Allocate 50% of sample to high-prevalence stratum"
display as text "          (vs. natural proportion of " %5.1f `W_1' * 100 "%)"
display _newline

* Set oversampling rate
local oversample_rate = 0.50  // 50% of sample to high-prevalence stratum

* Target: At least 100 rare cases for stable subgroup analysis
local target_rare_cases = 100

display as text "Target minimum rare cases: " `target_rare_cases'
display _newline

* Calculate required sample
* Expected rare: n × (oversample_rate × P_1 + (1 - oversample_rate) × P_2)
local expected_rare_rate = `oversample_rate' * `P_1' + (1 - `oversample_rate') * `P_2'
local n_oversample = ceil(`target_rare_cases' / `expected_rare_rate')

display as text "Expected rare case rate with oversampling: " %9.4f `expected_rare_rate'
display as result "Required total sample for " `target_rare_cases' " rare cases: " ///
    %12.0fc `n_oversample'
display _newline

* Allocation
local n_1_over = ceil(`n_oversample' * `oversample_rate')
local n_2_over = ceil(`n_oversample' * (1 - `oversample_rate'))

display as text "Allocation by stratum:"
display as text "  Stratum 1 (Urban): " %12.0fc `n_1_over' " households (" ///
    %5.1f `oversample_rate' * 100 "%)"
display as text "  Stratum 2 (Other): " %12.0fc `n_2_over' " households (" ///
    %5.1f (1 - `oversample_rate') * 100 "%)"
display _newline

local expected_rare_over = `n_1_over' * `P_1' + `n_2_over' * `P_2'
display as text "Expected high-income HH in sample: " %8.1f `expected_rare_over'
display _newline

* Calculate achieved precision
local var_oversample = (`W_1'^2 * `P_1' * `Q_1' / `n_1_over') + ///
                       (`W_2'^2 * `P_2' * `Q_2' / `n_2_over')
local se_oversample = sqrt(`var_oversample')
local cv_oversample = `se_oversample' / `prevalence'

display as text "Achieved precision with oversampling:"
display as text "  Standard Error: " %12.6f `se_oversample'
display as text "  CV: " %8.2f `cv_oversample' * 100 "%"
display as text "  95% CI width: ±" %9.4f `z_value' * `se_oversample'
display _newline

*-------------------------------------------------------------------------------
* SECTION 6: COMPARISON SUMMARY
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 6: COMPARISON OF APPROACHES"
display as text "-" * 71
display _newline

display as text "Sample Size Comparison:"
display as text "=" * 70
display as text %30s "Method" %15s "Sample Size" %15s "Exp. Rare" %10s "Efficiency"
display as text "-" * 70
display as text %30s "Simple Random Sampling" %15.0fc `n_srs' ///
    %15.1f `expected_rare_srs' %10.2f 1.00
display as text %30s "Stratified - Proportional" %15.0fc `n_prop' ///
    %15.1f `expected_rare_prop' %10.2f `n_srs' / `n_prop'
display as text %30s "Stratified - Neyman" %15.0fc `n_neyman' ///
    %15.1f `expected_rare_neyman' %10.2f `n_srs' / `n_neyman'
display as text %30s "Stratified - Oversampling" %15.0fc `n_oversample' ///
    %15.1f `expected_rare_over' %10s "Target"
display as text "=" * 70
display _newline

*-------------------------------------------------------------------------------
* SECTION 7: KEY INSIGHTS AND RECOMMENDATIONS
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 7: KEY INSIGHTS"
display as text "-" * 71
display _newline

display as text "FINDINGS:"
display _newline

display as text "1. PROBLEM WITH SRS FOR RARE POPULATIONS:"
display as text "   - Required sample size: " %10.0fc `n_srs' " households"
display as text "   - Expected rare cases: only " %5.0f `expected_rare_srs'
local interviews_per_rare = `n_srs' / `expected_rare_srs'
display as text "   - Extremely inefficient: interviewing " %5.0f `interviews_per_rare' ///
    " HH for each rare case"
display _newline

display as text "2. STRATIFICATION BENEFIT:"
local prop_savings = (1 - `n_prop'/`n_srs') * 100
local neyman_savings = (1 - `n_neyman'/`n_srs') * 100
display as text "   - Proportional allocation saves " %5.1f `prop_savings' "% of sample"
display as text "   - Neyman allocation saves " %5.1f `neyman_savings' "% of sample"
display as text "   - Both approaches maintain target precision"
display _newline

display as text "3. OVERSAMPLING FOR SUBGROUP ANALYSIS:"
display as text "   - If analyzing the rare group itself (not just prevalence)"
display as text "   - Need minimum ~100 cases for stable estimates"
display as text "   - Oversampling achieves this with " %8.0fc `n_oversample' " HH"
display as text "   - Must apply weighting adjustments in analysis"
display _newline

display as text "RECOMMENDATIONS:"
display _newline
display as text "• For prevalence estimation: Use Neyman allocation"
display as text "• For detailed rare group analysis: Use oversampling"
display as text "• Always document stratification and weighting in methodology"
display as text "• Consider screening questions if rare population identifiable"
display _newline

*-------------------------------------------------------------------------------
* SECTION 8: EXPORT RESULTS
*-------------------------------------------------------------------------------

display as text "-" * 71
display as result "SECTION 8: EXPORT RESULTS"
display as text "-" * 71
display _newline

* Save stratum statistics
preserve
clear
set obs 2
gen stratum = _n
gen stratum_name = cond(stratum == 1, "Urban/High-Income Province", "Non-Urban/Other")
gen N_h = cond(stratum == 1, `N_1', `N_2')
gen N_rare_h = cond(stratum == 1, `N_rare_1', `N_rare_2')
gen W_h = cond(stratum == 1, `W_1', `W_2')
gen P_h = cond(stratum == 1, `P_1', `P_2')
gen S_h = cond(stratum == 1, `S_1', `S_2')
gen n_proportional = cond(stratum == 1, `n_1_prop', `n_2_prop')
gen n_neyman = cond(stratum == 1, `n_1_neyman', `n_2_neyman')
gen n_oversample = cond(stratum == 1, `n_1_over', `n_2_over')

export delimited using "lab1_6_outputs/stratum_statistics.csv", replace
display as text "Exported: stratum_statistics.csv"
restore

* Save comparison results
preserve
clear
set obs 4
gen method = ""
replace method = "Simple Random Sampling" in 1
replace method = "Stratified - Proportional" in 2
replace method = "Stratified - Neyman" in 3
replace method = "Stratified - Oversampling" in 4

gen sample_size = .
replace sample_size = `n_srs' in 1
replace sample_size = `n_prop' in 2
replace sample_size = `n_neyman' in 3
replace sample_size = `n_oversample' in 4

gen expected_rare = .
replace expected_rare = `expected_rare_srs' in 1
replace expected_rare = `expected_rare_prop' in 2
replace expected_rare = `expected_rare_neyman' in 3
replace expected_rare = `expected_rare_over' in 4

gen efficiency = .
replace efficiency = 1 in 1
replace efficiency = `n_srs' / `n_prop' in 2
replace efficiency = `n_srs' / `n_neyman' in 3
replace efficiency = . in 4

export delimited using "lab1_6_outputs/sample_size_comparison.csv", replace
display as text "Exported: sample_size_comparison.csv"
restore

* Save design parameters
preserve
clear
set obs 7
gen parameter = ""
gen value = ""
replace parameter = "Confidence Level" in 1
replace value = "`=round(`confidence_level'*100)'%" in 1
replace parameter = "Z-value" in 2
replace value = "`=round(`z_value', 0.0001)'" in 2
replace parameter = "Relative Precision (CV)" in 3
replace value = "`=round(`relative_precision'*100)'%" in 3
replace parameter = "Population Size" in 4
replace value = "`N_total'" in 4
replace parameter = "Rare Population Size" in 5
replace value = "`N_rare'" in 5
replace parameter = "Prevalence" in 6
replace value = "`=round(`prevalence', 0.0001)'" in 6
replace parameter = "Income Threshold (95th pct)" in 7
replace value = "R`=round(`income_threshold')'" in 7

export delimited using "lab1_6_outputs/design_parameters.csv", replace
display as text "Exported: design_parameters.csv"
restore

display _newline
display as text "Results exported to 'lab1_6_outputs/' directory:"
display as text "  - stratum_statistics.csv"
display as text "  - sample_size_comparison.csv"
display as text "  - design_parameters.csv"
display _newline

*-------------------------------------------------------------------------------
* VISUALIZATION (Optional - requires user to run separately)
*-------------------------------------------------------------------------------

* Create comparison bar chart
graph bar (asis) n_srs = `n_srs' n_prop = `n_prop' n_neyman = `n_neyman' ///
    n_over = `n_oversample' if _n == 1, ///
    bar(1, color(maroon)) bar(2, color(navy)) ///
    bar(3, color(forest_green)) bar(4, color(gold)) ///
    legend(order(1 "SRS" 2 "Proportional" 3 "Neyman" 4 "Oversampling") ///
           rows(1) position(6)) ///
    title("Sample Size Comparison for Rare Population Estimation") ///
    subtitle("Target: 10% CV for high-income households (`=round(`prevalence'*100, 0.1)'% prevalence)") ///
    ytitle("Required Sample Size") ///
    ylabel(, format(%12.0fc)) ///
    note("Source: GHS 2024 simulation")

* Note: The above graph command may need adjustment based on data structure
* Alternative approach using stored values:

preserve
clear
input str30 method sample_size
"SRS" `n_srs'
"Proportional" `n_prop'
"Neyman" `n_neyman'
"Oversampling" `n_oversample'
end

graph bar sample_size, over(method, sort(sample_size) descending) ///
    bar(1, color(navy)) ///
    title("Sample Size Comparison for Rare Population") ///
    subtitle("10% CV target for top 5% income households") ///
    ytitle("Required Sample Size") ///
    ylabel(, format(%9.0fc)) ///
    blabel(bar, format(%9.0fc))
    
graph export "lab1_6_outputs/sample_size_comparison.png", replace width(800)
display as text "Visualization saved: sample_size_comparison.png"
restore

display _newline
display as text "=" * 71
display as result "LAB 1.6 COMPLETE"
display as text "=" * 71

log close

*===============================================================================
* END OF SCRIPT
*===============================================================================
