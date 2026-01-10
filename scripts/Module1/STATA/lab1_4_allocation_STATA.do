*===============================================================================
* LAB 1.4: SAMPLE ALLOCATION METHODS
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 1, Module 1.4: Optimal Allocation Strategies
*===============================================================================
*
* LEARNING OBJECTIVES:
*   - Calculate sample sizes using Proportional Allocation
*   - Apply Neyman (Optimal) Allocation based on stratum variance
*   - Implement Square Root (Power) Allocation for domain estimation
*   - Compare allocation strategies and their trade-offs
*
* DATA SOURCE: Zambara General Household Survey 2024
* FILE: ghs-2024-hhold-v1.dta
*
* ZAMBARA REGION MAPPING (Province codes):
*   1 = Western Drylands     6 = Mining Belt
*   2 = Coastal Plains       7 = Capital Region
*   3 = Northern Bushveld    8 = Eastern Forests
*   4 = Central Plateau      9 = Southern Cape
*   5 = Eastern Highlands
*
* AUTHOR: SADC Sampling Workshop
* DATE: March 2026
*===============================================================================

clear all
set more off
cap log close

* Start log file
log using "lab1_4_allocation.log", replace

display as text ""
display as text "=" * 70
display as result "LAB 1.4: SAMPLE ALLOCATION METHODS"
display as text "=" * 70
display as text ""

*-------------------------------------------------------------------------------
* 1. LOAD AND PREPARE DATA
*-------------------------------------------------------------------------------

display as text "Loading Zambara household survey data..."

use "ghs-2024-hhold-v1.dta", clear

display as text "  Raw observations: " _N

* Examine province variable
tab prov, missing

* Data cleaning
* - Remove missing weights
* - Remove implausible income values (coded as 9999999)
* - Keep only valid records

drop if missing(house_wgt) | house_wgt <= 0
drop if missing(totmhinc) | totmhinc >= 9999999 | totmhinc < 0
drop if missing(prov)

display as text ""
display as text "After cleaning:"
display as text "  Valid observations: " _N

* Count unique provinces
qui levelsof prov, local(provinces)
local n_prov : word count `provinces'
display as text "  Provinces represented: `n_prov'"

*-------------------------------------------------------------------------------
* 2. CREATE ZAMBARA REGION LABELS
*-------------------------------------------------------------------------------

* Create region name variable
gen str25 region_name = ""
replace region_name = "Western Drylands"  if prov == 1
replace region_name = "Coastal Plains"    if prov == 2
replace region_name = "Northern Bushveld" if prov == 3
replace region_name = "Central Plateau"   if prov == 4
replace region_name = "Eastern Highlands" if prov == 5
replace region_name = "Mining Belt"       if prov == 6
replace region_name = "Capital Region"    if prov == 7
replace region_name = "Eastern Forests"   if prov == 8
replace region_name = "Southern Cape"     if prov == 9

*-------------------------------------------------------------------------------
* 3. CALCULATE STRATUM-LEVEL STATISTICS
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 1: Calculate Population and Variance by Region"
display as text "-" * 70
display as text ""

* Preserve data for later use
preserve

* Calculate statistics by province
collapse ///
    (count) n_sample = totmhinc ///
    (sum) N_h = house_wgt ///
    (mean) mean_income = totmhinc ///
    (sd) sd_income = totmhinc ///
    , by(prov region_name)

* Calculate variance
gen var_income = sd_income^2

* Sort by province
sort prov

* Calculate total population
egen N_total = total(N_h)

* Calculate population proportions (weights)
gen W_h = N_h / N_total

* Calculate components for Neyman allocation
gen N_h_S_h = N_h * sd_income

* Calculate square root for power allocation
gen sqrt_N_h = sqrt(N_h)

* Display population statistics
display as text "Population Statistics by Zambara Region:"
display as text ""
list prov region_name n_sample N_h W_h mean_income sd_income, ///
    table sep(0) noobs abbreviate(20)

* Save totals for later use
qui sum N_total
local N_total = r(mean)
display as text ""
display as text "Total estimated population: " %15.0fc `N_total'

*-------------------------------------------------------------------------------
* 4. CALCULATE ALLOCATIONS
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 2: Apply Allocation Methods"
display as text "-" * 70
display as text ""

* Define total sample size to allocate
local n_total = 20000

display as text "Target total sample size: n = " %10.0fc `n_total'
display as text ""

*--- 4.1 PROPORTIONAL ALLOCATION ---
* Formula: n_h = n * (N_h / N)

gen n_proportional = round(`n_total' * W_h)

*--- 4.2 NEYMAN (OPTIMAL) ALLOCATION ---
* Formula: n_h = n * (N_h * S_h) / sum(N_h * S_h)

egen sum_N_h_S_h = total(N_h_S_h)
gen n_neyman = round(`n_total' * (N_h_S_h / sum_N_h_S_h))

*--- 4.3 SQUARE ROOT (POWER) ALLOCATION ---
* Formula: n_h = n * (N_h^0.5) / sum(N_h^0.5)

egen sum_sqrt_N_h = total(sqrt_N_h)
gen n_power = round(`n_total' * (sqrt_N_h / sum_sqrt_N_h))

*--- 4.4 EQUAL ALLOCATION (for reference) ---
local n_regions = _N
gen n_equal = round(`n_total' / `n_regions')

*-------------------------------------------------------------------------------
* 5. DISPLAY ALLOCATION RESULTS
*-------------------------------------------------------------------------------

display as text "ALLOCATION RESULTS BY METHOD:"
display as text "=" * 70
display as text ""

* Sort by proportional allocation (descending)
gsort -n_proportional

list region_name N_h n_proportional n_neyman n_power n_equal, ///
    table sep(0) noobs abbreviate(20)

* Verify totals
display as text ""
display as text "Allocation Totals:"
qui sum n_proportional
display as text "  Proportional: " %10.0f r(sum)
qui sum n_neyman
display as text "  Neyman:       " %10.0f r(sum)
qui sum n_power
display as text "  Power (0.5):  " %10.0f r(sum)
qui sum n_equal
display as text "  Equal:        " %10.0f r(sum)

*-------------------------------------------------------------------------------
* 6. CALCULATE EXPECTED PRECISION (RSE) FOR EACH METHOD
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 3: Compare Expected Precision (RSE)"
display as text "-" * 70
display as text ""

* Calculate RSE for each method
* RSE = (CV / sqrt(n)) * 100 where CV = S_h / mean_h

gen cv_income = sd_income / mean_income

gen rse_proportional = (cv_income / sqrt(n_proportional)) * 100
gen rse_neyman = (cv_income / sqrt(n_neyman)) * 100
gen rse_power = (cv_income / sqrt(n_power)) * 100
gen rse_equal = (cv_income / sqrt(n_equal)) * 100

* Display RSE comparison
display as text "EXPECTED RELATIVE STANDARD ERROR (RSE %) BY METHOD:"
display as text ""

gsort -rse_proportional
list region_name n_proportional rse_proportional ///
     n_neyman rse_neyman n_power rse_power, ///
    table sep(0) noobs abbreviate(15)

*-------------------------------------------------------------------------------
* 7. IDENTIFY PROBLEMATIC ALLOCATIONS
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 4: Identify Domain Precision Issues"
display as text "-" * 70
display as text ""

* Flag regions with RSE > 15%
local rse_threshold = 15

gen problem_region = (rse_proportional > `rse_threshold')

qui count if problem_region == 1
local n_problems = r(N)

if `n_problems' > 0 {
    display as text "Regions with RSE > 15% under PROPORTIONAL allocation:"
    display as text ""
    list region_name n_proportional rse_proportional n_power rse_power ///
        if problem_region == 1, table sep(0) noobs
    
    display as text ""
    display as text "RECOMMENDATION: Power allocation improves precision for small domains"
    display as text "while maintaining reasonable national-level efficiency."
}
else {
    display as text "All regions have acceptable precision (RSE <= 15%) under all methods."
}

*-------------------------------------------------------------------------------
* 8. CALCULATE NATIONAL-LEVEL PRECISION
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 5: National-Level Efficiency Comparison"
display as text "-" * 70
display as text ""

* For stratified sampling, Var(y_bar_st) = sum(W_h^2 * S_h^2 / n_h)

gen var_comp_prop = (W_h^2 * var_income) / n_proportional
gen var_comp_neyman = (W_h^2 * var_income) / n_neyman
gen var_comp_power = (W_h^2 * var_income) / n_power
gen var_comp_equal = (W_h^2 * var_income) / n_equal

* Sum to get national variance
egen var_nat_prop = total(var_comp_prop)
egen var_nat_neyman = total(var_comp_neyman)
egen var_nat_power = total(var_comp_power)
egen var_nat_equal = total(var_comp_equal)

* Calculate overall mean
gen mean_comp = W_h * mean_income
egen overall_mean = total(mean_comp)

* Calculate SE and RSE
gen se_nat_prop = sqrt(var_nat_prop)
gen se_nat_neyman = sqrt(var_nat_neyman)
gen se_nat_power = sqrt(var_nat_power)
gen se_nat_equal = sqrt(var_nat_equal)

gen rse_nat_prop = (se_nat_prop / overall_mean) * 100
gen rse_nat_neyman = (se_nat_neyman / overall_mean) * 100
gen rse_nat_power = (se_nat_power / overall_mean) * 100
gen rse_nat_equal = (se_nat_equal / overall_mean) * 100

* Display national precision
display as text "National-Level Precision Comparison:"
display as text ""
display as text "  Method          Variance        SE      RSE(%)"
display as text "  ------------------------------------------------"

qui sum var_nat_prop
local v1 = r(mean)
qui sum se_nat_prop
local s1 = r(mean)
qui sum rse_nat_prop
local r1 = r(mean)
display as text "  Proportional  " %12.0f `v1' "   " %7.1f `s1' "   " %5.2f `r1'

qui sum var_nat_neyman
local v2 = r(mean)
qui sum se_nat_neyman
local s2 = r(mean)
qui sum rse_nat_neyman
local r2 = r(mean)
display as text "  Neyman        " %12.0f `v2' "   " %7.1f `s2' "   " %5.2f `r2'

qui sum var_nat_power
local v3 = r(mean)
qui sum se_nat_power
local s3 = r(mean)
qui sum rse_nat_power
local r3 = r(mean)
display as text "  Power (0.5)   " %12.0f `v3' "   " %7.1f `s3' "   " %5.2f `r3'

qui sum var_nat_equal
local v4 = r(mean)
qui sum se_nat_equal
local s4 = r(mean)
qui sum rse_nat_equal
local r4 = r(mean)
display as text "  Equal         " %12.0f `v4' "   " %7.1f `s4' "   " %5.2f `r4'

* Efficiency relative to Neyman
display as text ""
display as text "Efficiency relative to Neyman (optimal):"
display as text "  Proportional: " %5.1f (`v2'/`v1' * 100) "%"
display as text "  Power (0.5):  " %5.1f (`v2'/`v3' * 100) "%"
display as text "  Equal:        " %5.1f (`v2'/`v4' * 100) "%"

*-------------------------------------------------------------------------------
* 9. APPLY MINIMUM SAMPLE SIZE CONSTRAINT
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 6: Apply Minimum Sample Size Constraint"
display as text "-" * 70
display as text ""

local min_n = 800

display as text "Minimum sample size per region: `min_n'"
display as text ""

* Create constrained power allocation
gen n_power_min = n_power

* Set minimum
replace n_power_min = `min_n' if n_power_min < `min_n'

* Calculate excess that needs to be redistributed
qui sum n_power_min
local sum_constrained = r(sum)
local excess = `sum_constrained' - `n_total'

* Reduce from regions above minimum (proportionally to excess)
if `excess' > 0 {
    gen reduction_pool = n_power_min - `min_n' if n_power_min > `min_n'
    replace reduction_pool = 0 if missing(reduction_pool)
    
    egen total_pool = total(reduction_pool)
    gen reduction = round(reduction_pool * (`excess' / total_pool)) if total_pool > 0
    replace reduction = 0 if missing(reduction)
    
    replace n_power_min = n_power_min - reduction
    
    drop reduction_pool total_pool reduction
}

* Calculate change
gen change = n_power_min - n_power

* Display comparison
sort n_power
display as text "Power Allocation with Minimum Constraint (n_min = `min_n'):"
display as text ""
list region_name n_power n_power_min change, table sep(0) noobs abbreviate(20)

qui sum n_power_min
display as text ""
display as text "Total after constraint: " %10.0f r(sum)

*-------------------------------------------------------------------------------
* 10. EXPORT RESULTS
*-------------------------------------------------------------------------------

display as text ""
display as text "-" * 70
display as text "STEP 7: Export Results"
display as text "-" * 70
display as text ""

* Sort by population
gsort -N_h

* Keep key variables
keep prov region_name N_h W_h sd_income mean_income ///
     n_proportional n_neyman n_power n_power_min ///
     rse_proportional rse_neyman rse_power

* Export to CSV
export delimited using "lab1_4_allocation_results.csv", replace

display as text "Results exported to: lab1_4_allocation_results.csv"

*-------------------------------------------------------------------------------
* 11. SUMMARY AND KEY FINDINGS
*-------------------------------------------------------------------------------

display as text ""
display as text "=" * 70
display as text "SUMMARY: SAMPLE ALLOCATION METHODS"
display as text "=" * 70
display as text ""

display as text "KEY FORMULAS:"
display as text "-" * 40
display as text "Proportional:  n_h = n × (N_h / N)"
display as text "Neyman:        n_h = n × (N_h × S_h) / Σ(N_h × S_h)"
display as text "Power (α):     n_h = n × (N_h^α) / Σ(N_h^α)"
display as text "               where α = 0.5 for square root allocation"
display as text ""

display as text "ALLOCATION COMPARISON (n = 20,000):"
display as text "-" * 40

qui sum n_proportional
local prop_min = r(min)
local prop_max = r(max)
qui sum n_neyman
local neyman_min = r(min)
local neyman_max = r(max)
qui sum n_power
local power_min = r(min)
local power_max = r(max)

display as text "Proportional: Range " %5.0f `prop_min' " - " %5.0f `prop_max'
display as text "Neyman:       Range " %5.0f `neyman_min' " - " %5.0f `neyman_max'
display as text "Power (0.5):  Range " %5.0f `power_min' " - " %5.0f `power_max'

display as text ""
display as text "RECOMMENDATIONS FOR ZAMBARA:"
display as text "-" * 40
display as text "1. Proportional allocation optimizes national precision but"
display as text "   leaves small regions (Western Drylands) with RSE > 25%."
display as text ""
display as text "2. Neyman allocation is theoretically optimal for national"
display as text "   estimates but can worsen small-domain precision."
display as text ""
display as text "3. Power allocation (α=0.5) with minimum constraint (n≥800)"
display as text "   provides balanced precision across all domains."
display as text ""
display as text "4. For Lindiwe's redesign: RECOMMEND Power allocation with"
display as text "   minimum floor to ensure RSE < 15% for all regions."

display as text ""
display as text "=" * 70
display as text "END OF LAB 1.4"
display as text "=" * 70

restore

log close

*-------------------------------------------------------------------------------
* END OF SCRIPT
*-------------------------------------------------------------------------------
