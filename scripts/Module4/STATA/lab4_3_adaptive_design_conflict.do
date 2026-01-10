*===============================================================================
* LAB 4.3: ADAPTIVE DESIGN IN CONFLICT ZONES
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 4, Module 4.2: Error Mitigation and Quality Assurance
*===============================================================================
*
* OBJECTIVE: Compare three strategies for handling inaccessible PSUs
*   Strategy A: Drop inaccessible PSUs (complete case analysis)
*   Strategy B: Re-weight remaining PSUs within affected strata
*   Strategy C: Substitute with "safe" neighbor PSUs
*
* DATA: GHS 2024 Household File
* CONFLICT ZONE: Eastern Highlands (KwaZulu-Natal, Province 5)
*                50% of PSUs made inaccessible
*
* ZAMBARA NARRATIVE: Eastern Forests region faces security crisis
*
*===============================================================================

clear all
set more off
cap log close

* Start log file
log using "lab4_3_adaptive_design.log", replace

display _n
display "================================================================"
display "  LAB 4.3: ADAPTIVE DESIGN IN CONFLICT ZONES"
display "  SADC Advanced Sampling Workshop - Day 4"
display "================================================================"
display _n

*-------------------------------------------------------------------------------
* 1. LOAD AND PREPARE DATA
*-------------------------------------------------------------------------------

display "SECTION 1: Loading and Preparing Data"
display "-------------------------------------" _n

* Load GHS 2024 household data
* NOTE: Adjust path as needed for your system
use "ghs-2024-hhold-v1.dta", clear

display "Dataset loaded: " _N " households" _n

* Key variables:
* - uqnr: Unique household identifier
* - psu: Primary Sampling Unit
* - stratum: Sampling stratum (first digit = province)
* - prov: Province (1-9)
* - geotype: Geography type (1=Urban, 2=Traditional, 3=Farms)
* - house_wgt: Household weight
* - totmhinc: Total monthly household income
* - hholdsz: Household size

* Create province variable from stratum if needed
cap confirm variable prov
if _rc != 0 {
    gen prov = floor(stratum / 10000)
}

* Province labels
label define prov_lbl 1 "Western Cape" 2 "Eastern Cape" 3 "Northern Cape" ///
    4 "Free State" 5 "KwaZulu-Natal" 6 "North West" 7 "Gauteng" ///
    8 "Mpumalanga" 9 "Limpopo"
label values prov prov_lbl

* Check province distribution
display "Province Distribution:"
tab prov

* Count PSUs by province
preserve
collapse (count) n_hh = house_wgt, by(prov psu)
collapse (count) n_psu = psu (sum) n_hh, by(prov)
display _n "PSU Counts by Province:"
list prov n_psu n_hh, clean noobs
restore

*-------------------------------------------------------------------------------
* 2. SIMULATE CONFLICT ZONE: MAKE 50% OF KZN PSUs INACCESSIBLE
*-------------------------------------------------------------------------------

display _n "SECTION 2: Simulating Conflict Zone"
display "------------------------------------" _n

* Define conflict zone
local CONFLICT_PROV = 5
display "Conflict Zone: KwaZulu-Natal (Eastern Highlands)" _n

* Set seed for reproducibility
set seed 20260305

* Get unique PSUs in KZN
preserve
keep if prov == `CONFLICT_PROV'
collapse (count) n = house_wgt, by(psu)
gen random = runiform()
sort random
local n_kzn_psu = _N
local n_inaccessible = round(`n_kzn_psu' * 0.50)

display "Total PSUs in conflict zone: `n_kzn_psu'"
display "PSUs made inaccessible: `n_inaccessible' (50%)"

* Mark top 50% as inaccessible
gen inaccessible = _n <= `n_inaccessible'

* Save inaccessible PSU list
keep psu inaccessible
tempfile inaccessible_psus
save `inaccessible_psus'
restore

* Merge inaccessibility flag
merge m:1 psu using `inaccessible_psus', nogen
replace inaccessible = 0 if inaccessible == .

* Create accessibility flag (1 = accessible)
gen accessible = (inaccessible == 0)
label var accessible "PSU is accessible"

* Summary of accessibility
display _n "Accessibility Summary by Province:"
tab prov accessible, row

* Count affected households
count if accessible == 0
local n_hh_inaccessible = r(N)
local pct_inaccessible = round(100 * `n_hh_inaccessible' / _N, 0.1)
display _n "Households in inaccessible PSUs: `n_hh_inaccessible' (`pct_inaccessible'%)" _n

* Save full dataset with accessibility flag
tempfile full_data
save `full_data'

*-------------------------------------------------------------------------------
* 3. CALCULATE "TRUE" POPULATION PARAMETERS (Full Sample Benchmark)
*-------------------------------------------------------------------------------

display _n "SECTION 3: Establishing True Population Parameters"
display "--------------------------------------------------" _n

* Define survey design for FULL sample
* singleunit(certainty): Handle singleton PSUs (strata with only 1 PSU)
* This occurs when dropping PSUs leaves some strata with only one PSU
svyset psu [pw=house_wgt], strata(stratum) singleunit(certainty)

* Calculate "true" estimates from full sample
* Mean household income (national)
svy: mean totmhinc
matrix TRUE_INCOME_NAT = e(b)
local true_income_nat = TRUE_INCOME_NAT[1,1]

* Mean household income (KZN only)
svy, subpop(if prov == `CONFLICT_PROV'): mean totmhinc
matrix TRUE_INCOME_KZN = e(b)
local true_income_kzn = TRUE_INCOME_KZN[1,1]

* Mean household size (national)
svy: mean hholdsz
matrix TRUE_HHSIZE = e(b)
local true_hhsize = TRUE_HHSIZE[1,1]

display "TRUE POPULATION PARAMETERS (Full Sample):"
display "-----------------------------------------"
display "Mean HH Income (National): R " %12.0fc `true_income_nat'
display "Mean HH Income (KZN):      R " %12.0fc `true_income_kzn'
display "Mean HH Size (National):   " %6.2f `true_hhsize'
display ""

*-------------------------------------------------------------------------------
* 4. STRATEGY A: DROP INACCESSIBLE PSUs
*-------------------------------------------------------------------------------

display _n "SECTION 4: STRATEGY A - Drop Inaccessible PSUs"
display "----------------------------------------------" _n

* Keep only accessible PSUs
use `full_data', clear
keep if accessible == 1

local n_stratA = _N
display "Strategy A Sample Size: `n_stratA' households"

* Check for singleton strata
preserve
collapse (count) n_psu = psu, by(stratum)
bysort stratum: gen singleton = (n_psu == 1)
qui count if singleton == 1
local n_singleton = r(N)
if `n_singleton' > 0 {
    display _n "WARNING: `n_singleton' strata have only 1 PSU after dropping."
    display "         Using singleunit(certainty) to handle."
}
restore

* Define survey design (original weights)
* singleunit(certainty): singleton PSUs contribute 0 variance
svyset psu [pw=house_wgt], strata(stratum) singleunit(certainty)

* Calculate estimates
svy: mean totmhinc
matrix EST_A_INCOME = e(b)
matrix SE_A_INCOME = e(V)
local est_A_income = EST_A_INCOME[1,1]
local se_A_income = sqrt(SE_A_INCOME[1,1])

svy, subpop(if prov == `CONFLICT_PROV'): mean totmhinc
matrix EST_A_INC_KZN = e(b)
local est_A_inc_kzn = EST_A_INC_KZN[1,1]

svy: mean hholdsz
matrix EST_A_HHSIZE = e(b)
local est_A_hhsize = EST_A_HHSIZE[1,1]

* Calculate bias
local bias_A_income = `est_A_income' - `true_income_nat'
local bias_A_inc_kzn = `est_A_inc_kzn' - `true_income_kzn'
local bias_A_hhsize = `est_A_hhsize' - `true_hhsize'
local relbias_A = 100 * `bias_A_income' / `true_income_nat'

display "STRATEGY A ESTIMATES:"
display "Mean HH Income (National): R " %12.0fc `est_A_income' " | Bias: R " %10.0fc `bias_A_income'
display "Mean HH Income (KZN):      R " %12.0fc `est_A_inc_kzn' " | Bias: R " %10.0fc `bias_A_inc_kzn'
display "Mean HH Size (National):   " %6.2f `est_A_hhsize' " | Bias: " %6.3f `bias_A_hhsize'
display _n "Relative Bias (Income): " %6.2f `relbias_A' "%" _n

*-------------------------------------------------------------------------------
* 5. STRATEGY B: RE-WEIGHT REMAINING PSUs IN AFFECTED STRATA
*-------------------------------------------------------------------------------

display _n "SECTION 5: STRATEGY B - Re-weight Remaining PSUs"
display "-------------------------------------------------" _n

* Load full data
use `full_data', clear

* Calculate weight adjustment factors by stratum
* For each stratum: factor = sum(weights_all) / sum(weights_accessible)
preserve
collapse (sum) wgt_total = house_wgt, by(stratum)
rename wgt_total wgt_stratum_total
tempfile wgt_total
save `wgt_total'
restore

preserve
keep if accessible == 1
collapse (sum) wgt_accessible = house_wgt, by(stratum)
rename wgt_accessible wgt_stratum_access
tempfile wgt_access
save `wgt_access'
restore

* Merge weight totals
preserve
use `wgt_total', clear
merge 1:1 stratum using `wgt_access', nogen
gen wgt_factor = wgt_stratum_total / wgt_stratum_access
gen has_inaccessible = (wgt_factor > 1.001)

display "Strata with Inaccessible PSUs:"
list stratum wgt_factor if has_inaccessible == 1, clean noobs
local n_affected_strata = r(N)

tempfile wgt_factors
save `wgt_factors'
restore

* Create Strategy B dataset with adjusted weights
use `full_data', clear
keep if accessible == 1
merge m:1 stratum using `wgt_factors', nogen keepusing(wgt_factor)
gen house_wgt_adj = house_wgt * wgt_factor
label var house_wgt_adj "Adjusted household weight"

local n_stratB = _N
display _n "Strategy B Sample Size: `n_stratB' households"

* Check weight sums
qui sum house_wgt
local orig_wgt_sum = r(sum)
qui sum house_wgt_adj
local adj_wgt_sum = r(sum)
display "Original weight sum (accessible): " %15.0fc `orig_wgt_sum'
display "Adjusted weight sum:              " %15.0fc `adj_wgt_sum'

* Define survey design with adjusted weights
* singleunit(certainty): Handle any singleton strata
svyset psu [pw=house_wgt_adj], strata(stratum) singleunit(certainty)

* Calculate estimates
svy: mean totmhinc
matrix EST_B_INCOME = e(b)
local est_B_income = EST_B_INCOME[1,1]

svy, subpop(if prov == `CONFLICT_PROV'): mean totmhinc
matrix EST_B_INC_KZN = e(b)
local est_B_inc_kzn = EST_B_INC_KZN[1,1]

svy: mean hholdsz
matrix EST_B_HHSIZE = e(b)
local est_B_hhsize = EST_B_HHSIZE[1,1]

* Calculate bias
local bias_B_income = `est_B_income' - `true_income_nat'
local bias_B_inc_kzn = `est_B_inc_kzn' - `true_income_kzn'
local bias_B_hhsize = `est_B_hhsize' - `true_hhsize'
local relbias_B = 100 * `bias_B_income' / `true_income_nat'

display _n "STRATEGY B ESTIMATES:"
display "Mean HH Income (National): R " %12.0fc `est_B_income' " | Bias: R " %10.0fc `bias_B_income'
display "Mean HH Income (KZN):      R " %12.0fc `est_B_inc_kzn' " | Bias: R " %10.0fc `bias_B_inc_kzn'
display "Mean HH Size (National):   " %6.2f `est_B_hhsize' " | Bias: " %6.3f `bias_B_hhsize'
display _n "Relative Bias (Income): " %6.2f `relbias_B' "%" _n

*-------------------------------------------------------------------------------
* 6. STRATEGY C: SUBSTITUTE WITH "SAFE" NEIGHBOR PSUs
*-------------------------------------------------------------------------------

display _n "SECTION 6: STRATEGY C - Substitute with Neighbor PSUs"
display "------------------------------------------------------" _n

* Load full data
use `full_data', clear

* Identify inaccessible PSUs and their strata
preserve
keep if accessible == 0
collapse (count) n_hh = house_wgt (mean) mean_hhsize = hholdsz, by(psu stratum)
local n_inac_psu = _N
display "Inaccessible PSUs to substitute: `n_inac_psu'" _n

* Save list of inaccessible PSUs
tempfile inac_list
save `inac_list'
restore

* Get list of accessible PSUs by stratum for substitution
preserve
keep if accessible == 1
collapse (count) n = house_wgt, by(psu stratum)
gen random = runiform()
sort stratum random
by stratum: gen rank = _n
tempfile accessible_psus
save `accessible_psus'
restore

* Create substitution assignments
* For each inaccessible PSU, assign a random accessible PSU from same stratum
preserve
use `inac_list', clear
rename psu inaccessible_psu
gen random = runiform()
sort stratum random
by stratum: gen inac_rank = _n

* Merge with accessible PSUs (match by stratum and rank, with wraparound)
merge m:1 stratum using `accessible_psus', keep(master match) nogen keepusing(psu)
rename psu substitute_psu

* If no direct match, use first accessible PSU in stratum
replace substitute_psu = . if substitute_psu == .
* Manual assignment for unmatched (simplified approach)
tempfile substitutions
save `substitutions'
restore

* Build substitution mapping more carefully
use `inac_list', clear
rename psu inaccessible_psu
levelsof stratum, local(strata)

* For simplicity, random assign within stratum
gen substitute_psu = .
tempfile sub_assign
save `sub_assign'

use `accessible_psus', clear
foreach s of local strata {
    qui levelsof psu if stratum == `s', local(avail_psus)
    local n_avail : word count `avail_psus'
    if `n_avail' > 0 {
        use `sub_assign', clear
        qui count if stratum == `s'
        local n_need = r(N)
        forvalues i = 1/`n_need' {
            local pick = mod(`i'-1, `n_avail') + 1
            local sub_psu : word `pick' of `avail_psus'
            qui replace substitute_psu = `sub_psu' if stratum == `s' & _n == `i'
        }
        save `sub_assign', replace
    }
}

use `sub_assign', clear
display "Substitution Assignments (first 10):"
list inaccessible_psu substitute_psu stratum in 1/10, clean noobs

tempfile substitutions
save `substitutions'

* Create Strategy C dataset
use `full_data', clear
keep if accessible == 1
gen is_substitute = 0
gen original_psu = psu
tempfile base_stratC
save `base_stratC'

* Add substitute records
use `substitutions', clear
levelsof inaccessible_psu, local(inac_psus)
levelsof substitute_psu, local(sub_psus)

local n_subs = _N
forvalues i = 1/`n_subs' {
    local inac_psu = inaccessible_psu[`i']
    local sub_psu = substitute_psu[`i']
    
    * Get data from substitute PSU
    use `full_data', clear
    keep if psu == `sub_psu'
    replace psu = `inac_psu'
    gen is_substitute = 1
    gen original_psu = `sub_psu'
    
    append using `base_stratC'
    save `base_stratC', replace
}

use `base_stratC', clear
local n_stratC = _N

display _n "Strategy C Sample Size: `n_stratC' households"
count if is_substitute == 0
display "Original accessible: " r(N)
count if is_substitute == 1
display "Substituted: " r(N)

* Compare inaccessible vs substitutes (bias flag)
display _n "SUBSTITUTION BIAS FLAG:"
display "Comparing inaccessible PSUs (true values) vs substitutes:"

* Stats for inaccessible (from full data)
use `full_data', clear
keep if accessible == 0
qui sum totmhinc
local mean_inc_inac = r(mean)
qui sum hholdsz
local mean_hhs_inac = r(mean)
local n_inac = r(N)

* Stats for substitutes
use `base_stratC', clear
keep if is_substitute == 1
qui sum totmhinc
local mean_inc_sub = r(mean)
qui sum hholdsz
local mean_hhs_sub = r(mean)
local n_sub = r(N)

display "                        Mean Income    Mean HH Size    N"
display "Inaccessible (Original): R " %10.0fc `mean_inc_inac' "    " %6.2f `mean_hhs_inac' "    " `n_inac'
display "Substitutes:             R " %10.0fc `mean_inc_sub' "    " %6.2f `mean_hhs_sub' "    " `n_sub'

* Define survey design for Strategy C
* singleunit(certainty): Handle any singleton strata
use `base_stratC', clear
svyset psu [pw=house_wgt], strata(stratum) singleunit(certainty)

* Calculate estimates
svy: mean totmhinc
matrix EST_C_INCOME = e(b)
local est_C_income = EST_C_INCOME[1,1]

svy, subpop(if prov == `CONFLICT_PROV'): mean totmhinc
matrix EST_C_INC_KZN = e(b)
local est_C_inc_kzn = EST_C_INC_KZN[1,1]

svy: mean hholdsz
matrix EST_C_HHSIZE = e(b)
local est_C_hhsize = EST_C_HHSIZE[1,1]

* Calculate bias
local bias_C_income = `est_C_income' - `true_income_nat'
local bias_C_inc_kzn = `est_C_inc_kzn' - `true_income_kzn'
local bias_C_hhsize = `est_C_hhsize' - `true_hhsize'
local relbias_C = 100 * `bias_C_income' / `true_income_nat'

display _n "STRATEGY C ESTIMATES:"
display "Mean HH Income (National): R " %12.0fc `est_C_income' " | Bias: R " %10.0fc `bias_C_income'
display "Mean HH Income (KZN):      R " %12.0fc `est_C_inc_kzn' " | Bias: R " %10.0fc `bias_C_inc_kzn'
display "Mean HH Size (National):   " %6.2f `est_C_hhsize' " | Bias: " %6.3f `bias_C_hhsize'
display _n "Relative Bias (Income): " %6.2f `relbias_C' "%" _n

*-------------------------------------------------------------------------------
* 7. COMPARISON OF ALL STRATEGIES
*-------------------------------------------------------------------------------

display _n "================================================================"
display "SECTION 7: COMPARISON OF ALL STRATEGIES"
display "================================================================" _n

* Create comparison matrix
matrix COMPARISON = J(4, 7, .)
matrix rownames COMPARISON = "True" "A_Drop" "B_Reweight" "C_Substitute"
matrix colnames COMPARISON = "Income_Nat" "Bias_Inc" "RelBias_Pct" "HHSize" "Bias_HHS" "N" "N_PSU"

* Fill in values
* Row 1: True values
use `full_data', clear
local n_full = _N
qui distinct psu
local n_psu_full = r(ndistinct)
matrix COMPARISON[1,1] = `true_income_nat'
matrix COMPARISON[1,2] = 0
matrix COMPARISON[1,3] = 0
matrix COMPARISON[1,4] = `true_hhsize'
matrix COMPARISON[1,5] = 0
matrix COMPARISON[1,6] = `n_full'
matrix COMPARISON[1,7] = `n_psu_full'

* Row 2: Strategy A
keep if accessible == 1
local n_A = _N
qui distinct psu
local n_psu_A = r(ndistinct)
matrix COMPARISON[2,1] = `est_A_income'
matrix COMPARISON[2,2] = `bias_A_income'
matrix COMPARISON[2,3] = `relbias_A'
matrix COMPARISON[2,4] = `est_A_hhsize'
matrix COMPARISON[2,5] = `bias_A_hhsize'
matrix COMPARISON[2,6] = `n_A'
matrix COMPARISON[2,7] = `n_psu_A'

* Row 3: Strategy B (same sample as A, different weights)
matrix COMPARISON[3,1] = `est_B_income'
matrix COMPARISON[3,2] = `bias_B_income'
matrix COMPARISON[3,3] = `relbias_B'
matrix COMPARISON[3,4] = `est_B_hhsize'
matrix COMPARISON[3,5] = `bias_B_hhsize'
matrix COMPARISON[3,6] = `n_A'
matrix COMPARISON[3,7] = `n_psu_A'

* Row 4: Strategy C
use `base_stratC', clear
local n_C = _N
qui distinct psu
local n_psu_C = r(ndistinct)
matrix COMPARISON[4,1] = `est_C_income'
matrix COMPARISON[4,2] = `bias_C_income'
matrix COMPARISON[4,3] = `relbias_C'
matrix COMPARISON[4,4] = `est_C_hhsize'
matrix COMPARISON[4,5] = `bias_C_hhsize'
matrix COMPARISON[4,6] = `n_C'
matrix COMPARISON[4,7] = `n_psu_C'

display "STRATEGY COMPARISON TABLE:"
display "--------------------------" _n
matlist COMPARISON, format(%12.2f) twidth(15)

* Display formatted comparison
display _n
display "Strategy            | Income (R)  | Bias (R)    | RelBias% | HH Size | Bias HHS | N"
display "--------------------+-------------+-------------+----------+---------+----------+--------"
display "Full Sample (True)  | " %11.0fc `true_income_nat' " |           0 |     0.00 | " %7.2f `true_hhsize' " |    0.000 | " %6.0f `n_full'
display "A: Drop PSUs        | " %11.0fc `est_A_income' " | " %11.0fc `bias_A_income' " | " %8.2f `relbias_A' " | " %7.2f `est_A_hhsize' " | " %8.3f `bias_A_hhsize' " | " %6.0f `n_A'
display "B: Re-weight        | " %11.0fc `est_B_income' " | " %11.0fc `bias_B_income' " | " %8.2f `relbias_B' " | " %7.2f `est_B_hhsize' " | " %8.3f `bias_B_hhsize' " | " %6.0f `n_A'
display "C: Substitute       | " %11.0fc `est_C_income' " | " %11.0fc `bias_C_income' " | " %8.2f `relbias_C' " | " %7.2f `est_C_hhsize' " | " %8.3f `bias_C_hhsize' " | " %6.0f `n_C'

*-------------------------------------------------------------------------------
* 8. VISUALIZATION
*-------------------------------------------------------------------------------

display _n "SECTION 8: Creating Visualizations"
display "-----------------------------------" _n

* Create dataset for plotting
clear
input str15 strategy relbias
"A: Drop" `relbias_A'
"B: Re-weight" `relbias_B'
"C: Substitute" `relbias_C'
end

* Bar graph of relative bias
graph bar relbias, over(strategy) ///
    title("Relative Bias in Mean Household Income by Strategy") ///
    subtitle("Conflict Zone: 50% of KwaZulu-Natal PSUs Inaccessible") ///
    ytitle("Relative Bias (%)") ///
    bar(1, color(navy)) ///
    yline(0, lcolor(red) lpattern(dash)) ///
    note("Source: Lab 4.3 Simulation")

graph export "lab4_3_bias_comparison.png", replace width(800) height(600)
display "Saved: lab4_3_bias_comparison.png"

*-------------------------------------------------------------------------------
* 9. SUMMARY AND RECOMMENDATIONS
*-------------------------------------------------------------------------------

display _n "================================================================"
display "SECTION 9: SUMMARY AND RECOMMENDATIONS"
display "================================================================" _n

display "SCENARIO: 50% of PSUs in KwaZulu-Natal (Eastern Highlands) inaccessible" _n

display "STRATEGY COMPARISON:"
display "--------------------" _n

display "Strategy A (Drop PSUs):"
display "  - Simplest approach"
display "  - Relative Bias: " %6.2f `relbias_A' "%"
display "  - Risk: Coverage bias if dropped PSUs differ systematically" _n

display "Strategy B (Re-weight):"
display "  - Maintains target population representation"
display "  - Relative Bias: " %6.2f `relbias_B' "%"
display "  - Assumption: Accessible PSUs represent inaccessible ones"
display "  - Risk: Inflated variance, assumption may not hold" _n

display "Strategy C (Substitute):"
display "  - Maintains sample size"
display "  - Relative Bias: " %6.2f `relbias_C' "%"
display "  - Risk: Substitutes may differ from original PSUs"
display "  - Requires careful matching and documentation" _n

* Determine best strategy
local abs_A = abs(`relbias_A')
local abs_B = abs(`relbias_B')
local abs_C = abs(`relbias_C')

local best = "A (Drop)"
local best_bias = `abs_A'
if `abs_B' < `best_bias' {
    local best = "B (Re-weight)"
    local best_bias = `abs_B'
}
if `abs_C' < `best_bias' {
    local best = "C (Substitute)"
    local best_bias = `abs_C'
}

display "RECOMMENDED APPROACH:"
display "Based on lowest absolute bias, Strategy `best' performed best." _n

display "IMPORTANT CAVEATS:"
display "1. Results depend on how inaccessible PSUs differ from accessible ones"
display "2. Always document the approach and conduct sensitivity analysis"
display "3. Report both adjusted and unadjusted estimates when possible"
display "4. Consider combining strategies (e.g., substitute + re-weight)" _n

*-------------------------------------------------------------------------------
* 10. EXPORT RESULTS
*-------------------------------------------------------------------------------

display "SECTION 10: Exporting Results"
display "-----------------------------" _n

* Export comparison table to CSV
clear
input str20 strategy income_nat bias_income relbias_pct hhsize bias_hhsize n
"Full Sample (True)" `true_income_nat' 0 0 `true_hhsize' 0 `n_full'
"A: Drop PSUs" `est_A_income' `bias_A_income' `relbias_A' `est_A_hhsize' `bias_A_hhsize' `n_A'
"B: Re-weight" `est_B_income' `bias_B_income' `relbias_B' `est_B_hhsize' `bias_B_hhsize' `n_A'
"C: Substitute" `est_C_income' `bias_C_income' `relbias_C' `est_C_hhsize' `bias_C_hhsize' `n_C'
end

export delimited using "lab4_3_strategy_comparison.csv", replace
display "Saved: lab4_3_strategy_comparison.csv"

display _n "================================================================"
display "  LAB 4.3 COMPLETE"
display "================================================================"

log close

*===============================================================================
* END OF LAB 4.3
*===============================================================================
