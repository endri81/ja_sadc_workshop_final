********************************************************************************
* LAB 3.4: HANDLING LONELY PSUs (SINGLETON STRATA)
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 3, Module 3.4
*
* Software: Stata 15+ with survey (svy) commands
* Data: GHS 2024 Household File (ghs-2024-hhold-v1.dta)
*
* Learning Objectives:
*   - Understand why singleton strata cause variance estimation problems
*   - Simulate singleton stratum scenarios
*   - Compare different correction methods (singleunit options)
*   - Apply appropriate solutions in practice
*
* Author: SADC Workshop Team
* Date: January 2026
********************************************************************************

clear all
set more off
capture log close

* Set working directory (adjust path as needed)
* cd "your/working/directory"

* Create output directory
capture mkdir "lab3_4_output"

* Start log file
log using "lab3_4_output/lab3_4_lonely_psu.log", replace

********************************************************************************
* SECTION 1: SETUP AND DATA PREPARATION
********************************************************************************

display _newline
display "=" * 72
display "LAB 3.4: HANDLING LONELY PSUs (SINGLETON STRATA)"
display "=" * 72
display _newline

* Load GHS 2024 household data
use "ghs-2024-hhold-v1.dta", clear

display "Data loaded: " _N " households"
display _newline

********************************************************************************
* SECTION 2: UNDERSTAND THE PROBLEM
********************************************************************************

display "-" * 72
display "SECTION 2: THE SINGLETON PSU PROBLEM"
display "-" * 72
display _newline

* Examine current stratification
preserve
    collapse (count) n_hh = house_wgt, by(stratum psu)
    collapse (count) n_psu = psu (sum) n_hh, by(stratum)
    
    summarize n_psu, detail
    local min_psu = r(min)
    local max_psu = r(max)
    
    * Count singleton strata
    count if n_psu == 1
    local n_singletons = r(N)
    
    display "Current design summary:"
    display "  PSUs per stratum range: `min_psu' to `max_psu'"
    display "  Existing singleton strata: `n_singletons'"
restore

display _newline
display "THE PROBLEM:"
display "  Variance estimation formula requires at least 2 PSUs per stratum:"
display "  "
display "  Var_h = (n_h / (n_h - 1)) * sum[(z_hi - z_h_bar)^2]"
display "  "
display "  When n_h = 1:"
display "    - Denominator (n_h - 1) = 0 -> Division by zero!"
display "    - Only one PSU -> Cannot estimate within-stratum variance"
display "    - No information about variability in that stratum"
display _newline

********************************************************************************
* SECTION 3: SIMULATE SINGLETON STRATA
********************************************************************************

display "-" * 72
display "SECTION 3: SIMULATING SINGLETON STRATA"
display "-" * 72
display _newline

* Save original data
preserve

* Strategy: Create artificial singletons by modifying stratum assignments
* We'll identify strata with 2 PSUs and remove one PSU from some of them

* First, find PSUs to remove
bysort stratum psu: gen psu_tag = (_n == 1)
bysort stratum: egen n_psu_in_stratum = total(psu_tag)

* Identify strata with exactly 2 PSUs
tab n_psu_in_stratum if psu_tag == 1

* Select some strata with 2 PSUs to convert to singletons
* Get list of strata with 2 PSUs
levelsof stratum if n_psu_in_stratum == 2 & psu_tag == 1, local(strata_2psu)

* Take first 3 strata from this list
local count = 0
local target_strata ""
foreach s of local strata_2psu {
    local count = `count' + 1
    if `count' <= 3 {
        local target_strata "`target_strata' `s'"
    }
}

display "Converting these strata to singletons: `target_strata'"

* For each target stratum, keep only households from one PSU
foreach s of local target_strata {
    * Find the two PSUs in this stratum
    levelsof psu if stratum == `s', local(psus_in_s)
    local psu_count = 0
    foreach p of local psus_in_s {
        local psu_count = `psu_count' + 1
        if `psu_count' == 2 {
            * Remove the second PSU
            display "  Removing PSU `p' from stratum `s'"
            drop if stratum == `s' & psu == `p'
        }
    }
}

* Verify singletons were created
drop psu_tag n_psu_in_stratum
bysort stratum psu: gen psu_tag = (_n == 1)
bysort stratum: egen n_psu_in_stratum = total(psu_tag)

display _newline
display "Singleton strata after modification:"
list stratum n_psu_in_stratum if n_psu_in_stratum == 1 & psu_tag == 1, noobs

* Count singletons
count if n_psu_in_stratum == 1 & psu_tag == 1
local n_new_singletons = r(N)
display _newline
display "Number of singleton strata created: `n_new_singletons'"

* Clean up helper variables
drop psu_tag n_psu_in_stratum

* Save simulated data
save "lab3_4_output/ghs_with_singletons.dta", replace

********************************************************************************
* SECTION 4: DEMONSTRATE THE ERROR
********************************************************************************

display _newline
display "-" * 72
display "SECTION 4: DEMONSTRATING THE ERROR"
display "-" * 72
display _newline

* Set up survey design with DEFAULT singleunit option (missing)
* This will produce missing standard errors for estimates affected by singletons

display "Setting: svyset ..., singleunit(missing)"
display "This is the DEFAULT behavior - produces missing SE"
display _newline

svyset psu [pw=house_wgt], strata(stratum) singleunit(missing)
svydescribe

* Try to estimate mean income
display _newline
display "Attempting to estimate mean household income..."
svy: mean totmhinc

display _newline
display "Note: SE may be missing (.) if singletons affect the estimate"
display _newline

********************************************************************************
* SECTION 5: CORRECTION METHOD 1 - CERTAINTY
********************************************************************************

display "-" * 72
display "SECTION 5: METHOD 1 - CERTAINTY"
display "-" * 72
display _newline

display "Setting: svyset ..., singleunit(certainty)"
display "Treatment: Singleton PSU contributes ZERO to variance"
display "Rationale: Treat as if PSU was selected with certainty (probability = 1)"
display "Effect: Variance may be UNDERESTIMATED"
display _newline

svyset psu [pw=house_wgt], strata(stratum) singleunit(certainty)

* Estimate mean
svy: mean totmhinc
matrix results_certainty = r(table)
local mean_certainty = results_certainty[1,1]
local se_certainty = results_certainty[2,1]
local ci_lb_certainty = results_certainty[5,1]
local ci_ub_certainty = results_certainty[6,1]

display _newline
display "Results (Certainty Method):"
display "  Mean: R " %12.0fc `mean_certainty'
display "  SE:   R " %12.0fc `se_certainty'
display "  95% CI: [R " %12.0fc `ci_lb_certainty' ", R " %12.0fc `ci_ub_certainty' "]"
display _newline

********************************************************************************
* SECTION 6: CORRECTION METHOD 2 - CENTERED
********************************************************************************

display "-" * 72
display "SECTION 6: METHOD 2 - CENTERED"
display "-" * 72
display _newline

display "Setting: svyset ..., singleunit(centered)"
display "Treatment: Center singleton around GRAND MEAN instead of stratum mean"
display "Rationale: Use overall variability as proxy for missing stratum variance"
display "Effect: Usually INCREASES variance estimate (conservative)"
display _newline

svyset psu [pw=house_wgt], strata(stratum) singleunit(centered)

* Estimate mean
svy: mean totmhinc
matrix results_centered = r(table)
local mean_centered = results_centered[1,1]
local se_centered = results_centered[2,1]
local ci_lb_centered = results_centered[5,1]
local ci_ub_centered = results_centered[6,1]

display _newline
display "Results (Centered Method):"
display "  Mean: R " %12.0fc `mean_centered'
display "  SE:   R " %12.0fc `se_centered'
display "  95% CI: [R " %12.0fc `ci_lb_centered' ", R " %12.0fc `ci_ub_centered' "]"
display _newline

********************************************************************************
* SECTION 7: CORRECTION METHOD 3 - SCALED
********************************************************************************

display "-" * 72
display "SECTION 7: METHOD 3 - SCALED"
display "-" * 72
display _newline

display "Setting: svyset ..., singleunit(scaled)"
display "Treatment: Use a scaled version of centered method"
display "Rationale: Adjusts for degrees of freedom"
display "Effect: Similar to centered but with df adjustment"
display _newline

svyset psu [pw=house_wgt], strata(stratum) singleunit(scaled)

* Estimate mean
svy: mean totmhinc
matrix results_scaled = r(table)
local mean_scaled = results_scaled[1,1]
local se_scaled = results_scaled[2,1]
local ci_lb_scaled = results_scaled[5,1]
local ci_ub_scaled = results_scaled[6,1]

display _newline
display "Results (Scaled Method):"
display "  Mean: R " %12.0fc `mean_scaled'
display "  SE:   R " %12.0fc `se_scaled'
display "  95% CI: [R " %12.0fc `ci_lb_scaled' ", R " %12.0fc `ci_ub_scaled' "]"
display _newline

********************************************************************************
* SECTION 8: COMPARISON TABLE
********************************************************************************

display "-" * 72
display "SECTION 8: COMPARISON OF ALL METHODS"
display "-" * 72
display _newline

* Calculate CV and CI width for each method
local cv_certainty = (`se_certainty' / `mean_certainty') * 100
local cv_centered = (`se_centered' / `mean_centered') * 100
local cv_scaled = (`se_scaled' / `mean_scaled') * 100

local ciw_certainty = `ci_ub_certainty' - `ci_lb_certainty'
local ciw_centered = `ci_ub_centered' - `ci_lb_centered'
local ciw_scaled = `ci_ub_scaled' - `ci_lb_scaled'

local se_ratio_certainty = 1
local se_ratio_centered = `se_centered' / `se_certainty'
local se_ratio_scaled = `se_scaled' / `se_certainty'

display "COMPARISON OF LONELY PSU HANDLING METHODS"
display "========================================="
display _newline

display %20s "Method" " | " %12s "Mean (R)" " | " %10s "SE (R)" " | " %8s "CV (%)" " | " %10s "SE Ratio"
display "-" * 72

display %20s "Certainty" " | " %12.0fc `mean_certainty' " | " %10.0fc `se_certainty' " | " %8.2f `cv_certainty' " | " %10.4f `se_ratio_certainty'
display %20s "Centered" " | " %12.0fc `mean_centered' " | " %10.0fc `se_centered' " | " %8.2f `cv_centered' " | " %10.4f `se_ratio_centered'
display %20s "Scaled" " | " %12.0fc `mean_scaled' " | " %10.0fc `se_scaled' " | " %8.2f `cv_scaled' " | " %10.4f `se_ratio_scaled'

display "-" * 72
display _newline

* Create comparison dataset
clear
input str20 method mean se cv ci_lower ci_upper ci_width se_ratio
"Certainty" `mean_certainty' `se_certainty' `cv_certainty' `ci_lb_certainty' `ci_ub_certainty' `ciw_certainty' `se_ratio_certainty'
"Centered" `mean_centered' `se_centered' `cv_centered' `ci_lb_centered' `ci_ub_centered' `ciw_centered' `se_ratio_centered'
"Scaled" `mean_scaled' `se_scaled' `cv_scaled' `ci_lb_scaled' `ci_ub_scaled' `ciw_scaled' `se_ratio_scaled'
end

* Save comparison table
export delimited using "lab3_4_output/lonely_psu_comparison.csv", replace
display "Saved: lonely_psu_comparison.csv"
save "lab3_4_output/lonely_psu_comparison.dta", replace
display "Saved: lonely_psu_comparison.dta"

********************************************************************************
* SECTION 9: BASELINE COMPARISON (NO SINGLETONS)
********************************************************************************

display _newline
display "-" * 72
display "SECTION 9: BASELINE (ORIGINAL DATA WITHOUT SINGLETONS)"
display "-" * 72
display _newline

* Load original data
restore

* Set up survey design
svyset psu [pw=house_wgt], strata(stratum)
svydescribe

* Estimate mean
svy: mean totmhinc
matrix results_baseline = r(table)
local mean_baseline = results_baseline[1,1]
local se_baseline = results_baseline[2,1]

display _newline
display "Baseline Results (Original data, no artificial singletons):"
display "  Mean: R " %12.0fc `mean_baseline'
display "  SE:   R " %12.0fc `se_baseline'
display _newline

display "Impact of singleton strata:"
display "  Baseline SE:   R " %10.0fc `se_baseline'
display "  Certainty SE:  R " %10.0fc `se_certainty' " (" %5.1f ((`se_certainty'/`se_baseline' - 1)*100) "% change)"
display "  Centered SE:   R " %10.0fc `se_centered' " (" %5.1f ((`se_centered'/`se_baseline' - 1)*100) "% change)"
display "  Scaled SE:     R " %10.0fc `se_scaled' " (" %5.1f ((`se_scaled'/`se_baseline' - 1)*100) "% change)"
display _newline

********************************************************************************
* SECTION 10: VISUALIZATION
********************************************************************************

display "-" * 72
display "SECTION 10: VISUALIZATION"
display "-" * 72
display _newline

* Load comparison data and add baseline
use "lab3_4_output/lonely_psu_comparison.dta", clear

local cv_baseline = (`se_baseline' / `mean_baseline') * 100
local ciw_baseline = 2 * 1.96 * `se_baseline'
local ci_lb_baseline = `mean_baseline' - 1.96 * `se_baseline'
local ci_ub_baseline = `mean_baseline' + 1.96 * `se_baseline'

set obs 4
replace method = "Baseline" in 4
replace mean = `mean_baseline' in 4
replace se = `se_baseline' in 4
replace cv = `cv_baseline' in 4
replace ci_lower = `ci_lb_baseline' in 4
replace ci_upper = `ci_ub_baseline' in 4
replace ci_width = `ciw_baseline' in 4
replace se_ratio = `se_baseline' / `se_certainty' in 4

* Create method order variable
gen method_order = .
replace method_order = 1 if method == "Baseline"
replace method_order = 2 if method == "Certainty"
replace method_order = 3 if method == "Scaled"
replace method_order = 4 if method == "Centered"
sort method_order

* Bar chart of SEs
graph bar se, over(method, sort(method_order) label(angle(45))) ///
    title("Standard Error by Lonely PSU Handling Method") ///
    subtitle("Mean Household Income (GHS 2024 with Simulated Singletons)") ///
    ytitle("Standard Error (Rands)") ///
    bar(1, color("0 51 102")) ///
    blabel(bar, format(%12.0fc) size(small)) ///
    ylabel(, format(%12.0fc)) ///
    scheme(s1color)
graph export "lab3_4_output/lonely_psu_se_comparison.png", replace width(1000)
display "Saved: lonely_psu_se_comparison.png"

* CI comparison plot
twoway (rcap ci_lower ci_upper method_order, lcolor("0 51 102") lwidth(thick)) ///
       (scatter mean method_order, mcolor("0 51 102") msize(large)), ///
    title("95% Confidence Intervals by Method") ///
    subtitle("Mean Household Income") ///
    ytitle("Income (Rands)") ///
    xtitle("") ///
    xlabel(1 "Baseline" 2 "Certainty" 3 "Scaled" 4 "Centered", angle(45)) ///
    ylabel(, format(%12.0fc)) ///
    legend(off) ///
    scheme(s1color)
graph export "lab3_4_output/lonely_psu_ci_comparison.png", replace width(1000)
display "Saved: lonely_psu_ci_comparison.png"
display _newline

* Save final comparison with baseline
save "lab3_4_output/lonely_psu_comparison_full.dta", replace
export delimited using "lab3_4_output/lonely_psu_comparison_full.csv", replace

********************************************************************************
* SECTION 11: PRACTICAL RECOMMENDATIONS
********************************************************************************

display "-" * 72
display "SECTION 11: PRACTICAL RECOMMENDATIONS"
display "-" * 72
display _newline

display "STATA SINGLEUNIT OPTIONS:"
display "========================="
display _newline

display "1. singleunit(missing)  - DEFAULT"
display "   Treatment: Produces missing (.) standard errors"
display "   Use when: You want to flag affected estimates"
display _newline

display "2. singleunit(certainty)"
display "   Treatment: Singleton contributes ZERO to variance"
display "   Use when: PSU was actually selected with certainty"
display "   Caution: UNDERESTIMATES variance if not true certainty"
display _newline

display "3. singleunit(centered)"
display "   Treatment: Center around grand mean"
display "   Use when: Want conservative (larger) variance"
display "   Equivalent to R: options(survey.lonely.psu = 'adjust')"
display _newline

display "4. singleunit(scaled)"
display "   Treatment: Scaled version of centered"
display "   Use when: Want df-adjusted conservative estimate"
display _newline

display "COMPARISON WITH R OPTIONS:"
display "=========================="
display _newline
display "  Stata                  R equivalent"
display "  -----                  ------------"
display "  singleunit(missing)    survey.lonely.psu = 'fail'"
display "  singleunit(certainty)  survey.lonely.psu = 'certainty'"
display "  singleunit(centered)   survey.lonely.psu = 'adjust'"
display "  singleunit(scaled)     survey.lonely.psu = 'adjust' (approx)"
display "  [not available]        survey.lonely.psu = 'average'"
display "  [not available]        survey.lonely.psu = 'remove'"
display _newline

display "BEST PRACTICE:"
display "--------------"
display "1. PREVENT singletons in design stage:"
display "   - Sample at least 2 PSUs per stratum"
display "   - Collapse small strata during design"
display "2. If singletons occur (e.g., non-response):"
display "   - Document the issue"
display "   - Use 'centered' for conservative estimates"
display "   - Report sensitivity analysis with multiple methods"
display "3. For official statistics:"
display "   - Follow agency guidelines"
display "   - Common choice: 'centered' or 'certainty' depending on context"
display _newline

********************************************************************************
* WRAP UP
********************************************************************************

display "=" * 72
display "LAB 3.4 COMPLETE"
display "=" * 72
display _newline
display "Output files saved to: lab3_4_output/"
display _newline
display "Files created:"
display "  - lonely_psu_comparison.csv"
display "  - lonely_psu_comparison.dta"
display "  - lonely_psu_comparison_full.csv"
display "  - lonely_psu_comparison_full.dta"
display "  - lonely_psu_se_comparison.png"
display "  - lonely_psu_ci_comparison.png"
display "  - ghs_with_singletons.dta"
display "  - lab3_4_lonely_psu.log"
display _newline

log close
