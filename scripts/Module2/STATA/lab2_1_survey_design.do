********************************************************************************
*                                                                              *
*   SADC REGIONAL TRAINING WORKSHOP ON ADVANCED SAMPLING METHODS               *
*   Day 2: Computational Tools and Spatial Integration                         *
*                                                                              *
*   LAB 2.1: DEFINING COMPLEX SURVEY DESIGN OBJECTS IN STATA                   *
*                                                                              *
*   Republic of Zambara — Household Living Conditions Survey                   *
*   Data Source: Zambara National Statistics Office (ZNSO)                     *
*   [Based on Statistics South Africa GHS 2024]                                *
*                                                                              *
********************************************************************************
*                                                                              *
*   Author:      Prof. Endri Raço (Consultant)                                 *
*   Workshop:    SADC Advanced Sampling Methods, March 2026                    *
*   Location:    Johannesburg, South Africa                                    *
*                                                                              *
*   Learning Objectives:                                                       *
*   LO2.1: Configure survey design settings in STATA using svyset              *
*   LO2.2: Understand the consequences of ignoring complex design              *
*   LO2.3: Compare naive vs. design-based standard errors                      *
*                                                                              *
*   Narrative Context:                                                         *
*   Nomsa leads the STATA track at ZNSO. She demonstrates to her team          *
*   how the svy prefix commands properly account for the complex survey        *
*   design, revealing dramatic differences from their previous naive analyses. *
*                                                                              *
*   Reference:                                                                 *
*   StataCorp. Stata Survey Data Reference Manual. College Station, TX.        *
*                                                                              *
********************************************************************************

********************************************************************************
* SECTION 0: ENVIRONMENT SETUP
********************************************************************************

clear all
set more off
capture log close

* Set working directory (adjust as needed)
* cd "C:\Workshop\Day2"

* Start log file
log using "lab2_1_survey_design.log", replace text

* Display header
display _newline(2)
display as text "{hline 70}"
display as result "LAB 2.1: DEFINING COMPLEX SURVEY DESIGN OBJECTS"
display as result "SADC Regional Training Workshop — Day 2"
display as text "{hline 70}"
display _newline

* Record session info
display as text "Date: " c(current_date) " " c(current_time)
display as text "Stata Version: " c(stata_version)
display as text "{hline 70}"
display _newline(2)


********************************************************************************
* SECTION 1: DATA LOADING AND EXPLORATION
********************************************************************************

display as text "{hline 70}"
display as result "SECTION 1: LOADING ZAMBARA HOUSEHOLD SURVEY DATA"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 1.1 Load the Zambara (GHS 2024) household data
* ------------------------------------------------------------------------------

* Load the dataset
use "ghs-2024-hhold-v1.dta", clear

* Basic dataset description
display as text "DATASET OVERVIEW"
display as text "{hline 50}"
describe, short

display _newline
display as text "Number of households: " _N
display as text "Number of variables:  " c(k)
display _newline

* ------------------------------------------------------------------------------
* 1.2 Examine key sampling design variables
* ------------------------------------------------------------------------------

* Key variables for complex survey design:
* - psu: Primary Sampling Unit (cluster identifier)
* - stratum: Stratification variable
* - house_wgt: Household sampling weight
* - prov: Province (domain variable)
* - geotype: Geography type

display as text "KEY SAMPLING DESIGN VARIABLES"
display as text "{hline 50}"
display _newline

* Check for presence of key variables
local key_vars "psu stratum house_wgt prov geotype"
foreach var of local key_vars {
    capture confirm variable `var'
    if _rc == 0 {
        display as text "  `var': " as result "✓ Present"
    }
    else {
        display as text "  `var': " as error "✗ MISSING"
    }
}

display _newline

* ------------------------------------------------------------------------------
* 1.3 Summarize design variables
* ------------------------------------------------------------------------------

display as text "DESIGN VARIABLE SUMMARY"
display as text "{hline 50}"
display _newline

* Number of PSUs (clusters)
quietly: egen tag_psu = tag(psu)
quietly: count if tag_psu == 1
local n_psu = r(N)
display as text "Number of PSUs (clusters): " as result %8.0fc `n_psu'

* Number of strata
quietly: egen tag_stratum = tag(stratum)
quietly: count if tag_stratum == 1
local n_strata = r(N)
display as text "Number of strata:          " as result %8.0fc `n_strata'

drop tag_psu tag_stratum

* Weight distribution
display _newline
display as text "Household weight distribution:"
summarize house_wgt, detail

* Households per PSU
display _newline
display as text "Households per PSU:"
bysort psu: gen hh_count = _N
summarize hh_count if psu != psu[_n-1], detail
drop hh_count

* Province distribution
display _newline
display as text "Households by Province:"
tabulate prov, missing


********************************************************************************
* SECTION 2: THE PROBLEM — NAIVE ANALYSIS IGNORES DESIGN
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 2: THE PROBLEM WITH NAIVE ANALYSIS"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 2.1 What Nomsa's team was doing wrong
* ------------------------------------------------------------------------------

display as text "SCENARIO: Nomsa discovers her team's analysis mistake"
display as text "{hline 50}"
display _newline

display as text "Nomsa explains: 'We've been running commands like -mean- and"
display as text "-regress- without the svy prefix. This produces INCORRECT"
display as text "standard errors because it ignores:"
display _newline
display as text "  1. CLUSTERING: Households in the same PSU are correlated"
display as text "  2. STRATIFICATION: Deliberate selection from each stratum"
display as text "  3. UNEQUAL WEIGHTS: Different selection probabilities'"
display _newline(2)

* ------------------------------------------------------------------------------
* 2.2 Calculate naive (SRS) estimates
* ------------------------------------------------------------------------------

display as text "NAIVE ANALYSIS (Ignoring Complex Design)"
display as text "{hline 50}"
display _newline

display as text "Outcome variable: totmhinc (Total Monthly Household Income)"
display _newline

* Check for missing values
quietly: count if missing(totmhinc)
local n_missing = r(N)
quietly: count if !missing(totmhinc)
local n_valid = r(N)

display as text "Missing values:      " as result %8.0fc `n_missing'
display as text "Valid observations:  " as result %8.0fc `n_valid'
display _newline

* Keep only non-missing income observations for analysis
preserve
keep if !missing(totmhinc)

* ------------------------------------------------------------------------------
* Method 1: Completely unweighted mean (treats as SRS, ignores weights)
* ------------------------------------------------------------------------------

display as text "Method 1: Unweighted mean (ignores weights entirely)"
display as text "{hline 50}"

mean totmhinc

* Store results
local naive_unweighted_mean = e(b)[1,1]
local naive_unweighted_se = sqrt(e(V)[1,1])

display _newline
display as text "  Mean income:  R " as result %12.2fc `naive_unweighted_mean'
display as text "  Naive SE:     R " as result %12.2fc `naive_unweighted_se'
display _newline

* ------------------------------------------------------------------------------
* Method 2: Weighted mean but naive SE (applies weights, ignores clustering)
* ------------------------------------------------------------------------------

display as text "Method 2: Weighted mean with naive SE (ignores clustering)"
display as text "{hline 50}"

* Calculate weighted mean using -mean- with pweights
* This applies weights but INCORRECTLY treats observations as independent
mean totmhinc [pweight = house_wgt]

local naive_weighted_mean = e(b)[1,1]
local naive_weighted_se = sqrt(e(V)[1,1])

display _newline
display as text "  Weighted mean: R " as result %12.2fc `naive_weighted_mean'
display as text "  Naive SE:      R " as result %12.2fc `naive_weighted_se'
display _newline(2)


********************************************************************************
* SECTION 3: THE SOLUTION — PROPER SURVEY DESIGN SPECIFICATION
********************************************************************************

display as text "{hline 70}"
display as result "SECTION 3: DEFINING THE COMPLEX SURVEY DESIGN"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 3.1 The svyset command
* ------------------------------------------------------------------------------

display as text "CREATING THE SURVEY DESIGN WITH svyset"
display as text "{hline 50}"
display _newline

display as text "The svyset command specifies the complex sampling design:"
display _newline
display as text "  svyset psu [pw=house_wgt], strata(stratum)"
display _newline
display as text "Where:"
display as text "  psu            = Primary sampling unit (cluster identifier)"
display as text "  [pw=house_wgt] = Probability weights"
display as text "  strata(stratum)= Stratification variable"
display _newline

* ------------------------------------------------------------------------------
* 3.2 Set up the survey design
* ------------------------------------------------------------------------------

display as text "Setting survey design..."
display _newline

* Define the survey design
* Note: vce(linearized) is the default variance estimation method
svyset psu [pw = house_wgt], strata(stratum) vce(linearized) singleunit(centered)

* Display design settings
display _newline
display as text "SURVEY DESIGN SETTINGS"
display as text "{hline 50}"
svyset

display _newline
display as text "Design verification:"
display as text "  PSU variable:     psu"
display as text "  Weight variable:  house_wgt"
display as text "  Strata variable:  stratum"
display as text "  VCE method:       Linearized (Taylor series)"
display _newline


********************************************************************************
* SECTION 4: DESIGN-BASED ESTIMATION
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 4: DESIGN-BASED ESTIMATION"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 4.1 Mean estimation with proper standard errors
* ------------------------------------------------------------------------------

display as text "ESTIMATING MEAN HOUSEHOLD INCOME (Design-Based)"
display as text "{hline 50}"
display _newline

* Calculate design-based mean using svy prefix
svy: mean totmhinc

* Store design-based results
local design_mean = e(b)[1,1]
local design_se = sqrt(e(V)[1,1])

* Calculate coefficient of variation
local cv = 100 * `design_se' / `design_mean'

display _newline
display as text "Design-based estimation results:"
display _newline
display as text "  Point estimate: R " as result %12.2fc `design_mean'
display as text "  Standard error: R " as result %12.2fc `design_se'
display as text "  CV:             " as result %6.2f `cv' "%"
display _newline

* Calculate and display confidence interval
local ci_lower = `design_mean' - 1.96 * `design_se'
local ci_upper = `design_mean' + 1.96 * `design_se'

display as text "  95% CI: [R " as result %10.2fc `ci_lower' ///
        as text ", R " as result %10.2fc `ci_upper' as text "]"
display _newline


********************************************************************************
* SECTION 5: COMPARISON — NAIVE vs. DESIGN-BASED
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 5: COMPARING NAIVE vs. DESIGN-BASED STANDARD ERRORS"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 5.1 Calculate Design Effect (DEFF)
* ------------------------------------------------------------------------------

display as text "DESIGN EFFECT (DEFF) CALCULATION"
display as text "{hline 50}"
display _newline

* DEFF = Var(design-based) / Var(naive weighted)
local deff = (`design_se' / `naive_weighted_se')^2

display as text "DEFF = Var(complex design) / Var(SRS with weights)"
display as text "DEFF = " as result %6.3f `deff'
display _newline

display as text "Interpretation:"
display as text "  A DEFF of " as result %4.2f `deff' as text " means the variance under the complex design"
display as text "  is " as result %4.2f `deff' as text " times larger than under SRS assumptions."
display _newline

if `deff' > 1 {
    display as error "  WARNING: DEFF > 1 indicates clustering INFLATES variance."
    display as error "  Ignoring the design leads to UNDERESTIMATED standard errors!"
}
else {
    display as text "  Note: DEFF < 1 indicates stratification reduces variance more than"
    display as text "  clustering increases it (uncommon in household surveys)."
}

* ------------------------------------------------------------------------------
* 5.2 SE comparison ratio
* ------------------------------------------------------------------------------

local se_ratio = `design_se' / `naive_weighted_se'
local underestimate_pct = 100 * (`design_se' - `naive_weighted_se') / `design_se'

display _newline(2)
display as text "STANDARD ERROR COMPARISON"
display as text "{hline 50}"
display _newline

display as text "SE Ratio (Design / Naive) = " as result %6.3f `se_ratio'
display _newline

display as text "This means:"
display as text "  - Naive SE:        R " as result %10.2fc `naive_weighted_se'
display as text "  - Design-based SE: R " as result %10.2fc `design_se'
display as text "  - Difference:      R " as result %10.2fc (`design_se' - `naive_weighted_se')
display _newline

display as text "The naive approach UNDERESTIMATES the standard error by " ///
        as result %4.1f `underestimate_pct' as text "%"
display _newline

display as text "PRACTICAL CONSEQUENCES:"
display as text "  - Confidence intervals too narrow"
display as text "  - P-values too small"
display as text "  - False positive rate inflated"
display as text "  - Type I error rate exceeds nominal level"
display _newline

* ------------------------------------------------------------------------------
* 5.3 Formatted comparison table
* ------------------------------------------------------------------------------

display _newline(2)
display as text "COMPREHENSIVE COMPARISON TABLE"
display as text "{hline 70}"
display _newline

* Create comparison table using putexcel or display
display as text _col(1) "Method" _col(28) "Mean" _col(42) "SE" _col(54) "CV(%)"
display as text "{hline 70}"
display as text _col(1) "Unweighted (SRS)" ///
        _col(22) "R " %10.2fc `naive_unweighted_mean' ///
        _col(36) "R " %8.2fc `naive_unweighted_se' ///
        _col(52) %6.2f 100*`naive_unweighted_se'/`naive_unweighted_mean'

display as text _col(1) "Weighted (Naive SE)" ///
        _col(22) "R " %10.2fc `naive_weighted_mean' ///
        _col(36) "R " %8.2fc `naive_weighted_se' ///
        _col(52) %6.2f 100*`naive_weighted_se'/`naive_weighted_mean'

display as result _col(1) "Design-Based (Correct)" ///
        _col(22) "R " %10.2fc `design_mean' ///
        _col(36) "R " %8.2fc `design_se' ///
        _col(52) %6.2f `cv'
display as text "{hline 70}"
display _newline


********************************************************************************
* SECTION 6: DESIGN EFFECTS USING estat effects
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 6: DETAILED DESIGN EFFECT ANALYSIS"
display as text "{hline 70}"
display _newline

* Re-run svy mean and request design effects
svy: mean totmhinc

display _newline
display as text "DESIGN EFFECTS FROM estat effects:"
display as text "{hline 50}"
estat effects

display _newline
display as text "Interpretation of estat effects output:"
display as text "  DEFF: Design effect (variance ratio)"
display as text "  DEFT: Root design effect (SE ratio)"
display as text "  MoE:  Margin of error"
display _newline


********************************************************************************
* SECTION 7: DOMAIN ESTIMATION BY PROVINCE
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 7: DOMAIN ESTIMATION BY PROVINCE"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 7.1 Provincial estimates with design-based SEs
* ------------------------------------------------------------------------------

display as text "MEAN HOUSEHOLD INCOME BY PROVINCE (Design-Based)"
display as text "{hline 50}"
display _newline

svy: mean totmhinc, over(prov)

* Display design effects by province
display _newline
display as text "DESIGN EFFECTS BY PROVINCE:"
display as text "{hline 50}"
estat effects

* ------------------------------------------------------------------------------
* 7.2 Compare with naive provincial estimates
* ------------------------------------------------------------------------------

display _newline(2)
display as text "NAIVE vs. DESIGN-BASED SE BY PROVINCE"
display as text "{hline 50}"
display _newline

* Store design-based results
matrix design_results = e(b)
matrix design_var = e(V)

* Calculate naive results for comparison
preserve
collapse (mean) mean_income = totmhinc ///
         (sd) sd_income = totmhinc ///
         (count) n = totmhinc ///
         (sum) sum_wgt = house_wgt ///
         [pw = house_wgt], by(prov)

* Naive SE calculation (treating as SRS within each province)
gen naive_se = sd_income / sqrt(n)

* Display comparison
list prov mean_income naive_se n, clean noobs

restore


********************************************************************************
* SECTION 8: EXPORT RESULTS
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 8: EXPORTING RESULTS"
display as text "{hline 70}"
display _newline

* ------------------------------------------------------------------------------
* 8.1 Export main comparison table to Excel
* ------------------------------------------------------------------------------

* Create export file
putexcel set "lab2_1_results.xlsx", sheet("SE_Comparison") replace

* Header
putexcel A1 = "Method"
putexcel B1 = "Mean"
putexcel C1 = "SE"
putexcel D1 = "CV(%)"
putexcel E1 = "95% CI Lower"
putexcel F1 = "95% CI Upper"

* Row 1: Unweighted
putexcel A2 = "Unweighted (SRS)"
putexcel B2 = `naive_unweighted_mean'
putexcel C2 = `naive_unweighted_se'
putexcel D2 = 100*`naive_unweighted_se'/`naive_unweighted_mean'
putexcel E2 = `naive_unweighted_mean' - 1.96*`naive_unweighted_se'
putexcel F2 = `naive_unweighted_mean' + 1.96*`naive_unweighted_se'

* Row 2: Weighted (Naive)
putexcel A3 = "Weighted (Naive SE)"
putexcel B3 = `naive_weighted_mean'
putexcel C3 = `naive_weighted_se'
putexcel D3 = 100*`naive_weighted_se'/`naive_weighted_mean'
putexcel E3 = `naive_weighted_mean' - 1.96*`naive_weighted_se'
putexcel F3 = `naive_weighted_mean' + 1.96*`naive_weighted_se'

* Row 3: Design-Based
putexcel A4 = "Design-Based (Correct)"
putexcel B4 = `design_mean'
putexcel C4 = `design_se'
putexcel D4 = `cv'
putexcel E4 = `ci_lower'
putexcel F4 = `ci_upper'

display as text "Exported: lab2_1_results.xlsx (Sheet: SE_Comparison)"

* ------------------------------------------------------------------------------
* 8.2 Export summary statistics
* ------------------------------------------------------------------------------

putexcel set "lab2_1_results.xlsx", sheet("Summary_Stats") modify

putexcel A1 = "Metric"
putexcel B1 = "Value"

putexcel A2 = "Total Households"
putexcel B2 = `n_valid'

putexcel A3 = "Number of PSUs"
putexcel B3 = `n_psu'

putexcel A4 = "Number of Strata"
putexcel B4 = `n_strata'

putexcel A5 = "Mean HH Income (Design)"
putexcel B5 = `design_mean'

putexcel A6 = "SE (Design-Based)"
putexcel B6 = `design_se'

putexcel A7 = "SE (Naive Weighted)"
putexcel B7 = `naive_weighted_se'

putexcel A8 = "Design Effect (DEFF)"
putexcel B8 = `deff'

putexcel A9 = "SE Ratio (Design/Naive)"
putexcel B9 = `se_ratio'

putexcel A10 = "SE Underestimation (%)"
putexcel B10 = `underestimate_pct'

display as text "Exported: lab2_1_results.xlsx (Sheet: Summary_Stats)"

* ------------------------------------------------------------------------------
* 8.3 Export provincial comparison
* ------------------------------------------------------------------------------

putexcel set "lab2_1_results.xlsx", sheet("Provincial") modify

* Save provincial estimates
svy: mean totmhinc, over(prov)
matrix prov_est = e(b)'
matrix prov_se = vecdiag(cholesky(diag(vecdiag(e(V)))))'

putexcel A1 = "Province"
putexcel B1 = "Mean Income"
putexcel C1 = "Design SE"

forvalues i = 1/9 {
    putexcel A`=`i'+1' = `i'
    putexcel B`=`i'+1' = matrix(prov_est[`i',1])
    putexcel C`=`i'+1' = matrix(prov_se[`i',1])
}

display as text "Exported: lab2_1_results.xlsx (Sheet: Provincial)"

restore


********************************************************************************
* SECTION 9: KEY TAKEAWAYS
********************************************************************************

display _newline(2)
display as text "{hline 70}"
display as result "SECTION 9: KEY TAKEAWAYS FOR NOMSA'S TEAM"
display as text "{hline 70}"
display _newline

display as text "NOMSA'S SUMMARY FOR THE ZNSO TEAM:"
display _newline

display as text "1. ALWAYS USE svyset BEFORE ANALYSIS"
display as text "   - Specify PSU, weights, and strata"
display as text "   - Use singleunit(centered) for strata with single PSU"
display _newline

display as text "2. ALWAYS USE THE svy: PREFIX"
display as text "   - svy: mean, svy: proportion, svy: total"
display as text "   - svy: regress, svy: logit for regression"
display as text "   - NEVER use standard commands without svy:"
display _newline

display as text "3. THE DESIGN EFFECT MATTERS"
display as text "   - Our DEFF = " %4.2f `deff' " means variance is " %4.2f `deff' " times larger"
display as text "   - Use estat effects to obtain DEFF after estimation"
display _newline

display as text "4. CONSEQUENCES OF NAIVE ANALYSIS"
display as text "   - SE underestimated by " %4.1f `underestimate_pct' "%"
display as text "   - Confidence intervals too narrow"
display as text "   - P-values misleadingly small"
display _newline

display as text "5. DOCUMENTATION IS ESSENTIAL"
display as text "   - Record svyset in all do-files"
display as text "   - Report DEFF in publications"
display as text "   - Include design description in metadata"
display _newline(2)

display as text "{hline 70}"
display as result "LAB 2.1 COMPLETED SUCCESSFULLY"
display as text "{hline 70}"


********************************************************************************
* CLOSE LOG AND CLEAN UP
********************************************************************************

log close

display _newline
display as text "Log file saved: lab2_1_survey_design.log"
display as text "Results file saved: lab2_1_results.xlsx"
display _newline


********************************************************************************
*                        END OF LAB 2.1 STATA SCRIPT                           *
********************************************************************************
