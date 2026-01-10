********************************************************************************
* LAB 3.3: VARIANCE ESTIMATION METHODS (OPTIMIZED)
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 3: Advanced Weighting and Variance Estimation
********************************************************************************
*
* OPTIMIZATIONS:
*   - Reduced bootstrap replicates (200 vs 500)
*   - Province analysis uses Taylor only
*   - Streamlined output
*
* AUTHOR: SADC Workshop Team
* DATE: March 2026
********************************************************************************

clear all
set more off
capture log close
log using "lab3_3_variance_estimation.log", replace

display _n
display "=" * 70
display "LAB 3.3: VARIANCE ESTIMATION METHODS"
display "=" * 70
display _n

********************************************************************************
* SECTION 1-2: DATA LOADING AND PREPARATION
********************************************************************************

display "Loading and preparing data..."

set seed 20260308

use "ghs-2024-hhold-v1.dta", clear

* Find income variable
local income_var ""
foreach v in totmhinc hhincome income totinc {
    capture confirm variable `v'
    if _rc == 0 {
        local income_var "`v'"
        continue, break
    }
}

gen income = `income_var'

* Keep complete cases only
keep if !missing(income) & !missing(psu) & !missing(stratum) & !missing(house_wgt)
keep if house_wgt > 0

display "Analysis sample: " _N " households"
quietly: tab psu
display "PSUs: " r(r)
quietly: tab stratum
display "Strata: " r(r)
display _n

* Set up survey design
svyset psu [pw=house_wgt], strata(stratum) singleunit(certainty)

********************************************************************************
* SECTION 3: TAYLOR SERIES LINEARIZATION
********************************************************************************

display "-" * 70
display "METHOD 1: TAYLOR SERIES LINEARIZATION"
display "-" * 70

timer clear 1
timer on 1

svy, vce(linearized): mean income

timer off 1
quietly: timer list 1
local time_taylor = r(t1)

matrix b = e(b)
matrix V = e(V)
local mean_taylor = b[1,1]
local se_taylor = sqrt(V[1,1])
local cv_taylor = (`se_taylor' / `mean_taylor') * 100
local ci_l_taylor = `mean_taylor' - 1.96 * `se_taylor'
local ci_u_taylor = `mean_taylor' + 1.96 * `se_taylor'

display _n
display "  Mean: R " %12.0fc `mean_taylor'
display "  SE: R " %12.0fc `se_taylor'
display "  CV: " %5.2f `cv_taylor' "%"
display "  95% CI: [" %12.0fc `ci_l_taylor' ", " %12.0fc `ci_u_taylor' "]"
display "  Time: " %6.3f `time_taylor' " sec"
display _n

********************************************************************************
* SECTION 4: JACKKNIFE REPLICATION
********************************************************************************

display "-" * 70
display "METHOD 2: JACKKNIFE REPLICATION"
display "-" * 70

timer clear 2
timer on 2

svy, vce(jackknife): mean income

timer off 2
quietly: timer list 2
local time_jk = r(t2)

matrix b = e(b)
matrix V = e(V)
local mean_jk = b[1,1]
local se_jk = sqrt(V[1,1])
local cv_jk = (`se_jk' / `mean_jk') * 100
local ci_l_jk = `mean_jk' - 1.96 * `se_jk'
local ci_u_jk = `mean_jk' + 1.96 * `se_jk'

display _n
display "  Mean: R " %12.0fc `mean_jk'
display "  SE: R " %12.0fc `se_jk'
display "  CV: " %5.2f `cv_jk' "%"
display "  95% CI: [" %12.0fc `ci_l_jk' ", " %12.0fc `ci_u_jk' "]"
display "  Time: " %6.3f `time_jk' " sec"
display _n

********************************************************************************
* SECTION 5: BOOTSTRAP REPLICATION
********************************************************************************

display "-" * 70
display "METHOD 3: BOOTSTRAP REPLICATION"
display "-" * 70

local n_boot = 200
display "  Replicates: `n_boot' (use 500 for production)"

timer clear 3
timer on 3

svy, vce(bootstrap, reps(`n_boot') seed(20260308)): mean income

timer off 3
quietly: timer list 3
local time_boot = r(t3)

matrix b = e(b)
matrix V = e(V)
local mean_boot = b[1,1]
local se_boot = sqrt(V[1,1])
local cv_boot = (`se_boot' / `mean_boot') * 100
local ci_l_boot = `mean_boot' - 1.96 * `se_boot'
local ci_u_boot = `mean_boot' + 1.96 * `se_boot'

display _n
display "  Mean: R " %12.0fc `mean_boot'
display "  SE: R " %12.0fc `se_boot'
display "  CV: " %5.2f `cv_boot' "%"
display "  95% CI: [" %12.0fc `ci_l_boot' ", " %12.0fc `ci_u_boot' "]"
display "  Time: " %6.3f `time_boot' " sec"
display _n

********************************************************************************
* SECTION 6: COMPARISON TABLE
********************************************************************************

display "=" * 70
display "COMPARISON TABLE: Mean Household Income"
display "=" * 70
display _n

* SE ratios
local se_ratio_jk = `se_jk' / `se_taylor'
local se_ratio_boot = `se_boot' / `se_taylor'

* CI widths
local ci_width_taylor = `ci_u_taylor' - `ci_l_taylor'
local ci_width_jk = `ci_u_jk' - `ci_l_jk'
local ci_width_boot = `ci_u_boot' - `ci_l_boot'

display _col(1) "Method" _col(24) "Mean" _col(38) "SE" _col(50) "CV" _col(58) "SE_Ratio" _col(70) "Time"
display "-" * 80

display _col(1) "Taylor Linearization" ///
        _col(20) %12.0fc `mean_taylor' ///
        _col(35) %9.0fc `se_taylor' ///
        _col(48) %5.2f `cv_taylor' "%" ///
        _col(58) %6.4f 1.0000 ///
        _col(68) %6.3f `time_taylor' "s"

display _col(1) "Jackknife (JK1)" ///
        _col(20) %12.0fc `mean_jk' ///
        _col(35) %9.0fc `se_jk' ///
        _col(48) %5.2f `cv_jk' "%" ///
        _col(58) %6.4f `se_ratio_jk' ///
        _col(68) %6.3f `time_jk' "s"

display _col(1) "Bootstrap (`n_boot' reps)" ///
        _col(20) %12.0fc `mean_boot' ///
        _col(35) %9.0fc `se_boot' ///
        _col(48) %5.2f `cv_boot' "%" ///
        _col(58) %6.4f `se_ratio_boot' ///
        _col(68) %6.3f `time_boot' "s"

display "-" * 80
display _n

********************************************************************************
* SECTION 7: INTERPRETATION
********************************************************************************

display "-" * 70
display "INTERPRETATION"
display "-" * 70
display _n

local se_diff_jk = (`se_jk' - `se_taylor') / `se_taylor' * 100
local se_diff_boot = (`se_boot' - `se_taylor') / `se_taylor' * 100

display "SE Comparison (vs Taylor):"
display "  Jackknife: " %+6.2f `se_diff_jk' "%"
display "  Bootstrap: " %+6.2f `se_diff_boot' "%"
display _n

display "Time Comparison:"
display "  Taylor: " %6.3f `time_taylor' "s (baseline)"
display "  Jackknife: " %6.3f `time_jk' "s (" %4.0f `time_jk'/`time_taylor' "x slower)"
display "  Bootstrap: " %6.3f `time_boot' "s (" %4.0f `time_boot'/`time_taylor' "x slower)"
display _n

********************************************************************************
* SECTION 8: CREATE RESULTS DATASET
********************************************************************************

display "-" * 70
display "SAVING RESULTS"
display "-" * 70
display _n

preserve

clear
set obs 3

gen str25 method = ""
replace method = "Taylor Linearization" in 1
replace method = "Jackknife (JK1)" in 2
replace method = "Bootstrap (`n_boot' reps)" in 3

gen double mean_income = .
replace mean_income = `mean_taylor' in 1
replace mean_income = `mean_jk' in 2
replace mean_income = `mean_boot' in 3

gen double se = .
replace se = `se_taylor' in 1
replace se = `se_jk' in 2
replace se = `se_boot' in 3

gen double cv = .
replace cv = `cv_taylor' in 1
replace cv = `cv_jk' in 2
replace cv = `cv_boot' in 3

gen double ci_lower = .
replace ci_lower = `ci_l_taylor' in 1
replace ci_lower = `ci_l_jk' in 2
replace ci_lower = `ci_l_boot' in 3

gen double ci_upper = .
replace ci_upper = `ci_u_taylor' in 1
replace ci_upper = `ci_u_jk' in 2
replace ci_upper = `ci_u_boot' in 3

gen double se_ratio = se / `se_taylor'

gen double time_sec = .
replace time_sec = `time_taylor' in 1
replace time_sec = `time_jk' in 2
replace time_sec = `time_boot' in 3

list, clean noobs

save "variance_methods_comparison.dta", replace
export delimited using "variance_methods_comparison.csv", replace

display "Saved: variance_methods_comparison.dta"
display "Saved: variance_methods_comparison.csv"

restore

********************************************************************************
* SECTION 9: VISUALIZATION
********************************************************************************

display _n
display "-" * 70
display "CREATING VISUALIZATIONS"
display "-" * 70
display _n

use "variance_methods_comparison.dta", clear
encode method, gen(method_num)

* CI Comparison
twoway (rcap ci_lower ci_upper method_num, horizontal lcolor(navy) lwidth(thick)) ///
       (scatter method_num mean_income, mcolor(navy) msize(large)), ///
       xlabel(, format(%12.0fc)) ///
       ylabel(1 "Taylor" 2 "Jackknife" 3 "Bootstrap", valuelabel angle(0)) ///
       ytitle("") xtitle("Mean Income (Rands)") ///
       title("Variance Estimation Methods Comparison") ///
       subtitle("Mean Household Income with 95% CI") ///
       legend(off) scheme(s2color)

graph export "variance_methods_comparison.png", replace width(1500) height(900)
display "Saved: variance_methods_comparison.png"

* SE Comparison
graph bar se, over(method, sort(1) label(angle(15))) ///
    blabel(bar, format(%9.0fc)) ///
    ytitle("Standard Error (Rands)") ///
    title("Standard Errors by Method") ///
    bar(1, color(navy)) scheme(s2color)

graph export "variance_se_comparison.png", replace width(1500) height(900)
display "Saved: variance_se_comparison.png"

* Time Comparison
graph bar time_sec, over(method, sort(1) label(angle(15))) ///
    blabel(bar, format(%6.2f)) ///
    ytitle("Time (seconds)") ///
    title("Computation Time by Method") ///
    bar(1, color(navy)) scheme(s2color)

graph export "variance_time_comparison.png", replace width(1500) height(900)
display "Saved: variance_time_comparison.png"
display _n

********************************************************************************
* SECTION 10: PROVINCE ANALYSIS (TAYLOR ONLY)
********************************************************************************

display "-" * 70
display "PROVINCE ANALYSIS (Taylor only for speed)"
display "-" * 70
display _n

use "ghs-2024-hhold-v1.dta", clear

* Recreate income and filter
local income_var ""
foreach v in totmhinc hhincome income totinc {
    capture confirm variable `v'
    if _rc == 0 {
        local income_var "`v'"
        continue, break
    }
}
gen income = `income_var'
keep if !missing(income) & !missing(psu) & !missing(stratum) & !missing(house_wgt) & house_wgt > 0

svyset psu [pw=house_wgt], strata(stratum) singleunit(certainty)

capture confirm variable prov
if _rc == 0 {
    svy: mean income, over(prov)
    
    * Save province results
    matrix prov_b = e(b)
    matrix prov_V = e(V)
    local n_prov = colsof(prov_b)
    
    display _n
    display _col(1) "Province" _col(20) "Mean" _col(35) "SE" _col(48) "CV"
    display "-" * 55
    
    forvalues i = 1/`n_prov' {
        local mean_p = prov_b[1, `i']
        local se_p = sqrt(prov_V[`i', `i'])
        local cv_p = (`se_p' / `mean_p') * 100
        display _col(1) "Province `i'" _col(17) %12.0fc `mean_p' _col(32) %9.0fc `se_p' _col(46) %5.2f `cv_p' "%"
    }
}
display _n

********************************************************************************
* SECTION 11: RECOMMENDATIONS
********************************************************************************

display "-" * 70
display "RECOMMENDATIONS"
display "-" * 70
display _n

display "1. TAYLOR (routine): Fast, accurate for means/totals/proportions"
display "2. JACKKNIFE (QA): Identifies influential PSUs, robust"
display "3. BOOTSTRAP (complex): Flexible, use for medians/ratios/Gini"
display _n

display "STATA SYNTAX:"
display "  Taylor:    svy, vce(linearized): mean varname"
display "  Jackknife: svy, vce(jackknife): mean varname"
display "  Bootstrap: svy, vce(bootstrap, reps(500)): mean varname"
display _n

display "=" * 70
display "LAB 3.3 COMPLETE"
display "=" * 70
display _n

log close

********************************************************************************
* END
********************************************************************************
