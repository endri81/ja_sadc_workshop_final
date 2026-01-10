*===============================================================================
* LAB 4.2: HOT-DECK IMPUTATION
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 4, Module 4.2: Error Mitigation and Quality Assurance
*===============================================================================
*
* OBJECTIVE: Compare imputation methods for handling missing income data
*   1. Introduce MAR missing values in income based on geotype
*   2. Mean Imputation (the "bad" way - distorts variance)
*   3. Hot-Deck Imputation (the "good" way - preserves distribution)
*   4. Compare distributions via density plots
*
* DATA: GHS 2024 Household File
* KEY VARIABLE: totmhinc (Total Monthly Household Income)
* DONOR CLASSES: Province × Household Size Group
*
* ZAMBARA NARRATIVE: Lindiwe addresses 24% missing income data
*
*===============================================================================

clear all
set more off
cap log close

* Start log file
log using "lab4_2_hotdeck_imputation.log", replace

display _n
display "================================================================"
display "  LAB 4.2: HOT-DECK IMPUTATION"
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
* - totmhinc: Total monthly household income
* - hholdsz: Household size
* - prov: Province (1-9)
* - geotype: Geography type (1=Urban, 2=Traditional, 3=Farms)
* - house_wgt: Household weight

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

* Geotype labels
label define geo_lbl 1 "Urban" 2 "Traditional" 3 "Farms"
label values geotype geo_lbl

* Check original income distribution
display "Original Income Distribution:"
sum totmhinc, detail

* Keep only valid income cases for this exercise
keep if totmhinc != . & totmhinc >= 0
local n_valid = _N
display _n "Records with valid income: `n_valid'" _n

* Store original statistics
qui sum totmhinc
local true_mean = r(mean)
local true_sd = r(sd)
local true_median = r(p50)
local true_n = r(N)

display "TRUE STATISTICS (Full Data):"
display "  N: `true_n'"
display "  Mean: R " %12.0fc `true_mean'
display "  Median: R " %12.0fc `true_median'
display "  Std Dev: R " %12.0fc `true_sd'
display ""

*-------------------------------------------------------------------------------
* 2. INTRODUCE MISSING AT RANDOM (MAR) MECHANISM
*-------------------------------------------------------------------------------

display _n "SECTION 2: Introducing Missing Values (MAR)"
display "-------------------------------------------" _n

* Set seed for reproducibility
set seed 20260305

* MAR mechanism: Missing probability depends on geotype
* Urban: 30% missing (privacy concerns)
* Traditional: 20% missing
* Farms: 15% missing

display "MAR Mechanism - Missing Probabilities by Geotype:"
display "  Urban: 30%"
display "  Traditional: 20%"
display "  Farms: 15%"
display ""

* Generate uniform random
gen rand = runiform()

* Assign missing probability based on geotype
gen missing_prob = .
replace missing_prob = 0.30 if geotype == 1  // Urban
replace missing_prob = 0.20 if geotype == 2  // Traditional
replace missing_prob = 0.15 if geotype == 3  // Farms

* Flag as missing
gen is_missing = (rand < missing_prob)
label var is_missing "Income flagged as missing"

* Store true income
gen income_true = totmhinc
label var income_true "True income (before making missing)"

* Create observed income (NA for missing)
gen income_observed = totmhinc
replace income_observed = . if is_missing == 1
label var income_observed "Observed income (with missing)"

* Summary of missing values
display "Missing Values by Geotype:"
tab geotype is_missing, row

* Total missing
qui count if is_missing == 1
local n_missing = r(N)
local pct_missing = round(100 * `n_missing' / `true_n', 0.1)
display _n "Total Missing: `n_missing' of `true_n' (`pct_missing'%)" _n

* Verify MAR: Compare observed vs missing
display "Verifying MAR - Comparing Observed vs Missing:"
display "Household Size by Missing Status:"
tabstat hholdsz, by(is_missing) stat(mean n)

display _n "Geotype Distribution by Missing Status:"
tab geotype is_missing, col

*-------------------------------------------------------------------------------
* 3. METHOD 1: MEAN IMPUTATION (THE BAD WAY)
*-------------------------------------------------------------------------------

display _n "SECTION 4: Mean Imputation (The Bad Way)"
display "----------------------------------------" _n

* Calculate overall mean from observed cases
qui sum income_observed
local mean_income = r(mean)

display "Imputation Value (Overall Mean): R " %12.0fc `mean_income' _n

* Perform mean imputation
gen income_mean_imp = income_observed
replace income_mean_imp = `mean_income' if is_missing == 1
label var income_mean_imp "Income after mean imputation"

* Post-imputation statistics
qui sum income_mean_imp
local meanimp_mean = r(mean)
local meanimp_sd = r(sd)
local meanimp_median = r(p50)

display "POST-MEAN IMPUTATION STATISTICS:"
display "  Mean: R " %12.0fc `meanimp_mean'
display "  Median: R " %12.0fc `meanimp_median'
display "  Std Dev: R " %12.0fc `meanimp_sd'
display ""

* Calculate distortion
local sd_change_mean = 100 * (`meanimp_sd' - `true_sd') / `true_sd'
local mean_bias = `meanimp_mean' - `true_mean'
local mean_bias_pct = 100 * `mean_bias' / `true_mean'

display "MEAN IMPUTATION PROBLEMS:"
display "  Mean Bias: R " %10.0fc `mean_bias' " (" %5.2f `mean_bias_pct' "%)"
display "  Std Dev Change: " %5.1f `sd_change_mean' "% (SHOULD BE ~0%)"
display "  -> Variance is UNDERESTIMATED!" _n

*-------------------------------------------------------------------------------
* 4. METHOD 2: HOT-DECK IMPUTATION (THE GOOD WAY)
*-------------------------------------------------------------------------------

display _n "SECTION 5: Hot-Deck Imputation (The Good Way)"
display "---------------------------------------------" _n

* Define imputation classes: Province × Household Size Group
* HH Size groups: 1, 2-3, 4-5, 6+

gen hhsize_grp = .
replace hhsize_grp = 1 if hholdsz == 1
replace hhsize_grp = 2 if hholdsz >= 2 & hholdsz <= 3
replace hhsize_grp = 3 if hholdsz >= 4 & hholdsz <= 5
replace hhsize_grp = 4 if hholdsz >= 6

label define hhsgrp_lbl 1 "1" 2 "2-3" 3 "4-5" 4 "6+"
label values hhsize_grp hhsgrp_lbl

* Create imputation class
egen imp_class = group(prov hhsize_grp)
label var imp_class "Imputation class (Province x HH Size)"

* Check class sizes
display "Imputation Classes (Province x HH Size):"
tab imp_class, m

* Count donors per class
bysort imp_class: egen n_donors = total(is_missing == 0)
bysort imp_class: egen n_recipients = total(is_missing == 1)

display _n "Class Summary:"
tabstat n_donors n_recipients, by(imp_class) stat(mean) nototal

* Check smallest classes
preserve
collapse (first) n_donors n_recipients, by(imp_class)
gsort n_donors
display _n "Smallest Imputation Classes (by number of donors):"
list in 1/10
restore

* Perform Hot-Deck Imputation
* For each missing case, randomly select a donor from same class

gen income_hotdeck = income_observed
label var income_hotdeck "Income after hot-deck imputation"

* Generate random order for donor selection
gen rand_donor = runiform()

* Sort within class by random order
gsort imp_class -is_missing rand_donor

* For each imputation class, assign donor values to recipients
display _n "Performing Hot-Deck Imputation..."

* Method: Within each class, cycle through donors for recipients
forvalues c = 1/`=r(max)' {
    * Get donor values in this class
    qui levelsof income_observed if imp_class == `c' & is_missing == 0, local(donors)
    local n_donors_c : word count `donors'
    
    if `n_donors_c' > 0 {
        * Count recipients in this class
        qui count if imp_class == `c' & is_missing == 1
        local n_recip = r(N)
        
        if `n_recip' > 0 {
            * For each recipient, randomly assign a donor value
            forvalues i = 1/`n_recip' {
                * Random donor index
                local donor_idx = ceil(runiform() * `n_donors_c')
                local donor_val : word `donor_idx' of `donors'
                
                * Find i-th recipient in this class and assign
                qui replace income_hotdeck = `donor_val' ///
                    if imp_class == `c' & is_missing == 1 & income_hotdeck == .
            }
        }
    }
}

* Alternative: Use STATA's hotdeck command if available
* cap which hotdeck
* if _rc == 0 {
*     hotdeck income_observed, by(imp_class) store(income_hotdeck2)
* }

* Check for any remaining missing after hot-deck
qui count if income_hotdeck == . & is_missing == 1
if r(N) > 0 {
    display "Warning: " r(N) " cases still missing after hot-deck"
    * Fallback: use class mean
    bysort imp_class: egen class_mean = mean(income_observed)
    replace income_hotdeck = class_mean if income_hotdeck == . & is_missing == 1
    drop class_mean
}

display "Hot-Deck Imputation Complete." _n

* Post-imputation statistics
qui sum income_hotdeck
local hotdeck_mean = r(mean)
local hotdeck_sd = r(sd)
local hotdeck_median = r(p50)

display "POST-HOT-DECK IMPUTATION STATISTICS:"
display "  Mean: R " %12.0fc `hotdeck_mean'
display "  Median: R " %12.0fc `hotdeck_median'
display "  Std Dev: R " %12.0fc `hotdeck_sd'
display ""

* Calculate performance
local sd_change_hd = 100 * (`hotdeck_sd' - `true_sd') / `true_sd'
local mean_bias_hd = `hotdeck_mean' - `true_mean'
local mean_bias_hd_pct = 100 * `mean_bias_hd' / `true_mean'

display "HOT-DECK PERFORMANCE:"
display "  Mean Bias: R " %10.0fc `mean_bias_hd' " (" %5.2f `mean_bias_hd_pct' "%)"
display "  Std Dev Change: " %5.1f `sd_change_hd' "% (SHOULD BE ~0%)"
display "  -> Variance is PRESERVED!" _n

*-------------------------------------------------------------------------------
* 5. COMPARISON TABLE
*-------------------------------------------------------------------------------

display _n "================================================================"
display "SECTION 6: Comparison of Methods"
display "================================================================" _n

display "COMPARISON TABLE:"
display "--------------------------------------------------------------------------------"
display "Method                 |    N    |    Mean (R)  |   Median (R) |  Std Dev (R) "
display "--------------------------------------------------------------------------------"
display "True (Full Data)       | " %6.0f `true_n' " | " %12.0fc `true_mean' " | " %12.0fc `true_median' " | " %11.0fc `true_sd'

qui sum income_observed
local obs_n = r(N)
local obs_mean = r(mean)
local obs_sd = r(sd)
local obs_median = r(p50)
display "Complete Cases Only    | " %6.0f `obs_n' " | " %12.0fc `obs_mean' " | " %12.0fc `obs_median' " | " %11.0fc `obs_sd'

display "Mean Imputation        | " %6.0f `true_n' " | " %12.0fc `meanimp_mean' " | " %12.0fc `meanimp_median' " | " %11.0fc `meanimp_sd'
display "Hot-Deck Imputation    | " %6.0f `true_n' " | " %12.0fc `hotdeck_mean' " | " %12.0fc `hotdeck_median' " | " %11.0fc `hotdeck_sd'
display "--------------------------------------------------------------------------------"
display ""

display "BIAS AND DISTORTION:"
display "--------------------------------------------------------------------------------"
display "Method                 | Mean Bias (R) | Mean Bias % | SD Change %"
display "--------------------------------------------------------------------------------"
display "True (Full Data)       |             0 |        0.00 |        0.0"
local obs_bias = `obs_mean' - `true_mean'
local obs_bias_pct = 100 * `obs_bias' / `true_mean'
local obs_sd_chg = 100 * (`obs_sd' - `true_sd') / `true_sd'
display "Complete Cases Only    | " %13.0fc `obs_bias' " | " %11.2f `obs_bias_pct' " | " %10.1f `obs_sd_chg'
display "Mean Imputation        | " %13.0fc `mean_bias' " | " %11.2f `mean_bias_pct' " | " %10.1f `sd_change_mean'
display "Hot-Deck Imputation    | " %13.0fc `mean_bias_hd' " | " %11.2f `mean_bias_hd_pct' " | " %10.1f `sd_change_hd'
display "--------------------------------------------------------------------------------"

*-------------------------------------------------------------------------------
* 6. DENSITY PLOTS
*-------------------------------------------------------------------------------

display _n "SECTION 7: Creating Density Plots"
display "---------------------------------" _n

* Calculate income cap for visualization (99th percentile)
qui sum income_true, detail
local income_cap = r(p99)

* Combined density plot
twoway (kdensity income_true if income_true <= `income_cap', ///
            lcolor(navy) lwidth(medthick) lpattern(solid)) ///
       (kdensity income_mean_imp if income_mean_imp <= `income_cap', ///
            lcolor(cranberry) lwidth(medthick) lpattern(dash)) ///
       (kdensity income_hotdeck if income_hotdeck <= `income_cap', ///
            lcolor(forest_green) lwidth(medthick) lpattern(dash_dot)), ///
    title("Income Distribution: Original vs Imputation Methods") ///
    subtitle("MAR Missing Mechanism: `pct_missing'% Missing") ///
    xtitle("Monthly Household Income (R)") ytitle("Density") ///
    legend(order(1 "Original (True)" 2 "Mean Imputation" 3 "Hot-Deck") ///
           rows(1) position(6)) ///
    scheme(s2color) ///
    note("Source: Lab 4.2 - GHS 2024 Simulation")

graph export "lab4_2_density_comparison.png", replace width(1000) height(600)
display "Saved: lab4_2_density_comparison.png"

* Faceted density plots
graph twoway (kdensity income_true if income_true <= `income_cap', ///
    lcolor(navy) lwidth(medthick)), ///
    title("Original (True)") xtitle("") ytitle("Density") ///
    name(g1, replace) nodraw

graph twoway (kdensity income_mean_imp if income_mean_imp <= `income_cap', ///
    lcolor(cranberry) lwidth(medthick)), ///
    title("Mean Imputation") xtitle("") ytitle("Density") ///
    name(g2, replace) nodraw

graph twoway (kdensity income_hotdeck if income_hotdeck <= `income_cap', ///
    lcolor(forest_green) lwidth(medthick)), ///
    title("Hot-Deck Imputation") xtitle("Income (R)") ytitle("Density") ///
    name(g3, replace) nodraw

graph combine g1 g2 g3, cols(1) ///
    title("Income Distributions by Imputation Method") ///
    note("Source: Lab 4.2")

graph export "lab4_2_density_facet.png", replace width(800) height(900)
display "Saved: lab4_2_density_facet.png"

* Zoomed plot showing mean imputation spike
local zoom_min = `mean_income' - 15000
local zoom_max = `mean_income' + 15000

twoway (kdensity income_true if income_true >= `zoom_min' & income_true <= `zoom_max', ///
            lcolor(navy) lwidth(medthick) lpattern(solid) bwidth(2000)) ///
       (kdensity income_mean_imp if income_mean_imp >= `zoom_min' & income_mean_imp <= `zoom_max', ///
            lcolor(cranberry) lwidth(medthick) lpattern(dash) bwidth(2000)) ///
       (kdensity income_hotdeck if income_hotdeck >= `zoom_min' & income_hotdeck <= `zoom_max', ///
            lcolor(forest_green) lwidth(medthick) lpattern(dash_dot) bwidth(2000)), ///
    xline(`mean_income', lcolor(cranberry) lpattern(dot)) ///
    title("Zoomed View: Mean Imputation Creates Artificial Spike") ///
    subtitle("Notice the spike at the imputed mean value") ///
    xtitle("Monthly Household Income (R)") ytitle("Density") ///
    legend(order(1 "Original" 2 "Mean Imp." 3 "Hot-Deck") rows(1) position(6)) ///
    note("Vertical line = Mean imputation value (R `=round(`mean_income')')")

graph export "lab4_2_density_zoom.png", replace width(1000) height(600)
display "Saved: lab4_2_density_zoom.png" _n

*-------------------------------------------------------------------------------
* 7. ADDITIONAL DIAGNOSTICS
*-------------------------------------------------------------------------------

display _n "SECTION 8: Additional Diagnostics"
display "----------------------------------" _n

* Check imputed values only
display "IMPUTED VALUES ONLY (for missing cases):"
preserve
keep if is_missing == 1

qui sum income_true
local imp_true_mean = r(mean)
local imp_true_sd = r(sd)

qui sum income_mean_imp
local imp_meanimp_mean = r(mean)
local imp_meanimp_sd = r(sd)

qui sum income_hotdeck
local imp_hd_mean = r(mean)
local imp_hd_sd = r(sd)

display "  N imputed: " _N
display "  True Mean (hidden): R " %12.0fc `imp_true_mean'
display "  Mean Imp Value:     R " %12.0fc `imp_meanimp_mean'
display "  Hot-Deck Mean:      R " %12.0fc `imp_hd_mean'
display ""
display "  True SD:            R " %12.0fc `imp_true_sd'
display "  Mean Imp SD:        R " %12.0fc `imp_meanimp_sd' " (All same value!)"
display "  Hot-Deck SD:        R " %12.0fc `imp_hd_sd'

restore

*-------------------------------------------------------------------------------
* 8. SUMMARY AND KEY TAKEAWAYS
*-------------------------------------------------------------------------------

display _n "================================================================"
display "SECTION 9: Summary and Key Takeaways"
display "================================================================" _n

display "KEY FINDINGS:"
display "-------------" _n

display "1. MEAN IMPUTATION PROBLEMS:"
display "   - Underestimates variance by " %5.1f abs(`sd_change_mean') "%"
display "   - Creates artificial spike at imputed value"
display "   - All imputed values are identical"
display "   - Distorts distribution shape" _n

display "2. HOT-DECK ADVANTAGES:"
display "   - Preserves variance (change: " %5.1f `sd_change_hd' "%)"
display "   - Uses real observed values as donors"
display "   - Maintains distribution shape"
display "   - Imputed values have natural variability" _n

display "3. RECOMMENDATIONS:"
display "   - NEVER use mean imputation for continuous variables"
display "   - Hot-deck is appropriate for item non-response"
display "   - Choose imputation classes based on predictive variables"
display "   - Document imputation method in survey metadata"
display "   - Consider multiple imputation for variance estimation" _n

*-------------------------------------------------------------------------------
* 9. EXPORT RESULTS
*-------------------------------------------------------------------------------

display "SECTION 10: Exporting Results"
display "-----------------------------" _n

* Create comparison dataset
preserve
clear
input str25 method n mean median sd mean_bias mean_bias_pct sd_change_pct
"True (Full Data)" `true_n' `true_mean' `true_median' `true_sd' 0 0 0
"Complete Cases" `obs_n' `obs_mean' `obs_median' `obs_sd' `obs_bias' `obs_bias_pct' `obs_sd_chg'
"Mean Imputation" `true_n' `meanimp_mean' `meanimp_median' `meanimp_sd' `mean_bias' `mean_bias_pct' `sd_change_mean'
"Hot-Deck Imputation" `true_n' `hotdeck_mean' `hotdeck_median' `hotdeck_sd' `mean_bias_hd' `mean_bias_hd_pct' `sd_change_hd'
end

export delimited using "lab4_2_imputation_comparison.csv", replace
display "Saved: lab4_2_imputation_comparison.csv"
restore

* Save slide statistics
preserve
clear
input str25 metric value
"N Total" `true_n'
"N Missing" `n_missing'
"Pct Missing" `pct_missing'
"True Mean" `true_mean'
"True SD" `true_sd'
"Mean Imp Mean" `meanimp_mean'
"Mean Imp SD" `meanimp_sd'
"Mean Imp SD Change" `sd_change_mean'
"HotDeck Mean" `hotdeck_mean'
"HotDeck SD" `hotdeck_sd'
"HotDeck SD Change" `sd_change_hd'
end

export delimited using "lab4_2_slide_statistics.csv", replace
display "Saved: lab4_2_slide_statistics.csv"
restore

display _n "================================================================"
display "  LAB 4.2 COMPLETE"
display "================================================================"

log close

*===============================================================================
* END OF LAB 4.2
*===============================================================================
