*===============================================================================
* LAB 4.1: RESPONSE RATE ANALYSIS
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 4: Advanced Error Mitigation and Quality Assurance
*===============================================================================
*
* LEARNING OBJECTIVES:
* 1. Understand and simulate realistic non-response patterns
* 2. Calculate AAPOR Standard Response Rates (RR1)
* 3. Analyze response rates by domain (Province, Geotype)
* 4. Diagnose response bias using auxiliary variables
*
* DATA: Statistics South Africa General Household Survey 2024
*       Household file: ghs-2024-hhold-v1.dta
*
* ZAMBARA NARRATIVE:
* Lindiwe examines the field monitoring dashboard and discovers concerning
* non-response patterns. Before she can address the problem, she needs to
* quantify the extent and distribution of non-response across the country.
*
* REFERENCE:
* AAPOR (2023). Standard Definitions: Final Dispositions of Case Codes and 
*               Outcome Rates for Surveys. 10th edition.
* Groves, R.M. et al. (2009). Survey Methodology. 2nd ed. Wiley.
*
*===============================================================================

clear all
set more off
cap log close

* Start log file
log using "lab4_1_response_rate_analysis.log", replace

display _newline
display as text "=" * 70
display as result "LAB 4.1: RESPONSE RATE ANALYSIS"
display as text "SADC Advanced Sampling Workshop - Day 4"
display as text "=" * 70
display _newline

*-------------------------------------------------------------------------------
* SECTION 1: SET WORKING DIRECTORY AND LOAD DATA
*-------------------------------------------------------------------------------

display as text _newline "SECTION 1: Loading GHS 2024 Household Data"
display as text "-" * 50

* Set working directory (adjust as needed)
* cd "your/working/directory"

* Set seed for reproducibility
set seed 20260305  // Day 4 date: 5 March 2026

* Try to load actual data, if not available create synthetic data
capture confirm file "ghs-2024-hhold-v1.dta"

if _rc != 0 {
    display as text _newline "NOTE: Data file not found."
    display as text "Creating synthetic demonstration data..."
    display _newline
    
    * Create synthetic data matching GHS 2024 structure
    clear
    set obs 20940  // Approximate GHS 2024 household sample size
    
    * Generate household ID
    gen hhid = _n
    
    * Generate PSU (approximately 3,218 PSUs)
    gen psu = ceil(runiform() * 3218)
    
    * Generate stratum
    gen stratum = floor(runiform() * 248) + 10101
    
    * Generate province with realistic distribution
    * WC=1, EC=2, NC=3, FS=4, KZN=5, NW=6, GP=7, MP=8, LP=9
    gen double rand_prov = runiform()
    gen prov = 1 if rand_prov < 0.08
    replace prov = 2 if rand_prov >= 0.08 & rand_prov < 0.20
    replace prov = 3 if rand_prov >= 0.20 & rand_prov < 0.22
    replace prov = 4 if rand_prov >= 0.22 & rand_prov < 0.27
    replace prov = 5 if rand_prov >= 0.27 & rand_prov < 0.43
    replace prov = 6 if rand_prov >= 0.43 & rand_prov < 0.49
    replace prov = 7 if rand_prov >= 0.49 & rand_prov < 0.76
    replace prov = 8 if rand_prov >= 0.76 & rand_prov < 0.83
    replace prov = 9 if rand_prov >= 0.83
    drop rand_prov
    
    * Generate geotype (Urban=1, Traditional=2, Farms=3)
    gen double rand_geo = runiform()
    gen geotype = 1 if rand_geo < 0.65
    replace geotype = 2 if rand_geo >= 0.65 & rand_geo < 0.93
    replace geotype = 3 if rand_geo >= 0.93
    drop rand_geo
    
    * Generate household weight
    gen house_wgt = 200 + runiform() * 4800
    
    * Generate household size (mean ~3.2)
    gen hholdsz = max(1, round(rnormal(3.2, 1.8)))
    
    * Generate total monthly household income
    gen totmhinc = max(0, rnormal(8500, 12000))
}
else {
    * Load actual GHS data
    use "ghs-2024-hhold-v1.dta", clear
}

* Create province labels
label define prov_lbl 1 "Western Cape" 2 "Eastern Cape" 3 "Northern Cape" ///
    4 "Free State" 5 "KwaZulu-Natal" 6 "North West" 7 "Gauteng" ///
    8 "Mpumalanga" 9 "Limpopo"
label values prov prov_lbl

* Create geotype labels  
label define geo_lbl 1 "Urban" 2 "Traditional" 3 "Farms"
label values geotype geo_lbl

* Display data structure
display as text _newline "Data Structure:"
describe, short
display as text "  Observations: " _N
codebook psu, compact
codebook stratum, compact

*-------------------------------------------------------------------------------
* SECTION 2: SIMULATE REALISTIC NON-RESPONSE PATTERNS
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 2: Simulating Non-Response Patterns"
display as text "-" * 50

* EXPLANATION OF NON-RESPONSE SIMULATION:
* We create realistic non-response that correlates with observable characteristics:
*
* 1. URBAN AREAS: Higher refusal rates (privacy concerns, busy schedules)
* 2. FARMS: Higher non-contact rates (dispersed, difficult access)
* 3. SMALL HOUSEHOLDS: Higher non-contact (fewer people home)
* 4. CERTAIN PROVINCES: Access challenges (e.g., Northern Cape - vast, sparse)
*
* This creates non-response that is NOT Missing Completely at Random (MCAR)
* but rather Missing at Random (MAR) conditional on observables.

* Create non-response propensity components
gen urban_effect = (geotype == 1) * 0.08
gen farm_effect = (geotype == 3) * 0.12
gen small_hh_effect = (hholdsz <= 2) * 0.06

* Province effects
gen prov_effect = 0
replace prov_effect = 0.05 if prov == 3  // Northern Cape - vast distances
replace prov_effect = 0.04 if prov == 2  // Eastern Cape - rural access
replace prov_effect = 0.03 if prov == 9  // Limpopo - traditional areas
replace prov_effect = 0.02 if prov == 8  // Mpumalanga

* Combined non-response propensity
gen nr_propensity = urban_effect + farm_effect + small_hh_effect + prov_effect

* Random component for assignment
gen double random_u = runiform()

* Assign response status based on propensity
* 1 = Complete Interview, 2 = Refusal, 3 = Non-Contact
* Target overall response rate: approximately 85%

gen response_status = .

* Complete interviews (higher probability if low non-response propensity)
replace response_status = 1 if random_u > (0.15 + nr_propensity)

* Refusals (more common in urban areas)
replace response_status = 2 if response_status == . & ///
    random_u > (0.08 + nr_propensity * 0.3) & geotype == 1
replace response_status = 2 if response_status == . & ///
    random_u > (0.05 + nr_propensity * 0.3)

* Non-contact (remainder)
replace response_status = 3 if response_status == .

* Label response status
label define resp_lbl 1 "Complete" 2 "Refusal" 3 "Non-Contact"
label values response_status resp_lbl

* Display response status distribution
display as text _newline "Simulated Response Status Distribution:"
tab response_status

*-------------------------------------------------------------------------------
* SECTION 3: CALCULATE AAPOR RESPONSE RATE 1 (RR1)
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 3: AAPOR Response Rate Calculations"
display as text "-" * 50

* AAPOR RESPONSE RATE 1 (RR1) DEFINITION:
* RR1 = I / (I + P + R + NC + O + UH + UO)
*
* Simplified for this exercise:
* RR1 = Complete / (Complete + Refusal + Non-Contact)

* Count response outcomes
count if response_status == 1
local n_complete = r(N)
count if response_status == 2
local n_refusal = r(N)
count if response_status == 3
local n_noncontact = r(N)
local n_total = _N

* Calculate overall RR1
local overall_rr1 = (`n_complete' / `n_total') * 100

display as text _newline "Overall AAPOR Response Rate 1 (RR1):"
display as result "  RR1 = " %5.1f `overall_rr1' "%"
display as text "  Complete interviews: " %8.0fc `n_complete'
display as text "  Refusals:           " %8.0fc `n_refusal'
display as text "  Non-contacts:       " %8.0fc `n_noncontact'
display as text "  Total sample:       " %8.0fc `n_total'

*-------------------------------------------------------------------------------
* SECTION 4: RESPONSE RATES BY PROVINCE
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 4: Response Rates by Province"
display as text "-" * 50

* Create response rate table by province
preserve

collapse (count) n_total=hhid ///
    (sum) n_complete = (response_status == 1) ///
    (sum) n_refusal = (response_status == 2) ///
    (sum) n_noncontact = (response_status == 3), by(prov)

* Calculate rates
gen rr1 = (n_complete / n_total) * 100
gen refusal_rate = (n_refusal / n_total) * 100
gen noncontact_rate = (n_noncontact / n_total) * 100

* Sort by RR1
sort rr1

* Display results
display as text _newline "Response Rates by Province (sorted by RR1):"
display as text _newline "Province         Total  Complete    RR1%  Refusal%    NC%"
display as text "-" * 60

forvalues i = 1/`=_N' {
    local prov_val = prov[`i']
    local prov_name: label prov_lbl `prov_val'
    local n_t = n_total[`i']
    local n_c = n_complete[`i']
    local rr = rr1[`i']
    local ref = refusal_rate[`i']
    local nc = noncontact_rate[`i']
    
    display as text "%-15s" "`prov_name'" %8.0f `n_t' %10.0f `n_c' ///
        %8.1f `rr' %10.1f `ref' %8.1f `nc'
}

* Save province results
save "lab4_1_rr_by_province.dta", replace

* Identify provinces with low response rates
display as text _newline "⚠️  ALERT: Provinces with RR1 < 82%:"
list prov rr1 if rr1 < 82, noobs clean

restore

*-------------------------------------------------------------------------------
* SECTION 5: RESPONSE RATES BY GEOTYPE
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 5: Response Rates by Geography Type"
display as text "-" * 50

* Create response rate table by geotype
preserve

collapse (count) n_total=hhid ///
    (sum) n_complete = (response_status == 1) ///
    (sum) n_refusal = (response_status == 2) ///
    (sum) n_noncontact = (response_status == 3), by(geotype)

* Calculate rates
gen rr1 = (n_complete / n_total) * 100
gen refusal_rate = (n_refusal / n_total) * 100
gen noncontact_rate = (n_noncontact / n_total) * 100

* Display results
display as text _newline "Response Rates by Geography Type:"
display as text _newline "Geotype      Total  Complete    RR1%  Refusal%    NC%"
display as text "-" * 55

forvalues i = 1/`=_N' {
    local geo_val = geotype[`i']
    local geo_name: label geo_lbl `geo_val'
    local n_t = n_total[`i']
    local n_c = n_complete[`i']
    local rr = rr1[`i']
    local ref = refusal_rate[`i']
    local nc = noncontact_rate[`i']
    
    display as text "%-12s" "`geo_name'" %8.0f `n_t' %10.0f `n_c' ///
        %8.1f `rr' %10.1f `ref' %8.1f `nc'
}

* Save geotype results
save "lab4_1_rr_by_geotype.dta", replace

* Key insight
display as text _newline "📊 KEY INSIGHT:"
sum refusal_rate if geotype == 1
local urban_ref = r(mean)
sum refusal_rate if geotype == 2
local trad_ref = r(mean)
sum noncontact_rate if geotype == 3
local farm_nc = r(mean)

display as text "   Urban refusal rate (" %4.1f `urban_ref' "%) is higher than " ///
    "Traditional (" %4.1f `trad_ref' "%)"
display as text "   Farm non-contact rate (" %4.1f `farm_nc' "%) is highest"

restore

*-------------------------------------------------------------------------------
* SECTION 6: CROSS-TABULATION (PROVINCE × GEOTYPE)
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 6: Response Rates by Province × Geotype"
display as text "-" * 50

* Calculate RR1 for each cell
preserve

collapse (count) n_total=hhid ///
    (sum) n_complete = (response_status == 1), by(prov geotype)

gen rr1 = (n_complete / n_total) * 100

* Display cross-tab
display as text _newline "RR1 (%) by Province and Geotype:"
tabdisp prov geotype, cell(rr1) format(%5.1f)

* Identify problem cells (RR1 < 80%)
display as text _newline "⚠️  CELLS WITH RR1 < 80% (n >= 50):"
list prov geotype rr1 n_total if rr1 < 80 & n_total >= 50, noobs clean

restore

*-------------------------------------------------------------------------------
* SECTION 7: RESPONSE BIAS ANALYSIS
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 7: Response Bias Analysis"
display as text "-" * 50

* RESPONSE BIAS CONCEPT:
* If respondents differ systematically from non-respondents on key variables,
* survey estimates will be biased. We compare mean household size.

* Create respondent indicator
gen respondent = (response_status == 1)
label define resp_ind 0 "Non-Respondent" 1 "Respondent"
label values respondent resp_ind

* Calculate mean household size by response status
tabstat hholdsz, by(respondent) stats(n mean sd se) nototal

* Store values for bias calculation
sum hholdsz if respondent == 1
local mean_resp = r(mean)
local n_resp = r(N)
local se_resp = r(sd) / sqrt(r(N))

sum hholdsz if respondent == 0
local mean_nonresp = r(mean)
local n_nonresp = r(N)
local se_nonresp = r(sd) / sqrt(r(N))

sum hholdsz
local mean_all = r(mean)

* T-test for difference
ttest hholdsz, by(respondent)
local t_stat = r(t)
local p_value = r(p)

* Display results
display as text _newline "Household Size Comparison:"
display as text _newline "  Respondents (n=" %8.0fc `n_resp' "):"
display as text "    Mean household size: " %6.3f `mean_resp' " (SE: " %6.4f `se_resp' ")"

display as text _newline "  Non-Respondents (n=" %8.0fc `n_nonresp' "):"
display as text "    Mean household size: " %6.3f `mean_nonresp' " (SE: " %6.4f `se_nonresp' ")"

local diff = `mean_resp' - `mean_nonresp'
display as text _newline "  Difference: " %6.3f `diff'
display as text "  t-statistic: " %6.3f `t_stat'
display as text "  p-value: " %6.4f `p_value'

* Interpretation
display as text _newline "📊 INTERPRETATION:"
if `p_value' < 0.05 {
    display as result "   ⚠️  SIGNIFICANT DIFFERENCE DETECTED"
    display as text "   Respondents and non-respondents differ significantly on household size."
    if `mean_resp' > `mean_nonresp' {
        display as text "   Non-respondents tend to be SMALLER households."
        display as text "   This suggests potential UNDERCOVERAGE of small households."
    }
    else {
        display as text "   Non-respondents tend to be LARGER households."
        display as text "   This suggests potential UNDERCOVERAGE of large households."
    }
}
else {
    display as text "   ✓ No significant difference detected (p > 0.05)"
}

* Non-response bias formula
* Bias = (1 - RR) × (Y_R - Y_NR)
local rr = `n_resp' / (`n_resp' + `n_nonresp')
local bias_est = (1 - `rr') * (`mean_resp' - `mean_nonresp')
local rel_bias = (`bias_est' / `mean_all') * 100

display as text _newline "  Estimated Non-Response Bias in Mean HH Size: " %6.4f `bias_est'
display as text "  Relative Bias: " %5.2f `rel_bias' "%"

*-------------------------------------------------------------------------------
* SECTION 8: VISUALIZATION - RESPONSE BIAS
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 8: Creating Visualizations"
display as text "-" * 50

* Graph 1: Histogram of household size by response status
histogram hholdsz, by(response_status, cols(1) note("")) ///
    freq ///
    title("Household Size Distribution by Response Status") ///
    subtitle("Lab 4.1: Response Bias Visualization") ///
    xtitle("Household Size (persons)") ///
    ytitle("Frequency") ///
    color(navy%70) ///
    name(g1, replace)

graph export "lab4_1_response_bias_histogram.png", replace width(1000)

* Graph 2: Response rate by province (horizontal bar)
preserve
use "lab4_1_rr_by_province.dta", clear
sort rr1
gen order = _n

graph hbar rr1, over(prov, sort(order)) ///
    title("AAPOR RR1 by Province") ///
    subtitle("Dashed line = 85% target response rate") ///
    ytitle("Response Rate (%)") ///
    yline(85, lcolor(red) lpattern(dash)) ///
    blabel(bar, format(%4.1f)) ///
    bar(1, color(navy%80)) ///
    name(g2, replace)

graph export "lab4_1_rr1_by_province.png", replace width(800)
restore

* Graph 3: Stacked bar of response outcomes by geotype
preserve
gen count = 1
collapse (sum) count, by(geotype response_status)
bysort geotype: egen total = sum(count)
gen pct = (count / total) * 100

reshape wide pct count, i(geotype) j(response_status)

graph bar pct1 pct2 pct3, over(geotype) stack ///
    title("Response Outcomes by Geography Type") ///
    ytitle("Percentage") ///
    legend(order(1 "Complete" 2 "Refusal" 3 "Non-Contact") rows(1)) ///
    bar(1, color(green%80)) ///
    bar(2, color(red%80)) ///
    bar(3, color(orange%80)) ///
    name(g3, replace)

graph export "lab4_1_response_by_geotype.png", replace width(800)
restore

* Graph 4: Mean household size comparison
preserve
collapse (mean) mean_hh = hholdsz (sd) sd_hh = hholdsz (count) n = hholdsz, ///
    by(respondent)

gen se_hh = sd_hh / sqrt(n)
gen ci_lo = mean_hh - 1.96 * se_hh
gen ci_hi = mean_hh + 1.96 * se_hh

graph bar mean_hh, over(respondent) ///
    title("Mean Household Size: Respondents vs Non-Respondents") ///
    subtitle("Error bars show 95% confidence intervals") ///
    ytitle("Mean Household Size") ///
    blabel(bar, format(%5.2f)) ///
    bar(1, color(navy%80)) ///
    name(g4, replace)

* Note: Adding CI requires a twoway graph approach
graph export "lab4_1_bias_comparison.png", replace width(800)
restore

display as text _newline "Plots saved:"
display as text "  - lab4_1_response_bias_histogram.png"
display as text "  - lab4_1_rr1_by_province.png"
display as text "  - lab4_1_response_by_geotype.png"
display as text "  - lab4_1_bias_comparison.png"

*-------------------------------------------------------------------------------
* SECTION 9: SUMMARY OUTPUT TABLE FOR SLIDES
*-------------------------------------------------------------------------------

display as text _newline(2) "SECTION 9: Summary Tables for Presentation"
display as text "-" * 50

* Create summary dataset
preserve

* National level summary
clear
set obs 1
gen domain = "NATIONAL"
gen n_total = `n_total'
gen n_complete = `n_complete'
gen rr1 = `overall_rr1'
gen refusal_pct = (`n_refusal' / `n_total') * 100
gen noncontact_pct = (`n_noncontact' / `n_total') * 100

* Append province summaries
append using "lab4_1_rr_by_province.dta"

* Clean up for export
decode prov, gen(prov_name)
replace domain = prov_name if domain == ""
drop prov prov_name

* Display and export
display as text _newline "=== TABLE FOR SLIDES: Response Rates Summary ==="
list domain n_total n_complete rr1 refusal_rate noncontact_rate, noobs clean

export delimited using "lab4_1_response_rates_summary.csv", replace

display as text _newline "Table exported to: lab4_1_response_rates_summary.csv"

restore

*-------------------------------------------------------------------------------
* SECTION 10: KEY FINDINGS FOR ZAMBARA NARRATIVE
*-------------------------------------------------------------------------------

display as text _newline(2)
display as text "=" * 70
display as result "KEY FINDINGS FOR LINDIWE'S REPORT"
display as text "=" * 70

display as text _newline "📋 ZAMBARA HOUSEHOLD LIVING CONDITIONS SURVEY - RESPONSE ANALYSIS"

display as text _newline "1. OVERALL RESPONSE RATE:"
display as text "   AAPOR RR1 = " %5.1f `overall_rr1' "%"
if `overall_rr1' >= 85 {
    display as text "   Status: ACCEPTABLE"
}
else if `overall_rr1' >= 80 {
    display as text "   Status: MARGINAL"
}
else {
    display as text "   Status: CONCERNING"
}

display as text _newline "2. GEOGRAPHIC PATTERNS:"
* Load province results to find lowest
preserve
use "lab4_1_rr_by_province.dta", clear
sort rr1
local lowest_prov = prov[1]
local lowest_prov_name: label prov_lbl `lowest_prov'
local lowest_rr = rr1[1]
restore

preserve
use "lab4_1_rr_by_geotype.dta", clear
sum refusal_rate if geotype == 1
local urban_ref = r(mean)
sum noncontact_rate if geotype == 3
local farm_nc = r(mean)
restore

display as text "   Lowest response: `lowest_prov_name' (" %4.1f `lowest_rr' "%)"
display as text "   Highest refusal: Urban areas (" %4.1f `urban_ref' "%)"
display as text "   Highest non-contact: Farms (" %4.1f `farm_nc' "%)"

display as text _newline "3. RESPONSE BIAS EVIDENCE:"
display as text "   Mean HH size (respondents): " %5.2f `mean_resp'
display as text "   Mean HH size (non-respondents): " %5.2f `mean_nonresp'
if `p_value' < 0.05 {
    display as text "   Difference: SIGNIFICANT (p = " %6.4f `p_value' ")"
}
else {
    display as text "   Difference: Not significant (p = " %6.4f `p_value' ")"
}

display as text _newline "4. RECOMMENDED ACTIONS:"
display as text "   a) Increase callback attempts in Farm areas"
display as text "   b) Deploy refusal conversion protocols in Urban areas"
display as text "   c) Consider non-response weight adjustments"
display as text "   d) Document bias direction for reporting"

display as text _newline
display as text "=" * 70
display as text "END OF LAB 4.1"
display as text "=" * 70

* Close log
log close

*===============================================================================
* END OF DO-FILE
*===============================================================================
