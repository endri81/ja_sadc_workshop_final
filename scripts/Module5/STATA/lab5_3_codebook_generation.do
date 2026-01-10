* =============================================================================
* LAB 5.3: AUTOMATED CODEBOOK GENERATION
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 5: Documentation Standards
* =============================================================================
*
* OBJECTIVES:
*   1. Generate a comprehensive codebook with variable metadata
*   2. Export metadata in standard formats (Markdown and Excel)
*   3. Visualize variable "health" (missingness patterns)
*
* DATA: Statistics South Africa General Household Survey 2024 (Household File)
* FILE: ghs-2024-hhold-v1.dta
*
* =============================================================================

clear all
set more off
capture log close

* Start log file
log using "lab5_3_codebook_generation.log", replace text

display as text _n
display as text "========================================================================"
display as text "LAB 5.3: AUTOMATED CODEBOOK GENERATION"
display as text "========================================================================"
display as text _n

* -----------------------------------------------------------------------------
* SECTION 1: LOAD DATA
* -----------------------------------------------------------------------------

display as text "SECTION 1: Loading GHS 2024 Household Data"
display as text "--------------------------------------------------"
display as text _n

* Load the dataset
use "ghs-2024-hhold-v1.dta", clear

* Store basic dataset information
local n_obs = _N
local n_vars : word count `r(varlist)'
qui describe, short
local n_vars = r(k)

display as text "Dataset loaded successfully!"
display as text "  Observations: " %12.0fc `n_obs'
display as text "  Variables: " `n_vars'
display as text _n

* -----------------------------------------------------------------------------
* SECTION 2: EXTRACT VARIABLE METADATA
* -----------------------------------------------------------------------------

display as text "SECTION 2: Extracting Variable Metadata"
display as text "--------------------------------------------------"
display as text _n

* Get list of all variables
qui describe, varlist
local allvars `r(varlist)'

* Create a temporary dataset to store codebook information
preserve

* Count number of variables
local nvars : word count `allvars'
display as text "Building codebook metadata for `nvars' variables..."

* Create empty dataset for codebook
clear
set obs `nvars'

* Generate variable position
gen position = _n

* Initialize string variables with appropriate lengths
gen str32 varname = ""
gen str200 varlabel = ""
gen str20 vartype = ""
gen str15 stata_format = ""
gen double n_obs = `n_obs'
gen double n_missing = .
gen double n_valid = .
gen double pct_missing = .
gen long n_unique = .
gen str200 value_labels_sample = ""
gen double min_value = .
gen double max_value = .
gen double mean_value = .

* Save the empty codebook frame
tempfile codebook
save `codebook', replace

* Restore original data to extract metadata
restore

* Loop through each variable and extract metadata
local i = 1
foreach var of varlist * {
    
    * Get variable name
    local vname "`var'"
    
    * Get variable label
    local vlabel : variable label `var'
    if "`vlabel'" == "" local vlabel "(no label)"
    
    * Get storage type
    local vtype : type `var'
    
    * Get display format
    local vformat : format `var'
    
    * Calculate missing values
    qui count if missing(`var')
    local nmiss = r(N)
    local nvalid = `n_obs' - `nmiss'
    local pctmiss = (`nmiss' / `n_obs') * 100
    
    * Count unique values
    qui tab `var', matrow(unique_vals)
    local nunique = r(r)
    if `nunique' == . local nunique = 0
    
    * Get value labels (if any)
    local vallabel : value label `var'
    local vallabel_sample ""
    if "`vallabel'" != "" {
        * Get first few value label mappings
        qui label list `vallabel'
        local vallabel_sample "`vallabel' (see label list)"
    }
    
    * Get min, max, mean for numeric variables
    local minval = .
    local maxval = .
    local meanval = .
    
    capture confirm numeric variable `var'
    if _rc == 0 {
        qui summarize `var', meanonly
        if r(N) > 0 {
            local minval = r(min)
            local maxval = r(max)
            local meanval = r(mean)
        }
    }
    
    * Update codebook dataset
    preserve
    use `codebook', clear
    
    qui replace varname = "`vname'" in `i'
    qui replace varlabel = substr("`vlabel'", 1, 200) in `i'
    qui replace vartype = "`vtype'" in `i'
    qui replace stata_format = "`vformat'" in `i'
    qui replace n_missing = `nmiss' in `i'
    qui replace n_valid = `nvalid' in `i'
    qui replace pct_missing = `pctmiss' in `i'
    qui replace n_unique = `nunique' in `i'
    qui replace value_labels_sample = "`vallabel_sample'" in `i'
    qui replace min_value = `minval' in `i'
    qui replace max_value = `maxval' in `i'
    qui replace mean_value = `meanval' in `i'
    
    save `codebook', replace
    restore
    
    * Progress indicator (every 20 variables)
    if mod(`i', 20) == 0 {
        display as text "  Processed `i' of `nvars' variables..."
    }
    
    local i = `i' + 1
}

display as text "  Processed `nvars' of `nvars' variables... Done!"
display as text _n

* Load the completed codebook
use `codebook', clear

* -----------------------------------------------------------------------------
* SECTION 3: VARIABLE HEALTH ASSESSMENT
* -----------------------------------------------------------------------------

display as text "SECTION 3: Variable Health Assessment"
display as text "--------------------------------------------------"
display as text _n

* Create health status categories
gen str25 health_status = ""
replace health_status = "1. Complete (0%)" if pct_missing == 0
replace health_status = "2. Excellent (<1%)" if pct_missing > 0 & pct_missing < 1
replace health_status = "3. Good (1-5%)" if pct_missing >= 1 & pct_missing < 5
replace health_status = "4. Moderate (5-10%)" if pct_missing >= 5 & pct_missing < 10
replace health_status = "5. Concerning (10-25%)" if pct_missing >= 10 & pct_missing < 25
replace health_status = "6. Poor (25-50%)" if pct_missing >= 25 & pct_missing < 50
replace health_status = "7. Critical (>50%)" if pct_missing >= 50

* Health status summary
display as text "Variable Health Summary:"
display as text _n
tab health_status, sort

* Overall completeness
qui summarize pct_missing
local overall_complete = 100 - r(mean)
display as text _n
display as text "Overall dataset completeness: " %5.2f `overall_complete' "%"

* Identify problematic variables (>10% missing)
display as text _n
display as text "Variables with >10% missing:"
display as text _n

list varname varlabel pct_missing if pct_missing > 10, ///
    separator(0) noobs abbreviate(20)

* Count problematic variables
qui count if pct_missing > 10
local n_problematic = r(N)
display as text _n
display as text "Total variables with >10% missing: `n_problematic'"

* -----------------------------------------------------------------------------
* SECTION 4: EXPORT TO EXCEL
* -----------------------------------------------------------------------------

display as text _n
display as text "SECTION 4: Export Codebook to Excel"
display as text "--------------------------------------------------"
display as text _n

* Sort by position for export
sort position

* Export main codebook sheet
export excel position varname varlabel vartype stata_format ///
    n_obs n_valid n_missing pct_missing n_unique ///
    min_value max_value mean_value health_status value_labels_sample ///
    using "ghs2024_codebook.xlsx", ///
    sheet("Codebook") sheetreplace firstrow(variables)

display as text "Exported: Codebook sheet"

* Create and export health summary
preserve
contract health_status, freq(n_variables)
gen pct_of_total = (n_variables / `nvars') * 100
export excel health_status n_variables pct_of_total ///
    using "ghs2024_codebook.xlsx", ///
    sheet("Health_Summary") sheetmodify firstrow(variables)
restore

display as text "Exported: Health_Summary sheet"

* Export problematic variables
preserve
keep if pct_missing > 10
sort pct_missing
gsort -pct_missing
if _N > 0 {
    export excel varname varlabel pct_missing n_missing ///
        using "ghs2024_codebook.xlsx", ///
        sheet("Problematic_Variables") sheetmodify firstrow(variables)
}
restore

display as text "Exported: Problematic_Variables sheet"

* Create dataset info
preserve
clear
set obs 7
gen str30 attribute = ""
gen str100 value = ""
replace attribute = "Dataset Name" in 1
replace value = "GHS 2024 Household File" in 1
replace attribute = "File" in 2
replace value = "ghs-2024-hhold-v1.dta" in 2
replace attribute = "Observations" in 3
replace value = string(`n_obs', "%12.0fc") in 3
replace attribute = "Variables" in 4
replace value = "`nvars'" in 4
replace attribute = "Overall Completeness (%)" in 5
replace value = string(`overall_complete', "%5.2f") in 5
replace attribute = "Generation Date" in 6
replace value = "`c(current_date)' `c(current_time)'" in 6
replace attribute = "Software" in 7
replace value = "Stata `c(stata_version)'" in 7

export excel attribute value ///
    using "ghs2024_codebook.xlsx", ///
    sheet("Dataset_Info") sheetmodify firstrow(variables)
restore

display as text "Exported: Dataset_Info sheet"
display as text _n
display as text "Excel codebook saved: ghs2024_codebook.xlsx"

* -----------------------------------------------------------------------------
* SECTION 5: EXPORT TO MARKDOWN
* -----------------------------------------------------------------------------

display as text _n
display as text "SECTION 5: Export Codebook to Markdown"
display as text "--------------------------------------------------"
display as text _n

* Create markdown file
tempname md
file open `md' using "ghs2024_codebook.md", write replace

* Header
file write `md' "# GHS 2024 Household File - Codebook" _n _n
file write `md' "**Generated:** `c(current_date)' `c(current_time)'" _n _n
file write `md' "## Dataset Overview" _n _n
file write `md' "- **File:** ghs-2024-hhold-v1.dta" _n
file write `md' "- **Observations:** " %12.0fc (`n_obs') _n
file write `md' "- **Variables:** `nvars'" _n
file write `md' "- **Overall Completeness:** " %5.2f (`overall_complete') "%" _n _n

* Health summary table
file write `md' "## Variable Health Summary" _n _n
file write `md' "| Health Status | Variables | % of Total |" _n
file write `md' "|---------------|-----------|------------|" _n

* Calculate health summary for markdown
forvalues h = 1/7 {
    local status_labels `" "1. Complete (0%)" "2. Excellent (<1%)" "3. Good (1-5%)" "4. Moderate (5-10%)" "5. Concerning (10-25%)" "6. Poor (25-50%)" "7. Critical (>50%)" "'
    local status_label : word `h' of `status_labels'
    qui count if health_status == "`status_label'"
    local ncount = r(N)
    local pct = (`ncount' / `nvars') * 100
    if `ncount' > 0 {
        file write `md' "| `status_label' | `ncount' | " %5.1f (`pct') "% |" _n
    }
}

file write `md' _n

* Variable list table
file write `md' "## Complete Variable List" _n _n
file write `md' "| # | Variable | Label | Type | Missing (%) | Unique |" _n
file write `md' "|---|----------|-------|------|-------------|--------|" _n

* Write each variable row
sort position
local nrows = _N
forvalues i = 1/`nrows' {
    local pos = position[`i']
    local vname = varname[`i']
    local vlabel = varlabel[`i']
    * Truncate long labels
    if length("`vlabel'") > 50 {
        local vlabel = substr("`vlabel'", 1, 47) + "..."
    }
    local vtype = vartype[`i']
    local pmiss = pct_missing[`i']
    local nuniq = n_unique[`i']
    
    file write `md' "| `pos' | `" "`vname'" "` | `vlabel' | `vtype' | " %5.1f (`pmiss') "% | `nuniq' |" _n
}

file write `md' _n

* Problematic variables section
qui count if pct_missing > 10
if r(N) > 0 {
    file write `md' "## Variables Requiring Attention (>10% Missing)" _n _n
    file write `md' "| Variable | Label | Missing (%) | N Missing |" _n
    file write `md' "|----------|-------|-------------|-----------|" _n
    
    gsort -pct_missing
    forvalues i = 1/`nrows' {
        if pct_missing[`i'] > 10 {
            local vname = varname[`i']
            local vlabel = varlabel[`i']
            if length("`vlabel'") > 40 {
                local vlabel = substr("`vlabel'", 1, 37) + "..."
            }
            local pmiss = pct_missing[`i']
            local nmiss = n_missing[`i']
            
            file write `md' "| `vname' | `vlabel' | " %5.1f (`pmiss') "% | " %9.0fc (`nmiss') " |" _n
        }
    }
}

file close `md'

display as text "Markdown codebook saved: ghs2024_codebook.md"

* -----------------------------------------------------------------------------
* SECTION 6: VISUALIZE VARIABLE HEALTH
* -----------------------------------------------------------------------------

display as text _n
display as text "SECTION 6: Variable Health Visualization"
display as text "--------------------------------------------------"
display as text _n

* Sort by missingness for plotting
gsort -pct_missing

* --- PLOT 1: Horizontal Bar Chart of Missingness ---

* For large datasets, show top 50 variables with highest missingness
preserve

* Keep only variables with non-zero missingness, limit to top 50
keep if pct_missing > 0
if _N > 50 {
    keep in 1/50
    local plot_title "Variable Missingness (Top 50 Variables with Missing Data)"
} 
else {
    local plot_title "Variable Missingness (All Variables with Missing Data)"
}

* Create numeric ID for plotting
gen plot_order = _n
labmask plot_order, values(varname)

* Create color variable based on health status
gen color_group = .
replace color_group = 1 if pct_missing > 0 & pct_missing < 1    // Green
replace color_group = 2 if pct_missing >= 1 & pct_missing < 5   // Light green
replace color_group = 3 if pct_missing >= 5 & pct_missing < 10  // Yellow
replace color_group = 4 if pct_missing >= 10 & pct_missing < 25 // Orange
replace color_group = 5 if pct_missing >= 25 & pct_missing < 50 // Red
replace color_group = 6 if pct_missing >= 50                     // Dark red

* Create horizontal bar chart
graph hbar pct_missing, over(plot_order, sort(pct_missing) descending ///
    label(labsize(tiny))) ///
    bar(1, color(cranberry)) ///
    ylabel(, angle(horizontal) labsize(small)) ///
    ytitle("Percent Missing") ///
    title("`plot_title'") ///
    subtitle("GHS 2024 Household File") ///
    note("Variables ordered by missingness rate") ///
    yline(5 10 25, lpattern(dash) lcolor(gray)) ///
    scheme(s2color)

graph export "variable_health_barchart.png", replace width(1800) height(1200)
display as text "Saved: variable_health_barchart.png"

restore

* --- PLOT 2: Pie Chart of Health Status Distribution ---

preserve

* Collapse to health status counts
contract health_status, freq(n_vars)
gen pct = (n_vars / `nvars') * 100

* Create pie chart
graph pie n_vars, over(health_status) ///
    plabel(_all percent, format(%4.1f) size(small)) ///
    legend(size(vsmall) cols(1) position(3)) ///
    title("Variable Health Distribution") ///
    subtitle("GHS 2024 Household File (`nvars' variables)") ///
    scheme(s2color)

graph export "variable_health_distribution.png", replace width(1200) height(900)
display as text "Saved: variable_health_distribution.png"

restore

* --- PLOT 3: Scatter Plot of Missingness by Variable Position ---

preserve

* Create scatter plot showing missingness pattern across dataset
twoway (scatter pct_missing position, mcolor(cranberry%60) msize(small)) ///
    (lowess pct_missing position, lcolor(navy) lwidth(medium)), ///
    ytitle("Percent Missing") ///
    xtitle("Variable Position in Dataset") ///
    ylabel(0(10)100, angle(horizontal)) ///
    title("Missingness Pattern Across Dataset") ///
    subtitle("GHS 2024 Household File") ///
    legend(label(1 "Individual Variables") label(2 "Smoothed Trend") ///
           position(6) rows(1)) ///
    yline(5 10 25, lpattern(dash) lcolor(gray%50)) ///
    scheme(s2color)

graph export "variable_health_scatter.png", replace width(1200) height(600)
display as text "Saved: variable_health_scatter.png"

restore

* --- PLOT 4: Histogram of Missingness Distribution ---

preserve

histogram pct_missing, ///
    bin(20) ///
    freq ///
    color(navy%70) ///
    lcolor(white) ///
    xtitle("Percent Missing") ///
    ytitle("Number of Variables") ///
    title("Distribution of Variable Missingness") ///
    subtitle("GHS 2024 Household File") ///
    xline(5 10 25, lpattern(dash) lcolor(cranberry)) ///
    note("Vertical lines at 5%, 10%, and 25% thresholds") ///
    scheme(s2color)

graph export "variable_health_histogram.png", replace width(1000) height(600)
display as text "Saved: variable_health_histogram.png"

restore

* --- PLOT 5: Cumulative Distribution ---

preserve

* Sort by missingness
sort pct_missing

* Create cumulative percentage
gen cumulative_pct = (_n / _N) * 100

* Create cumulative distribution plot
twoway (line cumulative_pct pct_missing, lcolor(navy) lwidth(medium)) ///
    (area cumulative_pct pct_missing, color(navy%20)), ///
    ytitle("Cumulative % of Variables") ///
    xtitle("Percent Missing (Threshold)") ///
    ylabel(0(25)100, angle(horizontal)) ///
    xlabel(0(10)100) ///
    title("Cumulative Distribution of Variable Missingness") ///
    subtitle("What % of variables have missingness below each threshold?") ///
    xline(5 10 25, lpattern(dash) lcolor(gray)) ///
    legend(off) ///
    scheme(s2color)

graph export "variable_health_cumulative.png", replace width(1000) height(600)
display as text "Saved: variable_health_cumulative.png"

restore

* -----------------------------------------------------------------------------
* SECTION 7: SUMMARY REPORT
* -----------------------------------------------------------------------------

display as text _n
display as text "========================================================================"
display as text "LAB 5.3 COMPLETE: CODEBOOK GENERATION SUMMARY"
display as text "========================================================================"
display as text _n

display as text "DATASET SUMMARY:"
display as text "  File: ghs-2024-hhold-v1.dta"
display as text "  Observations: " %12.0fc `n_obs'
display as text "  Variables: `nvars'"
display as text "  Overall Completeness: " %5.2f `overall_complete' "%"
display as text _n

display as text "VARIABLE HEALTH BREAKDOWN:"
tab health_status, sort

display as text _n
display as text "OUTPUT FILES GENERATED:"
display as text "  1. ghs2024_codebook.xlsx           - Complete codebook in Excel format"
display as text "  2. ghs2024_codebook.md             - Codebook in Markdown format"
display as text "  3. variable_health_barchart.png    - Bar chart of missingness"
display as text "  4. variable_health_distribution.png - Pie chart of health status"
display as text "  5. variable_health_scatter.png     - Scatter plot by position"
display as text "  6. variable_health_histogram.png   - Histogram of missingness"
display as text "  7. variable_health_cumulative.png  - Cumulative distribution"

display as text _n
display as text "========================================================================"
display as text "END OF LAB 5.3"
display as text "========================================================================"

* Save the codebook dataset
save "ghs2024_codebook_data.dta", replace
display as text _n
display as text "Codebook dataset saved: ghs2024_codebook_data.dta"

log close

* =============================================================================
* END OF SCRIPT
* =============================================================================
