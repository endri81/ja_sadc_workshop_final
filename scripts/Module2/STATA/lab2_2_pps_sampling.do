*===============================================================================
* LAB 2.2: PPS SAMPLING SELECTION
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 2: Computational Tools and Spatial Integration
*===============================================================================
*
* OBJECTIVE:
* Demonstrate Probability Proportional to Size (PPS) systematic sampling
* by creating a sampling frame from household data and selecting PSUs.
*
* LEARNING OUTCOMES:
* 1. Aggregate household data to create a PSU-level sampling frame
* 2. Implement PPS systematic sampling algorithm
* 3. Calculate and verify selection probabilities
* 4. Understand the relationship between MOS and selection probability
*
* DATA SOURCE:
* Statistics South Africa General Household Survey 2024 (Household file)
* Renamed to Zambara National Household Survey for training purposes
*
* REFERENCES:
* - Cochran, W.G. (1977). Sampling Techniques, 3rd ed. Wiley. Chapter 9.
* - Lohr, S.L. (2022). Sampling: Design and Analysis, 3rd ed. CRC Press. Ch 6.
* - Stats SA (2024). GHS 2024 Metadata: Randomised PPS Systematic Sampling.
*
* AUTHOR: SADC Workshop Team
* DATE: March 2026
*===============================================================================

clear all
set more off
cap log close

* Start log file
log using "lab2_2_pps_sampling.log", replace

display _newline(2)
display "=" * 70
display "LAB 2.2: PPS SAMPLING SELECTION"
display "=" * 70
display _newline(1)

*===============================================================================
* PART 1: LOAD DATA AND CREATE SAMPLING FRAME
*===============================================================================

display "PART 1: Creating the PSU Sampling Frame"
display "-" * 50
display _newline(1)

* Load household data
use "ghs-2024-hhold-v1.dta", clear

display "Loaded household data:"
describe, short
display _newline(1)

* Examine PSU variable
display "PSU (Primary Sampling Unit) summary:"
codebook psu, compact
distinct psu
display _newline(1)

*-------------------------------------------------------------------------------
* Step 1.1: Aggregate household counts by PSU to create Measure of Size (MOS)
*-------------------------------------------------------------------------------

* Create the sampling frame by collapsing to PSU level
* MOS = number of households in each PSU
* Note: This replaces the household data with the PSU frame

collapse (count) n_households=psu (mean) mean_income=totmhinc ///
         (first) stratum prov, by(psu)

* Sort by PSU
sort psu

* Save frame statistics
quietly count
local n_psus = r(N)
quietly summarize n_households
local total_mos = r(sum)

display "PSU Sampling Frame created:"
display "  Number of PSUs (frame units): `n_psus'"
display "  Total households in frame: `total_mos'"
display _newline(1)

* Summary statistics of MOS
display "Measure of Size (MOS = household count) distribution:"
summarize n_households, detail
display _newline(1)

* MOS distribution by province
display "MOS Distribution by Province:"
table prov, statistic(count n_households) statistic(sum n_households) ///
            statistic(mean n_households) statistic(min n_households) ///
            statistic(max n_households)
display _newline(1)

* Save the frame for later use
save "zambara_psu_frame.dta", replace

*===============================================================================
* PART 2: IMPLEMENT PPS SYSTEMATIC SAMPLING
*===============================================================================

display "PART 2: PPS Systematic Sampling Implementation"
display "-" * 50
display _newline(1)

*-------------------------------------------------------------------------------
* Step 2.1: Set up the PPS sampling parameters
*-------------------------------------------------------------------------------

* Sample size: select 100 PSUs
local n_sample = 100

* Total measure of size
quietly summarize n_households
local total_mos = r(sum)
local n_frame = r(N)

* Sampling interval
local sampling_interval = `total_mos' / `n_sample'

display "Sampling parameters:"
display "  Sample size (n): `n_sample' PSUs"
display "  Frame size (N): `n_frame' PSUs"
display "  Total MOS: `total_mos' households"
display "  Sampling interval (I): " %9.2f `sampling_interval'
display _newline(1)

*-------------------------------------------------------------------------------
* Step 2.2: Calculate selection probabilities under PPS
*-------------------------------------------------------------------------------

* Under PPS, the selection probability for PSU i is:
* π_i = n × MOS_i / Total_MOS

gen prob_selection = `n_sample' * n_households / `total_mos'

* Check for certainty selections (prob > 1)
gen certainty = (prob_selection > 1)

quietly count if certainty == 1
local n_certainty = r(N)

quietly summarize prob_selection
display "Selection probability check:"
display "  PSUs with π > 1 (certainty selections): `n_certainty'"
display "  Max selection probability: " %9.4f r(max)
display "  Min selection probability: " %9.4f r(min)
display _newline(1)

*-------------------------------------------------------------------------------
* Step 2.3: Randomized PPS Systematic Sampling Algorithm
*-------------------------------------------------------------------------------

display "Implementing Randomized PPS Systematic Sampling..."
display _newline(1)

* Set seed for reproducibility
set seed 42

* Step 1: Randomize PSU order (this is the "Randomized" part of RPPS)
gen random_order = runiform()
sort random_order
gen seq_number = _n

* Step 2: Calculate cumulative MOS
gen cumulative_mos = sum(n_households)
gen cumulative_mos_prev = cumulative_mos - n_households

* Step 3: Generate random start between 0 and sampling_interval
local random_start = runiform() * `sampling_interval'
display "Sampling interval (I): " %9.2f `sampling_interval'
display "Random start (r): " %9.2f `random_start'
display _newline(1)

* Step 4: Generate selection indicator
gen selected = 0

* Step 5: Select PSUs at systematic intervals
* Loop through selection points and mark selected PSUs

forvalues i = 0/`=`n_sample'-1' {
    local selection_point = `random_start' + `i' * `sampling_interval'
    
    * Find PSU where this point falls
    quietly replace selected = 1 if cumulative_mos_prev < `selection_point' ///
                                  & cumulative_mos >= `selection_point' ///
                                  & selected == 0
}

* Display first and last selection points
display "First selection point: " %9.2f `random_start'
display "Last selection point: " %9.2f (`random_start' + (`n_sample'-1) * `sampling_interval')
display _newline(1)

* Count selected PSUs
quietly count if selected == 1
local n_selected = r(N)
display "PSUs selected: `n_selected'"
display _newline(1)

* Save frame with selection indicators
save "zambara_psu_frame_with_selection.dta", replace

*===============================================================================
* PART 3: VERIFY SELECTION PROBABILITIES
*===============================================================================

display "PART 3: Verification of Selection Probabilities"
display "-" * 50
display _newline(1)

*-------------------------------------------------------------------------------
* Step 3.1: Compare expected vs actual selection
*-------------------------------------------------------------------------------

display "Sample composition:"
display "  Expected sample size: `n_sample'"
display "  Actual sample size: `n_selected'"
display _newline(1)

* Summary of selected PSUs
display "Selected PSUs - MOS distribution:"
summarize n_households if selected == 1, detail
display _newline(1)

* Compare Frame vs Sample statistics
display "MOS Comparison (Frame vs PPS Sample):"
display _newline(1)
display "Statistic" _col(20) "Frame" _col(35) "Sample"
display "-" * 45

quietly summarize n_households
local frame_mean = r(mean)
local frame_min = r(min)
local frame_max = r(max)
local frame_sum = r(sum)

quietly summarize n_households if selected == 1
local sample_mean = r(mean)
local sample_min = r(min)
local sample_max = r(max)
local sample_sum = r(sum)

display "Mean" _col(20) %9.1f `frame_mean' _col(35) %9.1f `sample_mean'
display "Min" _col(20) %9.0f `frame_min' _col(35) %9.0f `sample_min'
display "Max" _col(20) %9.0f `frame_max' _col(35) %9.0f `sample_max'
display "Sum" _col(20) %9.0f `frame_sum' _col(35) %9.0f `sample_sum'
display _newline(1)

*-------------------------------------------------------------------------------
* Step 3.2: Verify selection probabilities
*-------------------------------------------------------------------------------

display "Selection Probability Verification:"

quietly summarize prob_selection
display "  Sum of selection probabilities (should ≈ n): " %9.2f r(sum)
display "  Expected sum: `n_sample'"
display _newline(1)

* Show selection probabilities for first 10 selected PSUs
display "Selection probabilities of first 10 selected PSUs:"
list psu n_households prob_selection if selected == 1 in 1/10, clean noobs
display _newline(1)

*-------------------------------------------------------------------------------
* Step 3.3: Calculate base weights (inverse of selection probability)
*-------------------------------------------------------------------------------

* Base weight = 1 / π_i
gen base_weight = 1 / prob_selection

display "Base Weights (1/π) for selected PSUs:"
quietly summarize base_weight if selected == 1
display "  Mean base weight: " %9.2f r(mean)
display "  Range: " %9.2f r(min) " to " %9.2f r(max)
display _newline(1)

* Verify: sum of weights should estimate frame size
quietly summarize base_weight if selected == 1
local sum_weights = r(sum)
display "Weight verification:"
display "  Sum of base weights: " %9.0f `sum_weights'
display "  Actual frame size: `n_frame'"
display "  Ratio (should ≈ 1): " %9.3f (`sum_weights' / `n_frame')
display _newline(1)

*-------------------------------------------------------------------------------
* Step 3.4: Provincial distribution comparison
*-------------------------------------------------------------------------------

display "Provincial Distribution (Frame vs PPS Sample):"
display _newline(1)

* Create comparison table
preserve
    * Frame statistics by province
    collapse (count) n_psus_frame=psu (sum) mos_frame=n_households, by(prov)
    egen total_mos_frame = sum(mos_frame)
    gen pct_mos_frame = 100 * mos_frame / total_mos_frame
    tempfile frame_prov
    save `frame_prov'
restore

preserve
    * Sample statistics by province
    keep if selected == 1
    collapse (count) n_psus_sample=psu (sum) mos_sample=n_households, by(prov)
    egen total_psus_sample = sum(n_psus_sample)
    gen pct_psus_sample = 100 * n_psus_sample / total_psus_sample
    
    * Merge with frame stats
    merge 1:1 prov using `frame_prov', nogenerate
    
    display "Province" _col(15) "% MOS (Frame)" _col(35) "% PSUs (Sample)"
    display "-" * 50
    
    forvalues p = 1/9 {
        quietly summarize pct_mos_frame if prov == `p'
        if r(N) > 0 {
            local pct_frame = r(mean)
            quietly summarize pct_psus_sample if prov == `p'
            local pct_sample = r(mean)
            display "`p'" _col(15) %9.1f `pct_frame' _col(35) %9.1f `pct_sample'
        }
    }
    display _newline(1)
    display "Note: Under PPS, sample % should approximate frame MOS %"
restore

display _newline(1)

*===============================================================================
* PART 4: EXTRACT AND SAVE THE PPS SAMPLE
*===============================================================================

display "PART 4: Saving Selected Sample"
display "-" * 50
display _newline(1)

* Create sample dataset
preserve
    keep if selected == 1
    keep psu stratum prov n_households prob_selection base_weight seq_number
    
    * Label variables
    label variable psu "Primary Sampling Unit ID"
    label variable stratum "Stratum ID"
    label variable prov "Province"
    label variable n_households "Measure of Size (household count)"
    label variable prob_selection "Selection probability under PPS"
    label variable base_weight "Base weight (1/probability)"
    label variable seq_number "Sequence number in randomized list"
    
    * Save
    save "zambara_pps_sample.dta", replace
    export delimited using "zambara_pps_sample.csv", replace
    
    display "Saved: zambara_pps_sample.dta"
    display "Saved: zambara_pps_sample.csv"
restore

display _newline(1)

*===============================================================================
* PART 5: ALTERNATIVE - USING STATA'S BUILT-IN PPS COMMAND
*===============================================================================

display "PART 5: Alternative Method - Using gsample (if available)"
display "-" * 50
display _newline(1)

* Note: STATA 17+ has gsample command for PPS sampling
* For earlier versions, use the manual method above or samplepps (user-written)

display "The manual implementation above demonstrates the algorithm."
display "STATA 17+ users can also use: gsample with pps option"
display "User-written alternatives: samplepps, ppssamp"
display _newline(1)

* Demonstrate using samplepps if available (may need to install)
* ssc install samplepps
* capture which samplepps
* if _rc == 0 {
*     samplepps, nclusters(100) size(n_households) gen(selected_alt)
* }

*===============================================================================
* SUMMARY
*===============================================================================

display _newline(2)
display "=" * 70
display "LAB 2.2 SUMMARY"
display "=" * 70
display _newline(1)

display "KEY RESULTS:"
display "  1. Created sampling frame with `n_frame' PSUs"
display "  2. Total MOS (households): `total_mos'"
display "  3. Selected `n_selected' PSUs using PPS systematic sampling"
display "  4. Sampling interval: " %9.2f `sampling_interval'
display "  5. Selection probabilities verified"
display _newline(1)

display "KEY FORMULAS:"
display "  Selection probability: π_i = n × MOS_i / Σ MOS"
display "  Sampling interval: I = Σ MOS / n"
display "  Base weight: w_i = 1 / π_i"
display _newline(1)

display "INTERPRETATION:"
display "  - Larger PSUs (more households) have higher selection probability"
display "  - PPS sampling is self-weighting for household-level estimates"
display "  - Provincial representation proportional to total households"
display _newline(1)

display "OUTPUT FILES:"
display "  - zambara_psu_frame.dta: Full PSU sampling frame"
display "  - zambara_psu_frame_with_selection.dta: Frame with selection indicators"
display "  - zambara_pps_sample.dta: Selected PSU sample"
display "  - zambara_pps_sample.csv: Selected sample (CSV format)"
display _newline(1)

display "=" * 70
display "END OF LAB 2.2 (STATA)"
display "=" * 70

log close

*===============================================================================
* END OF DO FILE
*===============================================================================
