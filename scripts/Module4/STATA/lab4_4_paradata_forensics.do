*===============================================================================
* LAB 4.4: PARADATA FORENSICS
* SADC Regional Training Workshop on Advanced Sampling Methods
* Day 4, Module 4.4: Quality Assurance Integration
*===============================================================================
*
* OBJECTIVE: Use paradata to detect data quality issues
*   1. Simulate paradata: Interview duration and timestamps
*   2. Flag "Curbstoning" (fake interviews): duration < 5 min or 2 AM
*   3. Generate Field Auditor Report listing suspicious enumerators
*
* DATA: GHS 2024 Household File
*
* ZAMBARA NARRATIVE: Lindiwe discovers suspicious patterns in field data
*
*===============================================================================

clear all
set more off
cap log close

* Start log file
log using "lab4_4_paradata_forensics.log", replace

display _n
display "================================================================"
display "  LAB 4.4: PARADATA FORENSICS"
display "  SADC Advanced Sampling Workshop - Day 4"
display "================================================================"
display _n

*-------------------------------------------------------------------------------
* 1. LOAD AND PREPARE DATA
*-------------------------------------------------------------------------------

display "SECTION 1: Loading and Preparing Data"
display "-------------------------------------" _n

* Load GHS 2024 household data
use "ghs-2024-hhold-v1.dta", clear

display "Dataset loaded: " _N " households" _n

* Create unique household ID
gen hh_id = _n
label var hh_id "Unique household identifier"

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

*-------------------------------------------------------------------------------
* 2. SIMULATE PARADATA: ENUMERATOR IDs
*-------------------------------------------------------------------------------

display _n "SECTION 2: Simulating Enumerator Assignments"
display "--------------------------------------------" _n

* Set seed for reproducibility
set seed 20260305

* Number of enumerators
local n_enumerators = 50

* Get unique PSUs
preserve
collapse (count) n = hh_id, by(psu)
local n_psus = _N

* Assign enumerators to PSUs (random assignment)
gen enum_num = ceil(runiform() * `n_enumerators')
gen enumerator_id = "ENUM_" + string(enum_num, "%03.0f")
keep psu enumerator_id

tempfile psu_enum
save `psu_enum'
restore

* Merge enumerator IDs to household data
merge m:1 psu using `psu_enum', nogen

display "Enumerators assigned: `n_enumerators'"
display "PSUs covered: `n_psus'"

* Check enumerator workload
preserve
collapse (count) n_interviews = hh_id (count) n_psus = psu, by(enumerator_id)
qui sum n_interviews
display _n "Enumerator Workload Summary:"
display "  Mean interviews: " %6.1f r(mean)
display "  Min interviews: " r(min)
display "  Max interviews: " r(max)
restore

*-------------------------------------------------------------------------------
* 3. SIMULATE PARADATA: INTERVIEW DURATION
*-------------------------------------------------------------------------------

display _n "SECTION 3: Simulating Interview Duration"
display "----------------------------------------" _n

* GHS questionnaire: expect 25-45 minutes for good interview
* Distribution types:
*   - Legitimate (90%): Normal, mean 35, SD 8
*   - Curbstoning (3%): Very short, mean 3, SD 1.5
*   - Complex (7%): Long, mean 55, SD 10

* Generate interview type
gen rand_type = runiform()
gen interview_type = "legitimate"
replace interview_type = "curbstoning" if rand_type < 0.03
replace interview_type = "complex" if rand_type >= 0.03 & rand_type < 0.10

* Generate duration based on type
gen duration_minutes = .
replace duration_minutes = rnormal(3, 1.5) if interview_type == "curbstoning"
replace duration_minutes = rnormal(55, 10) if interview_type == "complex"
replace duration_minutes = rnormal(35, 8) if interview_type == "legitimate"

* Ensure minimum 1 minute
replace duration_minutes = max(1, duration_minutes)

label var duration_minutes "Interview duration in minutes"

* Summary
qui sum duration_minutes, detail
display "Interview Duration Summary:"
display "  Mean: " %6.1f r(mean) " minutes"
display "  Median: " %6.1f r(p50) " minutes"
display "  Min: " %6.1f r(min) " minutes"
display "  Max: " %6.1f r(max) " minutes"

* Duration distribution
gen duration_cat = .
replace duration_cat = 1 if duration_minutes < 5
replace duration_cat = 2 if duration_minutes >= 5 & duration_minutes < 15
replace duration_cat = 3 if duration_minutes >= 15 & duration_minutes < 30
replace duration_cat = 4 if duration_minutes >= 30 & duration_minutes < 45
replace duration_cat = 5 if duration_minutes >= 45 & duration_minutes < 60
replace duration_cat = 6 if duration_minutes >= 60

label define dur_lbl 1 "<5 min" 2 "5-15 min" 3 "15-30 min" ///
    4 "30-45 min" 5 "45-60 min" 6 ">60 min"
label values duration_cat dur_lbl

display _n "Duration Distribution:"
tab duration_cat

*-------------------------------------------------------------------------------
* 4. SIMULATE PARADATA: INTERVIEW TIMESTAMPS
*-------------------------------------------------------------------------------

display _n "SECTION 4: Simulating Interview Timestamps"
display "------------------------------------------" _n

* Survey period: March 1-31, 2026
* Normal hours: 8 AM - 6 PM
* Suspicious: Before 7 AM or after 9 PM

* Generate interview date (random within March 2026)
gen interview_day = ceil(runiform() * 31)
gen interview_date = mdy(3, interview_day, 2026)
format interview_date %td
label var interview_date "Interview date"

* Generate interview hour based on type
gen interview_hour = .

* Curbstoning: 20% at suspicious hours, 80% normal to avoid detection
gen rand_hour = runiform()
replace interview_hour = floor(runiform() * 4) + 1 if interview_type == "curbstoning" & rand_hour < 0.10
replace interview_hour = 23 if interview_type == "curbstoning" & rand_hour >= 0.10 & rand_hour < 0.20
replace interview_hour = floor(runiform() * 9) + 9 if interview_type == "curbstoning" & rand_hour >= 0.20

* Complex: often later in day (10 AM - 7 PM)
replace interview_hour = floor(runiform() * 10) + 10 if interview_type == "complex"

* Legitimate: normal working hours (8 AM - 6 PM mostly)
replace interview_hour = floor(runiform() * 11) + 8 if interview_type == "legitimate"

* Add some evening interviews for legitimate (after 6 PM)
replace interview_hour = floor(runiform() * 3) + 18 if interview_type == "legitimate" & runiform() < 0.05

label var interview_hour "Hour of interview (0-23)"

* Generate minutes
gen interview_minute = floor(runiform() * 60)

display "Interview Hour Distribution:"
tab interview_hour

*-------------------------------------------------------------------------------
* 5. FLAG CURBSTONING INDICATORS
*-------------------------------------------------------------------------------

display _n "SECTION 5: Flagging Curbstoning Indicators"
display "------------------------------------------" _n

* Define thresholds
local DURATION_THRESHOLD = 5
local SUSPICIOUS_HOURS "0 1 2 3 4 5 22 23"

* Flag 1: Very short interviews
gen flag_short_duration = (duration_minutes < `DURATION_THRESHOLD')
label var flag_short_duration "Interview < 5 minutes"

* Flag 2: Suspicious interview time (10 PM - 5 AM)
gen flag_suspicious_time = inlist(interview_hour, 0, 1, 2, 3, 4, 5, 22, 23)
label var flag_suspicious_time "Interview at suspicious hour"

* Flag 3: Combined - either condition
gen flag_any = (flag_short_duration == 1 | flag_suspicious_time == 1)
label var flag_any "Any curbstoning flag"

* Flag 4: Severe - both conditions
gen flag_severe = (flag_short_duration == 1 & flag_suspicious_time == 1)
label var flag_severe "Both curbstoning flags"

* Summary of flags
qui count
local total_n = r(N)

qui count if flag_short_duration == 1
local n_short = r(N)
local pct_short = round(100 * `n_short' / `total_n', 0.01)

qui count if flag_suspicious_time == 1
local n_time = r(N)
local pct_time = round(100 * `n_time' / `total_n', 0.01)

qui count if flag_any == 1
local n_any = r(N)
local pct_any = round(100 * `n_any' / `total_n', 0.01)

qui count if flag_severe == 1
local n_severe = r(N)
local pct_severe = round(100 * `n_severe' / `total_n', 0.01)

display "CURBSTONING FLAGS SUMMARY:"
display "-------------------------"
display "Total interviews: `total_n'"
display ""
display "Flag 1 - Short duration (<`DURATION_THRESHOLD' min): `n_short' (`pct_short'%)"
display "Flag 2 - Suspicious time (10PM-5AM): `n_time' (`pct_time'%)"
display "Any flag: `n_any' (`pct_any'%)"
display "Severe (both flags): `n_severe' (`pct_severe'%)"

*-------------------------------------------------------------------------------
* 6. ANALYZE BY ENUMERATOR
*-------------------------------------------------------------------------------

display _n "SECTION 6: Enumerator-Level Analysis"
display "------------------------------------" _n

* Calculate metrics by enumerator
preserve
collapse (count) n_interviews = hh_id ///
    (mean) mean_duration = duration_minutes ///
    (min) min_duration = duration_minutes ///
    (sd) sd_duration = duration_minutes ///
    (sum) n_short = flag_short_duration ///
    (sum) n_suspicious_time = flag_suspicious_time ///
    (sum) n_any_flag = flag_any ///
    (sum) n_severe = flag_severe ///
    (mean) pct_short = flag_short_duration ///
    (mean) pct_suspicious_time = flag_suspicious_time ///
    (mean) pct_any_flag = flag_any, ///
    by(enumerator_id)

* Convert to percentages
replace pct_short = pct_short * 100
replace pct_suspicious_time = pct_suspicious_time * 100
replace pct_any_flag = pct_any_flag * 100

* Calculate risk score
gen risk_score = 0.4 * pct_short + 0.3 * pct_suspicious_time + ///
    0.2 * (100 - min(mean_duration, 35) / 35 * 100) + ///
    0.1 * (100 - min(sd_duration, 10) / 10 * 100)

* Risk category
gen risk_category = "NORMAL"
replace risk_category = "LOW" if risk_score > 5
replace risk_category = "MEDIUM" if risk_score > 15
replace risk_category = "HIGH" if risk_score > 30

label var risk_score "Curbstoning risk score"
label var risk_category "Risk category"

* Sort by risk score
gsort -risk_score

display "Enumerator Risk Distribution:"
tab risk_category

* Save enumerator analysis
tempfile enum_analysis
save `enum_analysis'

*-------------------------------------------------------------------------------
* 7. IDENTIFY HIGH-RISK ENUMERATORS
*-------------------------------------------------------------------------------

display _n "SECTION 7: High-Risk Enumerators"
display "--------------------------------" _n

* High risk
display "HIGH-RISK ENUMERATORS:"
list enumerator_id n_interviews mean_duration n_any_flag pct_any_flag risk_score ///
    if risk_category == "HIGH", clean noobs

qui count if risk_category == "HIGH"
local n_high_risk = r(N)

* Medium risk
display _n "MEDIUM-RISK ENUMERATORS (top 10):"
list enumerator_id n_interviews mean_duration n_any_flag pct_any_flag risk_score ///
    if risk_category == "MEDIUM" in 1/10, clean noobs

qui count if risk_category == "MEDIUM"
local n_medium_risk = r(N)

restore

*-------------------------------------------------------------------------------
* 8. GENERATE FIELD AUDITOR REPORT
*-------------------------------------------------------------------------------

display _n "================================================================"
display "SECTION 8: FIELD AUDITOR REPORT"
display "================================================================" _n

display "ZAMBARA NATIONAL STATISTICS OFFICE"
display "FIELD AUDITOR REPORT: PARADATA QUALITY REVIEW"
display "Report Generated: " c(current_date)
display "Survey: General Household Survey 2026"
display "Analysis Period: March 1-31, 2026"
display "================================================================" _n

display "EXECUTIVE SUMMARY"
display "-----------------"
display "Total interviews reviewed: `total_n'"
display "Total enumerators: `n_enumerators'"
display "Interviews flagged: `n_any' (`pct_any'%)"
display "High-risk enumerators: `n_high_risk'"
display "Medium-risk enumerators: `n_medium_risk'" _n

display "FLAGGING CRITERIA"
display "-----------------"
display "1. Short Duration: Interview < 5 minutes"
display "2. Suspicious Time: Interview conducted 10 PM - 5 AM"
display "3. Risk Score: Weighted combination of duration and time flags" _n

display "RECOMMENDATIONS"
display "---------------"
display "1. HIGH-RISK: Immediately suspend `n_high_risk' enumerators pending investigation"
display "2. MEDIUM-RISK: Implement enhanced supervision for `n_medium_risk' enumerators"
display "3. Conduct callback verification on 10% of flagged interviews"
display "4. Review training protocols for interview pacing"
display "5. Implement real-time paradata monitoring in future surveys" _n

display "================================================================"
display "END OF REPORT"
display "================================================================" _n

*-------------------------------------------------------------------------------
* 9. VISUALIZATIONS
*-------------------------------------------------------------------------------

display "SECTION 9: Creating Visualizations"
display "----------------------------------" _n

* Plot 1: Duration distribution
histogram duration_minutes if duration_minutes < 80, ///
    bin(40) color(navy) ///
    xline(5, lcolor(red) lpattern(dash)) ///
    title("Interview Duration Distribution") ///
    subtitle("Red line: 5-minute curbstoning threshold") ///
    xtitle("Duration (minutes)") ytitle("Frequency") ///
    note("Source: Lab 4.4 Paradata Forensics")

graph export "lab4_4_duration_distribution.png", replace width(1000) height(600)
display "Saved: lab4_4_duration_distribution.png"

* Plot 2: Interview hours
graph bar (count), over(interview_hour) ///
    bar(1, color(navy)) ///
    title("Interview Hour Distribution") ///
    ytitle("Count") ///
    note("Suspicious hours: 10 PM - 5 AM")

graph export "lab4_4_interview_hours.png", replace width(1000) height(600)
display "Saved: lab4_4_interview_hours.png"

* Plot 3: Flagged by enumerator
preserve
use `enum_analysis', clear
graph bar pct_any_flag, over(enumerator_id, sort(1) descending label(angle(90) labsize(tiny))) ///
    bar(1, color(navy)) ///
    title("Flagged Interview Rate by Enumerator") ///
    ytitle("Percent Flagged") ///
    note("Source: Lab 4.4")

graph export "lab4_4_enumerator_flags.png", replace width(1200) height(600)
display "Saved: lab4_4_enumerator_flags.png"
restore

*-------------------------------------------------------------------------------
* 10. EXPORT RESULTS
*-------------------------------------------------------------------------------

display _n "SECTION 10: Exporting Results"
display "-----------------------------" _n

* Export enumerator analysis
preserve
use `enum_analysis', clear
export delimited using "lab4_4_enumerator_analysis.csv", replace
display "Saved: lab4_4_enumerator_analysis.csv"
restore

* Export flagged interviews
preserve
keep if flag_any == 1
keep hh_id psu enumerator_id duration_minutes interview_hour ///
    flag_short_duration flag_suspicious_time
export delimited using "lab4_4_flagged_interviews.csv", replace
display "Saved: lab4_4_flagged_interviews.csv"
restore

* Export high-risk enumerators
preserve
use `enum_analysis', clear
keep if risk_category == "HIGH"
export delimited using "lab4_4_high_risk_enumerators.csv", replace
display "Saved: lab4_4_high_risk_enumerators.csv"
restore

* Export summary statistics
preserve
clear
input str30 metric value
"Total Interviews" `total_n'
"Total Enumerators" `n_enumerators'
"Flagged Short Duration" `n_short'
"Flagged Suspicious Time" `n_time'
"Any Flag" `n_any'
"High Risk Enumerators" `n_high_risk'
"Medium Risk Enumerators" `n_medium_risk'
end
export delimited using "lab4_4_summary_statistics.csv", replace
display "Saved: lab4_4_summary_statistics.csv"
restore

display _n "================================================================"
display "  LAB 4.4 COMPLETE"
display "================================================================"

log close

*===============================================================================
* END OF LAB 4.4
*===============================================================================
