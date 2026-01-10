********************************************************************************
*                                                                              *
*   SADC REGIONAL TRAINING WORKSHOP ON ADVANCED SAMPLING METHODS               *
*   Lab 2.4: AI/ML for Non-Response Prediction                                 *
*                                                                              *
*   STATA VERSION - Logistic Regression Model                                  *
*                                                                              *
*   Objective: Build a predictive model to identify households likely to       *
*              refuse participation, enabling targeted follow-up strategies    *
*                                                                              *
*   Data: Statistics South Africa General Household Survey 2024                *
*   Software: STATA 16+                                                        *
*                                                                              *
*   Author: SADC Workshop Materials                                            *
*   Date: March 2026                                                           *
*                                                                              *
********************************************************************************

clear all
set more off
capture log close

********************************************************************************
* CONFIGURATION - MODIFY PATHS AS NEEDED
********************************************************************************

* Data path - adjust to your local directory
global survey_path "./data/GHS_2024/ghs-2024-hhold-v1.dta"

* Output directory
global output_dir "./outputs/"

* Create output directory if needed
capture mkdir "$output_dir"

* Start log file
log using "${output_dir}lab2_4_nonresponse_prediction.log", replace

********************************************************************************
* HEADER
********************************************************************************

display as result _n "=" * 70
display as result "LAB 2.4: AI/ML FOR NON-RESPONSE PREDICTION (STATA - LOGISTIC REGRESSION)"
display as result "=" * 70 _n

display "Date: $S_DATE  Time: $S_TIME"
display "Stata version: `c(version)'" _n

********************************************************************************
* PART 1: LOAD AND EXPLORE DATA
********************************************************************************

display as result "-" * 70
display as result "PART 1: Loading and Exploring GHS 2024 Data"
display as result "-" * 70 _n

* Load the household data
use "$survey_path", clear

* Dataset summary
display "Dataset loaded successfully!"
display "  Observations: " _N
display "  Variables: " `c(k)' _n

* Check key variables
describe prov geotype totmhinc hholdsz, short

********************************************************************************
* PART 2: SIMULATE NON-RESPONSE VARIABLE
********************************************************************************

display as result _n "-" * 70
display as result "PART 2: Simulating Non-Response Patterns"
display as result "-" * 70 _n

* In real surveys, non-response is correlated with:
* - Income (higher income -> higher refusal rates)
* - Urban areas (more refusals than rural)
* - Household size (smaller households harder to contact)
* - Province (regional variation)

* Clean income variable
* Replace missing/negative with median
summarize totmhinc, detail
local med_income = r(p50)

generate income_clean = totmhinc
replace income_clean = `med_income' if income_clean == . | income_clean < 0

* Create log income
generate log_income = ln(income_clean + 1)

* Create income quintiles
xtile income_quintile = income_clean, nq(5)

* Label income quintiles
label define quintilelbl 1 "Q1 (Lowest)" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5 (Highest)"
label values income_quintile quintilelbl

* Display income distribution
tabulate income_quintile

********************************************************************************
* Simulate non-response probability
********************************************************************************

* Set seed for reproducibility
set seed 42

* Base probability of refusal (10%)
generate prob_refuse = 0.10

* Income effect (higher income -> higher refusal)
replace prob_refuse = prob_refuse + 0.15 if income_quintile == 5
replace prob_refuse = prob_refuse + 0.10 if income_quintile == 4
replace prob_refuse = prob_refuse + 0.05 if income_quintile == 3
replace prob_refuse = prob_refuse + 0.02 if income_quintile == 2

* Urban effect (geotype == 1 is urban)
replace prob_refuse = prob_refuse + 0.05 if geotype == 1

* Small household effect
replace prob_refuse = prob_refuse + 0.03 if hholdsz <= 2

* Gauteng effect (province 7 has higher refusal)
replace prob_refuse = prob_refuse + 0.05 if prov == 7

* Add random noise
generate noise = rnormal(0, 0.02)
replace prob_refuse = prob_refuse + noise
drop noise

* Bound probabilities
replace prob_refuse = 0.01 if prob_refuse < 0.01
replace prob_refuse = 0.50 if prob_refuse > 0.50

* Generate binary outcome
generate refused = runiform() < prob_refuse

* Label outcome variable
label define refusedlbl 0 "Responded" 1 "Refused"
label values refused refusedlbl
label variable refused "Household refused participation"

* Summary
display _n "Non-Response Simulation Summary:"
display "  Total households: " _N
count if refused == 1
display "  Simulated refusals: " r(N)
summarize refused
display "  Overall refusal rate: " %5.1f r(mean)*100 "%"

* Refusal rate by income quintile
display _n "Refusal Rate by Income Quintile:"
tabulate income_quintile refused, row

********************************************************************************
* PART 3: PREPARE FEATURES FOR MODEL
********************************************************************************

display as result _n "-" * 70
display as result "PART 3: Preparing Features for Logistic Regression"
display as result "-" * 70 _n

* Create dummy variables for categorical predictors
* Province dummies (base = province 1)
tabulate prov, generate(prov_d)

* Geotype dummies (base = geotype 1)
tabulate geotype, generate(geo_d)

* Income quintile dummies (base = quintile 1)
tabulate income_quintile, generate(inc_d)

* List variables for model
display "Features prepared for model:"
display "  - Province indicators (9 categories)"
display "  - Geotype indicators (3 categories)" 
display "  - Income quintile indicators (5 categories)"
display "  - Log income (continuous)"
display "  - Household size (continuous)"

* Sample size
count if !missing(refused, prov, geotype, log_income, hholdsz)
display _n "Complete cases for model: " r(N)

********************************************************************************
* PART 4: TRAIN-TEST SPLIT
********************************************************************************

display as result _n "-" * 70
display as result "PART 4: Creating Train-Test Split"
display as result "-" * 70 _n

* Create random split (70% train, 30% test)
set seed 42
generate double rand_split = runiform()
generate train = (rand_split <= 0.70)

* Verify split
tabulate train

display _n "Train-Test Split:"
count if train == 1
display "  Training set: " r(N) " (" %4.0f r(N)/_N*100 "%)"
count if train == 0
display "  Test set: " r(N) " (" %4.0f r(N)/_N*100 "%)"

* Class balance in train/test
display _n "Refusal rates by split:"
tabulate train refused, row

********************************************************************************
* PART 5: TRAIN LOGISTIC REGRESSION MODEL
********************************************************************************

display as result _n "-" * 70
display as result "PART 5: Training Logistic Regression Model"
display as result "-" * 70 _n

* Fit logistic regression on training data
display "Fitting logistic regression model on training data..."
display "(This may take a moment...)" _n

logit refused i.prov i.geotype i.income_quintile log_income hholdsz if train == 1, or

* Store estimation results
estimates store logit_model

* Display odds ratios
display _n "ODDS RATIOS (Exponentiated Coefficients):"
display "-" * 60
logit, or

********************************************************************************
* PART 6: FEATURE IMPORTANCE (ODDS RATIOS)
********************************************************************************

display as result _n "-" * 70
display as result "PART 6: Feature Importance Analysis"
display as result "-" * 70 _n

display "FEATURE IMPORTANCE (Based on Odds Ratios):"
display "=" * 60
display "An odds ratio > 1 indicates higher probability of refusal"
display "An odds ratio < 1 indicates lower probability of refusal"
display "=" * 60 _n

* Get coefficients and compute odds ratios
matrix b = e(b)
matrix V = e(V)

* Display key findings
display "KEY PREDICTORS OF NON-RESPONSE:"
display "-" * 60

* Income effect
display _n "1. INCOME QUINTILE (Base: Q1 - Lowest)"
forvalues q = 2/5 {
    local coef = _b[`q'.income_quintile]
    local or = exp(`coef')
    local se = _se[`q'.income_quintile]
    local z = `coef'/`se'
    local pval = 2*(1-normal(abs(`z')))
    local sig = ""
    if `pval' < 0.001 local sig "***"
    else if `pval' < 0.01 local sig "**"
    else if `pval' < 0.05 local sig "*"
    display "   Quintile `q': OR = " %6.3f `or' " `sig'"
}

* Geography effect
display _n "2. GEOGRAPHY TYPE (Base: Urban)"
forvalues g = 2/3 {
    capture {
        local coef = _b[`g'.geotype]
        local or = exp(`coef')
        local se = _se[`g'.geotype]
        local z = `coef'/`se'
        local pval = 2*(1-normal(abs(`z')))
        local sig = ""
        if `pval' < 0.001 local sig "***"
        else if `pval' < 0.01 local sig "**"
        else if `pval' < 0.05 local sig "*"
        display "   Geotype `g': OR = " %6.3f `or' " `sig'"
    }
}

* Continuous predictors
display _n "3. CONTINUOUS PREDICTORS"
local coef = _b[log_income]
local or = exp(`coef')
display "   Log Income: OR = " %6.3f `or' " (per unit increase)"

local coef = _b[hholdsz]
local or = exp(`coef')
display "   Household Size: OR = " %6.3f `or' " (per additional member)"

display _n "Significance: * p<0.05, ** p<0.01, *** p<0.001"

********************************************************************************
* PART 7: MODEL EVALUATION
********************************************************************************

display as result _n "-" * 70
display as result "PART 7: Model Evaluation on Test Set"
display as result "-" * 70 _n

* Predict probabilities on test set
predict prob_pred if train == 0, pr

* Create predicted class (threshold = 0.5)
generate pred_refused = (prob_pred >= 0.5) if train == 0

* Confusion matrix
display "CONFUSION MATRIX (Test Set):"
display "-" * 40
tabulate refused pred_refused if train == 0, row col

* Calculate metrics manually
* True positives, true negatives, etc.
count if refused == 1 & pred_refused == 1 & train == 0
local tp = r(N)
count if refused == 0 & pred_refused == 0 & train == 0
local tn = r(N)
count if refused == 0 & pred_refused == 1 & train == 0
local fp = r(N)
count if refused == 1 & pred_refused == 0 & train == 0
local fn = r(N)

local total_test = `tp' + `tn' + `fp' + `fn'
local accuracy = (`tp' + `tn') / `total_test' * 100
local sensitivity = `tp' / (`tp' + `fn') * 100
local specificity = `tn' / (`tn' + `fp') * 100
local precision = `tp' / (`tp' + `fp') * 100

display _n "MODEL PERFORMANCE METRICS:"
display "-" * 40
display "  Accuracy:    " %5.1f `accuracy' "%"
display "  Sensitivity: " %5.1f `sensitivity' "% (ability to detect refusals)"
display "  Specificity: " %5.1f `specificity' "% (ability to identify respondents)"
display "  Precision:   " %5.1f `precision' "% (when we predict refusal, how often correct)"

* ROC curve and AUC
display _n "Computing ROC curve and AUC..."

* Install lroc if needed (should be built-in for recent Stata)
capture roctab refused prob_pred if train == 0

* Alternative: Use estat classification
display _n "Classification Statistics:"
estat classification if train == 0

********************************************************************************
* PART 8: IDENTIFY HIGH-RISK HOUSEHOLDS
********************************************************************************

display as result _n "-" * 70
display as result "PART 8: Identifying High-Risk Households"
display as result "-" * 70 _n

* Predict probabilities for full dataset
predict prob_full, pr

* Create risk categories
generate risk_category = ""
replace risk_category = "Low" if prob_full < 0.15
replace risk_category = "Medium" if prob_full >= 0.15 & prob_full < 0.25
replace risk_category = "High" if prob_full >= 0.25 & prob_full < 0.35
replace risk_category = "Very High" if prob_full >= 0.35

* Risk distribution
display "RISK STRATIFICATION RESULTS:"
display "=" * 60
tabulate risk_category refused, row col

* Summary by risk category
display _n "Risk Category Summary:"
tabstat refused if risk_category != "", by(risk_category) statistics(n mean) nototal

* Count high-risk households
count if risk_category == "Very High"
local very_high_n = r(N)
count if risk_category == "High"
local high_n = r(N)

display _n "OPERATIONAL RECOMMENDATIONS:"
display "-" * 60
display "Based on the model predictions:" _n

display "1. VERY HIGH RISK households: " `very_high_n'
display "   -> Schedule senior interviewers"
display "   -> Plan multiple contact attempts"
display "   -> Prepare incentive protocols" _n

display "2. HIGH RISK households: " `high_n'
display "   -> Assign experienced interviewers"
display "   -> Allow flexible appointment times" _n

display "3. MEDIUM/LOW RISK households:"
display "   -> Standard interview protocols"
display "   -> Regular follow-up procedures"

********************************************************************************
* PART 9: EXPORT RESULTS
********************************************************************************

display as result _n "-" * 70
display as result "PART 9: Exporting Results"
display as result "-" * 70 _n

* Export coefficients and odds ratios
matrix results = r(table)
esttab logit_model using "${output_dir}lab2_4_logit_results.csv", ///
    cells(b(fmt(4)) se(fmt(4)) t(fmt(2)) p(fmt(4))) ///
    eform label csv replace
    
display "Logistic regression results saved to: ${output_dir}lab2_4_logit_results.csv"

* Export odds ratios table
esttab logit_model using "${output_dir}lab2_4_odds_ratios.rtf", ///
    eform ci obslast label ///
    title("Logistic Regression: Predictors of Survey Non-Response") ///
    mtitles("Odds Ratio") ///
    replace

display "Odds ratios table saved to: ${output_dir}lab2_4_odds_ratios.rtf"

* Export risk scores (sample of 1000)
preserve
    keep prov geotype income_quintile hholdsz prob_full risk_category
    keep in 1/1000
    export delimited using "${output_dir}lab2_4_risk_scores_sample.csv", replace
restore

display "Risk scores sample saved to: ${output_dir}lab2_4_risk_scores_sample.csv"

* Export feature importance summary
file open importance using "${output_dir}lab2_4_feature_importance_stata.txt", write replace

file write importance "=" * 60 _n
file write importance "FEATURE IMPORTANCE ANALYSIS - LOGISTIC REGRESSION" _n
file write importance "=" * 60 _n _n

file write importance "Model: Logistic Regression predicting survey non-response" _n
file write importance "Data: GHS 2024 Household Data" _n
file write importance "Sample size: " (_N) " households" _n _n

file write importance "KEY PREDICTORS (Odds Ratios):" _n
file write importance "-" * 40 _n

file write importance "Income Quintile (vs Q1 - Lowest):" _n
forvalues q = 2/5 {
    local or = exp(_b[`q'.income_quintile])
    file write importance "  Q`q': OR = " %6.3f (`or') _n
}

file write importance _n "Log Income: OR = " %6.3f (exp(_b[log_income])) _n
file write importance "Household Size: OR = " %6.3f (exp(_b[hholdsz])) _n

file write importance _n "INTERPRETATION:" _n
file write importance "OR > 1: Higher probability of refusal" _n
file write importance "OR < 1: Lower probability of refusal" _n
file write importance "OR = 1: No effect" _n

file close importance

display "Feature importance summary saved to: ${output_dir}lab2_4_feature_importance_stata.txt"

********************************************************************************
* PART 10: SUMMARY
********************************************************************************

display as result _n "=" * 70
display as result "LAB 2.4 COMPLETE: AI/ML Non-Response Prediction (STATA)"
display as result "=" * 70 _n

display "SUMMARY:"
display "-" * 40
display "Model: Logistic Regression"
count if train == 1
display "Training observations: " r(N)
count if train == 0
display "Test observations: " r(N)
display "Accuracy: " %5.1f `accuracy' "%"

display _n "TOP PREDICTORS OF NON-RESPONSE:"
display "  1. Income Quintile (higher income -> higher refusal)"
display "  2. Geography Type (urban -> higher refusal)"
display "  3. Household Size (smaller -> higher refusal)"

display _n "OUTPUT FILES:"
display "  - lab2_4_logit_results.csv (model coefficients)"
display "  - lab2_4_odds_ratios.rtf (formatted odds ratios table)"
display "  - lab2_4_risk_scores_sample.csv (household risk scores)"
display "  - lab2_4_feature_importance_stata.txt (importance summary)"
display "  - lab2_4_nonresponse_prediction.log (session log)"

display _n "KEY INSIGHT:"
display "-" * 40
display "The logistic regression model identifies households with"
display "elevated refusal risk BEFORE fieldwork begins, enabling:"
display "  - Targeted interviewer assignment"
display "  - Optimized contact strategies"  
display "  - Proactive non-response mitigation"

display _n "COMPARISON TO R RANDOM FOREST:"
display "-" * 40
display "Logistic Regression:"
display "  + Interpretable coefficients (odds ratios)"
display "  + Statistical inference (p-values, CIs)"
display "  + Linear relationships only"
display _n "Random Forest:"
display "  + Captures non-linear relationships"
display "  + Often better prediction accuracy"
display "  - Less interpretable (black box)"

display _n "=" * 70
display "END OF LAB 2.4 (STATA VERSION)"
display "=" * 70

log close

********************************************************************************
* END OF SCRIPT
********************************************************************************
