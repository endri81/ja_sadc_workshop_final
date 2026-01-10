# OPERATIONAL WORKFLOW GUIDE
## SADC Advanced Sampling Workshop — Content Creation Companion
### Practical Implementation Details

---

## 🔄 MODULAR WORKFLOW: STEP-BY-STEP

### Step 1: Day Initialization

When starting a new day, use this checklist:

```
USER: "START DAY [X]"

AI RESPONSE:
✅ Confirm Day X: [Theme]
✅ Review learning objectives (LO X.1 through X.N)
✅ List modules with slide ranges
✅ Identify required lab scripts
✅ Confirm Zambara narrative position
✅ Request any outstanding user inputs

THEN: Await user confirmation before proceeding
```

### Step 2: Lab Script Creation (MANDATORY BEFORE SLIDES)

For any module with numerical results:

```
USER: "LAB SCRIPT [X.Y]"

AI DELIVERS:
1. R Script: scripts/ModuleX/R/labX_Y_topic.R
2. STATA Script: scripts/ModuleX/STATA/labX_Y_topic.do
3. Expected outputs description
4. Instructions for running

USER RUNS LOCALLY → PROVIDES OUTPUT → THEN REQUEST SLIDES
```

### Step 3: Slide Batch Creation

```
USER: "SLIDES [A-B] DAY X MODULE Y"

AI DELIVERS:
1. Complete LaTeX code (compilable standalone)
2. Instructor speaking notes (bullet format)
3. Quality checklist (completed)
4. Compilation tips if special elements used

BATCH LIMIT: Maximum 15 slides per request
```

### Step 4: Review and Correction

```
USER: "FIX SLIDE [N]: [issue description]"

AI PROVIDES:
1. Corrected LaTeX for specific slide
2. Explanation of what was wrong
3. Verification that fix maintains consistency
```

---

## 📝 TEMPLATE: LAB SCRIPT (R VERSION)

```r
#===============================================================================
# SADC REGIONAL TRAINING WORKSHOP ON ADVANCED SAMPLING METHODS
# Lab Exercise X.Y: [Title]
# Day X: [Theme]
#
# Author: Workshop Facilitator
# Date: March 2026
#
# Description:
#   [Brief description of what this lab accomplishes]
#
# Learning Objectives Addressed:
#   - LO X.Y: [Specific objective]
#
# Data Requirements:
#   - Zambara Household Survey (based on GHS 2024)
#   - File: data/GHS_2024/ghs-2024-hhold-v1.dta
#
# Required Packages:
#   - haven (data import)
#   - survey (complex survey analysis)
#   - dplyr (data manipulation)
#===============================================================================

#-------------------------------------------------------------------------------
# SECTION 1: ENVIRONMENT SETUP
#-------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Set working directory (user should adjust)
# setwd("path/to/ja_sadc_workshop_final")

# Load required packages
library(haven)      # For reading .dta files
library(survey)     # For complex survey analysis
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization

# Set options for cleaner output
options(survey.lonely.psu = "adjust")  # Handle single-PSU strata

#-------------------------------------------------------------------------------
# SECTION 2: DATA LOADING AND PREPARATION
#-------------------------------------------------------------------------------

# Load Zambara (South Africa GHS) household data
zambara_hh <- read_dta("data/GHS_2024/ghs-2024-hhold-v1.dta")

# Quick data inspection
cat("Dataset dimensions:", nrow(zambara_hh), "households,", 
    ncol(zambara_hh), "variables\n")

# Rename for Zambara narrative (optional for consistency)
# Province mapping:
# Western Cape → Southern Cape
# Gauteng → Zambara Capital Region
# etc.

#-------------------------------------------------------------------------------
# SECTION 3: [MAIN ANALYSIS - CUSTOMIZE PER LAB]
#-------------------------------------------------------------------------------

# Example: Define survey design
zambara_design <- svydesign(
  id = ~psu,                    # Primary sampling unit
  strata = ~stratum,            # Stratification variable
  weights = ~house_wgt,         # Household weight
  data = zambara_hh,
  nest = TRUE                   # PSUs nested within strata
)

# Print design summary
summary(zambara_design)

#-------------------------------------------------------------------------------
# SECTION 4: OUTPUT FOR SLIDES
#-------------------------------------------------------------------------------

# Key results to copy into slides:
cat("\n========== RESULTS FOR SLIDES ==========\n")

# Example output
cat("Number of PSUs:", length(unique(zambara_hh$psu)), "\n")
cat("Number of strata:", length(unique(zambara_hh$stratum)), "\n")

#-------------------------------------------------------------------------------
# SECTION 5: EXPORT RESULTS
#-------------------------------------------------------------------------------

# Save key results for reference
results_summary <- data.frame(
  Metric = c("Total Households", "PSUs", "Strata"),
  Value = c(nrow(zambara_hh), 
            length(unique(zambara_hh$psu)),
            length(unique(zambara_hh$stratum)))
)

write.csv(results_summary, "outputs/labX_Y_summary.csv", row.names = FALSE)

cat("\n✅ Lab X.Y complete. Results exported to outputs/labX_Y_summary.csv\n")

#===============================================================================
# END OF LAB X.Y
#===============================================================================
```

---

## 📝 TEMPLATE: LAB SCRIPT (STATA VERSION)

```stata
*===============================================================================
* SADC REGIONAL TRAINING WORKSHOP ON ADVANCED SAMPLING METHODS
* Lab Exercise X.Y: [Title]
* Day X: [Theme]
*
* Author: Workshop Facilitator
* Date: March 2026
*
* Description:
*   [Brief description of what this lab accomplishes]
*
* Learning Objectives Addressed:
*   - LO X.Y: [Specific objective]
*
* Data Requirements:
*   - Zambara Household Survey (based on GHS 2024)
*   - File: data/GHS_2024/ghs-2024-hhold-v1.dta
*===============================================================================

*-------------------------------------------------------------------------------
* SECTION 1: ENVIRONMENT SETUP
*-------------------------------------------------------------------------------

clear all
set more off
capture log close

* Start log file
log using "outputs/labX_Y_log.smcl", replace

* Set working directory (user should adjust)
* cd "path/to/ja_sadc_workshop_final"

*-------------------------------------------------------------------------------
* SECTION 2: DATA LOADING AND PREPARATION
*-------------------------------------------------------------------------------

* Load Zambara (South Africa GHS) household data
use "data/GHS_2024/ghs-2024-hhold-v1.dta", clear

* Quick data inspection
describe, short
di "Dataset has " _N " households"

*-------------------------------------------------------------------------------
* SECTION 3: [MAIN ANALYSIS - CUSTOMIZE PER LAB]
*-------------------------------------------------------------------------------

* Define survey design
svyset psu [pw=house_wgt], strata(stratum) singleunit(centered)

* Display survey design settings
svydescribe

*-------------------------------------------------------------------------------
* SECTION 4: OUTPUT FOR SLIDES
*-------------------------------------------------------------------------------

di _newline
di "========== RESULTS FOR SLIDES =========="

* Example output
qui tab psu
di "Number of PSUs: " r(r)

qui tab stratum  
di "Number of strata: " r(r)

*-------------------------------------------------------------------------------
* SECTION 5: EXPORT RESULTS
*-------------------------------------------------------------------------------

* Preserve data and create results summary
preserve
    clear
    set obs 3
    gen Metric = ""
    gen Value = .
    
    replace Metric = "Total Households" in 1
    replace Value = 20940 in 1  // Update with actual count
    
    replace Metric = "PSUs" in 2
    replace Value = 3218 in 2   // Update with actual count
    
    replace Metric = "Strata" in 3
    replace Value = 248 in 3    // Update with actual count
    
    export delimited using "outputs/labX_Y_summary.csv", replace
restore

di _newline
di "✅ Lab X.Y complete. Results exported to outputs/labX_Y_summary.csv"

log close

*===============================================================================
* END OF LAB X.Y
*===============================================================================
```

---

## 📝 TEMPLATE: SLIDE BATCH DELIVERY

When AI delivers slides, format as:

```
============================================================
SLIDE BATCH: Day [X], Module [Y], Slides [A-B]
Topic: [Module Title]
============================================================

[LATEX CODE BLOCK - COMPLETE AND COMPILABLE]

============================================================
INSTRUCTOR NOTES
============================================================

Slide A: [Title]
- Key point to emphasize: ...
- Common question to anticipate: ...
- Transition to next slide: ...

Slide A+1: [Title]
- Key point to emphasize: ...
...

============================================================
QUALITY CHECKLIST ✓
============================================================
☑ No text overflow or truncation
☑ All figures render within bounds
☑ Consistent spacing throughout
☑ One main concept per slide
☑ All claims have citations
☑ Zambara narrative consistent
☑ Code verified (if applicable)
☑ Font sizes ≥ 9pt
☑ Color contrast sufficient
☑ Learning objective addressed

============================================================
NEXT BATCH PREVIEW
============================================================
Slides [B+1] through [C] will cover: [brief description]
Required before proceeding: [any dependencies]
```

---

## 🎯 EXAMPLE: COMPLETE SLIDE BATCH

Below is an example of what a delivered batch looks like:

```latex
%===============================================================================
% SADC WORKSHOP - DAY 1, MODULE 3: MULTI-STAGE STRATIFIED DESIGN
% Slides 23-26 (4 slides)
%===============================================================================

%--- Slide 23: Module Title Slide ---
\begin{frame}[plain]
\begin{tikzpicture}[remember picture, overlay]
  \fill[sadcblue] (current page.north west) rectangle (current page.south east);
  \node at (current page.center) {
    \begin{minipage}{0.8\textwidth}
      \centering
      \color{white}
      {\Huge\bfseries Multi-Stage Stratified Design}\\[0.8cm]
      {\Large for HES, LFS, and Multi-Purpose Surveys}\\[1cm]
      {\normalsize Day 1: Advanced Sample Design Foundations}
    \end{minipage}
  };
\end{tikzpicture}
\end{frame}

%--- Slide 24: The Challenge ---
\begin{frame}{Why Multi-Stage Sampling?}

\begin{storybox}
\small Lindiwe examines Zambara's previous survey: a simple two-stage design 
with provinces as strata. The resulting estimates for urban areas were 
excellent, but rural estimates had CVs exceeding 25\%.
\end{storybox}

\vspace{0.4cm}

\textbf{The fundamental trade-off:}

\begin{itemize}
  \item More stages $\rightarrow$ lower field costs
  \item More stages $\rightarrow$ higher design effect
  \item \alert{Key question:} How do we optimize this trade-off?
\end{itemize}

\vfill
\tiny\textcolor{gray}{Source: Cochran (1977), Chapter 10; Lohr (2022), Chapter 5}
\end{frame}

%--- Slide 25: Design Effect Formula ---
\begin{frame}{Quantifying the Cost: Design Effect}

\begin{defbox}[Design Effect (DEFF)]
\begin{equation}
DEFF = \frac{Var(\hat{\theta}_{complex})}{Var(\hat{\theta}_{SRS})}
\end{equation}
\end{defbox}

\vspace{0.3cm}

For stratified cluster sampling with equal cluster sizes:

\begin{equation}
DEFF \approx 1 + (b - 1) \cdot \rho
\end{equation}

\textbf{Where:}
\begin{description}[leftmargin=2cm, style=nextline]
  \item[$b$] Average cluster size (households per PSU)
  \item[$\rho$] Intraclass correlation coefficient (ICC)
\end{description}

\vfill
\tiny\textcolor{gray}{Source: Kish (1965); Lohr (2022), Section 5.5}
\end{frame}

%--- Slide 26: Zambara Example ---
\begin{frame}{Zambara Household Survey: Design Effect Analysis}

\begin{storybox}
\small From the GHS microdata, Lindiwe calculates the design effect 
for household income estimation.
\end{storybox}

\vspace{0.3cm}

\begin{table}[h]
\centering
\small
\begin{tabular}{lrr}
\toprule
\textbf{Parameter} & \textbf{Value} & \textbf{Source} \\
\midrule
Total households & 20,940 & Sample size \\
Number of PSUs & 3,218 & Cluster count \\
Avg. cluster size ($b$) & 6.5 & $n/m$ \\
Est. ICC ($\rho$) for income & 0.05 & Lab 1.3 output \\
\midrule
\textbf{Design Effect} & \textbf{1.28} & Formula \\
\bottomrule
\end{tabular}
\end{table}

\begin{keybox}
\small The design effect of 1.28 means we need 28\% more observations 
than SRS to achieve equivalent precision.
\end{keybox}

\vfill
\tiny\textcolor{gray}{Data: Statistics South Africa GHS 2024 (anonymized)}
\end{frame}
```

---

## 📊 EXAMPLE OUTPUT VERIFICATION FORMAT

When user runs lab scripts, they should report results like:

```
============================================================
LAB 1.3 OUTPUT VERIFICATION
============================================================

R OUTPUT:
---------
Design Effect (totmhinc): 1.284
Number of PSUs: 3218
Number of Strata: 248
Average cluster size: 6.51

STATA OUTPUT:
-------------
Design Effect (totmhinc): 1.284
Number of PSUs: 3218
Number of Strata: 248
Average cluster size: 6.51

VERIFIED: ✅ Results match
============================================================
```

---

## 🚨 CRITICAL DO's AND DON'Ts

### DO:
- ✅ Start every numerical slide with "Based on Lab X.Y output..."
- ✅ Include both R and STATA code for every software example
- ✅ Use Zambara names consistently (never "South Africa" in exercises)
- ✅ Cite methodology sources on every technical slide
- ✅ Keep one main idea per slide
- ✅ Include transition language in instructor notes
- ✅ Build complex ideas across multiple slides
- ✅ Use the custom tcolorbox environments for visual consistency

### DON'T:
- ❌ Never invent numbers without lab script verification
- ❌ Never exceed 6 lines of body text per slide
- ❌ Never use font size smaller than 9pt
- ❌ Never skip the quality checklist
- ❌ Never deliver more than 15 slides per batch
- ❌ Never use inconsistent Zambara geography names
- ❌ Never include tea/lunch breaks in slide content
- ❌ Never assume R package is loaded without explicit library() call

---

## 📁 FILE ORGANIZATION

```
ja_sadc_workshop_final/
├── data/
│   ├── GHS_2024/
│   │   ├── ghs-2024-hhold-v1.dta       # Household microdata
│   │   └── ghs-2024-person-v1.dta       # Person microdata
│   ├── shapefiles/                       # Geospatial data
│   └── synthetic/                        # Any generated test data
├── scripts/
│   ├── Module1/
│   │   ├── R/
│   │   │   ├── lab1_3_design_effect.R
│   │   │   └── lab1_4_optimal_allocation.R
│   │   └── STATA/
│   │       ├── lab1_3_design_effect.do
│   │       └── lab1_4_optimal_allocation.do
│   ├── Module2/ ... Module5/
├── lectures/
│   ├── Day1_Advanced_Design_Foundations.tex
│   ├── Day2_Computational_Tools.tex
│   ├── Day3_Weighting_Variance.tex
│   ├── Day4_Error_Mitigation.tex
│   └── Day5_Synthesis_Longitudinal.tex
├── outputs/
│   └── [exported results from lab scripts]
└── exercises/
    └── [participant exercise sheets if needed]
```

---

## 🔗 KNOWLEDGE PROJECT INTEGRATION

Upload these files to Claude Project knowledge:

1. **This System Prompt** (`SADC_Workshop_Claude_Project_System_Prompt.md`)
2. **This Operational Guide** (`SADC_Workshop_Operational_Guide.md`)
3. **Concept Note** (`SADC_Concept_Note_Advanced_Sampling_Workshop_REVISED_27_11.docx`)
4. **GHS Metadata PDF** (`ghs-2024-metadata.pdf`)
5. **GHS Release Notes** (`ghs-2024-release.pdf`)
6. **GHS Questionnaire** (`q-ghs-2024-capi.pdf`)
7. **Key Reference PDFs** (UN Sampling Guidelines, selected chapters)

Data files (.dta, .csv) can be referenced but should be run locally.

---

*Operational Guide v1.0 — Companion to System Prompt*
*SADC Regional Training Workshop, March 2026*
