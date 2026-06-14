# NHANES Kidney Function, Fluid Retention, and Blood Pressure Analysis

This project analyzes NHANES 2003-2004 data to study whether kidney dysfunction, measured with albumin-to-creatinine ratio (ACR), is associated with systolic blood pressure and hypertension risk, and whether extracellular fluid may mediate that relationship.

## Research Questions

- Is albuminuria stage associated with higher systolic blood pressure?
- Is log-transformed ACR associated with SBP after adjusting for age, gender, BMI, and diabetes status?
- Does extracellular fluid mediate the relationship between ACR and SBP?
- How well do ACR and demographic/clinical variables predict hypertension?

## Data

The analysis uses NHANES 2003-2004 tables:

- `DEMO_C`: demographics
- `BIX_C`: bioelectrical impedance / extracellular fluid
- `BMX_C`: body measures
- `BPX_C`: blood pressure readings
- `DIQ_C`: diabetes questionnaire
- `L16_C.xpt`: urine albumin and creatinine lab file

The local lab file is stored at `data/raw/L16_C.xpt`. Other NHANES tables are downloaded by `nhanesA` when the analysis is run.

## Methods

The workflow cleans and joins NHANES tables, calculates average systolic and diastolic blood pressure, computes ACR, assigns albuminuria stages, removes pregnant participants, and models hypertension status.

Main methods include:

- Exploratory data analysis and correlation heatmaps
- Albuminuria-stage comparisons with ANOVA and Tukey tests
- Linear regression for SBP
- Logistic regression and 5-fold cross-validation for hypertension prediction
- Causal mediation analysis using extracellular fluid as mediator

## Headline Results

- Mean SBP increased by albuminuria stage: A1 about 117, A2 about 124, and A3 about 136 mmHg.
- SBP differed significantly across ACR stages: ANOVA `p = 8.23e-10`.
- In adjusted linear regression, `log_ACR` was positively associated with SBP: coefficient `3.356`, `p < 2e-16`.
- Extracellular fluid was positively correlated with SBP: correlation `0.274`, `p < 2.2e-16`.
- The logistic hypertension model had cross-validated ROC AUC around `0.753`.
- Mediation analysis found a significant direct effect of log-ACR on SBP, but the indirect effect through extracellular fluid was not statistically significant.

## Repository Structure

```text
.
├── README.md
├── requirements.R
├── R/
│   ├── 01_load_data.R
│   ├── 02_clean_transform.R
│   ├── 03_eda.R
│   ├── 04_models.R
│   ├── 05_hypothesis_tests.R
│   └── run_analysis.R
├── data/
│   ├── raw/
│   │   └── L16_C.xpt
│   └── processed/
├── figures/
├── reports/
│   ├── final_analysis.html
│   └── final_report.pdf
└── results/
    └── mediation_summary_original.csv
```

## Reproduce The Analysis

Install dependencies:

```r
source("requirements.R")
```

Run the full analysis:

```r
source("R/run_analysis.R")
```

This will create processed data in `data/processed/`, result tables in `results/`, and figures in `figures/`.

## Existing Reports

- Rendered HTML analysis: `reports/final_analysis.html`
- Final PDF report: `reports/final_report.pdf`
- Original mediation summary table: `results/mediation_summary_original.csv`
