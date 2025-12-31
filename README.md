# 🩺 Kidney Function and Hypertension

**A Statistical & Causal Analysis using NHANES (2003–2004)**

📊 *Computational Statistics | R Programming | Causal Inference*

## 📌 Project Overview

This project investigates the **relationship between kidney dysfunction and hypertension**, with a particular focus on whether **impaired kidney function contributes to elevated blood pressure via fluid retention**.

Using data from the **National Health and Nutrition Examination Survey (NHANES 2003–2004)**, the study applies:

* Exploratory Data Analysis (EDA)
* Hypothesis testing
* Multivariable regression
* Logistic regression
* Mediation (causal) analysis
* Predictive modeling and validation

The analysis is implemented entirely in **R**, following a **modular, reproducible, and research-oriented structure**.

## 🎯 Research Questions

* Does impaired kidney function (measured using **Creatinine** and **Albumin-to-Creatinine Ratio, ACR**) influence blood pressure?
* Does **fluid retention (extracellular fluid)** mediate the relationship between kidney dysfunction and hypertension?
* Can kidney health markers be used to **predict hypertension risk**?
* How do these relationships vary across demographic and health subgroups?

## 🧠 Key Concepts & Methods

* **Causal framework** using Directed Acyclic Graphs (DAGs)
* **Log-transformation** of skewed biomarkers (ACR)
* **Multivariable linear regression** for continuous BP outcomes
* **Logistic regression** for hypertension classification
* **Mediation analysis** to study indirect effects via fluid retention
* **Model evaluation** using ROC curves and cross-validation

## 🗂️ Repository Structure

```text
├── data_loading/
│   └── load_data.R              # NHANES data import and preprocessing
│
├── eda/
│   └── eda.R                    # Descriptive statistics and distributions
│
├── tests/
│   └── tests.R                  # Z-tests, chi-square tests
│
├── models results/
│   └── Table showing the final results
│
├── reports/
│   ├── Final_report.pdf         # Full statistical report
│   └── final_p.html             # HTML + R Markdown combined report
│
├── README.md
```

## 📈 Key Findings

* **Log-transformed ACR** is a strong and significant predictor of systolic blood pressure.
* Each unit increase in log(ACR) increases:

  * **SBP by ~3.36 mmHg**
  * **Odds of hypertension by ~70%**
* **Creatinine alone** shows weak direct association with BP, suggesting indirect pathways.
* **Extracellular fluid** correlates with SBP, supporting a **fluid-retention mediation hypothesis**.
* Predictive model achieved:

  * **ROC AUC ≈ 0.76**
  * High sensitivity for hypertension detection

## 🛠️ Technologies Used

* **Language:** R
* **Libraries:**

  * `tidyverse`
  * `ggplot2`
  * `dplyr`
  * `mediation`
  * `caret`
  * `pROC`
* **Data Source:** NHANES (CDC)

To run:

```r
source("data_loading/load_data.R")
source("eda/eda.R")
source("tests/tests.R")
```

## ⭐ Why This Project Matters
This project demonstrates:
* Strong **statistical thinking**
* Real-world **healthcare data analysis**
* **Causal reasoning beyond correlation**
* Clean, production-quality **R code organization**
