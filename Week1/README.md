## Week 1 — Data Cleaning and Preliminary Analysis with R

**Objective:** Clean, preprocess, and perform exploratory analysis on a public dataset using R.

**Dataset:** Titanic passenger dataset (891 records, 12 variables) — public dataset with missing values and mixed categorical/numerical fields.

**Key steps covered:**
- Missing value analysis and context-aware imputation (group-wise median for Age, mode for Embarked)
- Outlier detection using the IQR method, treated via Winsorization
- Normalization (min-max, z-score) and categorical encoding (label + one-hot)
- Exploratory data analysis: descriptive statistics, survival breakdowns, correlation analysis, 7 visualizations

**Files in this folder:**
- `Week1_Data_Cleaning_R_Report.docx` — full report with code, outputs, and visuals
- `analysis.R` — complete reproducible R script
- `titanic_cleaned.csv` — final cleaned dataset
