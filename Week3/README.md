## Week 3 — Statistical Analysis and Predictive Modeling using R

**Objective:** Perform hypothesis testing and build a predictive model in R to predict outcomes on a chosen dataset.

**Dataset:** Titanic passenger dataset (continued from Weeks 1-2, cleaned — 891 records, 19 variables).

**Key highlights:**
- Normality testing (Shapiro-Wilk) on Age and Fare
- 5 formal hypothesis tests: chi-square (Sex, Class), Welch t-test (Age), Wilcoxon rank-sum (Fare), Spearman correlation (Age vs Fare)
- Logistic regression classifier with 10-fold cross-validation
- Model diagnostics: VIF multicollinearity check, residual plots, confusion matrix, ROC curve
- Test set performance: 81.4% accuracy, 0.86 AUC

**Files in this folder:**
- `Week3_Statistical_Analysis_Predictive_Modeling_R_Report.docx` — full report
- `model_analysis.R` — complete reproducible R script
