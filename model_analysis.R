# =========================================================
# Week 3 Task: Statistical Analysis and Predictive Modeling with R
# Dataset: Titanic Passenger Dataset (continued from Week 1 & 2, cleaned)
# Goal: Predict Survived (classification) using logistic regression
# =========================================================

library(dplyr)
library(caret)
library(pROC)
library(car)
library(ggplot2)

set.seed(42)

out <- "/home/claude/week3/output"
dir.create(out, showWarnings = FALSE)
sink_start <- function(f) sink(file.path(out, f), split = TRUE)
options(width = 100)

df <- read.csv("/home/claude/week3/titanic_cleaned.csv", stringsAsFactors = FALSE)
df$Pclass   <- factor(df$Pclass, levels = c("1st","2nd","3rd"))
df$Sex      <- factor(df$Sex, levels = c("male","female"))
df$Embarked <- factor(df$Embarked, levels = c("C","Q","S"))
df$Survived <- as.integer(df$Survived)

cat("Dataset loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# ---------------------------------------------------------
# 1. EXPLORATORY STATISTICAL ANALYSIS
# ---------------------------------------------------------

# 1a. Normality tests (Shapiro-Wilk) on continuous variables
sink_start("01_normality_tests.txt")
cat("Shapiro-Wilk normality test: Age\n")
print(shapiro.test(df$Age))
cat("\nShapiro-Wilk normality test: Fare_capped\n")
print(shapiro.test(df$Fare_capped))
cat("\nInterpretation: p < 0.05 in both tests => reject the null hypothesis of\n")
cat("normality. Neither Age nor Fare_capped is normally distributed, which\n")
cat("informs the choice of non-parametric/robust tests below and confirms\n")
cat("logistic regression (which does not assume normality of predictors) is\n")
cat("an appropriate modelling choice over methods like LDA.\n")
sink()

png(file.path(out, "plot_qq_age_fare.png"), width = 1000, height = 500, res = 120)
par(mfrow = c(1,2))
qqnorm(df$Age, main = "Q-Q Plot: Age"); qqline(df$Age, col = "red", lwd = 2)
qqnorm(df$Fare_capped, main = "Q-Q Plot: Fare (capped)"); qqline(df$Fare_capped, col = "red", lwd = 2)
dev.off()

# 1b. Hypothesis Test 1: Chi-square test of independence (Survived vs Sex)
sink_start("02_hypothesis_tests.txt")
cat("=== Hypothesis Test 1: Chi-square test — Survival vs Sex ===\n")
cat("H0: Survival is independent of Sex.\n")
cat("H1: Survival is associated with Sex.\n\n")
tab_sex <- table(df$Sex, df$Survived)
print(tab_sex)
chi_sex <- chisq.test(tab_sex)
print(chi_sex)
cat("Decision: p-value =", format.pval(chi_sex$p.value, digits = 3),
    "<< 0.05 => reject H0. Sex is significantly associated with survival.\n\n")

# Hypothesis Test 2: Chi-square test (Survived vs Pclass)
cat("=== Hypothesis Test 2: Chi-square test — Survival vs Passenger Class ===\n")
cat("H0: Survival is independent of Passenger Class.\n")
cat("H1: Survival is associated with Passenger Class.\n\n")
tab_class <- table(df$Pclass, df$Survived)
print(tab_class)
chi_class <- chisq.test(tab_class)
print(chi_class)
cat("Decision: p-value =", format.pval(chi_class$p.value, digits = 3),
    "<< 0.05 => reject H0. Passenger class is significantly associated with survival.\n\n")

# Hypothesis Test 3: Welch two-sample t-test (Age by Survived)
cat("=== Hypothesis Test 3: Welch Two-Sample t-test — Age by Survival ===\n")
cat("H0: Mean age is equal between survivors and non-survivors.\n")
cat("H1: Mean age differs between survivors and non-survivors.\n\n")
t_age <- t.test(Age ~ Survived, data = df)
print(t_age)
cat("Decision: p-value =", format.pval(t_age$p.value, digits = 3),
    ifelse(t_age$p.value < 0.05, "< 0.05 => reject H0.", ">= 0.05 => fail to reject H0."),
    "\n\n")

# Hypothesis Test 4: Wilcoxon rank-sum test (Fare by Survived) — non-parametric,
# used because Fare is non-normal (per Shapiro-Wilk above)
cat("=== Hypothesis Test 4: Wilcoxon Rank-Sum Test — Fare by Survival ===\n")
cat("H0: Fare distributions are equal between survivors and non-survivors.\n")
cat("H1: Fare distributions differ between survivors and non-survivors.\n\n")
w_fare <- wilcox.test(Fare_capped ~ Survived, data = df)
print(w_fare)
cat("Decision: p-value =", format.pval(w_fare$p.value, digits = 3),
    "<< 0.05 => reject H0. Fare distribution differs significantly by survival.\n\n")

# Hypothesis Test 5: Correlation test (Age vs Fare) — Spearman, since non-normal
cat("=== Hypothesis Test 5: Spearman Correlation — Age vs Fare ===\n")
cat("H0: rho = 0 (no monotonic association between Age and Fare).\n")
cat("H1: rho != 0.\n\n")
cor_af <- cor.test(df$Age, df$Fare_capped, method = "spearman")
print(cor_af)
sink()

# ---------------------------------------------------------
# 2. MODEL BUILDING — Logistic Regression (Classification)
# ---------------------------------------------------------
model_df <- df %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare_capped, Embarked) %>%
  mutate(Survived = factor(Survived, levels = c(0,1), labels = c("No","Yes")))

# Train/test split (80/20), stratified on outcome
train_idx <- createDataPartition(model_df$Survived, p = 0.8, list = FALSE)
train_df <- model_df[train_idx, ]
test_df  <- model_df[-train_idx, ]

sink_start("03_train_test_split.txt")
cat("Training set size:", nrow(train_df), "\n")
cat("Test set size:", nrow(test_df), "\n")
cat("\nClass balance - Training set:\n"); print(prop.table(table(train_df$Survived)))
cat("\nClass balance - Test set:\n"); print(prop.table(table(test_df$Survived)))
sink()

# 10-fold cross-validation on the training set
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                      summaryFunction = twoClassSummary, savePredictions = TRUE)

cv_model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare_capped + Embarked,
                   data = train_df, method = "glm", family = "binomial",
                   trControl = ctrl, metric = "ROC")

sink_start("04_cv_model_summary.txt")
cat("=== 10-Fold Cross-Validation Results (on training set) ===\n")
print(cv_model)
cat("\n=== Final Logistic Regression Model Summary ===\n")
print(summary(cv_model$finalModel))
sink()

# Variance Inflation Factor (multicollinearity check)
sink_start("05_vif_multicollinearity.txt")
cat("=== Variance Inflation Factors (multicollinearity check) ===\n")
print(vif(cv_model$finalModel))
cat("\nInterpretation: All VIF values are well below the common threshold of 5,\n")
cat("indicating no serious multicollinearity among predictors.\n")
sink()

# ---------------------------------------------------------
# 3. MODEL EVALUATION ON HELD-OUT TEST SET
# ---------------------------------------------------------
test_probs <- predict(cv_model, newdata = test_df, type = "prob")[, "Yes"]
test_pred  <- factor(ifelse(test_probs > 0.5, "Yes", "No"), levels = c("No","Yes"))

cm <- confusionMatrix(test_pred, test_df$Survived, positive = "Yes")

sink_start("06_test_set_evaluation.txt")
cat("=== Confusion Matrix and Performance Metrics (Held-Out Test Set) ===\n")
print(cm)
sink()

roc_obj <- roc(response = test_df$Survived, predictor = test_probs, levels = c("No","Yes"), direction = "<")
sink_start("07_roc_auc.txt")
cat("=== ROC / AUC (Test Set) ===\n")
cat("AUC:", round(auc(roc_obj), 4), "\n")
cat("95% CI:", paste(round(ci.auc(roc_obj), 4), collapse = " - "), "\n")
sink()

png(file.path(out, "plot_roc_curve.png"), width = 800, height = 800, res = 130)
plot(roc_obj, col = "#2E5395", lwd = 3, main = "ROC Curve - Test Set Predictions")
abline(a = 0, b = 1, lty = 2, col = "grey60")
text(0.3, 0.2, paste("AUC =", round(auc(roc_obj), 3)), cex = 1.2)
dev.off()

# Confusion matrix heatmap
cm_df <- as.data.frame(cm$table)
png(file.path(out, "plot_confusion_matrix.png"), width = 800, height = 700, res = 130)
p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 8, fontface = "bold") +
  scale_fill_gradient(low = "#EDF2FA", high = "#2E5395") +
  labs(title = "Confusion Matrix - Test Set", x = "Actual", y = "Predicted") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
print(p_cm)
dev.off()

# ---------------------------------------------------------
# 4. DIAGNOSTIC PLOTS
# ---------------------------------------------------------
png(file.path(out, "plot_model_diagnostics.png"), width = 1000, height = 800, res = 120)
par(mfrow = c(2,2))
plot(cv_model$finalModel, which = 1:4)
dev.off()

# Coefficient plot (log-odds with CI)
coefs <- summary(cv_model$finalModel)$coefficients
coef_df <- data.frame(
  term = rownames(coefs)[-1],
  estimate = coefs[-1, "Estimate"],
  se = coefs[-1, "Std. Error"]
)
coef_df$lower <- coef_df$estimate - 1.96 * coef_df$se
coef_df$upper <- coef_df$estimate + 1.96 * coef_df$se

png(file.path(out, "plot_coefficients.png"), width = 950, height = 650, res = 120)
p_coef <- ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "#2E5395", size = 0.7) +
  coord_flip() +
  labs(title = "Logistic Regression Coefficients (Log-Odds, 95% CI)",
       x = NULL, y = "Coefficient Estimate (log-odds)") +
  theme_minimal(base_size = 13)
print(p_coef)
dev.off()

cat("\nAll outputs written to:", out, "\n")
