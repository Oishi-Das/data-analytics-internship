# =========================================================
# Week 1 Task: Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic Passenger Dataset (Kaggle / public source)
# =========================================================

library(ggplot2)
library(dplyr)
library(corrplot)

out <- "/home/claude/week1/output"
dir.create(out, showWarnings = FALSE)
sink_start <- function(f) sink(file.path(out, f), split = TRUE)

options(width = 100)

# ---------------------------------------------------------
# 1. LOAD DATA
# ---------------------------------------------------------
sink_start("01_load_structure.txt")
titanic <- read.csv("/home/claude/week1/titanic.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
cat("Dimensions of dataset:\n")
print(dim(titanic))
cat("\nFirst 5 rows:\n")
print(head(titanic, 5))
cat("\nStructure of dataset:\n")
str(titanic)
sink()

# ---------------------------------------------------------
# 2. INITIAL SUMMARY
# ---------------------------------------------------------
sink_start("02_summary_raw.txt")
cat("Summary statistics (raw data):\n")
print(summary(titanic))
sink()

# ---------------------------------------------------------
# 3. MISSING VALUE ANALYSIS
# ---------------------------------------------------------
sink_start("03_missing_values.txt")
na_counts <- sapply(titanic, function(x) sum(is.na(x)))
na_pct <- round(100 * na_counts / nrow(titanic), 2)
missing_df <- data.frame(Column = names(na_counts), Missing_Count = na_counts, Missing_Pct = na_pct)
missing_df <- missing_df[order(-missing_df$Missing_Count), ]
rownames(missing_df) <- NULL
cat("Missing values per column:\n")
print(missing_df)
sink()

# Missing value bar chart
png(file.path(out, "plot_missing_values.png"), width = 900, height = 600, res = 120)
mp <- missing_df[missing_df$Missing_Count > 0, ]
bp <- barplot(mp$Missing_Pct, names.arg = mp$Column, col = "#4C72B0",
              main = "Percentage of Missing Values by Column",
              ylab = "% Missing", ylim = c(0, 100), las = 1)
text(bp, mp$Missing_Pct + 3, labels = paste0(mp$Missing_Pct, "%"))
dev.off()

# ---------------------------------------------------------
# 4. DATA CLEANING
# ---------------------------------------------------------
sink_start("04_cleaning_steps.txt")

clean <- titanic

# 4a. Cabin: >75% missing -> drop column, but keep a "HasCabin" flag first
cat("Cabin missing %:", round(mean(is.na(clean$Cabin)) * 100, 2), "\n")
clean$HasCabin <- ifelse(is.na(clean$Cabin), 0, 1)
clean$Cabin <- NULL

# 4b. Age: ~20% missing -> impute using median Age within each Pclass+Sex group
cat("\nAge missing before imputation:", sum(is.na(clean$Age)), "\n")
age_medians <- clean %>%
  group_by(Pclass, Sex) %>%
  summarise(med_age = median(Age, na.rm = TRUE), .groups = "drop")
print(age_medians)

clean <- clean %>%
  left_join(age_medians, by = c("Pclass", "Sex")) %>%
  mutate(Age = ifelse(is.na(Age), med_age, Age)) %>%
  select(-med_age)
cat("Age missing after imputation:", sum(is.na(clean$Age)), "\n")

# 4c. Embarked: 2 missing -> impute with mode (most frequent port)
mode_embarked <- names(sort(table(clean$Embarked), decreasing = TRUE))[1]
cat("\nMode of Embarked:", mode_embarked, "\n")
clean$Embarked[is.na(clean$Embarked)] <- mode_embarked

# 4d. Fare: check missing (none expected, but handle defensively) with median
if (any(is.na(clean$Fare))) {
  clean$Fare[is.na(clean$Fare)] <- median(clean$Fare, na.rm = TRUE)
}

# 4e. Drop columns not useful for numeric analysis (identifiers/free text)
clean$Name <- NULL
clean$Ticket <- NULL
clean$PassengerId <- NULL

cat("\nRemaining missing values after cleaning:\n")
print(colSums(is.na(clean)))
sink()

# ---------------------------------------------------------
# 5. OUTLIER DETECTION (IQR method on Fare and Age)
# ---------------------------------------------------------
sink_start("05_outliers.txt")

detect_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  list(lower = lower, upper = upper, n_outliers = sum(x < lower | x > upper))
}

fare_out <- detect_outliers_iqr(clean$Fare)
age_out  <- detect_outliers_iqr(clean$Age)

cat("Fare outlier bounds: [", round(fare_out$lower,2), ",", round(fare_out$upper,2), "]\n")
cat("Fare outliers detected:", fare_out$n_outliers, "out of", nrow(clean), "\n\n")
cat("Age outlier bounds: [", round(age_out$lower,2), ",", round(age_out$upper,2), "]\n")
cat("Age outliers detected:", age_out$n_outliers, "out of", nrow(clean), "\n")

# Cap (Winsorize) Fare outliers at upper bound rather than deleting rows
clean$Fare_capped <- ifelse(clean$Fare > fare_out$upper, fare_out$upper, clean$Fare)
cat("\nFare summary before capping:\n"); print(summary(clean$Fare))
cat("Fare summary after capping:\n"); print(summary(clean$Fare_capped))
sink()

png(file.path(out, "plot_boxplots_outliers.png"), width = 1000, height = 500, res = 120)
par(mfrow = c(1, 2))
boxplot(clean$Fare, main = "Fare (raw) - outliers visible", col = "#DD8452", horizontal = TRUE)
boxplot(clean$Age, main = "Age - outliers visible", col = "#55A868", horizontal = TRUE)
dev.off()

# ---------------------------------------------------------
# 6. NORMALIZATION / SCALING
# ---------------------------------------------------------
sink_start("06_normalization.txt")

min_max_norm <- function(x) (x - min(x)) / (max(x) - min(x))

clean$Age_norm  <- min_max_norm(clean$Age)
clean$Fare_norm <- min_max_norm(clean$Fare_capped)
clean$Age_z     <- as.numeric(scale(clean$Age))
clean$Fare_z    <- as.numeric(scale(clean$Fare_capped))

cat("Age - original range: [", round(min(clean$Age),2), ",", round(max(clean$Age),2), "]\n")
cat("Age_norm (min-max) range: [", round(min(clean$Age_norm),2), ",", round(max(clean$Age_norm),2), "]\n")
cat("Age_z (z-score) mean/sd: [", round(mean(clean$Age_z),2), ",", round(sd(clean$Age_z),2), "]\n\n")

cat("Fare - original range: [", round(min(clean$Fare_capped),2), ",", round(max(clean$Fare_capped),2), "]\n")
cat("Fare_norm (min-max) range: [", round(min(clean$Fare_norm),2), ",", round(max(clean$Fare_norm),2), "]\n")
sink()

# ---------------------------------------------------------
# 7. ENCODING CATEGORICAL VARIABLES
# ---------------------------------------------------------
sink_start("07_encoding.txt")

# Label encoding for binary categorical: Sex
clean$Sex_encoded <- ifelse(clean$Sex == "male", 1, 0)

# One-hot encoding for Embarked (3 levels: C, Q, S)
clean$Embarked_C <- as.integer(clean$Embarked == "C")
clean$Embarked_Q <- as.integer(clean$Embarked == "Q")
clean$Embarked_S <- as.integer(clean$Embarked == "S")

# Factor conversion for modeling-ready columns
clean$Pclass  <- factor(clean$Pclass, levels = c(1,2,3), labels = c("1st","2nd","3rd"))
clean$Survived_label <- factor(clean$Survived, levels = c(0,1), labels = c("No","Yes"))

cat("Sex encoding check:\n")
print(table(clean$Sex, clean$Sex_encoded))
cat("\nEmbarked one-hot encoding check:\n")
print(table(clean$Embarked, clean$Embarked_C, clean$Embarked_Q, clean$Embarked_S))
cat("\nStructure after encoding:\n")
str(clean)
sink()

write.csv(clean, file.path(out, "titanic_cleaned.csv"), row.names = FALSE)

# ---------------------------------------------------------
# 8. EXPLORATORY DATA ANALYSIS
# ---------------------------------------------------------
sink_start("08_eda_summary.txt")
cat("Summary statistics (cleaned data):\n")
print(summary(clean[, c("Age","Fare","SibSp","Parch")]))

cat("\nSurvival rate overall:\n")
print(round(prop.table(table(clean$Survived_label)) * 100, 1))

cat("\nSurvival rate by Sex:\n")
print(round(prop.table(table(clean$Sex, clean$Survived_label), margin = 1) * 100, 1))

cat("\nSurvival rate by Pclass:\n")
print(round(prop.table(table(clean$Pclass, clean$Survived_label), margin = 1) * 100, 1))

cat("\nCorrelation matrix (numeric variables):\n")
num_vars <- clean[, c("Survived","Age","Fare_capped","SibSp","Parch","Sex_encoded")]
corr_mat <- cor(num_vars, use = "complete.obs")
print(round(corr_mat, 2))
sink()

# Visualization 1: Age distribution
png(file.path(out, "plot_age_distribution.png"), width = 900, height = 600, res = 120)
ggplot(clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#4C72B0", color = "white") +
  labs(title = "Age Distribution of Passengers (post-imputation)", x = "Age", y = "Count") +
  theme_minimal(base_size = 13)
dev.off()

# Visualization 2: Survival by Sex
png(file.path(out, "plot_survival_by_sex.png"), width = 900, height = 600, res = 120)
ggplot(clean, aes(x = Sex, fill = Survived_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Rate by Sex", x = "Sex", y = "Proportion", fill = "Survived") +
  theme_minimal(base_size = 13)
dev.off()

# Visualization 3: Survival by Pclass
png(file.path(out, "plot_survival_by_pclass.png"), width = 900, height = 600, res = 120)
ggplot(clean, aes(x = Pclass, fill = Survived_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Proportion", fill = "Survived") +
  theme_minimal(base_size = 13)
dev.off()

# Visualization 4: Fare vs Age scatter, colored by survival
png(file.path(out, "plot_fare_age_scatter.png"), width = 900, height = 600, res = 120)
ggplot(clean, aes(x = Age, y = Fare_capped, color = Survived_label)) +
  geom_point(alpha = 0.6) +
  labs(title = "Fare vs Age by Survival Outcome", x = "Age", y = "Fare (capped)", color = "Survived") +
  theme_minimal(base_size = 13)
dev.off()

# Visualization 5: Correlation heatmap
png(file.path(out, "plot_correlation_heatmap.png"), width = 800, height = 800, res = 120)
corrplot(corr_mat, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.8,
         title = "Correlation Matrix - Numeric Variables", mar = c(0,0,2,0))
dev.off()

cat("All outputs written to:", out, "\n")
