# =========================================================
# Week 2 Task: Data Visualization and Insight Communication with R
# Dataset: Titanic Passenger Dataset (continued from Week 1, cleaned)
# =========================================================

library(ggplot2)
library(dplyr)
library(lattice)
library(RColorBrewer)

out <- "/home/claude/week2/output"
dir.create(out, showWarnings = FALSE)

df <- read.csv("/home/claude/week2/titanic_cleaned.csv", stringsAsFactors = FALSE)
df$Pclass <- factor(df$Pclass, levels = c("1st","2nd","3rd"))
df$Survived_label <- factor(df$Survived_label, levels = c("No","Yes"))
df$Sex <- factor(df$Sex, levels = c("male","female"))
df$Embarked <- factor(df$Embarked, levels = c("C","Q","S"),
                       labels = c("Cherbourg","Queenstown","Southampton"))

cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")

theme_report <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11, color = "grey30"),
        legend.position = "bottom")

# ---------------------------------------------------------
# CHART 1 (Bar): Passenger count by class, split by survival
# ---------------------------------------------------------
png(file.path(out, "01_bar_survival_count_by_class.png"), width = 1000, height = 650, res = 130)
p1 <- ggplot(df, aes(x = Pclass, fill = Survived_label)) +
  geom_bar(position = "dodge", color = "white") +
  scale_fill_manual(values = c("No" = "#E15759", "Yes" = "#4E79A7")) +
  labs(title = "How Many Passengers Survived, by Class?",
       subtitle = "Raw passenger counts (not percentages) across the three ticket classes",
       x = "Passenger Class", y = "Number of Passengers", fill = "Survived") +
  theme_report
print(p1)
dev.off()

# ---------------------------------------------------------
# CHART 2 (Stacked/100% Bar): Survival rate by Sex and Class combined
# ---------------------------------------------------------
png(file.path(out, "02_bar_survival_rate_sex_class.png"), width = 1050, height = 650, res = 130)
p2 <- ggplot(df, aes(x = Pclass, fill = Survived_label)) +
  geom_bar(position = "fill", color = "white") +
  facet_wrap(~Sex) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#E15759", "Yes" = "#59A14F")) +
  labs(title = "Survival Rate by Class, Split by Sex",
       subtitle = "Being female mattered more than class, but class still mattered a lot within each sex",
       x = "Passenger Class", y = "Proportion Survived", fill = "Survived") +
  theme_report
print(p2)
dev.off()

# ---------------------------------------------------------
# CHART 3 (Histogram + density): Age distribution by survival
# ---------------------------------------------------------
png(file.path(out, "03_histogram_age_distribution.png"), width = 1000, height = 650, res = 130)
p3 <- ggplot(df, aes(x = Age, fill = Survived_label)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, alpha = 0.55, position = "identity", color = "white") +
  geom_density(aes(color = Survived_label), linewidth = 1, fill = NA) +
  scale_fill_manual(values = c("No" = "#E15759", "Yes" = "#4E79A7")) +
  scale_color_manual(values = c("No" = "#B02E30", "Yes" = "#2E5395")) +
  labs(title = "Age Distribution: Survivors vs Non-Survivors",
       subtitle = "Younger passengers (especially children) show a visibly higher survival density",
       x = "Age (years)", y = "Density", fill = "Survived", color = "Survived") +
  theme_report
print(p3)
dev.off()

# ---------------------------------------------------------
# CHART 4 (Scatter, faceted): Fare vs Age by class and survival
# ---------------------------------------------------------
png(file.path(out, "04_scatter_fare_age_by_class.png"), width = 1100, height = 650, res = 130)
p4 <- ggplot(df, aes(x = Age, y = Fare_capped, color = Survived_label)) +
  geom_point(alpha = 0.65, size = 1.8) +
  facet_wrap(~Pclass) +
  scale_color_manual(values = c("No" = "#E15759", "Yes" = "#4E79A7")) +
  labs(title = "Fare vs Age, Split by Passenger Class",
       subtitle = "Within every class, higher-fare passengers cluster toward survival",
       x = "Age (years)", y = "Fare (capped, £)", color = "Survived") +
  theme_report
print(p4)
dev.off()

# ---------------------------------------------------------
# CHART 5 (Line chart): Survival rate trend across age bins
# ---------------------------------------------------------
df$AgeBin <- cut(df$Age, breaks = seq(0, 80, by = 10), include.lowest = TRUE,
                  labels = paste0(seq(0,70,10), "-", seq(9,79,10)))
age_trend <- df %>%
  group_by(AgeBin) %>%
  summarise(SurvivalRate = mean(Survived) * 100, n = n(), .groups = "drop")

png(file.path(out, "05_line_survival_rate_by_age.png"), width = 1000, height = 650, res = 130)
p5 <- ggplot(age_trend, aes(x = AgeBin, y = SurvivalRate, group = 1)) +
  geom_line(color = "#4E79A7", linewidth = 1.3) +
  geom_point(size = 3, color = "#2E5395") +
  geom_text(aes(label = paste0(round(SurvivalRate), "%")), vjust = -1, size = 3.5) +
  ylim(0, 100) +
  labs(title = "Survival Rate Trend Across Age Groups",
       subtitle = "Survival is highest among young children and generally declines with age",
       x = "Age Group (years)", y = "Survival Rate (%)") +
  theme_report
print(p5)
dev.off()

# ---------------------------------------------------------
# CHART 6 (Boxplot): Fare distribution by class and survival
# ---------------------------------------------------------
png(file.path(out, "06_boxplot_fare_by_class_survival.png"), width = 1000, height = 650, res = 130)
p6 <- ggplot(df, aes(x = Pclass, y = Fare_capped, fill = Survived_label)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = c("No" = "#E15759", "Yes" = "#4E79A7")) +
  labs(title = "Fare Spread by Class and Survival Outcome",
       subtitle = "Survivors paid a visibly higher median fare within every class",
       x = "Passenger Class", y = "Fare (capped, £)", fill = "Survived") +
  theme_report
print(p6)
dev.off()

# ---------------------------------------------------------
# CHART 7 (Base R plotting system): Pie chart of embarkation ports
# ---------------------------------------------------------
port_counts <- table(df$Embarked)
port_pct <- round(100 * port_counts / sum(port_counts), 1)
port_labels <- paste0(names(port_counts), "\n", port_pct, "%")

png(file.path(out, "07_base_pie_embarkation_ports.png"), width = 850, height = 700, res = 130)
pie(port_counts, labels = port_labels,
    col = brewer.pal(3, "Set2"),
    main = "Passengers by Port of Embarkation\n(Base R plotting system)")
dev.off()

# ---------------------------------------------------------
# CHART 8 (Base R plotting system): Barplot of family size
# ---------------------------------------------------------
df$FamilySize <- df$SibSp + df$Parch + 1
fam_table <- table(cut(df$FamilySize, breaks = c(0,1,2,4,11),
                        labels = c("Alone","Small (2)","Medium (3-4)","Large (5+)")))

png(file.path(out, "08_base_barplot_family_size.png"), width = 950, height = 650, res = 130)
bp <- barplot(fam_table, col = brewer.pal(4, "Blues"),
              main = "Passengers by Family Size Group (Base R plotting system)",
              ylab = "Number of Passengers", ylim = c(0, max(fam_table) * 1.15))
text(bp, fam_table, labels = fam_table, pos = 3)
dev.off()

# ---------------------------------------------------------
# CHART 9 (Lattice): Survival rate by class, conditioned on sex
# ---------------------------------------------------------
surv_summary <- df %>%
  group_by(Pclass, Sex) %>%
  summarise(SurvivalRate = mean(Survived) * 100, .groups = "drop")

png(file.path(out, "09_lattice_survival_dotplot.png"), width = 1000, height = 650, res = 130)
p9 <- dotplot(Pclass ~ SurvivalRate | Sex, data = surv_summary,
              main = "Survival Rate by Class, Conditioned on Sex (Lattice system)",
              xlab = "Survival Rate (%)", ylab = "Passenger Class",
              pch = 19, col = "#2E5395", cex = 1.6,
              panel = function(...) {
                panel.dotplot(...)
                panel.abline(v = seq(0,100,20), col = "grey85", lty = 3)
              })
print(p9)
dev.off()

# ---------------------------------------------------------
# CHART 10 (Stacked bar): Embarkation port composition by class
# ---------------------------------------------------------
png(file.path(out, "10_bar_embarked_by_class.png"), width = 1000, height = 650, res = 130)
p10 <- ggplot(df, aes(x = Pclass, fill = Embarked)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Port of Embarkation Composition by Class",
       subtitle = "Southampton dominates all classes, but Cherbourg supplied a disproportionate share of 1st class",
       x = "Passenger Class", y = "Proportion of Passengers", fill = "Port") +
  theme_report
print(p10)
dev.off()

# ---------------------------------------------------------
# Console summary for documentation
# ---------------------------------------------------------
cat("\n--- Survival rate by age group ---\n")
print(age_trend)

cat("\n--- Survival rate by class x sex ---\n")
print(surv_summary)

cat("\n--- Embarkation port counts ---\n")
print(port_counts)

cat("\n--- Family size group counts ---\n")
print(fam_table)

cat("\nAll 10 visualizations written to:", out, "\n")
