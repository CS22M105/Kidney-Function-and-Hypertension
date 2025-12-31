library("dplyr")

# DAG for secondary aim
dag_model_final <- dagitty('
dag {
  log_ACR -> Extrafluid
  log_ACR -> SBP
  Extrafluid -> SBP
  Age -> log_ACR
  Age -> SBP
  Gender -> log_ACR
  Gender -> SBP
  BMI -> log_ACR
  BMI -> Extrafluid
  BMI -> SBP
  Diabetes_category -> log_ACR
  Diabetes_category -> SBP
}
')

# Plot the DAG
ggdag(dag_model_final, layout = "circle") +
  theme_dag() +
  geom_dag_node(color = "lightblue", fill = "lightblue") +
  geom_dag_text(color = "black")

# Gender vs hypertension: Chi square test
table(merged$Gender, merged$hypertension)
chisq.test(table(merged$Gender, merged$hypertension))

# ANOVA: SBP across ACR stages
anova_result <- aov(SBP ~ A_stage, data = merged)
summary(anova_result)

# if anova is significant follow up with Tukey test
TukeyHSD(anova_result)

# T-test: SBP in Diabetics vs Non-Diabetics
t.test(SBP ~ Diabetes_category, data = merged)

# T-test: BMI by Hypertension Status
t.test(BMI ~ hypertension, data = merged)
# ------------------------------------------------------------------------------
# Test how SBP differs in different age groups
# Create Age Groups
merged <- merged %>%
  mutate(Age_group = case_when(
    Age >= 20 & Age < 30 ~ "20-29",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 ~ "50+"
  ))

# Check new Age Groups
table(merged$Age_group)

# Run one-way ANOVA
anova_age <- aov(SBP ~ Age_group, data = merged)

# View summary
summary(anova_age)
TukeyHSD(anova_age)
# ------------------------------------------------------------------------------

# Relationship between log_ACR and Extrafluid
cor.test(merged$log_ACR, merged$Extrafluid, use = "complete.obs")

plot(merged$log_ACR, merged$Extrafluid,
     xlab = "log_ACR",
     ylab = "Extracellular Fluid Volume (L)",
     main = "log_ACR vs Extrafluid",
     col = "blue", pch = 19)
abline(lm(Extrafluid ~ log_ACR, data = merged), col = "red", lwd = 2)
# ------------------------------------------------------------------------------

# Relationship between Extrafluid and SBP
cor.test(merged$Extrafluid, merged$SBP, use = "complete.obs")
cor.test(merged$Extrafluid, merged$DBP, use = "complete.obs")

plot(merged$Extrafluid, merged$SBP,
     xlab = "Extracellular Fluid (L)",
     ylab = "Systolic Blood Pressure (mmHg)",
     main = "SBP vs Extrafluid",
     col = "pink", pch = 19)
abline(lm(SBP ~ Extrafluid, data = merged), col = "blue", lwd = 2)
# ------------------------------------------------------------------------------

# Relationship between log_ACR and SBP
cor.test(merged$log_ACR, merged$SBP, use = "complete.obs")

plot(merged$log_ACR, merged$SBP,
     xlab = "log(ACR + 1)",
     ylab = "Systolic Blood Pressure (SBP)",
     main = "Scatterplot: log_ACR vs SBP",
     col = "green", pch = 19)

# Add a regression line
abline(lm(SBP ~ log_ACR, data = merged), col = "red", lwd = 2)

# ------------------------------------------------------------------------------
