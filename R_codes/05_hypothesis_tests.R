suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})

run_hypothesis_tests <- function(analysis_data) {
  dir.create(here::here("results"), showWarnings = FALSE, recursive = TRUE)

  gender_htn <- chisq.test(table(analysis_data$Gender, analysis_data$hypertension))
  anova_stage <- aov(SBP ~ A_stage, data = analysis_data)
  diabetes_t <- t.test(SBP ~ Diabetes_category, data = analysis_data)
  bmi_htn_t <- t.test(BMI ~ hypertension, data = analysis_data)

  age_data <- analysis_data %>%
    mutate(Age_group = case_when(
      Age >= 20 & Age < 30 ~ "20-29",
      Age >= 30 & Age < 40 ~ "30-39",
      Age >= 40 & Age < 50 ~ "40-49",
      Age >= 50 ~ "50+"
    ))
  anova_age <- aov(SBP ~ Age_group, data = age_data)

  test_summary <- data.frame(
    test = c(
      "Gender vs hypertension chi-square",
      "SBP across ACR stages ANOVA",
      "SBP by diabetes status t-test",
      "BMI by hypertension status t-test",
      "SBP across age groups ANOVA"
    ),
    statistic = c(
      unname(gender_htn$statistic),
      unname(summary(anova_stage)[[1]]["A_stage", "F value"]),
      unname(diabetes_t$statistic),
      unname(bmi_htn_t$statistic),
      unname(summary(anova_age)[[1]]["Age_group", "F value"])
    ),
    p_value = c(
      gender_htn$p.value,
      unname(summary(anova_stage)[[1]]["A_stage", "Pr(>F)"]),
      diabetes_t$p.value,
      bmi_htn_t$p.value,
      unname(summary(anova_age)[[1]]["Age_group", "Pr(>F)"])
    )
  )

  write.csv(test_summary, here::here("results", "hypothesis_tests.csv"), row.names = FALSE)
  write.csv(TukeyHSD(anova_stage)$A_stage, here::here("results", "tukey_acr_stage.csv"), row.names = TRUE)
  write.csv(TukeyHSD(anova_age)$Age_group, here::here("results", "tukey_age_group.csv"), row.names = TRUE)
}
