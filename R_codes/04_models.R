suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(here)
  library(mediation)
  library(pROC)
})

run_models <- function(analysis_data) {
  dir.create(here::here("figures"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here::here("results"), showWarnings = FALSE, recursive = TRUE)

  mediation_data <- analysis_data %>%
    dplyr::select(SBP, log_ACR, Extrafluid, Age, Gender, BMI, Diabetes_category) %>%
    na.omit()

  med_model <- lm(Extrafluid ~ log_ACR + Age + Gender + BMI + Diabetes_category, data = mediation_data)
  out_model <- lm(SBP ~ log_ACR + Extrafluid + Age + Gender + BMI + Diabetes_category, data = mediation_data)

  set.seed(123)
  mediation_result <- mediate(
    model.m = med_model,
    model.y = out_model,
    treat = "log_ACR",
    mediator = "Extrafluid",
    boot = TRUE,
    sims = 1592
  )

  med_summary <- summary(mediation_result)
  mediation_table <- data.frame(
    effect = c("ACME", "ADE", "Total Effect", "Proportion Mediated"),
    estimate = c(med_summary$d0, med_summary$z0, med_summary$tau.coef, med_summary$n0),
    ci_lower = c(med_summary$d0.ci[1], med_summary$z0.ci[1], med_summary$tau.ci[1], med_summary$n0.ci[1]),
    ci_upper = c(med_summary$d0.ci[2], med_summary$z0.ci[2], med_summary$tau.ci[2], med_summary$n0.ci[2]),
    p_value = c(med_summary$d0.p, med_summary$z0.p, med_summary$tau.p, med_summary$n0.p)
  )
  write.csv(mediation_table, here::here("results", "mediation_summary.csv"), row.names = FALSE)

  png(here::here("figures", "mediation_plot.png"), width = 900, height = 650)
  plot(mediation_result)
  dev.off()

  model_linear <- lm(SBP ~ Creat_mg_per_dL + Age + Gender + BMI + Diabetes_category, data = analysis_data)
  model_linear_logacr <- lm(SBP ~ log_ACR + Age + Gender + BMI + Diabetes_category, data = analysis_data)

  linear_metrics <- data.frame(
    model = c("creatinine_adjusted_lm", "log_acr_adjusted_lm"),
    r_squared = c(summary(model_linear)$r.squared, summary(model_linear_logacr)$r.squared),
    adj_r_squared = c(summary(model_linear)$adj.r.squared, summary(model_linear_logacr)$adj.r.squared),
    sigma = c(summary(model_linear)$sigma, summary(model_linear_logacr)$sigma)
  )
  write.csv(linear_metrics, here::here("results", "linear_model_metrics.csv"), row.names = FALSE)

  complete_data <- analysis_data %>%
    dplyr::select(hypertension, log_ACR, Age, Gender, BMI, Diabetes_category) %>%
    na.omit()
  complete_data$hypertension <- factor(complete_data$hypertension, levels = c(0, 1), labels = c("No", "Yes"))

  train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
  set.seed(123)
  logit_cv_model <- train(
    hypertension ~ log_ACR + Age + Gender + BMI + Diabetes_category,
    data = complete_data,
    method = "glm",
    family = binomial,
    metric = "ROC",
    trControl = train_control
  )

  logit_model <- glm(
    hypertension ~ log_ACR + Age + Gender + BMI + Diabetes_category,
    data = analysis_data,
    family = binomial
  )

  pred_prob <- predict(logit_model, newdata = complete_data, type = "response")
  roc_obj <- roc(complete_data$hypertension, pred_prob)
  auc_value <- as.numeric(auc(roc_obj))

  png(here::here("figures", "roc_curve.png"), width = 900, height = 650)
  plot(roc_obj, col = "#1f78b4", lwd = 2, main = "ROC Curve for Hypertension Prediction")
  legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "#1f78b4", lwd = 2)
  dev.off()

  model_metrics <- data.frame(
    model = "logistic_regression_cv",
    cv_roc = logit_cv_model$results$ROC,
    cv_sensitivity = logit_cv_model$results$Sens,
    cv_specificity = logit_cv_model$results$Spec,
    apparent_auc = auc_value
  )
  write.csv(model_metrics, here::here("results", "classification_model_metrics.csv"), row.names = FALSE)
}
