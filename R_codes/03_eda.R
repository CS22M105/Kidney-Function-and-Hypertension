suppressPackageStartupMessages({
  library(corrplot)
  library(dplyr)
  library(ggplot2)
  library(here)
})

run_eda <- function(analysis_data) {
  dir.create(here::here("figures"), showWarnings = FALSE, recursive = TRUE)
  dir.create(here::here("results"), showWarnings = FALSE, recursive = TRUE)

  p_stage <- ggplot(analysis_data, aes(x = A_stage, y = SBP, fill = A_stage)) +
    geom_boxplot(na.rm = TRUE) +
    labs(title = "SBP by Albuminuria Stage", x = "Albuminuria Stage", y = "Systolic Blood Pressure") +
    theme_minimal() +
    theme(legend.position = "none")
  ggsave(here::here("figures", "acr_stage_sbp_boxplot.png"), p_stage, width = 7, height = 5, dpi = 300)

  p_log_acr <- ggplot(analysis_data, aes(x = log_ACR)) +
    geom_histogram(bins = 30, fill = "#2f6f4e", color = "white", na.rm = TRUE) +
    labs(title = "Log-Transformed ACR Distribution", x = "log(ACR + 1)", y = "Count") +
    theme_minimal()
  ggsave(here::here("figures", "log_acr_distribution.png"), p_log_acr, width = 7, height = 5, dpi = 300)

  p_fluid_sbp <- ggplot(analysis_data, aes(x = Extrafluid, y = SBP)) +
    geom_point(alpha = 0.45, na.rm = TRUE) +
    geom_smooth(method = "lm", se = TRUE, color = "#1f78b4", na.rm = TRUE) +
    labs(title = "Systolic Blood Pressure vs Extracellular Fluid", x = "Extracellular Fluid", y = "SBP") +
    theme_minimal()
  ggsave(here::here("figures", "sbp_vs_extrafluid.png"), p_fluid_sbp, width = 7, height = 5, dpi = 300)

  cor_data <- analysis_data %>%
    dplyr::select(SBP, DBP, Creat_mg_per_dL, Extrafluid, log_ACR, BMI, Weight)
  cor_matrix <- cor(cor_data, use = "complete.obs")
  write.csv(round(cor_matrix, 3), here::here("results", "correlation_matrix.csv"), row.names = TRUE)

  png(here::here("figures", "correlation_heatmap.png"), width = 900, height = 700)
  corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    tl.col = "black",
    addCoef.col = "black",
    number.cex = 0.7,
    col = colorRampPalette(c("#2166ac", "white", "#b2182b"))(200),
    title = "Correlation Heatmap",
    mar = c(0, 0, 2, 0)
  )
  dev.off()

  stage_summary <- analysis_data %>%
    group_by(A_stage) %>%
    summarise(
      n = dplyr::n(),
      mean_log_acr = mean(log_ACR, na.rm = TRUE),
      mean_fluid = mean(Extrafluid, na.rm = TRUE),
      mean_sbp = mean(SBP, na.rm = TRUE),
      .groups = "drop"
    )
  write.csv(stage_summary, here::here("results", "acr_stage_summary.csv"), row.names = FALSE)
}
