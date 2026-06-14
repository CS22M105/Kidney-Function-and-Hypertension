suppressPackageStartupMessages({
  library(here)
})

source(here::here("R", "01_load_data.R"))
source(here::here("R", "02_clean_transform.R"))
source(here::here("R", "03_eda.R"))
source(here::here("R", "04_models.R"))
source(here::here("R", "05_hypothesis_tests.R"))

sources <- load_nhanes_sources()
merged <- merge_nhanes_sources(sources)
analysis_data <- prepare_analysis_data(merged)

dir.create(here::here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
saveRDS(analysis_data, here::here("data", "processed", "analysis_dataset.rds"))

run_eda(analysis_data)
run_models(analysis_data)
run_hypothesis_tests(analysis_data)

message("Analysis complete. Outputs are in figures/, results/, and data/processed/.")
