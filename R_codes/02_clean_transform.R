suppressPackageStartupMessages({
  library(dplyr)
})

prepare_analysis_data <- function(merged) {
  merged %>%
    mutate(
      SBP = rowMeans(dplyr::select(., BPXSY1, BPXSY2, BPXSY3), na.rm = TRUE),
      DBP = rowMeans(dplyr::select(., BPXDI1, BPXDI2, BPXDI3), na.rm = TRUE)
    ) %>%
    rename(
      ID = SEQN,
      Age = RIDAGEYR,
      Gender = RIAGENDR,
      Pregnant = RIDEXPRG,
      Extrafluid = BIDECF,
      Weight = BMXWT,
      BMI = BMXBMI,
      Creatinine = URXUCRSI,
      Creatinine_mg_dL_original = URXUCR,
      Albumin = URXUMA,
      Diabetes = DIQ010
    ) %>%
    filter(Age >= 20) %>%
    mutate(
      Creat_mg_per_dL = Creatinine * 0.011312,
      ACR = ifelse(!is.na(Albumin) & !is.na(Creat_mg_per_dL),
                   (Albumin / Creat_mg_per_dL) * 100,
                   NA_real_),
      A_stage = case_when(
        ACR < 30 ~ "A1",
        ACR >= 30 & ACR <= 300 ~ "A2",
        ACR > 300 ~ "A3",
        TRUE ~ NA_character_
      ),
      Diabetes_category = case_when(
        Diabetes == "Yes" ~ 1,
        Diabetes == "No" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(
      Gender == "Male" |
        (Gender == "Female" & Pregnant != "Yes, positive lab pregnancy test or self-reported pregnant at exam")
    ) %>%
    mutate(
      log_ACR = log(ACR + 1),
      hypertension = ifelse(SBP >= 140 | DBP >= 90, 1, 0)
    )
}
