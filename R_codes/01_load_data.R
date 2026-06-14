suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
  library(here)
  library(nhanesA)
})

load_nhanes_sources <- function() {
  list(
    demo = nhanesA::nhanes("DEMO_C"),
    bia = nhanesA::nhanes("BIX_C"),
    bmx = nhanesA::nhanes("BMX_C"),
    bp = nhanesA::nhanes("BPX_C"),
    diabetes = nhanesA::nhanes("DIQ_C"),
    lab_albumin_cr = haven::read_xpt(here::here("data", "raw", "L16_C.xpt"))
  )
}

merge_nhanes_sources <- function(sources) {
  sources$demo %>%
    dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG) %>%
    inner_join(sources$bia %>% dplyr::select(SEQN, BIDECF), by = "SEQN") %>%
    inner_join(sources$bmx %>% dplyr::select(SEQN, BMXWT, BMXBMI), by = "SEQN") %>%
    inner_join(
      sources$bp %>%
        dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXDI1, BPXDI2, BPXDI3),
      by = "SEQN"
    ) %>%
    inner_join(
      sources$lab_albumin_cr %>% dplyr::select(SEQN, URXUCR, URXUCRSI, URXUMA),
      by = "SEQN"
    ) %>%
    inner_join(sources$diabetes %>% dplyr::select(SEQN, DIQ010), by = "SEQN")
}
