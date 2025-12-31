rm(list = ls())

install.packages(c("dagitty", "ggdag"))
library(NHANES)
library(nhanesA)
library(dplyr)
library(haven)
library(dagitty)
library(ggdag)

# Load data from NHANES 2003–2004
demo <- nhanes("DEMO_C")
bia <- nhanes("BIX_C")
bmx <- nhanes("BMX_C")
bp <- nhanes("BPX_C")
diabetes <- nhanes("DIQ_C") # DIQ010: Doctor diagnosed diabetes?

lab_albumin_cr <- read_xpt("/Users/farhatjahan/Desktop/YU/Probability/final_project/L16_C.xpt")
print(min(lab_albumin_cr$URXUCRSI, na.rm=TRUE))
print(max(lab_albumin_cr$URXUCRSI, na.rm=TRUE))


# Check if any dataset is NULL
list(demo, bia, bmx, bp, lab_albumin_cr, diabetes) %>%
  setNames(c("demo", "bia", "bmx", "bp", "lab_albumin_cr", "diabetes")) %>%
  lapply(function(df) if (is.null(df)) "MISSING" else paste("Rows:", nrow(df)))

# Step-by-step selective joins
merged <- demo %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG) %>%  # Age and Gender
  
  inner_join(bia %>% select(SEQN, BIDECF), by = "SEQN") %>%  # Extracellular fluid
  
  inner_join(bmx %>% select(SEQN, BMXWT, BMXBMI), by = "SEQN") %>%  # Weight and BMI
  
  inner_join(bp %>% select(SEQN, BPXSY1, BPXSY2, BPXSY3,
                           BPXDI1, BPXDI2, BPXDI3), by = "SEQN") %>%  # Blood pressure readings
  
  inner_join(lab_albumin_cr %>% select(SEQN, URXUCRSI, URXUMA), by = "SEQN") %>%  # Creatinine & Albumin
  
  inner_join(diabetes %>% select(SEQN, DIQ010), by = "SEQN")  # Diabetes status
# REMOVE UNNECESSARY DATAFRAMES
rm(bia, bmx, bp, demo,diabetes, lab_albumin_cr)

# Calculate average SBP and DBP
merged <- merged %>%
  mutate(SBP = rowMeans(select(., BPXSY1, BPXSY2, BPXSY3), na.rm = TRUE),
         DBP = rowMeans(select(., BPXDI1, BPXDI2, BPXDI3), na.rm = TRUE))

# Rename all columns to make them easy to read
merged <- merged %>%
  rename(
    ID = SEQN,
    Age = RIDAGEYR,
    Gender = RIAGENDR,
    Pregnant = RIDEXPRG,
    Extrafluid = BIDECF,
    Weight = BMXWT,
    BMI = BMXBMI,
    Creatinine = URXUCRSI, # umol/L
    Albumin = URXUMA, # ug/mL
    Diabetes = DIQ010
  )
# 1 umol/L == 1 mg/dL
# Filter for age > 20
merged <- merged %>% filter(Age >= 20)
table(merged$Pregnant, useNA = "ifany")

str(merged)
summary(merged)
summary(merged$Creatinine)
# We have wrong label for measuring creatinine, it should be ug/dL, not mg/dL.
# 1 mg/dL = 88.4 µmol/L
print(min(merged$Creatinine, na.rm=TRUE))
print(max(merged$Creatinine, na.rm=TRUE))

# fine till here

# --------------------------------
# Albumin-to-Creatinine Ratio (ACR) mg/g
# --------------------------------
merged <- merged %>%
  mutate(
    # Convert Creatinine (µmol/L) to mg/dL using molar mass of creatinine ≈ 113.12 g/mol
    Creat_mg_per_dL = Creatinine * 0.011312,
    
    # ACR = albumin (mg/L) / creatinine (mg/dL) × 100
    # Albumin is in µg/mL → 1 µg/mL = 1 mg/L
    ACR = ifelse(!is.na(Albumin) & !is.na(Creat_mg_per_dL),
                 (Albumin / Creat_mg_per_dL) * 100,
                 NA)
  )

print(min(merged$ACR, na.rm = TRUE))
print(max(merged$ACR, na.rm = TRUE))

merged <- merged %>% 
  mutate(
    A_stage = case_when(
      ACR < 30 ~ "A1",
      ACR >= 30 & ACR <= 300 ~ "A2",
      ACR > 300 ~ "A3",
      TRUE ~ NA_character_
    )
  )
sum(merged$ACR < 30, na.rm = TRUE)
sum(merged$ACR > 300, na.rm = TRUE)
sum(merged$ACR >= 30 & merged$ACR <= 300, na.rm = TRUE)
# --------------------------------
# Binary Diabetes Variable
# Diabetes: 1 = Yes, 2 = No
# --------------------------------
merged <- merged %>%
  mutate(Diabetes_category = case_when(
    Diabetes == "Yes" ~ 1,
    Diabetes == "No" ~ 0,
    TRUE ~ NA_real_
  ))
# Filter to:
# - Males
# - OR Females who are not pregnant
merged <- merged %>%
  filter(
    (Gender == "Male") |
      (Gender == "Female" & Pregnant != "Yes, positive lab pregnancy test or self-reported pregnant at exam")
  )

# --------------------------------
# Final dataset with selected columns
# --------------------------------
head(merged)


# Define the DAG structure
dag_model <- dagitty('
dag {
  log_ACR -> Extrafluid
  log_ACR -> SBP
  Extrafluid -> SBP
}
')

# Plot the DAG
ggdag(dag_model, layout = "circle") +
  theme_dag() +
  geom_dag_node(color = "orange", fill = "orange") +
  geom_dag_text(color = "red") 
