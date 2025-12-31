install.packages("summarytools")
install.packages("mediation")
install.packages("dplyr")
install.packages("caret")

library(caret)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(mediation)
library(pROC)

# Distribution of Key variables
hist(merged$Age, main = "Age distribution", xlab = "Age", col = "lightblue", breaks = 30)
summary(merged$Age)
hist(merged$SBP, main = "Systolic Blood Pressure", xlab = "SBP", col = "lightblue", breaks = 30)
summary(merged$SBP)
hist(merged$DBP, main = "Diastolic Blood Pressure", xlab = "DBP", col = "lightblue", breaks = 30)
summary(merged$DBP)
hist(merged$Creat_mg_per_dL, main = "Creatinine urine (umol/dL) distribution", xlab = "Creatinine", col = "lightblue", breaks = 30)
summary(merged$Creat_mg_per_dL)
hist(merged$Extrafluid, main = "Extra Cellular Fluid", xlab = "Extra Cellular Fluid", col = "lightblue", breaks = 30)
summary(merged$Extrafluid)
hist(merged$ACR, main = "albumin to creatinine Ratio", xlab = "ACR", col = "lightblue", breaks = 10)
summary(merged$ACR)
hist(merged$BMI, main = "Body Mass Index", xlab = "BMI", col = "lightblue", breaks = 30)
summary(merged$BMI)

# Distribution of relationships
# Scatterplots
pairs(merged[, c("SBP", "DBP", "Creat_mg_per_dL", "Extrafluid", "ACR", "BMI")], col = "darkgray")

merged <- merged %>%
  mutate(log_ACR = log(ACR + 1))

# Plot histogram
hist(merged$log_ACR,
     main = "Log-Transformed ACR Distribution",
     xlab = "log(ACR + 1)",
     col = "skyblue",
     breaks = 30,
     border = "white")

# Correlation matrix
cor_data <- merged %>% dplyr::select(SBP, DBP, Creat_mg_per_dL, Extrafluid, log_ACR, BMI, Weight)
cor_matrix <- cor(cor_data, use = "complete.obs")
round(cor_matrix, 2)

# Plot heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         title = "Correlation Heatmap", mar = c(0,0,1,0))

# stratified box plots of Albumin stages
ggplot(merged, aes(x = A_stage, y = SBP, fill = A_stage)) +
  geom_boxplot() +
  labs(title = "SBP by Albuminuria Stage", y = "Systolic Blood Pressure", x = "Albuminuria Stage (ACR)") +
  theme_minimal()


# Group Means – Kidney Function → Fluid Retention → BP
merged %>%
  group_by(A_stage) %>%
  summarise(
    mean_creatinine = mean(log_ACR, na.rm = TRUE),
    mean_fluid = mean(Extrafluid, na.rm = TRUE),
    mean_sbp = mean(SBP, na.rm = TRUE)
  )
# MEDIATION ANALYSIS

# Filter complete cases for all required variables
mediation_data <- merged %>%
  dplyr::select(SBP, ACR, Extrafluid, Age, Gender, BMI, Diabetes_category) %>%
  na.omit()

# Mediator model: how creatinine affects fluid retention
med_model <- lm(Extrafluid ~ ACR + Age + Gender + BMI + Diabetes_category, data = mediation_data)

# Outcome model: how creatinine and fluid retention affect SBP
out_model <- lm(SBP ~ ACR + Extrafluid + Age + Gender + BMI + Diabetes_category, data = mediation_data)

# Mediation: test if Extrafluid mediates ACR's effect on SBP
mediation_result <- mediate(
  model.m = med_model,
  model.y = out_model,
  treat = "ACR",
  mediator = "Extrafluid",
  boot = TRUE,
  sims = 1592
)

summary(mediation_result)
plot(mediation_result)



# Stratify by demographics (for secondary aim)
unique(merged$Gender)
table(merged$Gender, useNA = "ifany")

# By sex
merged %>%
  group_by(Gender) %>%
  summarise(
    mean_creatinine = mean(Creat_mg_per_dL, na.rm = TRUE),
    mean_sbp = mean(SBP, na.rm = TRUE)
  )

# By diabetes status
merged %>%
  group_by(Diabetes_category) %>%
  summarise(
    mean_creatinine = mean(Creat_mg_per_dL, na.rm = TRUE),
    mean_sbp = mean(SBP, na.rm = TRUE)
  )

merged <- merged %>%
  mutate(hypertension = ifelse(SBP >= 140 | DBP >= 90, 1, 0))

# Multivariate Linear regression
# For creatinine
model_linear <- lm(SBP ~ Creat_mg_per_dL + Age + Gender + BMI + Diabetes_category, data = merged)
summary(model_linear)
# For log_ACR
model_linear_logacr <- lm(SBP ~ log_ACR + Age + Gender + BMI + Diabetes_category, data = merged)
summary(model_linear_logacr)

# Five fold cross validation
# Create a clean dataset (complete cases)
complete_data <- merged %>%
  dplyr::select(hypertension, log_ACR, Age, Gender, BMI, Diabetes_category) %>%
  na.omit()
# Change hypertension to a factor for caret
complete_data$hypertension <- factor(complete_data$hypertension, levels = c(0, 1), labels = c("No", "Yes"))

# Define training control for 5-fold CV
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                              summaryFunction = twoClassSummary)

# Train logistic regression model with cross-validation
set.seed(123)  # For reproducibility
logit_cv_model <- train(hypertension ~ log_ACR + Age + Gender + BMI + Diabetes_category,
                        data = complete_data,
                        method = "glm",
                        family = binomial,
                        metric = "ROC",
                        trControl = train_control)

# View the CV results
print(logit_cv_model)

# Logistic regression

logit_model <- glm(hypertension ~ log_ACR + Age + Gender + BMI + Diabetes_category, 
                   data = merged, family = binomial)
summary(logit_model)


# Predict probabilities on the same complete-case dataset
pred_prob <- predict(logit_model, newdata = complete_data, type = "response")

# Now ROC
roc_obj <- roc(complete_data$hypertension, pred_prob)

# Plot ROC
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve for Hypertension Prediction")
auc_value <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

# Predicted probability curve for log_ACR
# Create a sequence of log_ACR values
log_acr_seq <- seq(min(merged$log_ACR, na.rm = TRUE),
                   max(merged$log_ACR, na.rm = TRUE), length.out = 100)

# Create a new data frame holding average values for other covariates
pred_data <- data.frame(
  log_ACR = log_acr_seq,
  Age = mean(merged$Age, na.rm = TRUE),
  Gender = "Male",  # You can also create separate lines for Female
  BMI = mean(merged$BMI, na.rm = TRUE),
  Diabetes_category = 0
)

# Predict probability
pred_data$prob <- predict(logit_model, newdata = pred_data, type = "response")

# Plot curve
plot(pred_data$log_ACR, pred_data$prob, type = "l", lwd = 2, col = "darkgreen",
     xlab = "log_ACR", ylab = "Predicted Probability of Hypertension",
     main = "Predicted Hypertension Risk vs log_ACR")


#  Fine till here.....in code as well as in report.
