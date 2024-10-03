
###################### Re-Admittance to the hospital for heart failure ######################

# Load necessary libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(gtsummary)
library(naniar)
library(car)
library(summarytools)
library(MASS)

setwd("C:/Users/frazieg/Documents/HeartFailure")
# Load data
data <- read_excel("SHEET FOR ANALYSIS.xlsx")

# Remove excess null rows
data <- data[1:201, ]

# Select relevant columns
data <- data %>%
  select(ID_2 = `ID 2`, SEX, Age, SMOKING, DRINKING, COMPLIANCE, SURGERY, T2DM, HTN, Dyslipidemia, KidneyDisease, ASTHMACOPD, READMISSIONS)

# Create dummy variables and recode factors
data <- data %>%
  mutate(across(c(SMOKING, DRINKING, COMPLIANCE, SURGERY, T2DM, HTN, ASTHMACOPD, Dyslipidemia, KidneyDisease),
                ~ case_when(.x == 'not stated' ~ NA_character_, .x == 'N/A' ~ NA_character_, 
                            .x == 'never' | .x == 'Never' ~ '0',
                            .x == 'none' | .x == 'no' | .x == 'No' ~ '0',
                            .x == 'yes' | .x == 'Yes' | .x == 'YES' ~ '1',
                            .x == 'N' | .x == 'n' ~ '0', 
                            .x == 'Y' | .x == 'y' ~ '1', 
                            TRUE ~ .x))) %>%
  mutate(SEX = case_when(
    SEX == 'F' ~ '0', 
    SEX == 'M' ~ '1', 
    TRUE ~ SEX
  )) %>%
  replace_with_na_all(condition = ~.x %in% c("", "NA"))

# Remove rows with missing values
data_clean <- na.omit(data)

# Create a binary variable for re-admission
data_clean <- data_clean %>%
  mutate(Readmitted = ifelse(READMISSIONS >= 1, 1, 0))


####################### Summary Statistics #########################

Variables <- data[ ,c("Age", "SEX", "SMOKING", "DRINKING", "COMPLIANCE", "SURGERY", "T2DM", 
                      "HTN", "Dyslipidemia", "KidneyDisease", "ASTHMACOPD", "READMISSIONS")]

summary_table <- dfSummary(Variables,
                           graph.col = TRUE,
                           valid.col = TRUE,
                           na.col = TRUE,
                           graph.magnif = 0.75,
                           style = "grid")
view(summary_table)



####################### Visualizations #########################

# Histogram of Age distribution by Re-Admission Status
ggplot(data_clean, aes(Age)) +
  geom_histogram(aes(fill = as.factor(SEX)), color = "black", binwidth = 2) +
  facet_wrap(~ Readmitted, labeller = labeller(Readmitted = c('0' = "No Re-Admission", '1' = "Re-Admission"))) +
  ggtitle("Age Distribution by Re-Admission Status")

######################## Logistic Regression Models ###########################

# Create regression models
model_smoking <- glm(Readmitted ~ SMOKING, family = binomial, data = data_clean)
model_age_sex <- glm(Readmitted ~ Age + SEX, family = binomial, data = data_clean)
model_full <- glm(Readmitted ~ SMOKING + DRINKING + COMPLIANCE + SURGERY + T2DM + HTN + KidneyDisease + ASTHMACOPD + Dyslipidemia, 
                  family = binomial, data = data_clean)


# Model summaries
summary(model_smoking)
summary(model_age_sex)
summary(model_full)


###################### Correlation check and Selection #######################

# Check for multicollinearity
vif(model_full)


# Correlation matrix
cor(data_clean %>% select(SMOKING, DRINKING, COMPLIANCE, SUGERY, T2DM, HTN, KidneyDisease, ASTHMACOPD, Dyslipidemia), use = complete.obs)

# Stepwise selection
best_model <- stepAIC(model_full, direction = "both")
summary(best_model)


######################## Regression tables #################################

tbl_regression(best_model, exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  italicize_levels() %>%
  modify_caption("Multivariate Regression for Re-Admission")


tbl_regression(model_full, exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  italicize_levels() %>%
  modify_caption("Multivariate Regression for Re-Admission")

