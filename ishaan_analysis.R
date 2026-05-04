# --- Importing libraries and functions ---
# install.packages("car", type = "binary")
# install.packages("ggplot2", type = "binary")
# install.packages("DHARMa", type = "binary")
library(car)
library(ggplot2)
library("DHARMa")
source("ishaan_functions.R")

# --- Loading data ---
df <- read.csv("C:\\Users\\ishaa\\Downloads\\stat_4355_project\\filtered_cardio.csv", sep = ";") 

# GOAL 1: Identifying how to keep healthy systolic / diastolic blood pressure
# 90 ~ 120 mmHg for systolic, 60 ~ 80 mmHg for diastolic (source: AHA)

# Build an initial model
model_v1 <- lm(ap_hi ~ age_years + height_imperial + weight_imperial + gender_dv + cholesterol_2_dv + cholesterol_3_dv + gluc_2_dv + gluc_3_dv + smoke_dv + alco_dv + active_dv, data = df)

# Determine how well the model does
summary_model_v1 <- summary(model_v1)

# Observations:
# 1. All but two variables, when considered as stand-alone, are significant in the model. Only the smoke_dv and active_dv are not significant.
# 2. The adjusted R^2 value is 0.1332, indicating that the model (as it stands) is not very good.
# EDIT: That's because model v1 is wrong! There should be 576 terms in all, not 12.

# Model v2 will add cross terms that come about because of categorical variables
model_v2 <- lm(formula = ap_hi ~ (1 + age_years + height_imperial + weight_imperial) * 
                                (1 + gender_dv) *
                                (1 + smoke_dv) * 
                                (1 + alco_dv) * 
                                (1 + active_dv) * 
                                (1 + cholesterol_2_dv + cholesterol_3_dv) * 
                                (1 + gluc_2_dv + gluc_3_dv), data = df)

# Determine how well the model does
summary_model_v2 <- summary(model_v2)

# Observations:
# The adjusted R^2 is still very low. Alternative idea - use a generalized linear model.

# Model v3 will use a generalized version
model_v3 <- glm(formula = ap_hi ~ (1 + age_years + height_imperial + weight_imperial) * 
                                (1 + gender_dv) *
                                (1 + smoke_dv) * 
                                (1 + alco_dv) * 
                                (1 + active_dv) * 
                                (1 + cholesterol_2_dv + cholesterol_3_dv) * 
                                (1 + gluc_2_dv + gluc_3_dv), data = df)

# Determine how well the model does
summary_model_v3 <- summary(model_v3)

# Observations:
# The adjusted R^2 is still very low, but without deviating from a linear model, this is about as good as we'll get.
# Let's analyze residuals for any transformation that needs to be done on ap_hi.

# Residual analysis
sim_res_model_v3 <- simulateResiduals(model_v3)
plot(sim_res_model_v3)

# GOAL 2: Identifying how to reduce risk of cardiovascular disease

# Build an initial model (logistic regression)
