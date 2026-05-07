# --- Importing libraries and functions ---
install.packages("car", type = "binary")
install.packages("ggplot2", type = "binary")
install.packages("DHARMa", type = "binary")
install.packages("MASS", type = "binary")
install.packages("olsrr", type = "binary")
library(car)
library(ggplot2)
library(DHARMa)
library(MASS)
library(olsrr)
source("C:\\Users\\ishaa\\Downloads\\stat_4355_project\\ishaan_files\\ishaan_functions.R")

# --- Loading data ---
df <- read.csv("C:\\Users\\ishaa\\Downloads\\stat_4355_project\\ishaan_files\\filtered_cardio.csv", sep = ";")

# GOAL 1: Identifying how to keep healthy diastolic blood pressure
# 60 ~ 80 mmHg for systolic (source: AHA)

# Model v3 will use a generalized version
model_v3 <- glm(formula = ap_lo ~ (1 + age_years + height_imperial + weight_imperial) * 
                  (1 + gender_dv) *
                  (1 + smoke_dv) * 
                  (1 + alco_dv) * 
                  (1 + active_dv) * 
                  (1 + cholesterol_2_dv + cholesterol_3_dv) * 
                  (1 + gluc_2_dv + gluc_3_dv), data = df)

# Determine how well the model does
summary_model_v3 <- summary(model_v3)

# Use Box-Cox to find an appropriate k to transform the response with y --> (y^k - 1) / k
boxcox(model_v3, plotit = TRUE)

# Model v6 will use forward selection from the base model
k <- 3/4
df$ap_lo_modified <- (df$ap_lo ^ k - 1) / k
model_v6 <- glm(ap_lo_modified ~ age_years + height_imperial + weight_imperial, data = df)

candidates <- c(
    "gender_dv",
    "smoke_dv",
    "alco_dv",
    "active_dv",
    "cholesterol_2_dv",
    "cholesterol_3_dv",
    "gluc_2_dv",
    "gluc_3_dv"
)

remaining <- candidates
selected <- c()
alpha <- 0.05

repeat {
    if (length(remaining) == 0) break
    
    pvals <- c()
    models <- list()
    for (term in remaining) {
        model_try <- update(model_v6, as.formula(paste("~ . *", term)))
        a <- anova(model_v6, model_try, test = "F")
        pvals[term] <- a$`Pr(>F)`[2]
        models[[term]] <- model_try
    }
    
    best_term <- names(which.min(pvals))
    best_p <- min(pvals, na.rm = TRUE)
    if (!is.na(best_p) && best_p < alpha) {
        model_v6 <- models[[best_term]]
        selected <- c(selected, best_term)
        remaining <- setdiff(remaining, best_term)
        cat("Added block:", best_term, "| p =", best_p, "\n")
    } else {
        break
    }
}

# Determine how well the model does
summary_model_v6 <- summary(model_v6)

# Model 6's selections in order: cholesterol_3_dv, cholesterol_2_dv, gender_dv, gluc_3_dv, gluc_2_dv, alco_dv

# Model v7 will use a single filtering pass
coefs <- summary(model_v6)$coefficients
keep_terms <- rownames(coefs)[coefs[, "Pr(>|t|)"] < 0.05]
keep_terms <- setdiff(keep_terms, "(Intercept)")
current_terms <- attr(terms(model_v7), "term.labels")

new_formula <- as.formula(
    paste("ap_lo_modified ~", paste(keep_terms, collapse = " + "))
)
model_v7 <- lm(new_formula, data = df)

# Determine how well the model does
summary_model_v7 <- summary(model_v7)

# Residual analysis - red line indicates quantile deviation
sim_res_model_v7 <- simulateResiduals(model_v7)
plotQQunif(sim_res_model_v7)
plotResiduals(sim_res_model_v7, smoothScatter = FALSE)

# Model 7's selections:
# Coefficients:
#                                                                          Estimate
# (Intercept)                                                             3.054e+01  ***
# age_years                                                               6.066e-02  ***
# height_imperial                                                        -5.052e-02  ***
# weight_imperial                                                         2.390e-02  ***
# cholesterol_3_dv                                                        4.217e+00  ***
# cholesterol_2_dv                                                        1.007e-01
# age_years:cholesterol_3_dv                                             -5.707e-02  ***
# weight_imperial:cholesterol_2_dv                                        2.287e-03  *
# age_years:gender_dv                                                     6.745e-03  ***
# cholesterol_2_dv:gender_dv                                              1.875e+00
# age_years:active_dv                                                     2.459e-04
# cholesterol_2_dv:active_dv                                              1.627e+00
# height_imperial:cholesterol_2_dv:gender_dv                             -2.708e-02
# height_imperial:gender_dv:gluc_3_dv                                    -4.626e-03  *
# height_imperial:cholesterol_2_dv:active_dv                             -2.520e-02
# cholesterol_2_dv:gender_dv:active_dv                                   -2.648e+00
# height_imperial:active_dv:gluc_3_dv                                     7.536e-04
# cholesterol_3_dv:active_dv:gluc_3_dv                                    6.866e+00  ***
# gender_dv:active_dv:gluc_2_dv                                           2.602e-01  **
# height_imperial:cholesterol_3_dv:gender_dv:gluc_3_dv                   -3.174e-03
# height_imperial:cholesterol_2_dv:gender_dv:alco_dv                     -9.403e-04
# height_imperial:cholesterol_2_dv:gender_dv:active_dv                    4.411e-02
# height_imperial:cholesterol_3_dv:active_dv:gluc_3_dv                   -1.115e-01  ***
# cholesterol_3_dv:gender_dv:active_dv:gluc_3_dv                         -1.177e+01  ***
# cholesterol_2_dv:gender_dv:active_dv:alco_dv                            6.584e+00
# height_imperial:cholesterol_2_dv:gender_dv:gluc_2_dv:alco_dv            1.943e-02
# height_imperial:cholesterol_3_dv:gender_dv:active_dv:gluc_3_dv          1.860e-01  ***
# height_imperial:cholesterol_2_dv:gender_dv:active_dv:alco_dv           -1.093e-01
# weight_imperial:cholesterol_2_dv:gender_dv:active_dv:alco_dv            7.090e-03
# height_imperial:cholesterol_2_dv:gender_dv:active_dv:gluc_2_dv:alco_dv  1.028e-02
# weight_imperial:cholesterol_2_dv:gender_dv:active_dv:gluc_2_dv:alco_dv -1.081e-02