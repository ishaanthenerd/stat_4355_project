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

# GOAL 1: Identifying how to keep healthy systolic blood pressure
# 90 ~ 120 mmHg for systolic (source: AHA)

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

# Residual analysis - red line indicates quantile deviation
sim_res_model_v3 <- simulateResiduals(model_v3)
plotQQunif(sim_res_model_v3)
plotResiduals(sim_res_model_v3, smoothScatter = FALSE)

# Tests from QQunif:
# KS test shows significance, i.e. the data can be said to be drawn from another distribution beyond a reasonable doubt --> interpreting QQ plot reveals that the data is heavy on both tails, so this is expected
# Dispersion test does not show significance, i.e. there is not enough evidence to say that the data is either over or under-dispersed
# Outlier test shows significance, i.e. the data can be said to have outliers beyond a reasonable doubt --> plenty of outliers exist at the top and bottom of the residual plot

# Use Box-Cox to find an appropriate k to transform the response with y --> (y^k - 1) / k
boxcox(model_v3, plotit = TRUE)

# Model v4 will use k = -2/3 from Box-Cox on top of v3
k <- -2/3
df$ap_hi_modified <- (df$ap_hi ^ k - 1) / k
model_v4 <- glm(formula = ap_hi_modified ~ (1 + age_years + height_imperial + weight_imperial) * 
                  (1 + gender_dv) *
                  (1 + smoke_dv) * 
                  (1 + alco_dv) * 
                  (1 + active_dv) * 
                  (1 + cholesterol_2_dv + cholesterol_3_dv) * 
                  (1 + gluc_2_dv + gluc_3_dv), data = df)

# Determine how well the model does
summary_model_v4 <- summary(model_v4)

# Residual analysis - red line indicates quantile deviation
sim_res_model_v4 <- simulateResiduals(model_v4)
plotQQunif(sim_res_model_v4)
plotResiduals(sim_res_model_v4, smoothScatter = FALSE)

# Model v5 will use forward model selection directly from v4
# EDIT: This does not run in a reasonable amount of time. We'll need to manually add the relevant terms.
ols_step_forward_p(model_v4)

# Model v6 will use forward selection from the base model
k <- -2/3
df$ap_hi_modified <- (df$ap_hi ^ k - 1) / k
model_v6 <- glm(ap_hi_modified ~ age_years + height_imperial + weight_imperial, data = df)

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
    paste("ap_hi_modified ~", paste(keep_terms, collapse = " + "))
)
model_v7 <- lm(new_formula, data = df)

# Determine how well the model does
summary_model_v7 <- summary(model_v7)

# Residual analysis - red line indicates quantile deviation
sim_res_model_v7 <- simulateResiduals(model_v7)
plotQQunif(sim_res_model_v7)
plotResiduals(sim_res_model_v7, smoothScatter = FALSE)