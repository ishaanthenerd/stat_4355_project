#Install packages 

#install.packages("ggplot2", type = "binary")
#install.packages("DHARMa", type = "binary")
 #install.packages("MASS", type = "binary")
 #install.packages("olsrr", type = "binary")
 #install.packages("glmnet")
 


library(car)
library(ggplot2)
library(DHARMa)
library(MASS)
library(olsrr)
 library(glmnet)
#setwd("C:\\Users\\diego\\OneDrive\\Documents\\School\\Spring 26\\STAT4355")

readLines("filtered_cardio.csv", n = 5)

t <- read.csv2("C:\\Users\\dxe230000\\Desktop\\filtered_cardio.csv")
#t <- read.csv2("cardio_train.csv")
#t <- subset(t, ap_hi >= 30 & ap_hi <=370 & ap_lo >=6 & ap_lo <= 370)
#t <- na.omit(t)
#t <- t[complete.cases(t), ]

# Remove people with absurd BMI
#t$bmi <- t$weight / (t$height / 100)^2
#t <- t[t$bmi >= 10, ]
#t <- t[t$bmi <= 50, ]

# Remove people with absurd height
#t <- t[t$height >= 120, ]
#t <- t[t$height <= 210, ]

# All weights remaining after the above steps are in a reasonable range; no need to filter

# All ages are in a reasonable range; no need to filter


#Imperial Units
#t$height_in <- t$height / 2.54 
#t$weight_lbs <- t$weight * 2.20462

#Age in years 

#t$age_years <- t$age /365

#Dummy variables
#t$gender_female <- ifelse(t$gender == 1, 1, 0)
#t$cholesterol_above <- ifelse(t$cholesterol == 2, 1,0) 
#t$cholesterol_wellabove <- ifelse(t$cholesterol == 3, 1,0)
#t$gluc_above <- ifelse(t$gluc == 2, 1,0)
#t$gluc_wellabove <- ifelse(t$gluc == 3, 1,0) 
names(t)


#Model to reduce with vif

#vifmodel <- lm(cardio ~age_years + weight_lbs + height_in + cholesterol_wellabove + cholesterol_above + gluc_wellabove 
#   + gluc_above + gender_female + active + alco + smoke + ap_hi + ap_lo, data = t)

#vif(vifmodel)

#logistic regression on all variables

cardiomodel <- glm(cardio ~ age_years + height_imperial + weight_imperial +
                     ap_hi + ap_lo + gender_dv +
                     cholesterol_2_dv + cholesterol_3_dv +
                     gluc_2_dv + gluc_3_dv +
                     smoke + alco + active,
                   data = t, family = binomial)
summary(cardiomodel)


#logistic regression on important variables for comparison
cardiomodel2 <- glm(cardio ~ age_years + height_imperial + weight_imperial +
                      ap_hi + ap_lo +
                      cholesterol_2_dv + cholesterol_3_dv +
                      gluc_3_dv +
                      smoke + alco + active,
                    data = t, family = binomial)
summary(cardiomodel2)


AIC(cardiomodel, cardiomodel2)
anova(cardiomodel2, cardiomodel, test = "Chisq")

drop1(cardiomodel2, test = "Chisq")



sim_res_model_cm2 <- simulateResiduals(cardiomodel2)
testoutliers_cm2 <- testOutliers(simulationOutput = sim_res_model_cm2, type = "bootstrap")


plotQQunif(sim_res_model_cm2)
plotResiduals(sim_res_model_cm2, smoothScatter = FALSE)

###-----------------------------
### Change response variable, include aphi and lo, family= binomial, 
## check box cox, select k at top of parabola


cardiomodel3 <- glm(formula = cardio ~ (1 + age_years + height_imperial + weight_imperial+ ap_hi+ ap_lo) * 
                  (1 + gender_dv) *
                  (1 + smoke_dv) * 
                  (1 + alco_dv) * 
                  (1 + active_dv) * 
                  (1 + cholesterol_2_dv + cholesterol_3_dv) * 
                  (1 + gluc_2_dv + gluc_3_dv), data = t, family = binomial)


summary_model_cm3 <- summary(cardiomodel3)

# Cardiomodel 3 did not converge 

# Residual analysis - red line indicates quantile deviation
sim_res_model_cm3 <- simulateResiduals(cardiomodel3)
testoutliers_cm3 <- testOutliers(simulationOutput = sim_res_model_cm3, type = "bootstrap")


plotQQunif(sim_res_model_cm3)

plotResiduals(sim_res_model_cm3, smoothScatter = FALSE)

#####


##Run model 6 with cardiomodel2 as base since cardiomodel3 did not converge


cardiomodel6 <- cardiomodel2 

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
    
    #
    model_try <- update(cardiomodel6, as.formula(paste("~ . *", term)))
    
    a <- anova(cardiomodel6, model_try, test = "Chisq")
    
    pvals[term] <- a$`Pr(>Chi)`[2]
    models[[term]] <- model_try
  }
  
  best_term <- names(which.min(pvals))
  best_p <- min(pvals, na.rm = TRUE)
  
  if (!is.na(best_p) && best_p < alpha) {
    cardiomodel6 <- models[[best_term]]
    selected <- c(selected, best_term)
    remaining <- setdiff(remaining, best_term)
    
    cat("Added block:", best_term, "| p =", best_p, "\n")
  } else {
    break
  }
}

summary(cardiomodel6)
AIC(cardiomodel2, cardiomodel6)


##Model 6 takes too long to run. 


## ran model 6 with a different model_try <- update(cardiomodel6, as.formula(paste("~. +, term)))


cardiomodel6b <- cardiomodel2 

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
    
    #
    model_try <- update(cardiomodel6b, as.formula(paste("~ . +", term)))
    
    a <- anova(cardiomodel6b, model_try, test = "Chisq")
    
    pvals[term] <- a$`Pr(>Chi)`[2]
    models[[term]] <- model_try
  }
  
  best_term <- names(which.min(pvals))
  best_p <- min(pvals, na.rm = TRUE)
  
  if (!is.na(best_p) && best_p < alpha) {
    cardiomodel6 <- models[[best_term]]
    selected <- c(selected, best_term)
    remaining <- setdiff(remaining, best_term)
    
    cat("Added block:", best_term, "| p =", best_p, "\n")
  } else {
    break
  }
}

summary(cardiomodel6b)
AIC(cardiomodel2, cardiomodel6b) 

##Going to run model 6 a little differently, going to change the candidates

cardiomodel6c <- cardiomodel2 

candidates <- c(
  "gender_dv",
  "gluc_2_dv",
  "cholesterol_3_dv:gluc_3_dv",
  "age_years:active",
  "weight_imperial:active",
  "ap_hi:ap_lo") 


remaining <- candidates
selected <- c()
alpha <- 0.05

repeat {
  if (length(remaining) == 0) break
  
  pvals <- c()
  models <- list()
  
  for (term in remaining) {
    
    #changed model_try 
    model_try <- update(cardiomodel6c, as.formula(paste("~ . +", term)))
    
    a <- anova(cardiomodel6c, model_try, test = "Chisq")
    
    pvals[term] <- a$`Pr(>Chi)`[2]
    models[[term]] <- model_try
  }
  
  best_term <- names(which.min(pvals))
  best_p <- min(pvals, na.rm = TRUE)
  
  if (!is.na(best_p) && best_p < alpha) {
    cardiomodel6 <- models[[best_term]]
    selected <- c(selected, best_term)
    remaining <- setdiff(remaining, best_term)
    
    cat("Added block:", best_term, "| p =", best_p, "\n")
  } else {
    break
  }
}

summary(cardiomodel6c)
AIC(cardiomodel2, cardiomodel6c) 
##Again as before cardiomodel6c is equal to cardiomodel2

##Run model 7 from model 6 


##Going to run lasso regression so model does not blowup 

# Make sure response is 0/1 numeric
y <- if (is.factor(t$cardio)) as.numeric(t$cardio) - 1 else t$cardio

# Build predictor matrix:
# main effects + all pairwise interactions only
x <- model.matrix(
  ~ (age_years + height_imperial + weight_imperial +
       ap_hi + ap_lo +
       gender_dv +
       cholesterol_2_dv + cholesterol_3_dv +
       gluc_2_dv + gluc_3_dv +
       smoke + alco + active)^2,
  data = t
)[, -1]   # remove intercept column

# Cross-validated lasso logistic regression
set.seed(123)
cardiolasso_cv <- cv.glmnet(
  x, y,
  family = "binomial",
  alpha = 1,
  nfolds = 10,
  standardize = TRUE
)

# Plot CV curve
plot(cardiolasso_cv)

# Coefficients at the more conservative lambda
coef_1se <- coef(cardiolasso_cv, s = "lambda.1se")
selected_terms <- rownames(coef_1se)[as.vector(coef_1se !=0)]
selected_terms <- setdiff(selected_terms, "(Intercept)")
selected_terms


interaction_terms <- selected_terms[grepl(":", selected_terms)]

if (length(interaction_terms) > 0) {
  maineffectsinterctions <- unique(unlist(strsplit(interaction_terms, ":")))
  selected_terms <- unique(c(selected_terms, maineffectsinterctions))
}

selected_terms 


## Model 8 Lasso regression
lasso_formula <- as.formula(
  paste("cardio ~", paste(selected_terms, collapse = " + "))
)

cardiomodel8 <- glm(lasso_formula, data = t, family = binomial)

summary(cardiomodel8)
AIC(cardiomodel2, cardiomodel8)
anova(cardiomodel2, cardiomodel8, test = "Chisq")

# Residual analysis - red line indicates quantile deviation
sim_res_model_cm8 <- simulateResiduals(cardiomodel8)
testoutliers_cm8 <- testOutliers(simulationOutput = sim_res_model_cm8, type = "bootstrap")


plotQQunif(sim_res_model_cm8)

plotResiduals(sim_res_model_cm8, smoothScatter = FALSE)


