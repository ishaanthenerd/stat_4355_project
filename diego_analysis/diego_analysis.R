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
library(ggplot2)
#setwd("C:\\Users\\diego\\OneDrive\\Documents\\School\\Spring 26\\STAT4355")

readLines("filtered_cardio.csv", n = 5)

t <- read.csv2("filtered_cardio.csv")

names(t)


#Model to reduce with vif

vifmodel <- lm(cardio ~age_years + weight_imperial + height_imperial + cholesterol_2_dv + cholesterol_3_dv + gluc_2_dv 
   + gluc_3_dv + gender_dv + active_dv + alco_dv + smoke_dv, data = t)

vif(vifmodel)

#logistic regression on all variables

cardiomodel <- glm(cardio ~ age_years + height_imperial + weight_imperial +
                     gender_dv +
                     cholesterol_2_dv + cholesterol_3_dv +
                     gluc_2_dv + gluc_3_dv +
                     smoke_dv + alco_dv + active_dv,
                   data = t, family = binomial)
summary(cardiomodel)




drop1(cardiomodel, test = "Chisq")





sim_res_model_cm <- simulateResiduals(cardiomodel)
testoutliers_cm <- testOutliers(simulationOutput = sim_res_model_cm, type = "bootstrap")


plotQQunif(sim_res_model_cm)
plotResiduals(sim_res_model_cm, smoothScatter = FALSE)

testDispersion(sim_res_model_cm)
testUniformity(sim_res_model_cm)








######-------------------------------------------------------------------------------------
#There was some prediction we ran here: the following is not what we used but I hope you remember what we did :)



# Confusion matrix
cm <- table(Predicted = pred_class, Actual = test$cardio)
cm

# Pull values
TN <- cm["0","0"]
TP <- cm["1","1"]
FN <- cm["0","1"]
FP <- cm["1","0"]

# Metrics
accuracy <- (TP + TN) / sum(cm)
sensitivity <- TP / (TP + FN)   # recall / TPR
specificity <- TN / (TN + FP)   # TNR
precision <- TP / (TP + FP)

accuracy
sensitivity
specificity
precision





###-----------------------------
### Change response variable, include aphi and lo, family= binomial, 
## check box cox, select k at top of parabola


cardiomodel3 <- glm(formula = cardio ~ (1 + age_years + height_imperial + weight_imperial) * 
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


#####------------------------------------------------------------------------------------------------
##Going to run lasso regression so model does not blowup 

# Make sure response is 0/1 numeric
class(t$cardio)
table(t$cardio)
y <- if (is.factor(t$cardio)) as.numeric(t$cardio) - 1 else t$cardio

# Build predictor matrix:
# main effects + all pairwise interactions only
x <- model.matrix(
  ~ (1 + age_years + height_imperial + weight_imperial) * 
    (1 + gender_dv) *
    (1 + smoke_dv) * 
    (1 + alco_dv) * 
    (1 + active_dv) * 
    (1 + cholesterol_2_dv + cholesterol_3_dv) * 
    (1 + gluc_2_dv + gluc_3_dv),
  data = t
)[, -1]   # remove intercept column
dim(x)

# Cross-validated lasso logistic regression
set.seed(123)
cardiolasso_cv <- cv.glmnet(
  x, y,
  family = "binomial",
  alpha = 1,
  #used 10 fold cross validation
  nfolds = 10,
  standardize = TRUE
)


# Plot CV curve
plot(cardiolasso_cv)

# Coefficients at lambda.1se
coef_1se <- coef(cardiolasso_cv, s = "lambda.1se")

#extracting variables not needed/ that lasso shrunk to 0: Keeping non 0 
selected_terms <- rownames(coef_1se)[as.vector(coef_1se !=0)]

selected_terms <- setdiff(selected_terms, "(Intercept)")
selected_terms


interaction_terms <- selected_terms[grepl(":", selected_terms)]

## Hierarchy Principle, want to include both interactions and variables inside interactions
if (length(interaction_terms) > 0) {
  main_effects_interactions <- unique(unlist(strsplit(interaction_terms, ":")))
  selected_terms <- unique(c(selected_terms, main_effects_interactions))
}

selected_terms 


## Model 8 Lasso regression selected terms get run as a regular logistic regression
lasso_formula <- as.formula(
  paste("cardio ~", paste(selected_terms, collapse = " + "))
)

cardiomodel8 <- glm(lasso_formula, data = t, family = binomial)

summary(cardiomodel8)


# compare models
AIC(cardiomodel, cardiomodel8)
anova(cardiomodel8, cardiomodel, test = "Chisq")

# check whether anything else in the pruned model can go
drop1(cardiomodel8, test = "Chisq")


# Residual analysis - red line indicates quantile deviation
sim_res_model_cm8 <- simulateResiduals(cardiomodel8)
testoutliers_cm8 <- testOutliers(simulationOutput = sim_res_model_cm8, type = "bootstrap")


plotQQunif(sim_res_model_cm8)

plotResiduals(sim_res_model_cm8, smoothScatter = FALSE)

testDispersion(sim_res_model_cm8)
testUniformity(sim_res_model_cm8)

