#Install packages 

 install.packages("ggplot2", type = "binary")
 install.packages("DHARMa", type = "binary")
 install.packages("MASS", type = "binary")
 install.packages("olsrr", type = "binary")
 install.packages("glmnet")
 install.packages("pROC")



library(car)
library(ggplot2)
library(DHARMa)
library(MASS)
library(olsrr)
library(glmnet)
library(ggplot2)
 library(pROC)
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

testDispersion(sim_res_model_cm2)
testUniformity(sim_res_model_cm2)








######-------------------------------------------------------------------------------------
#Prediction under model2 

set.seed(123)

# 70/30 split
n <- nrow(t)
train_idx <- sample(seq_len(n), size = round(0.7 * n))

train <- t[train_idx, ]
test  <- t[-train_idx, ]

# Fit the model on training data
cardiomodel2_train <- glm(
  cardio ~ age_years + height_imperial + weight_imperial +
    ap_hi + ap_lo +
    cholesterol_2_dv + cholesterol_3_dv +
    gluc_3_dv +
    smoke + alco + active,
  data = train,
  family = binomial
)

summary(cardiomodel2_train)

# Predicted probabilities on test set
pred_prob <- predict(cardiomodel2_train, newdata = test, type = "response")

# Classify using 0.5 cutoff
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)

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




plot_data <- data.frame(
  pred_prob = pred_prob,
  cardio = factor(test$cardio, levels = c(0, 1), labels = c("No Disease", "Disease"))
)

ggplot(plot_data, aes(x = pred_prob, fill = cardio, color = cardio)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Density of Predicted Cardiovascular Disease Probabilities",
    x = "Predicted Probability",
    y = "Density",
    fill = "Actual Class",
    color = "Actual Class"
  ) +
  theme_minimal()



thresholds <- seq(0.01, 0.99, by = 0.01)

results3 <- data.frame(
  threshold = thresholds,
  accuracy = NA,
  sensitivity = NA,   # TPR
  specificity = NA,   # TNR
  precision = NA,
  youden_j = NA,
  f1 = NA,
  tpr_precision_gap = NA
)

for (i in seq_along(thresholds)) {
  th <- thresholds[i]
  
  pred_class <- ifelse(pred_prob >= th, 1, 0)
  
  cm <- table(
    factor(pred_class, levels = c(0,1)),
    factor(test$cardio, levels = c(0,1))
  )
  
  TN <- cm["0","0"]
  TP <- cm["1","1"]
  FN <- cm["0","1"]
  FP <- cm["1","0"]
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  accuracy <- (TP + TN) / sum(cm)
  
  youden_j <- sensitivity + specificity - 1
  
  f1 <- ifelse(is.na(precision) || (precision + sensitivity) == 0,
               NA,
               2 * precision * sensitivity / (precision + sensitivity))
  
  gap <- abs(sensitivity - precision)
  
  results3$accuracy[i] <- accuracy
  results3$sensitivity[i] <- sensitivity
  results3$specificity[i] <- specificity
  results3$precision[i] <- precision
  results3$youden_j[i] <- youden_j
  results3$f1[i] <- f1
  results3$tpr_precision_gap[i] <- gap
}

best_youden <- results3[which.max(results3$youden_j), ]
best_f1 <- results3[which.max(results3$f1), ]
best_intersection <- results3[which.min(results3$tpr_precision_gap), ]

best_youden
best_f1
best_intersection

comparison <- rbind(
  Youden = best_youden,
  Harmonic_Mean_F1 = best_f1,
  TPR_Precision_Intersection = best_intersection
)

comparison







###-----------------------------
#####------------------------------------------------
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

#####------------------------------------------------------------------------------------------------
##Going to run lasso regression so model does not blowup 

# Make sure response is 0/1 numeric
class(t$cardio)
table(t$cardio)
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


# --- Prune cardiomodel8 while keeping hierarchy ---

coef_tab <- summary(cardiomodel8)$coefficients

coef_df <- data.frame(
  term = rownames(coef_tab),
  pval = coef_tab[, 4],
  stringsAsFactors = FALSE
)

# remove intercept
coef_df <- subset(coef_df, term != "(Intercept)")

# keep significant interaction terms
keep_interactions <- coef_df$term[
  grepl(":", coef_df$term) & coef_df$pval < 0.05
]

# keep significant main effects
keep_main_sig <- coef_df$term[
  !grepl(":", coef_df$term) & coef_df$pval < 0.05
]

# add parent main effects for every kept interaction
parent_main <- unique(unlist(strsplit(keep_interactions, ":")))

# final term list
keep_terms <- unique(c(keep_main_sig, parent_main, keep_interactions))

keep_terms
length(keep_terms)

# fit reduced post-lasso model
cardiomodel8_reduced_formula <- as.formula(
  paste("cardio ~", paste(keep_terms, collapse = " + "))
)

cardiomodel8_reduced <- glm(
  cardiomodel8_reduced_formula,
  data = t,
  family = binomial
)

summary(cardiomodel8_reduced)

# compare models
AIC(cardiomodel2, cardiomodel8_reduced, cardiomodel8)
anova(cardiomodel8_reduced, cardiomodel8, test = "Chisq")

# check whether anything else in the pruned model can go
drop1(cardiomodel8_reduced, test = "Chisq")



# Residual analysis - red line indicates quantile deviation
sim_res_model_cm8 <- simulateResiduals(cardiomodel8)
testoutliers_cm8 <- testOutliers(simulationOutput = sim_res_model_cm8, type = "bootstrap")


plotQQunif(sim_res_model_cm8)

plotResiduals(sim_res_model_cm8, smoothScatter = FALSE)

testDispersion(sim_res_model_cm8)
testUniformity(sim_res_model_cm8)

## Residual analysis reduced model 8 - red line indicates quantile deviation
sim_res_model_cm8_r <- simulateResiduals(cardiomodel8_reduced)
testoutliers_cm8 <- testOutliers(simulationOutput = sim_res_model_cm8_r, type = "bootstrap")


plotQQunif(sim_res_model_cm8_r)

plotResiduals(sim_res_model_cm8_r, smoothScatter = FALSE)

testDispersion(sim_res_model_cm8_r)
testUniformity(sim_res_model_cm8_r)

##Checked AUC to see how model differentiates between prescence of cardiovascular disease
probs <- as.numeric(unlist(predict(cardiomodel8_reduced, type = "response")))
actuals <- as.numeric(unlist(t$cardio))
## Calculate AUC 
roc_obj <- roc(actuals, probs)
auc(roc_obj)

plot(roc_obj, col= "lightblue", lwd=3, main = "ROc Curve for cm8_reduced")

####-------------------------------------------------------------------------------------------------  
#Plots for presentation: 

#cardiomodel2 plots easier to explain

##Odds-ratio plot on cardiomodel2

# Odds ratios and 95% CI
or_table <- data.frame(
  term = names(coef(cardiomodel2)),
  OR = exp(coef(cardiomodel2)),
  lower = exp(confint(cardiomodel2)[,1]),
  upper = exp(confint(cardiomodel2)[,2])
)

# Remove intercept
or_table <- subset(or_table, term != "(Intercept)")

# Optional nicer labels
or_table$term <- factor(
  or_table$term,
  levels = rev(or_table$term)
)

ggplot(or_table, aes(x = term, y = OR)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Odds Ratios from Main-Effects Logistic Model",
    x = "Predictor",
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal()

#AIC comparison plot

aic_table <- data.frame(
  model = c("Main Effects Model", "Full Post-Lasso Model", "Reduced Post-Lasso Model"),
  AIC = c(AIC(cardiomodel2), AIC(cardiomodel8), AIC(cardiomodel8_reduced))
)

ggplot(aic_table, aes(x = model, y = AIC)) +
  geom_col() +
  labs(
    title = "AIC Comparison of Cardiovascular Disease Models",
    x = "Model",
    y = "AIC"
  ) +
  theme_minimal()

## From the reduced model8 the most important interactions selected via LRT 
#age_years:ap_hi



#ap_hi:cholesterol_3_dv


#weight_imperial:ap_hi


#cholesterol_3_dv:gluc_3_dv








#diagnostic plots 
plotQQunif(sim_res_model_cm8_r)
plotResiduals(sim_res_model_cm8_r, smoothScatter = FALSE)

