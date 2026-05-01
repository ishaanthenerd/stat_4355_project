library(car)

t <- read.csv("cardio_train_standard.csv")
t <- subset(t, ap_hi >= 30 & ap_hi <=370 & ap_lo >=6 & ap_lo <= 370)
t <- na.omit(t)
t <- t[complete.cases(t), ]

# Remove people with absurd BMI
t$bmi <- t$weight / (t$height / 100)^2
t <- t[t$bmi >= 10, ]
t <- t[t$bmi <= 50, ]

# Remove people with absurd height
t <- t[t$height >= 120, ]
t <- t[t$height <= 210, ]

# All weights remaining after the above steps are in a reasonable range; no need to filter

# All ages are in a reasonable range; no need to filter


#Imperial Units
t$height_in <- t$height / 2.54 
t$weight_lbs <- t$weight * 2.20462

#Age in years 

t$age_years <- t$age /365

#Dummy variables
t$gender_female <- ifelse(t$gender == 1, 1, 0)
t$cholesterol_above <- ifelse(t$cholesterol == 2, 1,0) 
t$cholesterol_wellabove <- ifelse(t$cholesterol == 3, 1,0)
t$gluc_above <- ifelse(t$gluc == 2, 1,0)
t$gluc_wellabove <- ifelse(t$gluc == 3, 1,0) 
names(t)


#Model to reduce with vif

vifmodel <- lm(cardio ~age_years + weight_lbs + height_in + cholesterol_wellabove + cholesterol_above + gluc_wellabove 
               + gluc_above + gender_female + active + alco + smoke + ap_hi + ap_lo, data = t)

vif(vifmodel)


cardiomodel <- glm(cardio ~age_years + weight_lbs + height_in + cholesterol_wellabove + cholesterol_above + gluc_wellabove 
                  + gluc_above + gender_female + active + alco + smoke + ap_hi + ap_lo, data = t, family = binomial)
summary(cardiomodel)

