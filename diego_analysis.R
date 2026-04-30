
t <- read.csv("cardio_train_standard.csv")


t <- subset(t, ap_hi >= 80 & ap_hi <=250 & ap_lo >=40 & ap_lo <= 150)

#Impreial Units
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

library(car)

vif(vifmodel)
