# --- Loading data ---
df <- read.csv("C:\\Users\\ishaa\\Downloads\\stat_4355_project\\cardio_train.csv", sep=";")

# --- Filtering data ---

# Remove NAs and blanks
df <- df[rowSums(is.na(df) | df == "") != ncol(df), ]

# Remove absurd values for diastolic / systolic blood pressure
df <- df[df$lo >= 6, ]
df <- df[df$lo <= 360, ]
df <- df[df$ap_hi >= 30, ]
df <- df[df$ap_hi <= 370, ]

# Remove people with absurd BMI
df$bmi <- df$weight / (df$height / 100)^2
df <- df[df$bmi >= 10, ]
df <- df[df$bmi <= 50, ]

# Remove people with absurd height
df <- df[df$height >= 120, ]
df <- df[df$height <= 210, ]

# All weights remaining after the above steps are in a reasonable range; no need to filter

# All ages are in a reasonable range; no need to filter

# --- Manipulating data ---

# Convert age (days -> years)
df$age_years <- df$age / 365

# Convert height to imperial units (cm -> in)
df$height_imperial <- df$height / 2.54

# Convert weight to imperial units (kg -> lb)
df$weight_imperial <- df$weight * 2.20462262

# Create dummy variables for categories that are non-numerical 
# Variables: gender, cholesterol, gluc, smoke, alco, active, cardio
df$gender_dv <- ifelse(df$gender == 2, 1, 0)
df$cholesterol_2_dv <- ifelse(df$cholesterol == 2, 1, 0)
df$cholesterol_3_dv <- ifelse(df$cholesterol == 3, 1, 0)
df$gluc_2_dv <- ifelse(df$gluc == 2, 1, 0)
df$gluc_3_dv <- ifelse(df$gluc == 3, 1, 0)
df$smoking_dv <- ifelse(df$smoke == 1, 1, 0)
df$alco_dv <- ifelse(df$alco == 1, 1, 0)
df$active_dv <- ifelse(df$active == 1, 1, 0)
df$cardio_dv <- ifelse(df$cardio == 1, 1, 0)

# GOAL 1: Identifying how to keep healthy systolic / diastolic blood pressure

# GOAL 2: Identifying how to reduce risk of cardiovascular disease