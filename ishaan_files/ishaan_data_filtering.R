# --- Loading data ---
df <- read.csv("C:\\Users\\ishaa\\Downloads\\stat_4355_project\\cardio_train.csv", sep = ";") 

# --- Filtering data ---

# Remove NAs and blanks
df <- df[rowSums(is.na(df) | df == "") != ncol(df), ]

# Remove absurd values for diastolic / systolic blood pressure
df <- df[df$ap_lo >= 6, ]
df <- df[df$ap_lo <= 360, ]
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
df$age_years <- as.numeric(df$age / 365)

# Convert height to imperial units (cm -> in)
df$height_imperial <- as.numeric(df$height / 2.54)

# Convert weight to imperial units (kg -> lb)
df$weight_imperial <- as.numeric(df$weight * 2.20462262)

# Turn non-numerical variables into factors
df$gender <- as.factor(df$gender)
df$cholesterol <- as.factor(df$cholesterol)
df$gluc <- as.factor(df$gluc)
df$smoke <- as.factor(df$smoke)
df$alco <- as.factor(df$alco)
df$active <- as.factor(df$active)
df$cardio <- as.factor(df$cardio)

# Create dummy variables for categories that are non-numerical
# Variables: gender, cholesterol, gluc, smoke, alco, active, cardio
df$gender_dv <- as.numeric(df$gender == 2)
df$cholesterol_2_dv <- as.numeric(df$cholesterol == 2)
df$cholesterol_3_dv <- as.numeric(df$cholesterol == 3)
df$gluc_2_dv <- as.numeric(df$gluc == 2)
df$gluc_3_dv <- as.numeric(df$gluc == 3)
df$smoke_dv <- as.numeric(df$smoke == 1)
df$alco_dv <- as.numeric(df$alco == 1)
df$active_dv <- as.numeric(df$active == 1)
df$cardio_dv <- as.numeric(df$cardio == 1)

# Save data
write.csv2(df, file = "filtered_cardio.csv", row.names = FALSE)