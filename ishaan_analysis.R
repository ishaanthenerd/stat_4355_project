# --- Installing and importing libraries ---
# install.packages("car", type = "binary")
# install.packages("ggplot2", type = "binary")
library(car)
library(ggplot2)

# --- Add functions for plotting ---

# Plot two continuous variables
plot_two_continuous <- function(data, x_var, y_var, point_color = "blue", point_size = 2, file_path = "") {
    if (!is.data.frame(data)) {
        stop("Error: 'data' must be a data frame.")
    }
    if (!all(c(x_var, y_var) %in% names(data))) {
        stop("Error: Both variables must exist in the data frame.")
    }
    if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])) {
        stop("Error: Both variables must be numeric (continuous).")
    }
    
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
        geom_point(color = point_color, size = point_size, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
        labs(
            title = paste("Scatter Plot of", x_var, "vs", y_var),
            x = x_var,
            y = y_var
        ) +
        theme_minimal()
    print(p)

    if (file_path != "") {
        ggsave(file_path)
    }
}

# Plot two discrete variables
plot_discrete_continuous <- function(data, x_var, y_var, fill_color = "lightblue", file_path = "") {
    if (!is.data.frame(data)) {
        stop("Error: 'data' must be a data frame.")
    }
    if (!all(c(x_var, y_var) %in% names(data))) {
        stop("Error: Both variables must exist in the data frame.")
    }
    if (!is.factor(data[[x_var]]) && !is.character(data[[x_var]])) {
        stop("Error: 'x_var' must be categorical (factor or character).")
    }
    if (!is.numeric(data[[y_var]])) {
        stop("Error: 'y_var' must be numeric (continuous).")
    }
    
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
        geom_boxplot(fill = fill_color, alpha = 0.7) +
        labs(
            title = paste("Boxplot of", y_var, "by", x_var),
            x = x_var,
            y = y_var
        ) +
        theme_minimal()
    print(p)

    if (file_path != "") {
        ggsave(file_path)
    }
}

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
df$age_years <- df$age / 365

# Convert height to imperial units (cm -> in)
df$height_imperial <- df$height / 2.54

# Convert weight to imperial units (kg -> lb)
df$weight_imperial <- df$weight * 2.20462262

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
df$gender_dv <- ifelse(df$gender == "2", 1, 0)
df$cholesterol_2_dv <- ifelse(df$cholesterol == "2", 1, 0)
df$cholesterol_3_dv <- ifelse(df$cholesterol == "3", 1, 0)
df$gluc_2_dv <- ifelse(df$gluc == "2", 1, 0)
df$gluc_3_dv <- ifelse(df$gluc == "3", 1, 0)
df$smoke_dv <- ifelse(df$smoke == "1", 1, 0)
df$alco_dv <- ifelse(df$alco == "1", 1, 0)
df$active_dv <- ifelse(df$active == "1", 1, 0)
df$cardio_dv <- ifelse(df$cardio == "1", 1, 0)

# GOAL 1: Identifying how to keep healthy systolic / diastolic blood pressure
# 90 ~ 120 mmHg for systolic, 60 ~ 80 mmHg for diastolic (source: AHA)

# Building an initial model
model_v1 <- lm(ap_hi ~ age + gender_dv + height_imperial + weight_imperial + cholesterol_2_dv + cholesterol_3_dv + gluc_2_dv + gluc_3_dv + smoke_dv + alco_dv + active_dv, data = df)

# Checking for high VIFs
vif_v1 <- vif(model_v1)
vif_v1_df <- data.frame(Variable = names(vif_v1), VIF = vif_v1)
high_vif_threshold <- 5
ggplot(vif_v1_df, aes(x = Variable, y = VIF)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_y_continuous(limits = c(0, max(vif_v1_df$VIF))) +
    labs(title = "Variance Inflation Factor (VIF) for Regression Model",
        y = "VIF",
        x = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("vif_v1.png")

# All VIFs are under 2, so no need to remove variables

# Determine how well the model does
summary_model_v1 <- summary(model_v1)

# Observations:
# 1. All but two variables, when considered as stand-alone, are significant in the model. Only the smoke_dv and active_dv are not significant.
# 2. The adjusted R^2 value is 0.1332, indicating that the model (as it stands) is not very good.
# One idea to fix the prior point is to add clustering of data based on some subset of factors, then compute regression lines for each. 

# Handle clustering for variables with two values
for (col_name in c("gender", "smoke", "alco", "active", "cardio")) {
    print(col_name) # TODO: add logic
    # NOTE: you must use df[, col_name] instead of $.
}

# Handle clustering for variables with three values
for (col_name in c("cholesterol", "gluc")) {
    print(col_name) # TODO: add logic
    # NOTE: you must use df[, col_name] instead of $.
}

# GOAL 2: Identifying how to reduce risk of cardiovascular disease