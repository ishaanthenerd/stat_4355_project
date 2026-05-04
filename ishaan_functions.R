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