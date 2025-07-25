data <- read.csv("C:/Users/ACER NITRO/OneDrive/Desktop/mapua/3rd term/data sci/sample data/1_Vital_signs_diagnosis_data_Group_010.csv", stringsAsFactors = FALSE)
pacman::p_load("ggplot2")

#Remove missing values
data <- na.omit(data)

#Descriptive Stats
summary(data)

#Convert necessary columns to numeric
data$Diastolic_BP <- as.numeric(data$Diastolic_BP)
data$Age <- as.numeric(data$Age)
data$Weight_kg <- as.numeric(data$Weight_kg)
data$Hypertension <- as.numeric(data$Hypertension)
data$Systolic_BP <- as.numeric(data$Systolic_BP)

#Compute BMI if N/A
data$BMI <- ifelse(is.na(data$BMI), data$Weight_kg / (data$Height_cm/100)^2, data$BMI)

# Function to process and plot data for each hypertension level
process_group <- function(hypertension_level) {
  # Filter data by hypertension level
  group_data <- subset(data, Hypertension == hypertension_level)
  group_data <- na.omit(group_data)
  
  # Calculate means
  mean_age <- mean(group_data$Age, na.rm = TRUE)
  mean_weight <- mean(group_data$Weight_kg, na.rm = TRUE)
  cat("Hypertension Level", hypertension_level, ": Mean Age =", mean_age, ", Mean Weight =", mean_weight, "\n")
  
  # Normalize data
  group_data$Age_normalized <- group_data$Age / max(group_data$Age, na.rm = TRUE)
  group_data$Weight_normalized <- group_data$Weight_kg / max(group_data$Weight_kg, na.rm = TRUE)
  
  # Create a data frame for plotting
  bar_data <- data.frame(
    Variable = c("Mean Age", "Mean Weight"),
    Value = c(mean_age, mean_weight)
  )
  
  # Bar plot of Mean Age and Weight
  bar_plot <- ggplot(bar_data, aes(x = Variable, y = Value, fill = Variable)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("skyblue", "salmon")) +
    labs(title = paste("Mean Age and Weight for Hypertensive Patients (Hypertension Level", hypertension_level, ")"),
         y = "Value",
         x = NULL) +
    theme_light() +
    theme(legend.position = "none")
  
  print(bar_plot)
  
  # Simple Linear Regression plot
  regression_plot <- ggplot(group_data, aes(x = Age_normalized, y = Weight_normalized)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste("Regression: Normalized Weight vs Age by Hypertension Status (Hypertension Level", hypertension_level, ")"),
         x = "Normalized Age",
         y = "Normalized Weight") +
    theme_light()
  
  print(regression_plot)
  
  # Fit linear model
  model <- lm(Weight_normalized ~ Age_normalized, data = group_data)
  
  # Show summary of regression
  reg_summary <- summary(model)
  print(reg_summary)
  
  # Optional: print key parts clearly
  cat("\nSimple Linear Regression Results Simplified\n")
  cat("Intercept:", round(reg_summary$coefficients[1,1], 4), "\n")
  cat("Slope:", round(reg_summary$coefficients[2,1], 4), "\n")
  cat("R-squared:", round(reg_summary$r.squared, 4), "\n")
  cat("p-value for slope:", round(reg_summary$coefficients[2,4], 4), "\n")
}

# Use the function for different hypertension levels
process_group(1)
process_group(3)



