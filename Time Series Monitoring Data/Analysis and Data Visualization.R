data <- read.csv("C:/Users/ACER NITRO/OneDrive/Desktop/mapua/3rd term/data sci/sample data/4_Time_series_Mointoring_data_Group_010.csv", stringsAsFactors = FALSE)
pacman::p_load("ggplot2", "ggfortify", "dplyr", "readr", "reshape2")

#Remove N/A
data <- na.omit(data)

#Format as numeric
data <- data %>%
  mutate(
    Month_numerical = as.numeric(Month_numerical),
    Patient_1_avg_steps = as.numeric(Patient_1_avg_steps),
    Patient_1_BMI = as.numeric(Patient_1_BMI),
    Patient_1_Stress_Level = as.numeric(Patient_1_Stress_Level)
  )

#Descriptive Statistics
summary_stats <- data %>%
  select(Month_numerical, Patient_1_avg_steps, Patient_1_BMI, Patient_1_Stress_Level) %>%
  summary()

print(summary_stats)

#Time series Plot
ggplot(steps, aes(x = Month_numerical)) +
  geom_line(aes(y = Patient_1_avg_steps, color = "Steps", linetype = "Steps"), size = 1.2) +
  geom_line(aes(y = Patient_1_Stress_Level * 1000, color = "Stress Level", linetype = "Stress Level")) +
  geom_line(aes(y = Patient_1_BMI * 100, color = "BMI", linetype = "BMI")) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Stress Level / BMI")) +
  labs(title = "Time Series: Steps, Stress & BMI - Patient 1", x = "Month", y = "Steps", color = "Metric", linetype = "Metric") +
  theme_minimal() +
  scale_color_manual(values = c("Steps" = "blue", "Stress Level" = "red", "BMI" = "green")) +
  scale_linetype_manual(values = c("Steps" = "solid", "Stress Level" = "dashed", "BMI" = "dotdash"))

#Multiple Linear Regression
multiple_linear_reg <- lm(Patient_1_Stress_Level ~ Patient_1_avg_steps * Patient_1_BMI, data = data)
summary(multiple_linear_reg)

#Pearson
cor.test(data$Patient_1_avg_steps, data$Patient_1_Stress_Level, method = "pearson")
cor.test(data$Patient_1_BMI, data$Patient_1_Stress_Level, method = "pearson")

