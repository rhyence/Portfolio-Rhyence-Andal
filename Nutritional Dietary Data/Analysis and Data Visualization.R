data <- read.csv("C:/Users/ACER NITRO/OneDrive/Desktop/mapua/3rd term/data sci/sample data/3_Nutritional_Dietary_data_Group_010.csv", stringsAsFactors = FALSE)
pacman::p_load("ggplot2", "dplyr", "tidyr")

#Remove N/A
data <- data %>%
  drop_na(Daily_Caloric_Intake_kcal, BMI)

#Format
data$Daily_Caloric_Intake_kcal <- as.numeric(data$Daily_Caloric_Intake_kcal)
data$BMI <- as.numeric(data$BMI)

#Descriptive Statistics
summary_stats <- data %>%
  select(Daily_Caloric_Intake_kcal, BMI) %>%
  summary()

print(summary_stats)

#Scatter Plot of Daily Caloric Intake vs. BMI
ggplot(data, aes(x = Daily_Caloric_Intake_kcal, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Daily Caloric Intake vs. BMI", x = "Daily Caloric Intake", y = "BMI") +
  theme_minimal()


#Frequency of BMI
hist_data <- data.frame(BMI = data$BMI)
hist <- ggplot_build(ggplot(hist_data, aes(x = BMI)) +
                       geom_histogram(binwidth = 1, fill = "skyblue", color = "black"))$data[[1]]

#Plot
ggplot(hist, aes(x = x, y = count)) +
  geom_line(color = "skyblue") +
  labs(title = "Frequency Polygon of BMI", x = "BMI", y = "Frequency") +
  theme_minimal()

#Pearson Correlation Coefficient
cor_test_result <- cor.test(data$Daily_Caloric_Intake_kcal, data$BMI)
print(cor_test_result)
