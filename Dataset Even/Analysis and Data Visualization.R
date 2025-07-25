data <- read.csv("C:/Users/ACER NITRO/OneDrive/Desktop/mapua/3rd term/data sci/sample data/Dataset_even.csv", stringsAsFactors = FALSE)
pacman::p_load("ggplot2", "dplyr")

#Format data & Set BMI Groups
data <- data %>%
  mutate(
    BMI = as.numeric(BMI),
    Stress_Level = as.numeric(Stress_Level),
    BMI_group = case_when(
      BMI >= 18.5 & BMI < 25 ~ "Normal_Weight",
      BMI >= 25 ~ "Overweight",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(BMI_group) & !is.na(Stress_Level))  #Remove NA

#Descriptive statistics
summary_stats <- data %>%
  select(Stress_Level, BMI) %>%
  summary()
print(summary_stats)

#Plot
ggplot(data, aes(x = BMI, y = Stress_Level)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(
    title = "Stress Level vs. BMI",
    x = "BMI",
    y = "Stress Level"
  ) +
  theme_minimal()

#T-test
ttest_result <- t.test(Stress_Level ~ BMI_group, data = data)
print(ttest_result)


