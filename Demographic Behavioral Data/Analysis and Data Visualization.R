data <- read.csv("C:/Users/ACER NITRO/OneDrive/Desktop/mapua/3rd term/data sci/sample data/2_Demographic_Behavioral_data_Group_010.csv", stringsAsFactors = FALSE)
pacman::p_load("ggplot2", "dplyr")

#Convert to factor with labels
data$Education <- factor(data$Education,
                         levels = c(0,1,2,3),
                         labels = c("Uneducated (0)", "Primary (1)", "Secondary (2)", "Tertiary (3)"))

data$HLS <- factor(data$Health_Literacy_Score,
                               levels = c(1,2,3,4,5),
                               labels = c("Uneducated", "Slightly Educated", "Moderately Educated", "Educated", "Extremely Educated"))

#Descriptive Stats

# Frequency table with percentages
education_summary <- as.data.frame(prop.table(table(data$Education)) * 100)
colnames(education_summary) <- c("Education Level", "Percentage")
education_summary$Frequency <- table(data$Education)

health_summary <- as.data.frame(prop.table(table(data$HLS)) * 100)
colnames(health_summary) <- c("Health Literacy Level", "Percentage")
health_summary$Frequency <- table(data$HLS)

# View
education_summary
health_summary


#Boxplot
ggplot(data, aes(x = Education, y = Health_Literacy_Score, fill = Education)) +
  geom_boxplot() +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("Uneducated (1)", "Slightly Educated (2)", "Moderately Educated (3)", "Educated (4)", "Extremely Educated (5)")) +
  labs(title = "Health Literacy Scores by Education Level", x = "Education Level", y = "Health Literacy") +
  theme_minimal()

# One-Way ANOVA
anova_result <- aov(Health_Literacy_Score ~ Education, data = data)
summary(anova_result)