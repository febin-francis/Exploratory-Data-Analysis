View(NREGA)
dim(NREGA)
class(NREGA)
str(NREGA)
summary(NREGA)
print(head(NREGA))
mydata=read.csv("/Users/febinfrancis/Desktop/NREGA.csv", header=T)
names(mydata)

#Section A

#1 & 2)
mean = mean(mydata$Total.No..of.Workers)
print(mean)
median = median(mydata$Total.No..of.Workers)
print(median)
standard_deviation <- sd(data$`Total.No..of.Workers`)
print(standard_deviation)
range<- range(data$`Total.No..of.Workers`)
print(range)


attach(mydata)
library(dplyr)
library(ggplot2)

#Histogram
ggplot(NREGA, aes(x = Average.days.of.employment.provided.per.Household)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "red", alpha = 0.8) +
  labs(title = "Average days of employment provided per Household Distribution", x = "Average days of employment provided per Household", y = "Frequency")

#Interpretation
#The histogram is of "Average days of employment provided per Household Distribution".
  
#- The x-axis represents the "Average days of employment provided per Household", ranging from 0 to over 40.
#- The y-axis represents the frequency, indicating how many households fall into each category of average days employed, ranging from 0 to over 80.
#- The bars are colored in blue.
#- Most households have around 20-25 average days of employment, as indicated by the highest bars in the center.
#- There are fewer households with very low or very high average days of employment, shown by shorter bars on either end.

#In summary, this histogram suggests that the most common average days of employment provided per household is around 20-25 days.

#Bar graph
ggplot(mydata, aes(x = factor(state_name), fill = factor(state_name))) +
  geom_bar() +
  labs(title = "state_name", x = "State Name", y = "No.of.Count") +
  theme_minimal()

#Interpretation
#This bar graph represents data across various states in India.
#The x-axis represents different states in India.
#The y-axis represents counts from 0 to 80. 
#Different colors represent different states; for example, orange bars represent Andaman and Nicobar Islands through Gujarat, blue bars represent Haryana through Lakshadweep, etc.
#Uttar Pradesh (pink) has the highest bar reaching close to 80 on the y-axis.
#Maharashtra (also pink) and Andhra Pradesh (orange) have significant counts as well but are less than Uttar Pradesh.
#Some states like Nagaland (blue), Sikkim (purple), and Mizoram (pink) have very low counts.
#In summary, this bar graph suggests Uttar Pradesh has the highest count of the unspecified metric, followed by Maharashtra and Andhra Pradesh

#Scatter Plot
ggplot(data, aes(x = Total.No..of.Workers, y = Total.Exp.Rs..in.Lakhs..)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Total Number of Workers",
       y = "Total Expenditure (Rs. in Lakhs)",
       title = "Scatter Plot of Total Expenditure vs. Total Number of Workers")

#Interpretation
# This Scatter Plot is of Total Expenditure vs. Total Number of Workers".
  
#- The x-axis represents the "Total Number of Workers", ranging from 0 to 2,000,000.
#- The y-axis represents the "Total Expenditure (Rs in lakhs)", ranging from 0 to 50,000.
#- The black dots represent data points scattered across the graph. There is a dense clustering at the lower left corner indicating many instances where both total expenditure and number of workers are relatively low.
#- A blue line runs diagonally from the origin towards the upper right corner. This line represents the trend in the data and indicates a positive correlation between the two variables. This means as the number of workers increases, the total expenditure also tends to increase.

#In summary,there is a positive correlation between the total number of workers and the total expenditure. As the number of workers increases, the total expenditure also tends to increase. 

# Scatter plot
ggplot(data, aes(x = Total.No..of.Active.Job.Cards, y = Total.No..of.Active.Workers)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Total Number of Active Job Cards",
       y = "Total Number of Active Workers",
       title = "Scatter Plot of Active Workers vs. Total Number of Active Job Cards")

#Interpretation
#The scatter plot  is of Active Workers vs. Total Number of Active Job Cards.
  
#- The x-axis represents the "Total Number of Active Job Cards", ranging from 0 to 1,000,000.
#- The y-axis represents the "Total Number of Active Workers", ranging from 0 to 1,500,000.
#- The black dots represent data points scattered across the graph. There is a general trend where an increase in active job cards correlates with an increase in active workers.
#- A blue line runs diagonally across the plot, indicating a linear relationship between the two variables. This suggests a positive correlation between the total number of active workers and the total number of active job cards. As the number of active workers increases, so does the total number of job cards.
#- There are several outliers visible where there are more job cards than would be expected for the given number of workers. These could be instances where job cards are issued but not all are being utilized by active workers.

#In summary, this scatter plot suggests that there is a positive correlation between the total number of active workers and the total number of active job cards. However, the presence of outliers indicates that there may be other factors at play affecting the relationship between these two variables.


# Categorical Variable Analysis: Contingency Tables and Chi-square Tests
contingency_table <- table(data$`Total.Households.Worked`, data$`Total.No..of.Workers`)
contingency_table
chisq.test(contingency_table)

# Regression Analysis
model <- lm(`Total.Households.Worked` ~ `Total.No..of.Workers`, data = data)
summary(model)

plot(data$Total.No..of.Workers, data$Total.Households.Worked, 
     xlab = "Total Number of Workers", ylab = "Total Households Worked",
     main = "Regression Analysis: Total Households Worked vs. Total Number of Workers",
     col = "blue", pch = 19)
abline(model, col = "red")


#Section - B

#1)

# Building Model

Model1=lm(Total.No..of.JobCards.issued~Total.No..of.Workers)
summary(Model1)
anova(Model1)

# Model 2 
Model2=lm(Total.No..of.JobCards.issued~Approved.Labour.Budget)
summary(Model2)
anova(Model2)

# Model 3 
Model3=lm(Total.No..of.JobCards.issued~Total.No..of.Workers+Approved.Labour.Budget)
summary(Model3)
anova(Model3)

#Based on these models, Model 3 seems to be the most optimal for predicting the total number of job cards issued. It includes both significant independent variables, Total Number of Workers and Approved Labour Budget, resulting in the highest R-squared value and the best model fit.

# Extract predicted values from Model 3
predicted_values <- predict(Model3)

# Plot actual vs. predicted job cards
plot(data$Total.No..of.JobCards.issued, predicted_values, 
     xlab = "Actual Total Number of Job Cards", ylab = "Predicted Total Number of Job Cards",
     main = "Actual vs. Predicted Total Number of Job Cards",
     col = "blue", pch = 19)

# Add a diagonal reference line
abline(0, 1, col = "red")



#Now, to predict the total number of job cards if significant independent variables are increased by an additional 30%, we can use the coefficients from Model 3:


# Extract coefficients from Model 3
coefficients_model3 <- coef(Model3)

# Increase significant independent variables by 30%
new_total_workers <- coefficients_model3[2] * 1.3
new_approved_budget <- coefficients_model3[3] * 1.3

# Predict the total number of job cards
predicted_job_cards <- coefficients_model3[1] + new_total_workers + new_approved_budget
print(predicted_job_cards)



#2)
Model1=lm(Total.No..of.Workers~Number.of.Ongoing.Works)
summary(Model1)
anova(Model1)

# Model 2 
Model2=lm(Total.No..of.Workers~Number.of.Completed.Works)
summary(Model2)
anova(Model2)

# Model 3 
Model3=lm(Total.No..of.Workers~Number.of.Ongoing.Works+Number.of.Completed.Works)
summary(Model3)
anova(Model3)

#Based on these models, Model 1 and Model 3 seem to be the most promising for predicting worker engagement.

#Model 1
# Scatter plot
plot(data$Number.of.Ongoing.Works, data$Total.No..of.Workers, 
     xlab = "Number of Ongoing Works", ylab = "Total Number of Workers",
     main = "Relationship between Ongoing Works and Worker Engagement",
     col = "blue", pch = 19)

# Add regression line
abline(Model1, col = "red")


#Now, to predict worker engagement if significant independent variables increase by an additional 20%, we can use the coefficients from Model 1 (assuming it as the optimal model):


# Extract coefficients from Model 1
coefficients_model1 <- coef(Model1)

# Increase significant independent variable by 20%
new_ongoing_works <- coefficients_model1[2] * 1.2

# Predict the total number of workers engaged
predicted_workers <- coefficients_model1[1] + new_ongoing_works
print(predicted_workers)
