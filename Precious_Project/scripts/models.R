# Creating a Linear Regression Model to see the correlation between Risk rate an Pass Rate
library(stats)
library(ggplot2)
library(caTools)
library(readr)

# Linear Model: Pass Rate ####

#splitting the data 80:20 for Testing and Training Data

#this library for sample.split
split <- sample.split(combine_data_risk_pass$Pass_Rate, SplitRatio = 0.8)
train_data <- subset(combine_data_risk_pass, split == TRUE)
test_data <- subset(combine_data_risk_pass, split == FALSE)

# Training a Linear model
lr_model <- lm(Pass_Rate ~ Risk_One_Rate + Risk_Two_Rate + Risk_Three_Rate + Year + Facility_Type, data = train_data)

# Predicting
pred <- predict(lr_model, newdata = test_data)

# Add predictions to the test_data
test_data$pred <- pred

# Plotting the results
library(ggplot2) # Ensure you have this library for plotting
ggplot(test_data, aes(x = Pass_Rate, y = pred, color = Facility_Type)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Real vs Predicted Values",
       x = "True Pass Rate",
       y = "Predicted Pass Rate")

# Linear Model Risk Rate ####

# Chi - Square ####

