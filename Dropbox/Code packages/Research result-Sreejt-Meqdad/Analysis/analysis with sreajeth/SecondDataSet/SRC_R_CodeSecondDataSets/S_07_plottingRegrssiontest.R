#Hypotheses testing and Anova test for different groupd
# Written by Meqdad Hasan 
#clear plotting device, concule and all other variabels 

cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables



# step 0: Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)             # Load dplyr package


# Step 1: load data 
Data  <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)
Names <- colnames(Data)

sorted_df_desc <- Data[order(-Data$productivity), ]
sorted_df_desc <- Data[order(-Data$Sensitivity), ]
sorted_df_desc <- Data[order(-Data$Work_effectively), ]
sorted_df_desc <- Data[order(-Data$performance_decrease_under_thermal_and_Air), ]
sorted_df_desc <- Data[order(-Data$performance_decrease_under_noise_and_light), ]
sorted_df_desc <- Data[order(-Data$Combined_Effect_of_Air_thermal_noise_light), ]

x = Data$productivity       
y = Data$Sensitivity      
z = Data$Work_effectively
xx = Data$performance_decrease_under_thermal_and_Air
yy = Data$performance_decrease_under_noise_and_light
zz = Data$Combined_Effect_of_Air_thermal_noise_light

  

# Based on correlation matrix we have many correlations: 
# -1- # productivity && performance_decrease_under_thermal_and_Air   --->  0.7270199
# -2- # productivity && Sensitivity                                  --->  0.7205978
# -3- # productivity && Combined_Effect_of_Air_thermal_noise_light   --->  0.6571341
# -4- # performance_decrease_under_thermal_and_Air && performance_decrease_under_noise_and_light  --->  0.73312289
# -5- # performance_decrease_under_thermal_and_Air && Combined_Effect_of_Air_thermal_noise_light  --->  0.83607107
# -6- # performance_decrease_under_thermal_and_Air && Sensitivity  --->  0.58575757
# -7- # Sensitivity && Combined_Effect_of_Air_thermal_noise_light  --->  0.5909322
# -8- # performance_decrease_under_noise_and_light && Combined_Effect_of_Air_thermal_noise_light  --->  0.67403380



plot(x,y, type='l', col="blue")
points(x,y, col = "red", pch = 19)  # Adds red points to the line

plot(x,z, type='l', col="blue")
points(x,z, col = "red", pch = 19)  # Adds red points to the line.

plot(x,xx, type='l', col="blue")
points(x,xx, col = "red", pch = 19)  # Adds red points to the line

plot(x,yy, type='l', col="blue")
points(x,yy, col = "red", pch = 19)  # Adds red points to the line

 plot(x,zz, type='l', col="blue")
points(x,zz, col = "red", pch = 19)  # Adds red points to the line

# Arrange by Age in ascending order
sorted_data <- arrange(Data, Sensitivity)

x = Data$productivity    #Sensitivity #Performance #Li_So_Effect #Th_AQ_Effect
y = Data$Sensitivity  #Sensitivity #Performance #Li_So_Effect #Th_AQ_Effect
#plot it again 
plot(x,y, type='l', col="blue")
points(x,y, col = "red", pch = 19)  # Adds red points to the line

# Correlation between Variable1 and Variable2
correlation <- cor(x, y, method = "pearson")
print(correlation)

noNAData <- na.omit(Data)
correlation_matrix <- cor(noNAData[,c(18,19,20,21,22,23)], method = "pearson")
print(correlation_matrix)
# Print the result
cat("Pearson Correlation Coefficient:", correlation, "\n")


library(corrplot)              # Load corrplot package
# Visualize the correlation matrix
corrplot(correlation_matrix, method = "circle")

# Fit a linear regression model
model <- lm(noNAData$performance_decrease_under_thermal_and_Air ~ noNAData$Combined_Effect_of_Air_thermal_noise_light, data = noNAData)
# View the summary of the model
summary(model)


# Make predictions
newdata <- data.frame(newdata = seq(from = -2, to = 2, by = 0.1))
predictions <- predict(model, newdata = newdata)
print(predictions)
# Extract coefficients
coefficients <- coef(model)
# Intercept (b)
b <- coefficients[1]
# Slope (a)
a <- coefficients[2]
# Print the results
cat("Slope (a):", a, "\n")
cat("Intercept (b):", b, "\n")


# sorted_data <- arrange(noNAData, productivity)
# # Plot the data points
# plot(sorted_data$productivity, sorted_data$performance_decrease_under_thermal_and_Air, main = "Linear Regression", 
#      xlab = "productivity", ylab = "performance_decrease_under_thermal_and_Air", pch = 19)
# 
# # Add the regression line
# abline(model, col = "blue", lwd = 2)
# 
# # now plot the scatter plot based on categorial function. for example 
# # How_quickly_notice_IEQ_parameter_change && # productivity && performance_decrease_under_thermal_and_Air   --->  0.7270199
# # Basic scatter plot with points colored by group
# P <- ggplot(sorted_data, aes(x = productivity, y = performance_decrease_under_thermal_and_Air, color = Age )) +
#   geom_point() +  # Scatter points
#   geom_smooth(method = "lm", se = TRUE) +  # Add regression line (with confidence interval)
#   labs(title = "Regression Plot Colored by Group",
#        x = "Predictor Variable",
#        y = "Response Variable") +
#   theme_minimal()
# print(P)


sorted_data <- arrange(noNAData, productivity)
# Plot the data points
plot(sorted_data$productivity, sorted_data$Sensitivity, main = "Linear Regression", 
     xlab = "productivity", ylab = "Sensitivity", pch = 19)

# Add the regression line
abline(model, col = "blue", lwd = 2)

# now plot the scatter plot based on categorial function. for example 
# How_quickly_notice_IEQ_parameter_change && # productivity && performance_decrease_under_thermal_and_Air   --->  0.7270199
# Basic scatter plot with points colored by group
P <- ggplot(sorted_data, aes(x = productivity, y = Sensitivity, color = Age )) +
  geom_point() +  # Scatter points
  geom_smooth(method = "lm", se = TRUE) +  # Add regression line (with confidence interval)
  labs(title = "Regression Plot Colored by Group",
       x = "Predictor Variable",
       y = "Response Variable") +
  theme_minimal()
print(P)


# Calculate predictions
predictions <- predict(model)

# Calculate residuals
residuals <- noNAData$performance_decrease_under_thermal_and_Air - predictions

# Calculate error range
error_range <- range(residuals)
print(error_range)  # This will give you the minimum and maximum residuals

# Calculate IQR
IQR_value <- IQR(residuals)

# Define outlier threshold
outlier_threshold <- 1.5 * IQR_value

# Identify outliers
outliers <- Data[abs(residuals) > outlier_threshold, ]
print(outliers)  # This will show the rows in the original data that are outliers


# Calculate maximum difference
max_difference <- max(abs(residuals))
print(max_difference)
print(paste("Maximum Difference: ", max_difference))


# Calculate the variance of the residuals
variance_of_residuals <- var(residuals)
print(paste("Variance of Residuals:", variance_of_residuals))

# Calculate the standard deviation of the residuals
std_dev_of_residuals <- sd(residuals)
print(paste("Standard Deviation of Residuals:", std_dev_of_residuals))


clean_data <- Data[abs(residuals) <= outlier_threshold, ]

# Refit model without outliers
clean_model <- lm(clean_data$performance_decrease_under_thermal_and_Air ~ clean_data$Combined_Effect_of_Air_thermal_noise_light, data = clean_data)
summary(clean_model)
coefficients_clean <- coef(clean_model)
# Intercept (b)
b_clean <- coefficients_clean[1]
# Slope (a)
a_clean <- coefficients_clean[2]
# Print the results
cat("Slope (a):", a_clean, "\n")
cat("Intercept (b):", b_clean, "\n")
# Calculate predictions
predictions_clean <- predict(clean_model)
# Calculate residuals
residuals_clean <- clean_data$performance_decrease_under_thermal_and_Air - predictions_clean
error_range_clean <- range(residuals_clean)
print(error_range_clean)  # This will give you the minimum and maximum residuals
# Calculate maximum difference
max_difference <- max(abs(residuals_clean))
print(max_difference)
print(paste("Maximum Difference: ", max_difference))
# Calculate the variance of the residuals
variance_of_residuals_clean <- var(residuals_clean)
print(paste("Variance of Residuals:", variance_of_residuals_clean))
# Calculate the standard deviation of the residuals
std_dev_of_residuals_clean <- sd(residuals_clean)
print(paste("Standard Deviation of Residuals:", std_dev_of_residuals_clean))
##### Calculating square error ===============================================
SSE = sum(residuals_clean^2) ## sum of square error 
###============== Correlation of clean data
correlation_matrix_clean <- cor(clean_data[complete.cases(clean_data),c(18,19,20,21,22,23)], method = "pearson")
print(correlation_matrix)
# Visualize the correlation matrix
corrplot(correlation_matrix_clean, method = "circle")

###=================================================================================
####======================================= Multi-linear Regression











###=================================================================================
####======================================= Logistic Regression