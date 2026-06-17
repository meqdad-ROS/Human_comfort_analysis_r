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
data  <- read.csv("BothdataSets/Exploratory Factor Analysis/group_cfa_factors_input_data.csv", header = TRUE)
Names <- colnames(data)

x = data$Li_So_Effect      #Sensitivity #Performance #Li_So_Effect #Th_AQ_Effect
y = data$Th_AQ_Effect     #Sensitivity #Performance #Li_So_Effect #Th_AQ_Effect

plot(x,y, type='l', col="blue")
points(x,y, col = "red", pch = 19)  # Adds red points to the line

# Arrange by Age in ascending order
sorted_data <- arrange(data, Th_AQ_Effect)

x = data$Li_So_Effect  #Sensitivity #Performance #Li_So_Effect #Th_AQ_Effect
y = data$Th_AQ_Effect  #Sensitivity #Performance #Li_So_Effect #Th_AQ_Effect
#plot it again 
plot(x,y, type='l', col="blue")
points(x,y, col = "red", pch = 19)  # Adds red points to the line

# Correlation between Variable1 and Variable2
correlation <- cor(x, y, method = "pearson")
print(correlation)

correlation_matrix <- cor(data[,c(7,8,9,10)], method = "pearson")
print(correlation_matrix)
# Print the result
cat("Pearson Correlation Coefficient:", correlation, "\n")


library(corrplot)              # Load corrplot package
# Visualize the correlation matrix
corrplot(correlation_matrix, method = "circle")

# Fit a linear regression model
model <- lm(data$Th_AQ_Effec ~ data$Li_So_Effect, data = data)
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

# Plot the data points
plot(data$Li_So_Effect, data$Th_AQ_Effec, main = "Linear Regression", 
     xlab = "Li_So_Effect", ylab = "Th_AQ_Effec", pch = 19)

# Add the regression line
abline(model, col = "blue", lwd = 2)

# Calculate predictions
predictions <- predict(model)

# Calculate residuals
residuals <- data$Th_AQ_Effec - predictions

# Calculate error range
error_range <- range(residuals)
print(error_range)  # This will give you the minimum and maximum residuals

# Calculate IQR
IQR_value <- IQR(residuals)

# Define outlier threshold
outlier_threshold <- 1.5 * IQR_value

# Identify outliers
outliers <- data[abs(residuals) > outlier_threshold, ]
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


clean_data <- data[abs(residuals) <= outlier_threshold, ]

# Refit model without outliers
clean_model <- lm(clean_data$Th_AQ_Effec ~ clean_data$Li_So_Effect, data = clean_data)
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
residuals_clean <- clean_data$Th_AQ_Effec - predictions_clean
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
correlation_matrix_clean <- cor(clean_data[,c(7,8,9,10)], method = "pearson")
print(correlation_matrix)
# Visualize the correlation matrix
corrplot(correlation_matrix_clean, method = "circle")

###=================================================================================
####======================================= Multi-linear Regression











###=================================================================================
####======================================= Logistic Regression