#Linear regression and other alternative algorithm for data analysis 
# Written by Meqdad Hasan 
#clear all environment variable and ploting device and consul. 
#-------------------------------------------------------------------------------
cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables
#-------------------------------------------------------------------------------
# step 0: Load necessary libraries
library(readxl)  #Load excel library
library(ggplot2)
library(reshape2)
library(plyr)
library(car)
library(multcomp)
library(lavaan)
library(semPlot)
library(psych)
library(splithalf)
library(ltm)
library(dplyr)
library(R6)

#-------------------------------------------------------------------------------
# step 00: source all required classes and files
source("regrissorClass.r")
#-------------------------------------------------------------------------------
# Step 1: load Data 
# Data from first and second datasets both merged together _Origrenal merged responce data
data_BOr <- na.omit(read.csv("BothdataSets/Source_DataSets_/BothDataSetsfull-Cleaned-Cleaned.csv", header = TRUE))   #Load the raw data
View(data_BOr)
# # Data from first  and second datasets both merged together _Confermatory Factor analysis score data
data_BCFA <- read.csv("BothdataSets/Exploratory Factor Analysis/group_BOTH_cfa_factors_input_data.csv", header = TRUE)
View(data_BCFA)
                                 #-------------------------------------------------------------------------------
# Data from  second datasets _Original responce data
data_orig_02 <- read_excel("SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx")#, sheet = "Form Responses 1") 
View(data_orig_02)
#-------------------------------------------------------------------------------
# Data from Second datasets _Confermatory Factor analysis score data
data_SCFs <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)
View(data_SCFs)

# ==============================================================================
# EXECUTION (HOW TO USE THE OOP STRUCTURE)
# EXECUTION (1st _ both datasets ... Oregenal responses)
# ==============================================================================
# First letus create 4 different objects from the same classes each with a differnt dataset from the above. 
# first Object for the (both data set)
analy_BOr <- regrissorClass$new(data_BOr)
# List all methods
all_methods <- analy_BOr$list_all_methods()
print(all_methods)


analy_BOr$calculate_summary()$print_summary()  #print summary of the dataframe
analy_BOr$get_numeric_info()$column_names    # print column names of numarical column
analy_BOr$get_categorical_info()$column_names # print column names of categorical column
analy_BOr$plot_histogram("TC.sensitivity")
print(analy_BOr$get_categorical_info()$frequency_tables$Gender)
lapply(analy_BOr$get_categorical_info()$frequency_tables, print)
# List models
analy_BOr$list_regression_models()
# Fit simple linear regression (e.g., productivity ~ performance_decrease_under_thermal_and_Air)
lin_model <- analy_BOr$fit_linear_regression("TC.sensitivity", "Performance.under.TC")
print(lin_model$r_squared)  # R-squared value
print(lin_model$p_values)   # P-values for coefficients
# Fit multiple linear regression (e.g., productivity ~ multiple predictors)
multi_lin_model <- analy_BOr$fit_multiple_linear_regression("TC.sensitivity", c("Performance.under.TC", "TC.sensitivity"))
print(multi_lin_model$sse)  # Sum of Squared Errors
# Fit a linear model
lin_model <- analy_BOr$fit_linear_regression("TC.sensitivity", "Performance.under.TC")
# Plot it
analy_BOr$plot_regression_model(lin_model, type = "linear")

# Fit logistic (after adding binary column)
median_val <- median(analy_BOr$data$productivity, na.rm = TRUE)
values <- ifelse(analy_BOr$data$productivity > median_val, 1, 0)
analy_BOr$add_column("binary_outcome", values)
log_model <- analy_BOr$fit_logistic_regression("binary_outcome", "performance_decrease_under_thermal_and_Air")
analy_BOr$plot_regression_model(log_model, type = "logistic")

# Fit multiple linear and plot
multi_model <- analy_BOr$fit_multiple_linear_regression("productivity", c("performance_decrease_under_thermal_and_Air", "Sensitivity"))
analy_BOr$plot_regression_model(multi_model, type = "multiple_linear")
#-======-=========-=======-================
# Independent t-test: Compare productivity by Gender
t_result <- analy_BOr$perform_t_test("productivity", "Gender", type = "independent", plot = TRUE)
print(t_result$p_value)  # Check significance

# One-sample t-test: Is mean productivity different from 5.0?
t_one <- analy_BOr$perform_t_test("productivity", type = "one_sample", mu = 5.0, plot = TRUE)

# Paired t-test: If you have paired data (e.g., add a 'before' and 'after' column first)
analy_BOr$add_column("after_productivity", ...)  # Simulate paired data
t_paired <- analy_BOr$perform_t_test("productivity", "condition", type = "paired", plot = TRUE)
#-======-=========-=======-================
# Calculate correlations for multiple numeric variables
cor_result <- analy_BOr$calculate_pearson_correlation(
  c("productivity", "performance_decrease_under_thermal_and_Air", "Sensitivity"), 
  plot = TRUE
)
print(cor_result$correlation_matrix)  # View r-values
print(cor_result$p_value_matrix)      # View p-values

# Example output interpretation:
# If productivity and performance_decrease have r = -0.7, p = 0.001, 
# there's a strong negative linear relationship (higher performance decrease -> lower productivity).
# 1. Kruskal-Wallis H test (e.g., productivity across professions, assuming >2 professions)
kw_result <- analy_BOr$perform_kruskal_wallis("productivity", "profession", plot = TRUE)
print(kw_result$p_value)  # p < 0.05 indicates significant differences

# 2. Mann-Whitney U test (e.g., productivity between Male and Female)
mw_result <- analy_BOr$perform_mann_whitney_ks("productivity", "Gender", type = "mann_whitney", plot = TRUE)
print(mw_result$p_value)

# K-S test (e.g., distribution of performance_decrease between two groups)
ks_result <- analy_BOr$perform_mann_whitney_ks("performance_decrease_under_thermal_and_Air", "Gender", type = "ks", plot = TRUE)
print(ks_result$p_value)

# 3. Levene's test (e.g., variance homogeneity of productivity by profession)
lev_result <- analy_BOr$perform_levene_anderson("productivity", "profession", type = "levene", plot = TRUE)
print(lev_result$p_value)  # p < 0.05 indicates unequal variances

# Anderson-Darling test (e.g., normality of productivity by Gender)
ad_result <- analy_BOr$perform_levene_anderson("productivity", "Gender", type = "anderson_darling", plot = TRUE)
print(ad_result$p_value)

# 1. Response Surface Analysis (e.g., productivity as response, thermal and air as predictors)
rsa_result <- analy_BOr$perform_response_surface_analysis("productivity", c("performance_decrease_under_thermal_and_Air", "Sensitivity"), plot = TRUE)
print(rsa_result$stationary_point)  # Optimal point

# 2. Cluster Analysis (e.g., cluster on IEQ factors, analyze by Gender)
ca_result <- analy_BOr$perform_cluster_analysis(c("productivity", "performance_decrease_under_thermal_and_Air"), "Gender", k = 3, plot = TRUE)
print(ca_result$clusters$centers)  # Cluster centroids

# 3. Thematic Analysis (e.g., on categorical columns like comments or profession)
ta_result <- analy_BOr$perform_thematic_analysis(c("profession", "Country"), plot = TRUE)
print(head(ta_result$themes))  # Top themes














# ==============================================================================
# EXECUTION (2nd _ both datasets ... CFA Data)
# ==============================================================================
# # For logistic, first ensure a binary outcome (e.g., create one or use existing)
### analy_BOr$add_column("binary_outcome", ifelse(analy_BOr$data$productivity > median(analy_BOr$data$productivity, na.rm = TRUE), 1, 0))
### log_model <- analy_BOr$fit_logistic_regression("binary_outcome", c("Gender", "Age"))

# Second Object for the (both data EDFA)
analy_BCFA <- regrissorClass$new(data_BCFA)
analy_BCFA$calculate_summary()$print_summary()
analy_BCFA$get_numeric_info()$column_names
analy_BCFA$get_categorical_info()$column_names

#---------------------------------------------Plot Histogram-----------------------------
analy_BCFA$plot_histogram("Sensitivity")
analy_BCFA$plot_histogram("Performance")
analy_BCFA$plot_histogram("Th_AQ_Effect")
analy_BCFA$plot_histogram("Li_So_Effect")
#------------------------  Instead of histogram print plot frequency table for categorical varaibles-------------
print(analy_BCFA$get_categorical_info()$frequency_tables$Gender)
lapply(analy_BCFA$get_categorical_info()$frequency_tables, print)
# ---------------------------------------Linear Regression Model-------------------------
# List models
analy_BCFA$list_regression_models()

# It found from scatter plot that the best correlation is found between  "Th_AQ_Effect", "Li_So_Effect
#1-------------------------------------------------------
lin_model <- analy_BCFA$fit_linear_regression("Th_AQ_Effect", "Li_So_Effect")
print(lin_model$r_squared)  # R-squared value
print(lin_model$p_values)   # P-values for coefficients
# Plot it
analy_BCFA$plot_regression_model(lin_model, type = "linear")
# Fit multiple linear regression (e.g., productivity ~ multiple predictors)
multi_lin_model <- analy_BCFA$fit_multiple_linear_regression("Sensitivity", c("Th_AQ_Effect", "Li_So_Effect"))
print(multi_lin_model$sse)  # Sum of Squared Errors
analy_BCFA$plot_regression_model(multi_lin_model , type = "multiple_linear")
#2-------------------------------------------------------
# Fit a linear model 2
lin_model_2 <- analy_BCFA$fit_linear_regression("Th_AQ_Effect", "Sensitivity")
analy_BCFA$plot_regression_model(lin_model_2, type = "linear")
print(lin_model_2$r_squared)  # R-squared value
print(lin_model_2$p_values)   # P-values for coefficients

#3-------------------------------------------------------
# Fit a linear model 3
lin_model_3 <- analy_BCFA$fit_linear_regression("Th_AQ_Effect", "Performance")
analy_BCFA$plot_regression_model(lin_model_3, type = "linear")
print(lin_model_3$r_squared)  # R-squared value
print(lin_model_3$p_values)   # P-values for coefficients

#4-------------------------------------------------------
# Fit a linear model 4
lin_model_4 <- analy_BCFA$fit_linear_regression("Li_So_Effect", "Sensitivity")
analy_BCFA$plot_regression_model(lin_model_4, type = "linear")
print(lin_model_4$r_squared)  # R-squared value
print(lin_model_4$p_values)   # P-values for coefficients

multi_lin_model_2 <- analy_BCFA$fit_multiple_linear_regression("Performance", c("Th_AQ_Effect", "Li_So_Effect"))
print(multi_lin_model_2$sse)  # Sum of Squared Errors
analy_BCFA$plot_regression_model(multi_lin_model_2 , type = "multiple_linear")
#5-------------------------------------------------------
# Fit a linear model 5
lin_model_5 <- analy_BCFA$fit_linear_regression("Li_So_Effect", "Performance")
analy_BCFA$plot_regression_model(lin_model_5, type = "linear")
print(lin_model_5$r_squared)  # R-squared value
print(lin_model_5$p_values)   # P-values for coefficients

multi_lin_model_3 <- analy_BCFA$fit_multiple_linear_regression("Li_So_Effect", c("Sensitivity", "Performance"))
print(multi_lin_model_3$sse)  # Sum of Squared Errors
analy_BCFA$plot_regression_model(multi_lin_model_3 , type = "multiple_linear")
#6-------------------------------------------------------
# Fit a linear model 6
lin_model_6 <- analy_BCFA$fit_linear_regression("Performance", "Sensitivity")
analy_BCFA$plot_regression_model(lin_model_6, type = "linear")
print(lin_model_6$r_squared)  # R-squared value
print(lin_model_6$p_values)   # P-values for coefficients

multi_lin_model_4 <- analy_BCFA$fit_multiple_linear_regression("Th_AQ_Effect", c("Sensitivity", "Performance"))
print(multi_lin_model_4$sse)  # Sum of Squared Errors
analy_BCFA$plot_regression_model(multi_lin_model_4 , type = "multiple_linear")
#-------------------------------------------------------
#--------------------------------------------Perform T-test---------------------------------
# Independent t-test: Compare productivity by Gender
t_result_BCFA <- analy_BCFA$perform_t_test("Sensitivity", "Gender", type = "independent", plot = TRUE)
print(t_result_BCFA$p_value)  # Check significance
cat("If p < 0.05, reject null: means differ significantly.\n")
#Only a 0.23% chance this difference occurred by random chance,
#We can reject the null hypothesis that both genders have equal mean Sensitivity

# One-sample t-test: Is mean Sensitivity different from 5.0?
t_one_BCFA <- analy_BCFA$perform_t_test("Sensitivity", type = "one_sample", mu = 5.0, plot = TRUE)
#Since the data distribution is far left from the hypothesized mean (mu), the null hypothesis that the true mean Sensitivity equals 5 is very unlikely.
#This is a visual cue supporting rejection of the null hypothesis.
#Because the observed Sensitivity values are far less than 5 (confirmed by histogram and likely by computed mean), the:
#  p-value will be very small (< 0.05), meaning you reject the null hypothesis.
#The confidence interval will NOT include 5.
#The sample mean will be much less than 5.


# Paired t-test: If you have paired data (e.g., add a 'before' and 'after' column first)
# Number of rows in your data
n <- nrow(analy_BCFA$data)
set.seed(42)
analy_BCFA_after <- analy_BCFA
analy_BCFA_after$add_column("after_Sensitivity", analy_BCFA_after$data$Sensitivity + rnorm(n, mean = 0.2, sd = 0.1))


library(tidyr)
df <- analy_BCFA_after$data

# Assuming 'after_Sensitivity' correctly added to df already
# Convert to long format
df_long <- tidyr::pivot_longer(df,
                               cols = c("Sensitivity", "after_Sensitivity"),
                               names_to = "time",
                               values_to = "Sensitivity_value")
after_sensitivity_values <- analy_BCFA_after$data$Sensitivity + rnorm(n, mean = 0.2, sd = 0.1)
# Map 'time' values to more readable factor levels
df_long$time <- factor(df_long$time, levels = c("Sensitivity", "after_Sensitivity"),
                       labels = c("before", "after"))

# Create a new regrissorClass object with the long data
analy_BCFA_long <- regrissorClass$new(df_long)

t_paired_result <- analy_BCFA_long$perform_t_test(outcome = "Sensitivity_value",
                                                  group_column = "time",
                                                  type = "paired",
                                                  plot = TRUE)
print(t_paired_result$p_value)


#Since the data column have been collected from tow different interval befor 2019 and after 2024, #Where 2019 (span from raw 1:to:360) and 2024 (span from 361:to 572), it significant to compare both data together and to perform paired t-test to them. Thus the following paired t-test try to figure out if the sensitivity differes among genders. Here, the null hypothesis (H0 = there is no difference between groups)

# Step 1: Extract before 2019 Sensitivity and after 2024 Sensitivity
df_before <- analy_BCFA$data [1:360, ]
df_after  <- analy_BCFA$data [361:572, ]
# Step 2: Extract corresponding genders for both intervals
# Check gender counts before aligning
table(df_before$Gender)
table(df_after$Gender)

# Split both datasets by gender
before_male   <- df_before[df_before$Gender == "Male", ]
before_female <- df_before[df_before$Gender == "Female", ]

after_male    <- df_after[df_after$Gender == "Male", ]
after_female  <- df_after[df_after$Gender == "Female", ]

# Step 3: Check lengths are equal to proceed with pairing
if (dim(df_before)[1]<=dim(df_after)[1]){
  i = dim(df_before)[1]
  j = dim(before_male)[1]
  k = dim(before_female)[1]
} else {
  i = dim(df_after)[1]
  j = dim(after_male)[1]
  k = dim(after_female)[1]
}
print("length of our sampeld data is ")
cat(i, j, k)

# Now order both dataset in mean of males and females and compine them sample of the bigger oneor both: 
# Sample j and K random row indices without replacement
random_indices_male_after <- sample(nrow(after_male), size = j, replace = FALSE)
random_indices_female_after <- sample(nrow(after_female), size = k, replace = FALSE)
random_indices_male_before <- sample(nrow(before_male), size = j, replace = FALSE)
random_indices_female_before <- sample(nrow(before_female), size = k, replace = FALSE)
# Create the new data frame using those indices
before_ordered <- rbind(before_male[random_indices_male_before, ], ... = before_female[random_indices_female_before, ])
after_ordered <- rbind(after_male[random_indices_male_after, ], ... = after_female[random_indices_female_after, ])

# Verify same number of rows after ordering
if(nrow(before_ordered) != nrow(after_ordered)) {
  stop("Number of samples for before and after do not match after ordering by gender")
}

# Step 4: Create data frame combining before and after with genders for paired analysis
paired_df <- data.frame(
  id = 1:before_ordered$Gender,
  Gender = before_ordered,  # aligned gender
  Sensitivity_before = before_ordered$Sensitivity,
  Sensitivity_after = after_ordered$Sensitivity
)
# Check for any mismatches or NA values
summary(paired_df)


# Proceed with reshaping for paired analysis
paired_long <- tidyr::pivot_longer(paired_df,
                                   cols = c("Sensitivity_before", "Sensitivity_after"),
                                   names_to = "time",
                                   values_to = "Sensitivity_value")

paired_long$time <- factor(paired_long$time, levels = c("Sensitivity_before", "Sensitivity_after"),
                           labels = c("before", "after"))

paired_long$Gender <- factor(paired_long$Gender)  # Ensure Gender is factor

# Create regrissorClass object for paired data analysis
before_after_paired <- regrissorClass$new(paired_long)

t_paired_result <- before_after_paired$perform_t_test(outcome = "Sensitivity_value",
                                                  group_column = "time",
                                                  type = "paired",
                                                  plot = TRUE)
print(t_paired_result$p_value)
## Sensitivity, Performance, Th_AQ_Effect, Li_So_Effect
## Location.of.Leaving, Occupation, age, Length, Weight, Gender 









# ==============================================================================
# EXECUTION (3rd _ Second Dataset ... Original response)
# ==============================================================================
# Third Object for the (Second Dataset)
analy_SODS  <- regrissorClass$new(data_orig_02)
analy_SODS$calculate_summary()$print_summary()


# ==============================================================================
# EXECUTION (4th _ Second Dataset ... CFA Data)
# ==============================================================================
## Fourth Object for the (Second Dataset _SCFA)
analy_SCFs <- regrissorClass$new(data_SCFs)
analy_SCFs$calculate_summary()$print_summary()
analy_SCFs$get_numeric_info()$column_names
analy_SCFs$get_categorical_info()$column_names
#---------------------------------------------Plot Histogram-----------------------------
analy_SCFs$plot_histogram("Sensitivity")
analy_SCFs$plot_histogram("productivity")
analy_SCFs$plot_histogram("performance_decrease_under_thermal_and_Air")
analy_SCFs$plot_histogram("Work_effectively")
analy_SCFs$plot_histogram("performance_decrease_under_noise_and_light")
analy_SCFs$plot_histogram("Combined_Effect_of_Air_thermal_noise_light")
#------------------------  Instead of histogram print plot frequency table for categorical varaibles-------------
print(analy_SCFs$get_categorical_info()$frequency_tables$Gender)
lapply(analy_SCFs$get_categorical_info()$frequency_tables, print)
# ---------------------------------------Linear Regression Model-------------------------
# List models
analy_SCFs$list_regression_models()

analy_SCFs$plot_scatter_by_group("productivity", "performance_decrease_under_thermal_and_Air", "Gender")
#-------------------------------------------------------------------------------
# After initialization, let us find out and print out the object summary 


