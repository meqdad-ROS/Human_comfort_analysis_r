#Hypotheses testing and Anova test for different groupd
# Written by Meqdad Hasan 
#clear plotting device, concule and all other variabels 

cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables



# step 0: Load necessary libraries
library(ggplot2)
library(reshape2)
##perform manova on some varaibles 
library(car)
library(multcomp)
source("SecondDataSet/SRC_R_CodeSecondDataSets/S_anova.r")
# Step 1: load Data 
myData  <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)
Data = myData
Names <- colnames(myData)


#Language	Gender	height	Age	Weight	How_long_in_this_role_environment	Typical_daily_work_schedule	Does_your_work_involve_high_cognitive_load	Actively_adjust_environment_during_IEQ_fluctuations	More_sensitive_to_IEQ_changes_due_to_health_conditions	How_quickly_notice_IEQ_parameter_change	Typical_response_to_IEQ_discomfort	Ever_requested_workplace_modifications	Headache_sleepy_at_work_enough_sleep_due_to_light_air	IEQ_training_receivedWilling_to_join_IEQ_programs	Country	profession	productivity	Work_effectively	performance_decrease_under_thermal_and_Air	Sensitivity	performance_decrease_under_noise_and_light	Combined_Effect_of_Air_thermal_noise_light
#performance_decrease_under_thermal_and_Air	Sensitivity	performance_decrease_under_noise_and_light	Combined_Effect_of_Air_thermal_noise_light
modelCat <- cbind(myData$productivity, 
                 myData$Work_effectively, 
                 myData$performance_decrease_under_thermal_and_Air, 
                 myData$Sensitivity, 
                 myData$performance_decrease_under_noise_and_light, 
                 myData$Combined_Effect_of_Air_thermal_noise_light
                 )

# Perform MANOVA (1)
manova_results1 <- manova( modelCat ~ Data$Language, data = Data)
# View the summary of the MANOVA results
summary(manova_results1)
# Perform ANOVA for each dependent variable
summary.aov(manova_results1)
# 2. Post hoc tests for each significant dependent variable
# For example, if Response 1 was significant:
aov_productivity <- aov(modelCat ~ Data$Language, data = myData)

groups <- unique(myData$Language)
numbOfGroups <- length(groups)
n_comparisons <- choose(length(groups), numbOfGroups*(numbOfGroups-1)/2)
alpha_adjusted <- 0.05 / n_comparisons  # Bonferroni correction


cat("Number of pairwise comparisons:", n_comparisons, "\n")
cat("Adjusted alpha level:", alpha_adjusted, "\n\n")

# Store results
pairwise_results <- data.frame(
  Comparison = character(),
  Pillai = numeric(),
  F_stat = numeric(),
  p_value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)




##========================================================================================
# Subset data for two groups
subset_data <- myData[myData$Language %in% c(groups[i], groups[j]), ]

# Create DV matrix
dv_matrix <- cbind(subset_data$productivity, 
                   subset_data$Work_effectively, 
                   subset_data$performance_decrease_under_thermal_and_Air, 
                   subset_data$Sensitivity, 
                   subset_data$performance_decrease_under_noise_and_light, 
                   subset_data$Combined_Effect_of_Air_thermal_noise_light)

# Run MANOVA
pairwise_manova <- manova(dv_matrix ~ subset_data$Language)
summary_result <- summary(pairwise_manova, test = "Pillai")

# Extract statistics
pillai <- summary_result$stats[1, "Pillai"]
f_stat <- summary_result$stats[1, "approx F"]
p_val <- summary_result$stats[1, "Pr(>F)"]

# Store results
pairwise_results <- rbind(pairwise_results, 
                          data.frame(
                            Comparison = paste(groups[i], "vs", groups[j]),
                            Pillai = pillai,
                            F_stat = f_stat,
                            p_value = p_val,
                            Significant = ifelse(p_val < alpha_adjusted, "Yes*", "No")
                          ))

cat("========================================\n")
cat("Comparison:", groups[i], "vs", groups[j], "\n")
cat("Pillai's Trace:", round(pillai, 4), "\n")
cat("F-statistic:", round(f_stat, 4), "\n")
cat("p-value:", format(p_val, scientific = TRUE), "\n")
cat("Significant (α =", alpha_adjusted, "):", 
    ifelse(p_val < alpha_adjusted, "YES ***", "No"), "\n")
cat("========================================\n\n")




##========================================================================================

# Perform MANOVA (2)
manova_results2 <- manova( modelCat ~ Data$Age  , data = Data)
# View the summary of the MANOVA results
summary(manova_results2)
# Perform ANOVA for each dependent variable
summary.aov(manova_results2)

# Perform MANOVA (3)
manova_results3 <- manova( modelCat ~ Data$Gender  , data = Data)
# View the summary of the MANOVA results
summary(manova_results3)
# Perform ANOVA for each dependent variable
summary.aov(manova_results3)

# Perform MANOVA (4)
manova_results4 <- manova( modelCat ~ Data$height  , data = Data)
# View the summary of the MANOVA results
summary(manova_results4)
# Perform ANOVA for each dependent variable
summary.aov(manova_results4)

# Perform MANOVA (5)
manova_results5 <- manova(modelCat ~ Data$Weight  , data = Data)
# View the summary of the MANOVA results
summary(manova_results5)
# Perform ANOVA for each dependent variable
summary.aov(manova_results5)


# Perform MANOVA (6)
manova_results6 <- manova(modelCat ~ Data$How_long_in_this_role_environment  , data = Data)
# View the summary of the MANOVA results
summary(manova_results6)
# Perform ANOVA for each dependent variable
summary.aov(manova_results6)

# Perform MANOVA (7)
manova_results7 <- manova(modelCat ~ Data$Typical_daily_work_schedule  , data = Data)
# View the summary of the MANOVA results
summary(manova_results7)
# Perform ANOVA for each dependent variable
summary.aov(manova_results7)

# Perform MANOVA (8)
manova_results8 <- manova(modelCat ~ Data$Does_your_work_involve_high_cognitive_load  , data = Data)
# View the summary of the MANOVA results
summary(manova_results8)
# Perform ANOVA for each dependent variable
summary.aov(manova_results8)

# Perform MANOVA (9)
manova_results9 <- manova(modelCat ~ Data$Actively_adjust_environment_during_IEQ_fluctuations  , data = Data)
# View the summary of the MANOVA results
summary(manova_results9)
# Perform ANOVA for each dependent variable
summary.aov(manova_results9)

# Perform MANOVA (10)
manova_results10 <- manova(modelCat ~ Data$More_sensitive_to_IEQ_changes_due_to_health_conditions , data = Data)
# View the summary of the MANOVA results
summary(manova_results10)
# Perform ANOVA for each dependent variable
summary.aov(manova_results10)

# Perform MANOVA (11)
manova_results11 <- manova(modelCat ~ Data$How_quickly_notice_IEQ_parameter_change, data = Data)
# View the summary of the MANOVA results
summary(manova_results11)
# Perform ANOVA for each dependent variable
summary.aov(manova_results11)

# Perform MANOVA (12)
manova_results12 <- manova(modelCat ~ Data$Typical_response_to_IEQ_discomfort, data = Data)
# View the summary of the MANOVA results
summary(manova_results12)
# Perform ANOVA for each dependent variable
summary.aov(manova_results12)

# Perform MANOVA (13)
manova_results13 <- manova(modelCat ~ Data$Ever_requested_workplace_modifications, data = Data)
# View the summary of the MANOVA results
summary(manova_results13)
# Perform ANOVA for each dependent variable
summary.aov(manova_results13)

# Perform MANOVA (14)
manova_results14 <- manova(modelCat ~ Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air  , data = Data)
# View the summary of the MANOVA results
summary(manova_results14)
# Perform ANOVA for each dependent variable
summary.aov(manova_results14)

# Perform MANOVA (15)
manova_results15 <- manova(modelCat ~ Data$IEQ_training_receivedWilling_to_join_IEQ_programs  , data = Data)
# View the summary of the MANOVA results
summary(manova_results15)
# Perform ANOVA for each dependent variable
summary.aov(manova_results15)

# Perform MANOVA (16)
manova_results16 <- manova(modelCat ~ Data$Country  , data = Data)
# View the summary of the MANOVA results
summary(manova_results16)
# Perform ANOVA for each dependent variable
summary.aov(manova_results16)

# Perform MANOVA (17)
manova_results17 <- manova(modelCat ~ Data$profession  , data = Data)
# View the summary of the MANOVA results
summary(manova_results17)
# Perform ANOVA for each dependent variable
summary.aov(manova_results17)


