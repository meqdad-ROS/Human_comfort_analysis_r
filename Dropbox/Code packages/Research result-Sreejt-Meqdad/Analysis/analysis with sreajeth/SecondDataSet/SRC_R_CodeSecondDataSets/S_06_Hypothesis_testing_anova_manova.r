#Hypotheses testing and Anova test for different groupd
# Written by Meqdad Hasan 
#clear plotting device, concule and all other variabels 

cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables



# step 0: Load necessary libraries
library(ggplot2)
library(reshape2)
source("SecondDataSet/SRC_R_CodeSecondDataSets/S_anova.r")
# Step 1: load Data 
myData  <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)
Data = myData
Names <- colnames(myData)
# print(Names)
# print(dim(Data))
# print(length(Names))
## Here we should test one way anova on each categorical variable one by one each with one factor. 

pdf("Anova_test_1_&_2_way output_results.pdf")
# sink("Anova_test_1_&_2_way output_results.pdf")

cat("OUtput result of ONEWAY ANOVA test over variables and factors.\n")


l = 1
for (i in c(1:17)){  #### Iterate over each categorical variable
  for (j in c(18:23)){   #### Iterate over each numerical variable
    result.of.test<- One_way_Anova(myData[,i],myData[,j])
    printable.text <- paste("[", l , "] The result of one way test in groub of ", Names[i]," regarding their ", Names[j], " is",result.of.test, "\n")
    cat(printable.text)
    l = l+1
  }
}

## Here we should test TWo way anova on each categorical variable grouped with any other with one numarical factor. 
cat("OUtput result of ONEWAY ANOVA test over variables and factors.\n")
l = 1
for (i in c(1:16)){  #### Iterate over each categorical variable
  for (n in c((i+1):17)){
    for (j in c(18:23)){   #### Iterate over each numerical variable
      print(n)
      print(i)
      print(j)
      result.of.test<- two_way_anova(Data[,i],Data[,n],Data[,j])
      printable.text <- paste("[", l , "] The result of Two way anova test of variable ", Names[i]," in groub of ", Names[n]," regarding their ", Names[j], " is",result.of.test, "\n")
      cat(printable.text)
      l = l+1
    }
  }
}

# Close the sink
# sink()

# Close the PDF device
dev.off()

new.df <- data.frame(
  group = myData$Gender,
  value = myData$Sensitivity
)

# Create a boxplot using base R
boxplot(value ~ group, data = new.df,
        main = "Boxplot of Values by Group",
        xlab = "Group",
        ylab = "Value",
        col = c("lightblue", "lightgreen", "lightpink"),
        border = "darkblue")
#########################################################################################
##perform manova on some varaibles 
library(car)
library(multcomp)

cat("\014")  #clear consol from code 
#while (dev.cur() > 1) dev.off()  # clear all plot device 
#rm(list = ls()) # clear all environment variables

# Names <- colnames(Data)
# new.df <- Data.frame(
#   group = Data$Location.of.Leaving,
#   value1 = Data$Sensitivity ,
#   value2 = Data$Performance, 
#   value3 = Data$Th_AQ_Effect,
#   value4 = Data$Li_So_Effect
# )
myData = Data
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

# Get compact letter display (groupings)
cld(posthoc_glht)
print("Post-hoc for Productivity:")
print(posthoc_productivity)

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




##-=============================================(1)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_thermal_and_Air ~ Data$Language, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Language`[ tukey_results$`Data$Language`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(2)======================================###
##perform anova test then post hoc test
anova_model02 <- lm(Data$Work_effectively ~ Data$Age, data = Data)
anova_results <- Anova(anova_model02)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model02))
print(tukey_results)
par(mar = c(8, 5, 4, 2) + 3)  # Increased bottom margin for group names
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Age`[ tukey_results$`Age`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


##-=============================================(3)======================================###
##perform anova test then post hoc test
anova_model03 <- lm(Data$Work_effectively ~ Data$How_long_in_this_role_environment, data = Data)
anova_results <- Anova(anova_model03)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model03))
print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$How_long_in_this_role_environment`[ tukey_results$`Data$How_long_in_this_role_environment`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(4)======================================###
##perform anova test then post hoc test
anova_model03 <- lm(Data$performance_decrease_under_thermal_and_Air ~ Data$Actively_adjust_environment_during_IEQ_fluctuations, data = Data)
anova_results <- Anova(anova_model03)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model03))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Actively_adjust_environment_during_IEQ_fluctuations`[ tukey_results$`Data$Actively_adjust_environment_during_IEQ_fluctuations`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(5)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_noise_and_light ~ Data$Actively_adjust_environment_during_IEQ_fluctuations, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Actively_adjust_environment_during_IEQ_fluctuations`[ tukey_results$`Data$Actively_adjust_environment_during_IEQ_fluctuations`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)



##-=============================================(6)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$productivity ~ Data$More_sensitive_to_IEQ_changes_due_to_health_conditions, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[ tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(7)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_thermal_and_Air ~ Data$More_sensitive_to_IEQ_changes_due_to_health_conditions, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[ tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(8)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$Sensitivity ~ Data$More_sensitive_to_IEQ_changes_due_to_health_conditions, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[ tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(9)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_noise_and_light ~ Data$More_sensitive_to_IEQ_changes_due_to_health_conditions, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[ tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions
`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(10)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$Combined_Effect_of_Air_thermal_noise_light ~ Data$More_sensitive_to_IEQ_changes_due_to_health_conditions, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[ tukey_results$`Data$More_sensitive_to_IEQ_changes_due_to_health_conditions`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


##-=============================================(11)======================================###
##-=============================================(11)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$productivity ~ Data$How_quickly_notice_IEQ_parameter_change, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(12)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_thermal_and_Air ~ Data$How_quickly_notice_IEQ_parameter_change, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(6, 12, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(13)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_noise_and_light ~ Data$How_quickly_notice_IEQ_parameter_change, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(6, 12, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(14)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$Combined_Effect_of_Air_thermal_noise_light ~ Data$How_quickly_notice_IEQ_parameter_change, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(6, 15, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


##-=============================================()======================================###
##-=============================================(15)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$productivity ~ Data$Typical_response_to_IEQ_discomfort, data = Data)
anova_results <- Anova(anova_model)
# Increase margins if needed:
par(mar = c(5, 18, 5, 7))  # Larger margins for more space
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Typical_response_to_IEQ_discomfort`[ tukey_results$`Data$Typical_response_to_IEQ_discomfort`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

# ##-=============================================(15.1)======================================###
# ##perform anova test then post hoc test
# anova_model <- lm(Data$performance_decrease_under_thermal_and_Air ~ Data$Typical_response_to_IEQ_discomfort, data = Data)
# anova_results <- Anova(anova_model)
# # Increase margins if needed:
# par(mar = c(5, 18, 5, 7))  # Larger margins for more space
# #print(anova_results)
# tukey_results <- TukeyHSD(aov(anova_model))
# #print(tukey_results)
# plot(tukey_results, las = 1)
# # Filter rows where p adj < 0.05
# significant_results <- tukey_results$`Data$Typical_response_to_IEQ_discomfort`[ tukey_results$`Data$Typical_response_to_IEQ_discomfort`[, 'p adj'] < 0.05,]
# # Print the significant results
# print(significant_results)
# 
# ##-=============================================(15.2)======================================###
# ##perform anova test then post hoc test
# anova_model <- lm(Data$performance_decrease_under_noise_and_light ~ Data$Typical_response_to_IEQ_discomfort, data = Data)
# anova_results <- Anova(anova_model)
# # Increase margins if needed:
# par(mar = c(5, 18, 5, 7))  # Larger margins for more space
# #print(anova_results)
# tukey_results <- TukeyHSD(aov(anova_model))
# #print(tukey_results)
# plot(tukey_results, las = 1)
# # Filter rows where p adj < 0.05
# significant_results <- tukey_results$`Data$Typical_response_to_IEQ_discomfort`[ tukey_results$`Data$Typical_response_to_IEQ_discomfort`[, 'p adj'] < 0.05,]
# # Print the significant results
# print(significant_results)

# ##-=============================================(15.3)======================================###
# ##perform anova test then post hoc test
# anova_model <- lm(Data$Combined_Effect_of_Air_thermal_noise_light ~ Data$Typical_response_to_IEQ_discomfort, data = Data)
# anova_results <- Anova(anova_model)
# # Increase margins if needed:
# par(mar = c(5, 18, 5, 7))  # Larger margins for more space
# #print(anova_results)
# tukey_results <- TukeyHSD(aov(anova_model))
# #print(tukey_results)
# plot(tukey_results, las = 1)
# # Filter rows where p adj < 0.05
# significant_results <- tukey_results$`Data$Typical_response_to_IEQ_discomfort`[ tukey_results$`Data$Typical_response_to_IEQ_discomfort`[, 'p adj'] < 0.05,]
# # Print the significant results
# print(significant_results)

##-=============================================(16)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$productivity ~ Data$Ever_requested_workplace_modifications, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Ever_requested_workplace_modifications`[ tukey_results$`Data$Ever_requested_workplace_modifications`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(17)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_thermal_and_Air ~ Data$Ever_requested_workplace_modifications, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(5, 18, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Ever_requested_workplace_modifications`[ tukey_results$`Data$Ever_requested_workplace_modifications`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(18)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_noise_and_light ~ Data$Ever_requested_workplace_modifications, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(5, 18, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Ever_requested_workplace_modifications`[ tukey_results$`Data$Ever_requested_workplace_modifications`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(19)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$Combined_Effect_of_Air_thermal_noise_light ~ Data$Ever_requested_workplace_modifications, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(5, 18, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Ever_requested_workplace_modifications`[ tukey_results$`Data$Ever_requested_workplace_modifications`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


##-=============================================()======================================###
##-=============================================(20)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$Work_effectively ~ Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(5, 18, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(21)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$performance_decrease_under_noise_and_light ~ Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(5, 18, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(22)======================================###
##perform anova test then post hoc test
anova_model <- lm(Data$Combined_Effect_of_Air_thermal_noise_light ~ Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air, data = Data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
# Increase margins if needed:
par(mar = c(5, 18, 5, 6))  # Larger margins for more space
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`Data$Headache_sleepy_at_work_enough_sleep_due_to_light_air`[ tukey_results$`Data$How_quickly_notice_IEQ_parameter_change`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)