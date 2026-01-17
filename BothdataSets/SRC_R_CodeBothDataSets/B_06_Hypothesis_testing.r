#Hypotheses testing and Anova test for different groupd
# Written by Meqdad Hasan 
#clear plotting device, concule and all other variabels 

cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables



# step 0: Load necessary libraries
library(ggplot2)
library(reshape2)
source("BothdataSets/anova.r")
# Step 1: load data 
data  <- read.csv("BothdataSets/Exploratory Factor Analysis/group_cfa_factors_input_data.csv", header = TRUE)
Names <- colnames(data)
# print(Names)
# print(dim(data))
# print(length(Names))
## Here we should test one way anova on each categorical variable one by one each with one factor. 

pdf("Anova_test_1_&_2_way output_results.pdf")
# sink("Anova_test_1_&_2_way output_results.pdf")

cat("OUtput result of ONEWAY ANOVA test over variables and factors.\n")


l = 1
for (i in c(1:6)){  #### Iterate over each categorical variable
  for (j in c(7:10)){   #### Iterate over each numerical variable
    result.of.test<- One_way_Anova(data[,i],data[,j])
    printable.text <- paste("[", l , "] The result of one way test in groub of ", Names[i]," regarding their ", Names[j], " is",result.of.test, "\n")
    cat(printable.text)
    l = l+1
  }
}

## Here we should test TWo way anova on each categorical variable grouped with any other with one numarical factor. 
cat("OUtput result of ONEWAY ANOVA test over variables and factors.\n")
l = 1
for (i in c(1:5)){  #### Iterate over each categorical variable
  for (n in c((i+1):6)){
    for (j in c(7:10)){   #### Iterate over each numerical variable
      print(n)
      print(i)
      print(j)
      result.of.test<- two_way_anova(data[,i],data[,n],data[,j])
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
  group = data$Gender,
  value = data$Sensitivity
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

# Names <- colnames(data)
# new.df <- data.frame(
#   group = data$Location.of.Leaving,
#   value1 = data$Sensitivity ,
#   value2 = data$Performance, 
#   value3 = data$Th_AQ_Effect,
#   value4 = data$Li_So_Effect
# )

# Perform MANOVA (1)
manova_results <- manova(cbind(data$Sensitivity, data$Performance, data$Th_AQ_Effect, data$Li_So_Effect) ~ data$Location.of.Leaving, data = data)
# View the summary of the MANOVA results
summary(manova_results)
# Perform ANOVA for each dependent variable
summary.aov(manova_results)

# Perform MANOVA (2)
manova_results <- manova(cbind(data$Sensitivity, data$Performance, data$Th_AQ_Effect, data$Li_So_Effect) ~ data$Occupation  , data = data)
# View the summary of the MANOVA results
summary(manova_results)
# Perform ANOVA for each dependent variable
summary.aov(manova_results)

# Perform MANOVA (3)
manova_results <- manova(cbind(data$Sensitivity, data$Performance, data$Th_AQ_Effect, data$Li_So_Effect) ~ data$age  , data = data)
# View the summary of the MANOVA results
summary(manova_results)
# Perform ANOVA for each dependent variable
summary.aov(manova_results)

# Perform MANOVA (4)
manova_results <- manova(cbind(data$Sensitivity, data$Performance, data$Th_AQ_Effect, data$Li_So_Effect) ~ data$Length  , data = data)
# View the summary of the MANOVA results
summary(manova_results)
# Perform ANOVA for each dependent variable
summary.aov(manova_results)

# Perform MANOVA (5)
manova_results <- manova(cbind(data$Sensitivity, data$Performance, data$Th_AQ_Effect, data$Li_So_Effect) ~ data$Weight  , data = data)
# View the summary of the MANOVA results
summary(manova_results)
# Perform ANOVA for each dependent variable
summary.aov(manova_results)


# Perform MANOVA (6)
manova_results <- manova(cbind(data$Sensitivity, data$Performance, data$Th_AQ_Effect, data$Li_So_Effect) ~ data$Gender  , data = data)
# View the summary of the MANOVA results
summary(manova_results)
# Perform ANOVA for each dependent variable
summary.aov(manova_results)

##-=============================================(1)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Performance ~ data$Location.of.Leaving, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Location.of.Leaving`[ tukey_results$`data$Location.of.Leaving`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(2)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Th_AQ_Effect ~ data$Location.of.Leaving, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Location.of.Leaving`[ tukey_results$`data$Location.of.Leaving`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


##-=============================================(3)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Performance ~ data$Occupation, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Occupation`[ tukey_results$`data$Occupation`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(4)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Th_AQ_Effect ~ data$Occupation, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Occupation`[ tukey_results$`data$Occupation`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(5)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Li_So_Effect ~ data$Occupation, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Occupation`[ tukey_results$`data$Occupation`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)



##-=============================================(6)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Th_AQ_Effect ~ data$age, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$age`[ tukey_results$`data$age`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)
##-=============================================(7)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Li_So_Effect ~ data$age, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$age`[ tukey_results$`data$age`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(8)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Performance ~ data$Length, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Length`[ tukey_results$`data$Length`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(9)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Th_AQ_Effect ~ data$Weight, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Weight`[ tukey_results$`data$Weight`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(10)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Sensitivity ~ data$Gender, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Gender`[ tukey_results$`data$Gender`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


##-=============================================(11)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Performance ~ data$Gender, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Gender`[ tukey_results$`data$Gender`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)

##-=============================================(12)======================================###
##perform anova test then post hoc test
anova_model <- lm(data$Th_AQ_Effect ~ data$Gender, data = data)
anova_results <- Anova(anova_model)
#print(anova_results)
tukey_results <- TukeyHSD(aov(anova_model))
#print(tukey_results)
plot(tukey_results, las = 1)
# Filter rows where p adj < 0.05
significant_results <- tukey_results$`data$Gender`[ tukey_results$`data$Gender`[, 'p adj'] < 0.05,]
# Print the significant results
print(significant_results)


###======================== from 2ways anova checking interaction by interaction plot
#1-  Create an interaction plot
interaction.plot(data$Location.of.Leaving , data$Occupation, data$Sensitivity,
                 xlab = "Location.of.Leaving ", 
                 ylab = "Mean Sensitivity", 
                 trace.label = "Occupation",
                 col = c("red", "blue"), 
                 pch = c(1, 2), 
                 lwd = 2)

#2- Create a box plot
library(ggplot2)

ggplot(data, aes(x = interaction(data$Location.of.Leaving, data$Occupation), y = data$Sensitivity)) +
  geom_boxplot() +
  labs(x = "Interaction ofLocation.of.Leaving and Occupation", y = "Sensitivity") +
  theme_minimal()


# Install and load the emmeans package if you haven't already
# install.packages("emmeans")#
# install.packages("estimability")
library(emmeans)
###### here and after find the interaction between two groups and variabels First plot then do test 
######--------------------  [1]
anova_model <- aov(data$Sensitivity ~ data$Location.of.Leaving * data$Occupation, data = data)
# Calculate estimated marginal means
emm1 <- emmeans(anova_model, ~ Location.of.Leaving * Occupation)
# 3. Pairwise comparisons ------------------------------------------
cmp <- pairs(emm1, adjust = "tukey")   # or "bonferroni", "holm", etc.
# 4. Keep only significant comparisons -----------------------------
sig_cmp <- as.data.frame(cmp) |>        # convert to data.frame
  subset(p.value < 0.05)       # keep p < 0.05 (already adjusted)
# 5. Print / use the result ----------------------------------------
print(sig_cmp)

######--------------------  [2]
anova_model <- aov(data$Th_AQ_Effect ~ data$Location.of.Leaving * data$Occupation, data = data)
# Calculate estimated marginal means
emm2 <- emmeans(anova_model, ~ Location.of.Leaving * Occupation)

# 3. Pairwise comparisons ------------------------------------------
cmp <- pairs(emm2, adjust = "tukey")   # or "bonferroni", "holm", etc.
# 4. Keep only significant comparisons -----------------------------
sig_cmp <- as.data.frame(cmp) |>        # convert to data.frame
  subset(p.value < 0.05)       # keep p < 0.05 (already adjusted)
# 5. Print / use the result ----------------------------------------
print(sig_cmp)
###======================== from 2ways anova checking interaction by interaction plot
#1-  Create an interaction plot
interaction.plot(data$Location.of.Leaving , data$Occupation, data$Th_AQ_Effect,
                 xlab = "Location.of.Leaving ", 
                 ylab = "Mean Th_AQ_Effect", 
                 trace.label = "Occupation",
                 col = c("red", "blue"), 
                 pch = c(1, 2), 
                 lwd = 2)
#2- Create a box plot
ggplot(data, aes(x = interaction(data$Location.of.Leaving, data$Occupation), y = data$Th_AQ_Effect)) +
  geom_boxplot() +
  labs(x = "Interaction ofLocation.of.Leaving and Occupation", y = "Th_AQ_Effect") +
  theme_minimal()


######--------------------  [3]
anova_model <- aov(data$Performance ~ data$Location.of.Leaving * data$age, data = data)
# Calculate estimated marginal means
emm3 <- emmeans(anova_model, ~ Location.of.Leaving * age)

# 3. Pairwise comparisons ------------------------------------------
cmp <- pairs(emm3, adjust = "tukey")   # or "bonferroni", "holm", etc.
# 4. Keep only significant comparisons -----------------------------
sig_cmp <- as.data.frame(cmp) |>        # convert to data.frame
  subset(p.value < 0.05)       # keep p < 0.05 (already adjusted)
# 5. Print / use the result ----------------------------------------
print(sig_cmp)
###======================== from 2ways anova checking interaction by interaction plot
#1-  Create an interaction plot
interaction.plot(data$Location.of.Leaving , data$age, data$Performance,
                 xlab = "Location.of.Leaving ", 
                 ylab = "Mean Performance", 
                 trace.label = "age",
                 col = c("red", "blue"), 
                 pch = c(1, 2), 
                 lwd = 2)
#2- Create a box plot
ggplot(data, aes(x = interaction(data$Location.of.Leaving, data$age), y = data$Performance)) +
  geom_boxplot() +
  labs(x = "Interaction ofLocation.of.Leaving and age", y = "Performance") +
  theme_minimal()


######--------------------  [4]
anova_model <- aov(data$Th_AQ_Effect ~ data$Weight * data$Gender, data = data)
# Calculate estimated marginal means
emm4 <- emmeans(anova_model, ~ Weight * Gender)

# 3. Pairwise comparisons ------------------------------------------
cmp <- pairs(emm4, adjust = "tukey")   # or "bonferroni", "holm", etc.
# 4. Keep only significant comparisons -----------------------------
sig_cmp <- as.data.frame(cmp) |>        # convert to data.frame
  subset(p.value < 0.05)       # keep p < 0.05 (already adjusted)
# 5. Print / use the result ----------------------------------------
print(sig_cmp)
###======================== from 2ways anova checking interaction by interaction plot
#1-  Create an interaction plot
interaction.plot(data$Weight , data$Gender, data$Th_AQ_Effect,
                 xlab = "Weight", 
                 ylab = "Mean Th_AQ_Effect", 
                 trace.label = "Gender",
                 col = c("red", "blue"), 
                 pch = c(1, 2), 
                 lwd = 2)
#2- Create a box plot
ggplot(data, aes(x = interaction(data$Weight, data$Gender), y = data$Th_AQ_Effect)) +
  geom_boxplot() +
  labs(x = "Interaction of Weight and Gender", y = "Th_AQ_Effect") +
  theme_minimal()


######--------------------  [5]
anova_model <- aov(data$Li_So_Effect ~ data$Weight * data$Gender, data = data)
# Calculate estimated marginal means
emm5 <- emmeans(anova_model, ~ Weight * Gender)

# 3. Pairwise comparisons ------------------------------------------
cmp <- pairs(emm5, adjust = "tukey")   # or "bonferroni", "holm", etc.
# 4. Keep only significant comparisons -----------------------------
sig_cmp <- as.data.frame(cmp) |>        # convert to data.frame
  subset(p.value < 0.05)       # keep p < 0.05 (already adjusted)
# 5. Print / use the result ----------------------------------------
print(sig_cmp)
###======================== from 2ways anova checking interaction by interaction plot
#1-  Create an interaction plot
interaction.plot(data$Weight , data$Gender, data$Li_So_Effect,
                 xlab = "Weight", 
                 ylab = "Mean Li_So_Effect", 
                 trace.label = "Gender",
                 col = c("red", "blue"), 
                 pch = c(1, 2), 
                 lwd = 2)
#2- Create a box plot
ggplot(data, aes(x = interaction(data$Weight, data$Gender), y = data$Li_So_Effect)) +
  geom_boxplot() +
  labs(x = "Interaction of Weight and Gender", y = "Li_So_Effect") +
  theme_minimal()
