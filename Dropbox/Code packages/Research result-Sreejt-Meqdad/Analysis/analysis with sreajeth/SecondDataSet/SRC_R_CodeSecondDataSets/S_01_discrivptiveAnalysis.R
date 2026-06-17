cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables



#descriptive analysis Using R 
# this code explore median, mean, standard daviation, histogram, quarters of the data 
# and plot the data according to the qualitative data. 

#import Libraries 
# setwd("/path/to/your/directory")
# library(readxl)  #Load excel library
library(ggplot2) #load ggplot 2 package for plotting
#library(ltm)
library(psych)
#library(splithalf)
library(dplyr)
#source("plottingFunctions.R")


#Read data and import it: The whall set of data. 
# data <- read.csv("BothDataSets0.csv", header = TRUE)   #Load the raw data
#data <- read_excel("First Data Set.xlsx", sheet = "Real question")   #Load the raw data
# data <- read_excel("SecondDataSet/SecondDataSet.xlsx")#, sheet = "Form Responses 1")   #Load the raw data
data <- read.csv("SecondDataSet/Source_DataSets_/SecondDataSetFull.csv", header = TRUE)   #Load the raw data
# data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data


# View(data)  #view data
# write_xlsx(data, "data_modified.xlsx")
clean_data = na.omit(data)
no_entry = ncol(clean_data)


## Understanding the data...... 
#[1]  Timestamp
#[2]  Language  {Arabic, English}
#[3]  Gender    {male, Female}
#[4]  Length(Height)    {Above 190, 180 - 190, 170 - 180, 160 - 170, 150 - 160, Under 150}	
#[5]  age	  {18 - 30, 30 - 45, 45 - 60, Above 60}
#[6]  Weight(kg)    {}	
#[7]  How long have you been working in your current role or environment?
#                   ={Less than 1 year, 1-3 years, 3-5 years, More than 5 years}
#[8]  What is your typical daily work schedule or duration in your current role or environment? 
#                   ={Less than 4 hours, 4-6 hours, 6-8 hours, More than 8 hours}
#[9]  Does your work involve a high cognitive load, such as tasks requiring research, problem-solving, creative thinking, coding, design, writing, reading academic papers, or similar activities?
#                   ={Yes, heavily,     Yes, moderately,     No, my work is mostly routine,       No, my work is mostly physical}
#[10] Do cultural or personal preferences, such as lighting brightness, noise tolerance, or thermal conditions, affect your comfort in this workspace?
#                   = numaric range (1-5)
#[11] I tend to be highly sensitive to changes in air quality, no matter how small, in my workspace.
#                   = numaric range (1-5)
#[12] I tend to be highly sensitive to changes in thermal conditions, no matter how small, in my workspace.
#                   = numaric range (1-5)
#[13] I tend to be highly sensitive to changes in  the noise and sound level, no matter how small, in my workspace.
#                   = numaric range (1-5)
#[14] I tend to be highly sensitive to changes in  the light level, no matter how small, in my workspace.
#                   = numaric range (1-5)
#[15] When you notice a fluctuation in an IEQ factor, do you actively attempt to adjust your environment (e.g., adjusting thermostat, opening windows, using personal lighting, wearing noise-canceling headphones, etc)?
#                   ={Always, Sometimes, Rarely, Never}
#[16] Are you more sensitive to IEQ changes due to health conditions (e.g., allergies, migraines)?
#                   = {Yes,  No, Prefer not to say}
#[17] How quickly do you notice a change in each IEQ parameter (indoor air quality, Thermal Comfort, Acoustic Quality, Visual Comfort) in your work environment?
#                   = {Instantly, After a short while, After an extended period, I don’t notice at all}
#[18] "I can work effectively even if the air quality is not optimal, as long as the other three components (e.g., lighting, noise, and temperature) are within a comfortable range.
#                   = numaric range (1-5)
#[19] I can work effectively even if the thermal comfort is not obtained (higher or lower conditions), as long as the other three components are within the comfort level.
#                   = numaric range (1-5)
#[20] I can work effectively even if visual comfort is not achieved (e.g., due to high or low lighting levels, glare, or noticeable color differences under the light), as long as the other three components—such as thermal conditions, noise levels, and air quality—are within a comfortable range.
#                   = numaric range (1-5)
#[21] I can work effectively even if noise and sound conditions are not ideal (whether too loud or too quiet), as long as the other three components—such as thermal comfort, lighting, and air quality—are within a comfortable range.
#                   = numaric range (1-5)
# [22] If you experience discomfort from IEQ fluctuations, what is your typical response?
#                   = {I take a break, I attempt to adjust my environment, I continue working but feel less productive,
#                      It does not affect me}
# [23]  Have you ever requested workplace modifications (e.g., temperature adjustments, noise control, air purification)?
#                   = {Yes, frequently,
#                      Yes, occasionally,
#                      No, but I have considered it,
#                      No, I have never needed to}
# [24] When both air quality and noise levels are poor, how does this impact your work performance compared to a single factor?
#                   = numaric range (1-5)
# [25] When thermal discomfort (e.g., too hot/cold) and poor lighting are combined, how does this impact your work performance compared to a single factor?
#                   = numaric range (1-5)
# [26] In your working environment, to what extent does your performance decrease if the lighting level or conditions are not within the comfort range?
#                   = numaric range (1-5)
# [27] In your working environment, to what extent does your performance decrease if the thermal conditions are above or below the comfort range?
#                   = numaric range (1-5)
# [28] In your working environment, to what extent does your performance decrease if the air quality is poor or uncomfortable?
#                   = numaric range (1-5)
# [29] In your working environment, to what extent does your performance decrease if there is excessive noise above comfort levels or if the place is completely silent?
#                   = numaric range (1-5)
# [30] Do you believe that changes in air quality directly impact your productivity?
#                   = numaric range (1-5)
# [31] Do you believe that changes in thermal conditions directly impact your productivity?
#                   = numaric range (1-5)
# [32] Do you believe that changes in noise levels directly impact your productivity?
#                   = numaric range (1-5)
# [33] Do you believe that changes in lighting conditions directly impact your productivity?
#                   = numaric range (1-5)
# [34] Have you ever experienced a headache without a prior illness or felt sleepy in your working environment, despite having sufficient sleep, and you felt the cause was the light conditions or the air quality?
#                   = {Yes, I do experienced such case
#                      No, I do not
#                      I do not remember if it happened}

# [35] Do you believe that improving IEQ could significantly enhance your work performance?
#                   = numaric range (1-5)
# [36] How informed are you about specific IEQ parameters (e.g., CO2 levels, humidity) in your workplace?
#                   = numaric range (1-5)
# [37] Have you ever received training on how indoor environmental factors affect work performance? If not, would you be willing to participate in workplace programs aimed at improving Indoor Environmental Quality (IEQ)?
#                   = {Yes, I have received training on this topic, and it has greatly benefited my work.,
#                      Yes, I have received training on this topic; however, it has had no impact on my work performance.,
#                      No, I have not received training, but I am willing to participate in workplace programs to improve IEQ.,
#                      No, I have not received training, and I am not interested in participating in workplace programs.,
#                      }

# [38] In the previous question, if yes, to what extent does this affect your work performance?
#                   = numaric range (1-5)
# [39] What single improvement in Indoor Environmental Quality (IEQ) would most enhance your productivity or what aspect of your workplace environment would you like to improve? Has this factor previously affected your productivity? If so, how did you respond to it?
#                   = {open ended qualitative question}
# [40] Country where you work? Location of Leaving	  {USA, Sweden, New Zeland, Canada, Russia, Jordan, Kuwait, Saudi, Moldova
#                           Qatar, Saudi Arabia, Sudan, Australia, Oman, Saudia Arabia
#                           Egypt, Yemen}



numaric_col = c(10, 11, 12,13, 14, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 38)
qual_col = c(2, 3, 4, 5, 6, 7, 8, 9, 15, 16, 17, 22, 23, 34, 37, 40, 41)
# column number 39,  is an open ended question, while question number 1 is a time stamp
# a_num = 


# b_qual

numaric_data <- na.omit(as.data.frame(sapply(data[numaric_col], as.numeric)))
  
qualit_data <- na.omit(data[ , qual_col])

## Mean
mean_value <- colMeans(numaric_data, na.rm = TRUE)

# describe(clean_data)
summary(numaric_data)

for (i in c(1:ncol(numaric_data))){
  # Calculate the density estimate
  density_estimate <- density(numaric_data[,i])
  
  # Plot the density estimate
  plot(density_estimate,
       main = "Density Plot of Sample Data",
       xlab = names(numaric_data[,i]),
       ylab = "Density",  
       col = "blue",
       lwd = 2)
  
  hist(numaric_data[,i])
}
# apply(clean_data)
print(summary)

apply(numaric_data,2, mean)
apply(numaric_data,2, sd)

for (i in c(1:ncol(qualit_data))){
    freq_table = table(qualit_data[,i])
    prob_table = prop.table(freq_table)
    print(i)
    print(freq_table)
    print(prob_table)
    # Create labels with counts
    labels_with_counts <- paste0(names(freq_table), " (n=", freq_table, ")")
    
    pie(freq_table, labels = labels_with_counts, main = "Distribution of Categories (count)")
  }


 
 
# Median
# median_value <- median(numaric_data)

# Variance
variance_value <- apply(numaric_data, 2, var)

# # Standard Deviation
std_dev <- apply(numaric_data, 2, sd) 
# 
# # Quartiles
# quartiles <- quantile(numaric_data)

# # Frequency table
# freq_table <- table(qualit_data)
# 
# # Proportions
# prop_table <- prop.table(freq_table)


print("Mean value of all numaric data ============================")
print(mean_value)
print(" ============================ ============================")
print("Standard Daviation value of all numaric data ============================")
print(std_dev)
print(" ============================ ============================")
print("Variationa of all numaric data ============================")
print(variance_value)



# print("The frequency table of all qualitative data ============================")
# print(freq_table)
# print("the probability table of all qualitative data ============================")
# print(prop_table)


library(dplyr)
for (i in c(1:ncol(qualit_data))){
  freq_table = table(qualit_data[,i])
  prob_table = prop.table(freq_table)
  print(freq_table)
  print(prob_table)
  # Create labels with counts
  labels_with_counts <- paste0(names(freq_table), " (n=", freq_table, ")")
  
  pie(freq_table, labels = labels_with_counts, main = "Distribution of Categories (count)")
}

























