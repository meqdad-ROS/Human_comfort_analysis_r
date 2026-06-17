cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables

#descriptive analysis Using R 
# this code explore median, mean, standard daviation, histogram, quarters of the data 
# and plot the data according to the qualitative data. 

#import Libraries 
# setwd("/path/to/your/directory")
library(readxl)  #Load excel library
#library(ggplot2) #load ggplot 2 package for plotting
#library(ltm)
library(psych)
#library(splithalf)
#source("plottingFunctions.R")


#Read data and import it: The whall set of data. 
# data <- read.csv("BothDataSets0.csv", header = TRUE)   #Load the raw data
# data <- read_excel("FirstDataSet/First Data Set_Cleaned.xlsx", sheet = "Real question")   #Load the raw data
data <- read.csv("FirstDataSet/First Data Set.csv", header = TRUE)   #Load the raw data
#data <- read_excel("Second Data Set 001.xlsx", sheet = "Form Responses 1")   #Load the raw data
#data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data


View(data)  #view data
# write_xlsx(data, "data_modified.xlsx")
clean_data = na.omit(data)
no_entry = ncol(clean_data)
a = c(9:21)
b = c(2:8)
numeric_data= sapply(clean_data[,a], as.numeric)
qualit_data = clean_data[,b]
numeric_data = na.omit(numeric_data)

## Understanding the data...... 
#[1]  Timestamp
#[2]  Language  {Arabic, English}
#[3]  Location of Leaving	  {USA, Sweden, New Zeland, Canada, Russia, Jordan, Kuwait, Saudi, Moldova
#                           Qatar, Saudi Arabia, Sudan, Australia, Oman, Saudia Arabia
#                           Egypt, Yemen}
#

#[4]  Occupation	  {site worker, office worker, Student, Housewife, teacher, Agricultural engineer, 
#                   Manager, Nurse, University Prof, Engineer, Humanitarian Employee, Business w/man, 
#                   academic, Paramedic, Pharmaceutical, Logistic Assistant, Computer tech, Chemical, 
#                   administrative, Doctor, Chemical Analyst, Pilot, Interior designer, 
#                   Industrial Hygienist, Lawyer, Employee, accountant, Production Supervisor, 
#                   Architect, Database manager, Dispatch, Entrepreneur, Health Care Assistant, 
#                   Infrastructure officer, Medical Director, Officer, Researcher, 
#                   Senior Project Officer, Technician}

#[5]  age	  {45 - 60, 30 - 45, 18 - 30, Above 60}
#[6]  Length    {Above 190, 170 - 180, 180 - 190, 160 - 170, Under 150, 150 - 160}	
#[7]  Weight    {}	
#[8]  Gender    {male, Female}

#[9]  AQ sensitivity	
#[10] TC sensitivity	
#[11] SQ sensitivity	
#[12] LQ sensitivity
#[13] Performance under AQ	
#[14] Performance under TC	
#[15] Performance under LQ
#[16] Performance under SQ
#[17] Effect - LQ	
#[18] Effect - SQ	
#[19] Effect - AQ	
#[20] Effect - TC	
#[21] Other AQ - conditions

cat("\014")  #clear consol from code 

## Mean
mean_value <- colMeans(numeric_data, na.rm = TRUE)

# describe(clean_data)
summary(clean_data)

for (i in c(1:ncol(numeric_data))){
  # Calculate the density estimate
  density_estimate <- density(numeric_data[,i])
  
  # Plot the density estimate
  plot(density_estimate,
       main = "Density Plot of Sample Data",
       xlab = names(numeric_data[,i]),
       ylab = "Density",  
       col = "blue",
       lwd = 2)
  
  hist(numeric_data[,i])
}
# apply(clean_data)
print(summary)

apply(numeric_data,2, mean)
apply(numeric_data,2, sd)

for (i in c(1:ncol(qualit_data))){
    freq_table = table(qualit_data[,i])
    prob_table = prop.table(freq_table)
    print(freq_table)
    print(prob_table)
    # Create labels with counts
    labels_with_counts <- paste0(names(freq_table), " (n=", freq_table, ")")
    
    pie(freq_table, labels = labels_with_counts, main = "Distribution of Categories (count)")
  }


 
 
# Variance
variance_value <- apply(numeric_data, 2, var)

# # Standard Deviation
std_dev <- apply(numeric_data, 2, sd) 
# 
# # Quartiles
# quartiles <- quantile(numeric_data)

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

























