cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables


#descriptive analysis Using R 
# this code explore median, mean, standard daviation, histogram, quarters of the data 
# and plot the data according to the qualitative data. 

## Understanding the data...... 
#[1]  Timestamp
#[2]  Language  {Arabic, English}
#[3]  Location of Leaving	  {North America, Gulf Country, East Europ, Mediterranean, Oceania, Sweden}
#[4]  Occupation {House Wife, More-Movement, Office-work, outdoor-work}
#[5]  age	  {18 - 30, 30 - 45, 45 - 60, Above 60}
#[6]  Length(Height)    {Above 190, 180 - 190, 170 - 180, 160 - 170, 150 - 160, Under 150}
#[7]  Weight(kg)    {<=55 kg, 56-70 kg, 71-85 kg, 86-100 kg, 101-115 kg, 116-130 kg, >=131 kg}	
#[8]  Gender    {male, Female}

#[9]  AQ sensitivity    #                   = numaric range (1-5)
#[10] TC sensitivity	  #                   = numaric range (1-5)
#[11] SQ sensitivity	  #                   = numaric range (1-5)
#[12] LQ sensitivity    #                   = numaric range (1-5)

#[13] Performance under AQ	  #                   = numaric range (1-5)
#[14] Performance under TC	  #                   = numaric range (1-5)
#[15] Performance under LQ	  #                   = numaric range (1-5)	
#[16] Performance under SQ	  #                   = numaric range (1-5)

#[17] Effect - LQ		  #                   = numaric range (1-5)
#[18] Effect - SQ		  #                   = numaric range (1-5)
#[19] Effect - AQ		  #                   = numaric range (1-5)
#[20] Effect - TC 	  #                   = numaric range (1-5)

#[21] Other AQ - conditions  	  #                   = numaric range (1-5)

############################################################################################################
# 1- Sensitivity 
# 2- Performance 
# 3- Effect
# compare them with the following data 
# (1) gender, (2) age, (3) whight (group whight), (4) height,  (5) profession
#import Libraries 
# setwd("/path/to/your/directory")
library(readxl)  #Load excel library
library(ggplot2) #load ggplot 2 package for plotting
library(ggtext) # Load the ggtext package
library(psych)
library(patchwork)
library(dplyr)
source("SecondDataSet/plotpdf.R")


#Read data and import it: The whall set of data. 
data <- na.omit(read.csv("BothdataSets/BothDataSetsfull-Cleaned-Cleaned.csv", header = TRUE))   #Load the raw data
#data <- read_excel("First Data Set_Cleaned.xlsx", sheet = "Real question")   #Load the raw data
# data <- read.csv("FirstDataSet/First Data Set.csv", header = TRUE)   #Load the raw data #FirstDataSet/First Data Set.csv
# data <- read_excel("SecondDataSet/SecondDataSet-Cleaned-Final-Analyzed.xlsx")#, sheet = "Form Responses 1")   #Load the raw data
# data <- read.csv("SecondDataSet/SecondDataSetFull.csv", header = TRUE)   #Load the raw data
# data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data

numaric_col = c(9, 10, 11, 12,13, 14, 15, 16, 17, 18, 19, 20, 21)
qual_col = c(3, 4, 5, 6, 7, 8)
# column number 39,  is an open ended question, while question number 1 is a time stamp

# View(data)  #view data
# write_xlsx(data, "data_modified.xlsx")
data <- data[complete.cases(data), ]



# Sens_age <- data %>%
#   select(numaric_col)  %>%
#   sapply(as.numeric)  %>%
#   na.omit()
# 
# qualit_data <- data %>%
#   select(qual_col) %>%
#   na.omit()
# 
# senst <- data 
n <- length(qual_col)
plots_per_page <- 4

for (j in qual_col){
  plot_list_sens <- list()  # store plots temporarily for sensitivity plot
  plot_list_effect <- list() # store plots temporarily for 
  plot_list_perf <- list() # store plots temporarily for 
  plot_list_impact <- list() # store plots temporarily for 
  
  for (i in c(1:4)){
    #Sensetivity plot
    plot_sens <- plot_density_by_group(na.omit(data), 
                                       numeric_var = colnames(data[8+i]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(data[j]),
                                       caption_text = paste("Density plot of ", colnames(data[8+i])),
                                       plot_title = paste("Density by different ", colnames(data[j])),
                                       custom_colors = NULL)
    plot_list_sens [[length(plot_list_sens ) + 1]] <- plot_sens
    
    
    effictive_plot <- plot_density_by_group(na.omit(data), 
                                            numeric_var = colnames(data[16+i]),   #"Sensitive_to_air_quality_changes"
                                            group_var = colnames(data[j]),
                                            caption_text = paste("Density plot of ", colnames(data[16+i])),
                                            plot_title = paste("Density by different ", colnames(data[j])),
                                            custom_colors = NULL)
    plot_list_effect [[length(plot_list_effect ) + 1]] <- effictive_plot
    
    
    perf_plot <- plot_density_by_group(na.omit(data), 
                                       numeric_var = colnames(data[12+i]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(data[j]),
                                       caption_text = paste("Density plot of ", colnames(data[12+i])),
                                       plot_title = paste("Density by different ", colnames(data[j])),
                                       custom_colors = NULL)
    plot_list_perf[[length(plot_list_perf) + 1]] <-  perf_plot 
    
  }
  # Combine the plots into a 2x2 grid
  grid_plot_sens <- wrap_plots(plot_list_sens, ncol=2)
  # Display the grid
  print(grid_plot_sens)
  
  grid_plot_effect <- wrap_plots(plot_list_effect, ncol=2)
  # Display the grid
  print(grid_plot_effect)
  
  grid_plot_perf <- wrap_plots(plot_list_perf, ncol=2)
  # Display the grid
  print(grid_plot_perf)
}


otherVar = c(21)

for (j in qual_col){
  plot_list_others <- list()  # store plots temporarily for sensitivity plot
  
  
  for (i in otherVar){
    #Sensetivity plot
    plot_other <- plot_density_by_group(na.omit(data), 
                                        numeric_var = colnames(na.omit(data[i])),   #"Sensitive_to_air_quality_changes"
                                        group_var = colnames(na.omit(data[j])),
                                        caption_text = paste("Density plot of ", colnames(data[i])),
                                        plot_title = paste("Density by different ", colnames(data[j])),
                                        custom_colors = NULL)
    plot_list_others [[length(plot_list_others ) + 1]] <- plot_other
    
  }
  # Combine the plots into a 2x2 grid
  grid_plot_Others <- wrap_plots(plot_list_others, ncol=3)
  # Display the grid
  print(grid_plot_Others)
}

