#descriptive analysis Using R 
# this code explore median, mean, standard daviation, histogram, quarters of the data 
# and plot the data according to the qualitative data. 

## Understanding the data...... 
#[1]  Timestamp
#[2]  Language  {Arabic, English}
# Location of Leaving
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
#data <- read.csv("BothDataSets0.csv", header = TRUE)   #Load the raw data
#data <- read_excel("First Data Set.xlsx", sheet = "Real question")   #Load the raw data
# data <- read_excel("SecondDataSet/SecondDataSet-Cleaned-Final-Analyzed.xlsx")#, sheet = "Form Responses 1")   #Load the raw data
data <- read.csv("SecondDataSet/SecondDataSetFull.csv", header = TRUE)   #Load the raw data
# data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data

numaric_col = c(10, 11, 12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
qual_col = c(3, 4, 5, 6, 7, 8, 9)
# column number 39,  is an open ended question, while question number 1 is a time stamp

# View(data)  #view data
# write_xlsx(data, "data_modified.xlsx")
no_entry = ncol(data)



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
    plot_sens <- plot_density_by_group(data, 
                          numeric_var = colnames(data[9+i]),   #"Sensitive_to_air_quality_changes"
                          group_var = colnames(data[j]),
                          caption_text = paste("Density plot of ", colnames(data[9+i])),
                          plot_title = paste("Density by different ", colnames(data[j])),
                          custom_colors = NULL)
    plot_list_sens [[length(plot_list_sens ) + 1]] <- plot_sens
    
    
    effictive_plot <- plot_density_by_group(data, 
                                            numeric_var = colnames(data[17+i]),   #"Sensitive_to_air_quality_changes"
                                            group_var = colnames(data[j]),
                                            caption_text = paste("Density plot of ", colnames(data[17+i])),
                                            plot_title = paste("Density by different ", colnames(data[j])),
                                            custom_colors = NULL)
    plot_list_effect [[length(plot_list_effect ) + 1]] <- effictive_plot
      
      
    perf_plot <- plot_density_by_group(data, 
                                       numeric_var = colnames(data[13+i]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(data[j]),
                                       caption_text = paste("Density plot of ", colnames(data[13+i])),
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


otherVar = c(22)

for (j in qual_col){
  plot_list_others <- list()  # store plots temporarily for sensitivity plot

  
  for (i in otherVar){
    #Sensetivity plot
    plot_other <- plot_density_by_group(data, 
                                       numeric_var = colnames(data[i]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(data[j]),
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

