library(ggplot2)
library(patchwork)
source("BothdataSets/plotpdf.r")
data  <- read.csv("BothdataSets/Exploratory Factor Analysis/group_cfa_factors_input_data.csv", header = TRUE)

for (j in c(1:6)){
  plot_list_sens <- list()  # store plots temporarily for sensitivity plot
 
  
  for (i in c(7:10)){
    #Sensetivity plot
    plot_sens <- plot_density_by_group(na.omit(data), 
                                       numeric_var = colnames(data[i]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(data[j]),
                                       caption_text = paste("Density plot of ", colnames(data[i])),
                                       plot_title = paste("Density by different ", colnames(data[j])),
                                       custom_colors = NULL)
    plot_list_sens [[length(plot_list_sens ) + 1]] <- plot_sens
  }
  # Combine the plots into a 2x2 grid
  grid_plot_sens <- wrap_plots(plot_list_sens, ncol=2)
  # Display the grid
  print(grid_plot_sens)
  
}





for (j in c(7:10)){
  plot_list_sens <- list()  # store plots temporarily for sensitivity plot
  
  
  for (i in c(1:6)){
    #Sensetivity plot
    plot_sens <- plot_density_by_group(na.omit(data), 
                                       numeric_var = colnames(data[j]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(data[i]),
                                       caption_text = paste("Density plot of ", colnames(data[j])),
                                       plot_title = paste("Density by different ", colnames(data[i])),
                                       custom_colors = NULL)
    plot_list_sens [[length(plot_list_sens ) + 1]] <- plot_sens
  }
  # Combine the plots into a 2x2 grid
  grid_plot_sens <- wrap_plots(plot_list_sens, ncol=2)
  # Display the grid
  print(grid_plot_sens)
  
}
