cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables


#Performance under adverse Indoor Environmental quality
library(plyr)
source("SecondDataSet/SRC_R_CodeSecondDataSets/plotpdf.R")

#Read data and import it: The whall set of data. 
# data <- read.csv("BothDataSets0.csv", header = TRUE)   #Load the raw data
#data <- read_excel("First Data Set.xlsx", sheet = "Real question")   #Load the raw data
# data <- read_excel("SecondDataSet/SecondDataSet.xlsx")#, sheet = "Form Responses 1")   #Load the raw data
data.df1 <- read.csv("SecondDataSet/Source_DataSets_/SecondDataSetFull.csv", header = TRUE)   #Load the raw data
# data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data

#Factor analysis revield the factors are connected to the following" 
#MR1 connect columns number [29,26,25,38,35]  ###Effect of thermal discomfort on performance decrease, #effect of lighting discomfort on performance decrease
#                                             ###Performance_decrease_if_noise_conditions_not_in_comfort_range, ##IEQ_improvement_boosts_performance
#                                             ###If_yes_how_much_does_it_affect_work
#MR4      [30,31,32,33]    belief on effect of IEQ on productivity(Beliefe)    #### مهم مهم جدا
#MR2      [19,18,20,21]   I can work effectivly (High performance)       #### مهم مهم جدا
#MR3      [10,11,12,13,14]   Sensitivity + cultural effect on workspace    #### مهم مهم جدا
#MR5      [24]      How_does_poor_air_quality_noise_impact_work_compared_to_a_single_factor
#MR6      [36, 28, 27] ###informed about IEQ parameters? Performance_decrease_if_air_quality_is_poor? Performance_decrease_if_thermal_conditions_not_in_comfort_range?
data.df1<-na.omit(data.df1)


subset.MR1<-data.df1[,c(29,26,25,38,35)]; summary(subset.MR1)
subset.MR2<-data.df1[,c(19,18,20,21)]; summary(subset.MR2)
subset.MR3<-data.df1[,c(10,11,12,13,14)];summary(subset.MR3)
subset.MR4<-data.df1[,c(30,31,32,33)]; summary(subset.MR4)
subset.MR5<-data.df1[,24]; summary(subset.MR5)
subset.MR6<-data.df1[,c(36, 28, 27)]; summary(subset.MR6)

qual_col = c(3, 4, 5, 6, 7, 8, 9, 15, 16, 17, 22, 23, 34, 37, 40, 41)

# "weighted.sd" takes the vector of values (x) and the weights (w), which are
# the loadings here, and returns the weighted standard deviation of the
# vector of values.
weighted.sd = function(x, w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  x.sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
  return(x.sd.w)
}
# "re.scale" puts the scores back into the metric of the original questions.
re.scale <- function(f.scores, raw.data, loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data, 1, weighted.mean, w = loadings)
  sds <- apply(raw.data, 1, weighted.sd, w = loadings)
  grand.mean <- mean(means)
  grand.sd <- mean(sds)
  final.scores <- ((fz.scores * grand.sd) + grand.mean)
  return(final.scores)
}
get.scores.fun <- function(data){
  fact <- factanal(data, factors = 1, scores = "regression")
  f.scores <- fact$scores[,1]
  f.loads <- abs(fact$loadings[,1])
  rescaled.scores <- re.scale(f.scores, data, f.loads)
  output.list <- list(rescaled.scores, f.loads)
  names(output.list) <- c("rescaled.scores", "factor.loadings")
  return(output.list)
}
# get.scores.fun(subset.MR1)
###########  Get factor analysis 
#MR1 connect columns number [29,26,25,38,35]  ###Effect of thermal and lighting discomfort combined on performance decrease, 
#                                             ###Performance_decrease_if_lighting_not_in_comfort_range
#                                             ###Performance_decrease_if_noise_conditions_not_in_comfort_range, 
#                                             ###IEQ_improvement_boosts_performance
#                                             ###If_yes_how_much_does_it_affect_work
#MR4      [30,31,32,33]    belief on effect of IEQ on productivity(Beliefe)    #### مهم مهم جدا
#MR2      [19,18,20,21]   I can work effectivly (High performance)       #### مهم مهم جدا
#MR3      [10,11,12,13,14]   Sensitivity + cultural effect on workspace    #### مهم مهم جدا
#MR5      [24]      How_does_poor_air_quality_noise_impact_work_compared_to_a_single_factor
#MR6      [36, 28, 27]  ###informed about IEQ parameters? 
#                       ###Performance_decrease_if_air_quality_is_poor? 
#                       ###Performance_decrease_if_thermal_conditions_not_in_comfort_range?

sensitivity.IEQ.change<-get.scores.fun(subset.MR3)$rescaled.scores
summary(sensitivity.IEQ.change)

high.performance<-get.scores.fun(subset.MR2)$rescaled.scores
summary(performance)
# performance

belief.about.IEQ<-get.scores.fun(subset.MR4)$rescaled.scores
summary(belief.about.IEQ)

effect.on.performancedue.light.noise<-get.scores.fun(subset.MR1)$rescaled.scores
summary(effect.on.performancedue.light.noise)

decreas.Perf.due.air.therm<-get.scores.fun(subset.MR6)$rescaled.scores
summary(effect.on.performancedue.light.noise)

####### plot factors 
nuemaric.data<- cbind(effect.on.performancedue.light.noise,high.performance,sensitivity.IEQ.change,belief.about.IEQ,subset.MR5,decreas.Perf.due.air.therm)
new.Factored.Data<- cbind(data.df1[,qual_col],nuemaric.data)


for (j in c(1:16)){
  plot_list <- list()  # store plots temporarily for sensitivity plot

  for (i in c(17:22)){
    plots <- plot_density_by_group(new.Factored.Data, 
                                       numeric_var = colnames(new.Factored.Data[i]),   #"Sensitive_to_air_quality_changes"
                                       group_var = colnames(new.Factored.Data[j]),
                                       caption_text = paste("Density plot of ", colnames(new.Factored.Data[i])),
                                       plot_title = paste("Density by different ", colnames(new.Factored.Data[j])),
                                       custom_colors = NULL)
    plot_list[[length(plot_list) + 1]] <- plots
  }
  # Combine the plots into a 2x2 grid
  grid_plots <- wrap_plots(plot_list, ncol=2)
  # Display the grid
  print(grid_plots)
}
