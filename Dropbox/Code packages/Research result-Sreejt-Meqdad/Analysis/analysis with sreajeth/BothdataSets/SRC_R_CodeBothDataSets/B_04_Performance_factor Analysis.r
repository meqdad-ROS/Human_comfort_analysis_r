cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables


#Performance under adverse Indoor Environmental quality
library(plyr)
source("BothdataSets/SRC_R_CodeBothDataSets/B_plotpdf.R")

#Read data and import it: The whall set of data. 
data.df1  <- read.csv("BothdataSets/Source_DataSets_/BothDataSetsfull-Cleaned-Cleaned.csv", header = TRUE)
# data.df1  <- read.csv("FirstDataSet/First Data Set.csv", header = TRUE)   #Load the raw data
# data.df1 <- read.csv("SecondDataSet/SecondDataSetFull.csv", header = TRUE)   #Load the raw data
# data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data

#Factor analysis revield the factors are connected to the following" 
#MR1      [9,10,11,12]    ###Sensitivity to IEQ fuctuation #### مهم مهم جدا
#MR2      [13,14,15,16]    I can work effectivly (High performance)       #### مهم مهم جدا
#MR3      [17,18,19,20,21]   Effect on work performance due to change

data.df1<-na.omit(data.df1)

subset.MR1<-data.df1[,c(9,10,11,12)]; summary(subset.MR1)
subset.MR2<-data.df1[,c(13,14,15,16)]; summary(subset.MR2)
subset.MR3<-data.df1[,c(17,18,19,20)];summary(subset.MR3)


qual_col = c(3, 4, 5, 6, 7, 8)

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


sensitivity.IEQ.change<-get.scores.fun(subset.MR1)$rescaled.scores
summary(sensitivity.IEQ.change)

high.performance<-get.scores.fun(subset.MR2)$rescaled.scores
summary(high.performance)
# performance

effect.of.IEQ.on.performance<-get.scores.fun(subset.MR3)$rescaled.scores
summary(effect.of.IEQ.on.performance)


####### plot factors 
nuemaric.data<- cbind(sensitivity.IEQ.change,high.performance,effect.of.IEQ.on.performance)
new.Factored.Data<- cbind(data.df1[,qual_col],nuemaric.data)


for (j in c(1:6)){
  plot_list <- list()  # store plots temporarily for sensitivity plot

  for (i in c(7:9)){
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

