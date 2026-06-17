
# setwd("/path/to/your/directory")
library(readxl)  #Load excel library
library(ggplot2) #load ggplot 2 package for plotting
library(ltm)
source("plottingFunctions.r")


data <- read_excel("raw_data.xlsx")   #Load the raw data
View(data)  #view data
# write_xlsx(data, "data_modified.xlsx")

OrigenalSet = analyseData(data)

indecator01 <- rep(0,ra)
indecator02 <- rep(0,ra)
indecator03 <- rep(0,ra)
for (i in 1 : ra){
    if ((data[i,9] == data[i,10]) & (data[i,10] == data[i,11]) & (data[i,11] == data[i,12])){
      indecator01[i] <- 1 
    }
    if ((data[i,13] == data[i,14]) & (data[i,14] == data[i,15]) & (data[i,15] == data[i,16])){
      indecator02[i] <- 1 
    }
    if ((data[i,17] == data[i,18]) & (data[i,18] == data[i,19]) & (data[i,19] == data[i,20])& (data[i,20] == data[i,21])){
      indecator03[i] <- 1 
    }
}


sum(indecator01)
sum(indecator02)
sum(indecator03)

minDataIndex01 <- data[-c(which(indecator01 == 1)),]
miniSetremoved01 <- analyseData(minDataIndex01)

minDataIndex02 <- data[-c(which(indecator02 == 1)),]
miniSetremoved02 <- analyseData(minDataIndex02)

minDataIndex03 <- data[-c(which(indecator03 == 1)),]
miniSetremoved03 <- analyseData(minDataIndex03)

distinictDataset <- data[-unique(c(which(indecator01 == 1), which(indecator02 == 1), which(indecator03 == 1))),]
distinictDataSetAnalysis <- analyseData(distinictDataset)





SensitivityQuestions <- data[-c(which(indecator01 == 1)),c(1:12)]
SensitivityTest <- analyseData(SensitivityQuestions)

performanceQuestions <- data[-c(which(indecator02 == 1)),c(1:8,13:16)]
performanceTest <- analyseData(performanceQuestions)

effectQuestions <- data[-c(which(indecator03 == 1)),c(1:8,17:21)]
effectTest <- analyseData(minDataIndex03)


maleDataSet <- data[c(which(data[,'Gender'] == 'Male')),]
femaleDataSet <- data[c(which(data[,'Gender'] == 'Female')),]

analysMaleAll <- analyseData(maleDataSet)
analysFemaleAll <- analyseData(femaleDataSet)


