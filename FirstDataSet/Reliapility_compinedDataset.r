# split half analysis 
# cronbach Alpha analysis
# factor analysis


# setwd("/path/to/your/directory")
library(readxl)  #Load excel library
library(ggplot2) #load ggplot 2 package for plotting
library(ltm)
library(psych) 
library(splithalf)
source("plottingFunctions.R")



#data <- read.csv("BothDataSets.csv", header = TRUE)   #Load the raw data
data <- read_excel("FirstDataSet/First Data Set.xlsx", sheet = "Real question")   #Load the raw data
#data <- read_excel("Second Data Set 001.xlsx", sheet = "Form Responses 1")   #Load the raw data
#data <- read_excel("BothDataSets.xlsx", sheet = "Sheet1")   #Load the raw data


View(data)  #view data
# write_xlsx(data, "data_modified.xlsx")
clean_data = na.omit(data)
no_entry = ncol(clean_data)
totla_col = no_entry - 8

# performing split half test step by step on all the data (numarical data)
# split data 
# grouped by half of the length
# first_half = sapply(data[,c(9:(9+(totla_col/2)))], as.numeric)
# second_half = sapply(data[,c((9+(totla_col/2)):no_entry)], as.numeric)

# grouped by odd and even entries
odd_cols = seq(from=9, to = no_entry-1, by = 2)
even_cols = seq(from=10, to = no_entry, by = 2)

first_half = sapply(clean_data[,odd_cols], as.numeric)
second_half = sapply(clean_data[,even_cols], as.numeric)

#calculate correlation
split_cor = diag(cor(first_half, second_half, use = "complete.obs", method=("pearson"))) # method=("pearson", "kendall", "spearman")) 
                                                                                    #use =  "complete.obs", "everything"


print("correlaton of the two halvs"); print(split_cor)
#Spearman-Brown adjustment
split_cor_adj = ((2 * split_cor) / (1 + split_cor))
print("adjestment correlation"); print(split_cor_adj)

# Using the splitHalf function from psych package
numeric_data= sapply(clean_data[,9:21], as.numeric)
split_results <- splitHalf(numeric_data, raw=FALSE, brute=FALSE, n.sample=350, covar=FALSE,check.keys=TRUE,
                           key=NULL,ci=.05,use="pairwise")
guttman(numeric_data,key=NULL)
tenberge(numeric_data)
#glb(numeric_data,key=NULL)
glb.fa(numeric_data,key=NULL)
# View results
print(split_results)



##/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
## ......................... Cronbach alpha-test 
# Calculate Cronbach's alpha
cronbach_alpha <- alpha(numeric_data, check.keys=TRUE)

# Print the results
print(cronbach_alpha)



##/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
## ......................... Factor Analysis 
# KMO Test
kmo_result <- KMO(numeric_data)
print(kmo_result)

# Bartlett's Test
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
# Check the correlation matrix
print(cor_matrix)

bartlett_result <- cortest.bartlett(cor_matrix,n = 70,diag=TRUE)
print(bartlett_result)


# Factor Analysis
fa_result <- fa(numeric_data, nfactors = 3, rotate = "varimax",use="pairwise", cor="cor")
print(fa_result)

fa.diagram(fa_result)

