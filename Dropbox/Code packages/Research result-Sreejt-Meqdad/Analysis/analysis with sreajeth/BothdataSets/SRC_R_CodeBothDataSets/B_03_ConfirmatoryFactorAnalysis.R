cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables

# Install the lavaan package if you haven't already
# install.packages("lavaan")

# Load the lavaan package
library(lavaan)
#load explarator factro analysis 
# source("BothdataSets/ExploratoryFactorAnalysis.r")
# --1. load your data 
survey_data <- read.csv("BothdataSets/BothDataSetsfull-Cleaned-Cleaned.csv", header = TRUE)
# --- 1. Specify the CFA Model based on your EFA results ---
# Each line defines a latent factor and the observed variables (items) that measure it.
# The ` =~ ` operator means "is measured by".
Names <- colnames(survey_data)
cfa_model <- '
  MR3 =~  AQ.sensitivity + TC.sensitivity + SQ.sensitivity+ LQ.sensitivity 
  MR2 =~ Performance.under.AQ + Performance.under.TC + Performance.under.LQ + Performance.under.SQ 
  MR1 =~ Effect...AQ + Effect...TC
  MR4 =~ Effect...LQ + Effect...SQ

  # Allow the latent factors to correlate (oblique rotation from EFA suggests this)
  MR2 ~~ MR4
  MR2 ~~ MR3
  MR2 ~~ MR1
  MR4 ~~ MR3
  MR4 ~~ MR1
  MR3 ~~ MR1
'

# --- 2. Fit the CFA Model to Your Data ---
cfa_fit <- cfa(cfa_model, data = survey_data)

# --- 3. Examine Model Fit ---
# This is crucial for evaluating how well your hypothesized model fits the observed data.
# `fit.measures = TRUE` provides a wide range of fit indices.
# `standardized = TRUE` provides standardized parameter estimates (like correlations)
# and standardized residuals, which are easier to interpret.
summary(cfa_fit, fit.measures = TRUE, standardized = TRUE)

# --- 4. Examine Factor Loadings and Other Parameters ---
# This function gives you more detailed output on all estimated parameters.
# `standardized = TRUE` is highly recommended for interpreting the strength of loadings.
parameterEstimates(cfa_fit, standardized = TRUE)

# --- 5. Plot the CFA Model Diagram ---
library(semPlot)

# Save the plot to a PDF file
pdf("cfa_model_diagram.pdf")
semPaths(cfa_fit, whatLabels = "est", layout = "tree", curvePivot = TRUE,
         exoCov = TRUE, esize = 0.5, edge.label.cex = 0.8,
         fade = FALSE, intercepts = FALSE, residuals = FALSE,
         main = "Confirmatory Factor Analysis Model")
dev.off()

factor.weight<- lavPredict(cfa_fit)

write.csv(factor.weight, file = "BothdataSets/Exploratory Factor Analysis/cfa_factors_outout_data.csv", row.names = FALSE)

