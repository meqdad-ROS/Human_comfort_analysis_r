cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables

# Install the lavaan package if you haven't already
# install.packages("lavaan")

# Load the lavaan package
library(lavaan)
library(readxl)  #Load excel library
#load explarator factro analysis 
# source("BothdataSets/ExploratoryFactorAnalysis.r")
# --1. load your data 

# survey_data <- read_excel("SecondDataSet/SecondDataSet-Cleaned-Final-Analyzed.xlsx")
survey_data <- read_excel("SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx")
# --- 1. Specify the CFA Model based on your EFA results ---
# Each line defines a latent factor and the observed variables (items) that measure it.
# The ` =~ ` operator means "is measured by".
Names <- colnames(survey_data)
numaric_col = c(10, 11, 12,13, 14, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 38)
numaric_col = c(10, 11, 12,13, 14, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 38)
cfa_model <- '

  MR4 =~ Do_air_quality_changes_directly_impact_productivity + Do_thermal_changes_directly_impact_productivity + Do_noise_changes_directly_impact_productivity + Do_lighting_changes_directly_impact_productivity
  MR2 =~ Work_effectively_if_only_air_quality_is_not_optimal + Work_Effictive_if_high_low_lighting_levels_not_optimal_despite_other_differences + Work_effectively_if_only_thermal_comfort_is_not_optimal + Work_Effective_even_if_noise_sound_isnt_ideal
  MR1 =~ Performance_decrease_if_excessive_noise_or_complete_silence + Do_noise_changes_directly_impact_productivity + Do_lighting_changes_directly_impact_productivity + How_does_poor_air_quality_noise_impact_work_compared_to_a_single_factor + IEQ_improvement_boosts_performance
  MR3 =~ Sensitive_to_air_quality_changes + Sensitive_to_thermal_condition_changes + Sensitive_to_noise_sound_changes + Sensitive_to_light_level_changes + Performance_decrease_if_lighting_not_in_comfort_range
  MR5 =~ When_thermal_lighting_discomfort_combined_how_does_this_impact_work
  MR6 =~ Informed_about_IEQ_parameters + Performance_decrease_if_air_quality_is_poor + Performance_decrease_if_thermal_conditions_not_in_comfort_range



  # Allow the latent factors to correlate (oblique rotation from EFA suggests this)
  MR4 ~~ MR3
  MR4 ~~ MR6
  MR4 ~~ MR1
  MR3 ~~ MR6
  MR6 ~~ MR1

'
survey_data_1 <- na.omit(survey_data[, numaric_col])
# --- 2. Fit the CFA Model to Your Data ---
cfa_fit <- cfa(cfa_model, data = survey_data_1)

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

write.csv(factor.weight, file = "SecondDataSet/results/cfa_factors_outout_ALTERNATIVE.csv", row.names = FALSE)

