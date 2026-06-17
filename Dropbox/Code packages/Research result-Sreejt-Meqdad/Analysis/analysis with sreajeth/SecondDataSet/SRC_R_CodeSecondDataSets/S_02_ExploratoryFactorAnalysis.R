cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables
# install necessary packages if you haven't already

# install.packages("psych")
# install.packages("GPArotation") # Required for various rotations in psych

# Load the packages
library(psych)
library(GPArotation)
library(readxl)

# Load Your Data (assuming your data is already in an R data frame named 'survey_data') ---
survey_data <- read_excel("SecondDataSet/SecondDataSet-Cleaned-Final-Analyzed.xlsx")


numaric_col = c(10, 11, 12,13, 14, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 38)
qual_col = c(2, 3, 4, 5, 6, 7, 8, 9, 15, 16, 17, 22, 23, 34, 37, 40, 41)
# column number 39,  is an open ended question, while question number 1 is a time stamp
# Select the Numeric Ranking Variables for EFA ---
# Adjust this vector if you have a different set of variables for EFA.
efa_vars <- c(numaric_col)

# Create a subset of your data containing only the EFA variables
efa_data <- survey_data[,efa_vars]

# --- Step 3: Determine the Number of Factors ---
# It's good practice to examine the scree plot and eigenvalues to decide on the number of factors.
# A common rule of thumb is to retain factors with eigenvalues > 1 (Kaiser's criterion).

# Calculate eigenvalues from a principal components analysis (PCA)
pca_result <- principal(efa_data, nfactors = ncol(efa_data), rotate = "none")

# Print eigenvalues
print("Eigenvalues:")
print(pca_result$values)

# Plot the scree plot
# This plot helps visualize the point of inflection where the curve flattens
pdf("scree_plot.pdf") # Save the plot to a PDF file
plot(pca_result$values, type = "b", main = "Scree Plot",
     xlab = "Factor Number", ylab = "Eigenvalue", pch = 19, col = "blue")
abline(h = 1, col = "red", lty = 2) # Add a line at eigenvalue = 1
dev.off() # Close the PDF device



# Parallel Analysis 
print("Parallel Analysis:")
parallel_analysis_result <- fa.parallel(efa_data, fa = "fa", n.iter = 100) # fa = "fa" for factor analysis
print(parallel_analysis_result)

# Based on the scree plot and parallel analysis, decide on the optimal number of factors (k).
# k = 4 factors based
k_factors <- 6 # Placeholder: Adjust this based on your scree plot and parallel analysis

# --- Step 4: Perform Exploratory Factor Analysis (EFA) ---
# We'll use the 'fa()' function from the 'psych' package.
# 'nfactors': The number of factors to extract (determined in Step 3).
# 'rotate': The rotation method (e.g., "varimax" for orthogonal, "oblimin" for oblique).
#           "oblimin" is often preferred for social science data as factors are rarely truly uncorrelated.
# 'fm': Factor method (e.g., "minres" for minimum residual, "ml" for maximum likelihood).
#       "minres" is a good general-purpose method; "ml" assumes multivariate normality.

efa_result <- fa(efa_data, nfactors = k_factors, rotate = "oblimin", fm = "minres")
# efa_result <- fa(efa_data, nfactors = k_factors, rotate = "varimax", fm = "ml")
# efa_result <- fa(efa_data, nfactors = k_factors, rotate = "oblimin", fm = "ml")
# efa_result <- fa(efa_data, nfactors = k_factors, rotate = "varimax", fm = "minres")

# Print the EFA results
print("EFA Results:")
print(efa_result)

# To get more detailed output, including factor loadings, uniquenesses, and communalities:
print("Factor Loadings (with structure and pattern matrices for oblique rotation):")
print(efa_result$loadings)

# Plot the factor analysis results (factor diagram)
pdf("efa_factor_diagram.pdf") # Save the plot to a PDF file
fa.diagram(efa_result, main = "EFA Factor Diagram")
dev.off() # Close the PDF device

# Interpretation of EFA Results:
# - Look at the 'loadings' table: These values represent the correlation between each variable and each factor.
#   Variables with high loadings (e.g., > 0.3 or 0.4) on a factor are considered to "load" onto that factor.
# - Examine 'h2' (communalities): The proportion of variance in each variable explained by the extracted factors.
# - Examine 'u2' (uniquenesses): The proportion of variance in each variable *not* explained by the factors (1 - h2).
# - Review the 'Factor correlations' (if using oblique rotation like oblimin): This shows correlations between the factors themselves.
# - Use the 'fa.diagram.pdf' to visually interpret the factor structure.