# Response Surface Methodology Analysis for Environmental Conditions Dataset
# Install required packages if needed
# install.packages(c("rsm", "ggplot2", "dplyr", "tidyr", "plotly", "viridis"))

library(rsm)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# =============================================================================
# STEP 1: Load and Prepare Data
# =============================================================================

# Read the CSV file
data <- read.csv("BothdataSets/BothDataSetsfull-Cleaned-Cleaned.csv", stringsAsFactors = FALSE)

# Display data structure
cat("Dataset Dimensions:", nrow(data), "rows,", ncol(data), "columns\n\n")
cat("Column Names:\n")
print(names(data))

# =============================================================================
# STEP 2: Data Cleaning and Preparation
# =============================================================================

# Clean the data - remove rows with non-numeric values in key columns
# Convert sensitivity and performance columns to numeric
sensitivity_cols <- c("AQ.sensitivity", "TC.sensitivity", "SQ.sensitivity", "LQ.sensitivity")
performance_cols <- c("Performance.under.AQ", "Performance.under.TC", 
                      "Performance.under.LQ", "Performance.under.SQ")
effect_cols <- c("Effect...LQ", "Effect...SQ", "Effect...AQ", "Effect...TC")

# Clean column names (replace dots with underscores for easier handling)
names(data) <- gsub("\\.", "_", names(data))

# Convert to numeric, forcing errors to NA
for(col in c("AQ_sensitivity", "TC_sensitivity", "SQ_sensitivity", "LQ_sensitivity",
             "Performance_under_AQ", "Performance_under_TC", 
             "Performance_under_LQ", "Performance_under_SQ",
             "Effect___LQ", "Effect___SQ", "Effect___AQ", "Effect___TC")) {
  data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
}

# Remove rows with missing values in key columns
data_clean <- data %>%
  filter(!is.na(AQ_sensitivity) & !is.na(TC_sensitivity) & 
           !is.na(SQ_sensitivity) & !is.na(LQ_sensitivity) &
           !is.na(Performance_under_AQ) & !is.na(Performance_under_TC))

cat("\nCleaned Dataset:", nrow(data_clean), "observations\n")

#=============================================================================
# STEP 3: Define Response Surface Models
# =============================================================================

# We'll analyze how sensitivity factors (AQ, TC, SQ, LQ) affect performance
# Let's focus on a subset of factors for interpretable RSM

# Model 1: AQ and TC sensitivity vs Performance under AQ
cat("\n=== MODEL 1: AQ & TC Sensitivity → Performance under AQ ===\n")



# Code the variables (center and scale)
data_clean <- na.omit(data_clean)
data_rsm1 <- coded.data(data_clean, 
                        x1 ~ (AQ_sensitivity - mean(AQ_sensitivity)) / sd(AQ_sensitivity),
                        x2 ~ (TC_sensitivity - mean(TC_sensitivity)) / sd(TC_sensitivity))

# Fit second-order model
model1 <- rsm(Performance_under_AQ ~ SO(x1, x2), data = data_rsm1)
summary(model1)

# =============================================================================
# Model 2: SQ and LQ sensitivity vs Performance under LQ
cat("\n=== MODEL 2: SQ & LQ Sensitivity → Performance under LQ ===\n")

data_rsm2 <- coded.data(data_clean,
                        x1 ~ (SQ_sensitivity - mean(SQ_sensitivity, na.rm=TRUE)) / sd(SQ_sensitivity, na.rm=TRUE),
                        x2 ~ (LQ_sensitivity - mean(LQ_sensitivity, na.rm=TRUE)) / sd(LQ_sensitivity, na.rm=TRUE))

model2 <- rsm(Performance_under_LQ ~ SO(x1, x2), data = data_rsm2)
summary(model2)

# =============================================================================
# Model 3: AQ and TC sensitivity vs Effect on AQ
cat("\n=== MODEL 3: AQ & TC Sensitivity → Effect on AQ ===\n")

data_rsm3 <- coded.data(data_clean,
                        x1 ~ (AQ_sensitivity - mean(AQ_sensitivity, na.rm=TRUE)) / sd(AQ_sensitivity, na.rm=TRUE),
                        x2 ~ (TC_sensitivity - mean(TC_sensitivity, na.rm=TRUE)) / sd(TC_sensitivity, na.rm=TRUE))

model3 <- rsm(Effect___AQ ~ SO(x1, x2), data = data_rsm3)
summary(model3)

# =============================================================================
# STEP 4: Canonical Analysis
# =============================================================================

cat("\n=== CANONICAL ANALYSIS ===\n")

cat("\nModel 1 (Performance under AQ):\n")
canon1 <- canonical(model1)
print(canon1)

cat("\nModel 2 (Performance under LQ):\n")
canon2 <- canonical(model2)
print(canon2)

cat("\nModel 3 (Effect on AQ):\n")
canon3 <- canonical(model3)
print(canon3)

# =============================================================================
# STEP 5: Visualization - Contour Plots
# =============================================================================

# Set up plotting area
par(mfrow = c(2, 2))

# Model 1: Contour plot
contour(model1, ~ x1 + x2,
        image = TRUE,
        at = summary(model1)$canonical$xs,
        main = "Performance under AQ\nvs AQ & TC Sensitivity",
        xlab = "AQ Sensitivity (coded)",
        ylab = "TC Sensitivity (coded)",
        col.regions = viridis(100))

# Model 2: Contour plot
contour(model2, ~ x1 + x2,
        image = TRUE,
        at = summary(model2)$canonical$xs,
        main = "Performance under LQ\nvs SQ & LQ Sensitivity",
        xlab = "SQ Sensitivity (coded)",
        ylab = "LQ Sensitivity (coded)",
        col.regions = viridis(100))

# Model 3: Contour plot
contour(model3, ~ x1 + x2,
        image = TRUE,
        at = summary(model3)$canonical$xs,
        main = "Effect on AQ\nvs AQ & TC Sensitivity",
        xlab = "AQ Sensitivity (coded)",
        ylab = "TC Sensitivity (coded)",
        col.regions = viridis(100))

# =============================================================================
# STEP 6: 3D Perspective Plots
# =============================================================================

par(mfrow = c(1, 2))

persp(model1, ~ x1 + x2,
      col = rainbow(50),
      theta = 45,
      phi = 25,
      main = "3D: Performance under AQ",
      zlab = "Performance")

persp(model3, ~ x1 + x2,
      col = terrain.colors(50),
      theta = 45,
      phi = 25,
      main = "3D: Effect on AQ",
      zlab = "Effect")

# Reset plotting parameters
par(mfrow = c(1, 1))

# =============================================================================
# STEP 7: Optimization Analysis
# =============================================================================

cat("\n=== OPTIMIZATION ANALYSIS ===\n")

# Find optimal conditions for maximizing performance
cat("\nOptimal conditions for Model 1 (Performance under AQ):\n")
if(!is.na(canon1$eigen[1])) {
  if(all(canon1$eigen < 0)) {
    cat("Maximum point found at:\n")
    print(canon1$xs)
    
    # Convert back to original scale
    opt_AQ <- canon1$xs[1] * sd(data_clean$AQ_sensitivity, na.rm=TRUE) + 
      mean(data_clean$AQ_sensitivity, na.rm=TRUE)
    opt_TC <- canon1$xs[2] * sd(data_clean$TC_sensitivity, na.rm=TRUE) + 
      mean(data_clean$TC_sensitivity, na.rm=TRUE)
    
    cat("\nOriginal scale:\n")
    cat("  Optimal AQ Sensitivity:", round(opt_AQ, 2), "\n")
    cat("  Optimal TC Sensitivity:", round(opt_TC, 2), "\n")
  } else if(all(canon1$eigen > 0)) {
    cat("Minimum point found (saddle point analysis needed)\n")
  } else {
    cat("Saddle point found\n")
  }
}

# =============================================================================
# STEP 8: Model Diagnostics
# =============================================================================

cat("\n=== MODEL DIAGNOSTICS ===\n")

# Diagnostic plots for Model 1
par(mfrow = c(2, 2))
plot(model1, main = "Model 1: Performance under AQ")
par(mfrow = c(1, 1))

# =============================================================================
# STEP 9: Summary Statistics and Interpretation
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

cat("\nMean Sensitivity Levels:\n")
cat("  AQ Sensitivity:", round(mean(data_clean$AQ_sensitivity, na.rm=TRUE), 2), "\n")
cat("  TC Sensitivity:", round(mean(data_clean$TC_sensitivity, na.rm=TRUE), 2), "\n")
cat("  SQ Sensitivity:", round(mean(data_clean$SQ_sensitivity, na.rm=TRUE), 2), "\n")
cat("  LQ Sensitivity:", round(mean(data_clean$LQ_sensitivity, na.rm=TRUE), 2), "\n")

cat("\nMean Performance Levels:\n")
cat("  Under AQ:", round(mean(data_clean$Performance_under_AQ, na.rm=TRUE), 2), "\n")
cat("  Under TC:", round(mean(data_clean$Performance_under_TC, na.rm=TRUE), 2), "\n")
cat("  Under LQ:", round(mean(data_clean$Performance_under_LQ, na.rm=TRUE), 2), "\n")
cat("  Under SQ:", round(mean(data_clean$Performance_under_SQ, na.rm=TRUE), 2), "\n")

# =============================================================================
# STEP 10: Additional Analysis - Ridge Analysis
# =============================================================================

cat("\n=== RIDGE ANALYSIS ===\n")

# Ridge analysis for Model 1
cat("\nRidge analysis for maximizing Performance under AQ:\n")
ridge1 <- ridge.max(model1, dist = c(0, 0.5, 1, 1.5, 2))
print(ridge1)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\nInterpretation Guide:\n")
cat("1. Check R-squared values for model fit quality\n")
cat("2. Look at p-values to identify significant factors\n")
cat("3. Examine canonical analysis for optimal conditions\n")
cat("4. Use contour plots to visualize response surfaces\n")
cat("5. Negative eigenvalues indicate maximum points\n")
cat("6. Positive eigenvalues indicate minimum points\n")
cat("7. Mixed eigenvalues indicate saddle points\n")
