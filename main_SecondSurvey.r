#Linear regression and other alternative algorithm for data analysis 
# Written by Meqdad Hasan 
#clear all environment variable and ploting device and consul. 
#-------------------------------------------------------------------------------
cat("\014")  #clear consol from code 
while (dev.cur() > 1) dev.off()  # clear all plot device 
rm(list = ls()) # clear all environment variables

#-------------------------------------------------------------------------------
# step 0: Load necessary libraries
library(readxl)  #Load excel library
library(ggplot2)
library(reshape2)
library(plyr)
library(car)
library(multcomp)
library(lavaan)
library(semPlot)
library(psych)
library(splithalf)
library(ltm)
library(dplyr)
library(R6)
library(kSamples)
library(rsm)
library(DescTools)
library(gridExtra)
#-------------------------------------------------------------------------------
# step 00: source all required classes and files
source("regrissorClass.r")
source("ComparativeAnalyzer.r")
source("perform_kw_then_pairwise_mw.r")
source("cfTestClass.r")
cat("\014")  #clear consol from code 
#===============================================================================
# STEP 1: LOAD DATA
#===============================================================================

# # Data from first  and second datasets both merged together _Confirmatory Factor analysis score data
data_SCFs <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)
View(data_SCFs)
## Upload the original response after cleaning (the ranked) dataset of both survey after merging them out. 
data_SOrRank <- read_excel("SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx")
View(data_SOrRank)

#===============================================================================
# STEP 2: CREATE OBJECT INSTANCES
#===============================================================================
clean_data <- na.omit(data_SCFs)
# Second Object for the (Second Datasets  _SCFA) (Continuous Data)
analy_SCFA <- regrissorClass$new(clean_data)


# declare the new Object confirmatory instant object before any method 
# implementation(Second Datasets  _SCFA) (Continuous Data)
scfa_conf_Analyzer = cfTestClass$new(input=clean_data,
                                file_sep = ",", 
                                header = TRUE)

##Create new two objects  analy_SSOr  which is the regressor class instant, and  
## analy_SSOr_conf which is the cfTestClass instant
analy_SSOr = regrissorClass$new(na.omit(data_SOrRank))
analy_SSOr_conf = cfTestClass$new(input=na.omit(data_SOrRank),
                                 file_sep = ",", 
                                 header = TRUE)

#===============================================================================
# TEST: DESCRIPTIVE STATISTICS & HISTOGRAMS
#===============================================================================
###---- First continuous data summary 
analy_SCFA$calculate_summary()$print_summary()
numaric_names <- analy_SCFA$get_numeric_info()$column_names
categorical_names <- analy_SCFA$get_categorical_info()$column_names

#-------------------------------------------------------------------------------
# TEST: HISTOGRAMS — Iterate over all numeric variables
#-------------------------------------------------------------------------------
### Now self histogram implementation
for (i in numaric_names){
    analy_SCFA$plot_histogram(i)
}
#-------------------------------------------------------------------------------
# TEST: FREQUENCY TABLES — Categorical variables
#-------------------------------------------------------------------------------
lapply(analy_SCFA$get_categorical_info()$frequency_tables, print)
#===============================================================================
# TEST: SCATTER PLOTS BY GROUP — All numeric-pair × categorical combinations
#===============================================================================
analy_SCFA$list_regression_models()

for (i in c(1:(length(numaric_names)-1))){
  for (j in c((i+1):length(numaric_names))){
    for (k in categorical_names){
        analy_SCFA$plot_scatter_by_group(numaric_names[i], numaric_names[j], k)
    }
  }
}


combn(numaric_names, 2, function(pair){
    for (k in categorical_names){
      analy_SCFA$plot_scatter_by_group(pair[1], pair[2], k)
}}, simplify = FALSE)

pair_numeric_vars = combn(numaric_names, 2, simplify = FALSE)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#===============================================================================
# TEST: SIMPLE LINEAR REGRESSION — All numeric pairs
#===============================================================================
analy_SCFA$list_regression_models()

# It found from scatter plot that the correlation are found between 
# 1- productivity vs Combined Effect of Air thermal noise and light by headache and sleep (all groups)
# 2- productivity vs performance decrease under noise and light (some groups)
# 3- Productivity vs Sensitiviy (some groups)
# 4- Productivity vs Performance decrease under thermal and air (some groups)
# ==============================================================================
#create dataframe for saving the variable of regressions 
# 3. Initialize an empty data frame to store linear-regression results
lin_result_df <- data.frame(
  Var1 = character(),
  Var2 = character(),
  R_Squared = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)


# 4. Loop through each pair, fit the model, and store results
for (i in pair_numeric_vars) {
    var1 <- i[1]
    var2 <- i[2]
  
    # Fit the linear regression model (Var1 ~ Var2)
    lin_model <- analy_SCFA$fit_linear_regression(var1, var2)
    
    # Plot it
    analy_SCFA$plot_regression_model(lin_model, type = "linear")
    
    # Append results to the data frame
    lin_result_df <- rbind(lin_result_df, data.frame(
      Var1 = var1,
      Var2 = var2,
      R_Squared = lin_model$r_squared,
      P_Value = lin_model$p_values, # Assuming the second p-value is for the slope
      stringsAsFactors = FALSE
  ))
}
#          -------------------------------------------------------
cat('the following is the dataframe generated by all models of the linear regression')
print(lin_result_df)
#===============================================================================
# TEST: MULTIPLE LINEAR REGRESSION — All 3-variable combinations
#===============================================================================
pair_numeric_vars_3 = combn(numaric_names, 3, simplify = FALSE)
pair_2_catg_var = combn(categorical_names, 2, simplify = FALSE)
pair_3_catg_var =  combn(categorical_names, 3, simplify = FALSE)

mulinreg_result_df <- data.frame(
  Var1 = character(),
  Var2 = character(),
  Var3 = character(),
  sse = numeric(),  # Sum of Squared Errors
  stringsAsFactors = FALSE
)

# 4. Loop through each pair, fit the model, and store results
for (i in pair_numeric_vars_3) {
  var1 <- i[1]
  var2 <- i[2]
  var3 <- i[3]
  
  ## Fit multiple linear regression (e.g., productivity ~ c(var1, multiple predictors))
  multi_lin_model <- analy_SCFA$fit_multiple_linear_regression(var1, c(var2, var3))
  ## Plot it. 
  analy_SCFA$plot_regression_model(multi_lin_model , type = "multiple_linear")
  
  # Append results to the data frame
  mulinreg_result_df  <- rbind(mulinreg_result_df, data.frame(
      Var1 = var1,
      Var2 = var2,
      Var3 = var3,
      R_Squared = multi_lin_model$sse,
      stringsAsFactors = FALSE
    ))
}


cat('the following is the dataframe generated by all models of the linear regression')
print(mulinreg_result_df)


#===============================================================================
# TEST: COMPREHENSIVE MULTIPLE REGRESSION WITH EFFECT SIZES (η², ω²)
#       4-variable combinations — outcome ~ 3 predictors
#===============================================================================
# Simple + multiple. Change outcome and predictors. Made it once the numeric 
# and the next with different demographic predictors 

pair_4_numeric_vars <- combn(numaric_names, 4, simplify = FALSE)

# Initialize results list to store all model information
regression_results_list <- list()

# ============================================================
# LOOP: Iterate and calculate all metrics including effect sizes
# ============================================================

cat("\n", rep("=", 110), "\n", sep = "")
cat("COMPREHENSIVE MULTIPLE LINEAR REGRESSION WITH EFFECT SIZES\n")
cat(sprintf("Analyzing %d model combinations\n", length(pair_4_numeric_vars)))
cat(rep("=", 110), "\n\n", sep = "")

for (idx in seq_along(pair_4_numeric_vars)) {
  
  var_combo <- pair_4_numeric_vars[[idx]]
  outcome_var <- var_combo[1]
  predictor_vars <- var_combo[2:4]
  
  model_id <- sprintf("Model_%d", idx)
  
  cat(sprintf("\n%s. %s ~ %s + %s + %s\n", idx, outcome_var, 
              predictor_vars[1], predictor_vars[2], predictor_vars[3]))
  cat(rep("-", 110), "\n", sep = "")
  
  tryCatch({
    
    # Prepare and fit model
    model_data <- analy_SCFA$data[, c(outcome_var, predictor_vars), drop = FALSE]
    model_data_complete <- na.omit(model_data)
    
    formula_obj <- as.formula(paste(outcome_var, "~", 
                                    paste(predictor_vars, collapse = " + ")))
    lm_model <- lm(formula_obj, data = model_data_complete)
    model_summary <- summary(lm_model)
    
    # Display coefficients
    coef_table <- model_summary$coefficients
    cat("\nCoefficients:\n")
    print(coef_table, digits = 6)
    
    # Extract basic statistics
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic[1]
    f_pvalue <- pf(f_stat, model_summary$fstatistic[2], 
                   model_summary$fstatistic[3], lower.tail = FALSE)
    n_obs <- nrow(model_data_complete)
    
    # ========================================================
    # CRITICAL: Calculate effect sizes (η² and ω²)
    # ========================================================
    
    anova_table <- anova(lm_model)
    ss_residual <- anova_table["Residuals", "Sum Sq"]
    ss_total <- sum(anova_table[, "Sum Sq"])
    ss_regression <- ss_total - ss_residual
    
    # η² = SSregression / SStotal (proportion of variance explained)
    eta_squared <- ss_regression / ss_total
    
    # ω² = (SSregression - k*MSresidual) / (SStotal + MSresidual)
    # where k = number of predictors
    k_predictors <- length(predictor_vars)
    ms_residual <- anova_table["Residuals", "Mean Sq"]
    omega_squared <- max(0, (ss_regression - k_predictors * ms_residual) / 
                           (ss_total + ms_residual))
    
    # Count significant predictors
    coef_pvalues <- coef_table[-1, 4]  # Exclude intercept
    sig_count <- sum(coef_pvalues < 0.05)
    sig_predictors <- names(coef_pvalues)[coef_pvalues < 0.05]
    
    cat(sprintf("\nModel Statistics:\n"))
    cat(sprintf("  Sample Size:               %d\n", n_obs))
    cat(sprintf("  R²:                        %.6f\n", r_squared))
    cat(sprintf("  Adjusted R²:               %.6f\n", adj_r_squared))
    cat(sprintf("  F-statistic:               %.4f (p < 0.001)\n", f_stat))
    cat(sprintf("  Residual Std. Error:       %.6f\n", model_summary$sigma))
    
    cat(sprintf("\nEffect Sizes:\n"))
    cat(sprintf("  η² (Eta-squared):          %.6f  [%.2f%% of variance explained]\n", 
                eta_squared, eta_squared * 100))
    cat(sprintf("  ω² (Omega-squared):        %.6f  [conservative estimate]\n", 
                omega_squared))
    
    if (length(sig_predictors) > 0) {
      cat(sprintf("  Significant Predictors:    %s\n", 
                  paste(sig_predictors, collapse = ", ")))
    } else {
      cat(sprintf("  Significant Predictors:    None\n"))
    }
    
    # ========================================================
    # Store all results including effect sizes
    # ========================================================
    
    regression_results_list[[model_id]] <- list(
      outcome = outcome_var,
      predictors = predictor_vars,
      formula = formula_obj,
      model = lm_model,
      summary = model_summary,
      coefficients = coef_table,
      n_observations = n_obs,
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      eta_squared = eta_squared,           # ✓ EFFECT SIZE 1
      omega_squared = omega_squared,       # ✓ EFFECT SIZE 2
      f_statistic = f_stat,
      f_pvalue = f_pvalue,
      residual_se = model_summary$sigma,
      sig_predictors_count = sig_count,
      sig_predictors_names = sig_predictors
    )
    
    cat("\n")
    
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n\n", e$message))
  })
}

# ============================================================
# STEP 2: Build COMPREHENSIVE summary table WITH effect sizes
# ============================================================

cat("\n", rep("=", 110), "\n", sep = "")
cat("CONSOLIDATED SUMMARY: ALL MODELS WITH EFFECT SIZES\n")
cat(rep("=", 110), "\n\n", sep = "")

summary_table <- data.frame(
  Model = character(),
  Outcome = character(),
  Predictors = character(),
  N = integer(),
  R_Squared = numeric(),
  Adj_R_Squared = numeric(),
  Eta_Squared = numeric(),          # ✓ ADDED
  Omega_Squared = numeric(),        # ✓ ADDED
  F_Statistic = numeric(),
  F_PValue = character(),
  Sig_Predictors = integer(),
  stringsAsFactors = FALSE
)

for (model_id in names(regression_results_list)) {
  model_info <- regression_results_list[[model_id]]
  
  summary_table <- rbind(summary_table, data.frame(
    Model = model_id,
    Outcome = model_info$outcome,
    Predictors = paste(model_info$predictors, collapse = " + "),
    N = model_info$n_observations,
    R_Squared = round(model_info$r_squared, 6),
    Adj_R_Squared = round(model_info$adj_r_squared, 6),
    Eta_Squared = round(model_info$eta_squared, 6),         # ✓ EFFECT SIZE
    Omega_Squared = round(model_info$omega_squared, 6),     # ✓ EFFECT SIZE
    F_Statistic = round(model_info$f_statistic, 4),
    F_PValue = format.pval(model_info$f_pvalue, digits = 2),
    Sig_Predictors = model_info$sig_predictors_count,
    stringsAsFactors = FALSE
  ))
}

# Display full summary
print(summary_table, row.names = FALSE)

# ============================================================
# STEP 3: Ranking by EFFECT SIZE (not just R²)
# ============================================================

cat("\n\n", rep("=", 110), "\n", sep = "")
cat("TOP MODELS RANKED BY EFFECT SIZE (ω²)\n")
cat(rep("=", 110), "\n\n", sep = "")

top_by_omega <- summary_table[order(-summary_table$Omega_Squared), ]
top_by_omega <- head(top_by_omega, 5)

print(top_by_omega, row.names = FALSE)

# ============================================================
# STEP 4: Export complete results with effect sizes
# ============================================================

if (!dir.exists("regression_results")) {
  dir.create("regression_results")
}

# Write summary table to CSV
write.csv(summary_table, 
          file = "regression_results/models_summary_with_effect_sizes.csv",
          row.names = FALSE)

# Write detailed results
# sink(file = "regression_results/detailed_coefficient_tables.txt")

for (model_id in names(regression_results_list)) {
  model_info <- regression_results_list[[model_id]]
  
  cat(sprintf("\n%s: %s ~ %s\n", 
              model_id, model_info$outcome,
              paste(model_info$predictors, collapse = " + ")))
  cat(rep("=", 110), "\n\n", sep = "")
  
  cat("DESCRIPTIVE STATISTICS:\n")
  cat(sprintf("  N = %d | R² = %.6f | Adj.R² = %.6f\n",
              model_info$n_observations,
              model_info$r_squared,
              model_info$adj_r_squared))
  cat(sprintf("  η² = %.6f | ω² = %.6f\n",
              model_info$eta_squared,
              model_info$omega_squared))
  cat(sprintf("  F(%d, %d) = %.4f, p < 0.001\n",
              length(model_info$predictors),
              model_info$n_observations - length(model_info$predictors) - 1,
              model_info$f_statistic))
  
  cat("\nCOEFFICIENTS:\n")
  print(model_info$coefficients, digits = 6)
  cat("\n\n")
}

# sink()

cat("\n✓ Results exported:\n")
cat("  1. models_summary_with_effect_sizes.csv\n")
cat("  2. detailed_coefficient_tables.txt\n\n")

#===============================================================================
# TEST: FULL PIPELINE — REGRESSION WITH DEMOGRAPHICS & INTERACTIONS (ANCOVA)
#       Outcome: all numeric vars | Predictors: sig. vars | Controls: demographics
#===============================================================================
# Outcome: All numeric variables
# Predictors: All numeric variables (excluding outcome)
# Demographics: All categorical variables
# ==============================================================================

# Step 1: Extract numeric and categorical column names
numeric_vars <- analy_SCFA$numeric_cols
categorical_vars <- analy_SCFA$categorical_cols

# Write detailed results
# sink(file = "regression_results/detailed_logistic_regression_models.txt")

cat("\n=== AVAILABLE VARIABLES FOR REGRESSION ===\n")
cat("Numeric outcomes available:", paste(numeric_vars, collapse = ", "), "\n")
cat("Categorical demographics available:", paste(categorical_vars, collapse = ", "), "\n\n")

# Step 2: Initialize consolidated results data frame
comprehensive_reg_results <- data.frame(
  Outcome_Var = character(),
  Predictor_Vars = character(),
  Demographic_Vars = character(),
  Model_Type = character(),
  R_Squared = numeric(),
  Adjusted_R_Squared = numeric(),
  Omega_Squared = numeric(),
  F_Statistic = numeric(),
  P_Value = numeric(),
  N_Observations = integer(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Step 3: Initialize list to store detailed model objects
all_regression_models <- list()

# HELPER FUNCTION: Safe model extraction with validation
extract_model_metrics <- function(model_obj, model_summary) {
  
  # Validate input objects
  if (is.null(model_obj) || is.null(model_summary)) {
    return(NULL)
  }
  
  # Extract R-squared values with error handling
  r_sq <- tryCatch({
    as.numeric(model_summary$r.squared)
  }, error = function(e) {
    warning("Failed to extract R-squared")
    return(NA_real_)
  })
  
  adj_r_sq <- tryCatch({
    as.numeric(model_summary$adj.r.squared)
  }, error = function(e) {
    warning("Failed to extract adjusted R-squared")
    return(NA_real_)
  })
  
  # Extract F-statistic components safely
  f_stat <- tryCatch({
    if (!is.null(model_summary$fstatistic) && 
        length(model_summary$fstatistic) >= 3) {
      f_val <- as.numeric(model_summary$fstatistic[1])
      df1 <- as.numeric(model_summary$fstatistic[2])
      df2 <- as.numeric(model_summary$fstatistic[3])
      
      # Calculate p-value from F-distribution
      p_val <- pf(f_val, df1, df2, lower.tail = FALSE)
      
      list(f_stat = f_val, p_value = p_val)
    } else {
      warning("F-statistic not available or malformed")
      list(f_stat = NA_real_, p_value = NA_real_)
    }
  }, error = function(e) {
    warning("Error extracting F-statistic: ", e$message)
    list(f_stat = NA_real_, p_value = NA_real_)
  })
  
  # Extract number of observations directly from model frame
  n_obs <- tryCatch({
    as.integer(nrow(model.frame(model_obj)))
  }, error = function(e) {
    warning("Failed to extract n_observations from model frame")
    return(NA_integer_)
  })
  
  # Extract AIC and BIC
  aic_val <- tryCatch({
    as.numeric(AIC(model_obj))
  }, error = function(e) {
    warning("Failed to extract AIC")
    return(NA_real_)
  })
  
  bic_val <- tryCatch({
    as.numeric(BIC(model_obj))
  }, error = function(e) {
    warning("Failed to extract BIC")
    return(NA_real_)
  })
  
  # Return safely constructed list
  return(list(
    r_squared = r_sq,
    adj_r_squared = adj_r_sq,
    f_statistic = f_stat$f_stat,
    p_value = f_stat$p_value,
    n_observations = n_obs,
    aic = aic_val,
    bic = bic_val
  ))
}

# HELPER FUNCTION: Calculate omega-squared effect size
calculate_omega_squared <- function(model_obj) {
  
  tryCatch({
    anova_tbl <- anova(model_obj)
    
    if (nrow(anova_tbl) < 2) {
      warning("Insufficient ANOVA table rows for omega-squared calculation")
      return(NA_real_)
    }
    
    # SS_total = sum of all sum of squares
    ss_total <- sum(anova_tbl[, "Sum Sq"], na.rm = TRUE)
    
    # SS_regression = sum of all except residual row
    ss_regression <- sum(anova_tbl[-nrow(anova_tbl), "Sum Sq"], na.rm = TRUE)
    
    # MS_residual = residual sum of squares / residual degrees of freedom
    residual_row <- nrow(anova_tbl)
    ms_residual <- anova_tbl[residual_row, "Sum Sq"] / anova_tbl[residual_row, "Df"]
    
    # Number of predictors (all rows except residual)
    n_predictors <- nrow(anova_tbl) - 1
    
    # ω² = (SS_reg - k*MS_res) / (SS_total + MS_res)
    omega_sq <- (ss_regression - n_predictors * ms_residual) / (ss_total + ms_residual)
    
    # Ensure non-negative value
    return(max(0, as.numeric(omega_sq)))
    
  }, error = function(e) {
    warning("Error calculating omega-squared: ", e$message)
    return(NA_real_)
  })
}

# ==============================================================================
# MAIN ITERATION LOOP (OPTIMIZED)
# ------------------------------------------------------------------------------
# Optimizations applied based on prior analysis (top_by_omega / sig_predictors):
#   1. Predictor candidates restricted to sig_predictors (3 vars identified as
#      consistently significant), reducing predictor combos from 10 → ≤3 per outcome.
#   2. Demographic combinations limited to size 0 (none) and size 1 (individual),
#      reducing demo combos from 834 → 18 per predictor combo.
# Combined reduction: ~50,040 → ~216 model fits (~230× faster).
# ==============================================================================

for (outcome in numeric_vars) {
  
  # Restrict predictors to previously identified significant variables only,
  # excluding the current outcome variable if it appears among them.
  predictor_candidates <- setdiff(sig_predictors, outcome)
  
  if (length(predictor_candidates) < 2) {
    cat("Skipping outcome '", outcome, "': Insufficient sig. predictors after exclusion\n")
    next
  }
  
  # Generate all 2-predictor combinations from significant predictors only
  predictor_combos <- combn(predictor_candidates, 2, simplify = FALSE)
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("OUTCOME VARIABLE: ", outcome, " | Combinations: ", 
      length(predictor_combos), "\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  for (pred_idx in seq_along(predictor_combos)) {
    
    predictors <- predictor_combos[[pred_idx]]
    predictor_label <- paste(predictors, collapse = " + ")
    
    # Demographic combinations: baseline (none) plus the single best combination
    # identified by targeted second pass — Age + How_quickly_notice_IEQ_parameter_change.
    # This eliminates all per-demographic looping while retaining the controls
    # that consistently improved model fit across all three top outcomes.
    # include_logistic = FALSE: outcomes are continuous; logistic regression is
    # not appropriate here and was the main per-fit cost driver.
    all_demo_combos <- list(
      character(0),                                                   # baseline: no controls
      c("Age", "How_quickly_notice_IEQ_parameter_change")             # fixed best demographic block
    )
    
    for (demo_idx in seq_along(all_demo_combos)) {
      
      demographics <- all_demo_combos[[demo_idx]]
      demographic_label <- if (length(demographics) == 0) 
        "None" else paste(demographics, collapse = " + ")
      
      # Execute regression analysis with robust error handling
      model_results <- tryCatch({
        
        analy_SCFA$perform_comprehensive_regression_analysis(
          outcome_var = outcome,
          predictors = predictors,
          demographic_predictors = if (length(demographics) > 0) demographics else NULL,
          interaction_terms = TRUE,
          include_logistic = FALSE   # continuous outcomes; logistic not applicable
        )
        
      }, error = function(e) {
        cat("  ⚠ Regression Error (", demographic_label, "): ", 
            e$message, "\n", sep = "")
        return(NULL)
      })
      
      # Process successful regression models.
      # When demographics are present, extract from the demographic (ANCOVA) model
      # so that stored metrics reflect the full model including covariate adjustment.
      # Fall back to the base multiple model if the demographic model is unavailable.
      if (!is.null(model_results) && !is.null(model_results$multiple)) {
        
        use_demo_model <- length(demographics) > 0 && !is.null(model_results$demographic)
        
        model_obj     <- if (use_demo_model) model_results$demographic$model
                         else                model_results$multiple$model
        model_type    <- if (use_demo_model) "ANCOVA (demographics controlled)"
                         else                "Multiple Linear Regression"
        model_summary <- summary(model_obj)
        
        # Extract all metrics using safe helper function
        metrics <- extract_model_metrics(model_obj, model_summary)
        
        if (!is.null(metrics)) {
          
          # Calculate omega-squared
          omega_sq <- calculate_omega_squared(model_obj)
          
          # Construct results row with validated data
          results_row <- data.frame(
            Outcome_Var        = outcome,
            Predictor_Vars     = predictor_label,
            Demographic_Vars   = demographic_label,
            Model_Type         = model_type,
            R_Squared          = metrics$r_squared,
            Adjusted_R_Squared = metrics$adj_r_squared,
            Omega_Squared      = omega_sq,
            F_Statistic        = metrics$f_statistic,
            P_Value            = metrics$p_value,
            N_Observations     = metrics$n_observations,
            AIC                = metrics$aic,
            BIC                = metrics$bic,
            stringsAsFactors   = FALSE
          )
          
          # Append to consolidated results
          comprehensive_reg_results <- rbind(comprehensive_reg_results, results_row)
          
          # Store model object
          model_key <- paste(outcome, "|", predictor_label, "|", 
                             demographic_label, sep = "")
          all_regression_models[[model_key]] <- list(
            model   = model_obj,
            summary = model_summary,
            metrics = metrics
          )
          
          cat("  ✓ [", demographic_label, "] ω²=", round(omega_sq, 4),
              " | R²=", round(metrics$r_squared, 4),
              " | AdjR²=", round(metrics$adj_r_squared, 4),
              " | n=", metrics$n_observations, "\n", sep = "")
        }
      }
    }
  }
}

# ==============================================================================
# POST-PROCESSING AND RESULTS EXPORT
# ==============================================================================

cat("\n\n", rep("=", 80), "\n", sep = "")
cat("CONSOLIDATED")
cat(rep("=", 80), "\n\n", sep = "")

# Validate results data frame
if (nrow(comprehensive_reg_results) > 0) {
  
  # Sort by omega-squared (effect size)
  comprehensive_reg_results_sorted <- comprehensive_reg_results[
    order(-comprehensive_reg_results$Omega_Squared, na.last = TRUE), 
  ]
  
  print(head(comprehensive_reg_results_sorted, 15))
  
  # Export to CSV
  write.csv(
    comprehensive_reg_results_sorted,
    file = "comprehensive_regression_results.csv",
    row.names = FALSE,
    na = ""
  )
  
  cat("\n✓ Results exported: comprehensive_regression_results.csv\n")
  cat("Total valid models:", nrow(comprehensive_reg_results), "\n")
  cat("Models with ω² > 0.10:", 
      sum(comprehensive_reg_results_sorted$Omega_Squared > 0.10, na.rm = TRUE), "\n")
  
} else {
  
  cat("\n⚠ WARNING: No valid regression results were generated.\n")
  cat("Please verify data structure and variable availability.\n")
}

# Display top models
if (exists("comprehensive_reg_results_sorted") && nrow(comprehensive_reg_results_sorted) > 0) {
  cat("\n=== TOP MODELS BY EFFECT SIZE (ω²) ===\n")
  top_10 <- head(comprehensive_reg_results_sorted, 10)
  print(top_10[, c("Outcome_Var", "Predictor_Vars", "Demographic_Vars", 
                   "Omega_Squared", "R_Squared", "P_Value", "N_Observations")])
}
# sink()

#===============================================================================
# TEST: AUTOMATED INDEPENDENT T-TESTS WITH COHEN'S d EFFECT SIZES
#       All numeric × 2-level categorical combinations
#===============================================================================

# Step 1: Create output directory if it doesn't exist
output_dir <- "./regression_result"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Output directory created:", output_dir, "\n")
}

#==============================================================================
# Step 2: Initialize comprehensive results data frame
#==============================================================================
ttest_results <- data.frame(
  Test_ID = character(),
  Outcome_Variable = character(),
  Grouping_Variable = character(),
  Test_Type = character(),
  Sample_Size_Total = integer(),
  Sample_Size_Group1 = integer(),
  Sample_Size_Group2 = integer(),
  Mean_Group1 = numeric(),
  Mean_Group2 = numeric(),
  SD_Group1 = numeric(),
  SD_Group2 = numeric(),
  T_Statistic = numeric(),
  Degrees_of_Freedom = numeric(),
  P_Value = numeric(),
  Cohen_d = numeric(),
  Effect_Size_Interpretation = character(),
  Confidence_Interval_Lower = numeric(),
  Confidence_Interval_Upper = numeric(),
  Significance_Level = character(),
  stringsAsFactors = FALSE
)

#==============================================================================
# Step 3: Helper function to calculate Cohen's d
#==============================================================================
calculate_cohens_d <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  
  # Cohen's d
  cohens_d <- abs(mean1 - mean2) / pooled_sd
  return(cohens_d)
}

#==============================================================================
# Helper function to interpret Cohen's d
#==============================================================================
interpret_cohens_d <- function(d) {
  if (is.na(d)) return("N/A")
  if (d < 0.2) return("Negligible")
  else if (d < 0.5) return("Small")
  else if (d < 0.8) return("Medium")
  else if (d < 1.2) return("Large")
  else return("Very Large")
}

#==============================================================================
# Helper function to determine significance level
#==============================================================================
determine_significance <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else return("ns")
}

#==============================================================================
# Step 4: Extract numeric and categorical variable names
#==============================================================================
numeric_vars <- analy_SCFA$numeric_cols
categorical_vars <- analy_SCFA$categorical_cols

cat("\n=== T-TEST AUTOMATION INITIATED ===\n")
cat("Numeric variables found:", length(numeric_vars), "\n")
cat("Categorical variables found:", length(categorical_vars), "\n")
cat("Total combination tests to perform:", length(numeric_vars) * length(categorical_vars), "\n\n")

#==============================================================================
# Step 5: Main iteration loop - Independent t-tests
#==============================================================================
test_counter <- 0

for (outcome_var in numeric_vars) {
  for (group_var in categorical_vars) {
    
    # Check if grouping variable has exactly 2 levels (for t-test)
    unique_groups <- unique(na.omit(analy_SCFA$data[[group_var]]))
    
    if (length(unique_groups) == 2) {
      
      test_counter <- test_counter + 1
      
      cat("Test", test_counter, ": Comparing", outcome_var, "by", group_var, "\n")
      
      # Perform t-test
      ttest_result <- tryCatch({
        analy_SCFA$perform_t_test(
          outcome = outcome_var,
          group_column = group_var,
          type = "independent",
          plot = FALSE  # Suppress plots in batch mode
        )
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
        return(NULL)
      })
      
      # Extract results if successful
      if (!is.null(ttest_result)) {
        
        # Get raw data for detailed statistics
        group1_name <- unique_groups[1]
        group2_name <- unique_groups[2]
        
        group1_data <- na.omit(analy_SCFA$data[[outcome_var]][analy_SCFA$data[[group_var]] == group1_name])
        group2_data <- na.omit(analy_SCFA$data[[outcome_var]][analy_SCFA$data[[group_var]] == group2_name])
        
        # Calculate statistics
        n1 <- length(group1_data)
        n2 <- length(group2_data)
        n_total <- n1 + n2
        
        mean1 <- mean(group1_data, na.rm = TRUE)
        mean2 <- mean(group2_data, na.rm = TRUE)
        
        sd1 <- sd(group1_data, na.rm = TRUE)
        sd2 <- sd(group2_data, na.rm = TRUE)
        
        # Calculate Cohen's d
        cohens_d_val <- calculate_cohens_d(mean1, mean2, sd1, sd2, n1, n2)
        effect_interpretation <- interpret_cohens_d(cohens_d_val)
        
        # Determine significance
        sig_level <- determine_significance(ttest_result$p_value)
        
        # Extract CI bounds (from t-test result)
        ci_lower <- ttest_result$confidence_interval[1]
        ci_upper <- ttest_result$confidence_interval[2]
        
        # Append to results data frame
        ttest_results <- rbind(ttest_results, data.frame(
          Test_ID = paste0("T", sprintf("%03d", test_counter)),
          Outcome_Variable = outcome_var,
          Grouping_Variable = group_var,
          Test_Type = "Independent t-test",
          Sample_Size_Total = n_total,
          Sample_Size_Group1 = n1,
          Sample_Size_Group2 = n2,
          Mean_Group1 = round(mean1, 4),
          Mean_Group2 = round(mean2, 4),
          SD_Group1 = round(sd1, 4),
          SD_Group2 = round(sd2, 4),
          T_Statistic = round(as.numeric(ttest_result$t_statistic), 4),
          Degrees_of_Freedom = round(n1 + n2 - 2),
          P_Value = round(ttest_result$p_value, 6),
          Cohen_d = round(cohens_d_val, 4),
          Effect_Size_Interpretation = effect_interpretation,
          Confidence_Interval_Lower = round(ci_lower, 4),
          Confidence_Interval_Upper = round(ci_upper, 4),
          Significance_Level = sig_level,
          stringsAsFactors = FALSE
        ))
        
        cat("  ✓ Completed | t =", 
            round(as.numeric(ttest_result$t_statistic), 3),
            "| p =", 
            round(ttest_result$p_value, 4),
            "| d =",
            round(cohens_d_val, 3), "\n")
      }
      
    } else if (length(unique_groups) > 2) {
      cat("  Note: Skipped (", group_var, "has", length(unique_groups), "groups - requires ANOVA)\n")
    } else {
      cat("  Note: Skipped (insufficient groups for t-test)\n")
    }
  }
}

#==============================================================================
# Step 6: Export results to CSV file
#==============================================================================
csv_filename <- file.path(output_dir, "t_test_results.csv")

write.csv(ttest_results, 
          file = csv_filename, 
          row.names = FALSE, 
          quote = TRUE)

cat("\n✓ CSV export completed:", csv_filename, "\n")

#==============================================================================
# Step 7: Create comprehensive TXT report
#==============================================================================
txt_filename <- file.path(output_dir, "t_test_results.txt")

sink(txt_filename)  # Redirect output to file

cat("================================================================================\n")
cat("COMPREHENSIVE T-TEST ANALYSIS REPORT\n")
cat("Indoor Environmental Quality & Work Performance Study\n")
cat("================================================================================\n\n")

cat("ANALYSIS METADATA\n")
cat("Date Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Data Source:", analy_SCFA$data_source, "\n")
cat("Total Sample Size:", nrow(analy_SCFA$data), "\n")
cat("Number of Numeric Variables Tested:", length(numeric_vars), "\n")
cat("Number of Grouping Variables:", length(categorical_vars), "\n")
cat("Total Tests Performed:", test_counter, "\n\n")

cat("================================================================================\n")
cat("STATISTICAL TEST SUMMARY\n")
cat("================================================================================\n\n")

# Summary statistics
significant_tests <- sum(ttest_results$P_Value < 0.05, na.rm = TRUE)
p_001_tests <- sum(ttest_results$P_Value < 0.001, na.rm = TRUE)
p_01_tests <- sum(ttest_results$P_Value < 0.01, na.rm = TRUE)
p_05_tests <- sum(ttest_results$P_Value < 0.05, na.rm = TRUE)

cat("Significant Tests (p < 0.05):", significant_tests, "/", test_counter, "\n")
cat("Highly Significant (p < 0.01):", p_01_tests, "\n")
cat("Very Highly Significant (p < 0.001):", p_001_tests, "\n")

# Effect size distribution
effect_counts <- table(ttest_results$Effect_Size_Interpretation)
cat("\nEffect Size Distribution:\n")
for (effect in names(effect_counts)) {
  cat(sprintf("  %s: %d tests\n", effect, effect_counts[effect]))
}

cat("\n================================================================================\n")
cat("DETAILED RESULTS BY OUTCOME VARIABLE\n")
cat("================================================================================\n\n")

# Sort and display by outcome variable
for (outcome in unique(ttest_results$Outcome_Variable)) {
  subset_results <- ttest_results[ttest_results$Outcome_Variable == outcome, ]
  
  cat("─── OUTCOME VARIABLE:", outcome, "───\n\n")
  
  for (i in 1:nrow(subset_results)) {
    row <- subset_results[i, ]
    
    cat(sprintf("Test ID: %s\n", row$Test_ID))
    cat(sprintf("Grouping Variable: %s\n", row$Grouping_Variable))
    cat(sprintf("Sample Sizes: Total = %d | %s = %d, %s = %d\n",
                row$Sample_Size_Total,
                unique_groups[1], row$Sample_Size_Group1,
                unique_groups[2], row$Sample_Size_Group2))
    cat(sprintf("Descriptives: Mean(G1) = %.4f ± %.4f | Mean(G2) = %.4f ± %.4f\n",
                row$Mean_Group1, row$SD_Group1,
                row$Mean_Group2, row$SD_Group2))
    cat(sprintf("t-Test Results: t(%.0f) = %.4f, p = %.6f %s\n",
                row$Degrees_of_Freedom,
                row$T_Statistic,
                row$P_Value,
                row$Significance_Level))
    cat(sprintf("Effect Size: Cohen's d = %.4f (%s)\n",
                row$Cohen_d,
                row$Effect_Size_Interpretation))
    cat(sprintf("95%% CI: [%.4f, %.4f]\n",
                row$Confidence_Interval_Lower,
                row$Confidence_Interval_Upper))
    cat("\n")
  }
  
  cat("═══════════════════════════════════════════════════════════\n\n")
}

cat("\n================================================================================\n")
cat("INTERPRETATION GUIDELINES\n")
cat("================================================================================\n\n")

cat("Effect Size (Cohen's d):\n")
cat("  Negligible: d < 0.2\n")
cat("  Small:      0.2 ≤ d < 0.5\n")
cat("  Medium:     0.5 ≤ d < 0.8\n")
cat("  Large:      0.8 ≤ d < 1.2\n")
cat("  Very Large: d ≥ 1.2\n\n")

cat("Statistical Significance:\n")
cat("  *** p < 0.001 (Very highly significant)\n")
cat("  **  p < 0.01  (Highly significant)\n")
cat("  *   p < 0.05  (Significant)\n")
cat("  ns  p ≥ 0.05  (Not significant)\n\n")

cat("================================================================================\n")
cat("TECHNICAL NOTES\n")
cat("================================================================================\n\n")

cat("Test Method: Welch's t-test (independent samples)\n")
cat("- Does NOT assume equal variances (more conservative)\n")
cat("- Recommended for unequal sample sizes or variances\n")
cat("- Degrees of freedom adjusted using Welch-Satterthwaite equation\n\n")

cat("Effect Size Calculation: Cohen's d (pooled)\n")
cat("  d = |M1 - M2| / SDpooled\n")
cat("  Where SDpooled = sqrt[((n1-1)*SD1² + (n2-1)*SD2²) / (n1+n2-2)]\n\n")

cat("Confidence Intervals: 95% coverage probability\n")
cat("- Computed using t-distribution with Welch-Satterthwaite df\n\n")

cat("================================================================================\n")
cat("END OF REPORT\n")
cat("================================================================================\n")

sink()  # Return output to console

cat("\n✓ TXT report generated:", txt_filename, "\n")

#==============================================================================
# Step 8: Create summary statistics table
#==============================================================================
summary_stats <- data.frame(
  Metric = c(
    "Total Tests Performed",
    "Significant Tests (p < 0.05)",
    "Highly Significant (p < 0.01)",
    "Very Highly Significant (p < 0.001)",
    "Mean Effect Size (Cohen's d)",
    "Median Effect Size",
    "Min Cohen's d",
    "Max Cohen's d"
  ),
  Value = c(
    test_counter,
    significant_tests,
    p_01_tests,
    p_001_tests,
    round(mean(ttest_results$Cohen_d, na.rm = TRUE), 4),
    round(median(ttest_results$Cohen_d, na.rm = TRUE), 4),
    round(min(ttest_results$Cohen_d, na.rm = TRUE), 4),
    round(max(ttest_results$Cohen_d, na.rm = TRUE), 4)
  ),
  stringsAsFactors = FALSE
)

summary_filename <- file.path(output_dir, "t_test_summary_stats.csv")
write.csv(summary_stats, file = summary_filename, row.names = FALSE, quote = TRUE)

cat("✓ Summary statistics exported:", summary_filename, "\n\n")

#==============================================================================
# Step 9: Display results summary in console
#==============================================================================
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("T-TEST ANALYSIS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

cat("FILES GENERATED:\n")
cat("1.", csv_filename, "\n")
cat("2.", txt_filename, "\n")
cat("3.", summary_filename, "\n\n")

cat("SUMMARY STATISTICS:\n")
print(summary_stats)

cat("\nTop 10 Significant Tests (Ranked by Effect Size):\n")
top_tests <- ttest_results[order(-ttest_results$Cohen_d), ][1:min(10, nrow(ttest_results)), ]
print(top_tests[, c("Test_ID", "Outcome_Variable", "Grouping_Variable", "P_Value", "Cohen_d", "Effect_Size_Interpretation")])

cat("\n✓ ANALYSIS WORKFLOW COMPLETED SUCCESSFULLY\n")

#===============================================================================
# TEST: KRUSKAL-WALLIS H TEST (Omnibus, ≥3 groups)
#       + POST-HOC PAIRWISE MANN-WHITNEY U (Bonferroni-corrected)
#       + ANDERSON-DARLING K-SAMPLE DISTRIBUTIONAL TESTS
#===============================================================================
# SECTION 1: INITIALIZE CONSOLIDATED RESULT STORAGE STRUCTURES
#-------------------------------------------------------------------------------

# --- Summary tables for Kruskal-Wallis H tests (omnibus tests across ≥3 groups) ---
kw_summary_df <- data.frame(
  Outcome_Variable = character(),
  Grouping_Variable = character(),
  H_Statistic = numeric(),
  Degrees_of_Freedom = numeric(),
  P_Value = numeric(),
  Significant = logical(),
  N_Groups = integer(),
  N_Observations = integer(),
  stringsAsFactors = FALSE
)

# --- Summary tables for pairwise Mann-Whitney U tests (post-hoc comparisons) ---
mw_pairwise_summary <- data.frame(
  Outcome_Variable = character(),
  Grouping_Variable = character(),
  Group_1 = character(),
  Group_2 = character(),
  U_Statistic = numeric(),
  P_Value = numeric(),
  P_Value_Adjusted = numeric(),
  Significant = logical(),
  Effect_Size_R = numeric(),
  stringsAsFactors = FALSE
)

# --- Summary tables for Anderson-Darling K-sample distributional tests ---
ad_ksample_summary <- data.frame(
  Outcome_Variable = character(),
  Grouping_Variable = character(),
  Test_Statistic = numeric(),
  P_Value = numeric(),
  Significant = logical(),
  Test_Type = character(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)

# --- Master list to store all detailed results for later access ---
kruskal_wallis_detailed_results <- list()
anderson_darling_detailed_results <- list()
mannwhitney_detailed_results <- list()

cat("\n", rep("=", 80), "\n", sep = "")
cat("INITIALIZING AUTOMATED NON-PARAMETRIC TESTING PIPELINE\n")
cat(rep("=", 80), "\n\n", sep = "")

# ==============================================================================
# SECTION 2: EXTRACT VARIABLE NAMES AND GENERATE COMBINATIONS
# ==============================================================================

# Retrieve numeric and categorical variable names from the regrissorClass object
numeric_outcomes <- analy_SCFA$numeric_cols
categorical_grouping_vars <- analy_SCFA$categorical_cols

cat("Numeric outcome variables identified:", length(numeric_outcomes), "\n")
print(numeric_outcomes)

cat("\nCategorical grouping variables identified:", length(categorical_grouping_vars), "\n")
print(categorical_grouping_vars)

# Generate all unique combinations of (outcome × grouping_variable)
outcome_grouping_combinations <- expand.grid(
  outcome = numeric_outcomes,
  grouping = categorical_grouping_vars,
  stringsAsFactors = FALSE
)

n_combinations <- nrow(outcome_grouping_combinations)
cat("\nTotal unique outcome × grouping combinations to test:", n_combinations, "\n\n")

# ==============================================================================
# SECTION 3: AUTOMATED KRUSKAL-WALLIS H TEST LOOP
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("STAGE 1: KRUSKAL-WALLIS H TESTS (Omnibus Tests Across ≥3 Groups)\n")
cat(rep("=", 80), "\n\n", sep = "")

for (idx in seq_len(nrow(outcome_grouping_combinations))) {
  outcome_var <- outcome_grouping_combinations[idx, "outcome"]
  grouping_var <- outcome_grouping_combinations[idx, "grouping"]
  
  # Verify variable exists and grouping variable has ≥3 unique levels
  if (!outcome_var %in% names(analy_SCFA$data) || !grouping_var %in% names(analy_SCFA$data)) {
    cat("WARNING: Skipping", outcome_var, "×", grouping_var, "- variable not found\n")
    next
  }
  
  n_groups <- length(unique(na.omit(analy_SCFA$data[[grouping_var]])))
  
  # Kruskal-Wallis requires ≥3 groups; skip if < 3 unique values
  if (n_groups < 3) {
    cat("  Skipping", outcome_var, "×", grouping_var, "(n_groups =", n_groups, ")\n")
    next
  }
  
  cat("Testing:", outcome_var, "across", grouping_var, 
      sprintf("(%d groups, n=%d)\n", n_groups, nrow(analy_SCFA$data)))
  
  # Execute Kruskal-Wallis test
  kw_result <- tryCatch({
    analy_SCFA$perform_kruskal_wallis(outcome = outcome_var, 
                                      group_column = grouping_var, 
                                      plot = FALSE)  # Suppress plots in loop
  }, error = function(e) {
    cat("    ERROR:", e$message, "\n")
    return(NULL)
  })
  
  # Extract and store results
  if (!is.null(kw_result)) {
    result_row <- data.frame(
      Outcome_Variable = outcome_var,
      Grouping_Variable = grouping_var,
      H_Statistic = kw_result$h_statistic,
      Degrees_of_Freedom = kw_result$degrees_of_freedom,
      P_Value = kw_result$p_value,
      Significant = kw_result$p_value < 0.05,
      N_Groups = n_groups,
      N_Observations = nrow(analy_SCFA$data),
      stringsAsFactors = FALSE
    )
    
    kw_summary_df <- rbind(kw_summary_df, result_row)
    
    # Store full result object for detailed inspection
    result_key <- paste(outcome_var, grouping_var, sep = "_X_")
    kruskal_wallis_detailed_results[[result_key]] <- kw_result
    
    # Print inline summary
    cat(sprintf("    H = %.4f, df = %d, p = %.6f %s\n", 
                kw_result$h_statistic, 
                kw_result$degrees_of_freedom,
                kw_result$p_value,
                ifelse(kw_result$p_value < 0.05, "***", "")))
  }
}

cat("\n")
cat("Kruskal-Wallis testing completed.\n")
cat("Total significant findings (p < 0.05):", sum(kw_summary_df$Significant), "\n\n")

# ==============================================================================
# SECTION 4: AUTOMATED PAIRWISE MANN-WHITNEY U TESTS (Post-Hoc)
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("STAGE 2: PAIRWISE MANN-WHITNEY U TESTS (Post-Hoc Comparisons)\n")
cat("(Applied only to Kruskal-Wallis significant results)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Bonferroni correction: adjust alpha for multiple comparisons
bonferroni_alpha <- 0.05 / nrow(kw_summary_df)
cat("Bonferroni-corrected significance threshold: α =", round(bonferroni_alpha, 6), "\n\n")

for (idx in seq_len(nrow(kw_summary_df))) {
  kw_row <- kw_summary_df[idx, ]
  
  # Only conduct pairwise tests if Kruskal-Wallis was significant
  if (!kw_row$Significant) {
    next
  }
  
  outcome_var <- kw_row$Outcome_Variable
  grouping_var <- kw_row$Grouping_Variable
  
  cat("Conducting pairwise Mann-Whitney U tests for:",
      outcome_var, "×", grouping_var, "\n")
  
  # Call the perform_kw_then_pairwise_mw() function
  pairwise_results <- tryCatch({
    perform_kw_then_pairwise_mw(analy_SCFA, 
                                outcome_var, 
                                grouping_var, 
                                plot = FALSE)
  }, error = function(e) {
    cat("    ERROR in pairwise Mann-Whitney:", e$message, "\n")
    return(NULL)
  })
  
  # Extract pairwise comparisons from results (structure depends on perform_kw_then_pairwise_mw implementation)
  if (!is.null(pairwise_results) && !is.null(pairwise_results$pairwise_comparisons)) {
    pairwise_df <- pairwise_results$pairwise_comparisons
    
    # Add outcome and grouping variable columns
    pairwise_df$Outcome_Variable <- outcome_var
    pairwise_df$Grouping_Variable <- grouping_var
    
    # Apply Bonferroni correction to p-values
    pairwise_df$P_Value_Adjusted <- p.adjust(pairwise_df$P_Value, method = "bonferroni")
    pairwise_df$Significant <- pairwise_df$P_Value_Adjusted < 0.05
    
    # Calculate effect size (rank-biserial correlation: r = 1 - (2U / (n1 * n2)))
    if ("U_Statistic" %in% names(pairwise_df) && "N1" %in% names(pairwise_df)) {
      pairwise_df$Effect_Size_R <- 1 - (2 * pairwise_df$U_Statistic) / 
        (pairwise_df$N1 * pairwise_df$N2)
    }
    
    # Append to master pairwise summary
    mw_pairwise_summary <- rbind(mw_pairwise_summary, pairwise_df)
    
    # Store detailed results
    result_key <- paste(outcome_var, grouping_var, "pairwise", sep = "_X_")
    mannwhitney_detailed_results[[result_key]] <- pairwise_results
    
    cat(sprintf("    Completed %d pairwise comparisons\n", nrow(pairwise_df)))
  }
}

cat("\n")
cat("Pairwise Mann-Whitney U testing completed.\n")
cat("Total pairwise comparisons executed:", nrow(mw_pairwise_summary), "\n")
cat("Significant pairwise findings (Bonferroni-corrected):", 
    sum(mw_pairwise_summary$Significant), "\n\n")

# ==============================================================================
# SECTION 5: AUTOMATED ANDERSON-DARLING K-SAMPLE DISTRIBUTIONAL TESTS
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("STAGE 3: ANDERSON-DARLING K-SAMPLE DISTRIBUTIONAL TESTS\n")
cat("(Assesses whether distributions are equivalent across demographic groups)\n")
cat(rep("=", 80), "\n\n", sep = "")

for (idx in seq_len(nrow(outcome_grouping_combinations))) {
  outcome_var <- outcome_grouping_combinations[idx, "outcome"]
  grouping_var <- outcome_grouping_combinations[idx, "grouping"]
  
  # Validate variables
  if (!outcome_var %in% names(analy_SCFA$data) || !grouping_var %in% names(analy_SCFA$data)) {
    next
  }
  
  n_groups <- length(unique(na.omit(analy_SCFA$data[[grouping_var]])))
  
  # Anderson-Darling K-sample test requires ≥2 groups
  if (n_groups < 2) {
    next
  }
  
  cat("Testing distributional equivalence for:", outcome_var, "by", grouping_var, "\n")
  
  # Execute Anderson-Darling K-sample test
  ad_result <- tryCatch({
    analy_SCFA$perform_levene_anderson(outcome = outcome_var,
                                       group_column = grouping_var,
                                       type = "anderson_darling_k",
                                       plot = FALSE)  # Suppress plots
  }, error = function(e) {
    cat("    ERROR:", e$message, "\n")
    return(NULL)
  })
  
  # Extract and store results
  if (!is.null(ad_result)) {
    # Interpret Anderson-Darling result
    interpretation <- ifelse(ad_result$p_value < 0.05,
                             "Distributions differ significantly across groups",
                             "No significant distributional differences detected")
    
    result_row <- data.frame(
      Outcome_Variable = outcome_var,
      Grouping_Variable = grouping_var,
      Test_Statistic = ad_result$statistic,
      P_Value = ad_result$p_value,
      Significant = ad_result$p_value < 0.05,
      Test_Type = "Anderson-Darling K-Sample",
      Interpretation = interpretation,
      stringsAsFactors = FALSE
    )
    
    ad_ksample_summary <- rbind(ad_ksample_summary, result_row)
    
    # Store full result
    result_key <- paste(outcome_var, grouping_var, "AD_K", sep = "_X_")
    anderson_darling_detailed_results[[result_key]] <- ad_result
    
    cat(sprintf("    A² = %.4f, p = %.6f %s\n", 
                ad_result$statistic,
                ad_result$p_value,
                ifelse(ad_result$p_value < 0.05, "***", "")))
  }
}

# Standalone verification: confirm kSamples::ad.test() works before the main loop
set.seed(42)
x1 <- rnorm(30); x2 <- rnorm(25, 0.5); x3 <- rnorm(20, 1)
group_data <- list(x1, x2, x3)
cat("=== Standalone Verification: ad.test(list) ===\n")
ad_verify <- kSamples::ad.test(group_data)
print(ad_verify)
cat("\n")
cat("Anderson-Darling K-sample testing completed.\n")
cat("Total significant findings:", sum(ad_ksample_summary$Significant), "\n\n")

# ==============================================================================
# SECTION 6: EXTRACT EFFECT SIZES FROM cfTestClass METHODS
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 6: EXTRACT EFFECT SIZES AND STANDARDIZED METRICS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Initialize effect size consolidation table
effect_size_summary <- data.frame(
  Outcome_Variable = character(),
  Grouping_Variable = character(),
  Test_Type = character(),
  Effect_Size_Value = numeric(),
  Effect_Size_Type = character(),
  Effect_Size_Interpretation = character(),
  stringsAsFactors = FALSE
)

# Extract Cramér's V from Chi-Square tests (if categorical outcomes were analyzed)
# This is a placeholder; adapt if chi-square tests are conducted separately
cat("Effect size extraction from test results:\n\n")

# For Mann-Whitney U results, calculate effect sizes (rank-biserial r)
if (nrow(mw_pairwise_summary) > 0 && "Effect_Size_R" %in% names(mw_pairwise_summary)) {
  for (idx in seq_len(nrow(mw_pairwise_summary))) {
    row <- mw_pairwise_summary[idx, ]
    
    if (!is.na(row$Effect_Size_R)) {
      # Interpret effect size (Cohen's conventions for rank-biserial r)
      if (abs(row$Effect_Size_R) < 0.1) {
        interpretation <- "Negligible"
      } else if (abs(row$Effect_Size_R) < 0.3) {
        interpretation <- "Small"
      } else if (abs(row$Effect_Size_R) < 0.5) {
        interpretation <- "Medium"
      } else {
        interpretation <- "Large"
      }
      
      effect_size_row <- data.frame(
        Outcome_Variable = row$Outcome_Variable,
        Grouping_Variable = row$Grouping_Variable,
        Test_Type = "Mann-Whitney U (Pairwise)",
        Effect_Size_Value = row$Effect_Size_R,
        Effect_Size_Type = "Rank-Biserial r",
        Effect_Size_Interpretation = interpretation,
        stringsAsFactors = FALSE
      )
      
      effect_size_summary <- rbind(effect_size_summary, effect_size_row)
    }
  }
}

cat("Effect sizes extracted:", nrow(effect_size_summary), "\n")
cat("Mean effect size magnitude:", round(mean(abs(effect_size_summary$Effect_Size_Value), 
                                              na.rm = TRUE), 4), "\n\n")

# ==============================================================================
# SECTION 7: CONSOLIDATE AND DISPLAY SUMMARY TABLES
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("CONSOLIDATED RESULTS: SUMMARY TABLES\n")
cat(rep("=", 80), "\n\n", sep = "")

# --- KRUSKAL-WALLIS SUMMARY TABLE ---
cat("TABLE 1: KRUSKAL-WALLIS H TESTS (Omnibus Tests)\n")
cat(rep("-", 80), "\n")

# Sort by p-value for clarity
kw_summary_sorted <- kw_summary_df[order(kw_summary_df$P_Value), ]

# Display with formatted p-values
print(kw_summary_sorted, digits = 6)
cat("\n")

# --- MANN-WHITNEY U PAIRWISE SUMMARY (Significant Results Only) ---
cat("\nTABLE 2: MANN-WHITNEY U PAIRWISE COMPARISONS (Significant Results Only, p < 0.05)\n")
cat(rep("-", 80), "\n")

significant_mw <- mw_pairwise_summary[mw_pairwise_summary$Significant, ]

if (nrow(significant_mw) > 0) {
  mw_display <- significant_mw[order(significant_mw$P_Value_Adjusted), 
                               c("Outcome_Variable", "Grouping_Variable", "Group_1", 
                                 "Group_2", "U_Statistic", "P_Value_Adjusted", 
                                 "Effect_Size_R")]
  print(mw_display, digits = 6)
} else {
  cat("No significant pairwise comparisons detected (after Bonferroni correction).\n")
}
cat("\n")

# --- ANDERSON-DARLING SUMMARY TABLE ---
cat("\nTABLE 3: ANDERSON-DARLING K-SAMPLE DISTRIBUTIONAL TESTS\n")
cat(rep("-", 80), "\n")

ad_summary_sorted <- ad_ksample_summary[order(ad_ksample_summary$P_Value), ]
print(ad_summary_sorted, digits = 6)
cat("\n")

# --- EFFECT SIZES SUMMARY TABLE ---
cat("\nTABLE 4: STANDARDIZED EFFECT SIZES\n")
cat(rep("-", 80), "\n")

if (nrow(effect_size_summary) > 0) {
  effect_sorted <- effect_size_summary[order(abs(effect_size_summary$Effect_Size_Value), 
                                             decreasing = TRUE), ]
  print(effect_sorted, digits = 6)
} else {
  cat("No standardized effect sizes computed in this iteration.\n")
}
cat("\n")

# ==============================================================================
# SECTION 8: EXPORT RESULTS TO CSV FILES FOR PUBLICATION
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("EXPORTING RESULTS TO OUTPUT FILES\n")
cat(rep("=", 80), "\n\n", sep = "")

output_dir <- "Statistical_Results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Write Kruskal-Wallis results
write.csv(kw_summary_df, 
          file.path(output_dir, "01_Kruskal_Wallis_Summary.csv"),
          row.names = FALSE)
cat("✓ Exported: Kruskal-Wallis summary to CSV\n")

# Write Mann-Whitney U pairwise results
write.csv(mw_pairwise_summary, 
          file.path(output_dir, "02_Mann_Whitney_Pairwise_Summary.csv"),
          row.names = FALSE)
cat("✓ Exported: Mann-Whitney U pairwise comparisons to CSV\n")

# Write Anderson-Darling results
write.csv(ad_ksample_summary, 
          file.path(output_dir, "03_Anderson_Darling_Summary.csv"),
          row.names = FALSE)
cat("✓ Exported: Anderson-Darling K-sample results to CSV\n")

# Write effect sizes
write.csv(effect_size_summary, 
          file.path(output_dir, "04_Effect_Sizes_Summary.csv"),
          row.names = FALSE)
cat("✓ Exported: Effect sizes to CSV\n")

cat("\n")
cat("All results exported to directory:", getwd(), "/", output_dir, "\n\n")

# ==============================================================================
# SECTION 9: OPTIONAL – GENERATE PUBLICATION-READY SUMMARY REPORT
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("GENERATING CONSOLIDATED STATISTICAL REPORT\n")
cat(rep("=", 80), "\n\n", sep = "")

report_text <- sprintf(
  "
STATISTICAL ANALYSIS REPORT: NON-PARAMETRIC TESTS
Generated: %s

EXECUTIVE SUMMARY
─────────────────────────────────────────────────────────────────────────────

Total Outcome × Grouping Variable Combinations Tested: %d

KRUSKAL-WALLIS H TESTS (Omnibus):
  • Total tests executed: %d
  • Significant findings (p < 0.05): %d (%.1f%%)
  • Mean H-statistic: %.4f

MANN-WHITNEY U PAIRWISE TESTS:
  • Total pairwise comparisons: %d
  • Significant findings (Bonferroni-corrected): %d (%.1f%%)
  • Mean effect size (rank-biserial r): %.4f

ANDERSON-DARLING K-SAMPLE TESTS:
  • Total distributional tests: %d
  • Significant differences: %d (%.1f%%)

KEY FINDINGS
─────────────────────────────────────────────────────────────────────────────

1. OUTCOMES WITH SIGNIFICANT HETEROGENEITY (Kruskal-Wallis p < 0.05):
%s

2. MOST PRONOUNCED PAIRWISE DIFFERENCES (Largest Effect Sizes):
%s

3. DISTRIBUTIONAL EQUIVALENCE STATUS (Anderson-Darling):
%s

INTERPRETATION NOTES
─────────────────────────────────────────────────────────────────────────────
- Kruskal-Wallis H tests assess whether outcome distributions differ across 
  demographic/categorical groups (omnibus test; ≥3 groups required).
- Mann-Whitney U pairwise tests follow significant Kruskal-Wallis results,
  with Bonferroni correction applied for multiple comparisons.
- Anderson-Darling K-sample tests determine whether entire distributions are
  equivalent across strata, not just means or medians.
- Rank-biserial r effect sizes are interpreted: small (0.1), medium (0.3),
  large (0.5) per Cohen conventions for non-parametric rank tests.

RECOMMENDATIONS FOR PUBLICATION
─────────────────────────────────────────────────────────────────────────────
1. Present Kruskal-Wallis results alongside effect sizes (not typically reported).
2. For significant omnibus results, emphasize pairwise comparisons with 
   Bonferroni or FDR-corrected p-values.
3. Discuss Anderson-Darling findings in context of location shifts vs. 
   shape differences in distributions.
4. Report medians and IQRs alongside statistical tests (not means for 
   non-parametric data).

─────────────────────────────────────────────────────────────────────────────
Report compiled on: %s
Data source: cfa_Second_factors_outout_data.csv (n = %d)
",
  Sys.time(),
  nrow(outcome_grouping_combinations),
  nrow(kw_summary_df),
  sum(kw_summary_df$Significant),
  100 * sum(kw_summary_df$Significant) / nrow(kw_summary_df),
  mean(kw_summary_df$H_Statistic),
  nrow(mw_pairwise_summary),
  sum(mw_pairwise_summary$Significant, na.rm = TRUE),
  ifelse(nrow(mw_pairwise_summary) > 0, 
         100 * sum(mw_pairwise_summary$Significant, na.rm = TRUE) / nrow(mw_pairwise_summary),
         0),
  mean(mw_pairwise_summary$Effect_Size_R, na.rm = TRUE),
  nrow(ad_ksample_summary),
  sum(ad_ksample_summary$Significant),
  100 * sum(ad_ksample_summary$Significant) / nrow(ad_ksample_summary),
  # Significant KW outcomes
  ifelse(sum(kw_summary_df$Significant) > 0,
         paste("  •", kw_summary_df[kw_summary_df$Significant, "Outcome_Variable"], 
               "×", kw_summary_df[kw_summary_df$Significant, "Grouping_Variable"],
               "(p =", round(kw_summary_df[kw_summary_df$Significant, "P_Value"], 6), ")",
               collapse = "\n"),
         "  (None detected)"),
  # Top effect sizes
  ifelse(nrow(effect_size_summary) > 0,
         paste("  •", 
               head(effect_size_summary$Outcome_Variable, 3),
               "vs",
               head(effect_size_summary$Grouping_Variable, 3),
               "(r =", round(head(effect_size_summary$Effect_Size_Value, 3), 4), ")",
               collapse = "\n"),
         "  (None computed)"),
  # AD interpretation
  ifelse(sum(ad_ksample_summary$Significant) > 0,
         paste("  •", ad_ksample_summary[ad_ksample_summary$Significant, "Outcome_Variable"],
               "×", ad_ksample_summary[ad_ksample_summary$Significant, "Grouping_Variable"],
               collapse = "\n"),
         "  All distributions appear equivalent"),
  Sys.time(),
  nrow(analy_SCFA$data)
)

# Write report to file
report_filename <- file.path(output_dir, "Statistical_Analysis_Report.txt")
writeLines(report_text, con = report_filename)
cat("✓ Comprehensive statistical report exported to:", report_filename, "\n\n")

# Print report to console
cat(report_text)

cat(rep("=", 80), "\n", sep = "")
cat("AUTOMATED TESTING PIPELINE COMPLETED SUCCESSFULLY\n")
cat(rep("=", 80), "\n")

#===============================================================================
# TEST: K-MEANS CLUSTER ANALYSIS — IEQ factors, compared by demographics
#===============================================================================
# k = NULL lets the elbow method choose k automatically (minimum of 3 enforced).
# k_max = 10 sets the upper bound of the elbow search range.

# Cluster all 4 IEQ factors; compare clusters by Gender
# 1. Close all active plot windows and reset the graphics engine
graphics.off()

# 2. Reset graphics margins (bottom, left, top, right) to smaller defaults
par(mar = c(2, 2, 2, 2))
# extract the name of categorical variables from the class:
categorical_vars <- scfa_conf_Analyzer$categorical_cols


# ============================================================
# MULTI-DEMOGRAPHIC CLUSTER ANALYSIS — ITERATIVE PIPELINE
# ============================================================
# Author  : Meqdad
# Purpose : Iterate perform_cluster_analysis() over multiple
#           demographic variables and accumulate results into
#           a structured, analysis-ready data frame.
# ============================================================
# --- 0. Define demographic variables to iterate over --------
demographic_vars <- categorical_vars 
# Add or remove categorical column names as required
# --- 1. Initialize the accumulator data frame ---------------
#        Typed explicitly to avoid coercion issues on rbind()

cluster_results_df <- data.frame(
  categorical_var = character(),
  optimal_k       = integer(),
  final_k         = numeric(),
  tot_withinss    = numeric(),
  betweenss       = numeric(),
  totss           = numeric(),
  explained_var   = numeric(),   # betweenss / totss ratio
  cluster_sizes   = character(), # e.g. "69 | 81 | 61"
  withinss        = character(), # serialized per-cluster WCSS
  centers         = character(), # serialized centroid matrix
  differences     = character(), # serialized differences list
  stringsAsFactors = FALSE
)


# --- 2. Helper: safely serialize complex objects to string --

serialize_obj <- function(x) {
  tryCatch(
    paste(capture.output(print(x)), collapse = " || "),
    error = function(e) NA_character_
  )
}


# --- 3. Main iteration loop ---------------------------------

for (demo_var in demographic_vars) {
  
    message("\n>>> Running cluster analysis for: ", demo_var)
  
    # -- 3a. Run analysis with error guard --------------------
     tryCatch({
      cat('Current demographic',demo_var, '\n')
       ca_result <- analy_SCFA$perform_cluster_analysis(
        variables          = numeric_outcomes,
        demographic_column = demo_var,
        k                  = NULL,
        k_max              = 10,
        plot               = TRUE
      )
    
      
      # -- 3c. Safely extract all fields from object -----------
      #        Mirrors exactly the structure shown in your screenshot
      
      opt_k       <- ca_result$optimal_k                        # integer [1]
      fin_k       <- ca_result$final_k                          # double  [1]
      tot_wss     <- ca_result$clusters$tot.withinss            # double  [1]
      bss         <- ca_result$clusters$betweenss               # double  [1]
      tss         <- ca_result$clusters$totss                   # double  [1]
      expl_var    <- round(bss / tss, 6)                        # η² ratio
      sizes_str   <- paste(ca_result$clusters$size, collapse = " | ")
      withinss_str<- paste(round(ca_result$clusters$withinss, 4), collapse = " | ")
      centers_str <- serialize_obj(ca_result$clusters$centers)  # double [k x 6]
      diff_str    <- serialize_obj(ca_result$differences)       # list   [6]
      
      
      # -- 3d. Build single-row data frame for this iteration --
      
      row_i <- data.frame(
        categorical_var  = demo_var,
        optimal_k        = opt_k,
        final_k          = fin_k,
        tot_withinss     = tot_wss,
        betweenss        = bss,
        totss            = tss,
        explained_var    = expl_var,
        cluster_sizes    = sizes_str,
        withinss         = withinss_str,
        centers          = centers_str,
        differences      = diff_str,
        stringsAsFactors = FALSE
      )
      
      
      # -- 3e. Append row to accumulator -----------------------
      
      cluster_results_df <- rbind(cluster_results_df, row_i)
      
      message("    Completed: k=", fin_k,
              " | η²=", round(expl_var * 100, 2), "%",
              " | sizes=[", sizes_str, "]")
      
      }, error = function(e) {
      warning("Cluster analysis FAILED for [", demo_var, "]: ", e$message)
      return(NULL)
    })
  
  # -- 3b. Skip if analysis returned NULL ------------------
  if (is.null(ca_result)) next
  

}


# --- 4. Final output ----------------------------------------

message("\n=== Iteration complete: ", nrow(cluster_results_df),
        " demographic variable(s) processed ===\n")

print(cluster_results_df)


# --- 5. Optional: export to CSV for reporting ---------------

write.csv(
  cluster_results_df,
  file      = "cluster_analysis_by_demographic.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

###-------------------------------------------------------------------------####
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#===============================================================================
# TEST: RESPONSE SURFACE ANALYSIS — Method 1 (string-based, simple interface)
#       perform_response_surface_analysis(response, predictors, plot)
#===============================================================================
analy_SCFA$list_all_methods()

cat("\n=== First-Order Response Surface Analysis ===\n")
# NOTE: The block below is commented out because rs_first_order_1 depends on
# the commented-out perform_response_surface_analysis() call above.
# Uncomment the assignment above first, then uncomment this block.

#===============================================================================
# TEST: RESPONSE SURFACE ANALYSIS — Method 2 (formula-based interface)
#       perform_response_surface_analysis_two(formula, model_type, ...)
#===============================================================================
#===============================================================================
# AUTOMATED RESPONSE SURFACE ANALYSIS — IEQ WORKPLACE PERFORMANCE STUDY
# Author  : Meqdad (IAU Research Pipeline)
# Purpose : Fully automated first-order and second-order RSM on all valid
#           numeric-variable pairs extracted directly from the dataset.
#           Exports summary tables to CSV and all diagnostic/contour plots to PDF.
# Version : 3.0  |  No hard-coded variable names  |  Full error isolation
#===============================================================================
# ── 1. DISCOVER RESPONSE AND PREDICTOR VARIABLES AUTOMATICALLY ───────────────
# Strategy: any numeric column with standard deviation > 0 is eligible.
# Variables that are factor-encoded integers (few unique values ≤ 5) are treated
# as potential predictors only — not as primary response variables.
## roaw data and object (#analy_SCFA  -->> current object)
#current raw data --> data_SCFs
## numaric culoms --> numeric_vars
# Remove degenerate columns (zero variance or fewer than 5 complete observations)
eligible <- Filter(function(col) {
      vals <- analy_SCFA$data[[col]]
      vals <- vals[!is.na(vals)]
      length(vals) >= 10 && sd(vals) > 1e-9
    }, numeric_vars)

# Heuristic: columns with more than 5 unique values are treated as continuous
# responses AND predictors; those with ≤ 5 unique values are predictors only.
n_unique <- sapply(eligible, function(col) length(unique(na.omit(analy_SCFA$data[[col]]))))

response_vars  <- eligible[n_unique >  5]
predictor_vars <- eligible                   # all eligible columns may serve as predictors

cat("[INFO] Candidate response variables  :", length(response_vars),  "\n")
cat("        →", paste(response_vars, collapse = ", "), "\n")
cat("[INFO] Candidate predictor variables :", length(predictor_vars), "\n")
cat("        →", paste(predictor_vars, collapse = ", "), "\n\n")

if (length(predictor_vars) < 2) stop("Need at least 2 numeric predictor variables for RSM.")
if (length(response_vars)  < 1) stop("Need at least 1 numeric response variable for RSM.")

# ── 2. GENERATE ALL PREDICTOR PAIRS ──────────────────────────────────────────
pred_pairs <- combn(predictor_vars, 2, simplify = FALSE)

# Build the full configuration grid:
# For each response variable × each predictor pair, run both orders.
configs <- expand.grid(
  resp_idx = seq_along(response_vars),
  pair_idx = seq_along(pred_pairs),
  stringsAsFactors = FALSE
)

# Remove configurations where the response is one of the predictors in that pair
# (would create a trivially perfect fit or singularity)
configs <- configs[mapply(function(ri, pi) {
  !response_vars[ri] %in% pred_pairs[[pi]]
}, configs$resp_idx, configs$pair_idx), ]

cat("[INFO] Valid RSM configurations:", nrow(configs),
    "(each will run as first-order AND second-order)\n\n")

# ── 3. OUTPUT DIRECTORY ───────────────────────────────────────────────────────
timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- paste0("RSM_Results_", timestamp)
dir.create(output_dir,                              showWarnings = FALSE)
dir.create(file.path(output_dir, "individual_pdf"), showWarnings = FALSE)

cat("[INFO] Output directory:", output_dir, "\n\n")

# ── 4. HELPER: SAFE FILE-NAME STEM ───────────────────────────────────────────
make_stem <- function(response, pred1, pred2, order_tag) {
  sanitise <- function(s) gsub("[^A-Za-z0-9]", "_", s)
  paste(sanitise(response), sanitise(pred1), sanitise(pred2), order_tag, sep = "__")
}

# ── 5. CORE RSM RUNNER ───────────────────────────────────────────────────────
# Returns a named list with all extracted statistics, or NULL on failure.
run_rsm <- function(analy_SCFA, response, pred_pair, model_type) {
  
  p1 <- pred_pair[1]
  p2 <- pred_pair[2]
  
  # Subset to complete cases for this triplet
  working_df <- na.omit(analy_SCFA$data[, c(response, p1, p2), drop = FALSE])
  
  if (nrow(working_df) < 15) {
    message(sprintf("  [SKIP] %s ~ (%s, %s) %s — only %d complete rows",
                    response, p1, p2, model_type, nrow(working_df)))
    return(NULL)
  }
  
  # Temporarily create a local analy_SCFA on the clean subset so the class method
  # receives exactly the data it needs — avoids NA propagation issues.
  local_analy_SCFA <- regrissorClass$new(working_df)
  
  rsm_result <- tryCatch(
    local_analy_SCFA$perform_response_surface_analysis_two(
      formula         = as.formula(
        if (model_type == "first_order")
          paste(response, "~ FO(", p1, ",", p2, ")")
        else
          paste(response, "~ SO(", p1, ",", p2, ")")
      ),
      data            = working_df,
      model_type      = model_type,
      plot            = FALSE,
      coded           = TRUE,
      steepest_ascent = (model_type == "first_order"),
      canonical       = (model_type == "second_order"),
      contour         = FALSE
    ),
    error = function(e) {
      message(sprintf("  [ERROR] %s ~ (%s, %s) %s — %s",
                      response, p1, p2, model_type, e$message))
      NULL
    }
  )
  
  if (is.null(rsm_result)) return(NULL)
  
  mdl     <- rsm_result$model
  mdl_sum <- summary(mdl)
  coefs   <- coef(mdl)
  
  # Intercept
  intercept_val <- coefs[grepl("Intercept", names(coefs))][1]
  
  # Extract p-values from the lm-level summary (stored inside rsm summary)
  pval_table <- tryCatch(coef(mdl_sum), error = function(e) NULL)
  
  # Steepest-ascent direction (first-order only)
  steepest_df <- NULL
  if (model_type == "first_order" && !is.null(rsm_result$steepest_ascent)) {
    steepest_df <- as.data.frame(rsm_result$steepest_ascent)
  }
  
  # Stationary point and eigenvalues (second-order only)
  stat_pt    <- NULL
  eigenvals  <- NULL
  surface_type <- NA_character_
  if (model_type == "second_order") {
    if (!is.null(rsm_result$stationary_point))
      stat_pt <- rsm_result$stationary_point
    
    if (!is.null(rsm_result$canonical_analysis)) {
      eigenvals <- rsm_result$canonical_analysis$eigen.val
      if (!is.null(eigenvals)) {
        if (all(eigenvals < 0))      surface_type <- "Maximum"
        else if (all(eigenvals > 0)) surface_type <- "Minimum"
        else                         surface_type <- "Saddle point"
      }
    }
  }
  
  list(
    response         = response,
    pred1            = p1,
    pred2            = p2,
    model_type       = model_type,
    stem             = make_stem(response, p1, p2, model_type),
    model            = mdl,
    model_summary    = mdl_sum,
    r_squared        = mdl_sum$r.squared,
    adj_r_squared    = mdl_sum$adj.r.squared,
    coefficients     = coefs,
    intercept        = as.numeric(intercept_val),
    pval_table       = pval_table,
    n_obs            = nrow(working_df),
    working_df       = working_df,
    steepest         = steepest_df,
    stationary_point = stat_pt,
    eigenvalues      = eigenvals,
    surface_type     = surface_type,
    predictions      = rsm_result$predictions
  )
}

# ── 6. RUN ALL CONFIGURATIONS ─────────────────────────────────────────────────
cat("=== RUNNING ALL RSM CONFIGURATIONS ===\n\n")

all_results       <- list()
first_order_res   <- list()
second_order_res  <- list()

for (i in seq_len(nrow(configs))) {
  resp  <- response_vars[ configs$resp_idx[i] ]
  ppair <- pred_pairs[[   configs$pair_idx[i] ]]
  
  for (mtype in c("first_order", "second_order")) {
    label <- sprintf("%s ~ (%s, %s) [%s]", resp, ppair[1], ppair[2], mtype)
    cat(sprintf("  [%d/%d] %s\n",
                (i - 1) * 2 + ifelse(mtype == "first_order", 1, 2),
                nrow(configs) * 2,
                label))
    
    res <- run_rsm(analy_SCFA, resp, ppair, mtype)
    
    if (!is.null(res)) {
      all_results[[ res$stem ]]      <- res
      if (mtype == "first_order")
        first_order_res[[ res$stem ]] <- res
      else
        second_order_res[[ res$stem ]] <- res
    }
  }
}

cat(sprintf("\n[DONE] Successful models: %d (first-order: %d | second-order: %d)\n\n",
            length(all_results),
            length(first_order_res),
            length(second_order_res)))

# ── 7. BUILD SUMMARY DATA FRAME ───────────────────────────────────────────────
cat("=== EXPORTING CSV RESULTS ===\n\n")

build_summary_row <- function(res) {
  coef_names <- names(res$coefficients)
  non_int    <- res$coefficients[ !grepl("Intercept", coef_names) ]
  
  # Extract F-statistic and overall p-value from the model summary
  fstat   <- tryCatch(res$model_summary$fstatistic[1], error = function(e) NA_real_)
  df1     <- tryCatch(res$model_summary$fstatistic[2], error = function(e) NA_real_)
  df2     <- tryCatch(res$model_summary$fstatistic[3], error = function(e) NA_real_)
  overall_p <- tryCatch(pf(fstat, df1, df2, lower.tail = FALSE), error = function(e) NA_real_)
  
  data.frame(
    Stem           = res$stem,
    Response       = res$response,
    Predictor_1    = res$pred1,
    Predictor_2    = res$pred2,
    Model_Type     = res$model_type,
    N_Observations = res$n_obs,
    R_Squared      = round(res$r_squared,     4),
    Adj_R_Squared  = round(res$adj_r_squared, 4),
    F_Statistic    = round(fstat,             4),
    Overall_p      = round(overall_p,         6),
    Intercept      = round(res$intercept,     4),
    N_Coefficients = length(non_int),
    Surface_Type   = ifelse(is.na(res$surface_type), "N/A", res$surface_type),
    stringsAsFactors = FALSE
  )
}

summary_df <- do.call(rbind, lapply(all_results, build_summary_row))
rownames(summary_df) <- NULL

# Save main summary
csv_summary <- file.path(output_dir, "RSM_Summary.csv")
write.csv(summary_df, csv_summary, row.names = FALSE)
cat("[CSV] Summary table →", csv_summary, "\n")

# ── 7a. DETAILED COEFFICIENTS CSV ─────────────────────────────────────────────
coef_rows <- do.call(rbind, lapply(names(all_results), function(nm) {
  res  <- all_results[[nm]]
  ptbl <- res$pval_table
  cnames <- names(res$coefficients)
  df <- data.frame(
    Stem       = nm,
    Response   = res$response,
    Model_Type = res$model_type,
    Term       = cnames,
    Estimate   = round(res$coefficients, 5),
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  # Append SE, t-value, p-value if available
  if (!is.null(ptbl)) {
    matched <- match(cnames, rownames(ptbl))
    df$Std_Error <- ifelse(!is.na(matched), round(ptbl[matched, 2], 5), NA)
    df$t_value   <- ifelse(!is.na(matched), round(ptbl[matched, 3], 5), NA)
    df$p_value   <- ifelse(!is.na(matched), round(ptbl[matched, 4], 6), NA)
    df$Sig       <- ifelse(!is.na(matched),
                           ifelse(ptbl[matched, 4] < 0.001, "***",
                                  ifelse(ptbl[matched, 4] < 0.01,  "**",
                                         ifelse(ptbl[matched, 4] < 0.05,  "*",
                                                ifelse(ptbl[matched, 4] < 0.1,   ".",  "")))),
                           NA)
  }
  df
}))
csv_coefs <- file.path(output_dir, "RSM_Coefficients.csv")
write.csv(coef_rows, csv_coefs, row.names = FALSE)
cat("[CSV] Coefficients table →", csv_coefs, "\n")

# ── 7b. STATIONARY POINTS CSV (second-order only) ────────────────────────────
stat_rows <- do.call(rbind, lapply(names(second_order_res), function(nm) {
  res <- second_order_res[[nm]]
  if (is.null(res$stationary_point)) return(NULL)
  sp  <- res$stationary_point
  data.frame(
    Stem         = nm,
    Response     = res$response,
    Predictor_1  = res$pred1,
    Predictor_2  = res$pred2,
    Surface_Type = res$surface_type,
    SP_Pred1     = round(sp[res$pred1], 5),
    SP_Pred2     = round(sp[res$pred2], 5),
    Eigenvalue_1 = if (!is.null(res$eigenvalues)) round(res$eigenvalues[1], 5) else NA,
    Eigenvalue_2 = if (!is.null(res$eigenvalues)) round(res$eigenvalues[2], 5) else NA,
    stringsAsFactors = FALSE, row.names = NULL
  )
}))
if (!is.null(stat_rows) && nrow(stat_rows) > 0) {
  csv_stat <- file.path(output_dir, "RSM_Stationary_Points.csv")
  write.csv(stat_rows, csv_stat, row.names = FALSE)
  cat("[CSV] Stationary points →", csv_stat, "\n")
}

# ── 7c. STEEPEST-ASCENT CSV (first-order only) ────────────────────────────────
# Use dplyr::bind_rows() instead of do.call(rbind, ...) because each
# steepest-ascent path table carries different predictor columns depending
# on the formula used; bind_rows() fills missing columns with NA automatically.

steep_list <- lapply(names(first_order_res), function(nm) {
  res <- first_order_res[[nm]]
  
  # Guard: skip if steepest is absent or not a data frame
  if (is.null(res$steepest) || !is.data.frame(res$steepest) || nrow(res$steepest) == 0) {
    return(NULL)
  }
  
  df            <- res$steepest
  df$Stem       <- nm
  df$Response   <- res$response   # tag which response this path belongs to
  return(df)
})

# Remove NULLs before binding
steep_list <- Filter(Negate(is.null), steep_list)

if (length(steep_list) > 0) {
  # bind_rows fills mismatched columns with NA — safe across all formula variants
  steep_rows <- dplyr::bind_rows(steep_list)
  
  csv_steep <- file.path(output_dir, "RSM_Steepest_Ascent.csv")
  write.csv(steep_rows, csv_steep, row.names = FALSE)
  cat("[CSV] Steepest ascent →", csv_steep, "\n")
} else {
  cat("[INFO] No steepest-ascent results available to export.\n")
}

# ── 7d. BEST-MODEL RANKING CSV ────────────────────────────────────────────────
best_models <- summary_df %>%
  dplyr::group_by(Response, Model_Type) %>%
  dplyr::slice_max(order_by = R_Squared, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(R_Squared))

csv_best <- file.path(output_dir, "RSM_Best_Models.csv")
write.csv(best_models, csv_best, row.names = FALSE)
cat("[CSV] Best models →", csv_best, "\n\n")

# ── 8. DIAGNOSTIC PLOT HELPER ────────────────────────────────────────────────
plot_diagnostics <- function(res, add_title = TRUE) {
  mdl   <- res$model
  resids <- residuals(mdl)
  fitted_vals <- fitted(mdl)
  title_str <- sprintf("%s\n%s ~ (%s, %s)  |  R² = %.4f",
                       toupper(gsub("_", " ", res$model_type)),
                       res$response, res$pred1, res$pred2,
                       res$r_squared)
  
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), oma = c(0, 0, 3, 0))
  
  # Panel 1: Residuals vs Fitted
  plot(fitted_vals, resids,
       main = "Residuals vs Fitted",
       xlab = "Fitted Values", ylab = "Residuals",
       col = "#2E86AB", pch = 19, cex = 0.75)
  abline(h = 0, col = "#E84855", lwd = 2, lty = 2)
  lines(lowess(fitted_vals, resids), col = "#F9A620", lwd = 2)
  grid(lty = 3, col = "grey85")
  
  # Panel 2: Normal Q-Q
  qqnorm(resids, main = "Normal Q-Q Plot",
         col = "#2E86AB", pch = 19, cex = 0.75)
  qqline(resids, col = "#E84855", lwd = 2)
  grid(lty = 3, col = "grey85")
  
  # Panel 3: Scale-Location
  plot(fitted_vals, sqrt(abs(resids)),
       main = "Scale-Location",
       xlab = "Fitted Values", ylab = expression(sqrt("|Residuals|")),
       col = "#2E86AB", pch = 19, cex = 0.75)
  lines(lowess(fitted_vals, sqrt(abs(resids))), col = "#F9A620", lwd = 2)
  grid(lty = 3, col = "grey85")
  
  # Panel 4: Histogram of residuals
  hist(resids,
       main = "Residual Distribution",
       xlab = "Residuals",
       col  = "#2E86AB", border = "white", breaks = 15)
  curve(dnorm(x, mean(resids), sd(resids)) * length(resids) * diff(range(resids)) / 15,
        add = TRUE, col = "#E84855", lwd = 2)
  grid(lty = 3, col = "grey85")
  
  if (add_title)
    mtext(title_str, outer = TRUE, cex = 0.85, font = 2, line = 1)
  
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
}

# ── 9. CONTOUR PLOT HELPER (second-order) ────────────────────────────────────
plot_contour <- function(res) {
  mdl <- res$model
  p1  <- res$pred1
  p2  <- res$pred2
  df  <- res$working_df
  
  x_seq <- seq(min(df[[p1]], na.rm = TRUE), max(df[[p1]], na.rm = TRUE), length.out = 50)
  y_seq <- seq(min(df[[p2]], na.rm = TRUE), max(df[[p2]], na.rm = TRUE), length.out = 50)
  grid  <- expand.grid(x_seq, y_seq)
  names(grid) <- c(p1, p2)
  
  z <- tryCatch(predict(mdl, newdata = grid), error = function(e) NULL)
  if (is.null(z)) return(invisible(NULL))
  
  z_mat <- matrix(z, nrow = 50, ncol = 50)
  
  par(mar = c(5, 5, 4, 5))
  filled.contour(
    x      = x_seq, y = y_seq, z = z_mat,
    color.palette = function(n) colorRampPalette(c("#053061","#2166AC","#4393C3",
                                                   "#92C5DE","#F7F7F7","#FDDBC7",
                                                   "#F4A582","#D6604D","#B2182B"))(n),
    nlevels = 20,
    xlab    = p1, ylab = p2,
    main    = sprintf("Response Surface Contour\n%s ~ SO(%s, %s)  |  R² = %.4f",
                      res$response, p1, p2, res$r_squared),
    key.title = title(main = res$response, cex.main = 0.8),
    plot.axes = {
      axis(1); axis(2)
      # Overlay observed data points
      points(df[[p1]], df[[p2]],
             pch = 21, bg = "white", col = "grey30", cex = 0.6)
      # Overlay stationary point if available
      if (!is.null(res$stationary_point)) {
        sp <- res$stationary_point
        if (p1 %in% names(sp) && p2 %in% names(sp)) {
          points(sp[[p1]], sp[[p2]],
                 pch = 23, bg = "#FFD700", col = "black", cex = 1.8, lwd = 2)
          text(sp[[p1]], sp[[p2]],
               labels = paste0("SP\n(", res$surface_type, ")"),
               pos = 3, cex = 0.65, col = "black")
        }
      }
      grid(lty = 3, col = "grey85")
    }
  )
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}

# ── 10. COMBINED PDF — ALL MODELS ─────────────────────────────────────────────
cat("=== GENERATING PDF PLOTS ===\n\n")

pdf_combined <- file.path(output_dir, "RSM_All_Diagnostic_Plots.pdf")
pdf(pdf_combined, width = 11, height = 8.5, onefile = TRUE)

for (nm in names(all_results)) {
  res <- all_results[[nm]]
  cat(sprintf("  [PLOT] %s\n", nm))
  tryCatch(
    plot_diagnostics(res, add_title = TRUE),
    error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Diagnostic plot failed for:\n", nm, "\n\n", e$message),
           cex = 0.9, col = "red")
    }
  )
}

dev.off()
cat("[PDF] Combined diagnostics →", pdf_combined, "\n")

# ── 11. CONTOUR PDF — SECOND-ORDER ONLY ───────────────────────────────────────
pdf_contour <- file.path(output_dir, "RSM_Contour_Plots.pdf")
pdf(pdf_contour, width = 8.5, height = 7, onefile = TRUE)

for (nm in names(second_order_res)) {
  res <- second_order_res[[nm]]
  cat(sprintf("  [CONTOUR] %s\n", nm))
  tryCatch(
    plot_contour(res),
    error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Contour plot failed for:\n", nm, "\n\n", e$message),
           cex = 0.9, col = "red")
    }
  )
}

dev.off()
cat("[PDF] Contour plots →", pdf_contour, "\n\n")

# ── 12. INDIVIDUAL PDFs PER MODEL ─────────────────────────────────────────────
ind_dir <- file.path(output_dir, "individual_pdf")

for (nm in names(all_results)) {
  res      <- all_results[[nm]]
  ind_file <- file.path(ind_dir, paste0(nm, ".pdf"))
  
  tryCatch({
    pdf(ind_file, width = 11, height = 8.5)
    plot_diagnostics(res, add_title = TRUE)
    if (res$model_type == "second_order")
      plot_contour(res)
    dev.off()
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    message(sprintf("  [WARN] Could not save individual PDF for %s: %s", nm, e$message))
  })
}
cat("[PDF] Individual model PDFs →", ind_dir, "\n\n")

# ── 13. SAVE COMPLETE R OBJECT ────────────────────────────────────────────────
rds_file <- file.path(output_dir, "RSM_Complete_Results.rds")
saveRDS(list(
  all_results        = all_results,
  first_order_res    = first_order_res,
  second_order_res   = second_order_res,
  summary_df         = summary_df,
  best_models        = best_models,
  predictor_pairs    = pred_pairs,
  response_vars      = response_vars,
  predictor_vars     = predictor_vars
), rds_file)
cat("[RDS] Complete R workspace →", rds_file, "\n\n")

# ── 14. CONSOLE SUMMARY REPORT ────────────────────────────────────────────────
sep_line <- paste(rep("=", 80), collapse = "")

cat(sep_line, "\n")
cat("  RESPONSE SURFACE ANALYSIS — FINAL SUMMARY REPORT\n")
cat(sep_line, "\n\n")
cat(sprintf("  Models fitted   : %d\n", length(all_results)))
cat(sprintf("    ├─ First-order : %d\n", length(first_order_res)))
cat(sprintf("    └─ Second-order: %d\n\n", length(second_order_res)))

for (mtype in c("first_order", "second_order")) {
  sub <- summary_df[summary_df$Model_Type == mtype, ]
  if (nrow(sub) == 0) next
  sub_sorted <- sub[order(-sub$R_Squared), ]
  cat(sprintf("  ── TOP 5 %s MODELS (by R²) ──\n",
              toupper(gsub("_", "-", mtype))))
  print(head(sub_sorted[, c("Response","Predictor_1","Predictor_2",
                            "R_Squared","Adj_R_Squared","Overall_p",
                            "N_Observations","Surface_Type")], 5),
        row.names = FALSE)
  cat("\n")
}
cat(sep_line, "\n")
cat("  OUTPUT FILES\n")
cat(sep_line, "\n")
cat(sep_line, "\n")
cat(sprintf("\n  All results saved to:  %s/\n\n", output_dir))

#===============================================================================
# TEST: CHI-SQUARE TEST OF INDEPENDENCE + FISHER'S EXACT TEST (fallback)
#       All categorical variable pairs — with Cramér's V effect size
#===============================================================================
cat('-------','CHI-SQUARE TEST OF INDEPENDENCE + FISHERS EXACT TEST (fallback)','----------')

chi_output_dir <- paste0("ChiSquare_Results_", timestamp)
dir.create(chi_output_dir, showWarnings = FALSE)
cat("Results will be saved to:", chi_output_dir, "\n")


# Create a list to store all results for later access
all_chi_results <- list()
# Create an empty data frame to store a summary table of p-values and effect sizes
summary_df <- data.frame(
  Variable_1 = character(),
  Variable_2 = character(),
  ChiSq_P_Value = numeric(),
  Fishers_P_Value = numeric(),
  Cramers_V = numeric(),
  Association = character(),
  stringsAsFactors = FALSE
)




# extract the name of categorical variables from the class:
categorical_vars <- scfa_conf_Analyzer$categorical_cols
print(categorical_vars)
#iterate over all variables and made chi-square test and Fisher test between them (15) iteration
for (i in c(1:(length(categorical_vars)-1))){
  for (j in c((i+1):length(categorical_vars))){
    
    #prepare data
    var_1 <- categorical_vars[i]
    var_2 <- categorical_vars[j]
    # Test independence between var1 and var2
    chi_results <- scfa_conf_Analyzer$perform_chi_square_test(var1 = var_1, 
                                                         var2 = var_2
                                                         )
    
    # Initialize Fisher's p-value as NA
    fisher_p <- NA
    
    # Check if Chi-Square assumptions were violated (expected frequencies < 5)
    # If so, run Fisher's Exact Test as a robust fallback
    if (any(chi_results$expected_frequencies < 5, na.rm = TRUE)) {
      cat("\n⚠ Expected frequencies < 5 detected. Supplementing with Fisher's Exact Test...\n")
      
      # Create the contingency table directly from the data
      cont_table <- table(data_SCFs[[var_1]], data_SCFs[[var_2]])
      
      # Run Fisher's Exact Test with Monte Carlo simulation 
      # (simulate.p.value=TRUE is required for tables larger than 2x2 to avoid computation timeouts)
      fisher_res <- fisher.test(cont_table, simulate.p.value = TRUE, B = 2000)
      fisher_p <- fisher_res$p.value
      
      cat(sprintf("Fisher's Exact Test p-value: %.6f\n", fisher_p))
    }
    # Append the Fisher's result to the chi_results object for complete storage
    chi_results$fisher_p_value <- fisher_p
    
    # Store the full results in the list
    pair_name <- paste(var_1, "vs", var_2, sep = "_")
    all_chi_results[[pair_name]] <- chi_results
    
    # Append to the summary data frame
    summary_df <- rbind(summary_df, data.frame(
      Variable_1 = var_1,
      Variable_2 = var_2,
      ChiSq_P_Value = chi_results$p_value,
      Fishers_P_Value = fisher_p,
      Cramers_V = ifelse(is.null(chi_results$effect_sizes$cramers_v), NA, chi_results$effect_sizes$cramers_v),
      Association = chi_results$effect_sizes$cramers_v_interpretation,
      stringsAsFactors = FALSE
    ))
    
  }
}
# ============================================================
# PRINT SUMMARY TABLE
# ============================================================
cat("\n\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY OF ALL CATEGORICAL ASSOCIATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Format p-values for easier reading in the console
summary_df$ChiSq_P_Value <- format.pval(summary_df$ChiSq_P_Value, digits = 4)
summary_df$Fishers_P_Value <- format.pval(summary_df$Fishers_P_Value, digits = 4)

# Replace NA Fisher's with "N/A (ChiSq OK)"
summary_df$Fishers_P_Value[is.na(summary_df$Fishers_P_Value)] <- "N/A (ChiSq OK)"

print(summary_df, row.names = FALSE)

cat("\nNote: If Fishers_P_Value is 'N/A (ChiSq OK)', the Chi-Square assumptions were met.\n")
cat("If Fishers_P_Value is present, the Chi-Square approximation was unreliable (expected counts < 5).\n")
cat("\n", rep("=", 80), "\n", sep = "")
cat("EXPORTING RESULTS\n")
cat(rep("=", 80), "\n\n", sep = "")


# 4.1 Export Summary CSV
summary_file <- file.path(chi_output_dir, "ChiSquare_Summary.csv")
write.csv(summary_df, summary_file, row.names = FALSE)
cat("Summary saved to:", summary_file, "\n")

# 4.2 Export Detailed Results for each pair
detailed_dir <- file.path(chi_output_dir, "detailed_tables")
dir.create(detailed_dir, showWarnings = FALSE)
source("DetailedTable.r")

#===============================================================================
# TEST: STRATIFIED REGRESSION ANALYSIS
#       Performance ~ Sensitivity + Th_AQ_Effect + Li_So_Effect, by each categorical var
#===============================================================================
sratfied_summary_df <- data.frame(
  Stratify_By = character(),
  Stratum = character(),
  R_Squared = numeric(),
  stringsAsFactors = FALSE
)
# 2. Create an empty Master List to hold all raw result objects
all_stratified_results <- list()


for (i in categorical_vars){
  for (j in numeric_vars){
           # Example usage:
            stratified_results <- scfa_conf_Analyzer$perform_stratified_analysis(
              data            = scfa_conf_Analyzer$data,
              outcome = j,
              predictors = numeric_vars,
              stratify_by_var = i
          )
  ## Location.of.Leaving, Occupation, age, Length, Weight, Gender 
          # Save the entire result object into the master list (keyed by the variable name)
         all_stratified_results[[i]] <- stratified_results
        
  # View results:
          # Extract metrics for the summary data frame
         for (stratum_name in names(stratified_results)) {
            
            # Append a new row to the data frame
           sratfied_summary_df <- rbind(sratfied_summary_df, data.frame(
              Stratify_By = i,
              Stratum = stratum_name,
             R_Squared = stratified_results[[stratum_name]]$r_squared,
             stringsAsFactors = FALSE
           ))
         }
          # 5. Extract effect sizes from stratified analysis
         stratum_effect_sizes <- scfa_conf_Analyzer$extract_effect_sizes(
           list(r.squared = stratified_results[[stratum_name]]$r_squared),
            "regression"
         )
  }
}

# ============================================================
# PRINT OUT THE CONSOLIDATED RESULTS
# ============================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CONSOLIDATED STRATIFIED ANALYSIS RESULTS\n")
cat(rep("=", 70), "\n\n", sep = "")

# Print the final data frame
print(sratfied_summary_df, row.names = FALSE)

# Optional: You can easily sort the table to see which stratum had the highest R²
stratified_results <- sratfied_summary_df[order(-sratfied_summary_df$R_Squared), ]
# Print the final data frame
print(sratfied_summary_df, row.names = FALSE)

#### ------ create folders and export result to CSV and text files
STRATIFIED_dir <- paste0("STRATIFIED_regression_Results_", timestamp)
dir.create(STRATIFIED_dir, showWarnings = FALSE)
cat("Results will be saved to:", STRATIFIED_dir, "\n")
# Create subdirectories for different output types  ## Summary
summary_dir <- file.path(STRATIFIED_dir, "summaries")
dir.create(summary_dir, showWarnings = FALSE)
# Create subdirectories for different output types  ## Detailes
detailed_dir <- file.path(STRATIFIED_dir, "detailed_results")
dir.create(detailed_dir, showWarnings = FALSE)
# Create subdirectories for different output types  ## Model
model_dir <- file.path(STRATIFIED_dir, "models")
dir.create(model_dir, showWarnings = FALSE)
# Create subdirectories for different output types  ## Plot
plots_dir <- file.path(STRATIFIED_dir, "plots")
dir.create(plots_dir, showWarnings = FALSE)

# 4.1 Export Summary CSV
summary_file_stratfied <- file.path(summary_dir, "Stratified_Summary.csv")
write.csv(sratfied_summary_df, summary_file_stratfied, row.names = FALSE)
cat("Summary saved to:", summary_file_stratfied, "\n")

# Export sorted summary as well
sorted_summary_file <- file.path(summary_dir, "Stratified_Summary_Sorted_by_R2.csv")
write.csv(stratified_results, sorted_summary_file, row.names = FALSE)
cat("Sorted summary saved to:", sorted_summary_file, "\n")

source("Stratfied_Resut_Exportion.r")


# #===============================================================================
# #===============================================================================
# #===============================================================================
# # TEST: MEDIATION & MODERATION ANALYSIS (Lavaan SEM-based)
# #===============================================================================
# # Mediation Analysis Example
# # Run the Lavaan mediation analysis
# Set output directory
outputdirectory <- "SecondDataSet/Result of analysis/"

# Create output directory if it doesn't exist
if (!dir.exists(outputdirectory)) {
  dir.create(outputdirectory, recursive = TRUE)
}

# Create subdirectories for different result types
dir.create(file.path(outputdirectory, "mediation_Moderation_Sensitivity"), showWarnings = FALSE)
dir.create(file.path(outputdirectory, "mediation_Moderation_Sensitivity/mediation_results"), showWarnings = FALSE)
dir.create(file.path(outputdirectory, "mediation_Moderation_Sensitivity/moderation_results"), showWarnings = FALSE)
dir.create(file.path(outputdirectory, "mediation_Moderation_Sensitivity/sensitivity_results"), showWarnings = FALSE)
dir.create(file.path(outputdirectory, "mediation_Moderation_Sensitivity/plots"), showWarnings = FALSE)
dir.create(file.path(outputdirectory, "mediation_Moderation_Sensitivity/tables"), showWarnings = FALSE)
# =============================================================================
# Define outcome variables (performance metrics)
outcome_vars <- numeric_vars[c(1,2,4)]
# Define predictor variables (IEQ factors)
predictor_vars <- numeric_vars[c(3:6)]
# demographic_var <- categorical_vars[c(1:5,16:17)]
# beheverial_var <- categorical_vars[c(6:15)]
demographic_var <- categorical_vars
# mediator_var
mediator_var <- numeric_vars[c(3,5:6)]
# =============================================================================
# PART 1: MEDIATION ANALYSIS (Lavaan)
# =============================================================================
cat("MEDIATION ANALYSIS (Lavaan SEM)\n")
# Perform mediation analysis using cfTestClass
# Run mediation analyses for each combination
mediation_results <- list()

for (outcome in outcome_vars) {
  for (med in mediator_var){
    for (demo in demographic_vars) {
      if (outcome != med) {
        result <- tryCatch({
          scfa_conf_Analyzer$perform_lavaan_mediation(
            data = scfa_conf_Analyzer$data,  #
            demographic_var = demo,
            outcome_var = outcome,
            mediator_var = med,
            reference_group = NULL,
            n_boot = 500
          )
        }, error = function(e) {
          cat("\n  ERROR:", e$message, "\n")
          return(NULL)
        })
      #}
      
      if (!is.null(result)) {
          key <- paste(outcome, "~", med, "via", demo)
          mediation_results[[key]] <- result
        
          # Save results to file
          sink(file.path(outputdirectory, "mediation_Moderation_Sensitivity/mediation_results", paste0(gsub("[^A-Za-z0-9]", "_", key), ".txt")))
          cat("MEDIATION ANALYSIS RESULTS\n")
          cat(paste(rep("=", 60), collapse = ""), "\n\n")
          cat("Outcome variable:", outcome, "\n")
          cat("Mediator variable:", mediator_var, "\n")
          cat("Demographic variable:", demo, "\n\n")
          cat("--- Lavaan Estimates ---\n")
          if (!is.null(result$estimates)) {
            print(result$estimates[, c("lhs", "op", "rhs", "est", "ci.lower", "ci.upper", "pvalue")])
          }
          sink()
        }
      }
    }
  }
}
cat("\n\nMediation analysis completed")

# =============================================================================
# PART 5: MODERATION ANALYSIS
# =============================================================================
cat("MODERATION ANALYSIS\n")
# Define outcome variables (performance metrics)
outcome_vars <- numeric_vars[c(1,2,4)]
# Define predictor variables (IEQ factors)
predictor_vars <- numeric_vars[c(3:6)]
# demographic_var <- categorical_vars[c(1:5,16:17)]
# beheverial_var <- categorical_vars[c(6:15)]
demographic_vars <- categorical_vars[c(3,9:15)]
# mediator_var
mediator_vars <- numeric_vars[c(3,5:6)]
valid_results <- list()
  
cat("MODERATION ANALYSIS\n")

# Storage
moderation_results_list <- list()

for (outcome in outcome_vars) {
  for (pred in predictor_vars) {
    for (medi in mediator_vars) {
      for (mod in demographic_vars) {
        
        # Skip degenerate combinations
        if (pred == outcome || mod == outcome || pred == medi) next
        
        key <- paste0(outcome, "__", pred, "__mediator_", medi, "__mod_", mod)
        
        cat(paste0("\n", strrep("=", 60), "\n"))
        cat("Running:", outcome, "~", pred, "*", mod, "| mediator:", medi, "\n")
        cat(paste0(strrep("=", 60), "\n"))
        
        result <- tryCatch({
          scfa_conf_Analyzer$perform_mediation_moderation_analysis(
            data            = scfa_conf_Analyzer$data,
            demographic_var = pred,      # ← CORRECTED: X variable, not NULL
            outcome_var     = outcome,
            mediator_var    = medi,
            moderator_var   = mod,
            n_boot          = 1000,
            alpha           = 0.05
          )
        }, error = function(e) {
          cat("  ✗ Error for", key, ":", e$message, "\n")
          NULL
        }, warning = function(w) {
          cat("  ⚠ Warning for", key, ":", w$message, "\n")
          NULL
        })
        
        if (!is.null(result)) {
          moderation_results_list[[key]] <- result
          
          # Save to file
          out_path <- file.path(
            outputdirectory,
            "mediation_Moderation_Sensitivity/moderation_results",
            paste0(gsub("[^A-Za-z0-9]", "_", key), ".txt")
          )
          dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
          
          sink(out_path)
          cat("MODERATION ANALYSIS RESULTS\n")
          cat(strrep("=", 60), "\n\n")
          cat("Outcome   :", outcome, "\n")
          cat("Predictor :", pred,    "\n")
          cat("Mediator  :", medi,    "\n")
          cat("Moderator :", mod,     "\n\n")
          
          # Mediation path summary
          if (!is.null(result$mediation)) {
            cat("--- Mediation Path Estimates ---\n")
            cat("b-path (mediator → outcome):", result$mediation$b_effect,
                "  p =", round(result$mediation$b_p, 4), "\n")
          }
          
          # Moderation summary
          if (!is.null(result$moderation)) {
            cat("\n--- Interaction Coefficients ---\n")
            int_eff <- result$moderation$interaction_effects
            int_ps  <- result$moderation$interaction_ps
            for (nm in names(int_eff)) {
              cat(sprintf("  %s: coef = %.4f, p = %.4f %s\n",
                          nm, int_eff[[nm]], int_ps[[nm]],
                          ifelse(int_ps[[nm]] < 0.05, "[*]", "")))
            }
            cat("\nModeration significant:", result$moderation$moderation_significant, "\n")
          }
          sink()
        }
        
      }
    }
  }
}

cat("\n\nModeration analysis completed. Total valid results:", length(moderation_results_list), "\n")

# =============================================================================
# PART 6: SENSITIVITY ANALYSIS
# #===============================================================================
# # TEST: SENSITIVITY ANALYSIS & ROBUSTNESS TESTING
# #       Outlier impact, missing data methods, subgroup stability
# #===============================================================================
# =============================================================================
cat("\n\n", rep("=", 80), collapse = "")
cat("SENSITIVITY ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""),"\n")

# Run sensitivity analyses
sensitivity_results <- list()

for (outcome in outcome_vars) {
  key <- paste("Sensitivity:", outcome)
  cat("Running:", key)
  cat(paste0("\n", paste(rep("=", 60), collapse = ""), "\n"))
  cat(paste0("MODERATED MEDIATION ANALYSIS\n"))
  cat(paste0(paste(rep("=", 60), collapse = ""), "\n\n"))
  
  sensitivity_results = scfa_conf_Analyzer$perform_sensitivity_analysis(data =  scfa_conf_Analyzer$data,
                                                 model_type = "linear",
                                                 dependent_var = outcome,
                                                 independent_vars = predictor_vars,
                                                 outlier_method = "iqr",
                                                 missing_data_methods = c("listwise", "mean_imputation"),
                                                 alpha = 0.05)
      
      # Save results to file
      sink(file.path(outputdirectory, "mediation_Moderation_Sensitivity/sensitivity_results", paste0("sensitivity_", outcome, ".txt")))
      cat("SENSITIVITY ANALYSIS RESULTS\n")
      cat(paste(rep("=", 60), collapse = ""), "\n\n")
      cat("Outcome variable:", outcome, "\n")
      cat("Predictors:", paste(preds, collapse = ", "), "\n\n")
      
      if (!is.null(sensitivity_results$outlier_analysis)) {
        cat("--- Outlier Analysis ---\n")
        print(sensitivity_results$outlier_analysis$outlier_info)
        cat("\n--- Outlier Impact ---\n")
        print(sensitivity_results$outlier_analysis$impact_on_models)
      }
      
      if (!is.null(sensitivity_results$assumption_validation)) {
        cat("\n--- Assumption Validation ---\n")
        print(sensitivity_results$assumption_validation)
      }
      
      if (!is.null(sensitivity_results$subgroup_analysis)) {
        cat("\n--- Subgroup Analysis ---\n")
        print(sensitivity_results$subgroup_analysis)
      }
      
      sink()
}



cat("\n\nSensitivity analysis completed")
###=============================================================================
#===============================================================================
# TEST: NON-PARAMETRIC TESTS ON RANKED DISCRETE DATA (Second Survey)
#       Mann-Whitney U test + Kolmogorov-Smirnov test — by Gender
#===============================================================================
ks_result_Ranked <- analy_SSOr$perform_mann_whitney_ks("Performance.under.AQ", "age", type = "ks", plot = TRUE)
print(ks_result_Ranked$p_value)
# ==============================================================================
# EXECUTION: Non-Parametric Tests on Ranked Discrete Data
# ==============================================================================

# 1. Get Names of all colmons, then hardcoding the name of ranked data columns. 
names(data_SOrRank)
# 3. Define the ranked outcome variables from your CSV
ranked_vars <- analy_SSOr$numeric_cols

# Mann-Whitney and KS tests require exactly 2 groups. Gender is perfect for this.
group_var <- "Gender"

# Create empty data frames to store the summary of all tests
mw_summary <- data.frame(Variable=character(), U_statistic=numeric(), P_value=numeric(), Significance=character(), stringsAsFactors=FALSE)
ks_summary <- data.frame(Variable=character(), D_statistic=numeric(), P_value=numeric(), Significance=character(), stringsAsFactors=FALSE)

# 4. Loop through each ranked variable and run the tests
for (var in ranked_vars) {
      cat("\n", rep("=", 60), "\n", sep = "")
      cat("Testing:", var, "by", group_var, "\n")
      cat(rep("=", 60), "\n", sep = "")
  
      # --- Mann-Whitney U Test ---
      mw_res <- tryCatch({
        analy_SSOr$perform_mann_whitney_ks(outcome = var, group_column = group_var, type = "mann_whitney", plot = TRUE)
      }, error = function(e) {
        cat("Mann-Whitney Error:", e$message, "\n")
        return(NULL)
      })
  
      if (!is.null(mw_res)) {
          mw_summary <- rbind(mw_summary, data.frame(
          Variable = var,
          U_statistic = unname(mw_res$statistic),
          P_value = mw_res$p_value,
          Significance = ifelse(mw_res$p_value < 0.05, "Significant", "Not Significant")
    ))
  }
  
      # --- Kolmogorov-Smirnov Test ---
      ks_res <- tryCatch({
        analy_SSOr$perform_mann_whitney_ks(outcome = var, group_column = group_var, type = "ks", plot = FALSE)
      }, error = function(e) {
        cat("KS Error:", e$message, "\n")
        return(NULL)
      })
  
      if (!is.null(ks_res)) {
        ks_summary <- rbind(ks_summary, data.frame(
            Variable = var,
            D_statistic = unname(ks_res$statistic),
            P_value = ks_res$p_value,
            Significance = ifelse(ks_res$p_value < 0.05, "Significant", "Not Significant")
    ))
  }
}

# 5. Print the consolidated summary tables
cat("\n\n=== MANN-WHITNEY U TEST SUMMARY (Male vs Female) ===\n")
print(mw_summary)

cat("\n\n=== KOLMOGOROV-SMIRNOV TEST SUMMARY (Male vs Female) ===\n")
print(ks_summary)

out_dir <- file.path(outputdirectory, "Statistical Analysis", "mattWhitny_kurskel-result")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fname <- file.path(out_dir,
                   paste0("MW_kST_result",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

capture.output(
  cat("=== FULL ORDINAL ASSOCIATIONS (all pairs) ===\n"),
  print(ks_summary, row.names = FALSE),
  cat("\n\n=== SIGNIFICANT ASSOCIATIONS ONLY (p < 0.05) ===\n"),
  print(mw_summary, row.names = FALSE),
  file = fname, append = TRUE
)
cat("Saved to:", fname, "\n")


#===============================================================================
# TEST: ORDINAL ASSOCIATIONS — Goodman-Kruskal Gamma + Kendall's Tau-b
#       All ranked variable pairs
#===============================================================================
# Kendall's tau-b (for ties in ordinal data)
# Extract the two ordinal columns from your ranked data object
vars <- analy_SSOr$numeric_cols
pairs <- combn(vars, 2, simplify = FALSE)

ord_assoc <- do.call(rbind, lapply(pairs, function(p) {
  x <- analy_SSOr$data[[p[1]]]
  y <- analy_SSOr$data[[p[2]]]
  
  tau  <- cor.test(x, y, method = "kendall")
  gam  <- GoodmanKruskalGamma(x, y, conf.level = 0.95)
  
  data.frame(
    Var1 = p[1], Var2 = p[2],
    Tau_b = unname(tau$estimate), Tau_p = tau$p.value,
    Gamma = gam[1], Gamma_low = gam[2], Gamma_high = gam[3]
  )
}))

ord_assoc <- ord_assoc[order(-abs(ord_assoc$Gamma)), ]
print(ord_assoc)
##-------------------------------------------------------------------------------
# Create empty data frame to store results
# -------------------------------------------------------------------------------
# Initialize empty data frame to store results (with desired columns)
ordinal_assoc_results <- data.frame(
  Var1 = character(),
  Var2 = character(),
  Kendall_Tau_b = numeric(),
  Tau_b_P_Value = numeric(),
  Goodman_Gamma = numeric(),
  Gamma_P_Value = numeric(),
  N_Pairs = integer(),
  Tau_b_Significant = logical(),
  Gamma_Significant = logical(),
  stringsAsFactors = FALSE
)

# Generate all unique pairs of variables (assumes 'ranked_vars' exists)
var_pairs <- combn(ranked_vars, 2, simplify = FALSE)

cat("\n=== Calculating Ordinal Associations (Gamma & Tau-b) ===\n")

for (pair_idx in seq_along(var_pairs)) {
  var1_name <- var_pairs[[pair_idx]][1]
  var2_name <- var_pairs[[pair_idx]][2]
  
  # Extract raw data
  var1_raw <- analy_SSOr$data[[var1_name]]
  var2_raw <- analy_SSOr$data[[var2_name]]
  
  # Convert to numeric (for Kendall) and factor (for Gamma)
  var1_num <- as.numeric(var1_raw)   # factor → integer codes, numeric → unchanged
  var2_num <- as.numeric(var2_raw)
  var1_fac <- as.factor(var1_raw)
  var2_fac <- as.factor(var2_raw)
  
  # Complete cases (exclude any missing values)
  complete_idx <- complete.cases(var1_num, var2_num)
  n_pairs <- sum(complete_idx)
  
  # Skip if too few complete pairs
  if (n_pairs < 2) {
    new_row <- data.frame(
      Var1 = var1_name,
      Var2 = var2_name,
      Kendall_Tau_b = NA_real_,
      Tau_b_P_Value = NA_real_,
      Goodman_Gamma = NA_real_,
      Gamma_P_Value = NA_real_,
      N_Pairs = n_pairs,
      Tau_b_Significant = NA,
      Gamma_Significant = NA,
      stringsAsFactors = FALSE
    )
    ordinal_assoc_results <- rbind(ordinal_assoc_results, new_row)
    next
  }
  
  # ---- 1. Kendall's Tau-b ----
  tau_b_result <- tryCatch({
    ct <- cor.test(var1_num[complete_idx], var2_num[complete_idx],
                   method = "kendall", use = "complete.obs")
    list(estimate = as.numeric(ct$estimate), p_value = ct$p.value)
  }, error = function(e) {
    cat("\n    Warning: Kendall Tau-b failed for", var1_name, "&", var2_name, "-", e$message, "\n")
    list(estimate = NA_real_, p_value = NA_real_)
  })
  
  # ---- 2. Goodman-Kruskal Gamma ----
  gamma_result <- tryCatch({
    # Get estimate and 95% confidence interval
    gamma_obj <- DescTools::GoodmanKruskalGamma(var1_fac, var2_fac, conf.level = 0.95)
    
    # gamma_obj is a matrix with columns: "gamma", "lwr.ci", "upr.ci"
    if (is.matrix(gamma_obj) || is.data.frame(gamma_obj)) {
      gamma_est <- gamma_obj[1, "gamma"]
      lower    <- gamma_obj[1, "lwr.ci"]
      upper    <- gamma_obj[1, "upr.ci"]
    } else {
      # Fallback (should not happen with conf.level set)
      gamma_est <- as.numeric(gamma_obj)
      lower <- upper <- NA_real_
    }
    
    # Compute p-value from CI assuming asymptotic normality
    if (!is.na(lower) && !is.na(upper) && lower != upper) {
      se <- (upper - lower) / (2 * qnorm(0.975))
      z <- gamma_est / se
      p_value <- 2 * (1 - pnorm(abs(z)))
    } else {
      p_value <- NA_real_
    }
    
    list(estimate = gamma_est, p_value = p_value)
  }, error = function(e) {
    cat("\n    Warning: Goodman-Kruskal Gamma failed for", var1_name, "&", var2_name, "-", e$message, "\n")
    list(estimate = NA_real_, p_value = NA_real_)
  })
  
  # ---- Assemble row ----
  new_row <- data.frame(
    Var1 = var1_name,
    Var2 = var2_name,
    Kendall_Tau_b = tau_b_result$estimate,
    Tau_b_P_Value = tau_b_result$p_value,
    Goodman_Gamma = gamma_result$estimate,
    Gamma_P_Value = gamma_result$p_value,
    N_Pairs = n_pairs,
    Tau_b_Significant = ifelse(!is.na(tau_b_result$p_value) && tau_b_result$p_value < 0.05, TRUE, FALSE),
    Gamma_Significant = ifelse(!is.na(gamma_result$p_value) && gamma_result$p_value < 0.05, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  ordinal_assoc_results <- rbind(ordinal_assoc_results, new_row)
}

# View the full summary table
print(ordinal_assoc_results)


# ==============================================================================
# CALCULATE KENDALL'S TAU-B AND GOODMAN-KRUSKAL GAMMA (BULLETPROOF METHOD)
# ==============================================================================
# Create empty data frame to store results
ordinal_assoc_results <- data.frame(
  Var1 = character(),
  Var2 = character(),
  Gamma = numeric(),
  Gamma_P_Value = numeric(),
  Tau_b = numeric(),
  Tau_b_P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Generate all unique pairs of variables
var_pairs <- combn(ranked_vars, 2, simplify = FALSE)

cat("\n=== Calculating Ordinal Associations (Gamma & Tau-b) ===\n")

for (pair in var_pairs) {
  v1_name <- pair[1]
  v2_name <- pair[2]
  
  # 1. Extract data as NUMERIC (Required for cor.test)
  v1_num <- as.numeric(analy_SSOr$data[[v1_name]])
  v2_num <- as.numeric(analy_SSOr$data[[v2_name]])
  
  # 2. Extract data as FACTOR (Required for DescTools)
  v1_fac <- as.factor(analy_SSOr$data[[v1_name]])
  v2_fac <- as.factor(analy_SSOr$data[[v2_name]])
  
  # --- Calculate Kendall's Tau-b using cor.test ---
  tau_b_res <- tryCatch({
    res <- cor.test(v1_num, v2_num, method = "kendall")
    list(tau_b = as.numeric(res$estimate), p.value = res$p.value)
  }, error = function(e) {
    list(tau_b = NA_real_, p.value = NA_real_)
  })
  
  # --- Calculate Goodman and Kruskal's Gamma ---
  # We use the exact same two-step logic that worked in your manual test:
  # Step A: Use DescTools on FACTORS to get the Gamma estimate
  # Step B: Use base R on NUMERIC to get the p-value
  gamma_res <- tryCatch({
    # Step A: Get Estimate
    gamma_est <- GoodmanKruskalGamma(v1_fac, v2_fac, conf.level = NA)
    
    # Step B: Approximate p-value via Kendall's tau (no direct significance test exists for Gamma)
    p_val <- cor.test(v1_num, v2_num, method = "kendall")$p.value
    
    list(gamma = as.numeric(gamma_est), p.value = p_val)
  }, error = function(e) {
    list(gamma = NA_real_, p.value = NA_real_)
  })
  
  # Append to results
  ordinal_assoc_results <- rbind(ordinal_assoc_results, data.frame(
    Var1 = v1_name,
    Var2 = v2_name,
    Gamma = gamma_res$gamma,
    Gamma_P_Value = gamma_res$p.value,
    Tau_b = tau_b_res$tau_b,
    Tau_b_P_Value = tau_b_res$p.value,
    stringsAsFactors = FALSE
  ))
}

# View the fully populated table
cat("\n=== Full Ordinal Associations Summary ===\n")
print(ordinal_assoc_results)

# ==============================================================================
# FILTER TO SIGNIFICANT ASSOCIATIONS (p < 0.05)
# ==============================================================================
# We use a safe filter that ignores NAs so we don't get rows of <NA>
significant_assoc <- ordinal_assoc_results[
  (!is.na(ordinal_assoc_results$Tau_b_P_Value) & ordinal_assoc_results$Tau_b_P_Value < 0.05) |
    (!is.na(ordinal_assoc_results$Gamma_P_Value) & ordinal_assoc_results$Gamma_P_Value < 0.05), 
]

cat("\n=== Significant Ordinal Associations (p < 0.05) ===\n")
print(significant_assoc)


cat("\n=== Significant Ordinal Associations (p < 0.05) ===\n")
print(significant_assoc)


# 5. Print the consolidated summary tables
out_dir <- file.path(outputdirectory, "Statistical Analysis", "Ordinary_data_result")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fname <- file.path(out_dir,
                   paste0("ORDINAL ASSOCIATIONS - Goodman-Kruskal-result_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

capture.output(
  cat("=== FULL ORDINAL ASSOCIATIONS (all pairs) ===\n"),
  print(ord_assoc, row.names = FALSE),
  cat("\n\n=== SIGNIFICANT ASSOCIATIONS ONLY (p < 0.05) ===\n"),
  print(significant_assoc, row.names = FALSE),
  file = fname, append = TRUE
)
cat("Saved to:", fname, "\n")

#===============================================================================
# TEST: LEVENE'S TEST FOR HOMOGENEITY OF VARIANCE
#       + ANDERSON-DARLING K-SAMPLE (distributional equivalence across groups)
#       Applied to ranked and continuous data
#===============================================================================
# ==============================================================================
# ==============================================================================
# Clear all existing plot devices and reset graphics state
while (dev.cur() > 1) {
  dev.off()
}
# Reset par() to default specifications
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
cat("\n=== LEVENE'S TEST FOR VARIANCE HOMOGENEITY ===\n\n")
# ==============================================================================
# SECTION 1: LEVENE'S TEST FOR HOMOGENEITY OF VARIANCE (Ranked/Raw Data)
# ==============================================================================
# Purpose: Test if outcome variable variance is homogeneous across demographic groups
# Null hypothesis (H₀): Variances are equal across groups
# Alternative (H₁): At least one group has unequal variance
# Test 1.1: Variance homogeneity of "Performance under TC" across Gender
# Clear all existing plot devices and reset graphics state

test_name <- "Levene_Test"
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir   <- file.path(outputdirectory, "Statistical Analysis",
                       paste0(test_name, "_", timestamp))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(out_dir, paste0(test_name, "_summary_", timestamp, ".txt"))
sink(log_file, split = TRUE)
cat("=== LEVENE'S TEST FOR VARIANCE HOMOGENEITY (AUTOMATED) ===\n")
cat("Run:", timestamp, "\n")

results_list <- list()

for (outcome in analy_SSOr$numeric_cols) {
  for (group_col in analy_SSOr$categorical_cols) {
    
    grp_vals <- unique(na.omit(analy_SSOr$data[[group_col]]))
    if (length(grp_vals) < 2 || length(grp_vals) > 15) next   # skip IDs/timestamps/free text
    
    cat("\n---------------------------------------------\n")
    cat("Outcome:", outcome, "| Group:", group_col, "\n")
    
    res <- tryCatch({
      while (dev.cur() > 1) dev.off()
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
      
      plot_file <- file.path(out_dir, paste0("Levene_", outcome, "_by_", group_col, ".png"))
      png(plot_file, width = 800, height = 600)
      lev_res <- analy_SSOr$perform_levene_anderson(
        outcome = outcome, group_column = group_col,
        type = "levene", plot = TRUE
      )
      dev.off()
      lev_res
    }, error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
    
    if (is.null(res)) next
    
    cat("  F-statistic:", round(res$statistic, 4), "\n")
    cat("  p-value:", round(res$p_value, 4), "\n")
    cat("  Interpretation:",
        ifelse(res$p_value < 0.05,
               "SIGNIFICANTLY DIFFERENT variances -> use Welch/non-parametric\n",
               "NOT significantly different -> standard equal-variance test OK\n"))
    
    results_list[[paste(outcome, group_col, sep = "__")]] <- data.frame(
      Outcome = outcome, Group = group_col,
      F_stat = res$statistic, p_value = res$p_value,
      Significant = res$p_value < 0.05
    )
  }
}

summary_table <- do.call(rbind, results_list)
cat("\n\n=== FULL SUMMARY TABLE ===\n")
print(summary_table, row.names = FALSE)
sink()

write.csv(summary_table,
          file.path(out_dir, paste0(test_name, "_table_", timestamp, ".csv")),
          row.names = FALSE)

cat("All results and plots saved to:\n", out_dir, "\n")
# ==============================================================================
# SUMMARY AND RECOMMENDATIONS
# ==============================================================================

cat("\n\n=== STATISTICAL TESTING SUMMARY AND RECOMMENDATIONS ===\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n1. VARIANCE HOMOGENEITY (Levene's Test):\n")
cat("   • Assesses assumption of equal variances across groups\n")
cat("   • Failure (p < 0.05) → Use Welch's or non-parametric alternatives\n")

cat("\n2. UNIVARIATE NORMALITY (Anderson-Darling, Per-Group):\n")
cat("   • Tests whether scores within each group follow N(μ,σ²)\n")
cat("   • Failure (p < 0.05) → Consider transformation or non-parametric tests\n")

cat("\n3. DISTRIBUTIONAL EQUIVALENCE (K-Sample Anderson-Darling):\n")
cat("   • Tests whether demographic strata have identical distributions\n")
cat("   • Significant (p < 0.05) → Differential IEQ effects across groups\n")
cat("   • Non-significant (p ≥ 0.05) → Uniform environmental sensitivity profile\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
#===============================================================================
#===============================================================================
# TEST: ORDINAL LOGISTIC REGRESSION
#       Outcome: ordered factor (1–5) | Predictors: numeric + categorical
# AUTOMATED ORDINAL LOGISTIC REGRESSION: EACH numeric_col AS OUTCOME,
# ACH numeric_col AS OUTCOME, PRUNED numeric_cols + categorical_cols AS PREDICTORS
#===============================================================================
`%||%` <- function(a, b) if (!is.null(a)) a else b

test_name <- "Ordinal_Logistic_Regression"
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir   <- file.path(outputdirectory, "Statistical Analysis",
                       paste0(test_name, "_", timestamp))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Predictor screening (done once, outside the loop) -----------------------
usable_categorical <- Filter(function(col) {
  v <- unique(na.omit(analy_SSOr$data[[col]]))
  length(v) >= 2 && length(v) <= 8
}, analy_SSOr$categorical_cols)

high_corr      <- ord_assoc[abs(ord_assoc$Gamma) > 0.7, c("Var1", "Var2")]
drop_numeric   <- unique(high_corr$Var2)
usable_numeric <- setdiff(analy_SSOr$numeric_cols, drop_numeric)

# Keep the ORIGINAL full dataset safe — we will swap it in/out per model
original_data <- analy_SSOr$data

log_file <- file.path(out_dir, paste0(test_name, "_summary_", timestamp, ".txt"))
sink(log_file, split = TRUE)
cat("=== AUTOMATED ORDINAL LOGISTIC REGRESSION (SCREENED, PER-MODEL COMPLETE CASES) ===\n")
cat("Run:", timestamp, "\n")
cat("Categorical predictors retained:", paste(usable_categorical, collapse = ", "), "\n")
cat("Numeric predictors dropped for collinearity (|gamma|>0.7):",
    paste(drop_numeric, collapse = ", "), "\n")

fit_summary <- list()

for (outcome in analy_SSOr$numeric_cols) {
  
  predictors <- c(setdiff(usable_numeric, outcome), usable_categorical)
  
  cat("\n=======================================================\n")
  cat("Outcome:", outcome, "\n")
  cat("Predictors:", paste(predictors, collapse = ", "), "\n")
  
  # Build the complete-case subset for THIS outcome + THESE predictors ONLY
  # (ignores missingness in unrelated columns elsewhere in the dataset)
  sub_df <- original_data[, c(outcome, predictors), drop = FALSE]
  sub_df <- sub_df[complete.cases(sub_df), ]
  n_obs  <- nrow(sub_df)
  
  cat("  Complete cases for this outcome+predictor set:", n_obs, "\n")
  
  for (prede in predictors){
    res <- tryCatch({
      while (dev.cur() > 1) dev.off()
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
    
      plot_file <- file.path(out_dir, paste0("OrdLog_", outcome,"_vs_", prede,".png"))
      png(plot_file, width = 900, height = 700)
    
      analy_SSOr$data <- sub_df   # swap in the per-model complete-case subset
      model_res <- analy_SSOr$fit_ordinal_logistic_regression(
        outcome     = outcome,
        predictors  = prede,
      conf_level  = 0.95,
      plot        = TRUE
    )
    dev.off()
    model_res
    detail_file <- file.path(out_dir, paste0("OrdLog_", outcome, "_vs_", prede,"_detail.txt"))
    capture.output(print(model_res), file = detail_file)
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    NULL
  })
  
    resfull <- tryCatch({
      while (dev.cur() > 1) dev.off()
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
      
      plot_file <- file.path(out_dir, paste0("OrdLog_", outcome, ".png"))
      png(plot_file, width = 900, height = 700)
      
      analy_SSOr$data <- sub_df   # swap in the per-model complete-case subset
      model_res <- analy_SSOr$fit_ordinal_logistic_regression(
        outcome     = outcome,
        predictors  = predictors,
        conf_level  = 0.95,
        plot        = TRUE
      )
      dev.off()
      model_res
      detail_file <- file.path(out_dir, paste0("OrdLog_", outcome,"_detail.txt"))
      capture.output(print(model_res), file = detail_file)
    }, error = function(e) {
      cat("  ERROR:", conditionMessage(e), "\n")
      NULL
    }, finally = {
      analy_SSOr$data <- original_data   # always restore, success or failure
    })
    
    
    
    
    
  if (is.null(res)) next
  }
  detail_file <- file.path(out_dir, paste0("OrdLog_", outcome, "_detail.txt"))
  capture.output(print(res), file = detail_file)
  
  cat("\n--- Model summary (", outcome, ") ---\n", sep = "")
  print(res)
  
  fit_summary[[outcome]] <- data.frame(
    Outcome  = outcome,
    AIC      = tryCatch(res$AIC %||% AIC(res$model), error = function(e) NA),
    LogLik   = tryCatch(res$logLik %||% as.numeric(logLik(res$model)), error = function(e) NA),
    N        = n_obs
  )
}

analy_SSOr$data <- original_data   # final safety net, in case the loop exits early

summary_table <- do.call(rbind, fit_summary)
cat("\n\n=== FIT SUMMARY ACROSS ALL OUTCOMES ===\n")
print(summary_table, row.names = FALSE)
sink()

write.csv(summary_table,
          file.path(out_dir, paste0(test_name, "_table_", timestamp, ".csv")),
          row.names = FALSE)

cat("All models, plots, and detail files saved to:\n", out_dir, "\n")
# Interpret the results:
# - Odds_Ratio > 1: As the predictor increases, the odds of being in a higher performance category increase.
# - Odds_Ratio < 1: As the predictor increases, the odds of being in a higher performance category decrease.
# - P_Value < 0.05: The predictor is statistically significant.

# ==============================================================================
# ==============================================================================
# Thematic analisys 
# ==============================================================================
# ==============================================================================
# ==============================================================================
# thematic_analysis_main.r
#
# Written by Meqdad Hasan
#
# Thematic analysis of the IEQ (Indoor Environmental Quality) survey data,
# run separately on:
#   (A) the CONTINUOUS CFA/BCFA factor-score dataset
#         SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv
#   (B) the ORDINAL raw Likert-item dataset (1-5 scale)
#         SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx
#
# This script plugs directly into the existing project structure used by
# main_SecondSurvey.r (same relative paths, same library set) and adds one
# new analytical component: thematicAnalysisClass (see thematicAnalysisClass.r).
#
# Output: a single, fully self-contained, timestamped results folder
# containing CSV tables, PNG figures, a narrative TXT report and an RDS
# snapshot of every R object produced -- see save_thematic_report() below.
# ==============================================================================
cat("\014")                       # clear console
while (dev.cur() > 1) dev.off()   # clear all plotting devices
# rm(list = ls())                 # uncomment to clear environment variables too

#-------------------------------------------------------------------------------
# STEP 0: Load necessary libraries
#-------------------------------------------------------------------------------
library(readxl)
library(psych)
library(GPArotation)   # required by psych::fa() for oblique rotation (oblimin)
library(R6)

#-------------------------------------------------------------------------------
# STEP 00: source required classes
#-------------------------------------------------------------------------------
source("regrissorClass.r")
source("cfTestClass.r")
source("thematicAnalysisClass.r")
cat("\014")

#===============================================================================
# STEP 1: LOAD DATA (same paths/convention as main_SecondSurvey.r)
#===============================================================================

# Continuous data: CFA/BCFA factor scores (Second dataset)
data_SCFs <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)

# Ordinal data: cleaned raw Likert-item responses (Second dataset)
data_SOrRank <- as.data.frame(read_excel("SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx"))

#===============================================================================
# STEP 2: CREATE thematicAnalysisClass INSTANCES
#===============================================================================

clean_data_SCFs <- na.omit(data_SCFs)

# (A) Continuous factor scores -> auto-detected as "continuous"
thematic_continuous <- thematicAnalysisClass$new(input = clean_data_SCFs)

# (B) Raw Likert items -> auto-detected as "ordinal" (integer 1-5, <=10 levels)
#     unique_threshold can be widened/narrowed if your instrument uses a
#     different response-scale length (e.g. 7-point Likert -> threshold = 10 is fine;
#     a 0-100 continuous slider item would need continuous_vars specified explicitly).
thematic_ordinal <- thematicAnalysisClass$new(input = data_SOrRank, unique_threshold = 10)

cat("\nAuto-detected CONTINUOUS variables:\n")
print(thematic_continuous$continuous_cols)
cat("\nAuto-detected ORDINAL variables:\n")
print(thematic_ordinal$ordinal_cols)

#===============================================================================
# STEP 3: THEMATIC ANALYSIS -- CONTINUOUS DATA
#         Pearson-correlation EFA + correlation-distance hierarchical
#         clustering, cross-validated against one another.
#===============================================================================

results_continuous <- thematic_continuous$perform_thematic_analysis(
  variables      = thematic_continuous$continuous_cols,
  cor_type       = "pearson",
  rotation       = "oblimin",
  fm             = "minres",
  cluster_method = "ward.D2",
  plot           = TRUE
)

#===============================================================================
# STEP 4: THEMATIC ANALYSIS -- ORDINAL DATA
#         Polychoric-correlation EFA + correlation-distance hierarchical
#         clustering, cross-validated against one another.
#===============================================================================

results_ordinal <- thematic_ordinal$perform_thematic_analysis(
  variables      = thematic_ordinal$ordinal_cols,
  cor_type       = "polychoric",
  rotation       = "oblimin",
  fm             = "minres",
  cluster_method = "ward.D2",
  plot           = TRUE
)

#===============================================================================
# STEP 5: SAVE EVERYTHING -- creates <test_name>_<YYYYMMDD_HHMMSS>/ containing
#         CSV tables, PNG figures (scree plot, dendrogram, theme diagram),
#         a full narrative TXT report and an RDS snapshot of every object.
#===============================================================================

# Change test_name to whatever label you want this run archived under --
# the folder that gets created is: <test_name>_<YYYYMMDD_HHMMSS>
out_dir <- thematic_continuous$save_thematic_report(
  results_continuous = results_continuous,
  results_ordinal    = results_ordinal,
  test_name          = "IEQ_Thematic_Analysis",
  base_dir           = "."
)

cat("\n\nThematic analysis complete. All outputs saved under:\n  ", out_dir, "\n", sep = "")

# To reload every object from a previous run later, e.g. for further
# inspection or re-plotting without re-running the analysis:
#   saved <- readRDS(file.path(out_dir, "thematic_analysis_results.RDS"))
#   saved$results_continuous$efa$fa_result
#   saved$results_ordinal$cross_validation$comparison_table
###=============================================================================
analy_SCFA$list_all_methods()
scfa_conf_Analyzer$list_all_methods()
###=============================================================================
###=============================================================================
###=============================================================================
###=============================================================================
###=============================================================================