# Install R6 package if needed
# install.packages("R6")
library(R6)

# Define a DataAnalyzer class using R6
cfTestClass = R6Class("confermatoryTestClass",
                          public = list(
                            # Properties
                            data = NULL,
                            summary_stats = NULL,
                            numeric_cols = NULL,      # New: List of numeric column names
                            categorical_cols = NULL,  # New: List of categorical column names
                            data_source = NULL,       # New: Track source (e.g., file path or "data.frame")
                            
                            # Constructor (Enhanced for Data Engineering)
                            initialize = function(input, 
                                                  file_sep = ",", 
                                                  header = TRUE, 
                                                  stringsAsFactors = FALSE, 
                                                  na.strings = c("", "NA", "N/A")) {
                              # Step 1: Validate and load input
                              if (is.character(input)) {
                                # Assume it's a file path; load CSV
                                if (!file.exists(input)) {
                                  stop("File does not exist: ", input)
                                }
                                self$data = tryCatch({
                                  read.csv(input, sep = file_sep, header = header, 
                                           stringsAsFactors = stringsAsFactors, 
                                           na.strings = na.strings)
                                }, error = function(e) {
                                  stop("Error loading CSV: ", e$message)
                                })
                                self$data_source = paste("File:", input)
                                cat("Data loaded from file:", input, "\n")
                              } else if (is.data.frame(input)) {
                                # Direct data frame input
                                self$data = input
                                self$data_source = "Direct data.frame"
                                cat("Data provided as data.frame\n")
                              } else {
                                stop("Input must be a data frame or a valid file path (string)")
                              }
                              
                              # Step 2: Basic validation
                              if (nrow(self$data) == 0 || ncol(self$data) == 0) {
                                stop("Data must have at least one row and one column")
                              }
                              
                              # Step 3: Initial data cleaning 
                              # Remove completely empty rows/columns
                              self$data = self$data[rowSums(is.na(self$data)) != ncol(self$data), ]  # Remove all-NA rows
                              self$data = self$data[, colSums(is.na(self$data)) != nrow(self$data)]  # Remove all-NA columns
                              
                              # Convert empty strings to NA (common in CSVs)
                              self$data[self$data == ""] = NA
                              
                              # Step 4: Identify column types
                              self$numeric_cols = names(self$data)[sapply(self$data, is.numeric)]
                              self$categorical_cols = names(self$data)[sapply(self$data, function(x) !is.numeric(x))]
                              
                              cat("DataAnalyzer initialized with", nrow(self$data), "rows and", ncol(self$data), "columns\n")
                              cat("Numeric columns:", length(self$numeric_cols), "| Categorical columns:", length(self$categorical_cols), "\n")
                              cat("Data source:", self$data_source, "\n")
                            },

                            #===================================================
                            # New Method: All other methods here are implemented**
                            #===================================================
                            #New Method
                            # To integrate into regrissorClass, add the following to the public list:
                            
                            perform_chi_square_test = function(var1, var2, calculate_cramers = TRUE) {
                              # Validate input variables
                              private$validate_column(var1)
                              private$validate_column(var2)
                              
                              # Check if variables are categorical
                              if (!is.factor(self$data[[var1]]) && !is.character(self$data[[var1]])) {
                                warning(var1, " is not categorical. Converting to factor.")
                                self$data[[var1]] = as.factor(self$data[[var1]])
                              }
                              
                              if (!is.factor(self$data[[var2]]) && !is.character(self$data[[var2]])) {
                                warning(var2, " is not categorical. Converting to factor.")
                                self$data[[var2]] = as.factor(self$data[[var2]])
                              }
                              
                              # Create contingency table
                              contingency_table = table(self$data[[var1]], self$data[[var2]], useNA = "no")
                              
                              # Check for sufficient sample size
                              if (sum(contingency_table) < 20) {
                                warning("Small sample size (n < 20). Chi-square test may not be reliable.")
                              }
                              
                              # Perform chi-square test
                              chi_test = tryCatch({
                                chisq.test(contingency_table)
                              }, warning = function(w) {
                                warning("Chi-square test warning: ", w$message)
                                return(chisq.test(contingency_table))
                              })
                              
                              # Check for expected frequencies
                              expected_frequencies = chi_test$expected
                              min_expected = min(expected_frequencies[!is.na(expected_frequencies)])
                              
                              if (min_expected < 5) {
                                warning("Some expected frequencies are less than 5. Chi-square test validity compromised.")
                              }
                              
                              # Calculate effect sizes
                              effect_sizes = list()
                              
                              if (calculate_cramers) {
                                n = sum(contingency_table)
                                min_dim = min(dim(contingency_table)) - 1
                                
                                # Cramér's V (correct formula: V = sqrt(χ²/(n(k-1))) where k = min(rows, cols))
                                cramers_v = sqrt(chi_test$statistic / (n * min_dim))
                                
                                # ✓ CORRECTED: Use all() for logical AND of vector comparison
                                phi = ifelse(all(dim(contingency_table) == c(2, 2)), 
                                              sqrt(chi_test$statistic / n), 
                                              NA)
                                
                                effect_sizes$cramers_v = as.numeric(cramers_v)
                                effect_sizes$phi = phi
                                effect_sizes$cramers_v_interpretation = private$interpret_cramers_v(cramers_v)
                              }
                              
                              # Calculate statistics
                              total = sum(contingency_table)
                              row_totals = rowSums(contingency_table)
                              col_totals = colSums(contingency_table)
                              
                              # ✓ IMPROVED: Use sweep() for clarity (both methods work, but sweep is explicit)
                              percentages = round(100 * contingency_table / total, 2)
                              row_percentages = round(sweep(contingency_table, 1, row_totals, "/") * 100, 2)
                              col_percentages = round(sweep(contingency_table, 2, col_totals, "/") * 100, 2)
                              
                              # Prepare comprehensive results
                              results = list(
                                test_type = "Chi-square Test of Independence",
                                variable1 = var1,
                                variable2 = var2,
                                contingency_table = contingency_table,
                                percentages = percentages,
                                row_percentages = row_percentages,
                                col_percentages = col_percentages,
                                chi_square_statistic = as.numeric(chi_test$statistic),
                                degrees_of_freedom = as.numeric(chi_test$parameter),
                                p_value = chi_test$p.value,
                                sample_size = total,
                                expected_frequencies = chi_test$expected,
                                residuals = chi_test$residuals,
                                std_residuals = chi_test$residuals / sqrt(chi_test$expected),
                                effect_sizes = effect_sizes,
                                significant = chi_test$p.value < 0.05,
                                interpretation = ifelse(chi_test$p.value < 0.05, 
                                                        "Significant association between variables (p < 0.05)", 
                                                        "No significant association between variables (p ≥ 0.05)")
                              )
                              
                              # Print results
                              cat("\n", rep("=", 60), "\n", sep = "")
                              cat("CHI-SQUARE TEST OF INDEPENDENCE\n")
                              cat(rep("=", 60), "\n\n", sep = "")
                              cat(sprintf("Variables tested: %s vs %s\n", var1, var2))
                              cat(sprintf("Sample size (n): %d\n", total))
                              cat(sprintf("Chi-square statistic (χ²): %.4f\n", chi_test$statistic))
                              cat(sprintf("Degrees of freedom: %d\n", chi_test$parameter))
                              cat(sprintf("p-value: %.6f\n", chi_test$p.value))
                              cat(sprintf("Result: %s\n\n", results$interpretation))
                              
                              if (calculate_cramers && !is.na(effect_sizes$cramers_v)) {
                                cat(sprintf("Cramér's V: %.4f  (%s)\n", 
                                            effect_sizes$cramers_v, 
                                            effect_sizes$cramers_v_interpretation))
                              }
                              
                              if (!is.na(effect_sizes$phi)) {
                                cat(sprintf("Phi coefficient: %.4f  (2x2 table)\n", effect_sizes$phi))
                              }
                              
                              cat("\n--- Contingency Table ---\n")
                              print(contingency_table)
                              
                              cat("\n--- Row Percentages (within each row) ---\n")
                              print(row_percentages)
                              
                              cat("\n--- Column Percentages (within each column) ---\n")
                              print(col_percentages)
                              
                              cat("\n--- Standardized Residuals ---\n")
                              cat("(Values > ±2 indicate significant deviation from independence)\n")
                              print(round(chi_test$residuals / sqrt(chi_test$expected), 2))
                              
                              cat(rep("=", 60), "\n\n", sep = "")
                              
                              return(invisible(results))
                            },
                            #===================================================
                            # ==============================================================================
                            # Method 1: Mediation/Moderation Analysis (Categorical Safe)
                            # ==============================================================================
                            perform_mediation_moderation_analysis = function(data, 
                                                                             demographic_var, 
                                                                             outcome_var, 
                                                                             mediator_var, 
                                                                             moderator_var = NULL,
                                                                             n_boot = 1000,
                                                                             alpha = 0.05) {
                              
                              # --- Helper Function to dynamically find coefficient rows ---
                              # Handles dummy variables (e.g., "age30-45" when input is "age")
                              get_var_rows <- function(coef_names, var) {
                                if (var %in% coef_names) return(var)
                                matches <- grep(paste0("^", var), coef_names, value = TRUE)
                                if (length(matches) == 0) stop("Coefficient for '", var, "' not found in model.")
                                return(matches)
                              }
                              
                              # --- Data Preparation ---
                              # Ensure categorical variables are factors
                              if (!is.numeric(data[[demographic_var]])) {
                                data[[demographic_var]] <- as.factor(data[[demographic_var]])
                              }
                              if (!is.null(moderator_var) && !is.numeric(data[[moderator_var]])) {
                                data[[moderator_var]] <- as.factor(data[[moderator_var]])
                              }
                              
                              # Initialize results list
                              results = list()
                              
                              # ============================================================================
                              # Part 1: Mediation Analysis
                              # ============================================================================
                              cat("=== MEDIATION ANALYSIS ===\n")
                              is_x_categorical <- is.factor(data[[demographic_var]])
                              
                              # Step 1: Total effect (X -> Y)
                              total_model <- lm(as.formula(paste(outcome_var, "~", demographic_var)), data = data)
                              sum_total <- summary(total_model)
                              
                              # Step 2: Mediator effect (X -> M)
                              med_model <- lm(as.formula(paste(mediator_var, "~", demographic_var)), data = data)
                              sum_med <- summary(med_model)
                              
                              # Step 3: Direct effect (X -> Y controlling for M)
                              dir_model <- lm(as.formula(paste(outcome_var, "~", demographic_var, "+", mediator_var)), data = data)
                              sum_dir <- summary(dir_model)
                              
                              # Extract rows dynamically
                              x_rows <- get_var_rows(rownames(sum_total$coefficients), demographic_var)
                              m_row <- mediator_var # M is always numeric in this setup
                              
                              # Extract effects into named vectors
                              total_effects <- sum_total$coefficients[x_rows, "Estimate"]
                              total_ps <- sum_total$coefficients[x_rows, "Pr(>|t|)"]
                              a_effects <- sum_med$coefficients[x_rows, "Estimate"]
                              a_ps <- sum_med$coefficients[x_rows, "Pr(>|t|)"]
                              direct_effects <- sum_dir$coefficients[x_rows, "Estimate"]
                              direct_ps <- sum_dir$coefficients[x_rows, "Pr(>|t|)"]
                              b_effect <- sum_dir$coefficients[m_row, "Estimate"]
                              b_p <- sum_dir$coefficients[m_row, "Pr(>|t|)"]
                              
                              # Calculate indirect effects
                              indirect_effects <- a_effects * b_effect
                              
                              # Bootstrapping for indirect effects
                              boot_indirect <- matrix(NA, nrow = n_boot, ncol = length(x_rows))
                              colnames(boot_indirect) <- x_rows
                              
                              cat("Bootstrapping indirect effects...\n")
                              for (i in 1:n_boot) {
                                boot_sample <- data[sample(nrow(data), replace = TRUE), ]
                                b_med_model <- lm(as.formula(paste(mediator_var, "~", demographic_var)), data = boot_sample)
                                b_dir_model <- lm(as.formula(paste(outcome_var, "~", demographic_var, "+", mediator_var)), data = boot_sample)
                                
                                b_a <- summary(b_med_model)$coefficients[x_rows, "Estimate"]
                                b_b <- summary(b_dir_model)$coefficients[m_row, "Estimate"]
                                boot_indirect[i, ] <- b_a * b_b
                              }
                              
                              # Calculate CIs and Proportion Mediated
                              boot_cis <- apply(boot_indirect, 2, quantile, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
                              prop_mediated <- ifelse(total_effects != 0, indirect_effects / total_effects, NA)
                              
                              # Store mediation results
                              results$mediation <- list(
                                total_effects = total_effects,
                                total_ps = total_ps,
                                a_effects = a_effects,
                                a_ps = a_ps,
                                direct_effects = direct_effects,
                                direct_ps = direct_ps,
                                b_effect = b_effect,
                                b_p = b_p,
                                indirect_effects = indirect_effects,
                                indirect_cis = boot_cis,
                                proportion_mediated = prop_mediated,
                                is_categorical = is_x_categorical
                              )
                              
                              # Print Mediation Results
                              for (r in x_rows) {
                                cat(sprintf("\n--- Path: %s ---\n", r))
                                cat(sprintf("Total effect: %.4f (p = %.4f)\n", total_effects[r], total_ps[r]))
                                cat(sprintf("Effect on mediator (a): %.4f (p = %.4f)\n", a_effects[r], a_ps[r]))
                                cat(sprintf("Direct effect (c'): %.4f (p = %.4f)\n", direct_effects[r], direct_ps[r]))
                                cat(sprintf("Indirect effect (a*b): %.4f [%.4f, %.4f]\n", 
                                            indirect_effects[r], boot_cis[1, r], boot_cis[2, r]))
                                sig <- (boot_cis[1, r] > 0 || boot_cis[2, r] < 0)
                                cat(sprintf("Mediation significant: %s\n", ifelse(sig, "Yes", "No")))
                              }
                              cat(sprintf("\nEffect of %s on %s (b): %.4f (p = %.4f)\n", mediator_var, outcome_var, b_effect, b_p))
                              
                              # ============================================================================
                              # Part 2: Moderation Analysis
                              # ============================================================================
                              if (!is.null(moderator_var)) {
                                cat("\n=== MODERATION ANALYSIS ===\n")
                                
                                # Use R's formula interface with * to handle categorical interactions automatically
                                mod_model <- lm(as.formula(paste(outcome_var, "~", demographic_var, "*", moderator_var)), data = data)
                                mod_sum <- summary(mod_model)
                                coef_names <- rownames(mod_sum$coefficients)
                                
                                # Find main and interaction rows dynamically
                                x_main_rows <- get_var_rows(coef_names, demographic_var)
                                m_main_rows <- get_var_rows(coef_names, moderator_var)
                                int_rows <- grep(":", coef_names, value = TRUE)
                                
                                if (length(int_rows) == 0) stop("No interaction term found. Check variables.")
                                
                                int_effects <- mod_sum$coefficients[int_rows, "Estimate"]
                                int_ps <- mod_sum$coefficients[int_rows, "Pr(>|t|)"]
                                mod_sig <- any(int_ps < alpha)
                                
                                # Simple Slopes Analysis
                                simple_slopes <- NULL
                                if (is.numeric(data[[moderator_var]])) {
                                  mod_mean <- mean(data[[moderator_var]], na.rm = TRUE)
                                  mod_sd <- sd(data[[moderator_var]], na.rm = TRUE)
                                  mod_vals <- c(mod_mean - mod_sd, mod_mean, mod_mean + mod_sd)
                                  
                                  vcov_mat <- vcov(mod_model)
                                  slopes_list <- list()
                                  for (xr in x_main_rows) {
                                    int_r <- grep(paste0(xr, ":"), int_rows, value = TRUE)
                                    if (length(int_r) == 0) int_r <- grep(paste0(":", xr), int_rows, value = TRUE)
                                    
                                    b1 <- mod_sum$coefficients[xr, "Estimate"]
                                    b3 <- mod_sum$coefficients[int_r, "Estimate"]
                                    var_b1 <- vcov_mat[xr, xr]
                                    var_b3 <- vcov_mat[int_r, int_r]
                                    cov_b1_b3 <- vcov_mat[xr, int_r]
                                    
                                    sl <- b1 + (b3 * mod_vals)
                                    se_sl <- sqrt(var_b1 + (mod_vals^2 * var_b3) + (2 * mod_vals * cov_b1_b3))
                                    t_sl <- sl / se_sl
                                    p_sl <- 2 * pt(abs(t_sl), df = mod_model$df.residual, lower.tail = FALSE)
                                    
                                    slopes_list[[xr]] <- data.frame(moderator_value = mod_vals, slope = sl, se = se_sl, t = t_sl, p = p_sl)
                                  }
                                  simple_slopes <- slopes_list
                                  cat("Moderator is numeric. Simple slopes calculated at Mean ± SD.\n")
                                  
                                } else {
                                  cat("Moderator is categorical. Interaction terms represent group differences.\n")
                                  simple_slopes <- "Categorical moderator detected. Use emmeans::emtrends() for detailed simple slopes."
                                }
                                
                                # Store moderation results
                                results$moderation <- list(
                                  x_main_effects = mod_sum$coefficients[x_main_rows, "Estimate"],
                                  m_main_effects = mod_sum$coefficients[m_main_rows, "Estimate"],
                                  interaction_effects = int_effects,
                                  interaction_ps = int_ps,
                                  simple_slopes = simple_slopes,
                                  moderation_significant = mod_sig
                                )
                                
                                # Print Moderation Results
                                for (ir in int_rows) {
                                  cat(sprintf("Interaction (%s): %.4f (p = %.4f)\n", ir, int_effects[ir], int_ps[ir]))
                                }
                                cat(sprintf("Overall moderation significant: %s\n", ifelse(mod_sig, "Yes", "No")))
                              }
                              
                              return(results)
                            },
                            # Method 4: Lavaan Mediation Analysis (Handles Multi-Level Categorical X)
                            # ==============================================================================
                            perform_lavaan_mediation = function(data, 
                                                                demographic_var, 
                                                                outcome_var, 
                                                                mediator_var, 
                                                                reference_group = NULL,
                                                                n_boot = 1000, 
                                                                alpha = 0.05) {
                              
                              if (!requireNamespace("lavaan", quietly = TRUE)) {
                                stop("Package 'lavaan' is required for this analysis. Install with install.packages('lavaan')")
                              }
                              
                              # Drop NAs for the variables used
                              target_cols <- c(demographic_var, outcome_var, mediator_var)
                              analysis_data <- data[complete.cases(data[, target_cols, drop = FALSE]), ]
                              
                              is_categorical <- is.factor(analysis_data[[demographic_var]]) || is.character(analysis_data[[demographic_var]])
                              
                              cat("=== LAVAAN MEDIATION ANALYSIS ===\n")
                              
                              if (is_categorical) {
                                # --- CATEGORICAL X LOGIC ---
                                analysis_data[[demographic_var]] <- as.factor(analysis_data[[demographic_var]])
                                levels_x <- levels(analysis_data[[demographic_var]])
                                
                                # Set reference group (default is the first level)
                                if (is.null(reference_group)) {
                                  reference_group <- levels_x[1]
                                }
                                if (!reference_group %in% levels_x) stop("Reference group not found in demographic_var levels.")
                                
                                non_ref_levels <- setdiff(levels_x, reference_group)
                                
                                # Create safe dummy variable names (removes spaces and hyphens for lavaan syntax)
                                dummy_names <- make.names(paste0(demographic_var, "_", non_ref_levels))
                                
                                # Create dummy columns in the dataset
                                for (i in seq_along(non_ref_levels)) {
                                  analysis_data[[dummy_names[i]]] <- ifelse(analysis_data[[demographic_var]] == non_ref_levels[i], 1, 0)
                                }
                                
                                # Build Lavaan Syntax: M ~ X
                                m_syntax <- paste0(mediator_var, " ~ ", paste0("a", 1:length(dummy_names), "*", dummy_names, collapse = " + "))
                                # Build Lavaan Syntax: Y ~ X + M
                                y_syntax <- paste0(outcome_var, " ~ ", paste0("c_prime", 1:length(dummy_names), "*", dummy_names, collapse = " + "), " + b*", mediator_var)
                                
                                # Build Indirect Effects Syntax (Each level vs Reference)
                                safe_ind_names <- paste0("ind_", dummy_names)
                                ind_syntax <- paste0(paste0(safe_ind_names, " := a", 1:length(dummy_names), "*b"), collapse = "\n")
                                
                                model_syntax <- paste(m_syntax, y_syntax, ind_syntax, sep = "\n")
                                
                                cat("Demographic Variable: ", demographic_var, "(Categorical)\n")
                                cat("Reference Group: ", reference_group, "\n")
                                cat("Comparisons: ", paste(non_ref_levels, collapse = ", "), "\n\n")
                                
                              } else {
                                # --- CONTINUOUS X LOGIC ---
                                m_syntax <- paste0(mediator_var, " ~ a*", demographic_var)
                                y_syntax <- paste0(outcome_var, " ~ c_prime*", demographic_var, " + b*", mediator_var)
                                ind_syntax <- "ind_overall := a*b"
                                
                                model_syntax <- paste(m_syntax, y_syntax, ind_syntax, sep = "\n")
                                cat("Demographic Variable: ", demographic_var, "(Continuous)\n\n")
                              }
                              
                              cat("Lavaan Model Syntax:\n", model_syntax, "\n\n")
                              
                              # Fit the model with Bootstrapping
                              cat("Fitting model with bootstrapping (n_boot =", n_boot, ")... Please wait.\n")
                              fit <- tryCatch({
                                lavaan::sem(model_syntax, data = analysis_data, se = "bootstrap", bootstrap = n_boot)
                              }, error = function(e) {
                                warning("Bootstrapping failed (likely small sample in a group). Falling back to standard errors. Error: ", e$message)
                                lavaan::sem(model_syntax, data = analysis_data)
                              })
                              
                              # Extract and print results
                              cat("\n=== MEDIATION RESULTS ===\n")
                              
                              # Get parameter estimates with bootstrapped confidence intervals
                              estimates <- lavaan::parameterEstimates(fit, boot.ci.type = "perc", standardized = TRUE)
                              
                              # Filter to show only direct and indirect effects
                              effects_to_show <- estimates[estimates$op %in% c("~", ":="), ]
                              print(effects_to_show[, c("lhs", "op", "rhs", "est", "ci.lower", "ci.upper", "pvalue")])
                              
                              # Return the lavaan fit object and estimates
                              return(invisible(list(
                                fit = fit,
                                estimates = estimates,
                                syntax = model_syntax,
                                is_categorical = is_categorical
                              )))
                            }, 
                            # ==============================================================================
                            # Method 2: Sensitivity Analysis and Robustness Testing
                            # ==============================================================================
                            perform_sensitivity_analysis = function(data, 
                                                                     model_type = "linear",
                                                                     dependent_var,
                                                                     independent_vars,
                                                                     outlier_method = "iqr",
                                                                     missing_data_methods = c("listwise", "mean_imputation"),
                                                                     alpha = 0.05) {
                              
                              results = list()
                              cat("=== SENSITIVITY ANALYSIS AND ROBUSTNESS TESTING ===\n\n")
                              
                              # Part 1: Outlier Detection
                              cat("1. OUTLIER ANALYSIS\n")
                              
                              detect_outliers = function(data_frame, var, method = "iqr") {
                                clean_vec = na.omit(data_frame[[var]])
                                if (length(clean_vec) == 0) return(integer(0))
                                
                                if (method == "iqr") {
                                  Q1 = quantile(clean_vec, 0.25)
                                  Q3 = quantile(clean_vec, 0.75)
                                  IQR = Q3 - Q1
                                  lower_bound = Q1 - 1.5 * IQR
                                  upper_bound = Q3 + 1.5 * IQR
                                  outliers = which(data_frame[[var]] < lower_bound | data_frame[[var]] > upper_bound)
                                } else if (method == "zscore") {
                                  z_scores = scale(data_frame[[var]])
                                  outliers = which(abs(z_scores) > 3)
                                }
                                return(outliers)
                              }
                              
                              outlier_info = list()
                              for (var in independent_vars) {
                                outliers = detect_outliers(data, var, outlier_method)
                                outlier_info[[var]] = list(
                                  n_outliers = length(outliers),
                                  outlier_indices = outliers,
                                  outlier_percentage = length(outliers) / nrow(data) * 100
                                )
                                cat(sprintf("%s: %d outliers (%.2f%%)\n", var, length(outliers), length(outliers) / nrow(data) * 100))
                              }
                              
                              cat("\nOutlier Impact Analysis:\n")
                              outlier_impact = list()
                              
                              if (model_type == "linear") {
                                formula_obj = as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))
                                original_model = lm(formula_obj, data = data)
                                original_summary = summary(original_model)
                                original_r_squared = original_summary$r.squared
                                original_adj_r_squared = original_summary$adj.r.squared
                                
                                outlier_impact$original = list(
                                  r_squared = original_r_squared,
                                  adj_r_squared = original_adj_r_squared,
                                  coefficients = original_summary$coefficients
                                )
                                cat(sprintf("Original model (no outliers removed): R² = %.4f, Adj R² = %.4f\n", original_r_squared, original_adj_r_squared))
                              }
                              
                              for (var in independent_vars) {
                                outliers = outlier_info[[var]]$outlier_indices
                                if (length(outliers) > 0) {
                                  clean_data = data[-outliers, ]
                                  
                                  if (model_type == "linear") {
                                    clean_model = lm(formula_obj, data = clean_data)
                                    clean_summary = summary(clean_model)
                                    clean_r_squared = clean_summary$r.squared
                                    clean_adj_r_squared = clean_summary$adj.r.squared
                                    
                                    outlier_impact[[paste(var, "clean", sep = "_")]] = list(
                                      r_squared = clean_r_squared,
                                      adj_r_squared = clean_adj_r_squared,
                                      coefficients = clean_summary$coefficients
                                    )
                                    cat(sprintf("After removing %s outliers: R² = %.4f, Adj R² = %.4f (ΔR² = %.4f)\n", 
                                                var, clean_r_squared, clean_adj_r_squared, clean_r_squared - original_r_squared))
                                  }
                                }
                              }
                              
                              results$outlier_analysis = list(
                                outlier_method = outlier_method,
                                outlier_info = outlier_info,
                                impact_on_models = outlier_impact
                              )
                              
                              # Part 2: Missing Data Handling Sensitivity
                              cat("\n2. MISSING DATA HANDLING SENSITIVITY\n")
                              
                              missing_data_results = list()
                              target_cols = c(dependent_var, independent_vars)
                              original_data_complete = data[complete.cases(data[, target_cols, drop = FALSE]), ]
                              
                              cat(sprintf("Complete cases: %d/%d (%.2f%%)\n", 
                                          nrow(original_data_complete), nrow(data), nrow(original_data_complete) / nrow(data) * 100))
                              
                              if (model_type == "linear") {
                                comp_model = lm(formula_obj, data = original_data_complete)
                                comp_summary = summary(comp_model)
                                
                                missing_data_results$listwise = list(
                                  method = "listwise_deletion",
                                  n_obs = nrow(original_data_complete),
                                  r_squared = comp_summary$r.squared,
                                  adj_r_squared = comp_summary$adj.r.squared,
                                  coefficients = comp_summary$coefficients
                                )
                              }
                              
                              if ("mean_imputation" %in% missing_data_methods) {
                                mean_imputed_data = data
                                for (var in target_cols) {
                                  if (is.numeric(mean_imputed_data[[var]]) && any(is.na(mean_imputed_data[[var]]))) {
                                    mean_val = mean(mean_imputed_data[[var]], na.rm = TRUE)
                                    mean_imputed_data[[var]][is.na(mean_imputed_data[[var]])] = mean_val
                                  }
                                }
                                
                                if (model_type == "linear") {
                                  mean_model = lm(formula_obj, data = mean_imputed_data)
                                  mean_summary = summary(mean_model)
                                  
                                  missing_data_results$mean_imputation = list(
                                    method = "mean_imputation",
                                    n_obs = nrow(mean_imputed_data),
                                    r_squared = mean_summary$r.squared,
                                    adj_r_squared = mean_summary$adj.r.squared,
                                    coefficients = mean_summary$coefficients
                                  )
                                }
                              }
                              
                              cat("\nMissing Data Method Comparison:\n")
                              for (method_name in names(missing_data_results)) {
                                m_res = missing_data_results[[method_name]]
                                cat(sprintf("%s: n = %d, R² = %.4f, Adj R² = %.4f\n", m_res$method, m_res$n_obs, m_res$r_squared, m_res$adj_r_squared))
                              }
                              
                              results$missing_data_analysis = missing_data_results
                              
                              # Part 3: Assumption Validation
                              cat("\n3. ASSUMPTION VALIDATION\n")
                              assumption_results = list()
                              
                              if (model_type == "linear" && nrow(original_data_complete) > 0) {
                                model = lm(formula_obj, data = original_data_complete)
                                resids = residuals(model)
                                
                                # Residual Normality
                                shapiro_test = if(length(resids) <= 5000) shapiro.test(resids) else list(statistic=NA, p.value=NA)
                                assumption_results$normality = list(
                                  test = "Shapiro-Wilk",
                                  statistic = shapiro_test$statistic,
                                  p_value = shapiro_test$p.value,
                                  normal = if(!is.na(shapiro_test$p.value)) shapiro_test$p.value > alpha else "N/A (N > 5000)"
                                )
                                
                                # Homogeneity of variance using lmtest safely
                                bp_p_val = NA
                                bp_stat = NA
                                if (requireNamespace("lmtest", quietly = TRUE)) {
                                  bp_test = lmtest::bptest(model)
                                  bp_p_val = bp_test$p.value
                                  bp_stat = bp_test$statistic
                                }
                                
                                assumption_results$homogeneity = list(
                                  test = "Breusch-Pagan",
                                  statistic = bp_stat,
                                  p_value = bp_p_val,
                                  homogeneous = if(!is.na(bp_p_val)) bp_p_val > alpha else "lmtest package missing"
                                )
                                
                                assumption_results$linearity = list(max_residual_abs = max(abs(resids)))
                                
                                cat(sprintf("Normality of residuals (Shapiro-Wilk): p = %s\n", 
                                            if(!is.na(shapiro_test$p.value)) round(shapiro_test$p.value, 5) else "N/A"))
                                cat(sprintf("Homogeneity of variance (Breusch-Pagan): p = %s\n", 
                                            if(!is.na(bp_p_val)) round(bp_p_val, 5) else "N/A"))
                              }
                              results$assumption_validation = assumption_results
                              
                              # Part 4: Subgroup Consistency Analysis
                              cat("\n4. SUBGROUP CONSISTENCY ANALYSIS\n")
                              demographic_vars = c("Gender", "Occupation", "Location.of.Leaving", "age", "Weight")
                              subgroup_vars = demographic_vars[demographic_vars %in% colnames(data)]
                              subgroup_results = list()
                              
                              if (model_type == "linear" && length(subgroup_vars) > 0) {
                                for (subgroup_var in subgroup_vars) {
                                  if (is.factor(data[[subgroup_var]]) || is.character(data[[subgroup_var]])) {
                                    lvls = unique(na.omit(as.character(data[[subgroup_var]])))
                                    level_models = list()
                                    
                                    for (lvl in lvls) {
                                      sub_data = data[which(data[[subgroup_var]] == lvl), ]
                                      sub_data_complete = sub_data[complete.cases(sub_data[, target_cols, drop = FALSE]), ]
                                      
                                      if (nrow(sub_data_complete) >= (length(independent_vars) + 5)) {
                                        lvl_model = lm(formula_obj, data = sub_data_complete)
                                        lvl_sum = summary(lvl_model)
                                        level_models[[lvl]] = list(n_obs = nrow(sub_data_complete), r_squared = lvl_sum$r.squared)
                                      }
                                    }
                                    subgroup_results[[subgroup_var]] = list(variable_type = "categorical", level_models = level_models)
                                    
                                    cat(sprintf("\n%s Subgroup Analysis:\n", subgroup_var))
                                    for (lvl in names(level_models)) {
                                      cat(sprintf("  %s (n = %d): R² = %.4f\n", lvl, level_models[[lvl]]$n_obs, level_models[[lvl]]$r_squared))
                                    }
                                    
                                  } else if (is.numeric(data[[subgroup_var]])) {
                                    med_split = median(data[[subgroup_var]], na.rm = TRUE)
                                    low_group = data[which(data[[subgroup_var]] <= med_split), ]
                                    high_group = data[which(data[[subgroup_var]] > med_split), ]
                                    
                                    low_comp = low_group[complete.cases(low_group[, target_cols, drop = FALSE]), ]
                                    high_comp = high_group[complete.cases(high_group[, target_cols, drop = FALSE]), ]
                                    
                                    cat(sprintf("\n%s Subgroup Analysis (median split = %.2f):\n", subgroup_var, med_split))
                                    if (nrow(low_comp) > length(independent_vars)) {
                                      l_sum = summary(lm(formula_obj, data = low_comp))
                                      cat(sprintf("  Low group (n = %d): R² = %.4f\n", nrow(low_comp), l_sum$r_squared))
                                    }
                                    if (nrow(high_comp) > length(independent_vars)) {
                                      h_sum = summary(lm(formula_obj, data = high_comp))
                                      cat(sprintf("  High group (n = %d): R² = %.4f\n", nrow(high_comp), h_sum$r_squared))
                                    }
                                  }
                                }
                              }
                              results$subgroup_analysis = subgroup_results
                              return(results)
                            },
                            
                            # ==============================================================================
                            # Method 3: Effect Size Calculation and Standardized Reporting
                            # ==============================================================================
                            calculate_effect_sizes = function(data, 
                                                               model_type = "linear",
                                                               dependent_var,
                                                               independent_vars,
                                                               test_type = "all",
                                                               alpha = 0.05) {
                              
                              results = list()
                              cat("=== EFFECT SIZE CALCULATION AND STANDARDIZED REPORTING ===\n\n")
                              target_cols = c(dependent_var, independent_vars)
                              complete_data = data[complete.cases(data[, target_cols, drop = FALSE]), ]
                              
                              # Part 1: Linear Models
                              if (model_type == "linear" && test_type %in% c("all", "linear")) {
                                cat("1. LINEAR MODEL EFFECT SIZES\n")
                                formula_obj = as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))
                                model = lm(formula_obj, data = complete_data)
                                model_summary = summary(model)
                                
                                ss_total = sum((complete_data[[dependent_var]] - mean(complete_data[[dependent_var]]))^2)
                                anova_table = anova(model)
                                
                                # Extracted securely by stripping the residual row out of calculation parameters
                                ss_residual = anova_table["Residuals", "Sum Sq"]
                                ss_regression = ss_total - ss_residual
                                ms_residual = anova_table["Residuals", "Mean Sq"]
                                
                                effect_sizes_linear = list(
                                  model_level = list(
                                    r_squared = model_summary$r.squared,
                                    adj_r_squared = model_summary$adj.r.squared,
                                    eta_squared = ss_regression / ss_total,
                                    omega_squared = (ss_regression - (length(independent_vars) * ms_residual)) / (ss_total + ms_residual)
                                  ),
                                  predictor_level = list()
                                )
                                
                                # Standardized coefficients (Beta) calculation
                                std_coefs = numeric()
                                sd_y = sd(complete_data[[dependent_var]])
                                for (var in independent_vars) {
                                  if (var %in% rownames(model_summary$coefficients)) {
                                    std_coefs[var] = model_summary$coefficients[var, "Estimate"] * (sd(complete_data[[var]]) / sd_y)
                                  }
                                }
                                effect_sizes_linear$standardized_coefficients = std_coefs
                                
                                # Row indexing for partial effect calculations
                                for (var in independent_vars) {
                                  if (var %in% rownames(anova_table)) {
                                    ss_p = anova_table[var, "Sum Sq"]
                                    effect_sizes_linear$predictor_level[[var]] = list(
                                      eta_squared = ss_p / ss_total,
                                      omega_squared = (ss_p - anova_table[var, "Df"] * ms_residual) / (ss_total + ms_residual)
                                    )
                                  }
                                }
                                
                                cat(sprintf("Model R² = %.4f | Model Omega² = %.4f\n", effect_sizes_linear$model_level$r_squared, effect_sizes_linear$model_level$omega_squared))
                                results$linear_model_effects = effect_sizes_linear
                              }
                              
                              # Part 2: ANOVA Effect Size Calculation
                              if (test_type %in% c("all", "anova")) {
                                cat("\n2. ANOVA EFFECT SIZES\n")
                                categorical_vars = names(which(sapply(complete_data, is.factor)))
                                anova_effects = list()
                                
                                for (f_var in categorical_vars) {
                                  if (nlevels(complete_data[[f_var]]) > 2) {
                                    a_mod = aov(as.formula(paste(dependent_var, "~", f_var)), data = complete_data)
                                    a_sum = summary(a_mod)[[1]]
                                    
                                    ss_between = a_sum[f_var, "Sum Sq"]
                                    ss_res     = a_sum["Residuals", "Sum Sq"]
                                    ss_tot     = ss_between + ss_res
                                    ms_res     = a_sum["Residuals", "Mean Sq"]
                                    df_between = a_sum[f_var, "Df"]
                                    
                                    eta_sq = ss_between / ss_tot
                                    omega_sq = (ss_between - (df_between * ms_res)) / (ss_tot + ms_res)
                                    
                                    anova_effects[[f_var]] = list(eta_squared = eta_sq, omega_squared = omega_sq)
                                    cat(sprintf("%s: η² = %.4f, ω² = %.4f\n", f_var, eta_sq, omega_sq))
                                  }
                                }
                                results$anova_effects = anova_effects
                              }
                              
                              # Part 3: t-tests and Non-Parametric Rank Effect Size
                              if (test_type %in% c("all", "ttest")) {
                                cat("\n3. BINARY COMPARISON EFFECT SIZES\n")
                                binary_vars = names(which(sapply(complete_data, function(x) is.factor(x) && nlevels(x) == 2)))
                                ttest_effects = list()
                                mw_effects = list()
                                
                                for (g_var in binary_vars) {
                                  lvls = levels(complete_data[[g_var]])
                                  g1 = complete_data[[dependent_var]][which(complete_data[[g_var]] == lvls[1])]
                                  g2 = complete_data[[dependent_var]][which(complete_data[[g_var]] == lvls[2])]
                                  
                                  if (length(g1) >= 2 && length(g2) >= 2) {
                                    # t-Test Cohen's d Calculations
                                    n1 = length(g1); n2 = length(g2)
                                    pooled_sd = sqrt(((n1 - 1) * var(g1) + (n2 - 1) * var(g2)) / (n1 + n2 - 2))
                                    d_val = if(pooled_sd > 0) abs(mean(g1) - mean(g2)) / pooled_sd else 0
                                    
                                    ttest_effects[[g_var]] = list(cohens_d = d_val, hedges_g = d_val * (1 - 3 / (4 * (n1 + n2) - 9)))
                                    cat(sprintf("%s: Cohen's d = %.4f\n", g_var, d_val))
                                    
                                    # Non-parametric Rank Biserial Calculation
                                    if (shapiro.test(g1)$p.value < 0.05 || shapiro.test(g2)$p.value < 0.05) {
                                      w_test = wilcox.test(g1, g2, exact = FALSE)
                                      # Map Rank Biserial Correlation
                                      r_biserial = 1 - (2 * w_test$statistic) / (as.numeric(n1) * n2)
                                      mw_effects[[g_var]] = list(rank_biserial = r_biserial)
                                      cat(sprintf("%s (Non-Normal): Rank-Biserial r = %.4f\n", g_var, r_biserial))
                                    }
                                  }
                                }
                                results$ttest_effects = ttest_effects
                                results$mannwhitney_effects = mw_effects
                              }
                              
                              # Part 4: Chi-Square Cramér's V 
                              if (test_type %in% c("all", "chisquare")) {
                                cat("\n4. CHI-SQUARE TEST EFFECT SIZES\n")
                                cat_vars = names(which(sapply(complete_data, is.factor)))
                                chisquare_effects = list()
                                
                                if (length(cat_vars) >= 2) {
                                  pairs = combn(cat_vars, 2, simplify = FALSE)
                                  for (pair in pairs) {
                                    c_table = table(complete_data[[pair[1]]], complete_data[[pair[2]]])
                                    suppressWarnings(chi_res = chisq.test(c_table))
                                    
                                    n_total = sum(c_table)
                                    min_dim = min(dim(c_table)) - 1
                                    v_cramer = if(min_dim > 0) sqrt(chi_res$statistic / (n_total * min_dim)) else 0
                                    
                                    chisquare_effects[[paste(pair, collapse="_vs_")]] = list(cramers_v = unname(v_cramer))
                                    cat(sprintf("%s vs %s: Cramér's V = %.4f\n", pair[1], pair[2], v_cramer))
                                  }
                                }
                                results$chisquare_effects = chisquare_effects
                              }
                              
                              # Part 5: Logistic Regression Effect Size
                              if (test_type %in% c("all", "logistic")) {
                                cat("\n5. LOGISTIC REGRESSION EFFECT SIZES\n")
                                if (is.factor(complete_data[[dependent_var]]) && nlevels(complete_data[[dependent_var]]) == 2) {
                                  log_mod = glm(as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+"))), family = binomial, data = complete_data)
                                  null_mod = glm(as.formula(paste(dependent_var, "~ 1")), family = binomial, data = complete_data)
                                  
                                  mcfadden = 1 - (logLik(log_mod) / logLik(null_mod))
                                  results$logistic_effects = list(mcfadden_pseudo_r2 = as.numeric(mcfadden), odds_ratios = exp(coef(log_mod)))
                                  cat(sprintf("McFadden's Pseudo R² = %.4f\n", mcfadden))
                                } else {
                                  cat("Dependent variable is not binary (factor with 2 levels) - skipping.\n")
                                }
                              }
                              
                              return(results)
                            }, 
                            
                            # ==============================================================================
                            # Class Extractors & Method Wrappers (R6 Compatible S3 Interface Formatting)
                            # ==============================================================================
                            extract_effect_sizes = function(test_result, test_type, data_env = NULL) {
                              if (is.null(test_result)) stop("No valid test result metadata structure provided.")
                              effect_sizes = list()
                              
                              # Self reference configuration setup to adaptively capture if run inside an R6 context
                              interpret_wrapper = function(val, type) {
                                if (!is.null(data_env) && "interpret_cohen_d" %in% names(data_env)) {
                                  if (type == "d") return(data_env$interpret_cohen_d(val))
                                  if (type == "r") return(data_env$interpret_correlation(val))
                                  if (type == "eta") return(data_env$interpret_eta_squared(val))
                                }
                                return("N/A")
                              }
                              
                              if (test_type == "t_test") {
                                if (!is.null(test_result$statistic) && !is.null(test_result$parameter)) {
                                  df = test_result$parameter
                                  # Extrapolate true baseline variance parameters from sample standard error matrix allocations
                                  cohen_d = unname(test_result$estimate / (test_result$stderr * sqrt(df + 1)))
                                  effect_sizes$cohen_d = cohen_d
                                  effect_sizes$interpretation = interpret_wrapper(cohen_d, "d")
                                }
                              } else if (test_type == "chi_square") {
                                if (!is.null(test_result$statistic)) {
                                  n_total = if(!is.null(test_result$observed)) sum(test_result$observed) else NA
                                  if (!is.na(n_total)) {
                                    phi_coeff = unname(sqrt(test_result$statistic / n_total))
                                    effect_sizes$phi_coefficient = phi_coeff
                                    effect_sizes$interpretation = interpret_wrapper(phi_coeff, "r")
                                  }
                                }
                              } else if (test_type == "regression") {
                                if (!is.null(test_result$r.squared)) {
                                  effect_sizes$r_squared = test_result$r.squared
                                } else if (inherits(test_result, "lm")) {
                                  effect_sizes$r_squared = summary(test_result)$r.squared
                                }
                              }
                              return(effect_sizes)
                            },
                            ###========================================================================
                            ## New method
                            perform_stratified_analysis = function(data, outcome, predictors, stratify_by_var) {
                              if (!outcome %in% names(data) || !stratify_by_var %in% names(data)) {
                                stop("Specified outcome or stratification columns are missing.")
                              }
                              
                              strata = unique(na.omit(data[[stratify_by_var]]))
                              stratified_results = list()
                              
                              cat(sprintf("Running analysis pipeline across %d distinct strata levels...\n", length(strata)))
                              formula_obj = as.formula(paste(outcome, "~", paste(predictors, collapse = "+")))
                              
                              for (lvl in strata) {
                                stratum_name = paste0(stratify_by_var, "_", lvl)
                                sub_data = data[which(data[[stratify_by_var]] == lvl), ]
                                clean_sub = sub_data[complete.cases(sub_data[, c(outcome, predictors), drop = FALSE]), ]
                                
                                # Structural verification step to filter out volatile data groups
                                if (nrow(clean_sub) < (length(predictors) + 5)) {
                                  # Custom workflow mapping control layout instead of an illegal parenthetical next() call
                                  cat(sprintf("Skipping stratum %s: Insufficient degrees of freedom (N=%d).\n", lvl, nrow(clean_sub)))
                                  next
                                }
                                
                                fit = lm(formula_obj, data = clean_sub)
                                stratified_results[[stratum_name]] = list(n_obs = nrow(clean_sub), r_squared = summary(fit)$r.squared, model = fit)
                              }
                              return(stratified_results)
                            },
                            
                            
                            
                        
                            
                            #===================================================
                            # New Method: List all methods in the class**
                            list_all_methods = function() {
                              # Get all public members
                              all_members = ls(self)
                              # Filter to methods (functions), excluding properties
                              methods = all_members[sapply(all_members, function(x) is.function(self[[x]]))]
                              return(sort(methods))
                            }
                          ),
                       
                       private = list(
                         # Private method for internal validation
                         validate_column = function(column) {
                           if (!column %in% names(self$data)) {
                             stop("Column '", column, "' not found in data")
                           }
                         },
                         # ============================================================================
                         # HELPER FUNCTION (Place in private list of R6 class)
                         # ============================================================================
                         # Helper function to interpret Cramér's V (common conventions)
                         # Different sources use slightly different cutoffs; this follows Cohen (1988)
                         interpret_cramers_v = function(v) {
                           if (is.na(v) || v < 0) return("Invalid V")
                           if (v < 0.1) return("Negligible association")
                           if (v < 0.3) return("Weak association")
                           if (v < 0.5) return("Moderate association")
                           if (v < 1.0) return("Strong association")
                           return("Perfect association")
                         }
                       )
)

                            