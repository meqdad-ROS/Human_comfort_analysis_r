# Install R6 package if needed
# install.packages("R6")

library(R6)

# Define a DataAnalyzer class using R6
regrissorClass <- R6Class("regrissorClass",
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
                              self$data <- tryCatch({
                                read.csv(input, sep = file_sep, header = header, 
                                         stringsAsFactors = stringsAsFactors, 
                                         na.strings = na.strings)
                              }, error = function(e) {
                                stop("Error loading CSV: ", e$message)
                              })
                              self$data_source <- paste("File:", input)
                              cat("Data loaded from file:", input, "\n")
                            } else if (is.data.frame(input)) {
                              # Direct data frame input
                              self$data <- input
                              self$data_source <- "Direct data.frame"
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
                            self$data <- self$data[rowSums(is.na(self$data)) != ncol(self$data), ]  # Remove all-NA rows
                            self$data <- self$data[, colSums(is.na(self$data)) != nrow(self$data)]  # Remove all-NA columns
                            
                            # Convert empty strings to NA (common in CSVs)
                            self$data[self$data == ""] <- NA
                            
                            # Step 4: Identify column types
                            self$numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
                            self$categorical_cols <- names(self$data)[sapply(self$data, function(x) !is.numeric(x))]
                            
                            cat("DataAnalyzer initialized with", nrow(self$data), "rows and", ncol(self$data), "columns\n")
                            cat("Numeric columns:", length(self$numeric_cols), "| Categorical columns:", length(self$categorical_cols), "\n")
                            cat("Data source:", self$data_source, "\n")
                          },
                          
                          # ... (Rest of the methods remain the same for now)
                          
                          # Method to calculate summary statistics
                          calculate_summary = function() {
                            if (is.null(self$data)) {
                              stop("No data available")
                            }
                            self$summary_stats <- list(
                              mean_values = sapply(self$data[self$numeric_cols], mean, na.rm = TRUE),
                              median_values = sapply(self$data[self$numeric_cols], median, na.rm = TRUE),
                              sd_values = sapply(self$data[self$numeric_cols], sd, na.rm = TRUE),
                              missing_values = sapply(self$data, function(x) sum(is.na(x)))
                            )
                            cat("Summary statistics calculated\n")
                            return(invisible(self))
                          },
                          
                          # Method to print summary
                          print_summary = function() {
                            if (is.null(self$summary_stats)) {
                              cat("No summary statistics available. Run calculate_summary() first.\n")
                              return(invisible(self))
                            }
                            
                            cat("\n=== Summary Statistics ===\n")
                            cat("\nMean values:\n")
                            print(self$summary_stats$mean_values)
                            cat("\nMedian values:\n")
                            print(self$summary_stats$median_values)
                            cat("\nStandard deviations:\n")
                            print(self$summary_stats$sd_values)
                            cat("\nMissing values:\n")
                            print(self$summary_stats$missing_values)
                            
                            return(invisible(self))
                          },
                          
                          # Method to filter data
                          filter_data = function(column, condition) {
                            filtered <- subset(self$data, eval(parse(text = paste(column, condition))))
                            cat("Filtered to", nrow(filtered), "rows\n")
                            return(filtered)
                          },
                          
                          # Method to add a new column
                          add_column = function(column_name, values) {
                            if (length(values) != nrow(self$data)) {
                              stop("Length of values must match number of rows")
                            }
                            self$data[[column_name]] <- values
                            cat("Added column:", column_name, "\n")
                            return(invisible(self))
                          },
                          
                          # Method to visualize (Histogram) data
                          plot_histogram = function(column) {
                            if (!column %in% names(self$data)) {
                              stop("Column not found in data")
                            }
                            if (!is.numeric(self$data[[column]])) {
                              stop("Column must be numeric")
                            }
                            
                            hist(self$data[[column]], 
                                 main = paste("Histogram of", column),
                                 xlab = column,
                                 col = "steelblue",
                                 border = "white")
                            
                            return(invisible(self))
                          },
                          
                          # Method to visualize (scatter-plot) data
                          plot_scatter_by_group = function(x_column, y_column, category_column) {
                            # Validate inputs
                            if (!x_column %in% names(self$data)) {
                              stop("X-column '", x_column, "' not found in data")
                            }
                            if (!y_column %in% names(self$data)) {
                              stop("Y-column '", y_column, "' not found in data")
                            }
                            if (!category_column %in% names(self$data)) {
                              stop("Category column '", category_column, "' not found in data")
                            }
                            if (!is.numeric(self$data[[x_column]])) {
                              stop("X-column must be numeric")
                            }
                            if (!is.numeric(self$data[[y_column]])) {
                              stop("Y-column must be numeric")
                            }
                            if (is.numeric(self$data[[category_column]])) {
                              stop("Category column must be categorical (not numeric)")
                            }
                            
                            # Prepare data: Remove NA rows for selected columns
                            plot_data <- self$data[, c(x_column, y_column, category_column)]
                            plot_data <- na.omit(plot_data)
                            
                            if (nrow(plot_data) == 0) {
                              stop("No valid data to plot after removing NAs")
                            }
                            
                            # Get unique categories and assign colors
                            categories <- unique(plot_data[[category_column]])
                            num_categories <- length(categories)
                            colors <- rainbow(num_categories)  # Assign distinct colors
                            par(mar = c(5, 4, 8, 8)) 
                            # Create scatter plot
                            plot(plot_data[[x_column]], plot_data[[y_column]], 
                                 col = colors[as.factor(plot_data[[category_column]])], 
                                 pch = 16,  # Solid circles
                                 xlab = x_column, 
                                 ylab = y_column, 
                                 main = paste("Scatter Plot of", x_column, "vs", y_column, "by", category_column),
                                 grid = TRUE)
                            
                            # 1. Allow drawing outside the plot region
                            par(xpd = TRUE)
                            # Add legend
                            legend("topright", inset = c(-1, -0.5), 
                                   legend = categories, col = colors, 
                                   pch = 16, title = category_column)
                            
                            cat("Scatter plot created for", x_column, "vs", y_column, "grouped by", category_column, "\n")
                            return(invisible(self))
                          }, 
                          
                          # New Method: Get numeric columns info
                          get_numeric_info = function() {
                            if (is.null(self$data)) {
                              stop("No data available")
                            }
                            list(
                              column_names = self$numeric_cols,
                              data_subset = self$data[, self$numeric_cols, drop = FALSE]
                            )
                          },
                          
                          # New Method: Get categorical columns info
                          get_categorical_info = function() {
                            if (is.null(self$data)) {
                              stop("No data available")
                            }
                            if (length(self$categorical_cols) == 0) {
                              cat("No categorical columns found\n")
                              return(list(column_names = character(0), data_subset = data.frame(), frequency_tables = list()))
                            }
                            
                            # Compute frequency tables for each categorical column
                            frequency_tables <- lapply(self$categorical_cols, function(col) {
                              freq <- table(self$data[[col]], useNA = "ifany")  # Include NA counts if present
                              data.frame(Category = names(freq), Frequency = as.numeric(freq), stringsAsFactors = FALSE)
                            })
                            names(frequency_tables) <- self$categorical_cols
                            
                            list(
                              column_names = self$categorical_cols,
                              data_subset = self$data[, self$categorical_cols, drop = FALSE],
                              frequency_tables = frequency_tables
                            )
                          },
                          
                          # New Method: List available regression models
                          list_regression_models = function() {
                            cat("Available Regression Models in regrissorClass:\n")
                            cat("1. fit_linear_regression(outcome, predictor)\n")
                            cat("   Simple linear model with one numeric predictor.\n")
                            cat("2. fit_logistic_regression(outcome, predictors)\n")
                            cat("   For binary outcomes; predictors is a character vector.\n")
                            cat("3. fit_multiple_linear_regression(outcome, predictors)\n")
                            cat("   Multiple numeric predictors for a numeric outcome.\n")
                            cat("4. perform_comprehensive_regression_analysis(outcome_var, predictors,\n")
                            cat("     demographic_predictors, interaction_terms, include_logistic)\n")
                            cat("   Runs simple, multiple, demographic (ANCOVA), interaction, and\n")
                            cat("   optionally logistic regression in one call. Returns a named list.\n")
                            cat("Note: Ensure outcome and predictors are appropriate for the model type.\n")
                            return(invisible(self))
                          },
                          
                          # New Method: Fit simple linear regression
                          fit_linear_regression = function(outcome, predictor) {
                            if (is.null(self$data)) {
                              stop("No data available")
                            }
                            if (!outcome %in% names(self$data) || !predictor %in% names(self$data)) {
                              stop("Outcome or predictor column not found")
                            }
                            if (!is.numeric(self$data[[outcome]])) {
                              stop("Outcome must be numeric for linear regression")
                            }
                            if (!is.numeric(self$data[[predictor]])) {
                              stop("Predictor must be numeric")
                            }
                            
                            # Fit model
                            formula <- as.formula(paste(outcome, "~", predictor))
                            model <- lm(formula, data = self$data)
                            summary_model <- summary(model)
                            
                            # Extract stats
                            list(
                              model = model,
                              coefficients = summary_model$coefficients,
                              r_squared = summary_model$r.squared,
                              adj_r_squared = summary_model$adj.r.squared,
                              f_statistic = summary_model$fstatistic,
                              p_values = summary_model$coefficients[, "Pr(>|t|)"],
                              sse = sum(resid(model)^2),
                              aic = AIC(model),
                              bic = BIC(model),
                              residuals = resid(model)
                            )
                          },
                          
                          # New Method: Fit logistic regression
                          fit_logistic_regression = function(outcome, predictors) {
                            if (is.null(self$data)) {
                              stop("No data available")
                            }
                            if (!outcome %in% names(self$data)) {
                              stop("Outcome column not found")
                            }
                            if (!all(predictors %in% names(self$data))) {
                              stop("One or more predictor columns not found")
                            }
                            # Check if outcome is binary (0/1 or factor with 2 levels)
                            outcome_data <- self$data[[outcome]]
                            if (is.numeric(outcome_data)) {
                              if (!all(outcome_data %in% c(0, 1, NA))) {
                                stop("Outcome must be binary (0/1) for logistic regression")
                              }
                            } else if (length(unique(na.omit(outcome_data))) != 2) {
                              stop("Outcome must have exactly 2 unique values for logistic regression")
                            }
                            
                            # Fit model
                            formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
                            model <- glm(formula, data = self$data, family = binomial)
                            summary_model <- summary(model)
                            
                            # Extract stats
                            list(
                              model = model,
                              coefficients = summary_model$coefficients,
                              deviance = summary_model$deviance,
                              aic = summary_model$aic,
                              p_values = summary_model$coefficients[, "Pr(>|z|)"],
                              null_deviance = summary_model$null.deviance,
                              residual_deviance = summary_model$deviance,
                              residuals = resid(model)
                            )
                          },
                          
                          
                          # New Method: Comprehensive regression analysis
                          # Runs simple, multiple, demographic (ANCOVA-style), interaction, and optionally logistic
                          # regression in a single call. Returns a named list of all results.
                          perform_comprehensive_regression_analysis = function(
                              outcome_var,
                              predictors         = NULL,
                              demographic_predictors = NULL,
                              interaction_terms  = FALSE,
                              include_logistic   = FALSE) {
                            
                            # ── Validation ────────────────────────────────────────────────────────────
                            if (is.null(self$data)) stop("No data available for analysis")
                            if (!outcome_var %in% names(self$data))
                              stop("Outcome variable not found: ", outcome_var)
                            if (!is.numeric(self$data[[outcome_var]]))
                              stop("Outcome must be numeric for regression")
                            
                            # Default: all numeric columns except the outcome
                            if (is.null(predictors))
                              predictors <- setdiff(self$numeric_cols, outcome_var)
                            
                            missing_preds <- setdiff(predictors, names(self$data))
                            if (length(missing_preds) > 0)
                              stop("Predictors not found: ", paste(missing_preds, collapse = ", "))
                            if (!all(sapply(self$data[predictors], is.numeric)))
                              stop("All numeric predictors must be numeric")
                            
                            if (!is.null(demographic_predictors)) {
                              missing_demo <- setdiff(demographic_predictors, names(self$data))
                              if (length(missing_demo) > 0)
                                stop("Demographic predictors not found: ", paste(missing_demo, collapse = ", "))
                            }
                            
                            is_binary <- length(unique(na.omit(self$data[[outcome_var]]))) == 2
                            
                            cat("=== Comprehensive Regression Analysis ===\n")
                            cat("Outcome          :", outcome_var, "\n")
                            cat("Predictors       :", paste(predictors, collapse = ", "), "\n")
                            if (!is.null(demographic_predictors))
                              cat("Demographics     :", paste(demographic_predictors, collapse = ", "), "\n")
                            cat("Interaction terms:", interaction_terms, "\n")
                            cat("Binary outcome   :", is_binary, "\n\n")
                            
                            results <- list()
                            
                            # ── 1. Simple linear regressions ─────────────────────────────────────────
                            if (length(predictors) > 0) {
                              cat("=== Simple Linear Regressions ===\n")
                              for (pred in predictors) {
                                model_data <- na.omit(self$data[, c(outcome_var, pred)])
                                formula    <- as.formula(paste(outcome_var, "~", pred))
                                model      <- lm(formula, data = model_data)
                                s          <- summary(model)
                                cat("  ", pred, "->", outcome_var,
                                    "| R\u00b2 =", round(s$r.squared, 4),
                                    "| p =",  round(coef(s)[2, 4],  4), "\n")
                                results[[paste0("simple_", pred)]] <- list(
                                  model        = model,
                                  formula      = formula,
                                  r_squared    = s$r.squared,
                                  p_value      = coef(s)[2, 4],
                                  coefficients = coef(model)
                                )
                              }
                            }
                            
                            # ── 2. Multiple linear regression ────────────────────────────────────────
                            if (length(predictors) > 1) {
                              cat("\n=== Multiple Linear Regression ===\n")
                              multi_data    <- na.omit(self$data[, c(outcome_var, predictors)])
                              formula_multi <- as.formula(paste(outcome_var, "~",
                                                                paste(predictors, collapse = " + ")))
                              multi_model   <- lm(formula_multi, data = multi_data)
                              s_multi       <- summary(multi_model)
                              print(s_multi)
                              
                              vif_results <- NULL
                              if (requireNamespace("car", quietly = TRUE)) {
                                vif_results <- car::vif(multi_model)
                                cat("\nVIF values:\n")
                                print(vif_results)
                              }
                              
                              results$multiple <- list(
                                model               = multi_model,
                                formula             = formula_multi,
                                r_squared           = s_multi$r.squared,
                                adjusted_r_squared  = s_multi$adj.r.squared,
                                coefficients        = coef(multi_model),
                                vif                 = vif_results,
                                summary             = s_multi
                              )
                            }
                            
                            # ── 3a. Individual demographic effects (per-variable) ────────────────────
                            # Categorical demographics → Type II ANOVA; continuous → simple lm.
                            # This gives the univariate effect of each demographic on its own.
                            if (!is.null(demographic_predictors) && length(demographic_predictors) > 0) {
                              cat("\n=== Individual Demographic Effects ===\n")
                              for (demo_pred in demographic_predictors) {
                                col <- self$data[[demo_pred]]
                                if (is.character(col) || is.factor(col)) {
                                  # Categorical: Type II ANOVA via car::Anova
                                  demo_data_i              <- na.omit(self$data[, c(outcome_var, demo_pred)])
                                  demo_data_i[[demo_pred]] <- as.factor(demo_data_i[[demo_pred]])
                                  anova_result <- car::Anova(
                                    lm(as.formula(paste(outcome_var, "~", demo_pred)), data = demo_data_i)
                                  )
                                  cat("ANOVA for", demo_pred, ":\n")
                                  print(anova_result)
                                  results[[paste0("demo_anova_", demo_pred)]] <- anova_result
                                } else {
                                  # Continuous: simple linear regression
                                  demo_data_i <- na.omit(self$data[, c(outcome_var, demo_pred)])
                                  formula_i   <- as.formula(paste(outcome_var, "~", demo_pred))
                                  model_i     <- lm(formula_i, data = demo_data_i)
                                  s_i         <- summary(model_i)
                                  cat(" ", demo_pred, "->", outcome_var,
                                      "| R\u00b2 =", round(s_i$r.squared, 4),
                                      "| p =",  round(coef(s_i)[2, 4],  4), "\n")
                                  results[[paste0("demo_", demo_pred)]] <- list(
                                    model        = model_i,
                                    formula      = formula_i,
                                    r_squared    = s_i$r.squared,
                                    p_value      = coef(s_i)[2, 4],
                                    coefficients = coef(model_i)
                                  )
                                }
                              }
                            }
                            
                            # ── 3b. Combined demographic model (ANCOVA-style) ────────────────────────
                            # All numeric predictors + all demographics in one model.
                            if (!is.null(demographic_predictors) && length(demographic_predictors) > 0) {
                              cat("\n=== Combined Demographic Model (ANCOVA) ===\n")
                              all_preds  <- c(predictors, demographic_predictors)
                              demo_data  <- na.omit(self$data[, c(outcome_var, all_preds)])
                              for (d in demographic_predictors)
                                demo_data[[d]] <- as.factor(demo_data[[d]])
                              
                              formula_demo <- as.formula(paste(outcome_var, "~",
                                                               paste(all_preds, collapse = " + ")))
                              demo_model   <- lm(formula_demo, data = demo_data)
                              s_demo       <- summary(demo_model)
                              print(s_demo)
                              
                              results$demographic <- list(
                                model              = demo_model,
                                formula            = formula_demo,
                                r_squared          = s_demo$r.squared,
                                adjusted_r_squared = s_demo$adj.r.squared,
                                coefficients       = coef(demo_model),
                                summary            = s_demo
                              )
                            }
                            
                            # ── 4. Interaction terms (numeric × demographic) ──────────────────────────
                            if (interaction_terms && !is.null(demographic_predictors) &&
                                length(demographic_predictors) > 0) {
                              cat("\n=== Regression with Interaction Terms ===\n")
                              all_preds      <- c(predictors, demographic_predictors)
                              interact_data  <- na.omit(self$data[, c(outcome_var, all_preds)])
                              for (d in demographic_predictors)
                                interact_data[[d]] <- as.factor(interact_data[[d]])
                              
                              # One interaction term per numeric–demographic pair
                              interaction_pairs <- unlist(lapply(predictors, function(p)
                                lapply(demographic_predictors, function(d) paste(p, "*", d))))
                              formula_interact <- as.formula(paste(
                                outcome_var, "~",
                                paste(c(predictors, demographic_predictors, interaction_pairs),
                                      collapse = " + ")))
                              
                              interact_model <- lm(formula_interact, data = interact_data)
                              s_interact     <- summary(interact_model)
                              print(s_interact)
                              
                              results$interaction <- list(
                                model              = interact_model,
                                formula            = formula_interact,
                                r_squared          = s_interact$r.squared,
                                adjusted_r_squared = s_interact$adj.r.squared,
                                coefficients       = coef(interact_model),
                                summary            = s_interact
                              )
                            }
                            
                            # ── 5. Optional logistic regression ──────────────────────────────────────
                            if (include_logistic) {
                              if (!is_binary) {
                                # Median-split to create a binary outcome
                                median_val  <- median(self$data[[outcome_var]], na.rm = TRUE)
                                binary_col  <- paste0(outcome_var, "_binary")
                                self$data[[binary_col]] <- ifelse(self$data[[outcome_var]] > median_val, 1L, 0L)
                                logistic_outcome <- binary_col
                                cat("\n=== Logistic Regression (median split: >", median_val, "= 1) ===\n")
                              } else {
                                logistic_outcome <- outcome_var
                                cat("\n=== Logistic Regression ===\n")
                              }
                              
                              logistic_preds <- c(predictors,
                                                  if (!is.null(demographic_predictors)) demographic_predictors)
                              logistic_data  <- na.omit(self$data[, c(logistic_outcome, logistic_preds)])
                              formula_log    <- as.formula(paste(logistic_outcome, "~",
                                                                 paste(logistic_preds, collapse = " + ")))
                              log_model      <- glm(formula_log, data = logistic_data, family = binomial)
                              s_log          <- summary(log_model)
                              print(s_log)
                              
                              results$logistic <- list(
                                model        = log_model,
                                formula      = formula_log,
                                aic          = log_model$aic,
                                coefficients = coef(log_model),
                                summary      = s_log
                              )
                            }
                            
                            cat("\n=== Analysis Complete ===\n")
                            cat("Result sections:", paste(names(results), collapse = ", "), "\n")
                            return(results)
                          },
                          
                          # New Method: Fit multiple linear regression
                          fit_multiple_linear_regression = function(outcome, predictors) {
                            if (is.null(self$data)) {
                              stop("No data available")
                            }
                            if (!outcome %in% names(self$data)) {
                              stop("Outcome column not found")
                            }
                            if (!all(predictors %in% names(self$data))) {
                              stop("One or more predictor columns not found")
                            }
                            if (!is.numeric(self$data[[outcome]])) {
                              stop("Outcome must be numeric for multiple linear regression")
                            }
                            if (!all(sapply(self$data[predictors], is.numeric))) {
                              stop("All predictors must be numeric")
                            }
                            
                            # Fit model
                            formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
                            model <- lm(formula, data = self$data)
                            summary_model <- summary(model)
                            
                            # Extract stats
                            list(
                              model = model,
                              coefficients = summary_model$coefficients,
                              r_squared = summary_model$r.squared,
                              adj_r_squared = summary_model$adj.r.squared,
                              f_statistic = summary_model$fstatistic,
                              p_values = summary_model$coefficients[, "Pr(>|t|)"],
                              sse = sum(resid(model)^2),
                              aic = AIC(model),
                              bic = BIC(model),
                              residuals = resid(model)
                            )
                          }, 
                          
                          
                          # New Method: Plot regression model
                          plot_regression_model = function(model, type = "linear") { 
                            if (is.null(model$model)) {
                                stop("Invalid model: Ensure model is from a fit_* method")
                              }
                            if (!type %in% c("linear", "logistic", "multiple_linear")) {
                                stop("Type must be 'linear', 'logistic', or 'multiple_linear'")
                              }
                                
                            if (type == "linear") {
                              # Scatter plot with regression line
                              predictor <- names(model$model$model)[2]  # Assumes single predictor
                              outcome <- names(model$model$model)[1]
                              plot(self$data[[predictor]], self$data[[outcome]], 
                                    xlab = predictor, ylab = outcome, main = paste("Linear Regression:",                                        outcome, "~", predictor), col = "blue", pch = 16)
                              abline(model$model, col = "red", lwd = 2)
                              legend("topright", legend = c("Data", "Fitted Line"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))
                              } else if (type == "logistic") {
                            # Plot with sigmoid curve
                                predictor <- names(model$model$model)[2]  # Assumes single predictor for simplicity
                                outcome <- names(model$model$model)[1]
                                plot(self$data[[predictor]], self$data[[outcome]], 
                                      xlab = predictor, ylab = outcome, main = paste("Logistic Regression:",                                           outcome, "~", predictor), col = "blue", pch = 16)
                                # Add fitted curve
                                pred_vals <- seq(min(self$data[[predictor]], na.rm = TRUE), max(self$data[[predictor]], na.rm = TRUE), length.out = 100)
                                fitted_probs <- predict(model$model, newdata = data.frame(setNames(list(pred_vals), predictor)), type = "response")
                                lines(pred_vals, fitted_probs, col = "red", lwd = 2)
                                legend("topright", legend = c("Data", "Fitted Curve"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))
                              } else if (type == "multiple_linear") {
                                  # Residuals vs Fitted plot
                                   plot(fitted(model$model), resid(model$model),
                                         xlab = "Fitted Values", ylab = "Residuals", 
                                         main = "Multiple Linear Regression: Residuals vs Fitted",col = "blue", pch = 16)
                                   abline(h = 0, col = "red", lwd = 2)
                                   legend("topright", legend = c("Residuals", "Zero Line"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))
                         }
                                
                        cat("Plot generated for", type, "regression model\n")
                        return(invisible(self))
                         }, 
                        
                        #-------------------------------------------------------
                        # New Method: Perform t-test
                        perform_t_test = function(outcome, group_column = NULL, type = "independent", plot = FALSE, mu = NULL, assume_equal_var = TRUE) {
                          if (is.null(self$data)) {
                            stop("No data available")
                          }
                          if (!outcome %in% names(self$data)) {
                            stop("Outcome column not found")
                          }
                          if (!is.numeric(self$data[[outcome]])) {
                            stop("Outcome must be numeric")
                          }
                          if (type == "independent") {
                            if (is.null(group_column) || !group_column %in% names(self$data)) {
                              stop("Group column required for independent t-test")
                            }
                            groups <- unique(na.omit(self$data[[group_column]]))
                            if (length(groups) != 2) {
                              stop("Group column must have exactly 2 unique groups for independent t-test")
                            }
                            # Recommendation: Ensure equal variances using Levene's test
                            if (!requireNamespace("car", quietly = TRUE)) {
                              stop("Package 'car' required for variance check. Install with install.packages('car')")
                            }
                            levene_result <- car::leveneTest(self$data[[outcome]] ~ as.factor(self$data[[group_column]]))
                            levene_p <- levene_result$`Pr(>F)`[1]
                            if (levene_p < 0.05) {
                              warning("Variances are unequal (Levene's test p = ", round(levene_p, 4), "). Consider Welch's t-test (var.equal = FALSE) for unequal variances.")
                            } else {
                              cat("Variances are equal (Levene's test p = ", round(levene_p, 4), "). Proceeding with equal variances assumed.\n")
                            }
                            result <- t.test(self$data[[outcome]] ~ self$data[[group_column]], var.equal = TRUE)  # Assumes equal variances if check passes
                            if (plot) {
                              boxplot(self$data[[outcome]] ~ self$data[[group_column]], 
                                      main = paste("Boxplot of", outcome, "by", group_column), 
                                      xlab = group_column, ylab = outcome, col = c("lightblue", "lightgreen"))
                            }
                          } else if (type == "welch") {
                            if (is.null(group_column) || !group_column %in% names(self$data)) {
                              stop("Group column required for Welch's t-test")
                            }
                            groups <- unique(na.omit(self$data[[group_column]]))
                            if (length(groups) != 2) {
                              stop("Group column must have exactly 2 unique groups for Welch's t-test")
                            }
                            # Informational: Check variances using Levene's test (no warning since unequal variances are allowed)
                            if (!requireNamespace("car", quietly = TRUE)) {
                              stop("Package 'car' required for variance check. Install with install.packages('car')")
                            }
                            levene_result <- car::leveneTest(self$data[[outcome]] ~ as.factor(self$data[[group_column]]))
                            levene_p <- levene_result$`Pr(>F)`[1]
                            if (levene_p < 0.05) {
                              cat("Variances are unequal (Levene's test p = ", round(levene_p, 4), "). Proceeding with Welch's t-test for unequal variances.\n")
                            } else {
                              cat("Variances are equal (Levene's test p = ", round(levene_p, 4), "). Welch's t-test still applied (robust to equal variances).\n")
                            }
                            result <- t.test(self$data[[outcome]] ~ self$data[[group_column]], var.equal = FALSE)  # Welch's test: does not assume equal variances
                            if (plot) {
                              boxplot(self$data[[outcome]] ~ self$data[[group_column]], 
                                      main = paste("Boxplot of", outcome, "by", group_column, "(Welch's Test)"), 
                                      xlab = group_column, ylab = outcome, col = c("lightblue", "lightgreen"))
                            }
                          } else if (type == "paired") {
                            if (is.null(group_column) || !group_column %in% names(self$data)) {
                              stop("Group column required for paired t-test")
                            }
                            groups <- unique(na.omit(self$data[[group_column]]))
                            if (length(groups) != 2) {
                              stop("Group column must have exactly 2 unique groups for paired t-test")
                            }
                            # Recommendation: Ensure data is matched (equal lengths, no missing pairs)
                            group1 <- self$data[[outcome]][self$data[[group_column]] == groups[1]]
                            group2 <- self$data[[outcome]][self$data[[group_column]] == groups[2]]
                            if (length(group1) != length(group2)) {
                              stop("Paired groups must have equal lengths (", length(group1), " vs ", length(group2), "). Ensure data is properly matched.")
                            }
                            if (any(is.na(group1)) || any(is.na(group2))) {
                              stop("Paired data cannot have missing values in either group. Clean data first.")
                            }
                            cat("Paired data validated: Equal lengths (", length(group1), ") and no missing pairs.\n")
                            result <- t.test(group1, group2, paired = TRUE)
                            if (plot) {
                              plot(group1, group2, main = paste("Paired Plot of", outcome, "by", group_column), 
                                   xlab = paste(groups[1]), ylab = paste(groups[2]), col = "blue", pch = 16)
                              abline(0, 1, col = "red")  # Line of equality
                            }
                          } else if (type == "one_sample") {
                            if (is.null(mu)) {
                              stop("mu (known mean) required for one-sample t-test")
                            }
                            result <- t.test(self$data[[outcome]], mu = mu)
                            if (plot) {
                              # Assign to local variable to avoid name conflicts
                              outcome_data <- self$data[[outcome]]
                              # Plot histogram with x-axis extended to include mu
                              hist(outcome_data, main = paste("Histogram of", outcome, "with mu =", mu),
                                   xlab = outcome, col = "lightblue", border = "white",
                                   xlim = c(min(outcome_data, na.rm = TRUE), max(mu, max(outcome_data, na.rm = TRUE))))
                              abline(v = mu, col = "red", lwd = 2)  # Reference line at mu
                            }
                          } else {
                            stop("Type must be 'independent', 'welch', 'paired', or 'one_sample'")
                          }
                          # Recommendation: Add comments for statistical interpretation
                          # - p_value < 0.05: Reject the null hypothesis (significant difference in means).
                          # - p_value >= 0.05: Fail to reject the null hypothesis (no significant difference).
                          # - Confidence interval: Range where the true mean difference likely lies (95% confidence).
                          # Return results
                          list(
                            t_statistic = result$statistic,
                            p_value = result$p.value,
                            confidence_interval = result$conf.int,
                            means = result$estimate,
                            test_type = type,
                            outcome = outcome,
                            group_column = group_column
                          )
                        },

                    #-------------------
                    # New Method: Calculate Pearson correlation
                    calculate_pearson_correlation = function(variables, plot = FALSE) {
                        if (is.null(self$data)) {
                          stop("No data available")
                          }
                        if (length(variables) < 2) {
                         stop("At least 2 variables required for correlation analysis")
                         }
                        if (!all(variables %in% names(self$data))) {
                         stop("One or more variables not found in data")
                        }
                        if (!all(sapply(self$data[variables], is.numeric))) {
                          stop("All variables must be numeric for Pearson correlation")
                        }
                          
                        # Subset data
                        data_subset <- self$data[, variables, drop = FALSE]
                            
                        # Compute correlation matrix
                        cor_matrix <- cor(data_subset, use = "pairwise.complete.obs", method = "pearson")
                              
                        # Compute p-values for each pair
                        p_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables))
                        rownames(p_matrix) <- colnames(p_matrix) <- variables
                        for (i in 1:(length(variables) - 1)) {
                          for (j in (i + 1):length(variables)){
                              test <- cor.test(data_subset[[variables[i]]], data_subset[[variables[j]]], method = "pearson")
                              p_matrix[i, j] <- p_matrix[j, i] <- test$p.value
                              }
                          }
                                  
                        # Optional plotting
                        if (plot) {
                            if (length(variables) <= 5) {
                            # Pairs plot for small number of variables
                            pairs(data_subset, main = "Pairs Plot of Pearson Correlations")
                              } else {
                                # Simple heatmap for larger sets
                                image(cor_matrix, axes = FALSE, main = "Pearson Correlation Heatmap")
                                axis(1, at = 0:(length(variables)-1)/(length(variables)-1), labels = variables)
                                axis(2, at = 0:(length(variables)-1)/(length(variables)-1), labels = variables)
                                      for (i in 1:length(variables)) {
                                         for (j in 1:length(variables)) {
                                             text((i-1)/(length(variables)-1), (j-1)/(length(variables)-1),
                                                   round(cor_matrix[i, j], 2), cex = 0.8)
                                            }
                                        }
                                      }
                                  }
                                  
                        cat("Pearson correlation analysis completed for variables:", paste(variables, collapse = ", "), "\n")
                                  
                        # Return results
                        list(
                            correlation_matrix = cor_matrix,
                            p_value_matrix = p_matrix,
                            variables = variables,
                            method = "Pearson Product-Moment Correlation"
                            )
                    },
                    
                    #--------------------------
                    # New Method: Perform Kruskal-Wallis H test
                    perform_kruskal_wallis = function(outcome, group_column, plot = FALSE) {
                        if (is.null(self$data)) {
                           stop("No data available")
                            }
                        if (!outcome %in% names(self$data) || !group_column %in% names(self$data)) {
                          stop("Outcome or group column not found")
                            }
                        if (!is.numeric(self$data[[outcome]])) {
                          stop("Outcome must be numeric")
                            }
                        groups <- unique(na.omit(self$data[[group_column]]))
                        if (length(groups) < 3) {
                          stop("Kruskal-Wallis requires at least 3 groups")
                           }
                            
                        # Perform test
                        result <- kruskal.test(self$data[[outcome]] ~ as.factor(self$data[[group_column]]))
                              
                        # Optional plot
                        if (plot) {
                            boxplot(self$data[[outcome]] ~ self$data[[group_column]], 
                            main = paste("Boxplot of", outcome, "by", group_column, "(Kruskal-Wallis)"), 
                                    xlab = group_column, ylab = outcome, col = rainbow(length(groups)))
                                  }
                              
                        cat("Kruskal-Wallis test completed for", outcome, "by", group_column, "\n")
                              
                        # Return results
                        list(
                            h_statistic = result$statistic,
                            p_value = result$p.value,
                            degrees_of_freedom = result$parameter,
                            method = "Kruskal-Wallis H test"
                            )
                    },
                      
                # New Method: Perform Mann-Whitney U or K-S test
                perform_mann_whitney_ks = function(outcome, group_column, type = "mann_whitney", plot = FALSE) {
                  if (is.null(self$data)) {
                      stop("No data available")
                    }
                  if (!outcome %in% names(self$data) || !group_column %in% names(self$data)) {
                      stop("Outcome or group column not found")
                    }
                  if (!is.numeric(self$data[[outcome]])) {
                      stop("Outcome must be numeric")
                    }
                  groups <- unique(na.omit(self$data[[group_column]]))
                  if (length(groups) != 2) {
                      stop("Test requires exactly 2 groups")
                    }
                            
                  # Perform test
                  if (type == "mann_whitney") {
                  result <- wilcox.test(self$data[[outcome]] ~ self$data[[group_column]])
                    } else if (type == "ks") {
                      group1 <- self$data[[outcome]][self$data[[group_column]] == groups[1]]
                      group2 <- self$data[[outcome]][self$data[[group_column]] == groups[2]]
                      result <- ks.test(group1, group2)
                    } else {
                      stop("Type must be 'mann_whitney' or 'ks'")
                      }
                            
                  # Optional plot
                  if (plot) {
                      if (type == "mann_whitney") {
                        boxplot(self$data[[outcome]] ~ self$data[[group_column]], 
                                  main = paste("Boxplot of", outcome, "by", group_column, "(Mann-Whitney)"), 
                                  xlab = group_column, ylab = outcome, col = c("lightblue", "lightgreen"))
                          } else {
                                # Empirical CDF plot for K-S
                                group1 <- self$data[[outcome]][self$data[[group_column]] == groups[1]]
                                group2 <- self$data[[outcome]][self$data[[group_column]] == groups[2]]
                                plot(ecdf(group1), main = paste("Empirical CDF of", outcome, "by", group_column, "(K-S)"), col = "blue")
                                lines(ecdf(group2), col = "red")
                                legend("bottomright", legend = groups, col = c("blue", "red"), lty = 1)
                                }
                        }
                            
                  cat(type, "test completed for", outcome, "by", group_column, "\n")
                  # Return results
                  list(
                      statistic = result$statistic,
                      p_value = result$p.value,
                      method = paste(type, "test")
                      )
                },
                      
                # New Method: Perform Levene's or Anderson-Darling test
                perform_levene_anderson = function(outcome, group_column, type = "levene", plot = FALSE) {
                  if (is.null(self$data)) {
                    stop("No data available")
                  }
                  if (!outcome %in% names(self$data) || !group_column %in% names(self$data)) {
                    stop("Outcome or group column not found")
                  }
                  if (!is.numeric(self$data[[outcome]])) {
                    stop("Outcome must be numeric")
                  }
                  groups <- unique(na.omit(self$data[[group_column]]))
                  if (length(groups) < 2) {
                    stop("Test requires at least 2 groups")
                  }
                  # Perform test
                  if (type == "levene") {
                    if (!requireNamespace("car", quietly = TRUE)) {
                      stop("Package 'car' required for Levene's test. Install with install.packages('car')")
                    }
                    result <- car::leveneTest(self$data[[outcome]] ~ as.factor(self$data[[group_column]]))
                    statistic <- result$`F value`[1]
                    p_value <- result$`Pr(>F)`[1]
                  } else if (type == "anderson_darling_nor") {
                    if (!requireNamespace("nortest", quietly = TRUE)) {
                      stop("Package 'nortest' required for Anderson-Darling normality test. Install with install.packages('nortest')")
                    }
                    # Anderson-Darling for each group (aggregate p-values or compare)
                    p_values <- sapply(groups, function(g) {
                      nortest::ad.test(self$data[[outcome]][self$data[[group_column]] == g])$p.value
                    })
                    statistic <- NA  # Not a single stat; use p-values
                    p_value <- min(p_values)  # Conservative approach
                  } else if (type == "anderson_darling_k") {
                    if (!requireNamespace("kSamples", quietly = TRUE)) {
                      stop("Package 'kSamples' required for K-sample Anderson-Darling test. Install with install.packages('kSamples')")
                    }
                    # Prepare data as a list of vectors (one per group)
                    group_data <- split(self$data[[outcome]], self$data[[group_column]])
                    result <- kSamples::ad.test(group_data)  # K-sample test
                    statistic <- result$statistic
                    p_value <- result$p.value
                  } else {
                    stop("Type must be 'levene', 'anderson_darling_nor', or 'anderson_darling_k'")
                  }
                  
                  # Optional plot
                  if (plot) {
                    if (type == "levene") {
                      boxplot(self$data[[outcome]] ~ self$data[[group_column]], 
                              main = paste("Boxplot of", outcome, "by", group_column, "(Levene's)"), 
                              xlab = group_column, ylab = outcome, col = rainbow(length(groups)))
                    } else {
                      # Q-Q plot for each group
                      par(mfrow = c(1, length(groups)))
                      for (g in groups) {
                        qqnorm(self$data[[outcome]][self$data[[group_column]] == g], 
                               main = paste("Q-Q Plot for", g, "(Anderson-Darling)"))
                        qqline(self$data[[outcome]][self$data[[group_column]] == g])
                      }
                      par(mfrow = c(1, 1))
                    }
                  }
                  
                  cat(type, "test completed for", outcome, "by", group_column, "\n")
                  
                  # Return results
                  list(
                    statistic = statistic,
                    p_value = p_value,
                    method = paste(type, "test")
                  )
                },
                
                
                ##-------------------------------------------------------------------------------
                # New Method: Perform Response Surface Analysis
                perform_response_surface_analysis = function(response, predictors, plot = TRUE) {
                    if (is.null(self$data)) {
                        stop("No data available")
                        }
                    if (!response %in% names(self$data) || !all(predictors %in% names(self$data))) {
                       stop("Response or predictors not found")
                       }
                    if (!is.numeric(self$data[[response]]) || !all(sapply(self$data[predictors], is.numeric))) {
                       stop("Response and predictors must be numeric")
                       }
                    if (length(predictors) != 2) {
                       stop("Response surface analysis requires exactly 2 predictors")
                       }
                    if (!requireNamespace("rsm", quietly = TRUE)) {
                       stop("Package 'rsm' required. Install with install.packages('rsm')")
                       }
                      
                    # Fit second-order model
                    formula <- as.formula(paste(response, "~ SO(", paste(predictors, collapse = ","), ")"))
                    model <- rsm::rsm(formula, data = self$data)
                          
                    # Optional plots
                    if (plot) {
                      par(mfrow = c(1, 2))
                      
                      # Contour plot using base contour via rsm method dispatch
                      tryCatch({
                        contour(model, ~ x1 + x2, image = TRUE, main = "Contour Plot")
                      }, error = function(e) {
                        cat("Contour plot skipped:", e$message, "\n")
                      })
                      
                      # Residuals vs Fitted
                      plot(fitted(model), resid(model),
                           main = "Residuals vs Fitted",
                           xlab = "Fitted Values",
                           ylab = "Residuals",
                           col  = "steelblue", pch = 16)
                      abline(h = 0, col = "red", lwd = 2)
                      
                      par(mfrow = c(1, 1))
                    }
                    cat("Response surface analysis completed for", response, "with predictors", paste(predictors, collapse = ", "), "\n")
                          
                    # Return results
                    list(
                        model = model,
                        summary = summary(model),
                        stationary_point = rsm::canonical(model)$xs,
                        method = "Response Surface Methodology"
                        )
                    },
                ##------------------------------------------------------------------------                  
                # New Method: Perform Cluster Analysis with Demographic Differences
                # k = NULL means auto-detect from elbow method (minimum 3 enforced).
                # k = integer means use that exact k (elbow plot still shown).
                # k_max controls the search range for the elbow (default 10).
                perform_cluster_analysis = function(variables, demographic_column, k = NULL, k_max = 10, plot = TRUE) {
                    if (is.null(self$data)) {
                       stop("No data available")
                       }
                    if (!all(variables %in% names(self$data)) || !demographic_column %in% names(self$data)) {
                       stop("Variables or demographic column not found")
                       }
                    if (!all(sapply(self$data[variables], is.numeric))) {
                       stop("Variables must be numeric")
                       }

                    # Scale data
                    data_scaled <- scale(self$data[, variables, drop = FALSE])

                    # --- Elbow analysis: compute WSS for k = 1 to k_max ---
                    set.seed(123)
                    wss <- sapply(1:k_max, function(ki) {
                        kmeans(data_scaled, centers = ki, nstart = 25, iter.max = 50)$tot.withinss
                    })

                    # Perpendicular distance method: find the k with maximum distance
                    # from the line connecting the first and last points of the WSS curve.
                    find_elbow <- function(wss_vals) {
                        n   <- length(wss_vals)
                        x   <- (seq_len(n) - 1) / (n - 1)
                        y   <- (wss_vals - min(wss_vals)) / (max(wss_vals) - min(wss_vals))
                        x1  <- x[1];  y1 <- y[1]
                        x2  <- x[n];  y2 <- y[n]
                        num <- abs((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1)
                        den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
                        which.max(num / den)
                    }
                    optimal_k <- find_elbow(wss)

                    # Determine final k
                    if (!is.null(k)) {
                        final_k <- k
                        cat("Using user-specified k =", final_k, "\n")
                    } else {
                        # Auto-detected: enforce a minimum of 3
                        final_k <- max(optimal_k, 3)
                        cat("Elbow method detected optimal k =", optimal_k,
                            "| Final k used:", final_k,
                            ifelse(optimal_k < 3, "(minimum of 3 enforced)\n", "\n"))
                    }

                    # Elbow plot (shown before clustering regardless of final plot flag)
                    if (plot) {
                        elbow_df <- data.frame(k = 1:k_max, WSS = wss)
                        elbow_plot <- ggplot2::ggplot(elbow_df, ggplot2::aes(x = k, y = WSS)) +
                            ggplot2::geom_line() +
                            ggplot2::geom_point() +
                            ggplot2::geom_vline(xintercept = final_k, linetype = "dashed", color = "red") +
                            ggplot2::annotate("text", x = final_k + 0.3, y = max(wss) * 0.95,
                                     label = paste("k =", final_k), color = "red", hjust = 0) +
                            ggplot2::labs(
                                title    = "Elbow Plot — Optimal Number of Clusters",
                                subtitle = paste("Detected elbow at k =", optimal_k,
                                                 "| Final k =", final_k),
                                x = "Number of Clusters (k)",
                                y = "Total Within-Cluster Sum of Squares") +
                            ggplot2::theme_minimal()
                        print(elbow_plot)
                    }

                    # Perform k-means clustering with final_k
                    set.seed(123)
                    clusters <- kmeans(data_scaled, centers = final_k, nstart = 25)
                    self$data$cluster <- clusters$cluster

                    # Analyze differences by demographic (two-way ANOVA: demographic x cluster)
                    diff_results <- lapply(variables, function(var) {
                       aov_result <- aov(self$data[[var]] ~ as.factor(self$data[[demographic_column]]) *
                                                            as.factor(self$data$cluster))
                       summary(aov_result)
                         })
                    names(diff_results) <- variables

                    # Cluster and distribution plots
                    if (plot) {
                         if (!requireNamespace("factoextra", quietly = TRUE)) {
                              stop("Package 'factoextra' required for plots. Install with install.packages('factoextra')")
                              }
                         par(mfrow = c(1, 3))
                         factoextra::fviz_cluster(clusters, data = data_scaled, main = "Cluster Plot")
                         plot(hclust(dist(data_scaled)), main = "Dendrogram")
                         boxplot(self$data[[variables[1]]] ~ self$data$cluster + self$data[[demographic_column]],
                                 main = paste("Boxplot of", variables[1], "by Cluster and", demographic_column))
                         par(mfrow = c(1, 1))
                             }

                    cat("Cluster analysis completed with", final_k, "clusters on variables",
                        paste(variables, collapse = ", "), "\n")

                    # Return results
                    list(
                        clusters    = clusters,
                        differences = diff_results,
                        optimal_k   = optimal_k,
                        final_k     = final_k,
                        wss         = wss,
                        method      = "K-Means Clustering with Demographic Analysis"
                        )
                   },
                  
              # New Method: Perform Thematic Analysis on Categorical Responses
              perform_thematic_analysis = function(categorical_columns, plot = TRUE) {
                    if (is.null(self$data)) {
                        stop("No data available")
                        }
                    if (!all(categorical_columns %in% names(self$data))) {
                        stop("Categorical columns not found")
                        }
                    if (!requireNamespace("tm", quietly = TRUE) || !requireNamespace("wordcloud", quietly = TRUE)) {
                        stop("Packages 'tm' and 'wordcloud' required. Install with install.packages(c('tm', 'wordcloud'))")
                      }
                      
                    # Combine categorical responses into a corpus
                    text_data <- apply(self$data[, categorical_columns, drop = FALSE], 1, paste, collapse = " ")
                    corpus <- tm::Corpus(tm::VectorSource(text_data))
                    corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
                    corpus <- tm::tm_map(corpus, tm::removePunctuation)
                    corpus <- tm::tm_map(corpus, tm::removeNumbers)
                    corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("en"))
                    corpus <- tm::tm_map(corpus, tm::stemDocument)
                                    
                    # Create term-document matrix
                    tdm <- tm::TermDocumentMatrix(corpus)
                    term_freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
                    themes <- data.frame(term = names(term_freq), frequency = term_freq)
                                          
                    # Optional plots
                    if (plot) {
                            par(mfrow = c(1, 2))
                            wordcloud::wordcloud(words = themes$term, freq = themes$frequency, max.words = 100, colors = brewer.pal(8, "Dark2"))
                            barplot(themes$frequency[1:10], names.arg = themes$term[1:10], las = 2, main = "Top 10 Themes")
                            par(mfrow = c(1, 1))
                              }
                    cat("Thematic analysis completed on columns", paste(categorical_columns, collapse = ", "), "\n")
                                          
                    # Return results
                    list(
                        themes = themes,
                        corpus = corpus,
                        method = "Thematic Analysis"
                        )
                 }, 
              
####==========================================================================              
# ==============================================================================

# Perform Response Surface Analysis
# 
# This method implements response surface methodology (RSM) using the rsm package
# to fit first-order and second-order models, perform canonical analysis,
# and generate response surface visualizations.
# 
#  param formula Formula for the response surface model (e.g., y ~ FO(x1, x2) or y ~ SO(x1, x2))'#
#  param data Data frame containing the variables
#  param model_type Type of model: "first_order" (FO) or "second_order" (SO)
#  param plot Generate visualization plots (default: TRUE)
#  param coded Use coded variables (default: TRUE)
#  param steepest_ascent Calculate steepest ascent direction (default: TRUE for first-order)
#  param canonical Perform canonical analysis (default: TRUE for second-order)
#  param contour Generate contour plots (default: TRUE)

#  return List containing model results, coefficients, canonical analysis, and visualizations
# 
#  examples
# # First-order model
# result <- analy$perform_response_surface_analysis(
#   formula = Performance ~ FO(Sensitivity, Th_AQ_Effect),
#   model_type = "first_order"
# )
# 
# # Second-order model  
# result <- analy$perform_response_surface_analysis(
#   formula = Performance ~ SO(Sensitivity, Th_AQ_Effect),
#   model_type = "second_order"
# )
# 
perform_response_surface_analysis_two = function(formula, data, model_type = "second_order", 
                                              plot = TRUE, coded = TRUE, 
                                              steepest_ascent = TRUE, canonical = TRUE,
                                              contour = TRUE) {
  
  # Check if rsm package is available
  if (!requireNamespace("rsm", quietly = TRUE)) {
    stop("The 'rsm' package is required. Install it via: install.packages('rsm')")
  }
  
  cat("Performing", model_type, "response surface analysis...\n")
  
  # Prepare data (placeholder for user-defined coding logic)
  data_coded <- data 
  
  # Extract variables from formula to dynamically build rsm-compatible formulas
  formula_vars <- all.vars(formula)
  response_var <- formula_vars[1]
  predictor_vars <- formula_vars[2:length(formula_vars)]
  
  # Construct a character string for predictors separated by commas
  predictors_joined <- paste(predictor_vars, collapse = ", ")
  
  # Dynamically modify formula to match rsm requirement rules
  if (model_type == "first_order") {
    rsm_formula_str <- paste(response_var, "~ FO(", predictors_joined, ")")
  } else if (model_type == "second_order") {
    rsm_formula_str <- paste(response_var, "~ SO(", predictors_joined, ")")
  } else {
    stop("Invalid model_type. Use 'first_order' or 'second_order'")
  }
  
  rsm_formula <- as.formula(rsm_formula_str)
  
  # Fit the response surface model
  tryCatch({
    rsm_model <- rsm::rsm(rsm_formula, data = data_coded)
    cat(model_type, "model fitted successfully using formula:", rsm_formula_str, "\n")
  }, error = function(e) {
    stop("Error fitting model: ", e$message)
  })
  
  # Get model summary and coefficients
  model_summary <- summary(rsm_model)
  coefficients <- coef(rsm_model)
  
  # Initialize results list
  results <- list(
    model = rsm_model,
    model_type = model_type,
    coefficients = coefficients,
    summary = model_summary,
    formula = rsm_formula,
    data_used = data_coded
  )
  
  # Perform additional analyses based on model type
  if (model_type == "first_order" && steepest_ascent) {
    tryCatch({
      steepest_result <- rsm::steepest(rsm_model)
      results$steepest_ascent <- steepest_result
      cat("Steepest ascent analysis completed.\n")
    }, error = function(e) {
      cat("Warning: Could not calculate steepest ascent: ", e$message, "\n")
    })
  }
  
  if (model_type == "second_order" && canonical) {
    tryCatch({
      # Canonical analysis is extracted from the rsm summary object directly
      canonical_analysis <- model_summary$canonical
      results$canonical_analysis <- canonical_analysis
      cat("Canonical analysis completed.\n")
      
      if (!is.null(canonical_analysis$xs)) {
        results$stationary_point <- canonical_analysis$xs
        cat("Stationary point identified.\n")
      }
    }, error = function(e) {
      cat("Warning: Could not perform canonical analysis: ", e$message, "\n")
    })
  }
  
  # Generate plots if requested
  if (plot) {
    cat("Generating visualization plots...\n")
    tryCatch({
      if (model_type == "second_order" && contour) {
        par(mfrow = c(2, 2))  # 2x2 grid for plots
        
        if (length(predictor_vars) >= 2) {
          # Use proper formula structure for the contour function
          plot_formula <- as.formula(paste("~", paste(predictor_vars[1:min(3, length(predictor_vars))], collapse = " + ")))
          rsm::contour.lm(rsm_model, plot_formula, image = TRUE, main = "Response Surface Contour Plot")
        }
      }
      par(mfrow = c(1, 1)) # Reset canvas
    }, error = function(e) {
      cat("Warning: Could not generate plots: ", e$message, "\n")
      par(mfrow = c(1, 1))  # Reset on error
    })
  }
  
  # Generate predictions for visualization matrix
  tryCatch({
    if (length(predictor_vars) >= 2) {
      x1_range <- range(data_coded[[predictor_vars[1]]], na.rm = TRUE)
      x2_range <- range(data_coded[[predictor_vars[2]]], na.rm = TRUE)
      
      x1_grid <- seq(x1_range[1], x1_range[2], length.out = 20)
      x2_grid <- seq(x2_range[1], x2_range[2], length.out = 20)
      
      grid_data <- expand.grid(x1 = x1_grid, x2 = x2_grid)
      
      # Fix loop handling and set remaining variables to their mean
      if (length(predictor_vars) > 2) {
        for (i in 3:length(predictor_vars)) {
          if (predictor_vars[i] %in% names(grid_data)) {
            next # FIXED: removed parentheses from next()
          } else {
            grid_data[[predictor_vars[i]]] <- mean(data_coded[[predictor_vars[i]]], na.rm = TRUE)
          }
        }
      }
      
      names(grid_data)[1:min(2, length(predictor_vars))] <- predictor_vars[1:min(2, length(predictor_vars))]
      predictions <- predict(rsm_model, newdata = grid_data)
      
      results$predictions <- list(
        grid = grid_data,
        predicted_values = predictions,
        variable_names = predictor_vars
      )
    }
  }, error = function(e) {
    cat("Warning: Could not generate predictions: ", e$message, "\n")
  })
  
  class(results) <- "response_surface_result"
  return(results)
},


                ###=============================================================
                #### fit logistic regression method
fit_ordinal_logistic_regression = function(
    outcome,
    predictors,
    conf_level = 0.95,
    plot = TRUE
) {
  
  require(MASS)
  
  vars <- c(outcome, predictors)
  
  model_df <- self$data[, vars, drop = FALSE]
  
  model_df <- stats::na.omit(model_df)
  
  if (nrow(model_df) == 0)
    stop("No complete observations available.")
  
  # Outcome must be ordered factor
  if (!is.ordered(model_df[[outcome]])) {
    
    if (is.factor(model_df[[outcome]]) ||
        is.numeric(model_df[[outcome]])) {
      
      model_df[[outcome]] <- ordered(model_df[[outcome]])
      
    } else {
      
      stop("Outcome must be numeric or factor.")
    }
  }
  
  formula_obj <- reformulate(
    predictors,
    response = outcome
  )
  
  # Important in R6
  environment(formula_obj) <- environment()
  
  cat("Step 1\n")
  
  mf <- model.frame(
    formula_obj,
    data = model_df,
    na.action = na.omit
  )
  
  ord_model <- MASS::polr(
    formula_obj,
    data = mf,
    Hess = TRUE,
    method = "logistic"
  )
  
  cat("Step 2\n")
  
  coef_summary <- as.data.frame(
    coef(summary(ord_model))
  )
  
  coef_summary$Term <- rownames(coef_summary)
  
  coef_summary$P_Value <- 2 * (
    1 - pnorm(abs(coef_summary$`t value`))
  )
  
  results_df <- data.frame(
    Term = rownames(coef_summary),
    Estimate = coef_summary[, "Value"],
    Std_Error = coef_summary[, "Std. Error"],
    Z_Value = coef_summary[, "t value"],
    P_Value = coef_summary$P_Value,
    Odds_Ratio = exp(coef_summary[, "Value"]),
    row.names = NULL
  )
  
 
  
  prop_odds_test <- NULL
  
  if (requireNamespace("brant", quietly = TRUE)) {
    
    prop_odds_test <- tryCatch(
      brant::brant(ord_model),
      error = function(e) {
        warning(
          paste(
            "Brant test failed:",
            e$message
          )
        )
        NULL
      }
    )
  }
  
  if (plot) {
    
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("ggplot2 required.")
    
    p <- ggplot2::ggplot(
      results_df,
      ggplot2::aes(
        x = reorder(Term, Odds_Ratio),
        y = Odds_Ratio
      )
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_hline(
        yintercept = 1,
        linetype = "dashed",
        colour = "red"
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Ordinal Logistic Regression Odds Ratios",
        x = "Predictor",
        y = "Odds Ratio"
      ) +
      ggplot2::theme_minimal()
    
    print(p)
  }
  print(results_df)
  return(
    list(
      model = ord_model,
      coefficients = results_df,
      proportional_odds_test = prop_odds_test
    )
  )
},



                #===================================================
              # New Method: List all methods in the class**
              list_all_methods = function() {
                # Get all public members
                  all_members <- ls(self)
                  # Filter to methods (functions), excluding properties
                  methods <- all_members[sapply(all_members, function(x) is.function(self[[x]]))]
                  return(sort(methods))
                  }
                    
    
                        #---------------------------------------
                          # ... #========= #(Other methods like print_summary, filter_data, etc., unchanged)..........#==========
                        ),
                        
                        private = list(
                          # Private method for internal validation
                          validate_column = function(column) {
                            if (!column %in% names(self$data)) {
                              stop("Column '", column, "' not found in data")
                            }
                          }
                          
                        )
)




