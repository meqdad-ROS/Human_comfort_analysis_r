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
                            
                            # Create scatter plot
                            plot(plot_data[[x_column]], plot_data[[y_column]], 
                                 col = colors[as.factor(plot_data[[category_column]])], 
                                 pch = 16,  # Solid circles
                                 xlab = x_column, 
                                 ylab = y_column, 
                                 main = paste("Scatter Plot of", x_column, "vs", y_column, "by", category_column),
                                 grid = TRUE)
                            
                            # Add legend
                            legend("topright", legend = categories, col = colors, pch = 16, title = category_column)
                            
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
                            cat("1. Linear Regression (fit_linear_regression): Simple linear model with one predictor.\n")
                            cat("2. Logistic Regression (fit_logistic_regression): For binary outcomes.\n")
                            cat("3. Multiple Linear Regression (fit_multiple_linear_regression): Multiple predictors for numeric outcome.\n")
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
                
                
                ##-----------------------------------------------------
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
                       par(mfrow = c(2, 2))
                       rsm::contour(model, ~ x1 + x2, image = TRUE, main = "Contour Plot")
                       rsm::persp(model, ~ x1 + x2, main = "3D Surface Plot")
                       plot(fitted(model), resid(model), main = "Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals")
                       abline(h = 0, col = "red")
                       rsm::steepest(model, main = "Steepest Ascent Path")
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
                  
                # New Method: Perform Cluster Analysis with Demographic Differences
                perform_cluster_analysis = function(variables, demographic_column, k = 3, plot = TRUE) {
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
                        
                    # Perform k-means clustering
                    set.seed(123)
                    clusters <- kmeans(data_scaled, centers = k, nstart = 25)
                    self$data$cluster <- clusters$cluster
                            
                    # Analyze differences by demographic
                    diff_results <- lapply(variables, function(var) {
                       aov_result <- aov(self$data[[var]] ~ as.factor(self$data[[demographic_column]]) * as.factor(self$data$cluster))
                       summary(aov_result)
                         })
                    names(diff_results) <- variables
                                
                    # Optional plots
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
                                
                    cat("Cluster analysis completed with", k, "clusters on variables", paste(variables, collapse = ", "), "\n")
                                
                    # Return results
                    list(
                        clusters = clusters,
                        differences = diff_results,
                        method = "K-Means Clustering with Demographic Analysis"
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