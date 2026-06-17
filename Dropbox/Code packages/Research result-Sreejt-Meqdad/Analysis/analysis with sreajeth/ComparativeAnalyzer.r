library(R6)
library(tidyr)

ComparativeAnalyzer <- R6Class("ComparativeAnalyzer",
                               public = list(
                                 # Properties
                                 analyzer = NULL,  # Instance of regrissorClass
                                 numeric_col = NULL,  # e.g., "Sensitivity"
                                 categorical_col = NULL,  # e.g., "Gender"
                                 
                                 # Constructor
                                 initialize = function(analyzer, numeric_col, categorical_col) {
                                   if (!inherits(analyzer, "regrissorClass")) {
                                     stop("analyzer must be an instance of regrissorClass")
                                   }
                                   if (!numeric_col %in% names(analyzer$data) || !is.numeric(analyzer$data[[numeric_col]])) {
                                     stop("numeric_col must be a valid numeric column in the data")
                                   }
                                   if (!categorical_col %in% names(analyzer$data)) {
                                     stop("categorical_col must be a valid column in the data")
                                   }
                                   groups <- unique(na.omit(analyzer$data[[categorical_col]]))
                                   if (length(groups) != 2) {
                                     stop("categorical_col must have exactly 2 unique groups")
                                   }
                                   self$analyzer <- analyzer
                                   self$numeric_col <- numeric_col
                                   self$categorical_col <- categorical_col
                                   cat("ComparativeAnalyzer initialized for", numeric_col, "by", categorical_col, "\n")
                                 },
                                 
                                 # Method 1: Independent t-test by categorical group (parametric, assumes normality)
                                 perform_independent_t_test = function(plot = TRUE) {
                                   result <- self$analyzer$perform_t_test(self$numeric_col, self$categorical_col, type = "independent", plot = plot)
                                   cat("Independent t-test for", self$numeric_col, "by", self$categorical_col, "\n")
                                   cat("p-value:", result$p_value, "\n")
                                   if (result$p_value < 0.05) {
                                     cat("Reject null: Means differ significantly.\n")
                                   } else {
                                     cat("Fail to reject null: No significant difference.\n")
                                   }
                                   return(result)
                                 },
                                 
                                 # NEW Method: Mann-Whitney U test (non-parametric, for non-normally distributed groups)
                                 perform_mann_whitney_test = function(plot = TRUE) {
                                   result <- self$analyzer$perform_mann_whitney_ks(self$numeric_col, self$categorical_col, type = "mann_whitney", plot = plot)
                                   cat("Mann-Whitney U test for", self$numeric_col, "by", self$categorical_col, "\n")
                                   cat("p-value:", result$p_value, "\n")
                                   if (result$p_value < 0.05) {
                                     cat("Reject null: Distributions differ significantly.\n")
                                   } else {
                                     cat("Fail to reject null: No significant difference in distributions.\n")
                                   }
                                   return(result)
                                 },
                                 
                                 # Method 2: One-sample t-test against a specified mu
                                 perform_one_sample_t_test = function(mu = 5.0, plot = TRUE) {
                                   result <- self$analyzer$perform_t_test(self$numeric_col, type = "one_sample", mu = mu, plot = plot)
                                   cat("One-sample t-test for", self$numeric_col, "vs mu =", mu, "\n")
                                   cat("p-value:", result$p_value, "\n")
                                   if (result$p_value < 0.05) {
                                     cat("Reject null: Mean differs from", mu, ".\n")
                                   } else {
                                     cat("Fail to reject null: Mean not significantly different from", mu, ".\n")
                                   }
                                   return(result)
                                 },
                                 
                                 # Method 3: Paired t-test on simulated before/after (generalized from your code)
                                 perform_paired_t_test_simulated = function(mean_diff = 0.2, sd_diff = 0.1, plot = TRUE) {
                                   n <- nrow(self$analyzer$data)
                                   set.seed(42)
                                   after_values <- self$analyzer$data[[self$numeric_col]] + rnorm(n, mean = mean_diff, sd = sd_diff)
                                   temp_analyzer <- self$analyzer$clone()  # Clone to avoid modifying original
                                   temp_analyzer$add_column("after_numeric", after_values)
                                   
                                   df <- temp_analyzer$data
                                   df_long <- pivot_longer(df, cols = c(self$numeric_col, "after_numeric"), 
                                                           names_to = "time", values_to = "value")
                                   df_long$time <- factor(df_long$time, levels = c(self$numeric_col, "after_numeric"), 
                                                          labels = c("before", "after"))
                                   
                                   paired_analyzer <- regrissorClass$new(df_long)
                                   result <- paired_analyzer$perform_t_test("value", "time", type = "paired", plot = plot)
                                   cat("Paired t-test for simulated before/after", self$numeric_col, "\n")
                                   cat("p-value:", result$p_value, "\n")
                                   if (result$p_value < 0.05) {
                                     cat("Reject null: Significant change between before/after.\n")
                                   } else {
                                     cat("Fail to reject null: No significant change.\n")
                                   }
                                   return(result)
                                 },
                                 
                                 # Method 4: Before/After comparison by categorical group (generalized from your code)
                                 # Method 4: Before/After comparison by categorical group (updated to include Mann-Whitney)
                                 perform_before_after_comparison = function(plot = TRUE) {
                                   # Extract before/after data
                                   df_before <- self$analyzer$data[1:360, ]
                                   df_after <- self$analyzer$data[361:572, ]
                                   
                                   # Split by categorical group
                                   before_group1 <- df_before[df_before[[self$categorical_col]] == unique(df_before[[self$categorical_col]])[1], ]
                                   before_group2 <- df_before[df_before[[self$categorical_col]] == unique(df_before[[self$categorical_col]])[2], ]
                                   after_group1 <- df_after[df_after[[self$categorical_col]] == unique(df_after[[self$categorical_col]])[1], ]
                                   after_group2 <- df_after[df_after[[self$categorical_col]] == unique(df_after[[self$categorical_col]])[2], ]
                                   
                                   # Determine sample sizes
                                   j <- min(nrow(before_group1), nrow(after_group1))
                                   k <- min(nrow(before_group2), nrow(after_group2))
                                   i <- j + k
                                   
                                   set.seed(123)
                                   # Sample and order
                                   before_ordered <- rbind(before_group1[sample(nrow(before_group1), j), ], 
                                                           before_group2[sample(nrow(before_group2), k), ])
                                   after_ordered <- rbind(after_group1[sample(nrow(after_group1), j), ], 
                                                          after_group2[sample(nrow(after_group2), k), ])
                                   
                                   # Create paired df
                                   paired_df <- data.frame(
                                     id = 1:nrow(before_ordered),
                                     categorical = before_ordered[[self$categorical_col]],
                                     before = before_ordered[[self$numeric_col]],
                                     after = after_ordered[[self$numeric_col]]
                                   )
                                   
                                   # Reshape and analyze
                                   paired_long <- pivot_longer(paired_df, cols = c("before", "after"), 
                                                               names_to = "time", values_to = "value")
                                   paired_long$time <- factor(paired_long$time, levels = c("before", "after"))
                                   
                                   paired_analyzer <- regrissorClass$new(paired_long)
                                   
                                   # Paired t-test overall (unchanged)
                                   paired_result <- paired_analyzer$perform_t_test("value", "time", type = "paired", plot = plot)
                                   cat("Paired t-test for before/after", self$numeric_col, "\n")
                                   cat("p-value:", paired_result$p_value, "\n")
                                   if (paired_result$p_value < 0.05) {
                                     cat("Reject null: Significant change over time.\n")
                                   } else {
                                     cat("Fail to reject null: No significant change.\n")
                                   }
                                   
                                   # Independent t-tests and Mann-Whitney tests by categorical for before and after
                                   before_analyzer <- regrissorClass$new(paired_df)
                                   before_t <- before_analyzer$perform_t_test("before", "categorical", type = "independent", plot = plot)
                                   before_mw <- before_analyzer$perform_mann_whitney_ks("before", "categorical", type = "mann_whitney", plot = plot)
                                   
                                   after_t <- before_analyzer$perform_t_test("after", "categorical", type = "independent", plot = plot)
                                   after_mw <- before_analyzer$perform_mann_whitney_ks("after", "categorical", type = "mann_whitney", plot = plot)
                                   
                                   cat("Before period - Independent t-test by", self$categorical_col, "- p-value:", before_t$p_value, "\n")
                                   cat("Before period - Mann-Whitney by", self$categorical_col, "- p-value:", before_mw$p_value, "\n")
                                   cat("After period - Independent t-test by", self$categorical_col, "- p-value:", after_t$p_value, "\n")
                                   cat("After period - Mann-Whitney by", self$categorical_col, "- p-value:", after_mw$p_value, "\n")
                                   
                                   return(list(paired = paired_result, before_independent = before_t, before_mann_whitney = before_mw, after_independent = after_t, after_mann_whitney = after_mw))
                                 },
                                 
                                 # Method 5: Run all analyses in sequence (updated to include Mann-Whitney)
                                 run_all_analyses = function(mu = 5.0, mean_diff = 0.2, sd_diff = 0.1, plot = TRUE, use_nonparametric = FALSE) {
                                   cat("=== Running All Analyses for", self$numeric_col, "by", self$categorical_col, "===\n")
                                   if (use_nonparametric) {
                                     mw_result <- self$perform_mann_whitney_test(plot)
                                   } else {
                                     ind_result <- self$perform_independent_t_test(plot)
                                   }
                                   one_result <- self$perform_one_sample_t_test(mu, plot)
                                   paired_sim_result <- self$perform_paired_t_test_simulated(mean_diff, sd_diff, plot)
                                   before_after_results <- self$perform_before_after_comparison(plot)
                                   if (use_nonparametric) {
                                     return(list(mann_whitney = mw_result, one_sample = one_result, paired_sim = paired_sim_result, before_after = before_after_results))
                                   } else {
                                     return(list(independent = ind_result, one_sample = one_result, paired_sim = paired_sim_result, before_after = before_after_results))
                                   }
                                 }
                               )
)