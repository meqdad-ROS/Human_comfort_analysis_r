perform_kw_then_pairwise_mw <- function(analyzer, outcome, group_column, plot = FALSE) {
  # Validate inputs
  if (!inherits(analyzer, "regrissorClass")) {
    stop("analyzer must be an instance of regrissorClass")
  }
  if (!outcome %in% names(analyzer$data) || !group_column %in% names(analyzer$data)) {
    stop("Outcome or group column not found")
  }
  if (!is.numeric(analyzer$data[[outcome]])) {
    stop("Outcome must be numeric")
  }
  
  # Perform Kruskal-Wallis test
  kw_result <- analyzer$perform_kruskal_wallis(outcome, group_column, plot = plot)
  
  # Always print KW results
  cat("Kruskal-Wallis Test Results for", outcome, "by", group_column, ":\n")
  cat("p-value:", kw_result$p_value, "\n")
  cat("H-statistic:", kw_result$h_statistic, "\n")
  cat("Degrees of Freedom:", kw_result$degrees_of_freedom, "\n")
  
  # Check if significant
  if (kw_result$p_value < 0.05) {
    cat("Significant differences found (p < 0.05). Proceeding to pairwise Mann-Whitney tests.\n")
    
    # Get unique groups
    groups <- unique(na.omit(analyzer$data[[group_column]]))
    num_groups <- length(groups)
    
    if (num_groups < 2) {
      cat("Not enough groups for pairwise tests.\n")
      return(list(kw = kw_result))
    }
    
    # Generate all unique pairs
    pairs <- combn(groups, 2, simplify = FALSE)
    
    # Perform pairwise Mann-Whitney for each pair
    pairwise_results <- list()
    for (i in seq_along(pairs)) {
      pair <- pairs[[i]]
      # Subset data for the pair
      subset_data <- analyzer$data[analyzer$data[[group_column]] %in% pair, ]
      temp_analyzer <- regrissorClass$new(subset_data)
      
      mw_result <- temp_analyzer$perform_mann_whitney_ks(outcome, group_column, type = "mann_whitney", plot = plot)
      pairwise_results[[i]] <- mw_result
      
      cat("Pairwise Mann-Whitney:", pair[1], "vs", pair[2], "- p-value:", mw_result$p_value, ", statistic:", mw_result$statistic, "\n")
    }
    
    return(list(kw = kw_result, pairwise = pairwise_results, pairs = pairs))
  } else {
    cat("No significant differences found (p >= 0.05). Stopping after Kruskal-Wallis.\n")
    return(list(kw = kw_result))
  }
}