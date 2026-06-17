pairwise_com <- function(dataSet, numeric_var, group_var) {
  
  #Set the number of required pairwise comparession to be performed to define the MONVA output 
  groups <- unique(dataSet$group_var)
  numbOfGroups <- length(groups)
  n_comparisons <- choose(length(groups), numbOfGroups*(numbOfGroups-1)/2)
  alpha_adjusted <- 0.05 / n_comparisons  # Bonferroni correction 
  #print out the numbe rof groups, comparesion and the Bonferroni correction of Alpha
  cat("Number of pairwise comparisons:", n_comparisons, "\n")
  cat("Adjusted alpha level:", alpha_adjusted, "\n\n")
  # Store results
  pairwise_results <- data.frame(
    Comparison = character(),
    Pillai = numeric(),
    F_stat = numeric(),
    p_value = numeric(),
    Significant = character(),
    stringsAsFactors = FALSE
  )
  
  ##========================================================================================
  for(i in 1:(numbOfGroups-1)) {
    for(j in (i+1):length(numbOfGroups)) {
  # Subset data for two groups
  subset_data <- dataSet[dataSet$group_var %in% c(groups[i], groups[j]), ]
  
  # Create DV matrix
    for(i in 1:dim(numeric_var)[2]){
      dv_matrix <- cbind(subset_data[,i])
    }
    
  # Run MANOVA
  pairwise_manova <- manova(dv_matrix ~ subset_data$Language)
  summary_result <- summary(pairwise_manova, test = "Pillai")
  
  # Extract statistics
  pillai <- summary_result$stats[1, "Pillai"]
  f_stat <- summary_result$stats[1, "approx F"]
  p_val <- summary_result$stats[1, "Pr(>F)"]
  
  # Store results
  pairwise_results <- rbind(pairwise_results, 
                            data.frame(
                              Comparison = paste(groups[i], "vs", groups[j]),
                              Pillai = pillai,
                              F_stat = f_stat,
                              p_value = p_val,
                              Significant = ifelse(p_val < alpha_adjusted, "Yes*", "No")
                            ))
  
  cat("========================================\n")
  cat("Comparison:", groups[i], "vs", groups[j], "\n")
  cat("Pillai's Trace:", round(pillai, 4), "\n")
  cat("F-statistic:", round(f_stat, 4), "\n")
  cat("p-value:", format(p_val, scientific = TRUE), "\n")
  cat("Significant (α =", alpha_adjusted, "):", 
      ifelse(p_val < alpha_adjusted, "YES ***", "No"), "\n")
  cat("========================================\n\n")
    }
  }
  
  # Display summary table
  print(pairwise_results)
  
  
  
  ##========================================================================================
  
  
}