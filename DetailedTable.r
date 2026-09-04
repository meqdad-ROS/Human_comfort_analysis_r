for (pair_name in names(all_chi_results)) {
  res <- all_chi_results[[pair_name]]
  
  # Create a detailed results data frame for this pair
  detailed <- data.frame(
    Metric = c("Test Type", 
               "Variable 1", 
               "Variable 2",
               "Chi-Square Statistic",
               "Degrees of Freedom",
               "P-Value",
               "Sample Size",
               "Cramér's V",
               "Association Strength",
               "Significant (p < 0.05)"),
    Value = c("Chi-Square Test of Independence",
              res$variable1,
              res$variable2,
              round(res$chi_square_statistic, 4),
              res$degrees_of_freedom,
              format.pval(res$p_value, digits = 6),
              res$sample_size,
              ifelse(is.null(res$effect_sizes$cramers_v), NA, round(res$effect_sizes$cramers_v, 4)),
              ifelse(is.null(res$effect_sizes$cramers_v_interpretation), NA, res$effect_sizes$cramers_v_interpretation),
              res$significant)
  )
  
  # Add Fisher's p-value if available
  if (!is.na(res$fisher_p_value)) {
    detailed <- rbind(detailed, data.frame(
      Metric = "Fisher's Exact P-Value",
      Value = format.pval(res$fisher_p_value, digits = 6)
    ))
  }
  
  # Add expected frequencies min
  if (!is.null(res$expected_frequencies)) {
    detailed <- rbind(detailed, data.frame(
      Metric = "Min Expected Frequency",
      Value = round(min(res$expected_frequencies, na.rm = TRUE), 4)
    ))
  }
  
  # Save detailed results
  detail_file <- file.path(detailed_dir, paste0(pair_name, "_detailed.csv"))
  write.csv(detailed, detail_file, row.names = FALSE)
}
cat("Detailed tables saved to:", detailed_dir, "\n")

# 4.3 Export Contingency Tables
contingency_dir <- file.path(chi_output_dir, "contingency_tables")
dir.create(contingency_dir, showWarnings = FALSE)

for (pair_name in names(all_chi_results)) {
  res <- all_chi_results[[pair_name]]
  
  if (!is.null(res$contingency_table)) {
    # Convert table to data frame
    cont_table <- as.data.frame.matrix(res$contingency_table)
    
    # Add row totals
    cont_table$Row_Total <- rowSums(cont_table)
    
    # Add column totals as a separate row
    col_totals <- colSums(cont_table[, colnames(cont_table) != "Row_Total"])
    cont_table <- rbind(cont_table, c(col_totals, sum(col_totals)))
    rownames(cont_table)[nrow(cont_table)] <- "Column_Total"
    
    # Save contingency table
    cont_file <- file.path(contingency_dir, paste0(pair_name, "_contingency.csv"))
    write.csv(cont_table, cont_file, row.names = TRUE)
  }
}
cat("Contingency tables saved to:", contingency_dir, "\n")

# 4.4 Export Percentage Tables
percentage_dir <- file.path(chi_output_dir, "percentage_tables")
dir.create(percentage_dir, showWarnings = FALSE)

for (pair_name in names(all_chi_results)) {
  res <- all_chi_results[[pair_name]]
  
  if (!is.null(res$percentages)) {
    # Save percentages
    pct_file <- file.path(percentage_dir, paste0(pair_name, "_percentages.csv"))
    write.csv(res$percentages, pct_file, row.names = TRUE)
    
    # Save row percentages
    row_pct_file <- file.path(percentage_dir, paste0(pair_name, "_row_percentages.csv"))
    write.csv(res$row_percentages, row_pct_file, row.names = TRUE)
    
    # Save column percentages
    col_pct_file <- file.path(percentage_dir, paste0(pair_name, "_column_percentages.csv"))
    write.csv(res$col_percentages, col_pct_file, row.names = TRUE)
  }
}
cat("Percentage tables saved to:", percentage_dir, "\n")

# 4.5 Export Standardized Residuals
residuals_dir <- file.path(chi_output_dir, "residuals")
dir.create(residuals_dir, showWarnings = FALSE)

for (pair_name in names(all_chi_results)) {
  res <- all_chi_results[[pair_name]]
  
  if (!is.null(res$std_residuals)) {
    std_res_file <- file.path(residuals_dir, paste0(pair_name, "_std_residuals.csv"))
    write.csv(res$std_residuals, std_res_file, row.names = TRUE)
  }
}
cat("Residuals saved to:", residuals_dir, "\n")

#===============================================================================
# 5. CREATE COMPREHENSIVE REPORT
#===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("GENERATING REPORT\n")
cat(rep("=", 80), "\n\n", sep = "")

# Create a text report
report_file <- file.path(chi_output_dir, "ChiSquare_Report.txt")

# Count significant associations
significant_count <- sum(summary_df$Significant, na.rm = TRUE)
total_tests <- nrow(summary_df)

# Identify strongest associations
if (nrow(summary_df) > 0 && any(!is.na(summary_df$Cramers_V))) {
  strongest <- summary_df[which.max(summary_df$Cramers_V), ]
  strongest_text <- sprintf("Strongest association: %s vs %s (Cramér's V = %.4f, p = %s)",
                            strongest$Variable_1,
                            strongest$Variable_2,
                            strongest$Cramers_V,
                            format.pval(strongest$ChiSq_P_Value, digits = 4))
} else {
  strongest_text <- "No associations with valid Cramér's V found."
}

# Build report content
report_lines <- c(
  strrep("=", 80),
  "CHI-SQUARE TEST ANALYSIS REPORT",
  strrep("=", 80),
  paste("Analysis Date:", Sys.time()),
  paste("Data Source:", analy_SCFA$data_source),
  paste("Number of Observations:", nrow(analy_SCFA$data)),
  paste("Number of Categorical Variables:", length(categorical_vars)),
  paste("Number of Tests Performed:", total_tests),
  paste("Significant Associations (p < 0.05):", significant_count),
  paste("Significant Association Rate:", round(significant_count/total_tests * 100, 2), "%"),
  "",
  strrep("-", 80),
  "SUMMARY OF FINDINGS",
  strrep("-", 80),
  strongest_text,
  "",
  strrep("-", 80),
  "ALL SIGNIFICANT ASSOCIATIONS (p < 0.05)",
  strrep("-", 80)
)

# Add significant associations
if (significant_count > 0) {
  sig_associations <- summary_df[summary_df$Significant == TRUE, ]
  for (i in 1:nrow(sig_associations)) {
    row <- sig_associations[i, ]
    report_lines <- c(report_lines,
                      sprintf("  %d. %s vs %s", i, row$Variable_1, row$Variable_2),
                      sprintf("     Chi-Sq: χ²=%.4f, df=%d, p=%s", 
                              row$ChiSq_Statistic, row$ChiSq_DF, format.pval(row$ChiSq_P_Value, digits = 4)),
                      sprintf("     Cramér's V: %.4f (%s)", row$Cramers_V, row$Association_Strength),
                      sprintf("     Sample Size: %d", row$Sample_Size),
                      ""
    )
  }
} else {
  report_lines <- c(report_lines, "  No significant associations found at p < 0.05")
}

report_lines <- c(report_lines,
                  "",
                  strrep("-", 80),
                  "FILES GENERATED",
                  strrep("-", 80),
                  paste("1. Summary CSV:", "ChiSquare_Summary.csv"),
                  paste("2. Detailed Tables:", "detailed_tables/"),
                  paste("3. Contingency Tables:", "contingency_tables/"),
                  paste("4. Percentage Tables:", "percentage_tables/"),
                  paste("5. Residuals:", "residuals/"),
                  "",
                  strrep("=", 80),
                  "END OF REPORT",
                  strrep("=", 80)
)

writeLines(report_lines, report_file)
cat("Report saved to:", report_file, "\n")

#===============================================================================
# 6. SAVE COMPLETE RESULTS AS RDS
#===============================================================================

rds_file <- file.path(chi_output_dir, "ChiSquare_Complete_Results.rds")
saveRDS(list(
  all_results = all_chi_results,
  summary = summary_df,
  categorical_variables = categorical_vars,
  total_tests = total_tests,
  significant_count = significant_count,
  timestamp = timestamp
), rds_file)
cat("Complete R results saved to:", rds_file, "\n")

#===============================================================================
# 7. FINAL MESSAGE
#===============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("CHI-SQUARE ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n", sep = "")
cat("Results saved to:", chi_output_dir, "\n")
cat("\nFiles generated:\n")
cat("  - ChiSquare_Summary.csv : Summary of all tests\n")
cat("  - ChiSquare_Complete_Results.rds : Complete R data\n")
cat("  - ChiSquare_Report.txt : Text report\n")
cat("  - detailed_tables/ : Detailed results for each pair\n")
cat("  - contingency_tables/ : Raw contingency tables\n")
cat("  - percentage_tables/ : Percentage tables\n")
cat("  - residuals/ : Standardized residuals\n")
cat("\n")
