
# 4.2 Export Detailed Results for each stratification variable
for (strat_var in names(all_stratified_results)) {
  # Create a safe filename (remove special characters)
  safe_var_name <- gsub("[^A-Za-z0-9]", "_", strat_var)
  
  # Export each stratum's detailed results
  for (stratum_name in names(all_stratified_results[[strat_var]])) {
    safe_stratum_name <- gsub("[^A-Za-z0-9]", "_", stratum_name)
    
    # Export the detailed results as RDS (to preserve structure)
    rds_file <- file.path(detailed_dir, paste0(safe_var_name, "_", safe_stratum_name, ".rds"))
    saveRDS(all_stratified_results[[strat_var]][[stratum_name]], rds_file)
    
    # Export as text file for human-readable format
    txt_file <- file.path(detailed_dir, paste0(safe_var_name, "_", safe_stratum_name, ".txt"))
    sink(txt_file)
    cat("Stratification Variable:", strat_var, "\n")
    cat("Stratum:", stratum_name, "\n")
    cat("R-squared:", all_stratified_results[[strat_var]][[stratum_name]]$r_squared, "\n\n")
    print(all_stratified_results[[strat_var]][[stratum_name]])
    sink()
  }
}

cat("\nDetailed results saved to:", detailed_dir, "\n")

# 4.3 Export a text summary of all results
text_summary_file <- file.path(summary_dir, "Stratified_Analysis_Summary.txt")
sink(text_summary_file)
cat("============================================================\n")
cat("STRATIFIED REGRESSION ANALYSIS SUMMARY\n")
cat("============================================================\n")
cat("Analysis performed:", timestamp, "\n\n")
cat("Summary Statistics:\n")
cat("-------------------\n")
print(sratfied_summary_df, row.names = FALSE)
cat("\n\nTop 5 Best Performing Strata (by R-squared):\n")
cat("--------------------------------------------\n")
print(head(stratified_results, 5), row.names = FALSE)
sink()

cat("\nText summary saved to:", text_summary_file, "\n")