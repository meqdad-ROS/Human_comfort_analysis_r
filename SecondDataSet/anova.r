library(ggplot2)
library(reshape2)


One_way_Anova <- function(variable, value){
  new.df <- data.frame(
    Variable = variable,
    value = value
  )
  
  long_data <- melt(new.df)
  
  # Step 3: One-Way ANOVA
  one_way_anova <- aov(value ~ Variable, data = long_data)
  summary(one_way_anova)
  
  # Step 4: Interpretation of One-Way ANOVA
  anova_results <- summary(one_way_anova)
  p_value_one_way <- anova_results[[1]][["Pr(>F)"]][1]
  
  if (p_value_one_way < 0.05) {
    cat("One-Way ANOVA: Significant differences exist between groups (p =", p_value_one_way, ")\n")
  } else {
    cat("One-Way ANOVA: No significant differences between groups (p =", p_value_one_way, ")\n")
  }
  
  return(p_value_one_way)
}
###############################################################################################
###############################################################################################

two_way_anova <- function(variable1, variable2, value){
    new.df <- data.frame(
       Variable = variable1,
       group = variable2,
       value = value
    )
    two_way_anova <- aov(value ~ Variable * group , data = new.df)
    summary(two_way_anova)
  
    ##Interpretation of Two-Way ANOVA
    anova_results_two_way <- summary(two_way_anova)
    anova_results_two_way[[1]][["Pr(>F)"]]
    p_value_variable <- anova_results_two_way[[1]][["Pr(>F)"]][2]
    p_value_interaction <- anova_results_two_way[[1]][["Pr(>F)"]][3]
    p_value_group <- anova_results_two_way[[1]][["Pr(>F)"]][1]
    
    
    if (p_value_variable < 0.05) {
      cat("Two-Way ANOVA: Significant differences exist between variables (p =", p_value_variable, ")\n")
    } else {
      cat("Two-Way ANOVA: No significant differences between variables (p =", p_value_variable, ")\n")
    }
    
    if (p_value_group < 0.05) {
      cat("Two-Way ANOVA: Significant differences exist between groups (p =", p_value_group, ")\n")
    } else {
      cat("Two-Way ANOVA: No significant differences between groups (p =", p_value_group, ")\n")
    }
    
    if (p_value_interaction < 0.05) {
      cat("Two-Way ANOVA: Significant interaction between variable and group (p =", p_value_interaction, ")\n")
    } else {
      cat("Two-Way ANOVA: No significant interaction between variable and group (p =", p_value_interaction, ")\n")
    }
    
    return(anova_results_two_way[[1]][["Pr(>F)"]])
}

