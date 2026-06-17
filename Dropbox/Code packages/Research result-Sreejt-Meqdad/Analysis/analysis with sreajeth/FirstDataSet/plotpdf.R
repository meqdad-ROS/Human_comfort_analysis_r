library(ggplot2)
library(ggtext) # Load the ggtext package
# Load required package


# Define the density plot function
plot_density_by_group <- function(data, numeric_var, group_var, 
                                  caption_text = NULL, plot_title = NULL, 
                                  custom_colors = NULL) {
  
  # Convert input variable names to symbols for tidy evaluation
  num_var_sym <- rlang::sym(numeric_var)
  group_var_sym <- rlang::sym(group_var)
  
  # Generate default colors if none provided
  if (is.null(custom_colors)) {
      n_groups <- length(unique(data[[group_var]]))
      custom_colors <- scales::hue_pal()(n_groups)
  }
  
  # Create the density plot
  p <- ggplot(data, aes(x = !!num_var_sym, fill = !!group_var_sym)) +
        geom_density(alpha = 0.5) +
        theme_minimal(base_size = 14) +
        labs(x = numeric_var,
            y = "density",
            title = plot_title,
            caption = caption_text,
            fill = group_var) +
      scale_fill_manual(values = custom_colors) +
      theme(
      plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
      plot.caption = element_text(hjust = 0.5, size = 8),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
      )
  
  return(p)
}


# ggplot(data, aes(x = Sensitivity, fill = Height)) +
#   geom_density(alpha = 0.5) +
#   theme_minimal(base_size = 14) +
#   labs(x = "Sensitivity", y = "density", title = "c") +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(face = "bold", size = 16, hjust = -0.1),
#     legend.title = element_text(size = 12),
#     legend.text = element_text(size = 10),
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 12)
#   ) +
#   scale_fill_manual(values = c("#F4A582", "#FFD700", "#80CDC1", "#4DA6FF", "#8073AC", "#E6A3DC"))
