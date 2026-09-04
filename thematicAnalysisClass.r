# ==============================================================================
# thematicAnalysisClass.r
#
# Written by Meqdad Hasan
#
# PURPOSE
# -------
# Provides a rigorous, reproducible "thematic analysis" pipeline for NUMERIC
# survey data, applicable to BOTH:
#
#   (a) CONTINUOUS variables  (e.g. CFA/BCFA factor scores, standardized
#       composite scores, or any approximately interval-scaled measure), and
#   (b) ORDINAL variables     (e.g. 5-point Likert items coded 1-5).
#
# "Thematic analysis" here is operationalised in the psychometric sense: the
# discovery and validation of latent THEMES (constructs) that group a set of
# numeric variables together. Two independent, complementary techniques are
# used and then cross-validated against one another:
#
#   1. EXPLORATORY FACTOR ANALYSIS (EFA)
#        - Continuous data -> Pearson-correlation-based EFA
#          (psych::fa, fm = "minres" by default)
#        - Ordinal data    -> Polychoric-correlation-based EFA
#          (psych::polychoric + psych::fa on the polychoric matrix)
#        - Number of themes (factors) is suggested objectively via parallel
#          analysis (psych::fa.parallel / fa.parallel.poly) unless supplied.
#
#   2. CORRELATION-DISTANCE HIERARCHICAL CLUSTERING of the SAME variables
#        - distance = 1 - |correlation|  (Pearson or polychoric, matching #1)
#        - agglomeration via Ward's minimum-variance method (ward.D2)
#
#   3. CROSS-VALIDATION
#        - Each variable's EFA-assigned theme (its highest absolute loading)
#          is compared against its hierarchical-cluster membership.
#        - Cluster labels are re-aligned to the EFA theme labels that they
#          overlap with the most (greedy maximum-overlap matching), so the
#          two methods can be compared on a common labelling.
#        - Agreement is reported as (i) simple percentage agreement and
#          (ii) the Adjusted Rand Index (ARI), a chance-corrected measure of
#          agreement between two partitions of the same variables.
#
# All results (loading tables, cluster assignments, cross-validation tables,
# diagrams, dendrograms, a full narrative TXT report, and an RDS snapshot of
# every R object produced) are written by save_thematic_report() into a
# folder that is named after the supplied test name and the exact run
# timestamp, e.g.:
#
#     ThematicAnalysis_IEQ_Ordinal_20260904_221530/
#
# DEPENDENCIES: psych, GPArotation (for oblique rotation inside psych::fa)
# install.packages(c("psych", "GPArotation"))
# ==============================================================================

# install.packages(c("R6", "psych", "GPArotation"))
library(R6)
suppressWarnings(suppressMessages({
  library(psych)
  library(GPArotation)
}))

thematicAnalysisClass <- R6Class(
  "thematicAnalysisClass",
  public = list(

    # ---------------------------------------------------------------------
    # Properties
    # ---------------------------------------------------------------------
    data              = NULL,
    numeric_cols      = NULL,
    categorical_cols  = NULL,
    ordinal_cols      = NULL,   # numeric columns treated as ordinal (Likert)
    continuous_cols   = NULL,   # numeric columns treated as continuous
    data_source       = NULL,

    # ---------------------------------------------------------------------
    # Constructor
    # ---------------------------------------------------------------------
    # input           : data.frame OR path to a CSV file
    # ordinal_vars    : optional character vector -> force these numeric
    #                   columns to be treated as ORDINAL (skips auto-detect)
    # continuous_vars : optional character vector -> force these numeric
    #                   columns to be treated as CONTINUOUS
    # unique_threshold: for AUTO-detection only. A numeric column with
    #                   integer-only values and <= unique_threshold distinct
    #                   levels is classified as ordinal; everything else
    #                   numeric is classified as continuous.
    # ---------------------------------------------------------------------
    initialize = function(input,
                           file_sep = ",",
                           header = TRUE,
                           stringsAsFactors = FALSE,
                           na.strings = c("", "NA", "N/A"),
                           ordinal_vars = NULL,
                           continuous_vars = NULL,
                           unique_threshold = 10) {

      # Step 1: Validate and load input (file path or data.frame)
      if (is.character(input)) {
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

      # Step 3: Light cleaning (mirrors regrissorClass/cfTestClass behaviour)
      self$data <- self$data[rowSums(is.na(self$data)) != ncol(self$data), ]
      self$data <- self$data[, colSums(is.na(self$data)) != nrow(self$data)]
      self$data[self$data == ""] <- NA

      # Step 4: Identify numeric vs categorical columns
      self$numeric_cols     <- names(self$data)[sapply(self$data, is.numeric)]
      self$categorical_cols <- names(self$data)[sapply(self$data, function(x) !is.numeric(x))]

      # Step 5: Classify numeric columns into ordinal vs continuous
      private$classify_numeric_columns(ordinal_vars, continuous_vars, unique_threshold)

      cat("thematicAnalysisClass initialized with", nrow(self$data), "rows and", ncol(self$data), "columns\n")
      cat("Numeric columns:", length(self$numeric_cols),
          "| Categorical columns:", length(self$categorical_cols), "\n")
      cat("  -> Continuous (interval-scale):", length(self$continuous_cols),
          if (length(self$continuous_cols) > 0) paste0("[", paste(self$continuous_cols, collapse = ", "), "]") else "", "\n")
      cat("  -> Ordinal (Likert-type):      ", length(self$ordinal_cols),
          if (length(self$ordinal_cols) > 0) paste0("[", paste(self$ordinal_cols, collapse = ", "), "]") else "", "\n")
      cat("Data source:", self$data_source, "\n")
    },

    # =======================================================================
    # METHOD: re-classify numeric columns on demand (e.g. after add_column)
    # =======================================================================
    classify_variables = function(ordinal_vars = NULL, continuous_vars = NULL, unique_threshold = 10) {
      private$classify_numeric_columns(ordinal_vars, continuous_vars, unique_threshold)
      cat("Reclassified -> Continuous:", length(self$continuous_cols),
          "| Ordinal:", length(self$ordinal_cols), "\n")
      return(invisible(self))
    },

    # =======================================================================
    # METHOD 1: Check factorability (KMO + Bartlett's test of sphericity)
    # =======================================================================
    # cor_type: "pearson" for continuous data, "polychoric" for ordinal data
    check_factorability = function(variables, cor_type = c("pearson", "polychoric")) {
      cor_type <- match.arg(cor_type)
      private$validate_columns(variables)

      analysis_data <- self$data[, variables, drop = FALSE]
      analysis_data <- analysis_data[complete.cases(analysis_data), , drop = FALSE]
      n_obs <- nrow(analysis_data)

      cor_matrix <- private$get_correlation_matrix(analysis_data, cor_type)

      kmo_result      <- psych::KMO(cor_matrix)
      bartlett_result <- psych::cortest.bartlett(cor_matrix, n = n_obs)

      cat("\n", rep("=", 70), "\n", sep = "")
      cat("FACTORABILITY DIAGNOSTICS (", toupper(cor_type), " correlations)\n", sep = "")
      cat(rep("=", 70), "\n\n", sep = "")
      cat(sprintf("Sample size (complete cases): %d\n", n_obs))
      cat(sprintf("Overall KMO (Measure of Sampling Adequacy): %.4f  (%s)\n",
                  kmo_result$MSA, private$interpret_kmo(kmo_result$MSA)))
      cat(sprintf("Bartlett's test of sphericity: chi-sq = %.4f, df = %d, p = %.6f\n",
                  bartlett_result$chisq, bartlett_result$df, bartlett_result$p.value))
      cat(sprintf("Interpretation: %s\n",
                  ifelse(bartlett_result$p.value < 0.05 && kmo_result$MSA >= 0.5,
                         "Data are suitable for factor analysis (correlations are significantly different from an identity matrix and sampling adequacy is acceptable).",
                         "Caution: data may not be well suited for factor analysis (check KMO >= 0.5 and Bartlett p < 0.05).")))
      cat(rep("=", 70), "\n\n", sep = "")

      return(invisible(list(
        cor_type   = cor_type,
        n_obs      = n_obs,
        cor_matrix = cor_matrix,
        kmo        = kmo_result,
        bartlett   = bartlett_result
      )))
    },

    # =======================================================================
    # METHOD 2: Determine the number of themes (factors) via parallel analysis
    # =======================================================================
    determine_num_factors = function(variables, cor_type = c("pearson", "polychoric"), plot = TRUE) {
      cor_type <- match.arg(cor_type)
      private$validate_columns(variables)

      analysis_data <- self$data[, variables, drop = FALSE]
      analysis_data <- analysis_data[complete.cases(analysis_data), , drop = FALSE]

      cat("\nRunning parallel analysis (", cor_type, " correlations) to suggest the number of themes...\n", sep = "")

      pa <- tryCatch({
        if (cor_type == "polychoric") {
          psych::fa.parallel(analysis_data, cor = "poly", fa = "fa", plot = plot,
                              main = "Parallel Analysis Scree Plot (Polychoric / Ordinal)")
        } else {
          psych::fa.parallel(analysis_data, cor = "cor", fa = "fa", plot = plot,
                              main = "Parallel Analysis Scree Plot (Pearson / Continuous)")
        }
      }, error = function(e) {
        warning("Parallel analysis failed (", e$message, "); defaulting to 1 factor.")
        list(nfact = 1)
      })

      suggested <- max(1, pa$nfact)
      cat(sprintf("Parallel analysis suggests %d theme(s)/factor(s).\n\n", suggested))

      return(invisible(list(suggested_n_factors = suggested, parallel_analysis = pa)))
    },

    # =======================================================================
    # METHOD 3: Exploratory Factor Analysis (the core "theme discovery" step)
    # =======================================================================
    # variables    : character vector of numeric column names to analyse
    # cor_type     : "pearson" (continuous) or "polychoric" (ordinal)
    # n_factors    : if NULL, determined automatically via parallel analysis
    # rotation     : oblique rotation allowing correlated themes (default)
    # fm           : factoring method passed to psych::fa (minres/ml/wls...)
    # theme_labels : optional named vector/list to relabel Factor1, Factor2...
    perform_efa = function(variables,
                            cor_type = c("pearson", "polychoric"),
                            n_factors = NULL,
                            rotation = "oblimin",
                            fm = "minres",
                            theme_labels = NULL) {
      cor_type <- match.arg(cor_type)
      private$validate_columns(variables)

      analysis_data <- self$data[, variables, drop = FALSE]
      analysis_data <- analysis_data[complete.cases(analysis_data), , drop = FALSE]
      n_obs <- nrow(analysis_data)

      if (is.null(n_factors)) {
        pa_res    <- self$determine_num_factors(variables, cor_type = cor_type, plot = FALSE)
        n_factors <- pa_res$suggested_n_factors
      }
      n_factors <- max(1, min(n_factors, length(variables) - 1))

      cor_matrix <- private$get_correlation_matrix(analysis_data, cor_type)

      cat("\n", rep("=", 70), "\n", sep = "")
      cat(sprintf("EXPLORATORY FACTOR ANALYSIS  (%s correlations, %d theme(s))\n",
                  toupper(cor_type), n_factors))
      cat(rep("=", 70), "\n\n", sep = "")

      fa_result <- tryCatch({
        psych::fa(r = cor_matrix, nfactors = n_factors, n.obs = n_obs,
                  rotate = rotation, fm = fm)
      }, error = function(e) {
        stop("EFA failed: ", e$message)
      })

      loadings_mat <- unclass(fa_result$loadings)
      colnames(loadings_mat) <- if (!is.null(theme_labels) && length(theme_labels) == ncol(loadings_mat)) {
        as.character(theme_labels)
      } else {
        paste0("Theme_", seq_len(ncol(loadings_mat)))
      }

      # Assign each variable to the theme on which it loads most strongly
      dominant_theme_idx <- apply(abs(loadings_mat), 1, which.max)
      dominant_loading    <- sapply(seq_len(nrow(loadings_mat)), function(i) loadings_mat[i, dominant_theme_idx[i]])

      theme_assignment <- data.frame(
        Variable        = rownames(loadings_mat),
        Dominant_Theme  = colnames(loadings_mat)[dominant_theme_idx],
        Loading         = round(dominant_loading, 4),
        Loading_Strength = sapply(dominant_loading, private$interpret_loading),
        stringsAsFactors = FALSE
      )

      variance_df <- data.frame(
        Theme               = colnames(loadings_mat),
        SS_Loadings         = round(fa_result$Vaccounted["SS loadings", ], 4),
        Proportion_Variance = round(fa_result$Vaccounted["Proportion Var", ], 4),
        Cumulative_Variance = if ("Cumulative Var" %in% rownames(fa_result$Vaccounted))
          round(fa_result$Vaccounted["Cumulative Var", ], 4) else NA,
        stringsAsFactors = FALSE
      )

      cat("Loadings (variables assigned to their dominant theme):\n")
      print(theme_assignment, row.names = FALSE)
      cat("\nVariance explained per theme:\n")
      print(variance_df, row.names = FALSE)
      cat(sprintf("\nModel fit: chi-sq = %.4f, df = %d, p = %s | TLI = %.4f | RMSEA = %.4f\n",
                  ifelse(is.null(fa_result$STATISTIC), NA, fa_result$STATISTIC),
                  ifelse(is.null(fa_result$dof), NA, fa_result$dof),
                  ifelse(is.null(fa_result$PVAL), "NA", format.pval(fa_result$PVAL, digits = 3)),
                  ifelse(is.null(fa_result$TLI), NA, fa_result$TLI),
                  ifelse(is.null(fa_result$RMSEA), NA, fa_result$RMSEA[1])))

      # Ultra-Heywood case diagnostic (a uniqueness at/near zero, or a loading
      # > 1 in absolute value, signals an improper/inadmissible solution that
      # should be flagged explicitly rather than silently reported as fact).
      heywood_flag <- any(fa_result$uniquenesses < 0.01, na.rm = TRUE) || any(abs(loadings_mat) > 1.001, na.rm = TRUE)
      if (isTRUE(heywood_flag)) {
        cat("\nCAUTION: An ultra-Heywood case was detected (a communality >= 1 / uniqueness <= 0).\n")
        cat("This indicates an improper solution, often caused by too few variables per theme,\n")
        cat("a small sample size, or near-collinear items. Interpret loadings with care and\n")
        cat("consider merging themes, adding indicators, or using a smaller n_factors.\n")
      }
      cat(rep("=", 70), "\n\n", sep = "")

      return(invisible(list(
        cor_type          = cor_type,
        n_factors         = n_factors,
        n_obs             = n_obs,
        fa_result         = fa_result,
        loadings_matrix   = loadings_mat,
        theme_assignment  = theme_assignment,
        variance_explained = variance_df,
        variables         = variables,
        analysis_data     = analysis_data,   # complete-case data actually used (for later re-plotting)
        heywood_case      = heywood_flag
      )))
    },

    # =======================================================================
    # METHOD 4: Correlation-distance hierarchical clustering (validation arm)
    # =======================================================================
    perform_hierarchical_clustering = function(variables,
                                                cor_type = c("pearson", "polychoric"),
                                                k = NULL,
                                                method = "ward.D2",
                                                plot = TRUE) {
      cor_type <- match.arg(cor_type)
      private$validate_columns(variables)

      analysis_data <- self$data[, variables, drop = FALSE]
      analysis_data <- analysis_data[complete.cases(analysis_data), , drop = FALSE]

      cor_matrix <- private$get_correlation_matrix(analysis_data, cor_type)
      dist_matrix <- as.dist(1 - abs(cor_matrix))
      hc <- hclust(dist_matrix, method = method)

      if (is.null(k)) k <- 2
      k <- max(1, min(k, length(variables) - 1))

      cluster_membership <- cutree(hc, k = k)

      cluster_assignment <- data.frame(
        Variable = names(cluster_membership),
        Cluster  = paste0("Cluster_", cluster_membership),
        stringsAsFactors = FALSE
      )

      if (isTRUE(plot)) {
        plot(hc, main = paste("Hierarchical Clustering of Variables (", toupper(cor_type), " distance)"),
             xlab = "", sub = "", cex = 0.8)
        if (k > 1) rect.hclust(hc, k = k, border = "red")
      }

      cat("\nHierarchical clustering (", cor_type, " distance, ", method, " linkage, k = ", k, "):\n", sep = "")
      print(cluster_assignment, row.names = FALSE)

      return(invisible(list(
        cor_type           = cor_type,
        k                  = k,
        hclust_obj         = hc,
        distance_matrix    = dist_matrix,
        cor_matrix         = cor_matrix,
        cluster_assignment = cluster_assignment
      )))
    },

    # =======================================================================
    # METHOD 5: Cross-validate EFA-derived themes against cluster membership
    # =======================================================================
    # Aligns cluster labels to the EFA theme labels they overlap with most,
    # then reports percentage agreement and the Adjusted Rand Index (ARI).
    cross_validate_themes = function(efa_output, cluster_output) {

      efa_df     <- efa_output$theme_assignment[, c("Variable", "Dominant_Theme")]
      cluster_df <- cluster_output$cluster_assignment

      merged <- merge(efa_df, cluster_df, by = "Variable", all = TRUE)
      merged <- merged[complete.cases(merged), ]

      # --- Align cluster labels to EFA theme labels (greedy max-overlap matching) ---
      contingency <- table(merged$Cluster, merged$Dominant_Theme)
      relabel_map <- private$greedy_label_alignment(contingency)

      merged$Cluster_Aligned <- unname(relabel_map[merged$Cluster])
      merged$Agrees <- merged$Cluster_Aligned == merged$Dominant_Theme

      percent_agreement <- round(100 * mean(merged$Agrees), 2)
      ari <- private$adjusted_rand_index(merged$Dominant_Theme, merged$Cluster)

      cat("\n", rep("=", 70), "\n", sep = "")
      cat("CROSS-VALIDATION: EFA THEMES  vs.  HIERARCHICAL CLUSTERS\n")
      cat(rep("=", 70), "\n\n", sep = "")
      print(merged[, c("Variable", "Dominant_Theme", "Cluster", "Cluster_Aligned", "Agrees")], row.names = FALSE)
      cat(sprintf("\nSimple percentage agreement (after optimal label alignment): %.2f%%\n", percent_agreement))
      cat(sprintf("Adjusted Rand Index (chance-corrected agreement):            %.4f  (%s)\n",
                  ari, private$interpret_ari(ari)))
      cat("Note: ARI = 1 indicates perfect agreement between the two methods; ARI = 0 indicates\n")
      cat("      agreement no better than chance; negative values indicate systematic disagreement.\n")
      cat(rep("=", 70), "\n\n", sep = "")

      return(invisible(list(
        comparison_table   = merged,
        contingency_table  = contingency,
        percent_agreement  = percent_agreement,
        adjusted_rand_index = ari
      )))
    },

    # =======================================================================
    # METHOD 6: Full pipeline wrapper (EFA + clustering + cross-validation)
    #           Run this once per variable set (continuous OR ordinal).
    # =======================================================================
    perform_thematic_analysis = function(variables,
                                          cor_type = c("pearson", "polychoric"),
                                          n_factors = NULL,
                                          rotation = "oblimin",
                                          fm = "minres",
                                          theme_labels = NULL,
                                          cluster_method = "ward.D2",
                                          plot = TRUE) {
      cor_type <- match.arg(cor_type)

      cat("\n\n", rep("#", 78), "\n", sep = "")
      cat("# THEMATIC ANALYSIS PIPELINE  |  variable type: ",
          ifelse(cor_type == "polychoric", "ORDINAL (Likert)", "CONTINUOUS"), "\n", sep = "")
      cat(rep("#", 78), "\n", sep = "")

      factorability <- self$check_factorability(variables, cor_type = cor_type)

      if (is.null(n_factors)) {
        pa_res    <- self$determine_num_factors(variables, cor_type = cor_type, plot = plot)
        n_factors <- pa_res$suggested_n_factors
      } else {
        pa_res <- NULL
      }

      efa_output <- self$perform_efa(variables, cor_type = cor_type, n_factors = n_factors,
                                      rotation = rotation, fm = fm, theme_labels = theme_labels)

      cluster_output <- self$perform_hierarchical_clustering(variables, cor_type = cor_type,
                                                               k = n_factors, method = cluster_method,
                                                               plot = plot)

      crossval_output <- self$cross_validate_themes(efa_output, cluster_output)

      return(invisible(list(
        variable_type    = ifelse(cor_type == "polychoric", "ordinal", "continuous"),
        variables        = variables,
        cor_type         = cor_type,
        factorability    = factorability,
        parallel_analysis = pa_res,
        efa              = efa_output,
        clustering       = cluster_output,
        cross_validation = crossval_output
      )))
    },

    # =======================================================================
    # METHOD 7: Save a complete thematic-analysis report to disk.
    #
    # Creates: <base_dir>/<test_name>_<YYYYMMDD_HHMMSS>/
    #   - continuous_theme_loadings.csv / ordinal_theme_loadings.csv
    #   - continuous_cluster_assignment.csv / ordinal_cluster_assignment.csv
    #   - continuous_crossvalidation.csv / ordinal_crossvalidation.csv
    #   - continuous_scree_plot.png, continuous_dendrogram.png, continuous_fa_diagram.png (+ ordinal_*)
    #   - thematic_analysis_report.txt   (full narrative report)
    #   - thematic_analysis_results.RDS  (every R object, reloadable with readRDS)
    # =======================================================================
    save_thematic_report = function(results_continuous = NULL,
                                     results_ordinal = NULL,
                                     test_name = "ThematicAnalysis",
                                     base_dir = ".") {

      if (is.null(results_continuous) && is.null(results_ordinal)) {
        stop("Provide at least one of results_continuous or results_ordinal (outputs of perform_thematic_analysis()).")
      }

      timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
      folder_name <- paste0(test_name, "_", timestamp)
      out_dir <- file.path(base_dir, folder_name)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      cat("\nSaving thematic analysis outputs to:", normalizePath(out_dir, mustWork = FALSE), "\n")

      write_block <- function(res, prefix) {
        if (is.null(res)) return(invisible(NULL))

        write.csv(res$efa$theme_assignment,
                  file.path(out_dir, paste0(prefix, "_theme_loadings.csv")), row.names = FALSE)
        write.csv(res$efa$variance_explained,
                  file.path(out_dir, paste0(prefix, "_variance_explained.csv")), row.names = FALSE)
        write.csv(res$clustering$cluster_assignment,
                  file.path(out_dir, paste0(prefix, "_cluster_assignment.csv")), row.names = FALSE)
        write.csv(res$cross_validation$comparison_table,
                  file.path(out_dir, paste0(prefix, "_crossvalidation.csv")), row.names = FALSE)

        # Scree / parallel analysis plot
        # NOTE: uses the analysis_data snapshot stored on the EFA output (the exact
        # complete-case subset used to fit the model), never self$data — this method
        # may be called with results computed from a *different* class instance than
        # the one it is invoked on (e.g. continuous scores vs. ordinal Likert items
        # typically live in two separate thematicAnalysisClass objects).
        if (!is.null(res$parallel_analysis) && !is.null(res$efa$analysis_data)) {
          png(file.path(out_dir, paste0(prefix, "_scree_plot.png")), width = 900, height = 700, res = 120)
          tryCatch({
            psych::fa.parallel(res$efa$analysis_data,
                                cor = ifelse(res$cor_type == "polychoric", "poly", "cor"),
                                fa = "fa", plot = TRUE,
                                main = paste("Parallel Analysis -", prefix))
          }, error = function(e) cat("  (scree plot skipped:", e$message, ")\n"))
          dev.off()
        }

        # Dendrogram
        png(file.path(out_dir, paste0(prefix, "_dendrogram.png")), width = 900, height = 700, res = 120)
        plot(res$clustering$hclust_obj,
             main = paste("Hierarchical Clustering -", prefix),
             xlab = "", sub = "", cex = 0.8)
        if (res$clustering$k > 1) rect.hclust(res$clustering$hclust_obj, k = res$clustering$k, border = "red")
        dev.off()

        # Factor / theme diagram (wide canvas + smaller label font so long
        # survey item names stay legible instead of overlapping)
        png(file.path(out_dir, paste0(prefix, "_theme_diagram.png")), width = 1900, height = 1100, res = 130)
        tryCatch({
          psych::fa.diagram(res$efa$fa_result, main = paste("Thematic Structure -", prefix), cex = 0.6)
        }, error = function(e) cat("  (theme diagram skipped:", e$message, ")\n"))
        dev.off()

        cat(sprintf("  [%s] theme_loadings.csv, variance_explained.csv, cluster_assignment.csv, crossvalidation.csv, scree_plot.png, dendrogram.png, theme_diagram.png\n", prefix))
      }

      write_block(results_continuous, "continuous")
      write_block(results_ordinal, "ordinal")

      # ---- Full narrative TXT report -------------------------------------------------
      txt_path <- file.path(out_dir, "thematic_analysis_report.txt")
      sink(txt_path)

      cat("================================================================================\n")
      cat("THEMATIC ANALYSIS REPORT\n")
      cat("Test name:", test_name, "\n")
      cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Data source:", self$data_source, "\n")
      cat("================================================================================\n\n")

      report_section <- function(res, label) {
        if (is.null(res)) return(invisible(NULL))
        cat("--------------------------------------------------------------------------------\n")
        cat(sprintf("SECTION: %s VARIABLES (%s correlations)\n", toupper(label), toupper(res$cor_type)))
        cat("--------------------------------------------------------------------------------\n\n")
        cat("Variables analysed:", paste(res$variables, collapse = ", "), "\n\n")

        cat("Factorability diagnostics:\n")
        cat(sprintf("  KMO = %.4f (%s) | Bartlett chi-sq = %.4f, df = %d, p = %.6f\n",
                    res$factorability$kmo$MSA,
                    private$interpret_kmo(res$factorability$kmo$MSA),
                    res$factorability$bartlett$chisq,
                    res$factorability$bartlett$df,
                    res$factorability$bartlett$p.value))
        cat("\nNumber of themes retained:", res$efa$n_factors, "\n\n")

        cat("EFA theme assignment (dominant loading per variable):\n")
        print(res$efa$theme_assignment, row.names = FALSE)
        cat("\nVariance explained per theme:\n")
        print(res$efa$variance_explained, row.names = FALSE)
        if (isTRUE(res$efa$heywood_case)) {
          cat("\nCAUTION: An ultra-Heywood case was detected in this solution (see methodological\n")
          cat("notes). Loadings and variance-explained figures above should be interpreted cautiously.\n")
        }

        cat("\nHierarchical clustering assignment (", res$clustering$k, " clusters, ward.D2 linkage):\n", sep = "")
        print(res$clustering$cluster_assignment, row.names = FALSE)

        cat("\nCross-validation of EFA themes vs. clusters:\n")
        print(res$cross_validation$comparison_table[, c("Variable", "Dominant_Theme", "Cluster_Aligned", "Agrees")], row.names = FALSE)
        cat(sprintf("\n  Percentage agreement: %.2f%%\n", res$cross_validation$percent_agreement))
        cat(sprintf("  Adjusted Rand Index:  %.4f (%s)\n\n",
                    res$cross_validation$adjusted_rand_index,
                    private$interpret_ari(res$cross_validation$adjusted_rand_index)))
      }

      report_section(results_continuous, "continuous")
      report_section(results_ordinal, "ordinal")

      cat("================================================================================\n")
      cat("METHODOLOGICAL NOTES\n")
      cat("================================================================================\n")
      cat("- Continuous variables: Pearson correlations feed the EFA and the clustering\n")
      cat("  distance metric (1 - |r|), appropriate for approximately interval-scaled data\n")
      cat("  such as CFA/BCFA factor scores.\n")
      cat("- Ordinal variables: Polychoric correlations are used instead of Pearson, since\n")
      cat("  Likert-type items are ordered categories rather than continuous measurements;\n")
      cat("  polychoric correlations estimate the correlation between the assumed latent\n")
      cat("  continuous variables underlying the observed ordinal responses (Olsson, 1979).\n")
      cat("- The number of themes/factors is chosen via parallel analysis (Horn, 1965),\n")
      cat("  which retains factors whose eigenvalues exceed those from randomly generated\n")
      cat("  data of the same dimensions, avoiding both over- and under-extraction.\n")
      cat("- Oblique (oblimin) rotation is used by default because psychological/behavioural\n")
      cat("  themes are rarely uncorrelated in reality.\n")
      cat("- The Adjusted Rand Index (Hubert & Arabie, 1985) corrects the raw agreement rate\n")
      cat("  for the agreement expected by chance alone, given the size of each theme/cluster.\n")
      cat("================================================================================\n")
      cat("END OF REPORT\n")
      cat("================================================================================\n")

      sink()

      # ---- RDS snapshot of every R object --------------------------------------------
      saveRDS(list(test_name = test_name,
                   timestamp = timestamp,
                   results_continuous = results_continuous,
                   results_ordinal = results_ordinal),
              file = file.path(out_dir, "thematic_analysis_results.RDS"))

      cat("  [report] thematic_analysis_report.txt\n")
      cat("  [snapshot] thematic_analysis_results.RDS (reload with readRDS())\n")
      cat("\nAll thematic analysis outputs saved under:\n  ", normalizePath(out_dir, mustWork = FALSE), "\n\n", sep = "")

      return(invisible(out_dir))
    },

    # =======================================================================
    # METHOD: List all public methods (kept consistent with the rest of the project)
    # =======================================================================
    list_all_methods = function() {
      all_members <- ls(self)
      methods <- all_members[sapply(all_members, function(x) is.function(self[[x]]))]
      return(sort(methods))
    }
  ),

  private = list(

    # -----------------------------------------------------------------------
    # Auto-detect (or accept user-forced) ordinal vs continuous numeric cols
    # -----------------------------------------------------------------------
    classify_numeric_columns = function(ordinal_vars, continuous_vars, unique_threshold) {

      if (!is.null(ordinal_vars) || !is.null(continuous_vars)) {
        # User-specified split (recommended when the grouping is already known,
        # e.g. raw Likert-item datasets vs. CFA factor-score datasets)
        ordinal_vars    <- intersect(ordinal_vars %||% character(0), self$numeric_cols)
        continuous_vars <- intersect(continuous_vars %||% character(0), self$numeric_cols)
        remaining <- setdiff(self$numeric_cols, c(ordinal_vars, continuous_vars))

        if (length(remaining) > 0) {
          # Auto-classify whatever wasn't explicitly assigned
          auto <- private$auto_classify(remaining, unique_threshold)
          ordinal_vars    <- c(ordinal_vars, auto$ordinal)
          continuous_vars <- c(continuous_vars, auto$continuous)
        }
        self$ordinal_cols    <- ordinal_vars
        self$continuous_cols <- continuous_vars
      } else {
        auto <- private$auto_classify(self$numeric_cols, unique_threshold)
        self$ordinal_cols    <- auto$ordinal
        self$continuous_cols <- auto$continuous
      }
    },

    auto_classify = function(cols, unique_threshold) {
      ordinal <- character(0)
      continuous <- character(0)
      for (col in cols) {
        x <- na.omit(self$data[[col]])
        if (length(x) == 0) { continuous <- c(continuous, col); next }
        is_whole_number <- all(abs(x - round(x)) < 1e-8)
        n_unique <- length(unique(x))
        if (is_whole_number && n_unique <= unique_threshold) {
          ordinal <- c(ordinal, col)
        } else {
          continuous <- c(continuous, col)
        }
      }
      list(ordinal = ordinal, continuous = continuous)
    },

    validate_columns = function(columns) {
      missing <- setdiff(columns, names(self$data))
      if (length(missing) > 0) {
        stop("Column(s) not found in data: ", paste(missing, collapse = ", "))
      }
    },

    # -----------------------------------------------------------------------
    # Correlation matrix dispatcher: Pearson (continuous) or polychoric (ordinal)
    # -----------------------------------------------------------------------
    get_correlation_matrix = function(analysis_data, cor_type) {
      if (cor_type == "polychoric") {
        poly_res <- suppressWarnings(psych::polychoric(analysis_data))
        return(poly_res$rho)
      } else {
        return(cor(analysis_data, use = "pairwise.complete.obs", method = "pearson"))
      }
    },

    # -----------------------------------------------------------------------
    # Greedy maximum-overlap alignment of cluster labels -> EFA theme labels
    # (repeatedly picks the largest remaining cell in the contingency table)
    # -----------------------------------------------------------------------
    greedy_label_alignment = function(contingency_table) {
      tbl <- as.matrix(contingency_table)
      row_labels <- rownames(tbl)   # cluster labels
      col_labels <- colnames(tbl)   # EFA theme labels

      mapping <- setNames(rep(NA_character_, length(row_labels)), row_labels)
      remaining_rows <- row_labels
      remaining_cols <- col_labels

      while (length(remaining_rows) > 0 && length(remaining_cols) > 0) {
        sub <- tbl[remaining_rows, remaining_cols, drop = FALSE]
        best <- which(sub == max(sub), arr.ind = TRUE)[1, ]
        best_row <- remaining_rows[best[1]]
        best_col <- remaining_cols[best[2]]
        mapping[best_row] <- best_col
        remaining_rows <- setdiff(remaining_rows, best_row)
        remaining_cols <- setdiff(remaining_cols, best_col)
      }
      # Any leftover clusters (more clusters than themes) keep their own label
      if (any(is.na(mapping))) {
        unmapped <- names(mapping)[is.na(mapping)]
        mapping[unmapped] <- unmapped
      }
      return(mapping)
    },

    # -----------------------------------------------------------------------
    # Adjusted Rand Index between two categorical partitions of the same items
    # (computed from first principles; no external clustering package needed)
    # -----------------------------------------------------------------------
    adjusted_rand_index = function(partition1, partition2) {
      tbl <- table(partition1, partition2)
      n <- sum(tbl)
      choose2 <- function(x) x * (x - 1) / 2

      sum_ij <- sum(choose2(tbl))
      sum_a  <- sum(choose2(rowSums(tbl)))
      sum_b  <- sum(choose2(colSums(tbl)))
      expected_index <- (sum_a * sum_b) / choose2(n)
      max_index <- 0.5 * (sum_a + sum_b)

      if (max_index == expected_index) return(0)  # degenerate case guard
      ari <- (sum_ij - expected_index) / (max_index - expected_index)
      return(ari)
    },

    interpret_kmo = function(msa) {
      if (is.na(msa)) return("Invalid")
      if (msa < 0.5) return("Unacceptable")
      if (msa < 0.6) return("Miserable")
      if (msa < 0.7) return("Mediocre")
      if (msa < 0.8) return("Middling")
      if (msa < 0.9) return("Meritorious")
      return("Marvelous")
    },

    interpret_loading = function(x) {
      ax <- abs(x)
      if (is.na(ax)) return("N/A")
      if (ax < 0.30) return("Negligible")
      if (ax < 0.40) return("Weak")
      if (ax < 0.55) return("Moderate")
      if (ax < 0.70) return("Strong")
      return("Very strong")
    },

    interpret_ari = function(ari) {
      if (is.na(ari)) return("N/A")
      if (ari < 0)    return("Worse than chance")
      if (ari < 0.20) return("Slight agreement")
      if (ari < 0.40) return("Fair agreement")
      if (ari < 0.60) return("Moderate agreement")
      if (ari < 0.80) return("Substantial agreement")
      return("Almost perfect agreement")
    }
  )
)

# Small helper used only inside this file (base R has no built-in %||%)
`%||%` <- function(a, b) if (is.null(a)) b else a
