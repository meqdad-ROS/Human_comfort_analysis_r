# ==============================================================================
# thematic_analysis_main.r
#
# Written by Meqdad Hasan
#
# Thematic analysis of the IEQ (Indoor Environmental Quality) survey data,
# run separately on:
#   (A) the CONTINUOUS CFA/BCFA factor-score dataset
#         SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv
#   (B) the ORDINAL raw Likert-item dataset (1-5 scale)
#         SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx
#
# This script plugs directly into the existing project structure used by
# main_SecondSurvey.r (same relative paths, same library set) and adds one
# new analytical component: thematicAnalysisClass (see thematicAnalysisClass.r).
#
# Output: a single, fully self-contained, timestamped results folder
# containing CSV tables, PNG figures, a narrative TXT report and an RDS
# snapshot of every R object produced -- see save_thematic_report() below.
# ==============================================================================
cat("\014")                       # clear console
while (dev.cur() > 1) dev.off()   # clear all plotting devices
# rm(list = ls())                 # uncomment to clear environment variables too

#-------------------------------------------------------------------------------
# STEP 0: Load necessary libraries
#-------------------------------------------------------------------------------
library(readxl)
library(psych)
library(GPArotation)   # required by psych::fa() for oblique rotation (oblimin)
library(R6)

#-------------------------------------------------------------------------------
# STEP 00: source required classes
#-------------------------------------------------------------------------------
source("regrissorClass.r")
source("cfTestClass.r")
source("thematicAnalysisClass.r")
cat("\014")

#===============================================================================
# STEP 1: LOAD DATA (same paths/convention as main_SecondSurvey.r)
#===============================================================================

# Continuous data: CFA/BCFA factor scores (Second dataset)
data_SCFs <- read.csv("SecondDataSet/Result of analysis/Grouped_cfa_SECOND_factors_outout_data.csv", header = TRUE)

# Ordinal data: cleaned raw Likert-item responses (Second dataset)
data_SOrRank <- as.data.frame(read_excel("SecondDataSet/Source_DataSets_/SecondDataSet-Cleaned-Final-Analyzed.xlsx"))

#===============================================================================
# STEP 2: CREATE thematicAnalysisClass INSTANCES
#===============================================================================

clean_data_SCFs <- na.omit(data_SCFs)

# (A) Continuous factor scores -> auto-detected as "continuous"
thematic_continuous <- thematicAnalysisClass$new(input = clean_data_SCFs)

# (B) Raw Likert items -> auto-detected as "ordinal" (integer 1-5, <=10 levels)
#     unique_threshold can be widened/narrowed if your instrument uses a
#     different response-scale length (e.g. 7-point Likert -> threshold = 10 is fine;
#     a 0-100 continuous slider item would need continuous_vars specified explicitly).
thematic_ordinal <- thematicAnalysisClass$new(input = data_SOrRank, unique_threshold = 10)

cat("\nAuto-detected CONTINUOUS variables:\n")
print(thematic_continuous$continuous_cols)
cat("\nAuto-detected ORDINAL variables:\n")
print(thematic_ordinal$ordinal_cols)

#===============================================================================
# STEP 3: THEMATIC ANALYSIS -- CONTINUOUS DATA
#         Pearson-correlation EFA + correlation-distance hierarchical
#         clustering, cross-validated against one another.
#===============================================================================

results_continuous <- thematic_continuous$perform_thematic_analysis(
  variables      = thematic_continuous$continuous_cols,
  cor_type       = "pearson",
  rotation       = "oblimin",
  fm             = "minres",
  cluster_method = "ward.D2",
  plot           = TRUE
)

#===============================================================================
# STEP 4: THEMATIC ANALYSIS -- ORDINAL DATA
#         Polychoric-correlation EFA + correlation-distance hierarchical
#         clustering, cross-validated against one another.
#===============================================================================

results_ordinal <- thematic_ordinal$perform_thematic_analysis(
  variables      = thematic_ordinal$ordinal_cols,
  cor_type       = "polychoric",
  rotation       = "oblimin",
  fm             = "minres",
  cluster_method = "ward.D2",
  plot           = TRUE
)

#===============================================================================
# STEP 5: SAVE EVERYTHING -- creates <test_name>_<YYYYMMDD_HHMMSS>/ containing
#         CSV tables, PNG figures (scree plot, dendrogram, theme diagram),
#         a full narrative TXT report and an RDS snapshot of every object.
#===============================================================================

# Change test_name to whatever label you want this run archived under --
# the folder that gets created is: <test_name>_<YYYYMMDD_HHMMSS>
out_dir <- thematic_continuous$save_thematic_report(
  results_continuous = results_continuous,
  results_ordinal    = results_ordinal,
  test_name          = "IEQ_Thematic_Analysis",
  base_dir           = "."
)

cat("\n\nThematic analysis complete. All outputs saved under:\n  ", out_dir, "\n", sep = "")

# To reload every object from a previous run later, e.g. for further
# inspection or re-plotting without re-running the analysis:
#   saved <- readRDS(file.path(out_dir, "thematic_analysis_results.RDS"))
#   saved$results_continuous$efa$fa_result
#   saved$results_ordinal$cross_validation$comparison_table
