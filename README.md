# Statistical Analysis Framework — Project Guide

> A complete reference for new users: what this project does, what you need, and how to run it.

---

## Table of Contents

1. [What Is This Project?](#1-what-is-this-project)
2. [Prerequisites](#2-prerequisites)
3. [Installation & Setup](#3-installation--setup)
4. [Project Structure](#4-project-structure)
5. [Data Files](#5-data-files)
6. [How to Run the Project](#6-how-to-run-the-project)
7. [Understanding the Core Class (`regrissorClass`)](#7-understanding-the-core-class-regrissorclass)
8. [Available Analysis Scripts](#8-available-analysis-scripts)
9. [Output Files](#9-output-files)
10. [Common Issues & Tips](#10-common-issues--tips)

---

## 1. What Is This Project?

This project is an **R-based statistical analysis framework** for studying how **Indoor Environmental Quality (IEQ)** factors — such as thermal comfort, air quality, lighting, and sound — affect human **sensitivity** and **performance**.

It includes:
- A reusable object-oriented R class (`regrissorClass`) that wraps common statistical tests
- Analysis scripts organized by dataset
- Factor analysis (Exploratory and Confirmatory)
- Hypothesis testing, regression, ANOVA, clustering, and more
- Visualization and PDF/PNG report outputs

The study uses **two survey datasets** collected from participants, with demographic variables (gender, age, occupation, location) and measured responses.

---

## 2. Prerequisites

Before running anything, make sure you have the following installed on your computer.

### 2.1 Required Software

| Software | Version | Where to get it |
|---|---|---|
| **R** | 4.2 or higher | https://cran.r-project.org |
| **RStudio** | 2023.x or higher | https://posit.co/download/rstudio-desktop |

### 2.2 Required R Packages

This project uses `renv` for reproducible package management. When you restore the environment (see Section 3), all packages will be installed automatically. The key packages used throughout the project are:

| Package | Purpose |
|---|---|
| `R6` | Object-oriented classes (core framework) |
| `readxl` | Reading Excel data files |
| `dplyr`, `tidyr`, `plyr` | Data manipulation |
| `ggplot2`, `patchwork`, `ggtext` | Visualization |
| `psych` | Descriptive statistics, reliability (Cronbach's alpha) |
| `lavaan` | Confirmatory Factor Analysis (CFA) |
| `semPlot` | CFA diagram visualization |
| `car` | Levene's test, ANOVA utilities |
| `multcomp` | Post-hoc comparisons |
| `kSamples` | Kruskal-Wallis and non-parametric tests |
| `splithalf`, `ltm` | Reliability analysis |
| `rsm` | Response Surface Methodology |
| `reshape2` | Data reshaping |
| `tm`, `wordcloud` | Thematic / text analysis |

---

## 3. Installation & Setup

Follow these steps **in order** the first time you set up the project.

### Step 1 — Open the Project in RStudio

1. Open RStudio.
2. Go to **File → Open Project**.
3. Navigate to this folder and open **`analysis with sreajeth.Rproj`**.

> Always open the project this way. This ensures the working directory is set correctly and `renv` activates automatically.

### Step 2 — Restore the Package Environment

This project uses `renv` to lock package versions. On first open, run this in the RStudio Console:

```r
renv::restore()
```

This will download and install all required packages into a local `renv/library/` folder. It may take several minutes the first time.

If prompted with `"Do you want to proceed?"`, type `y` and press Enter.

### Step 3 — Verify Setup

After `renv::restore()` completes, verify R can load the core packages:

```r
library(R6)
library(readxl)
library(ggplot2)
library(psych)
library(lavaan)
```

If no errors appear, you are ready to run analyses.

---

## 4. Project Structure

```
analysis with sreajeth/
│
├── regrissorClass.r              ← Core OOP class — load this first
├── ComparativeAnalyzer.r         ← Comparative analysis wrapper class
├── perform_kw_then_pairwise_mw.r ← Kruskal-Wallis + pairwise Mann-Whitney helper
├── plottingFunctions.R           ← Custom plot utilities
│
├── main.r                        ← Main analysis workflow (start here)
├── main_BCFA.r                   ← Confirmatory Factor Analysis workflow
│
├── FirstDataSet/
│   ├── Source_DataSets_/         ← Raw & cleaned Excel/CSV data files
│   ├── discrivptiveAnalysis.R    ← Descriptive stats for Dataset 1
│   ├── Discriptive_grouped.R     ← Grouped descriptive stats
│   ├── Performance_factor Analysis.r
│   └── Reliapility_compinedDataset.r
│
├── SecondDataSet/
│   ├── Source_DataSets_/         ← Raw & cleaned Excel/CSV data files
│   ├── SRC_R_CodeSecondDataSets/ ← Analysis scripts for Dataset 2
│   ├── 2_Hypothesis_testing.r
│   └── anova.r
│
├── BothdataSets/
│   ├── Source_DataSets_/         ← Combined dataset files
│   └── SRC_R_CodeBothDataSets/   ← Analysis scripts using both datasets
│       ├── B_00_Reliapility_compinedDataset.r
│       ├── B_01_discrivptiveAnalysis.R
│       ├── B_02_ExploratoryFactorAnalysis.R
│       ├── B_03_ConfirmatoryFactorAnalysis.R
│       ├── B_04_Performance_factor Analysis.r
│       ├── B_05_anova.r
│       └── B_06_Hypothesis_testing.r
│
├── Explanatory Factor Analysis/  ← EFA output CSVs and R code snippets
├── renv/                         ← Package library (auto-managed, do not edit)
└── README.md                     ← This file
```

---

## 5. Data Files

The project works with survey data stored in Excel (`.xlsx`) and CSV formats.

| File | Location | Description |
|---|---|---|
| `First Data Set.xlsx` | `FirstDataSet/Source_DataSets_/` | Raw first survey dataset |
| `First Data Set_Cleaned.xlsx` | `FirstDataSet/Source_DataSets_/` | Cleaned first dataset |
| `SecondDataSet-Cleaned-Final-Analyzed.xlsx` | `SecondDataSet/Source_DataSets_/` | Cleaned second dataset |
| `BothDataSetsfull-Cleaned-Cleaned.csv` | `BothdataSets/Source_DataSets_/` | Combined master dataset |

> **Important:** Do not rename or move data files. The analysis scripts reference them by their existing paths relative to the project root.

---

## 6. How to Run the Project

### Option A — Run the Main Workflow (Recommended Starting Point)

Open **`main.r`** in RStudio and run it from top to bottom. It will:

1. Load the core class and helper files
2. Load four data sources
3. Create analyzer objects
4. Run descriptive statistics, regressions, t-tests, Mann-Whitney tests, Kruskal-Wallis tests, correlation analysis, response surface analysis, clustering, and thematic analysis

To run the entire file at once:

```r
source("main.r")
```

Or run it section by section using **Ctrl+Enter** (Windows/Linux) / **Cmd+Enter** (Mac) to execute one line or highlighted block at a time.

---

### Option B — Run the CFA (Confirmatory Factor Analysis) Workflow

Open and run **`main_BCFA.r`**:

```r
source("main_BCFA.r")
```

This script focuses on:
- Loading CFA-ready data
- Summary statistics and histograms
- Regression modeling
- Paired and one-sample t-tests
- Before/after comparisons using `ComparativeAnalyzer`

---

### Option C — Run Individual Analysis Scripts by Dataset

The scripts are numbered in order. Run them sequentially for each dataset:

**For the combined dataset (recommended):**

```
BothdataSets/SRC_R_CodeBothDataSets/
  B_00_Reliapility_compinedDataset.r   ← Step 1: Reliability (Cronbach's alpha)
  B_01_discrivptiveAnalysis.R          ← Step 2: Descriptive statistics
  B_02_ExploratoryFactorAnalysis.R     ← Step 3: EFA (find factor structure)
  B_03_ConfirmatoryFactorAnalysis.R    ← Step 4: CFA (confirm factor structure)
  B_04_Performance_factor Analysis.r  ← Step 5: Performance factor modeling
  B_05_anova.r                         ← Step 6: ANOVA tests
  B_06_Hypothesis_testing.r            ← Step 7: Hypothesis tests
```

Open each file in RStudio and run with `source()` or line-by-line.

---

### Option D — Write Your Own Analysis Using the Core Class

You can use `regrissorClass` interactively:

```r
# 1. Load the class definition
source("regrissorClass.r")
source("ComparativeAnalyzer.r")

# 2. Create an analyzer object from a CSV or data frame
analyzer <- regrissorClass$new("BothdataSets/Source_DataSets_/BothDataSetsfull-Cleaned-Cleaned.csv")

# 3. Explore the data
analyzer$calculate_summary()$print_summary()
analyzer$get_numeric_info()
analyzer$get_categorical_info()

# 4. Plot a histogram
analyzer$plot_histogram("Sensitivity")

# 5. Run a t-test (independent samples)
analyzer$perform_t_test("Sensitivity", "Gender", type = "independent")

# 6. Linear regression
model <- analyzer$fit_linear_regression("Performance", "Sensitivity")

# 7. Kruskal-Wallis test
analyzer$perform_kruskal_wallis("Performance", "Occupation")

# 8. Pearson correlation
analyzer$calculate_pearson_correlation(c("Sensitivity", "Performance"), plot = TRUE)

# 9. See all available methods
analyzer$list_all_methods()
```

---

## 7. Understanding the Core Class (`regrissorClass`)

The file `regrissorClass.r` defines a reusable R6 class. Think of it as a toolbox — you load your data into it, then call methods to run different analyses.

### Creating an Object

```r
source("regrissorClass.r")

# From a CSV file:
obj <- regrissorClass$new("path/to/data.csv")

# From an already-loaded data frame:
obj <- regrissorClass$new(my_dataframe)
```

### Key Methods Reference

| Method | What it does |
|---|---|
| `calculate_summary()` | Computes mean, median, SD, missing values |
| `print_summary()` | Displays the summary in the console |
| `get_numeric_info()` | Lists numeric columns and stats |
| `get_categorical_info()` | Lists categorical columns with frequency tables |
| `filter_data(column, condition)` | Subsets the data |
| `plot_histogram(column)` | Plots distribution of a column |
| `plot_scatter_by_group(x, y, group)` | Scatter plot colored by group |
| `fit_linear_regression(outcome, predictor)` | Simple linear model |
| `fit_multiple_linear_regression(outcome, predictors)` | Multiple predictors |
| `fit_logistic_regression(outcome, predictors)` | Binary outcome model |
| `perform_t_test(outcome, group, type)` | t-test (`"independent"`, `"welch"`, `"paired"`, `"one-sample"`) |
| `perform_kruskal_wallis(outcome, group)` | Non-parametric test for 3+ groups |
| `perform_mann_whitney_ks(outcome, group, type)` | Mann-Whitney U or KS test |
| `perform_levene_anderson(outcome, group, type)` | Variance or normality test |
| `calculate_pearson_correlation(variables)` | Correlation matrix with p-values |
| `perform_response_surface_analysis(response, predictors)` | RSM with 3D surface plot |
| `perform_cluster_analysis(variables, demographic, k)` | K-means clustering |
| `perform_thematic_analysis(categorical_columns)` | Word frequency / word cloud |
| `list_all_methods()` | Print all available methods |

---

## 8. Available Analysis Scripts

Below is a summary of every script and what it does.

| Script | What it does |
|---|---|
| `main.r` | Full pipeline: load data → descriptives → regression → hypothesis tests → clustering |
| `main_BCFA.r` | CFA-focused pipeline with before/after comparisons |
| `regrissorClass.r` | Core class definition (do not run directly — use `source()`) |
| `ComparativeAnalyzer.r` | Comparative analysis class for two-group comparisons |
| `perform_kw_then_pairwise_mw.r` | Kruskal-Wallis followed by pairwise Mann-Whitney tests |
| `plottingFunctions.R` | Helper plotting functions |
| `R-code_Sensitivity.r` | Sensitivity-specific analysis |
| `FirstDataSet/discrivptiveAnalysis.R` | Descriptive stats for Dataset 1 |
| `FirstDataSet/Discriptive_grouped.R` | Grouped descriptives for Dataset 1 |
| `FirstDataSet/Performance_factor Analysis.r` | Performance factor modeling, Dataset 1 |
| `FirstDataSet/Reliapility_compinedDataset.r` | Reliability analysis, Dataset 1 |
| `SecondDataSet/2_Hypothesis_testing.r` | Hypothesis tests for Dataset 2 |
| `SecondDataSet/anova.r` | ANOVA for Dataset 2 |
| `BothdataSets/SRC_R_CodeBothDataSets/B_00_...` | Reliability (both datasets) |
| `BothdataSets/SRC_R_CodeBothDataSets/B_01_...` | Descriptive stats (both datasets) |
| `BothdataSets/SRC_R_CodeBothDataSets/B_02_...` | Exploratory Factor Analysis |
| `BothdataSets/SRC_R_CodeBothDataSets/B_03_...` | Confirmatory Factor Analysis |
| `BothdataSets/SRC_R_CodeBothDataSets/B_04_...` | Performance factor analysis |
| `BothdataSets/SRC_R_CodeBothDataSets/B_05_...` | ANOVA (both datasets) |
| `BothdataSets/SRC_R_CodeBothDataSets/B_06_...` | Hypothesis testing (both datasets) |
| `BothdataSets/ResponeSurfaceAnalysis.R` | Response Surface Methodology |

---

## 9. Output Files

Running the scripts will produce:

| Output | Format | Generated by |
|---|---|---|
| Statistical results | Console / R output | All scripts |
| Histograms, scatter plots, regression plots | PNG (saved to project root) | main.r, class methods |
| CFA model diagram | PDF (`cfa_model_diagram.pdf`) | B_03 / main_BCFA.r |
| ANOVA results | PDF (`Anova_test_1_&_2_way output_results.pdf`) | ANOVA scripts |
| Factor scores | CSV (in dataset subdirectories) | EFA/CFA scripts |
| Sensitivity histogram | JPEG | Sensitivity scripts |

---

## 10. Common Issues & Tips

### "package 'X' is not installed"
Run `renv::restore()` in the Console. If a package still fails, install it manually:
```r
install.packages("package_name")
```

### "could not find file" or "cannot open connection"
You opened an R script directly without opening the `.Rproj` file first. Always start RStudio by opening `analysis with sreajeth.Rproj`. This sets the correct working directory automatically.

### "object 'regrissorClass' not found"
You need to source the class file first before using it:
```r
source("regrissorClass.r")
```

### Plots not appearing
Make sure the **Plots** pane in RStudio is visible (lower-right panel). If running `source()` on a full script, plots may not auto-display — run code interactively line-by-line instead.

### renv asks to update packages
You can safely skip updates to preserve the locked environment:
```r
renv::restore()   # restores exact locked versions, do not run renv::update()
```

### Running a specific test or section
Every script has clearly commented sections. Use **Ctrl+Shift+O** (Outline view) in RStudio to jump to sections within a file.

---

## Quick-Start Checklist

- [ ] R 4.2+ installed
- [ ] RStudio installed
- [ ] Project opened via `analysis with sreajeth.Rproj`
- [ ] `renv::restore()` run successfully in the Console
- [ ] `main.r` opened and ready to run

Once all boxes are checked, press **Ctrl+A** in `main.r` to select all, then **Ctrl+Enter** to run the full analysis.

---

*Last updated: May 2026*
