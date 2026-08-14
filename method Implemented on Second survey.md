| # | Method | Lines | Notes |
| --- | --- | --- | --- |
| 1 | Descriptive statistics | 67 | calculate_summary()$print_summary() |
| 2 | Histograms (all numeric vars) | 74–77 | Loop over numaric_names |
| 3 | Frequency tables (categorical) | 81 | lapply(...frequency_tables, print) |
| 4 | Scatter plots by group | 96–99 | combn() + plot_scatter_by_group() — correct version |
| 5 | Simple linear regression | 127–138 | All numeric pairs; R², p-value stored in df |
| 6 | Multiple linear regression (3-var) | 165–176 | All 3-combo combinations |
| 7 | Comprehensive regression with η², ω² | 211–418 | 4-var combos; effect sizes computed at 257–263; exported at 388 |
| 8 | ANCOVA (demographics-controlled) | 637 (loop: 426–754) | perform_comprehensive_regression_analysis() |
| 9 | Independent t-tests (automated) | 860 (loop: 847–937) | Cohen's d at 893; exported to CSV+TXT at 944–960 |
| 10 | Kruskal-Wallis H test (automated) | 1240 (loop: 1211–1274) | All numeric × ≥3-group categorical combos |
| 11 | Pairwise Mann-Whitney U (post-hoc) | 1310 (loop: 1288–1346) | Bonferroni α computed at 1291; after significant KW only |
| 12 | Anderson-Darling K-sample test | 1383 (loop: 1357–1432) | Standalone verification call at 1428 |
| 13 | K-Means cluster analysis (×6 demographics) | 1719, 1732, 1745, 1758, 1772, 1785 | Elbow method auto-selects k; Gender, Occupation, Location, Age, Length, Weight |
| 14 | RSA Method 2 — first-order FO() | 1833 | Formula-based; steepest ascent, canonical |
| 15 | RSA Method 2 — second-order SO() | 1849, 1864, 1958 | 2-var and 3-var models; canonical, contour |
| 16 | RSA Method 2 — by demographic groups | 1934, 2020 | Looped over Gender, Location, Occupation |
| 17 | RSA Method 1 — string-based | 2049 | perform_response_surface_analysis() (Sensitivity ~ Th_AQ_Effect + Li_So_Effect) |
| 18 | Chi-square + Fisher's Exact + Cramér's V | 2102 (loop: 2095–2159) | Fisher fallback at 2119; all categorical pairs |
| 19 | Stratified regression | 2179 (loop: 2173–2220) | Performance ~ Sensitivity + Th_AQ_Effect + Li_So_Effect, per categorical var |
| 20 | Mediation analysis (lavaan) | 2230 | Bootstrapped CI (n_boot=1000) |
| 21 | Moderation analysis | 2245 | perform_mediation_moderation_analysis() |
| 22 | Sensitivity analysis | 2260 | Outlier impact + missing-data methods |
| 23 | Mann-Whitney U on ranked data | 2295, 2323 | Standalone + loop over all ranked vars × Gender |
| 24 | Kolmogorov-Smirnov on ranked data | 2295, 2340 | Standalone + loop |
| 25 | Goodman-Kruskal Gamma (Impl. 1) | 2374, 2458 | CI-based p-value derivation |
| 26 | Goodman-Kruskal Gamma (Impl. 2) | 2555 | Kendall τ p-value as approximation (see ⚠️ below) |
| 27 | Kendall's Tau-b | 2379, 2543 | KendallTauB() and cor.test(method="kendall") |
| 28 | Levene's test | 2673, 2693, 2709 | 3 hardcoded combinations (see ⚠️ below) |
| 29 | Ordinal logistic regression | 2758 | fit_ordinal_logistic_regression() on ranked data |