# 06_summary-stats.R
# Summary statistics and verification of key findings
# Produces manuscript-ready tables and interpretation text

library(dplyr)
library(tidyr)

source("R/utils_labels.R")

cat("=== SUMMARY STATISTICS & VERIFICATION ===\n")

# Outputs go to figures/supplemental/ — make sure it exists on a fresh clone
if (!dir.exists("figures/supplemental")) dir.create("figures/supplemental", recursive = TRUE)

# Read all analysis results
trend_analysis <- read.csv("data/harmonized/trend_analysis_results.csv",
                           stringsAsFactors = FALSE)
detection_data <- read.csv("data/harmonized/detection_time_analysis.csv",
                           stringsAsFactors = FALSE)
metadata <- get_site_metadata()

trend_clean <- trend_analysis %>%
  filter(trend_class != "insufficient_data")

# --- Verify Claimed Statistics ---

cat("\n--- VERIFICATION OF PRELIMINARY FINDINGS ---\n\n")

# 1. Non-directional vs directional
overall_props <- trend_clean %>%
  count(trend_class) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Percentage for one class; 0 if the class is absent (a bare subset would
# return numeric(0) and crash sprintf)
pct_of <- function(cls) {
  v <- overall_props$pct[overall_props$trend_class %in% cls]
  if (length(v) == 0) 0 else sum(v)
}

non_dir <- pct_of(c("stable", "variable"))
dir_pct <- pct_of(c("increasing", "decreasing"))

cat(sprintf("Non-directional responses: %.1f%%\n", non_dir))
cat(sprintf("  - Stable (CV < 0.3): %.1f%%\n", pct_of("stable")))
cat(sprintf("  - Variable (CV >= 0.3): %.1f%%\n", pct_of("variable")))
cat(sprintf("Directional responses: %.1f%%\n", dir_pct))
cat(sprintf("  - Increasing: %.1f%%\n", pct_of("increasing")))
cat(sprintf("  - Decreasing: %.1f%%\n", pct_of("decreasing")))

# 2. Detection timepoints
sig_trends <- detection_data %>%
  filter(trend_class %in% c("increasing", "decreasing"), detected == TRUE)

if (nrow(sig_trends) > 0) {
  cat(sprintf("\nMedian timepoints for detection: %.0f\n",
              median(sig_trends$min_n_for_detection, na.rm = TRUE)))
  cat(sprintf("Range: %d to %d\n",
              min(sig_trends$min_n_for_detection, na.rm = TRUE),
              max(sig_trends$min_n_for_detection, na.rm = TRUE)))
}

# 3. Mean ratios by trend type
# NOTE: medians computed BEFORE means — summarise() evaluates sequentially, so
# once mean_ratio is reassigned to the group mean, a later median(mean_ratio)
# would just repeat the mean (this bug shipped in an earlier Table version)
trend_stats <- trend_clean %>%
  group_by(trend_class) %>%
  summarise(
    n = n(),
    median_ratio = median(mean_ratio, na.rm = TRUE),
    median_cv = median(cv, na.rm = TRUE),
    mean_ratio = mean(mean_ratio, na.rm = TRUE),
    mean_cv = mean(cv, na.rm = TRUE),
    mean_timepoints = mean(n_timepoints, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMean treatment/control ratios by trend type:\n")
for (i in seq_len(nrow(trend_stats))) {
  cat(sprintf("  %s: %.2fx control\n",
              trend_stats$trend_class[i], trend_stats$mean_ratio[i]))
}

# --- Statistical Tests ---

cat("\n--- STATISTICAL TESTS ---\n")

# Kruskal-Wallis: CV by trend type
kw_cv <- kruskal.test(cv ~ trend_class, data = trend_clean)
cat(sprintf("\nKruskal-Wallis test (CV ~ trend type): H = %.2f, p = %.4f\n",
            kw_cv$statistic, kw_cv$p.value))

# Kruskal-Wallis: mean ratio by trend type
kw_ratio <- kruskal.test(mean_ratio ~ trend_class, data = trend_clean)
cat(sprintf("Kruskal-Wallis test (ratio ~ trend type): H = %.2f, p = %.4f\n",
            kw_ratio$statistic, kw_ratio$p.value))

# --- Temporal Frequency Documentation ---

cat("\n--- TEMPORAL FREQUENCY BY DATASET ---\n")

harmonized <- read.csv("data/harmonized/harmonized_current.csv", stringsAsFactors = FALSE)
source("R/utils_dates.R")

freq_summary <- harmonized %>%
  mutate(year = extract_year(Date)) %>%
  group_by(source) %>%
  summarise(
    site_abbr = first(site_abbr),
    n_rows = n(),
    n_years = n_distinct(year, na.rm = TRUE),
    n_dates = n_distinct(Date),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    temporal_freq = ifelse(n_dates > n_years * 1.5, "Sub-annual", "Annual"),
    .groups = "drop"
  )

cat("\n")
print(as.data.frame(freq_summary %>%
                      select(site_abbr, source, year_min, year_max, n_years, temporal_freq) %>%
                      arrange(site_abbr)))

# --- Manuscript Table ---

cat("\n--- MANUSCRIPT TABLE ---\n")

manuscript_table <- trend_stats %>%
  mutate(
    trend_class = factor(trend_class, levels = c("stable", "variable", "increasing", "decreasing")),
    `Mean (Median) Ratio` = sprintf("%.2f (%.2f)", mean_ratio, median_ratio),
    `Mean (Median) CV` = sprintf("%.3f (%.3f)", mean_cv, median_cv),
    `Mean Timepoints` = sprintf("%.1f", mean_timepoints)
  ) %>%
  arrange(trend_class) %>%
  select(`Trend Type` = trend_class, N = n,
         `Mean (Median) Ratio`, `Mean (Median) CV`, `Mean Timepoints`)

print(as.data.frame(manuscript_table))
# Diagnostic/verification table — lives in supplemental/ so it does not sit
# beside (and disagree with) the numbered manuscript tables from 08
write.csv(manuscript_table, "figures/supplemental/Table_trend_summary.csv", row.names = FALSE)

# --- Supplemental: per-experiment statistics ---

experiment_stats <- trend_analysis %>%
  left_join(metadata %>% select(source, site_full_name),
            by = "source") %>%
  select(Site = site_abbr, `Site Name` = site_full_name, `Ecosystem` = site_type,
         `Experiment` = experiment_type, Treatment,
         `Trend` = trend_class, Slope = slope, `P-value` = p_value,
         `R-squared` = r_squared, CV = cv, `Mean Ratio` = mean_ratio,
         `N Timepoints` = n_timepoints) %>%
  arrange(Site, Treatment)

write.csv(experiment_stats, "figures/supplemental/Table_experiment_statistics.csv",
          row.names = FALSE)

# --- Summary Report ---

report_path <- "figures/supplemental/analysis_summary_report.txt"
sink(report_path)

cat("TEMPORAL TREND ANALYSIS SUMMARY REPORT\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

cat("DATASET OVERVIEW:\n")
cat("  Total sources:", length(unique(harmonized$source)), "\n")
cat("  Total rows:", nrow(harmonized), "\n")
cat("  Sites:", length(unique(harmonized$site_abbr)), "\n\n")

cat("TREND DISTRIBUTION:\n")
for (i in seq_len(nrow(overall_props))) {
  cat(sprintf("  %s: %d (%.1f%%)\n", overall_props$trend_class[i],
              overall_props$n[i], overall_props$pct[i]))
}

cat(sprintf("\nKEY FINDINGS:\n"))
cat(sprintf("  - %.0f%% of treatments show non-directional responses\n", non_dir))
cat(sprintf("  - %.0f%% show significant directional change\n", dir_pct))
if (nrow(sig_trends) > 0) {
  cat(sprintf("  - Directional trends require ~%.0f timepoints (median) to detect\n",
              median(sig_trends$min_n_for_detection, na.rm = TRUE)))
}

cat("\nTREND TYPE CHARACTERISTICS:\n")
for (i in seq_len(nrow(trend_stats))) {
  cat(sprintf("  %s (n=%d): mean ratio=%.2fx, mean CV=%.3f, mean timepoints=%.0f\n",
              trend_stats$trend_class[i], trend_stats$n[i],
              trend_stats$mean_ratio[i], trend_stats$mean_cv[i],
              trend_stats$mean_timepoints[i]))
}

cat(paste("\n", rep("=", 70), collapse = ""), "\n")

sink()

cat("\nSummary report written to:", report_path, "\n")
cat("=== COMPLETE ===\n")
