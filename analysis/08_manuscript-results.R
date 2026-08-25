# 08_manuscript-results.R
# Generate all numeric outputs needed for manuscript text, tables, and figures
# Reads from existing harmonized CSVs produced by scripts 01-07
# Outputs: manuscript_results.txt (console-style report) + CSV tables

library(dplyr)
library(tidyr)

source("R/utils_labels.R")
source("R/utils_dates.R")

cat("=== GENERATING MANUSCRIPT RESULTS ===\n\n")

# =============================================================================
# LOAD ALL DATA
# =============================================================================

harmonized     <- read.csv("data/harmonized/harmonized_current.csv", stringsAsFactors = FALSE)
trend_results  <- read.csv("data/harmonized/trend_analysis_results.csv", stringsAsFactors = FALSE)
detection_data <- read.csv("data/harmonized/detection_time_analysis.csv", stringsAsFactors = FALSE)
early_full     <- read.csv("data/harmonized/early_vs_full_lrr.csv", stringsAsFactors = FALSE)
sizer_results  <- read.csv("data/harmonized/sizer_analysis_results.csv", stringsAsFactors = FALSE)
sign_flips     <- read.csv("data/harmonized/sign_flip_analysis.csv", stringsAsFactors = FALSE)
metadata       <- get_site_metadata()
registry       <- read.csv("data/dataset_registry.csv", stringsAsFactors = FALSE)

# Filter to analyzable cases (>= 3 timepoints)
trend_clean <- trend_results %>%
  filter(!is.na(trend_class), trend_class != "insufficient_data")

# Included datasets only
registry_included <- registry %>% filter(include == TRUE)

# Open report file
report_path <- "figures/manuscript_results.txt"
sink(report_path, split = TRUE)

cat("MANUSCRIPT QUANTITATIVE RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# =============================================================================
# SECTION 3.1: DATASET OVERVIEW
# =============================================================================

cat("=== SECTION 3.1: DATASET OVERVIEW ===\n\n")

# Count unique sites, ecosystem types, experiment types
n_sites <- length(unique(trend_clean$site_abbr))
n_ecosystems <- length(unique(trend_clean$site_type))
n_experiment_types <- length(unique(trend_clean$experiment_type))
n_treatments <- nrow(trend_clean)

cat(sprintf("Number of LTER sites: %d\n", n_sites))
cat(sprintf("Number of ecosystem types: %d\n", n_ecosystems))
cat(sprintf("Number of experiment types: %d\n", n_experiment_types))
cat(sprintf("Total site-treatment combinations (analyzable, >= 3 timepoints): %d\n", n_treatments))

# Year span range
year_spans <- trend_clean$year_span
cat(sprintf("Year span range: %.0f to %.0f years\n",
            min(year_spans, na.rm = TRUE), max(year_spans, na.rm = TRUE)))
cat(sprintf("Mean year span: %.1f years\n", mean(year_spans, na.rm = TRUE)))
cat(sprintf("Median year span: %.1f years\n", median(year_spans, na.rm = TRUE)))

# Temporal frequency from raw data
freq_by_source <- harmonized %>%
  mutate(year = extract_year(Date)) %>%
  group_by(source) %>%
  summarise(
    site_abbr = first(site_abbr),
    n_rows = n(),
    n_years = n_distinct(year, na.rm = TRUE),
    n_dates = n_distinct(Date),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    temporal_freq = ifelse(n_dates > n_years * 1.5, "Sub-annual", "Annual"),
    year_range = paste0(year_min, "-", year_max)
  )

# Count only the analyzable sources so this line matches the site/treatment
# numbers reported above (freq_by_source spans everything in harmonized)
freq_analyzable <- freq_by_source %>%
  filter(source %in% unique(trend_clean$source))
n_annual <- sum(freq_analyzable$temporal_freq == "Annual")
n_subannual <- sum(freq_analyzable$temporal_freq == "Sub-annual")
cat(sprintf("Sampling frequency: %d annual, %d sub-annual datasets\n", n_annual, n_subannual))

# Ecosystem type counts
eco_counts <- trend_clean %>%
  distinct(site_abbr, site_type) %>%
  count(site_type, name = "n_sites") %>%
  arrange(desc(n_sites))
cat("\nSites by ecosystem type:\n")
for (i in seq_len(nrow(eco_counts))) {
  cat(sprintf("  %s: %d sites\n", eco_counts$site_type[i], eco_counts$n_sites[i]))
}

# Experiment type counts
exp_counts <- trend_clean %>%
  distinct(site_abbr, experiment_type) %>%
  count(experiment_type, name = "n_sites") %>%
  arrange(desc(n_sites))
cat("\nSites by experiment type:\n")
for (i in seq_len(nrow(exp_counts))) {
  cat(sprintf("  %s: %d sites\n", exp_counts$experiment_type[i], exp_counts$n_sites[i]))
}

# Stock vs flux
sf_counts <- trend_clean %>%
  distinct(site_abbr, stock_or_flux) %>%
  count(stock_or_flux, name = "n_sites") %>%
  arrange(desc(n_sites))
cat("\nSites by response type:\n")
for (i in seq_len(nrow(sf_counts))) {
  cat(sprintf("  %s: %d sites\n", sf_counts$stock_or_flux[i], sf_counts$n_sites[i]))
}

# --- TABLE 1: Case study overview ---
cat("\n--- TABLE 1: Case Study Overview ---\n")

table1 <- freq_by_source %>%
  left_join(metadata %>% distinct(source, site_full_name, site_type,
                                   experiment_type, stock_or_flux),
            by = "source") %>%
  left_join(registry_included %>% select(ready_filename, edi_package_id),
            by = c("source" = "ready_filename")) %>%
  filter(source %in% unique(trend_clean$source)) %>%
  select(
    Site = site_abbr,
    `Site Name` = site_full_name,
    Ecosystem = site_type,
    `Experiment Type` = experiment_type,
    Response = stock_or_flux,
    `EDI Package` = edi_package_id,
    `Year Range` = year_range,
    `N Years Sampled` = n_years,
    `Sampling Freq` = temporal_freq
  ) %>%
  arrange(Ecosystem, Site)

print(as.data.frame(table1))
write.csv(table1, "figures/Table1_case_study_overview.csv", row.names = FALSE)
cat("  -> Written to figures/Table1_case_study_overview.csv\n")

# =============================================================================
# SECTION 3.2: TREND CLASSIFICATION
# =============================================================================

cat("\n\n=== SECTION 3.2: TREND CLASSIFICATION ===\n\n")

# Overall proportions
trend_props <- trend_clean %>%
  count(trend_class) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

non_dir_n <- sum(trend_props$n[trend_props$trend_class %in% c("stable", "variable")])
non_dir_pct <- round(non_dir_n / nrow(trend_clean) * 100, 1)
dir_n <- sum(trend_props$n[trend_props$trend_class %in% c("increasing", "decreasing")])
dir_pct <- round(dir_n / nrow(trend_clean) * 100, 1)

cat(sprintf("Total analyzable treatment combinations: %d\n", nrow(trend_clean)))
cat(sprintf("Non-directional: %d (%.1f%%)\n", non_dir_n, non_dir_pct))
cat(sprintf("Directional: %d (%.1f%%)\n", dir_n, dir_pct))
cat("\nBreakdown:\n")
for (i in seq_len(nrow(trend_props))) {
  cat(sprintf("  %s: %d (%.1f%%)\n",
              trend_props$trend_class[i], trend_props$n[i], trend_props$pct[i]))
}

# Stats by trend class
trend_class_stats <- trend_clean %>%
  group_by(trend_class) %>%
  summarise(
    n = n(),
    ratio_mean = round(mean(mean_ratio, na.rm = TRUE), 2),
    ratio_median = round(median(mean_ratio, na.rm = TRUE), 2),
    ratio_sd = round(sd(mean_ratio, na.rm = TRUE), 2),
    cv_mean = round(mean(cv, na.rm = TRUE), 3),
    cv_median = round(median(cv, na.rm = TRUE), 3),
    year_span_mean = round(mean(year_span, na.rm = TRUE), 1),
    year_span_median = round(median(year_span, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\nTrend class statistics:\n")
for (i in seq_len(nrow(trend_class_stats))) {
  s <- trend_class_stats[i, ]
  cat(sprintf("  %s (n=%d):\n", s$trend_class, s$n))
  cat(sprintf("    Mean ratio: %.2f (median %.2f, SD %.2f)\n",
              s$ratio_mean, s$ratio_median, s$ratio_sd))
  cat(sprintf("    Mean CV: %.3f (median %.3f)\n", s$cv_mean, s$cv_median))
  cat(sprintf("    Mean year span: %.1f (median %.1f)\n",
              s$year_span_mean, s$year_span_median))
}

# Directional trends specifically
directional <- trend_clean %>%
  filter(trend_class %in% c("increasing", "decreasing"))

cat(sprintf("\nDirectional trends (n=%d):\n", nrow(directional)))
cat(sprintf("  Year span range: %.0f to %.0f years\n",
            min(directional$year_span, na.rm = TRUE),
            max(directional$year_span, na.rm = TRUE)))
cat(sprintf("  Mean year span: %.1f years\n",
            mean(directional$year_span, na.rm = TRUE)))

# How many sites have at least one directional trend?
sites_with_dir <- directional %>%
  distinct(site_abbr) %>%
  nrow()
cat(sprintf("  Sites with >= 1 directional trend: %d / %d\n", sites_with_dir, n_sites))

# --- TABLE 2: Trend type summary ---
cat("\n--- TABLE 2: Trend Type Summary ---\n")

table2 <- trend_class_stats %>%
  mutate(
    trend_class = factor(trend_class,
                         levels = c("stable", "variable", "increasing", "decreasing")),
    `Mean Ratio (SD)` = sprintf("%.2f (%.2f)", ratio_mean, ratio_sd),
    `Median Ratio` = sprintf("%.2f", ratio_median),
    `Mean CV` = sprintf("%.3f", cv_mean),
    `Median CV` = sprintf("%.3f", cv_median),
    `Mean Duration (yr)` = sprintf("%.1f", year_span_mean),
    `Median Duration (yr)` = sprintf("%.1f", year_span_median)
  ) %>%
  arrange(trend_class) %>%
  select(
    `Trend Type` = trend_class,
    N = n,
    `Mean Ratio (SD)`,
    `Median Ratio`,
    `Mean CV`,
    `Median CV`,
    `Mean Duration (yr)`,
    `Median Duration (yr)`
  )

print(as.data.frame(table2))
write.csv(table2, "figures/Table2_trend_type_summary.csv", row.names = FALSE)
cat("  -> Written to figures/Table2_trend_type_summary.csv\n")

# =============================================================================
# SECTION 3.3: EARLY VS FULL LRR COMPARISON
# =============================================================================

cat("\n\n=== SECTION 3.3: EARLY VS FULL LRR COMPARISON ===\n\n")

# Classify each case
early_full_classified <- early_full %>%
  mutate(
    # Classification based on LRR direction and magnitude change
    same_sign = sign(early_lrr) == sign(full_lrr),
    abs_change = abs(lrr_change),
    # Categories: accurate, underestimate, overestimate, wrong direction
    classification = case_when(
      # Wrong direction: early and full LRR have different signs
      # (only when both are meaningfully different from zero)
      !same_sign & abs(early_lrr) > 0.1 & abs(full_lrr) > 0.1 ~ "wrong_direction",
      # Accurate: change is small relative to full LRR magnitude
      abs_change < 0.1 ~ "accurate",
      # Underestimate: early LRR magnitude < full LRR magnitude (same sign)
      same_sign & abs(early_lrr) < abs(full_lrr) ~ "underestimate",
      # Overestimate: early LRR magnitude > full LRR magnitude (same sign)
      same_sign & abs(early_lrr) > abs(full_lrr) ~ "overestimate",
      # Default for edge cases
      TRUE ~ "accurate"
    )
  )

class_counts <- early_full_classified %>%
  count(classification) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat(sprintf("Total treatment combinations with early vs full LRR: %d\n", nrow(early_full_classified)))
cat("\nClassification:\n")
for (i in seq_len(nrow(class_counts))) {
  cat(sprintf("  %s: %d (%.1f%%)\n",
              class_counts$classification[i], class_counts$n[i], class_counts$pct[i]))
}

# Summary statistics
cat(sprintf("\nMean absolute LRR change: %.3f\n",
            mean(abs(early_full_classified$lrr_change), na.rm = TRUE)))
cat(sprintf("Median absolute LRR change: %.3f\n",
            median(abs(early_full_classified$lrr_change), na.rm = TRUE)))

# Correlation between early and full LRR
cor_test <- cor.test(early_full_classified$early_lrr, early_full_classified$full_lrr)
cat(sprintf("Pearson correlation (early vs full LRR): r = %.3f, p = %.2e\n",
            cor_test$estimate, cor_test$p.value))

# Cases where early LRR was negative but full was positive (or vice versa)
wrong_dir <- early_full_classified %>%
  filter(classification == "wrong_direction")
cat(sprintf("\nWrong direction cases: %d\n", nrow(wrong_dir)))
if (nrow(wrong_dir) > 0) {
  for (i in seq_len(nrow(wrong_dir))) {
    cat(sprintf("  %s / %s: early=%.3f, full=%.3f\n",
                wrong_dir$site_abbr[i], wrong_dir$Treatment[i],
                wrong_dir$early_lrr[i], wrong_dir$full_lrr[i]))
  }
}

# Illustrative cases
cat("\nIllustrative cases:\n")
illustrative_sites <- c("KNZ", "KBS", "HFR", "VCR", "HBR")
for (site in illustrative_sites) {
  site_data <- early_full_classified %>% filter(site_abbr == site)
  if (nrow(site_data) > 0) {
    cat(sprintf("\n  %s (%d treatments):\n", site, nrow(site_data)))
    for (j in seq_len(nrow(site_data))) {
      cat(sprintf("    %s: early=%.3f, full=%.3f, change=%.3f (%s)\n",
                  site_data$Treatment[j],
                  site_data$early_lrr[j], site_data$full_lrr[j],
                  site_data$lrr_change[j], site_data$classification[j]))
    }
  }
}

# --- TABLE 3: Early vs Full LRR ---
cat("\n--- TABLE 3: Early vs Full LRR Summary ---\n")

table3 <- early_full_classified %>%
  select(Site = site_abbr, Treatment, `Early LRR` = early_lrr,
         `Full LRR` = full_lrr, `LRR Change` = lrr_change,
         `N Early` = n_early, `N Total` = n_total, `N Years` = n_years,
         Classification = classification) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  arrange(Site, Treatment)

write.csv(table3, "figures/Table3_early_vs_full_lrr.csv", row.names = FALSE)
cat("  -> Written to figures/Table3_early_vs_full_lrr.csv\n")

# =============================================================================
# SECTION 3.4: SIZER RESULTS (SLOPE CHANGES)
# =============================================================================

cat("\n\n=== SECTION 3.4: SIZER RESULTS ===\n\n")

n_sizer_total <- nrow(sizer_results)
has_slope_change <- sizer_results %>% filter(n_slope_changes > 0)
n_with_changes <- nrow(has_slope_change)
pct_with_changes <- round(n_with_changes / n_sizer_total * 100, 1)

cat(sprintf("Total treatment combinations analyzed by SiZer: %d\n", n_sizer_total))
cat(sprintf("With >= 1 slope change: %d (%.1f%%)\n", n_with_changes, pct_with_changes))
cat(sprintf("Without slope changes: %d (%.1f%%)\n",
            n_sizer_total - n_with_changes, 100 - pct_with_changes))

# Distribution of number of slope changes
change_dist <- sizer_results %>%
  count(n_slope_changes) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
cat("\nDistribution of slope changes:\n")
for (i in seq_len(nrow(change_dist))) {
  cat(sprintf("  %d changes: %d (%.1f%%)\n",
              change_dist$n_slope_changes[i], change_dist$n[i], change_dist$pct[i]))
}

# Mean slope changes among those with changes
if (n_with_changes > 0) {
  cat(sprintf("\nAmong those with changes:\n"))
  cat(sprintf("  Mean slope changes: %.1f\n",
              mean(has_slope_change$n_slope_changes)))
  cat(sprintf("  Max slope changes: %d\n",
              max(has_slope_change$n_slope_changes)))
}

# By site
sizer_by_site <- sizer_results %>%
  group_by(site_abbr) %>%
  summarise(
    n_treatments = n(),
    n_with_changes = sum(n_slope_changes > 0),
    pct_with_changes = round(n_with_changes / n_treatments * 100, 1),
    mean_changes = round(mean(n_slope_changes), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_with_changes))

cat("\nSiZer results by site:\n")
for (i in seq_len(nrow(sizer_by_site))) {
  s <- sizer_by_site[i, ]
  cat(sprintf("  %s: %d/%d with changes (%.0f%%), mean=%.1f\n",
              s$site_abbr, s$n_with_changes, s$n_treatments,
              s$pct_with_changes, s$mean_changes))
}

# =============================================================================
# SECTION 3.5: DETECTION TIME
# =============================================================================

cat("\n\n=== SECTION 3.5: DETECTION TIME ===\n\n")

# Focus on directional trends that were detected
detected_directional <- detection_data %>%
  filter(trend_class %in% c("increasing", "decreasing"),
         detected == TRUE,
         !is.na(time_to_detect))

cat(sprintf("Directional trends with detection data: %d\n", nrow(detected_directional)))

if (nrow(detected_directional) > 0) {
  cat(sprintf("Median years to detect: %.1f\n",
              median(detected_directional$time_to_detect, na.rm = TRUE)))
  cat(sprintf("Mean years to detect: %.1f\n",
              mean(detected_directional$time_to_detect, na.rm = TRUE)))
  cat(sprintf("Range: %.1f to %.1f years\n",
              min(detected_directional$time_to_detect, na.rm = TRUE),
              max(detected_directional$time_to_detect, na.rm = TRUE)))
  cat(sprintf("Median timepoints for detection: %g\n",
              median(detected_directional$min_n_for_detection, na.rm = TRUE)))
}

# Filter to annually-sampled sites for fair comparison
annual_sources <- freq_by_source %>%
  filter(temporal_freq == "Annual") %>%
  pull(source)

detected_annual <- detected_directional %>%
  filter(source %in% annual_sources)

cat(sprintf("\nAnnually-sampled directional detections: %d\n", nrow(detected_annual)))
if (nrow(detected_annual) > 0) {
  cat(sprintf("  Median years to detect: %.1f\n",
              median(detected_annual$time_to_detect, na.rm = TRUE)))
  cat(sprintf("  Range: %.1f to %.1f years\n",
              min(detected_annual$time_to_detect, na.rm = TRUE),
              max(detected_annual$time_to_detect, na.rm = TRUE)))
}

# Case-by-case listing (all detected directional cases, not just annual)
cat("\nDetection times for all directional cases:\n")
detection_detail <- detected_directional %>%
  arrange(site_abbr, Treatment) %>%
  select(site_abbr, Treatment, trend_class, time_to_detect, min_n_for_detection)

for (i in seq_len(nrow(detection_detail))) {
  d <- detection_detail[i, ]
  cat(sprintf("  %s / %s (%s): %.1f years (%d timepoints)\n",
              d$site_abbr, d$Treatment, d$trend_class,
              d$time_to_detect, d$min_n_for_detection))
}

# By trend type
cat("\nDetection time by trend type:\n")
detection_by_trend <- detected_directional %>%
  group_by(trend_class) %>%
  summarise(
    n = n(),
    median_years = round(median(time_to_detect, na.rm = TRUE), 1),
    mean_years = round(mean(time_to_detect, na.rm = TRUE), 1),
    min_years = round(min(time_to_detect, na.rm = TRUE), 1),
    max_years = round(max(time_to_detect, na.rm = TRUE), 1),
    .groups = "drop"
  )
for (i in seq_len(nrow(detection_by_trend))) {
  d <- detection_by_trend[i, ]
  cat(sprintf("  %s (n=%d): median=%.1f, mean=%.1f, range=%.1f-%.1f years\n",
              d$trend_class, d$n, d$median_years, d$mean_years,
              d$min_years, d$max_years))
}

# =============================================================================
# SECTION 3.6: PATTERNS BY ECOSYSTEM TYPE AND STOCK/FLUX
# =============================================================================

cat("\n\n=== SECTION 3.6: PATTERNS BY ECOSYSTEM TYPE & STOCK/FLUX ===\n\n")

# Trend distribution by ecosystem type
cat("--- Trends by Ecosystem Type ---\n")
eco_trends <- trend_clean %>%
  group_by(site_type) %>%
  summarise(
    n = n(),
    n_stable = sum(trend_class == "stable"),
    n_variable = sum(trend_class == "variable"),
    n_increasing = sum(trend_class == "increasing"),
    n_decreasing = sum(trend_class == "decreasing"),
    pct_directional = round(sum(trend_class %in% c("increasing", "decreasing")) / n() * 100, 1),
    mean_ratio = round(mean(mean_ratio, na.rm = TRUE), 2),
    mean_cv = round(mean(cv, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_directional))

for (i in seq_len(nrow(eco_trends))) {
  e <- eco_trends[i, ]
  cat(sprintf("\n  %s (n=%d):\n", e$site_type, e$n))
  cat(sprintf("    Stable: %d, Variable: %d, Increasing: %d, Decreasing: %d\n",
              e$n_stable, e$n_variable, e$n_increasing, e$n_decreasing))
  cat(sprintf("    Directional: %.1f%%\n", e$pct_directional))
  cat(sprintf("    Mean ratio: %.2f, Mean CV: %.3f\n", e$mean_ratio, e$mean_cv))
}

# Trend distribution by stock/flux
cat("\n--- Trends by Stock vs Flux ---\n")
sf_trends <- trend_clean %>%
  group_by(stock_or_flux) %>%
  summarise(
    n = n(),
    n_stable = sum(trend_class == "stable"),
    n_variable = sum(trend_class == "variable"),
    n_increasing = sum(trend_class == "increasing"),
    n_decreasing = sum(trend_class == "decreasing"),
    pct_directional = round(sum(trend_class %in% c("increasing", "decreasing")) / n() * 100, 1),
    mean_ratio = round(mean(mean_ratio, na.rm = TRUE), 2),
    mean_cv = round(mean(cv, na.rm = TRUE), 3),
    .groups = "drop"
  )

for (i in seq_len(nrow(sf_trends))) {
  s <- sf_trends[i, ]
  cat(sprintf("\n  %s (n=%d):\n", s$stock_or_flux, s$n))
  cat(sprintf("    Stable: %d, Variable: %d, Increasing: %d, Decreasing: %d\n",
              s$n_stable, s$n_variable, s$n_increasing, s$n_decreasing))
  cat(sprintf("    Directional: %.1f%%\n", s$pct_directional))
  cat(sprintf("    Mean ratio: %.2f, Mean CV: %.3f\n", s$mean_ratio, s$mean_cv))
}

# Trend distribution by experiment type
cat("\n--- Trends by Experiment Type ---\n")
exp_trends <- trend_clean %>%
  group_by(experiment_type) %>%
  summarise(
    n = n(),
    pct_directional = round(sum(trend_class %in% c("increasing", "decreasing")) / n() * 100, 1),
    mean_ratio = round(mean(mean_ratio, na.rm = TRUE), 2),
    mean_cv = round(mean(cv, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_directional))

for (i in seq_len(nrow(exp_trends))) {
  e <- exp_trends[i, ]
  cat(sprintf("  %s (n=%d): %.1f%% directional, mean ratio=%.2f, mean CV=%.3f\n",
              e$experiment_type, e$n, e$pct_directional, e$mean_ratio, e$mean_cv))
}

# Detection time by ecosystem
cat("\n--- Detection Time by Ecosystem Type ---\n")
detect_eco <- detected_directional %>%
  left_join(trend_clean %>% select(source, Treatment, site_type),
            by = c("source", "Treatment")) %>%
  group_by(site_type) %>%
  summarise(
    n = n(),
    median_years = round(median(time_to_detect, na.rm = TRUE), 1),
    mean_years = round(mean(time_to_detect, na.rm = TRUE), 1),
    .groups = "drop"
  )

for (i in seq_len(nrow(detect_eco))) {
  d <- detect_eco[i, ]
  cat(sprintf("  %s (n=%d): median=%.1f, mean=%.1f years\n",
              d$site_type, d$n, d$median_years, d$mean_years))
}

# SiZer by ecosystem
cat("\n--- SiZer Slope Changes by Ecosystem Type ---\n")
sizer_eco <- sizer_results %>%
  left_join(metadata %>% distinct(source, site_type), by = "source") %>%
  group_by(site_type) %>%
  summarise(
    n = n(),
    pct_with_changes = round(sum(n_slope_changes > 0) / n() * 100, 1),
    mean_changes = round(mean(n_slope_changes), 2),
    .groups = "drop"
  )

for (i in seq_len(nrow(sizer_eco))) {
  s <- sizer_eco[i, ]
  cat(sprintf("  %s (n=%d): %.1f%% with changes, mean=%.1f changes\n",
              s$site_type, s$n, s$pct_with_changes, s$mean_changes))
}

# =============================================================================
# SIGN FLIPS SUMMARY
# =============================================================================

cat("\n\n=== SIGN FLIP ANALYSIS ===\n\n")

sign_flips_clean <- sign_flips %>%
  filter(!is.na(n_flips))

cat(sprintf("Treatment combinations analyzed: %d\n", nrow(sign_flips_clean)))
cat(sprintf("With >= 1 sign flip: %d (%.1f%%)\n",
            sum(sign_flips_clean$n_flips > 0),
            round(sum(sign_flips_clean$n_flips > 0) / nrow(sign_flips_clean) * 100, 1)))
cat(sprintf("Mean flips per combination: %.1f\n",
            mean(sign_flips_clean$n_flips, na.rm = TRUE)))
cat(sprintf("Max flips: %d\n", max(sign_flips_clean$n_flips, na.rm = TRUE)))

# =============================================================================
# SUPPLEMENTAL TABLE S1: Full Per-Treatment Statistics
# =============================================================================

cat("\n\n=== SUPPLEMENTAL TABLE S1 ===\n")

table_s1 <- trend_clean %>%
  left_join(metadata %>% distinct(source, site_full_name), by = "source") %>%
  left_join(early_full %>% select(source, Treatment, early_lrr, full_lrr, lrr_change),
            by = c("source", "Treatment")) %>%
  left_join(detection_data %>% select(source, Treatment, time_to_detect, detected),
            by = c("source", "Treatment")) %>%
  select(
    Site = site_abbr,
    `Site Name` = site_full_name,
    Ecosystem = site_type,
    Experiment = experiment_type,
    `Stock/Flux` = stock_or_flux,
    Treatment,
    Trend = trend_class,
    Slope = slope,
    `P-value` = p_value,
    `R-squared` = r_squared,
    CV = cv,
    `Mean Ratio` = mean_ratio,
    `N Timepoints` = n_timepoints,
    `Year Span` = year_span,
    `Early LRR` = early_lrr,
    `Full LRR` = full_lrr,
    `LRR Change` = lrr_change,
    Detected = detected,
    `Years to Detect` = time_to_detect
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>%
  arrange(Site, Treatment)

write.csv(table_s1, "figures/supplemental/TableS1_full_treatment_statistics.csv",
          row.names = FALSE)
cat("  -> Written to figures/supplemental/TableS1_full_treatment_statistics.csv\n")
cat(sprintf("  Rows: %d\n", nrow(table_s1)))

# =============================================================================
# SUPPLEMENTAL TABLE S2: SiZer Results
# =============================================================================

cat("\n--- SUPPLEMENTAL TABLE S2 ---\n")

table_s2 <- sizer_results %>%
  left_join(metadata %>% distinct(source, site_full_name, site_type), by = "source") %>%
  select(
    Site = site_abbr,
    `Site Name` = site_full_name,
    Ecosystem = site_type,
    Treatment = treatment,
    `N Timepoints` = n_timepoints,
    `N Slope Changes` = n_slope_changes,
    `Change Years` = change_years,
    `Segment Slopes` = segment_slopes,
    # model_p_values omitted: identical to the slope p-values by construction
    # (single-predictor segments, t^2 = F)
    `Segment P-values` = segment_p_values,
    Bandwidth = bandwidth
  ) %>%
  arrange(Site, Treatment)

write.csv(table_s2, "figures/supplemental/TableS2_sizer_results.csv",
          row.names = FALSE)
cat("  -> Written to figures/supplemental/TableS2_sizer_results.csv\n")
cat(sprintf("  Rows: %d\n", nrow(table_s2)))

# =============================================================================
# QUICK-REFERENCE: KEY NUMBERS FOR MANUSCRIPT TEXT
# =============================================================================

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("QUICK-REFERENCE: KEY NUMBERS FOR MANUSCRIPT TEXT\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("ABSTRACT / INTRO:\n")
cat(sprintf("  %d LTER sites, %d ecosystem types, %d experiment types\n",
            n_sites, n_ecosystems, n_experiment_types))
cat(sprintf("  %d site-treatment combinations analyzed\n", nrow(trend_clean)))
cat(sprintf("  Experiment durations: %.0f-%.0f years\n",
            min(year_spans, na.rm = TRUE), max(year_spans, na.rm = TRUE)))

cat("\nRESULTS 3.2 (Trends):\n")
cat(sprintf("  %.0f%% non-directional, %.0f%% directional\n", non_dir_pct, dir_pct))
for (i in seq_len(nrow(trend_props))) {
  cat(sprintf("  %s: %d (%.0f%%)\n",
              trend_props$trend_class[i], trend_props$n[i], trend_props$pct[i]))
}

cat("\nRESULTS 3.3 (Early vs Full):\n")
cat(sprintf("  Correlation: r=%.2f, p=%.2e\n", cor_test$estimate, cor_test$p.value))
cat(sprintf("  Mean |LRR change|: %.3f\n",
            mean(abs(early_full_classified$lrr_change), na.rm = TRUE)))
for (i in seq_len(nrow(class_counts))) {
  cat(sprintf("  %s: %d (%.0f%%)\n",
              class_counts$classification[i], class_counts$n[i], class_counts$pct[i]))
}

cat("\nRESULTS 3.4 (SiZer):\n")
cat(sprintf("  %.0f%% with slope changes\n", pct_with_changes))
cat(sprintf("  %d/%d treatment combinations\n", n_with_changes, n_sizer_total))

cat("\nRESULTS 3.5 (Detection):\n")
if (nrow(detected_directional) > 0) {
  cat(sprintf("  Median: %.1f years, Range: %.0f-%.0f years\n",
              median(detected_directional$time_to_detect, na.rm = TRUE),
              min(detected_directional$time_to_detect, na.rm = TRUE),
              max(detected_directional$time_to_detect, na.rm = TRUE)))
}

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Close report
sink()

cat("\n=== MANUSCRIPT RESULTS COMPLETE ===\n")
cat("Report written to:", report_path, "\n")
cat("Tables written to: figures/Table1_*.csv, Table2_*.csv, Table3_*.csv\n")
cat("Supplemental tables: figures/supplemental/TableS1_*.csv, TableS2_*.csv\n")
