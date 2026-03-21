# 04_detection-time.R
# Sequential analysis: how many timepoints are needed to detect significant trends?

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
# library(ggdist)  # disabled due to ggplot2 4.0 compatibility

source("R/utils_labels.R")
source("R/utils_analysis.R")
source("R/utils_plots.R")

cat("=== DETECTION TIME ANALYSIS ===\n")

# Read data
summary_data <- read.csv("data/harmonized/relative_response_summary.csv",
                         stringsAsFactors = FALSE)
summary_data$Date_parsed <- as.Date(summary_data$Date_parsed)

trend_analysis <- read.csv("data/harmonized/trend_analysis_results.csv",
                           stringsAsFactors = FALSE)
trend_analysis$trend_class <- as_trend_factor(trend_analysis$trend_class)

metadata <- get_site_metadata()

# Apply detection time analysis to all experiment-treatment combinations
detection_analysis <- summary_data %>%
  group_by(source, Treatment) %>%
  group_modify(~ detect_trend_timepoints(.x)) %>%
  ungroup()

# Join with trend classification
detection_with_class <- detection_analysis %>%
  left_join(trend_analysis %>%
              select(source, Treatment, trend_class, slope, p_value, cv, mean_ratio),
            by = c("source", "Treatment")) %>%
  mutate(site_abbr = source_to_abbr(source, metadata))

# Focus on significant trends
significant_trends <- detection_with_class %>%
  filter(trend_class %in% c("increasing", "decreasing"), detected == TRUE)

cat("\nSignificant trends detected:", nrow(significant_trends), "\n")

# Summary statistics — now including duration & density
if (nrow(significant_trends) > 0) {
  detection_summary <- significant_trends %>%
    group_by(trend_class) %>%
    summarise(
      n = n(),
      mean_years_to_detect = round(mean(time_to_detect, na.rm = TRUE), 1),
      median_years_to_detect = round(median(time_to_detect, na.rm = TRUE), 1),
      min_years_to_detect = round(min(time_to_detect, na.rm = TRUE), 1),
      max_years_to_detect = round(max(time_to_detect, na.rm = TRUE), 1),
      mean_density_per_yr = round(mean(meas_density_per_yr, na.rm = TRUE), 1),
      median_density_per_yr = round(median(meas_density_per_yr, na.rm = TRUE), 1),
      mean_total_duration_yr = round(mean(total_duration_yr, na.rm = TRUE), 1),
      mean_median_interval_d = round(mean(median_interval_days, na.rm = TRUE), 0),
      .groups = "drop"
    )

  cat("\n--- DETECTION TIME SUMMARY (with duration & density) ---\n")
  print(as.data.frame(detection_summary))

  cat("\nOverall median years to detect:",
      round(median(significant_trends$time_to_detect, na.rm = TRUE), 1), "\n")
  cat("Range:",
      round(min(significant_trends$time_to_detect, na.rm = TRUE), 1), "to",
      round(max(significant_trends$time_to_detect, na.rm = TRUE), 1), "years\n")
  cat("Median measurement density:",
      round(median(significant_trends$meas_density_per_yr, na.rm = TRUE), 1),
      "meas/yr\n")
}

# Save results (now includes density/duration columns)
write.csv(detection_with_class, "data/harmonized/detection_time_analysis.csv", row.names = FALSE)

# --- Plots ---

if (nrow(significant_trends) > 0) {
  # Plot 1: Years to detection by trend type
  p1 <- ggplot(significant_trends, aes(x = trend_class, y = time_to_detect,
                                       fill = trend_class)) +
    geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
                 linewidth = 0.3, color = "gray30") +
    geom_jitter(width = 0.15, alpha = 0.4, size = 0.8, color = "gray30") +
    scale_fill_trend() +
    labs(title = "Years to Detection",
         x = "Trend Type", y = "Years to Detection") +
    theme_ccycling() + theme(legend.position = "none")

  # Plot 2: Detection time (years) vs measurement density
  p2 <- ggplot(significant_trends, aes(x = meas_density_per_yr, y = time_to_detect,
                                       color = trend_class)) +
    geom_point(size = 1.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 0.5) +
    scale_color_trend() +
    labs(title = "Detection Time vs Measurement Density",
         subtitle = "Does denser sampling shorten detection?",
         x = "Measurements per Year", y = "Years to Detection",
         color = "Trend Type") +
    theme_ccycling()

  # Plot 3: Duration vs timepoints bubble (size = density)
  p3 <- ggplot(significant_trends, aes(x = total_duration_yr, y = min_n_for_detection,
                                       color = trend_class,
                                       size = meas_density_per_yr)) +
    geom_point(alpha = 0.6) +
    scale_color_trend() +
    scale_size_continuous(name = "Meas./yr", range = c(0.8, 5)) +
    labs(title = "Duration vs Timepoints to Detection",
         subtitle = "Bubble size = measurement density",
         x = "Total Study Duration (years)",
         y = "Timepoints to Detection",
         color = "Trend Type") +
    theme_ccycling()

  # Plot 4: Detection time vs effect size
  p4 <- ggplot(significant_trends, aes(x = abs(final_slope), y = time_to_detect,
                                       color = trend_class)) +
    geom_point(size = 1.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 0.5) +
    scale_color_trend() +
    scale_x_log10() +
    labs(title = "Detection Time vs Effect Size",
         x = "|Slope| (log scale)", y = "Years to Detection",
         color = "Trend Type") +
    theme_ccycling()

  # Plot 5: Detection time vs CV
  p5 <- ggplot(significant_trends, aes(x = cv, y = time_to_detect,
                                       color = trend_class)) +
    geom_point(size = 1.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 0.5) +
    scale_color_trend() +
    labs(title = "Detection Time vs Variability",
         x = "Coefficient of Variation", y = "Years to Detection",
         color = "Trend Type") +
    theme_ccycling()

  # Plot 6: Detection time by site
  p6 <- ggplot(significant_trends, aes(x = reorder(site_abbr, time_to_detect),
                                       y = time_to_detect,
                                       color = trend_class)) +
    geom_point(size = 1.5, alpha = 0.6) +
    scale_color_trend() +
    coord_flip() +
    labs(title = "Detection Time by Site",
         x = "Site", y = "Years to Detection",
         color = "Trend Type") +
    theme_ccycling()

  detection_plots <- (p1 | p2) / (p3 | p4) / (p5 | p6) +
    plot_annotation(tag_levels = "A")

  ggsave("figures/detection_time_analysis.png", detection_plots,
         width = 7.2, height = 9, dpi = 300, bg = "white")
  ggsave("figures/detection_time_analysis.pdf", detection_plots,
         width = 7.2, height = 9, device = cairo_pdf)

  # --- Correlations: duration & density ---
  cat("\n--- FACTORS AFFECTING DETECTION TIME ---\n")

  cor_density <- cor.test(significant_trends$meas_density_per_yr,
                          significant_trends$time_to_detect,
                          method = "spearman", use = "complete.obs")
  cat(sprintf("Meas density vs years to detect: rho = %.3f, p = %.4f\n",
              cor_density$estimate, cor_density$p.value))

  cor_dur <- cor.test(significant_trends$total_duration_yr,
                      significant_trends$min_n_for_detection,
                      method = "spearman", use = "complete.obs")
  cat(sprintf("Total duration vs N to detect: rho = %.3f, p = %.4f\n",
              cor_dur$estimate, cor_dur$p.value))

  cor_slope <- cor.test(abs(significant_trends$final_slope),
                        significant_trends$time_to_detect,
                        method = "spearman", use = "complete.obs")
  cat(sprintf("|Slope| vs years to detect: rho = %.3f, p = %.4f\n",
              cor_slope$estimate, cor_slope$p.value))

  cor_cv <- cor.test(significant_trends$cv,
                     significant_trends$time_to_detect,
                     method = "spearman", use = "complete.obs")
  cat(sprintf("CV vs years to detect: rho = %.3f, p = %.4f\n",
              cor_cv$estimate, cor_cv$p.value))

  # Multiple regression separating density from duration
  if (nrow(significant_trends) > 10) {
    detection_model <- lm(time_to_detect ~ abs(final_slope) + cv + mean_ratio +
                            meas_density_per_yr + total_duration_yr,
                          data = significant_trends)
    cat("\nMultiple regression predicting years to detection:\n")
    print(summary(detection_model))
  }
}

cat("\n=== COMPLETE ===\n")
