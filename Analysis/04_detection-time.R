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

# Summary statistics
if (nrow(significant_trends) > 0) {
  detection_summary <- significant_trends %>%
    group_by(trend_class) %>%
    summarise(
      n = n(),
      mean_n_to_detect = mean(min_n_for_detection, na.rm = TRUE),
      median_n_to_detect = median(min_n_for_detection, na.rm = TRUE),
      min_n_to_detect = min(min_n_for_detection, na.rm = TRUE),
      max_n_to_detect = max(min_n_for_detection, na.rm = TRUE),
      mean_years_to_detect = mean(time_to_detect, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\n--- DETECTION TIME SUMMARY ---\n")
  print(as.data.frame(detection_summary))

  cat("\nOverall median timepoints to detect:",
      median(significant_trends$min_n_for_detection, na.rm = TRUE), "\n")
  cat("Range:",
      min(significant_trends$min_n_for_detection, na.rm = TRUE), "to",
      max(significant_trends$min_n_for_detection, na.rm = TRUE), "\n")
}

# Save results
write.csv(detection_with_class, "data/harmonized/detection_time_analysis.csv", row.names = FALSE)

# --- Plots ---

if (nrow(significant_trends) > 0) {
  # Plot 1: Timepoints to detection (half-eye + boxplot + jitter)
  p1 <- ggplot(significant_trends, aes(x = trend_class, y = min_n_for_detection,
                                       fill = trend_class)) +
    geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
                 linewidth = 0.3, color = "gray30") +
    geom_jitter(width = 0.15, alpha = 0.4, size = 0.8, color = "gray30") +
    scale_fill_trend() +
    labs(title = "Timepoints to Detection",
         x = "Trend Type", y = "Number of Timepoints") +
    theme_ccycling() + theme(legend.position = "none")

  # Plot 2: Years to detection
  p2 <- ggplot(significant_trends, aes(x = trend_class, y = time_to_detect,
                                       fill = trend_class)) +
    geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
                 linewidth = 0.3, color = "gray30") +
    geom_jitter(width = 0.15, alpha = 0.4, size = 0.8, color = "gray30") +
    scale_fill_trend() +
    labs(title = "Years to Detection",
         x = "Trend Type", y = "Years to Detection") +
    theme_ccycling() + theme(legend.position = "none")

  # Plot 3: Detection time vs effect size
  p3 <- ggplot(significant_trends, aes(x = abs(final_slope), y = min_n_for_detection,
                                       color = trend_class)) +
    geom_point(size = 1.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 0.5) +
    scale_color_trend() +
    scale_x_log10() +
    labs(title = "Detection Time vs Effect Size",
         x = "|Slope| (log scale)", y = "Timepoints to Detection",
         color = "Trend Type") +
    theme_ccycling()

  # Plot 4: Detection time vs CV
  p4 <- ggplot(significant_trends, aes(x = cv, y = min_n_for_detection,
                                       color = trend_class)) +
    geom_point(size = 1.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 0.5) +
    scale_color_trend() +
    labs(title = "Detection Time vs Variability",
         x = "Coefficient of Variation", y = "Timepoints to Detection",
         color = "Trend Type") +
    theme_ccycling()

  detection_plots <- (p1 | p2) / (p3 | p4) +
    plot_annotation(tag_levels = "A")

  ggsave("figures/detection_time_analysis.png", detection_plots,
         width = 7.2, height = 6, dpi = 300, bg = "white")
  ggsave("figures/detection_time_analysis.pdf", detection_plots,
         width = 7.2, height = 6, device = cairo_pdf)
}

cat("\n=== COMPLETE ===\n")
