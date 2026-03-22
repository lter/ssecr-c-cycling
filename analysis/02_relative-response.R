# 02_relative-response.R
# Calculate treatment response relative to control for each experiment
# Generates per-experiment plots and combined faceted overview

library(dplyr)
library(ggplot2)

source("R/utils_labels.R")
source("R/utils_dates.R")
source("R/utils_analysis.R")
source("R/utils_plots.R")

cat("=== RELATIVE RESPONSE CALCULATION ===\n")

# Read harmonized data
df <- read.csv("data/harmonized/harmonized_current.csv", stringsAsFactors = FALSE)
metadata <- get_site_metadata()
cat("Loaded", nrow(df), "rows from harmonized dataset\n")

# Calculate relative responses
relative_data <- calculate_relative_response(df)
cat("Relative responses calculated:", nrow(relative_data), "treatment observations\n")

# Calculate mean and SE for each experiment, treatment, and timepoint
summary_data <- relative_data %>%
  group_by(source, Treatment, Date_parsed) %>%
  summarise(
    mean_response = mean(relative_response, na.rm = TRUE),
    se_response = sd(relative_response, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(site_abbr = source_to_abbr(source, metadata))

# Save intermediate results
write.csv(summary_data, "data/harmonized/relative_response_summary.csv", row.names = FALSE)
write.csv(relative_data, "data/harmonized/relative_response_full.csv", row.names = FALSE)
cat("Summary data saved:", nrow(summary_data), "rows\n")

# --- Plots ---

if (!dir.exists("figures")) dir.create("figures")

# Individual experiment plots
experiments <- unique(summary_data$source)

for (exp in experiments) {
  exp_data <- summary_data %>% filter(source == exp)
  if (nrow(exp_data) == 0) next

  site <- source_to_abbr(exp, metadata)
  trt_names <- sort(unique(exp_data$Treatment))
  trt_pal <- generate_treatment_palette(length(trt_names), trt_names)

  p <- ggplot(exp_data, aes(x = Date_parsed, y = mean_response,
                            color = Treatment, fill = Treatment)) +
    geom_ref_ratio() +
    geom_errorbar(aes(ymin = mean_response - se_response,
                      ymax = mean_response + se_response),
                  width = 0, alpha = 0.3, linewidth = 0.3) +
    geom_point(size = 1.2, alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.12, linewidth = 0.5) +
    scale_color_manual(values = trt_pal) +
    scale_fill_manual(values = trt_pal) +
    labs(title = paste("Treatment Response:", site),
         subtitle = "Points: mean \u00b1 SE; curves: loess with 95% CI",
         x = "Year", y = "Treatment / Control Ratio") +
    theme_ccycling() +
    theme(legend.position = "bottom")

  save_figure(file.path("figures", paste0(site, "_", gsub("\\.csv$", "", exp),
                                          "_relative_response.png")),
              p, size = "double")
}

# Faceted plot: all experiments — spaghetti with bold mean
all_plot <- ggplot(summary_data, aes(x = Date_parsed, y = mean_response)) +
  geom_ref_ratio() +
  geom_line(aes(group = Treatment), alpha = 0.2, linewidth = 0.2, color = "gray60") +
  geom_point(aes(group = Treatment), size = 0.4, alpha = 0.15, color = "gray60") +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.12,
              linewidth = 0.5, color = "#3A7CA5", fill = "#3A7CA5") +
  facet_wrap(~site_abbr, scales = "free") +
  labs(title = "Treatment Response Relative to Control",
       subtitle = "Gray lines: individual treatments; blue: mean loess trend",
       x = "Year", y = "Treatment / Control Ratio") +
  theme_ccycling() +
  theme(strip.text = element_text(size = 7))

ggsave("figures/all_experiments_relative_response.png",
       all_plot, width = 7.2, height = 7.2, dpi = 300, bg = "white")

# Log response ratio version
log_summary <- summary_data %>%
  group_by(source, Treatment, Date_parsed, site_abbr) %>%
  summarise(mean_log = mean(log(mean_response), na.rm = TRUE),
            se_log = sd(log(mean_response), na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

all_plot_log <- ggplot(log_summary, aes(x = Date_parsed, y = mean_log)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "gray55") +
  geom_line(aes(group = Treatment), alpha = 0.2, linewidth = 0.2, color = "gray60") +
  geom_point(aes(group = Treatment), size = 0.4, alpha = 0.15, color = "gray60") +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.12,
              linewidth = 0.5, color = "#3A7CA5", fill = "#3A7CA5") +
  facet_wrap(~site_abbr, scales = "free") +
  labs(title = "Log Response Ratio Across All Experiments",
       subtitle = "0 = no effect; blue line: linear fit",
       x = "Year", y = "Log Response Ratio") +
  theme_ccycling() +
  theme(strip.text = element_text(size = 7))

ggsave("figures/all_experiments_log_response.png",
       all_plot_log, width = 7.2, height = 7.2, dpi = 300, bg = "white")

# --- Trend-colored LRR variants ---
# Requires trend_analysis_results.csv from 03_trend-classification.R
trend_file <- "data/harmonized/trend_analysis_results.csv"
if (file.exists(trend_file)) {
  trend_analysis <- read.csv(trend_file, stringsAsFactors = FALSE)

  log_with_trend <- log_summary %>%
    left_join(trend_analysis %>% select(source, Treatment, trend_class),
              by = c("source", "Treatment")) %>%
    filter(!is.na(trend_class))

  library(patchwork)

  # Shared facet theme for 6x3 layout
  facet_theme <- theme_ccycling(base_size = 14) +
    theme(strip.text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 13),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 13))

  # --- Read summary data for bottom panels ---
  trend_clean <- trend_analysis %>% filter(!is.na(trend_class))
  trend_clean$trend_class <- as_trend_factor(trend_clean$trend_class)
  overall_proportions <- trend_clean %>%
    count(trend_class) %>%
    mutate(proportion = n / sum(n),
           percentage = round(proportion * 100, 1))

  # Compact summary panels (shared by both variants)
  make_summary_panels <- function() {
    pa <- overall_proportions %>%
      ggplot(aes(x = "", y = n, fill = trend_class)) +
      geom_bar(stat = "identity", position = "fill", width = 0.5,
               color = "white", linewidth = 0.3) +
      geom_text(aes(label = n),
                position = position_fill(vjust = 0.5), size = 5, fontface = "bold") +
      scale_fill_trend() +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(y = "Proportion", x = "", fill = "Trend") +
      theme_ccycling(base_size = 14) +
      theme(legend.position = "right",
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.line.y = element_blank())

    pb <- trend_clean %>%
      ggplot(aes(x = trend_class, y = mean_ratio, fill = trend_class)) +
      geom_ref_ratio() +
      geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
                   linewidth = 0.3, color = "gray30") +
      geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, color = "gray30") +
      scale_fill_trend() +
      scale_y_continuous(trans = "log2", breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 4)) +
      labs(x = "", y = "Mean Trt/Ctrl (log)") +
      theme_ccycling(base_size = 14) + theme(legend.position = "none") +
      rotate_x_labels(30)

    pc <- trend_clean %>%
      ggplot(aes(x = trend_class, y = cv, fill = trend_class)) +
      geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
                   linewidth = 0.3, color = "gray30") +
      geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, color = "gray30") +
      geom_ref_threshold(0.3) +
      scale_fill_trend() +
      labs(x = "", y = "CV") +
      theme_ccycling(base_size = 14) + theme(legend.position = "none") +
      rotate_x_labels(30)

    pd <- trend_clean %>%
      ggplot(aes(x = trend_class, y = year_span, fill = trend_class)) +
      geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
                   linewidth = 0.3, color = "gray30") +
      geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, color = "gray30") +
      scale_fill_trend() +
      labs(x = "", y = "Duration (yr)") +
      theme_ccycling(base_size = 14) + theme(legend.position = "none") +
      rotate_x_labels(30)

    (pa | pb | pc | pd)
  }

  summary_row <- make_summary_panels()

  # Variant A: grey spaghetti + one pooled smooth per trend class per site
  lrr_pooled_by_trend <- ggplot(log_with_trend, aes(x = Date_parsed, y = mean_log)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "gray55") +
    geom_line(aes(group = Treatment), alpha = 0.2, linewidth = 0.2, color = "gray60") +
    geom_point(aes(group = Treatment), size = 0.4, alpha = 0.15, color = "gray60") +
    geom_smooth(aes(color = trend_class, fill = trend_class),
                method = "loess", se = TRUE, alpha = 0.12, linewidth = 0.5) +
    scale_color_trend(name = "Trend") +
    scale_fill_trend(name = "Trend") +
    facet_wrap(~site_abbr, scales = "free", ncol = 6) +
    labs(x = "Year", y = "Log Response Ratio") +
    facet_theme

  combined_A <- lrr_pooled_by_trend / summary_row +
    plot_layout(heights = c(5, 1))

  ggsave("figures/all_experiments_lrr_pooled_by_trend.png",
         combined_A, width = 16, height = 10, dpi = 300, bg = "white")

  # Variant B: grey spaghetti + one total pooled smooth colored by dominant trend
  dominant_trend <- trend_analysis %>%
    filter(!is.na(trend_class)) %>%
    count(source, trend_class) %>%
    group_by(source) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(source, dominant_trend = trend_class)

  log_with_dominant <- log_summary %>%
    left_join(dominant_trend, by = "source") %>%
    filter(!is.na(dominant_trend))

  lrr_total_pooled <- ggplot(log_with_dominant, aes(x = Date_parsed, y = mean_log)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "gray55") +
    geom_line(aes(group = Treatment), alpha = 0.2, linewidth = 0.2, color = "gray60") +
    geom_point(aes(group = Treatment), size = 0.4, alpha = 0.15, color = "gray60") +
    geom_smooth(aes(color = dominant_trend, fill = dominant_trend),
                method = "loess", se = TRUE, alpha = 0.12, linewidth = 0.5) +
    scale_color_trend(name = "Dominant Trend") +
    scale_fill_trend(name = "Dominant Trend") +
    facet_wrap(~site_abbr, scales = "free", ncol = 6) +
    labs(x = "Year", y = "Log Response Ratio") +
    facet_theme

  combined_B <- lrr_total_pooled / summary_row +
    plot_layout(heights = c(5, 2))

  ggsave("figures/all_experiments_lrr_total_pooled_trend.png",
         combined_B, width = 16, height = 12, dpi = 300, bg = "white")

  cat("Trend-colored LRR variants saved (6x3 + summary panels)\n")
} else {
  cat("Skipping trend-colored LRR: run 03_trend-classification.R first\n")
}

cat("\nPlots saved to figures/\n")
cat("=== COMPLETE ===\n")
