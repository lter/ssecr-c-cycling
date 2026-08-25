# 03_trend-classification.R
# Classify temporal trends for each experiment-treatment combination
# Generates: trend summary plots, stacked bars, heatmaps, ecosystem groupings

library(dplyr)
library(ggplot2)
library(patchwork)
# library(ggdist)  # disabled due to ggplot2 4.0 compatibility

source("R/utils_labels.R")
source("R/utils_analysis.R")
source("R/utils_plots.R")

cat("=== TREND CLASSIFICATION ===\n")

# Read summary data from previous step
summary_data <- read.csv("data/harmonized/relative_response_summary.csv",
                         stringsAsFactors = FALSE)
summary_data$Date_parsed <- as.Date(summary_data$Date_parsed)
metadata <- get_site_metadata()

cat("Loaded", nrow(summary_data), "summary observations\n")

# --- Trend Classification ---

# Keep trend_class as character here so "insufficient_data" survives into the
# CSV (as_trend_factor has only the 4 analyzable levels and would turn it into
# NA); convert to factor only for plotting, after filtering.
trend_analysis <- summary_data %>%
  group_by(source, Treatment) %>%
  group_modify(~ classify_trend(.x)) %>%
  ungroup() %>%
  mutate(site_abbr = source_to_abbr(source, metadata))

# Join site metadata
trend_analysis <- trend_analysis %>%
  left_join(metadata %>% select(source, site_type, experiment_type, stock_or_flux),
            by = "source")

cat("Classified", nrow(trend_analysis), "treatment-experiment combinations\n")

# Save results
write.csv(trend_analysis, "data/harmonized/trend_analysis_results.csv", row.names = FALSE)

# --- Sign-Flip Analysis (Trend Reversals) ---

sign_flip_analysis <- summary_data %>%
  group_by(source, Treatment) %>%
  group_modify(~ count_sign_flips(.x)) %>%
  ungroup() %>%
  mutate(site_abbr = source_to_abbr(source, metadata))

# Add timepoints and compute flip percentage (flips / transitions)
n_pts <- summary_data %>%
  group_by(source, Treatment) %>%
  summarise(n_timepoints = n(), .groups = "drop")

sign_flip_analysis <- sign_flip_analysis %>%
  left_join(n_pts, by = c("source", "Treatment")) %>%
  mutate(flip_pct = ifelse(n_timepoints > 1,
                           100 * n_flips / (n_timepoints - 1), NA_real_))

write.csv(sign_flip_analysis, "data/harmonized/sign_flip_analysis.csv", row.names = FALSE)

cat("\nSign-flip summary:\n")
cat("  Mean flips per treatment:", round(mean(sign_flip_analysis$n_flips, na.rm = TRUE), 1), "\n")
cat("  Mean flip %:", round(mean(sign_flip_analysis$flip_pct, na.rm = TRUE), 1), "%\n")
cat("  Median flip %:", round(median(sign_flip_analysis$flip_pct, na.rm = TRUE), 1), "%\n")
cat("  Range flip %:", round(min(sign_flip_analysis$flip_pct, na.rm = TRUE), 1), "-",
    round(max(sign_flip_analysis$flip_pct, na.rm = TRUE), 1), "%\n")

# --- Summary Statistics ---

trend_clean <- trend_analysis %>%
  filter(trend_class != "insufficient_data") %>%
  mutate(trend_class = as_trend_factor(trend_class))

overall_proportions <- trend_clean %>%
  count(trend_class, .drop = FALSE) %>%
  mutate(proportion = n / sum(n),
         percentage = round(proportion * 100, 1))

cat("\n--- OVERALL TREND DISTRIBUTION ---\n")
print(as.data.frame(overall_proportions))

# NOTE: compute the median BEFORE the mean — summarise() evaluates
# sequentially, so once mean_ratio is reassigned to the group mean, a later
# median(mean_ratio) would just return that mean again
trend_stats <- trend_clean %>%
  group_by(trend_class) %>%
  summarise(
    n = n(),
    median_ratio = median(mean_ratio, na.rm = TRUE),
    mean_ratio = mean(mean_ratio, na.rm = TRUE),
    mean_cv = mean(cv, na.rm = TRUE),
    mean_timepoints = mean(n_timepoints, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- STATISTICS BY TREND TYPE ---\n")
print(as.data.frame(trend_stats))

# --- Plots ---

if (!dir.exists("figures")) dir.create("figures")

# Plot 1: Bar chart of trend classifications
p_bar <- ggplot(trend_clean, aes(x = trend_class, fill = trend_class)) +
  geom_bar(color = NA) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 2.5) +
  scale_fill_trend() +
  labs(title = "Classification of Treatment Response Trends",
       x = "Trend Classification",
       y = "Number of Treatment-Experiment Combinations") +
  theme_ccycling() +
  theme(legend.position = "none")

save_figure("figures/trend_classification_summary.png", p_bar, size = "onehalf")

# Plot 2: Stacked bar by experiment, faceted by ecosystem type
p_stacked <- trend_clean %>%
  ggplot(aes(x = site_abbr, fill = trend_class)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.3) +
  scale_fill_trend() +
  facet_wrap(~site_type, scales = "free_x") +
  labs(title = "Proportion of Trend Types by Site and Ecosystem",
       x = "Site", y = "Proportion", fill = "Trend Type") +
  theme_ccycling()

save_figure("figures/trend_by_experiment_faceted.png", p_stacked, size = "double")

# Plot 3: Trends by ecosystem type (aggregated)
p_ecosystem <- trend_clean %>%
  ggplot(aes(x = site_type, fill = trend_class)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.3) +
  scale_fill_trend() +
  labs(title = "Trend Distribution by Ecosystem Type",
       x = "Ecosystem Type", y = "Proportion", fill = "Trend Type") +
  theme_ccycling() +
  rotate_x_labels(30)

save_figure("figures/trend_by_ecosystem_type.png", p_ecosystem, size = "onehalf")

# Plot 4: Trends by stock vs flux
p_stock_flux <- trend_clean %>%
  filter(!is.na(stock_or_flux), stock_or_flux != "NA") %>%
  ggplot(aes(x = stock_or_flux, fill = trend_class)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.3) +
  scale_fill_trend() +
  labs(title = "Trend Distribution: Stocks vs Fluxes vs Other",
       x = "Response Variable Type", y = "Proportion", fill = "Trend Type") +
  theme_ccycling()

save_figure("figures/trend_by_stock_flux.png", p_stock_flux, size = "onehalf")

# Plot 5: Heatmap of trends by experiment
heatmap_data <- trend_clean %>%
  count(site_abbr, trend_class, .drop = FALSE) %>%
  group_by(site_abbr) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

p_heatmap <- ggplot(heatmap_data, aes(x = trend_class, y = site_abbr, fill = proportion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 2.5, color = "gray20") +
  scale_fill_heatmap(midpoint = 0.25) +
  labs(title = "Distribution of Trend Types Across Sites",
       subtitle = "Numbers show count; colors show proportion",
       x = "Trend Type", y = "Site", fill = "Proportion") +
  theme_ccycling() +
  rotate_x_labels(30)

save_figure("figures/trend_heatmap_by_site.png", p_heatmap, size = "double")

# Plot 6: Scatter - slope vs CV
p_scatter <- trend_clean %>%
  ggplot(aes(x = cv, y = slope, color = trend_class)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_ref_threshold(0, direction = "h") +
  geom_ref_threshold(0.3, direction = "v") +
  scale_color_trend() +
  labs(title = "Temporal Trend Characteristics",
       subtitle = "Dotted lines show classification thresholds",
       x = "Coefficient of Variation", y = "Slope (change in ratio per day)",
       color = "Trend Type") +
  theme_ccycling()

save_figure("figures/trend_characteristics_scatter.png", p_scatter, size = "double")

# Plot 7: Publication-quality multi-panel summary (half-eye + boxplot + jitter)
panel_a <- overall_proportions %>%
  ggplot(aes(x = "", y = n, fill = trend_class)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5,
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_fill(vjust = 0.5), size = 2.5, fontface = "bold") +
  scale_fill_trend() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(y = "Proportion", x = "", fill = "Trend Type") +
  theme_ccycling() +
  theme(legend.position = "right",
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

panel_b <- trend_clean %>%
  ggplot(aes(x = trend_class, y = mean_ratio, fill = trend_class)) +
  geom_ref_ratio() +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
               linewidth = 0.3, color = "gray30") +
  geom_jitter(width = 0.15, alpha = 0.35, size = 0.8, color = "gray30") +
  scale_fill_trend() +
  scale_y_continuous(trans = "log2", breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 4)) +
  labs(x = "Trend Type", y = "Mean Treatment/Control (log scale)") +
  theme_ccycling() + theme(legend.position = "none")

panel_c <- trend_clean %>%
  ggplot(aes(x = trend_class, y = cv, fill = trend_class)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
               linewidth = 0.3, color = "gray30") +
  geom_jitter(width = 0.15, alpha = 0.35, size = 0.8, color = "gray30") +
  geom_ref_threshold(0.3) +
  scale_fill_trend() +
  labs(x = "Trend Type", y = "Coefficient of Variation") +
  theme_ccycling() + theme(legend.position = "none")

panel_d <- trend_clean %>%
  ggplot(aes(x = trend_class, y = year_span, fill = trend_class)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
               linewidth = 0.3, color = "gray30") +
  geom_jitter(width = 0.15, alpha = 0.35, size = 0.8, color = "gray30") +
  scale_fill_trend() +
  labs(x = "Trend Type", y = "Experiment Duration (Years)") +
  theme_ccycling() + theme(legend.position = "none")

final_figure <- (panel_a | panel_b) / (panel_c | panel_d) +
  plot_annotation(tag_levels = "A")

ggsave("figures/Figure_temporal_trends_summary.png",
       final_figure, width = 7.2, height = 8, dpi = 300, bg = "white")
ggsave("figures/Figure_temporal_trends_summary.pdf",
       final_figure, width = 7.2, height = 8, device = cairo_pdf)

# Sign-flip visualization
p_signflips <- sign_flip_analysis %>%
  filter(!is.na(flip_pct)) %>%
  left_join(metadata %>% select(source, site_type), by = "source") %>%
  ggplot(aes(x = site_abbr, y = flip_pct)) +
  geom_boxplot(aes(fill = site_abbr), alpha = 0.4, outlier.shape = NA,
               linewidth = 0.3, color = "gray40") +
  geom_jitter(aes(color = site_abbr), width = 0.15, alpha = 0.5, size = 1) +
  scale_fill_site() +
  scale_color_site() +
  facet_wrap(~site_type, scales = "free_x") +
  labs(x = "Site", y = "Sign Flips (% of Transitions)") +
  theme_ccycling() +
  theme(legend.position = "none")

save_figure("figures/sign_flips_by_site.png", p_signflips, size = "double")

# --- Trend-colored LRR variants (moved here from 02_relative-response.R) ---
# These need the trend classifications computed above, so they belong after
# classification, not in 02 (which would create a circular dependency).

log_summary <- summary_data %>%
  mutate(mean_log = log(mean_response))

log_with_trend <- log_summary %>%
  left_join(trend_clean %>% select(source, Treatment, trend_class),
            by = c("source", "Treatment")) %>%
  filter(!is.na(trend_class))

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
# (source figure for Fig4 via 99_organize-figures.R — save PNG and PDF)
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
ggsave("figures/all_experiments_lrr_pooled_by_trend.pdf",
       combined_A, width = 16, height = 10, device = cairo_pdf)

# Variant B: grey spaghetti + one total pooled smooth colored by dominant trend
dominant_trend <- trend_clean %>%
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

cat("\nAll trend classification plots saved to figures/\n")
cat("=== COMPLETE ===\n")
