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

# Log response ratio version (summary_data already has one row per
# source x Treatment x timepoint, so this is a direct transform)
log_summary <- summary_data %>%
  mutate(mean_log = log(mean_response))

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

# NOTE: The trend-colored LRR variants (including the Fig4 source figure
# all_experiments_lrr_pooled_by_trend) live in 03_trend-classification.R.
# They need trend classifications, which are computed from this script's
# output — keeping them here created a circular dependency where a fresh
# run in numeric order never produced Fig4.

cat("\nPlots saved to figures/\n")
cat("=== COMPLETE ===\n")
