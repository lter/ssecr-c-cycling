# 05_lrr-analysis.R
# Log-Response Ratio analysis:
# 1. Standard LRR calculation with CIs
# 2. First 3 years vs whole duration comparison
# 3. Moving 3-year window LRR

library(dplyr)
library(ggplot2)
library(patchwork)

source("R/utils_labels.R")
source("R/utils_dates.R")
source("R/utils_analysis.R")
source("R/utils_plots.R")

cat("=== LOG-RESPONSE RATIO ANALYSIS ===\n")

# Read harmonized data and prepare summary for LRR
df <- read.csv("data/harmonized/harmonized_current.csv", stringsAsFactors = FALSE)
metadata <- get_site_metadata()

# Calculate means and SDs per source/date/treatment (needed for LRR with variance)
mean_sd <- df %>%
  filter(!is.na(Response.Variable), !is.na(Treatment)) %>%
  group_by(source, Date, Treatment) %>%
  summarise(
    means = mean(Response.Variable, na.rm = TRUE),
    response_sd = sd(Response.Variable, na.rm = TRUE),
    sample_size = n(),
    .groups = "drop"
  ) %>%
  mutate(response_sd = ifelse(is.na(response_sd), 0, response_sd))

cat("LRR input prepared:", nrow(mean_sd), "source-date-treatment combinations\n")

# Save intermediate file
write.csv(mean_sd, "data/harmonized/lrr_master_file.csv", row.names = FALSE)

# --- Standard LRR Calculation ---
lrr_results <- calculate_lrr(mean_sd, time_col = "Date")
lrr_results$site_abbr <- source_to_abbr(lrr_results$source, metadata)

cat("LRR calculated:", nrow(lrr_results), "treatment-control comparisons\n")
write.csv(lrr_results, "data/harmonized/lrr_results.csv", row.names = FALSE)

# --- First 3 Years vs Whole Duration ---

cat("\n--- EARLY vs FULL LRR COMPARISON ---\n")

lrr_results$year <- extract_year(lrr_results$Date)

# "Early" = the first three CALENDAR years of each treatment's record (not the
# first three sampled timepoints — for irregularly sampled series such as AND's
# three censuses, ranking timepoints would make the early window the whole
# record). A treatment enters the comparison if it is one of the classified
# treatments (>= 3 timepoints) and has at least one observation inside the
# early window and at least one after it.
early_vs_full <- lrr_results %>%
  filter(!is.na(year), is.finite(log_response_ratio)) %>%
  group_by(source, Treatment, site_abbr) %>%
  arrange(year) %>%
  mutate(early = year <= min(year) + 2) %>%
  summarise(
    early_lrr = mean(log_response_ratio[early], na.rm = TRUE),
    full_lrr = mean(log_response_ratio, na.rm = TRUE),
    lrr_change = full_lrr - early_lrr,
    n_early = sum(early),
    n_later = sum(!early),
    n_total = n(),
    n_years = n_distinct(year),
    year_span = max(year) - min(year),
    .groups = "drop"
  ) %>%
  filter(n_total >= 3, n_early >= 1, n_later >= 1)

cat("Early vs full comparison:", nrow(early_vs_full), "treatment-experiments\n")
write.csv(early_vs_full, "data/harmonized/early_vs_full_lrr.csv", row.names = FALSE)

# Plot: Early LRR vs Full LRR
if (nrow(early_vs_full) > 0) {
  p_early_full <- ggplot(early_vs_full,
                         aes(x = early_lrr, y = full_lrr,
                             color = site_abbr, shape = site_abbr)) +
    geom_ref_diagonal() +
    geom_point(size = 1.8, alpha = 0.7) +
    scale_color_site() +
    scale_shape_site() +
    labs(x = "Mean LRR (First 3 Years)",
         y = "Mean LRR (Full Duration)",
         color = "Site", shape = "Site") +
    theme_ccycling() +
    guides(color = guide_legend(ncol = 6),
           shape = guide_legend(ncol = 6))

  save_figure("figures/early_vs_full_lrr.png", p_early_full, size = "double")
}

# --- Moving 3-Year Window LRR ---

cat("\n--- MOVING WINDOW LRR ---\n")

moving_window_lrr <- lrr_results %>%
  filter(!is.na(year)) %>%
  group_by(source, Treatment, site_abbr) %>%
  arrange(year) %>%
  group_modify(function(data, keys) {
    years <- sort(unique(data$year))
    if (length(years) < 3) return(tibble())

    results <- list()
    for (i in 1:(length(years) - 2)) {
      window_years <- years[i:(i + 2)]
      window_data <- data %>% filter(year %in% window_years)
      if (nrow(window_data) == 0) next
      results[[length(results) + 1]] <- tibble(
        window_start = min(window_years),
        window_end = max(window_years),
        window_mid = mean(window_years),
        window_lrr = mean(window_data$log_response_ratio, na.rm = TRUE),
        window_se = sd(window_data$log_response_ratio, na.rm = TRUE) / sqrt(nrow(window_data)),
        window_n = nrow(window_data)
      )
    }
    bind_rows(results)
  }) %>%
  ungroup()

cat("Moving window results:", nrow(moving_window_lrr), "windows\n")
write.csv(moving_window_lrr, "data/harmonized/moving_window_lrr.csv", row.names = FALSE)

# Plot: Moving window for all sites (spaghetti with mean)
if (nrow(moving_window_lrr) > 0) {
  n_sites <- length(unique(moving_window_lrr$site_abbr))
  n_cols <- 4
  n_rows <- ceiling(n_sites / n_cols)

  p_window <- ggplot(moving_window_lrr,
                     aes(x = window_mid, y = window_lrr)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "gray55") +
    geom_line(aes(group = Treatment), alpha = 0.15, linewidth = 0.3, color = "gray50") +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.12,
                linewidth = 0.5, color = "#3A7CA5", fill = "#3A7CA5") +
    facet_wrap(~site_abbr, scales = "free", ncol = n_cols) +
    labs(title = "Moving 3-Year Window Log-Response Ratio",
         subtitle = "Gray lines: individual treatments; blue: mean trend",
         x = "Window Midpoint (Year)",
         y = "Mean LRR") +
    theme_ccycling() +
    theme(strip.text = element_text(size = 7))

  ggsave("figures/moving_window_lrr_all_sites.png", p_window,
         width = 10, height = n_rows * 2.5, dpi = 300, bg = "white")
}

# Focused plot for KNZ (best-sampled site)
knz_window <- moving_window_lrr %>% filter(site_abbr == "KNZ")
if (nrow(knz_window) > 0) {
  knz_trts <- sort(unique(knz_window$Treatment))
  knz_pal <- generate_treatment_palette(length(knz_trts), knz_trts)

  p_knz <- ggplot(knz_window,
                  aes(x = window_mid, y = window_lrr,
                      color = Treatment, fill = Treatment)) +
    geom_ribbon(aes(ymin = window_lrr - window_se,
                    ymax = window_lrr + window_se),
                alpha = 0.06, color = NA) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "gray55") +
    scale_color_manual(values = knz_pal) +
    scale_fill_manual(values = knz_pal) +
    labs(title = "Moving 3-Year Window LRR: Konza Prairie (KNZ)",
         subtitle = "Fertilization experiment; 0 = no treatment effect",
         x = "Window Midpoint (Year)", y = "Mean Log-Response Ratio",
         color = "Treatment", fill = "Treatment") +
    theme_ccycling() +
    guides(color = guide_legend(ncol = 3),
           fill = guide_legend(ncol = 3))

  save_figure("figures/moving_window_lrr_KNZ.png", p_knz, size = "double")
}

# LRR forest plot with CI bars
lrr_summary <- lrr_results %>%
  group_by(source, Treatment, site_abbr) %>%
  summarise(
    mean_lrr = mean(log_response_ratio, na.rm = TRUE),
    se_lrr = sd(log_response_ratio, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_forest <- ggplot(lrr_summary, aes(x = mean_lrr, y = site_abbr)) +
  geom_ref_lrr() +
  geom_errorbarh(aes(xmin = mean_lrr - 1.96 * se_lrr,
                     xmax = mean_lrr + 1.96 * se_lrr),
                 height = 0, linewidth = 0.2, color = "gray50") +
  geom_point(aes(color = site_abbr, shape = site_abbr),
             size = 1.5, alpha = 0.7,
             position = position_jitter(height = 0.15)) +
  scale_color_site() +
  scale_shape_site() +
  labs(title = "Log-Response Ratios by Site",
       subtitle = "Points show treatment means with 95% CI; dashed line = no effect",
       x = "Log-Response Ratio", y = "Site") +
  theme_ccycling() +
  theme(legend.position = "none")

save_figure("figures/lrr_forest_plot.png", p_forest, size = "double")

# --- Faceted LRR with Trend-Colored Smooth Fits ---
# Shows all experiments in a faceted layout; loess fits colored by trend class

cat("\n--- TREND-COLORED LRR FACETED FIGURE ---\n")

trend_analysis <- read.csv("data/harmonized/trend_analysis_results.csv",
                           stringsAsFactors = FALSE)
# Filter insufficient_data while trend_class is still character, THEN convert
# to factor (as_trend_factor has only the 4 analyzable levels)
trend_analysis <- trend_analysis %>%
  filter(trend_class != "insufficient_data")
trend_analysis$trend_class <- as_trend_factor(trend_analysis$trend_class)

# Read the relative response summary and join trend classification
summary_data <- read.csv("data/harmonized/relative_response_summary.csv",
                         stringsAsFactors = FALSE)
summary_data$Date_parsed <- as.Date(summary_data$Date_parsed)

summary_with_trend <- summary_data %>%
  left_join(trend_analysis %>% select(source, Treatment, trend_class),
            by = c("source", "Treatment")) %>%
  filter(!is.na(trend_class)) %>%               # drop insufficient-data treatments
  mutate(log_rr   = log(mean_response),
         se_log   = se_response / mean_response)  # delta-method SE on log scale

p_lrr_faceted <- ggplot(summary_with_trend, aes(x = Date_parsed, y = log_rr)) +
  geom_errorbar(aes(ymin = log_rr - se_log, ymax = log_rr + se_log),
                width = 0, alpha = 0.2, color = "grey50") +
  geom_point(aes(color = trend_class), size = 0.8, alpha = 0.5) +
  geom_smooth(aes(group = Treatment, color = trend_class, fill = trend_class),
              method = "loess", se = TRUE, alpha = 0.08, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "gray55") +
  scale_color_trend(name = "Trend") +
  scale_fill_trend(name = "Trend") +
  facet_wrap(~site_abbr, scales = "free") +
  labs(title = "Log Response Ratio Across All Sites",
       subtitle = "Smooth fits colored by temporal trend classification; 0 = no effect",
       x = "Date", y = "Log Response Ratio  ln(Treatment / Control)") +
  theme_ccycling() +
  theme(strip.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

n_sites <- length(unique(summary_with_trend$site_abbr))
n_facet_rows <- ceiling(n_sites / 4)

ggsave("figures/all_experiments_lrr_trend_colored.png",
       p_lrr_faceted, width = 10, height = n_facet_rows * 2.5,
       dpi = 300, bg = "white")
ggsave("figures/all_experiments_lrr_trend_colored.pdf",
       p_lrr_faceted, width = 10, height = n_facet_rows * 2.5,
       device = cairo_pdf)

cat("Saved: figures/all_experiments_lrr_trend_colored.png\n")

# --- Combined Layout: LRR Faceted + Temporal Trends Summary ---

cat("--- COMBINED FIGURE ---\n")

# Rebuild compact summary panels (mirrors 03_trend-classification.R panels)
trend_clean <- trend_analysis  # insufficient_data already filtered above

overall_proportions <- trend_clean %>%
  count(trend_class, .drop = FALSE) %>%
  mutate(proportion = n / sum(n),
         percentage = round(proportion * 100, 1))

pa <- overall_proportions %>%
  ggplot(aes(x = "", y = n, fill = trend_class)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5,
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = n),
            position = position_fill(vjust = 0.5), size = 2, fontface = "bold") +
  scale_fill_trend() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(y = "Proportion", x = "", fill = "Trend") +
  theme_ccycling(base_size = 7) +
  theme(legend.position = "right",
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

pb <- trend_clean %>%
  ggplot(aes(x = trend_class, y = mean_ratio, fill = trend_class)) +
  geom_ref_ratio() +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
               linewidth = 0.3, color = "gray30") +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.6, color = "gray30") +
  scale_fill_trend() +
  scale_y_continuous(trans = "log2", breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 4)) +
  labs(x = "", y = "Trt/Ctrl (log scale)") +
  theme_ccycling(base_size = 7) + theme(legend.position = "none")

pc <- trend_clean %>%
  ggplot(aes(x = trend_class, y = cv, fill = trend_class)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
               linewidth = 0.3, color = "gray30") +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.6, color = "gray30") +
  geom_ref_threshold(0.3) +
  scale_fill_trend() +
  labs(x = "", y = "CV") +
  theme_ccycling(base_size = 7) + theme(legend.position = "none")

pd <- trend_clean %>%
  ggplot(aes(x = trend_class, y = year_span, fill = trend_class)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, alpha = 0.5,
               linewidth = 0.3, color = "gray30") +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.6, color = "gray30") +
  scale_fill_trend() +
  labs(x = "", y = "Duration (years)") +
  theme_ccycling(base_size = 7) + theme(legend.position = "none")

summary_panels <- (pa | pb) / (pc | pd)

combined_figure <- p_lrr_faceted /
  summary_panels +
  plot_layout(heights = c(3, 2))

combined_h <- n_facet_rows * 2.5 + 6
ggsave("figures/Figure_combined_lrr_and_summary.png",
       combined_figure, width = 10, height = combined_h,
       dpi = 300, bg = "white")
ggsave("figures/Figure_combined_lrr_and_summary.pdf",
       combined_figure, width = 10, height = combined_h,
       device = cairo_pdf)

cat("Saved: figures/Figure_combined_lrr_and_summary.png\n")

cat("\n=== COMPLETE ===\n")
