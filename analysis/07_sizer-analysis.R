# 07_sizer-analysis.R
# SiZer slope-change analysis for all site-treatment combinations
# Detects significant changes in trend direction using scale-space smoothing
#
# Uses:
#   SiZer (CRAN) — Significant Zero crossings of the derivative
#   HERON (lter/HERON) — Helpers for River Observation, wraps SiZer
#
# Input:  data/harmonized/relative_response_summary.csv (treatment/control ratios)
# Output: data/harmonized/sizer_analysis_results.csv
#         figures/supplemental/sizer_map_*.png (SiZer colormaps)
#         figures/supplemental/sizer_ggplot_*.png (slope change visualizations)
#         figures/Figure_sizer_all_sites.png (multi-panel manuscript figure)

library(dplyr)
library(ggplot2)
library(SiZer)
library(HERON)
library(patchwork)

source("R/utils_labels.R")
source("R/utils_plots.R")

cat("=== SIZER SLOPE-CHANGE ANALYSIS ===\n")

# --- Parameters ---
# Bandwidth range for SiZer exploration (log10 scale, in years)
# h = c(2, 10) means SiZer examines smoothing windows from 2 to 10 years
BANDWIDTH_RANGE <- c(2, 10)

# Bandwidth for extracting slope classifications (years)
# bw = 5 detects trends visible at ~5-year smoothing windows
BANDWIDTH_SLICE <- 5

# Minimum timepoints required for SiZer analysis
MIN_TIMEPOINTS <- 5

# --- Read Data ---

summary_data <- read.csv("data/harmonized/relative_response_summary.csv",
                         stringsAsFactors = FALSE)
summary_data$Date_parsed <- as.Date(summary_data$Date_parsed)
metadata <- get_site_metadata()

cat("Loaded", nrow(summary_data), "summary observations\n")

# Filter to treatments with enough timepoints
valid_data <- summary_data %>%
  group_by(source, Treatment) %>%
  filter(n() >= MIN_TIMEPOINTS) %>%
  ungroup()

# Get unique source-treatment combinations
combos <- valid_data %>%
  distinct(source, Treatment, site_abbr)

cat("Running SiZer on", nrow(combos), "site-treatment combinations",
    "(min", MIN_TIMEPOINTS, "timepoints)\n")
cat("Skipping", nrow(distinct(summary_data, source, Treatment)) - nrow(combos),
    "combinations with insufficient data\n\n")

# Ensure output directories exist
if (!dir.exists("figures/supplemental")) dir.create("figures/supplemental", recursive = TRUE)

# --- Run SiZer for Each Combination ---

sizer_results <- list()
sizer_plots <- list()
skipped <- character(0)

for (i in seq_len(nrow(combos))) {
  src <- combos$source[i]
  trt <- combos$Treatment[i]
  abbr <- combos$site_abbr[i]

  # Create safe label for filenames (replace spaces and special chars)
  label <- paste0(abbr, "_", gsub("[^A-Za-z0-9_]", "", gsub("\\s+", "_", trt)))

  cat(sprintf("  [%d/%d] %s — %s ... ", i, nrow(combos), abbr, trt))

  data_sub <- valid_data %>%
    filter(source == src, Treatment == trt) %>%
    arrange(Date_parsed)

  # Convert date to numeric year for SiZer
  data_sub$year_numeric <- as.numeric(format(data_sub$Date_parsed, "%Y")) +
    as.numeric(format(data_sub$Date_parsed, "%j")) / 365.25

  tryCatch({
    # 1. Fit SiZer model
    sizer_obj <- SiZer::SiZer(
      x = data_sub$year_numeric,
      y = data_sub$mean_response,
      h = BANDWIDTH_RANGE,
      degree = 1, derv = 1, grid.length = 100
    )

    # 2. Extract slope classifications at chosen bandwidth
    sizer_info <- HERON::sizer_slice(
      sizer_object = sizer_obj,
      bandwidth = BANDWIDTH_SLICE
    )

    # 3. Identify slope change regions in the data
    place_info <- HERON::id_slope_changes(
      raw_data = data_sub,
      sizer_data = sizer_info,
      x = "year_numeric",
      y = "mean_response",
      group_dig = 3
    )

    # 4. Fit linear models to each segment
    seg_lm <- HERON::sizer_lm(
      data = place_info,
      x = "year_numeric",
      y = "mean_response",
      group_col = "groups"
    )

    # Count slope changes
    n_groups <- length(unique(place_info$groups))
    n_changes <- max(0, n_groups - 1)

    # Extract change years (boundaries between groups)
    change_years <- NA_character_
    if (n_changes > 0) {
      group_bounds <- place_info %>%
        group_by(groups) %>%
        summarise(end_year = max(year_numeric), .groups = "drop") %>%
        arrange(end_year)
      # Change points are at the end of each group except the last
      if (nrow(group_bounds) > 1) {
        change_yr_vals <- round(group_bounds$end_year[-nrow(group_bounds)], 1)
        change_years <- paste(change_yr_vals, collapse = "; ")
      }
    }

    # Extract segment slopes and p-values from sizer_lm output
    # sizer_lm returns list: [[1]] = Stat (model summaries), [[2]] = Estim (coefficients)
    # Estim has columns: section, term, estimate, std.error, statistic, p.value
    # Slope rows have term == "data[[x]]"; intercept rows have term == "(Intercept)"
    seg_slopes <- NA_character_
    seg_pvals <- NA_character_
    if (is.list(seg_lm) && length(seg_lm) >= 2) {
      coefs <- seg_lm[[2]]
      if (!is.null(coefs) && "estimate" %in% names(coefs)) {
        slope_rows <- coefs[coefs$term != "(Intercept)", ]
        if (nrow(slope_rows) > 0) {
          seg_slopes <- paste(round(slope_rows$estimate, 6), collapse = "; ")
          seg_pvals <- paste(round(slope_rows$p.value, 4), collapse = "; ")
        }
      }
      # Also get model-level p-values from Stat
      stats <- seg_lm[[1]]
      if (!is.null(stats) && "p.value" %in% names(stats)) {
        seg_pvals <- paste(round(stats$p.value, 4), collapse = "; ")
      }
    }

    # Store results
    sizer_results[[label]] <- data.frame(
      site_abbr = abbr,
      source = src,
      treatment = trt,
      n_timepoints = nrow(data_sub),
      n_slope_changes = n_changes,
      change_years = change_years,
      segment_slopes = seg_slopes,
      segment_p_values = seg_pvals,
      bandwidth = BANDWIDTH_SLICE,
      stringsAsFactors = FALSE
    )

    # 5. Build publication-quality ggplot (replaces HERON::sizer_ggplot)
    p <- ggplot(place_info, aes(x = year_numeric, y = mean_response)) +
      geom_ref_ratio() +
      geom_ribbon(aes(ymin = mean_response - se_response,
                      ymax = mean_response + se_response),
                  fill = "gray80", alpha = 0.3) +
      geom_smooth(aes(group = groups, color = slope_type),
                  method = "lm", formula = y ~ x, se = FALSE,
                  linewidth = 0.7) +
      geom_point(size = 1.2, alpha = 0.7, color = "gray25") +
      scale_color_manual(
        values = c("approx. zero" = "#B0B0B0",
                   "increasing/decreasing" = "#3A7CA5"),
        labels = c("approx. zero" = "Flat",
                   "increasing/decreasing" = "Significant"),
        name = "Slope"
      ) +
      labs(title = paste0(abbr, " \u2014 ", trt),
           x = "Year", y = "Treatment / Control Ratio") +
      theme_ccycling() +
      theme(legend.position = "none")

    # Add subtle slope-change boundary lines
    if (n_changes > 0) {
      p <- p + geom_vline(xintercept = change_yr_vals,
                          linetype = "dotted", linewidth = 0.3,
                          color = "gray45")
    }

    sizer_plots[[label]] <- p

    # 6. Save individual SiZer colormap (base R; tighten margins)
    png(file.path("figures", "supplemental", paste0("sizer_map_", label, ".png")),
        width = 3.5, height = 3.5, res = 300, units = "in")
    par(mar = c(4, 4, 2, 1), cex.main = 0.8, cex.axis = 0.7, cex.lab = 0.8)
    HERON::sizer_plot(sizer_object = sizer_obj, bandwidth_vec = BANDWIDTH_SLICE)
    title(main = paste0(abbr, " \u2014 ", trt), cex.main = 0.8)
    dev.off()

    cat(n_changes, "slope change(s)\n")

  }, error = function(e) {
    cat("SKIPPED —", e$message, "\n")
    skipped <<- c(skipped, label)
  })
}

# --- Save Individual ggplots ---

cat("\nSaving", length(sizer_plots), "ggplots...\n")

for (label in names(sizer_plots)) {
  save_figure(
    file.path("figures", "supplemental", paste0("sizer_ggplot_", label, ".png")),
    sizer_plots[[label]], size = "double"
  )
}

# --- Summary Table ---

if (length(sizer_results) > 0) {
  sizer_summary <- bind_rows(sizer_results)

  write.csv(sizer_summary,
            "data/harmonized/sizer_analysis_results.csv",
            row.names = FALSE)

  cat("\n--- SIZER SUMMARY ---\n")
  cat("Analyzed:", nrow(sizer_summary), "combinations\n")
  cat("Skipped:", length(skipped), "combinations\n")
  cat("\nSlope changes distribution:\n")
  change_dist <- table(sizer_summary$n_slope_changes)
  for (j in seq_along(change_dist)) {
    cat(sprintf("  %s change(s): %d combinations\n",
                names(change_dist)[j], change_dist[j]))
  }

  cat(sprintf("\nMean slope changes per treatment: %.1f\n",
              mean(sizer_summary$n_slope_changes)))
  cat(sprintf("Treatments with at least 1 change: %d (%.0f%%)\n",
              sum(sizer_summary$n_slope_changes > 0),
              100 * mean(sizer_summary$n_slope_changes > 0)))
} else {
  cat("\nWARNING: No SiZer results produced.\n")
}

# --- All-Sites Summary Figure ---

# Select one representative treatment per site
# Priority: the treatment with the most timepoints
site_labels <- sizer_summary %>%
  group_by(site_abbr) %>%
  slice_max(n_timepoints, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label = paste0(site_abbr, "_",
                        gsub("[^A-Za-z0-9_]", "", gsub("\\s+", "_", treatment))))

all_panels <- list()
for (j in seq_len(nrow(site_labels))) {
  lbl <- site_labels$label[j]
  site <- site_labels$site_abbr[j]
  if (lbl %in% names(sizer_plots)) {
    all_panels[[site]] <- sizer_plots[[lbl]] +
      ggtitle(site) +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 11))
  }
}

if (length(all_panels) >= 4) {
  # Arrange in 4-column grid (4x4 layout)
  n_panels <- length(all_panels)
  n_cols <- 4
  n_rows <- ceiling(n_panels / n_cols)

  # Pad with blank plots to fill the grid if needed
  while (length(all_panels) < n_rows * n_cols) {
    all_panels[[length(all_panels) + 1]] <- ggplot() + theme_void()
  }

  summary_figure <- wrap_plots(all_panels[1:(n_rows * n_cols)], ncol = n_cols)

  fig_width <- 12
  fig_height <- n_rows * 2.8
  ggsave("figures/Figure_sizer_all_sites.png",
         summary_figure, width = fig_width, height = fig_height,
         dpi = 300, bg = "white")
  ggsave("figures/Figure_sizer_all_sites.pdf",
         summary_figure, width = fig_width, height = fig_height,
         device = cairo_pdf)

  cat("\nSummary figure saved: figures/Figure_sizer_all_sites.png\n")
}

if (length(skipped) > 0) {
  cat("\nSkipped combinations:\n")
  for (s in skipped) cat("  -", s, "\n")
}

# --- Combined Figure: SiZer + Sign Flips ---

sign_flip_file <- "data/harmonized/sign_flip_analysis.csv"
if (file.exists(sign_flip_file) && length(all_panels) >= 4) {
  sign_flip_analysis <- read.csv(sign_flip_file, stringsAsFactors = FALSE)
  metadata <- get_site_metadata()

  # Ecosystem type colors (one color per type, matching the site palette families)
  ecosystem_colors <- c(
    "Grassland"  = "#CC7A29",
    "Forest"     = "#3C8556",
    "Tundra"     = "#4A7BA7",
    "Coastal"    = "#1B7A7D",
    "Urban"      = "#8E6FAD",
    "Freshwater" = "#5AAECC"
  )

  flip_data <- sign_flip_analysis %>%
    filter(!is.na(n_flips)) %>%
    left_join(metadata %>% select(source, site_type), by = "source") %>%
    mutate(site_abbr = factor(site_abbr, levels = sort(unique(site_abbr), decreasing = TRUE)))

  p_signflips <- ggplot(flip_data, aes(y = site_abbr, x = n_flips)) +
    geom_boxplot(aes(fill = site_type), alpha = 0.4, outlier.shape = NA,
                 linewidth = 0.3, color = "gray40") +
    geom_jitter(aes(color = site_type), height = 0.15, alpha = 0.5, size = 1.5) +
    scale_fill_manual(values = ecosystem_colors, name = "Ecosystem") +
    scale_color_manual(values = ecosystem_colors, name = "Ecosystem") +
    labs(y = NULL, x = "Number of Sign Flips") +
    theme_ccycling(base_size = 14) +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13)) +
    guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))

  # Re-enable SiZer slope legend on one representative panel
  # (patchwork collect will de-duplicate)
  last_real <- max(which(sapply(all_panels[1:(n_rows * n_cols)], function(p)
    inherits(p, "gg") && !inherits(p, "patchwork"))))
  all_panels_combo <- all_panels[1:(n_rows * n_cols)]
  all_panels_combo[[last_real]] <- all_panels_combo[[last_real]] +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11))

  sizer_left <- wrap_plots(all_panels_combo, ncol = n_cols, guides = "collect") +
    plot_annotation(theme = theme(legend.position = "bottom"))

  # Panel label "A" as a standalone text plot, left-aligned
  label_a <- ggplot() +
    annotate("text", x = 0, y = 0, label = "A", size = 7, fontface = "bold", hjust = 0) +
    scale_x_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 5))

  label_b <- ggplot() +
    annotate("text", x = 0, y = 0, label = "B", size = 7, fontface = "bold", hjust = 0) +
    scale_x_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 5))

  left_col <- (label_a / sizer_left) + plot_layout(heights = c(1, 40))
  right_col <- (label_b / p_signflips) + plot_layout(heights = c(1, 40))

  # Vertical separator between panels A and B
  separator <- ggplot() +
    geom_vline(xintercept = 0.5, color = "gray60", linewidth = 0.4) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  combined_sizer_flips <- wrap_plots(list(left_col, separator, right_col),
                                      ncol = 3, widths = c(2, 0.02, 1))

  combo_w <- fig_width * 1.5
  combo_h <- fig_height + 1
  ggsave("figures/Figure_sizer_and_sign_flips.png",
         combined_sizer_flips, width = combo_w, height = combo_h,
         dpi = 300, bg = "white")
  ggsave("figures/Figure_sizer_and_sign_flips.pdf",
         combined_sizer_flips, width = combo_w, height = combo_h,
         device = cairo_pdf)

  cat("Combined figure saved: figures/Figure_sizer_and_sign_flips.png\n")
}

cat("\nAll SiZer outputs saved to figures/supplemental/\n")
cat("Results table: data/harmonized/sizer_analysis_results.csv\n")
cat("=== COMPLETE ===\n")
