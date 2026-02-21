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
#         figures/Figure_sizer_case_studies.png (multi-panel manuscript figure)

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

# Case-study sites for manuscript figure
CASE_STUDY_SITES <- c("KNZ", "HBR", "KBS", "SBC", "HFR", "BNZ", "CAP")

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

    # 5. Generate ggplot with slope change overlays
    p <- HERON::sizer_ggplot(
      raw_data = place_info,
      sizer_data = sizer_info,
      x = "year_numeric", y = "mean_response",
      trendline = "sharp", vline = "changes",
      sharp_colors = c("#bbbbbb", "#2ca02c")
    ) +
      ggtitle(paste0(abbr, " — ", trt)) +
      labs(x = "Year", y = "Treatment / Control Ratio") +
      geom_hline(yintercept = 1, linetype = "dashed",
                 color = "gray50", linewidth = 0.3) +
      theme_ccycling()

    sizer_plots[[label]] <- p

    # 6. Save individual SiZer colormap
    png(file.path("figures", "supplemental", paste0("sizer_map_", label, ".png")),
        width = 5, height = 5, res = 300, units = "in")
    HERON::sizer_plot(sizer_object = sizer_obj, bandwidth_vec = BANDWIDTH_SLICE)
    title(main = paste0(abbr, " — ", trt), cex.main = 0.9)
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
  ggsave(
    file.path("figures", "supplemental", paste0("sizer_ggplot_", label, ".png")),
    sizer_plots[[label]],
    width = 7, height = 5, dpi = 300, bg = "white"
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

# --- Case-Study Summary Figure ---

# Select one representative treatment per case-study site
# Priority: the treatment with the most timepoints
case_study_labels <- sizer_summary %>%
  filter(site_abbr %in% CASE_STUDY_SITES) %>%
  group_by(site_abbr) %>%
  slice_max(n_timepoints, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label = paste0(site_abbr, "_",
                        gsub("[^A-Za-z0-9_]", "", gsub("\\s+", "_", treatment))))

case_panels <- list()
for (j in seq_len(nrow(case_study_labels))) {
  lbl <- case_study_labels$label[j]
  if (lbl %in% names(sizer_plots)) {
    case_panels[[case_study_labels$site_abbr[j]]] <- sizer_plots[[lbl]] +
      theme(plot.title = element_text(size = 8, face = "bold"))
  }
}

if (length(case_panels) >= 4) {
  # Arrange in 2-column grid
  n_panels <- length(case_panels)
  n_rows <- ceiling(n_panels / 2)

  case_figure <- wrap_plots(case_panels, ncol = 2) +
    plot_annotation(
      title = "SiZer Slope-Change Analysis: Case-Study Sites",
      subtitle = paste0("Bandwidth = ", BANDWIDTH_SLICE,
                        " years | Green = significant slope; Gray = flat"),
      tag_levels = "A"
    )

  ggsave("figures/Figure_sizer_case_studies.png",
         case_figure, width = 7.2, height = n_rows * 3.5,
         dpi = 300, bg = "white")
  ggsave("figures/Figure_sizer_case_studies.pdf",
         case_figure, width = 7.2, height = n_rows * 3.5,
         device = cairo_pdf)

  cat("\nCase-study figure saved: figures/Figure_sizer_case_studies.png\n")
}

if (length(skipped) > 0) {
  cat("\nSkipped combinations:\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nAll SiZer outputs saved to figures/supplemental/\n")
cat("Results table: data/harmonized/sizer_analysis_results.csv\n")
cat("=== COMPLETE ===\n")
