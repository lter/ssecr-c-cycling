# 99_organize-figures.R
# Run after all analysis scripts to copy main figures with numbered names
# and move everything else to figures/archive/
#
# Figure assignments:
#   Fig1  - Conceptual trajectories (hypotheses)
#   Fig2  - Site map and experiment timeline
#   Fig3  - (methodology overview — added manually)
#   Fig4  - All experiments LRR pooled by trend
#   Fig5  - Detection time analysis
#   Fig6  - Early vs full LRR comparison
#   Fig7  - SiZer and sign flips combined
#   FigS1 - Trend characteristics scatter

cat("=== ORGANIZING FIGURES ===\n")

fig_dir <- "figures"
archive_dir <- file.path(fig_dir, "archive")
if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)

# --- Define main figure mappings (source basename -> Fig number) ---
main_figs <- list(
  "Figure_conceptual_trajectories" = "Fig1_conceptual_trajectories",
  "Figure_site_map_timeline"       = "Fig2_site_map_timeline",
  "all_experiments_lrr_pooled_by_trend" = "Fig4_all_experiments_lrr_pooled_by_trend",
  "detection_time_analysis"        = "Fig5_detection_time_analysis",
  "early_vs_full_lrr"              = "Fig6_early_vs_full_lrr",
  "Figure_sizer_and_sign_flips"    = "Fig7_sizer_and_sign_flips",
  "trend_characteristics_scatter"  = "FigS1_trend_characteristics_scatter"
)

# Copy source files to numbered names
for (src_base in names(main_figs)) {
  dst_base <- main_figs[[src_base]]
  for (ext in c("png", "pdf")) {
    src <- file.path(fig_dir, paste0(src_base, ".", ext))
    dst <- file.path(fig_dir, paste0(dst_base, ".", ext))
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      cat(sprintf("  %s -> %s\n", basename(src), basename(dst)))
    }
  }
}

# --- Move non-main files to archive ---
keep_prefixes <- c("Fig1_", "Fig2_", "Fig3_", "Fig4_", "Fig5_",
                    "Fig6_", "Fig7_", "FigS")
keep_other <- c("Table", "manuscript_results.txt")

all_files <- list.files(fig_dir, full.names = FALSE, recursive = FALSE)
# Exclude directories
all_files <- all_files[!file.info(file.path(fig_dir, all_files))$isdir]

moved <- 0
for (f in all_files) {
  # Skip if it's a main figure
  is_main <- any(sapply(keep_prefixes, function(p) startsWith(f, p)))
  is_keep <- any(sapply(keep_other, function(p) startsWith(f, p)))

  if (!is_main && !is_keep) {
    file.rename(file.path(fig_dir, f), file.path(archive_dir, f))
    moved <- moved + 1
  }
}

cat(sprintf("\nKept %d main figure files in figures/\n",
            length(all_files) - moved))
cat(sprintf("Moved %d files to figures/archive/\n", moved))
cat("=== DONE ===\n")
