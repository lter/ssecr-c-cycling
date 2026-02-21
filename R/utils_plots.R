# utils_plots.R
# Publication-quality visual system for the C-Cycling project
# Nature/Science style: minimal, clean, muted colorblind-safe palettes

library(ggplot2)

# =============================================================================
# 1. TREND COLORS & LEVELS
# =============================================================================

TREND_COLORS <- c(
  "stable"     = "#5B9E6F",
  "variable"   = "#E8A838",
  "increasing" = "#3A7CA5",
  "decreasing" = "#D4726A"
)

TREND_LEVELS <- c("stable", "variable", "increasing", "decreasing")

as_trend_factor <- function(x) {
  factor(x, levels = TREND_LEVELS)
}

scale_fill_trend <- function(...) {
  scale_fill_manual(values = TREND_COLORS, drop = TRUE, ...)
}

scale_color_trend <- function(...) {
  scale_color_manual(values = TREND_COLORS, drop = TRUE, ...)
}

# =============================================================================
# 2. SITE PALETTE (17 LTER sites, grouped by ecosystem type)
# =============================================================================

SITE_COLORS <- c(
  # Grassland (warm tones)
  "CDR" = "#CC7A29", "KBS" = "#E8A838", "KNZ" = "#8B6914", "SEV" = "#D4A06A",
  # Forest (greens)
  "AND" = "#2D6A2D", "HBR" = "#5B9E6F", "HFR" = "#3C8556", "LUQ" = "#7FB069",
  # Tundra (cool blues)
  "ARC" = "#4A7BA7", "BNZ" = "#6B98C4", "MCM" = "#8BB5D9", "NWT" = "#3A5F8A",
  # Coastal (teals)
  "GCE" = "#1B7A7D", "PIE" = "#4CA3A3", "SBC" = "#2D8E8E", "VCR" = "#6BBFBF",
  # Urban (purple)
  "CAP" = "#8E6FAD",
  # Freshwater (cyan)
  "NTL" = "#5AAECC"
)

SITE_SHAPES <- c(
  "CDR" = 16, "KBS" = 17, "KNZ" = 15, "SEV" = 18,
  "AND" = 1,  "HBR" = 2,  "HFR" = 0,  "LUQ" = 5,
  "ARC" = 16, "BNZ" = 17, "MCM" = 15, "NWT" = 18,
  "GCE" = 1,  "PIE" = 2,  "SBC" = 0,  "VCR" = 5,
  "CAP" = 8,  "NTL" = 3
)

scale_color_site <- function(sites = NULL, ...) {
  pal <- if (!is.null(sites)) SITE_COLORS[sites] else SITE_COLORS
  scale_color_manual(values = pal, ...)
}

scale_fill_site <- function(sites = NULL, ...) {
  pal <- if (!is.null(sites)) SITE_COLORS[sites] else SITE_COLORS
  scale_fill_manual(values = pal, ...)
}

scale_shape_site <- function(sites = NULL, ...) {
  shp <- if (!is.null(sites)) SITE_SHAPES[sites] else SITE_SHAPES
  scale_shape_manual(values = shp, ...)
}

# =============================================================================
# 3. TREATMENT PALETTE (adaptive, colorblind-safe)
# =============================================================================

generate_treatment_palette <- function(n, treatment_names = NULL) {
  if (n <= 5) {
    pal <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00")[1:n]
  } else if (n <= 8) {
    pal <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
             "#E69F00", "#56B4E9", "#F0E442", "#000000")[1:n]
  } else {
    pal <- viridisLite::viridis(n, option = "D", begin = 0.05, end = 0.92)
  }
  if (!is.null(treatment_names) && length(treatment_names) == n) {
    names(pal) <- treatment_names
  }
  pal
}

# =============================================================================
# 4. THEME
# =============================================================================

theme_ccycling <- function(base_size = 8, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title          = element_text(face = "bold", size = base_size + 1,
                                         hjust = 0, margin = margin(b = 4)),
      plot.subtitle       = element_text(size = base_size, color = "gray25",
                                         hjust = 0, margin = margin(b = 6)),
      plot.caption        = element_text(size = base_size - 2, color = "gray40",
                                         hjust = 1),
      plot.title.position = "plot",
      axis.title       = element_text(size = base_size, color = "gray10"),
      axis.text        = element_text(size = base_size - 1, color = "gray20"),
      axis.text.x      = element_text(angle = 0, hjust = 0.5),
      axis.line        = element_line(linewidth = 0.3, color = "gray30"),
      axis.ticks       = element_line(linewidth = 0.25, color = "gray30"),
      axis.ticks.length = unit(2, "pt"),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      panel.border     = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = base_size,
                                      color = "gray10",
                                      margin = margin(b = 3, t = 3)),
      legend.position    = "bottom",
      legend.title       = element_text(size = base_size, face = "bold"),
      legend.text        = element_text(size = base_size - 1),
      legend.key         = element_rect(fill = NA, color = NA),
      legend.key.size    = unit(12, "pt"),
      legend.background  = element_rect(fill = NA, color = NA),
      legend.margin      = margin(t = 2),
      plot.margin   = margin(t = 8, r = 8, b = 8, l = 8),
      panel.spacing = unit(10, "pt")
    )
}

rotate_x_labels <- function(angle = 30) {
  theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1))
}

# =============================================================================
# 5. REFERENCE LINE HELPERS
# =============================================================================

geom_ref_ratio <- function(intercept = 1, ...) {
  geom_hline(yintercept = intercept, linetype = "dashed",
             linewidth = 0.3, color = "gray55", ...)
}

geom_ref_lrr <- function(intercept = 0, ...) {
  geom_vline(xintercept = intercept, linetype = "dashed",
             linewidth = 0.3, color = "gray55", ...)
}

geom_ref_threshold <- function(intercept, direction = "h", ...) {
  if (direction == "h") {
    geom_hline(yintercept = intercept, linetype = "dotted",
               linewidth = 0.3, color = "gray45", ...)
  } else {
    geom_vline(xintercept = intercept, linetype = "dotted",
               linewidth = 0.3, color = "gray45", ...)
  }
}

geom_ref_diagonal <- function(...) {
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              linewidth = 0.3, color = "gray55", ...)
}

# =============================================================================
# 6. HEATMAP SCALE
# =============================================================================

scale_fill_heatmap <- function(midpoint = 0.25, ...) {
  scale_fill_gradient2(low = "#F7F7F7", mid = "#B8D4E3", high = "#2166AC",
                       midpoint = midpoint, ...)
}

# =============================================================================
# 7. FIGURE DIMENSIONS & SAVE HELPER
# =============================================================================

FIG_SINGLE_COL  <- list(width = 3.5,  height = 3.0)
FIG_ONE_HALF    <- list(width = 5.5,  height = 4.0)
FIG_DOUBLE_COL  <- list(width = 7.2,  height = 5.0)
FIG_MULTI_PANEL <- list(width = 7.2,  height = 8.0)
FIG_DPI <- 300

save_figure <- function(filename, plot, size = "double", dpi = FIG_DPI, ...) {
  dims <- switch(size,
    "single"  = FIG_SINGLE_COL,
    "onehalf" = FIG_ONE_HALF,
    "double"  = FIG_DOUBLE_COL,
    "multi"   = FIG_MULTI_PANEL,
    FIG_DOUBLE_COL
  )
  # Sanitize filename: replace characters that cause issues in PDF device
  safe_filename <- gsub("%", "pct", filename)
  ggsave(safe_filename, plot,
         width = dims$width, height = dims$height,
         dpi = dpi, bg = "white", ...)
  pdf_name <- sub("\\.[^.]+$", ".pdf", safe_filename)
  ggsave(pdf_name, plot,
         width = dims$width, height = dims$height,
         device = cairo_pdf, ...)
}
