# 10_site-map.R
# Map of LTER sites included in the study, with experiment timelines

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(cowplot)

source("R/utils_plots.R")
source("R/utils_labels.R")

# --- Site coordinates (from LTER Network) ---

site_coords <- tibble::tribble(
  ~site_abbr, ~lat,      ~lon,       ~full_name,
  "ARC",       68.6283,  -149.5942,  "Arctic LTER",
  "BNZ",       64.6989,  -148.3203,  "Bonanza Creek",
  "CAP",       33.4255,  -111.9288,  "Central Arizona-Phoenix",
  "CDR",       45.4013,   -93.1997,  "Cedar Creek",
  "GCE",       31.4272,   -81.2937,  "Georgia Coastal",
  "HBR",       43.9340,   -71.7510,  "Hubbard Brook",
  "HFR",       42.5318,   -72.1893,  "Harvard Forest",
  "KBS",       42.3956,   -85.3737,  "Kellogg Bio. Stn.",
  "KNZ",       39.0885,   -96.5628,  "Konza Prairie",
  "LUQ",       18.3259,   -65.8165,  "Luquillo",
  "MCM",      -77.6228,   163.0523,  "McMurdo Dry Valleys",
  "NTL",       46.0085,   -89.6723,  "North Temperate Lakes",
  "NWT",       40.0543,  -105.5887,  "Niwot Ridge",
  "PIE",       42.7583,   -70.8917,  "Plum Island",
  "SBC",       34.4015,  -119.8420,  "Santa Barbara Coastal",
  "SEV",       34.3349,  -106.6316,  "Sevilleta",
  "VCR",       37.2804,   -75.9106,  "Virginia Coast Reserve"
)

# --- Experiment metadata (from dataset_registry.csv and data) ---

# Get date ranges from the actual data
summary_data <- read.csv("data/harmonized/relative_response_summary.csv",
                          stringsAsFactors = FALSE)
summary_data$Date_parsed <- as.Date(summary_data$Date_parsed)

metadata <- get_site_metadata()

date_ranges <- summary_data %>%
  mutate(site_abbr = source_to_abbr(source, metadata)) %>%
  group_by(site_abbr) %>%
  summarise(
    year_start = as.numeric(format(min(Date_parsed, na.rm = TRUE), "%Y")),
    year_end   = as.numeric(format(max(Date_parsed, na.rm = TRUE), "%Y")),
    .groups = "drop"
  )

# Experiment short descriptions
experiments <- tibble::tribble(
  ~site_abbr, ~experiment,
  "ARC",      "Fertilization & warming",
  "BNZ",      "Permafrost warming",
  "CAP",      "Desert fertilization",
  "CDR",      "BioCON (CO\u2082 \u00d7 N)",
  "GCE",      "Marsh disturbance",
  "HBR",      "MELNHE nutrient addition",
  "HFR",      "Soil warming",
  "KBS",      "Cropping system",
  "KNZ",      "Fire \u00d7 mowing \u00d7 nutrients",
  "LUQ",      "Canopy trimming",
  "MCM",      "Stoichiometry (CO\u2082 flux)",
  "NTL",      "Nutrient addition",
  "NWT",      "Warming \u00d7 N \u00d7 snow",
  "PIE",      "TIDE fertilization",
  "SBC",      "Kelp removal",
  "SEV",      "N fertilization",
  "VCR",      "Inundation \u00d7 wrack"
)

# Combine all site info
site_info <- site_coords %>%
  left_join(date_ranges, by = "site_abbr") %>%
  left_join(experiments, by = "site_abbr") %>%
  left_join(metadata %>% distinct(site_abbr, site_type), by = "site_abbr") %>%
  mutate(duration = year_end - year_start,
         label = paste0(site_abbr, ": ", experiment, " (", year_start, "\u2013", year_end, ")"))

# Ecosystem type colors (matching 07_sizer-analysis.R)
ecosystem_colors <- c(
  "Grassland"  = "#CC7A29",
  "Forest"     = "#3C8556",
  "Tundra"     = "#4A7BA7",
  "Coastal"    = "#1B7A7D",
  "Urban"      = "#8E6FAD",
  "Freshwater" = "#5AAECC"
)

# =============================================================================
# PANEL A: Map (continental US + Alaska + McMurdo inset)
# =============================================================================

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Main map: CONUS + nearby
conus_sites <- site_info %>% filter(!site_abbr %in% c("ARC", "BNZ", "MCM", "LUQ"))
alaska_sites <- site_info %>% filter(site_abbr %in% c("ARC", "BNZ"))
mcm_sites <- site_info %>% filter(site_abbr == "MCM")
luq_sites <- site_info %>% filter(site_abbr == "LUQ")

# Main map (CONUS)
p_map_main <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  geom_point(data = bind_rows(conus_sites, luq_sites),
             aes(x = lon, y = lat, fill = site_type),
             shape = 21, size = 3.5, color = "gray20", stroke = 0.4) +
  geom_text_repel(data = bind_rows(conus_sites, luq_sites),
            aes(x = lon, y = lat, label = site_abbr),
            size = 2.5, fontface = "bold", color = "gray20",
            min.segment.length = 0, segment.size = 0.3, segment.color = "gray50",
            box.padding = 0.4, point.padding = 0.3, max.overlaps = 20) +
  scale_fill_manual(values = ecosystem_colors, name = "Ecosystem") +
  coord_sf(xlim = c(-128, -64), ylim = c(10, 52), expand = FALSE) +
  theme_ccycling(base_size = 10) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = "bottom",
        panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.3),
        panel.background = element_rect(fill = "#E8F4F8")) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 4)))

# Alaska inset
p_alaska <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  geom_point(data = alaska_sites,
             aes(x = lon, y = lat, fill = site_type),
             shape = 21, size = 3, color = "gray20", stroke = 0.4) +
  geom_text(data = alaska_sites,
            aes(x = lon, y = lat, label = site_abbr),
            size = 2.2, fontface = "bold", nudge_y = -2, color = "gray20") +
  scale_fill_manual(values = ecosystem_colors) +
  coord_sf(xlim = c(-170, -138), ylim = c(58, 72), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.3),
        panel.background = element_rect(fill = "#E8F4F8"))

# McMurdo inset
p_mcm <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  geom_point(data = mcm_sites,
             aes(x = lon, y = lat, fill = site_type),
             shape = 21, size = 3, color = "gray20", stroke = 0.4) +
  geom_text(data = mcm_sites,
            aes(x = lon, y = lat, label = site_abbr),
            size = 2.2, fontface = "bold", nudge_x = -20, nudge_y = 5, color = "gray20") +
  scale_fill_manual(values = ecosystem_colors) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, -60), expand = FALSE,
           crs = st_crs(4326)) +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.3),
        panel.background = element_rect(fill = "#E8F4F8"))

# =============================================================================
# PANEL B: Timeline (experiment duration)
# =============================================================================

# Order sites by start year
site_info_ordered <- site_info %>%
  arrange(year_start, site_abbr) %>%
  mutate(site_label = paste0(site_abbr, " \u2014 ", experiment),
         site_label = factor(site_label, levels = unique(site_label)))

p_timeline <- ggplot(site_info_ordered,
                     aes(y = site_label,
                         xmin = year_start, xmax = year_end,
                         fill = site_type)) +
  geom_linerange(aes(x = NULL, xmin = year_start, xmax = year_end,
                     color = site_type),
                 linewidth = 3, alpha = 0.7) +
  geom_point(aes(x = year_start, color = site_type), size = 2) +
  geom_point(aes(x = year_end, color = site_type), size = 2) +
  scale_color_manual(values = ecosystem_colors, name = "Ecosystem") +
  scale_fill_manual(values = ecosystem_colors, name = "Ecosystem") +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  labs(x = "Year", y = NULL) +
  theme_ccycling(base_size = 10) +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8),
        panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2),
        plot.margin = margin(t = 8, r = 8, b = 8, l = 8))

# =============================================================================
# COMBINE
# =============================================================================

# Use cowplot to draw map with insets
p_map_with_insets <- ggdraw(p_map_main) +
  draw_plot(p_alaska, x = 0.08, y = 0.27, width = 0.25, height = 0.28) +
  draw_plot(p_mcm, x = 0.08, y = 0.15, width = 0.25, height = 0.16)

# Combine with cowplot for clean alignment, labels, and no extra whitespace
combined <- plot_grid(p_map_with_insets, p_timeline,
                      ncol = 2, rel_widths = c(1, 1),
                      labels = c("A", "B"), label_size = 16,
                      label_fontface = "bold")

ggsave("figures/Figure_site_map_timeline.png", combined,
       width = 14, height = 7, dpi = 300, bg = "white")
ggsave("figures/Figure_site_map_timeline.pdf", combined,
       width = 14, height = 7, device = cairo_pdf)

cat("Site map figure saved: figures/Figure_site_map_timeline.png\n")
