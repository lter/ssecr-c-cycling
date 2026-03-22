# 09_conceptual-figure.R
# Conceptual / hypothesis figure: possible trajectories of treatment effects
# on carbon cycling over time

library(ggplot2)
library(dplyr)

source("R/utils_plots.R")

# --- Define smooth trajectory curves ---

t <- seq(0, 20, length.out = 500)

trajectories <- bind_rows(
  # Stable: flat from origin, low variability
  tibble(year = t,
         y    = rep(0, length(t)),
         type = "Stable"),

  # Increasing: starts at 0, significant positive slope
  tibble(year = t,
         y    = 0.09 * t,
         type = "Increasing"),

  # Decreasing: starts at 0, significant negative slope
  tibble(year = t,
         y    = -0.08 * t,
         type = "Decreasing"),

  # Variable: starts at 0, oscillating with high CV but no net trend
  tibble(year = t,
         y    = 0.8 * sin(t * 0.7) + 0.4 * sin(t * 1.6),
         type = "Variable")
) %>%
  mutate(type = factor(type, levels = c("Stable", "Variable",
                                         "Increasing", "Decreasing")))

# Map to trend colors (lowercase names in TREND_COLORS)
concept_colors <- c(
  "Stable"     = unname(TREND_COLORS["stable"]),
  "Variable"   = unname(TREND_COLORS["variable"]),
  "Increasing" = unname(TREND_COLORS["increasing"]),
  "Decreasing" = unname(TREND_COLORS["decreasing"])
)

# --- Label positions (placed near curve endpoints) ---

label_data <- tibble(
  type  = c("Stable", "Increasing", "Decreasing", "Variable"),
  label = c("Accurate", "Underestimate", "Overestimate", "Uncertainty"),
  x     = c(20.3, 20.3, 20.3, 20.3),
  y     = c(0,
            0.09 * 20,
            -0.08 * 20,
            0.8 * sin(20 * 0.7) + 0.4 * sin(20 * 1.6)),
  y_nudge = c(0, 0, 0, 0)
) %>%
  mutate(type = factor(type, levels = levels(trajectories$type)))

# --- Build the figure ---

p <- ggplot(trajectories, aes(x = year, y = y, color = type)) +
  # Short-term shaded region (first 3 years)
  annotate("rect", xmin = 0, xmax = 3, ymin = -Inf, ymax = Inf,
           fill = "gray85", alpha = 0.5) +
  annotate("text", x = 1.5, y = Inf, label = "Short\nterm",
           vjust = 1.5, hjust = 0.5, size = 4, fontface = "italic",
           color = "gray30") +

  # Trajectory curves
  geom_line(linewidth = 1.2) +

  # End labels
  geom_text(data = label_data,
            aes(x = x, y = y + y_nudge, label = label, color = type),
            size = 4.5, fontface = "bold", hjust = 0, show.legend = FALSE) +

  # Scales
  scale_color_manual(values = concept_colors) +
  scale_x_continuous(breaks = seq(0, 20, 5), limits = c(0, 28),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2, 2), expand = c(0, 0),
                     breaks = NULL) +

  # Labels
  labs(x = "Time (years)",
       y = "Environmental Change Effects\non Carbon Cycling") +

  # Theme
  theme_ccycling(base_size = 13) +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.text = element_text(size = 11),
        plot.margin = margin(15, 10, 10, 10))

ggsave("figures/Figure_conceptual_trajectories.png", p,
       width = 7, height = 5, dpi = 300, bg = "white")
ggsave("figures/Figure_conceptual_trajectories.pdf", p,
       width = 7, height = 5, device = cairo_pdf)

cat("Conceptual figure saved: figures/Figure_conceptual_trajectories.png\n")
