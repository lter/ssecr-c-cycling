# Candidate replacement for Figure 4 (exploratory; not wired into 99_organize-figures.R)
library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

w  <- read.csv("data/harmonized/detection_probability_windows.csv", stringsAsFactors = FALSE)
tr <- read.csv("data/harmonized/trend_analysis_results.csv", stringsAsFactors = FALSE) %>%
  filter(trend_class != "insufficient_data")
sz <- read.csv("data/harmonized/sizer_analysis_results.csv", stringsAsFactors = FALSE)
rs <- read.csv("data/harmonized/relative_response_summary.csv", stringsAsFactors = FALSE) %>%
  mutate(yr = as.integer(substr(Date_parsed, 1, 4))) %>%
  group_by(source, Treatment) %>% summarise(start = min(yr), end = max(yr), .groups = "drop")

eco_cols <- c(Grassland = "#C99A2E", Forest = "#3B7A57", Tundra = "#6C8EBF", Coastal = "#2A9D8F",
              Urban = "#8E6BBF", Freshwater = "#4A90C2")

# ---- A/B: agreement of a window with the full record -------------------------
# windows covering >= 80% of a treatment's own record are left out: they are
# nearly the full record, which defines the reference verdict
ww <- w %>% filter(window_years < 0.8 * record_years, window_bin <= 20) %>%
  mutate(agree = ifelse(directional, p_same_sign, 1 - p_any),
         mislead = ifelse(directional, p_opposite_sign, p_any),
         group = ifelse(directional, "Long-term trend (n = 18)", "No long-term trend (n = 59)"))
per_trt <- ww %>% group_by(group, source, Treatment, window_bin) %>%
  summarise(agree = mean(agree), mislead = mean(mislead), .groups = "drop")
curve <- per_trt %>% group_by(group, window_bin) %>%
  summarise(agree = mean(agree), mislead = mean(mislead), n = n(), .groups = "drop") %>% filter(n >= 5)
overall <- per_trt %>% group_by(window_bin) %>% summarise(agree = mean(agree), n = n(), .groups = "drop") %>%
  filter(n >= 5) %>% mutate(group = "All treatments")

pA <- ggplot(curve, aes(window_bin, agree, colour = group)) +
  geom_line(data = filter(per_trt, group == "Long-term trend (n = 18)"),
            aes(group = interaction(source, Treatment)), colour = "grey80", linewidth = 0.35) +
  geom_line(linewidth = 1.3) +
  geom_line(data = overall, colour = "black", linetype = "22", linewidth = 0.9) +
  scale_colour_manual(values = c("#2C5F8A", "#7A8B99"), name = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + scale_x_continuous(breaks = seq(5, 20, 5)) +
  labs(x = "Length of record (years)", y = "Windows agreeing with\nthe full-record verdict") +
  theme_classic(base_size = 11) + theme(legend.position = c(0.62, 0.45), legend.background = element_blank())

pB <- ggplot(curve, aes(window_bin, mislead, colour = group)) +
  geom_line(linewidth = 1.2) + geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c("#B5473C", "#C99A2E"), name = NULL,
                      labels = c("Significant trend of the opposite sign", "Significant trend where the full record has none")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.2)) + scale_x_continuous(breaks = seq(5, 20, 5)) +
  labs(x = "Length of record (years)", y = "Windows giving a\nmisleading trend") +
  theme_classic(base_size = 11) + theme(legend.position = c(0.55, 0.88), legend.background = element_blank())

# ---- C/D: time to the first change of course (SiZer) --------------------------
chg <- sz %>% filter(n_slope_changes > 0) %>%
  mutate(first_year = as.integer(sub("[^0-9].*", "", change_years))) %>%
  left_join(rs, by = c("source", "treatment" = "Treatment")) %>%
  left_join(tr %>% distinct(source, site_type), by = "source") %>%
  mutate(years_to_change = first_year - start, record = end - start + 1)
cat("Time to first slope change: n =", nrow(chg), " median", median(chg$years_to_change),
    " range", paste(range(chg$years_to_change), collapse = "-"),
    "; within 3 yr:", sum(chg$years_to_change <= 3), "; after 10 yr:", sum(chg$years_to_change > 10), "\n")

pC <- ggplot(chg, aes(years_to_change, fill = site_type)) +
  geom_histogram(binwidth = 2, boundary = 0, colour = "white") +
  geom_vline(xintercept = 3, linetype = "dashed", colour = "grey30") +
  annotate("text", x = 3.4, y = Inf, label = "3-year study", hjust = 0, vjust = 1.5, size = 3, colour = "grey30") +
  scale_fill_manual(values = eco_cols, name = NULL) +
  labs(x = "Years from start of record to first slope change", y = "Treatments") +
  theme_classic(base_size = 11) + theme(legend.position = c(0.8, 0.7))

site_rec <- sz %>% left_join(rs, by = c("source", "treatment" = "Treatment")) %>%
  left_join(tr %>% distinct(source, site_type), by = "source") %>%
  group_by(site_abbr, site_type) %>% summarise(record = max(end - start + 1), n_trt = n(),
                                                n_chg = sum(n_slope_changes > 0), .groups = "drop")
pD <- ggplot() +
  geom_segment(data = site_rec, aes(x = 0, xend = record, y = reorder(site_abbr, record), yend = site_abbr),
               colour = "grey85", linewidth = 2.5, lineend = "round") +
  geom_point(data = chg, aes(years_to_change, site_abbr, fill = site_type), shape = 21, size = 2.6,
             position = position_jitter(height = 0.15, width = 0, seed = 1), alpha = 0.9) +
  geom_text(data = site_rec, aes(x = record + 1, y = site_abbr, label = paste0(n_chg, "/", n_trt)), size = 2.8, hjust = 0, colour = "grey30") +
  geom_vline(xintercept = 3, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = eco_cols, guide = "none") + scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(x = "Years since start of record", y = NULL) + theme_classic(base_size = 11)

fig <- (pA + pB) / (pC + pD) + plot_annotation(tag_levels = "A")
ggsave("figures/Figure4_candidate.png", fig, width = 10.5, height = 8, dpi = 300)
print(as.data.frame(curve %>% filter(window_bin %in% c(3, 5, 8, 10, 15, 20)) %>% mutate(across(c(agree, mislead), ~round(100 * .x))) %>% arrange(group, window_bin)))
print(as.data.frame(overall %>% filter(window_bin %in% c(3, 5, 8, 10, 15, 20)) %>% mutate(agree = round(100 * agree))))
