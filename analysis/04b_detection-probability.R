# ===========================
# 04b. Detection probability as a function of record length
# ===========================
# For every classified treatment, every window of L consecutive timepoints
# (all possible start years, not only the first L) is tested for a linear trend
# in the response ratio. For treatments with a significant full-record trend,
# the share of windows of a given length that recover that trend (p < 0.05, same
# sign) is the probability that a study of that length would have detected it;
# the share that are significant with the opposite sign is the rate of
# misleading trends. For treatments without a full-record trend, the share of
# significant windows is the rate of spurious trends. (Approach of White 2019
# BioScience; cf. Cusser et al. 2021 Ecol. Lett.) Unlike a first-detection time,
# this does not depend on when a record happens to start or stop and involves no
# sequential stopping rule.
#
# Inputs:  data/harmonized/relative_response_summary.csv, trend_analysis_results.csv
# Outputs: data/harmonized/detection_probability_windows.csv   (one row per treatment x window length)
#          data/harmonized/detection_probability_summary.csv   (mean curves by window length in years)
#          figures/detection_probability.png/.pdf
# ===========================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
source("R/utils_analysis.R")

rs <- read.csv("data/harmonized/relative_response_summary.csv", stringsAsFactors = FALSE)
rs$Date_parsed <- as.Date(rs$Date_parsed)
tr <- read.csv("data/harmonized/trend_analysis_results.csv", stringsAsFactors = FALSE) %>%
  filter(trend_class != "insufficient_data")

window_tests <- function(d) {
  d <- d[order(d$Date_parsed), ]
  t <- as.numeric(d$Date_parsed - min(d$Date_parsed)) / 365.25
  n <- nrow(d)
  full_sign <- sign(coef(lm(d$mean_response ~ t))[2])
  bind_rows(lapply(3:n, function(L) {
    starts <- seq_len(n - L + 1)
    r <- t(vapply(starts, function(s) {
      i <- s:(s + L - 1)
      f <- summary(lm(d$mean_response[i] ~ t[i]))$coefficients
      c(p = f[2, 4], b = f[2, 1], span = t[s + L - 1] - t[s])
    }, numeric(3)))
    sig <- r[, "p"] < 0.05
    data.frame(n_timepoints = L, window_years = median(r[, "span"]) + 1, n_windows = length(starts),
               p_same_sign = mean(sig & sign(r[, "b"]) == full_sign),
               p_opposite_sign = mean(sig & sign(r[, "b"]) != full_sign),
               p_any = mean(sig))
  }))
}

windows <- bind_rows(lapply(seq_len(nrow(tr)), function(k) {
  d <- rs[rs$source == tr$source[k] & rs$Treatment == tr$Treatment[k], ]
  if (nrow(d) < 4) return(NULL)
  cbind(source = tr$source[k], site_abbr = tr$site_abbr[k], Treatment = tr$Treatment[k],
        trend_class = tr$trend_class[k], n_total = nrow(d),
        record_years = as.numeric(diff(range(d$Date_parsed))) / 365.25 + 1,
        window_tests(d))
})) %>%
  mutate(directional = trend_class %in% c("increasing", "decreasing"),
         window_bin = round(window_years))
write.csv(windows, "data/harmonized/detection_probability_windows.csv", row.names = FALSE)

# Mean of per-treatment curves (each treatment weighted equally), by window length
curve <- function(df, value) {
  df %>% group_by(source, Treatment, window_bin) %>%
    summarise(v = mean(.data[[value]]), .groups = "drop") %>%
    group_by(window_bin) %>%
    summarise(n_treatments = n(), mean = mean(v), .groups = "drop")
}
summ <- bind_rows(
  curve(filter(windows, directional), "p_same_sign") %>% mutate(set = "Directional: trend detected (correct sign)"),
  curve(filter(windows, directional), "p_opposite_sign") %>% mutate(set = "Directional: significant, wrong sign"),
  curve(filter(windows, directional, record_years >= 20), "p_same_sign") %>% mutate(set = "Directional, records >= 20 yr: trend detected"),
  curve(filter(windows, !directional), "p_any") %>% mutate(set = "Non-directional: spurious significant trend")
)
write.csv(summ, "data/harmonized/detection_probability_summary.csv", row.names = FALSE)

cat("\n=== Detection probability by window length (mean of per-treatment curves) ===\n")
print(summ %>% filter(window_bin %in% c(3, 5, 8, 10, 15, 20, 25, 30)) %>%
        mutate(pct = round(100 * mean)) %>% select(set, window_bin, n_treatments, pct) %>%
        pivot_wider(names_from = window_bin, values_from = c(pct, n_treatments), names_sep = "_yr") %>% as.data.frame())

half <- summ %>% filter(set == "Directional: trend detected (correct sign)", mean >= 0.5) %>% slice_min(window_bin, n = 1)
cat(sprintf("\nShortest window at which the mean detection probability reaches 50%%: %s years\n",
            ifelse(nrow(half), half$window_bin, "not reached")))

# Figure
pA <- ggplot(filter(windows, directional), aes(window_years, p_same_sign)) +
  geom_line(aes(group = interaction(source, Treatment)), colour = "grey70", linewidth = 0.4) +
  geom_line(data = filter(summ, set == "Directional: trend detected (correct sign)"),
            aes(window_bin, mean), colour = "#2C5F8A", linewidth = 1.3) +
  geom_hline(yintercept = c(0.5, 0.8), linetype = "dotted", colour = "grey40") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Length of record (years)", y = "Windows detecting the long-term trend") +
  theme_classic(base_size = 12)
pB <- ggplot(filter(summ, set %in% c("Directional: significant, wrong sign", "Non-directional: spurious significant trend")),
             aes(window_bin, mean, colour = set)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("#B5473C", "#C99A2E"), name = NULL,
                      labels = c("Real trend, window shows opposite sign", "No long-term trend, window significant")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.3)) +
  labs(x = "Length of record (years)", y = "Windows with a misleading trend") +
  theme_classic(base_size = 12) + theme(legend.position = "bottom", legend.direction = "vertical")
fig <- pA + pB + plot_annotation(tag_levels = "A")
ggsave("figures/detection_probability.png", fig, width = 10, height = 5, dpi = 300)
ggsave("figures/detection_probability.pdf", fig, width = 10, height = 5)
cat("Figure written: figures/detection_probability.png\n")
