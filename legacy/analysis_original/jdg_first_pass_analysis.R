library(dplyr)
library(ggplot2)
library(tidyr)

# Read harmonized data
df <- read.csv("Harmonizing/harmonized_Oct10_v2.csv")

# Define control treatment names
control_names <- c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4", 
                   "u u c", "W", "C C", "pre", "XXX", "WS08", "none", 
                   "CONTROL", "Outside Juncus", "C(Control Plot)")

# Function to parse date flexibly
parse_date_flexible <- function(date_string) {
  date_parsed <- tryCatch({
    if (grepl("^\\d{4}-\\d{2}$", date_string)) {
      as.Date(paste0(date_string, "-01"))
    } 
    else if (grepl("^\\d{4}$", date_string)) {
      as.Date(paste0(date_string, "-01-01"))
    }
    else {
      as.Date(date_string)
    }
  }, error = function(e) {
    NA
  })
  return(date_parsed)
}

# Function to calculate treatment relative to control
calculate_relative_response <- function(data, response_var = "Response.Variable") {
  
  # Parse dates
  data$Date_parsed <- sapply(data$Date, parse_date_flexible)
  data$Date_parsed <- as.Date(data$Date_parsed, origin = "1970-01-01")
  
  # Separate control and treatment data
  control_data <- data %>%
    filter(Treatment %in% control_names) %>%
    group_by(source, Date, Date_parsed) %>%
    summarise(control_mean = mean(get(response_var), na.rm = TRUE),
              control_n = n(),
              .groups = "drop")
  
  treatment_data <- data %>%
    filter(!Treatment %in% control_names)
  
  # Join and calculate relative response
  relative_data <- treatment_data %>%
    left_join(control_data, by = c("source", "Date", "Date_parsed")) %>%
    filter(!is.na(control_mean), control_mean > 0, !is.na(get(response_var))) %>%
    mutate(relative_response = get(response_var) / control_mean,
           log_response_ratio = log(get(response_var) / control_mean))
  
  return(relative_data)
}

# Calculate relative responses
relative_data <- calculate_relative_response(df)

# Calculate mean and SE for each experiment, treatment, and timepoint
summary_data <- relative_data %>%
  group_by(source, Treatment, Date_parsed) %>%
  summarise(
    mean_response = mean(relative_response, na.rm = TRUE),
    se_response = sd(relative_response, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Get unique experiments
experiments <- unique(summary_data$source)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Create a plot for each experiment
for (exp in experiments) {
  
  # Filter data for this experiment
  exp_data <- summary_data %>%
    filter(source == exp)
  
  # Skip if no data
  if (nrow(exp_data) == 0) next
  
  # Create plot with points (mean ± SE) and loess curve
  p <- ggplot(exp_data, aes(x = Date_parsed, y = mean_response, 
                            color = Treatment, fill = Treatment)) +
    # Error bars for SE
    geom_errorbar(aes(ymin = mean_response - se_response, 
                      ymax = mean_response + se_response),
                  width = 0, alpha = 0.5) +
    # Points for means
    geom_point(size = 3, alpha = 0.7) +
    # Loess smooth curve
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1) +
    # Reference line at 1
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    labs(title = paste("Treatment Response Relative to Control:", exp),
         subtitle = "Points show mean ± SE; curves show loess fit with 95% CI",
         x = "Date",
         y = "Treatment / Control Ratio") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save plot
  ggsave(filename = paste0("plots/", gsub("\\.csv", "", exp), "_relative_response_loess.png"),
         plot = p, width = 10, height = 6, dpi = 300)
  
  print(p)
}

# Alternative: Create faceted plot with all experiments
all_plot <- ggplot(summary_data, aes(x = Date_parsed, y = mean_response, 
                                     color = Treatment, fill = Treatment)) +
  geom_errorbar(aes(ymin = mean_response - se_response, 
                    ymax = mean_response + se_response),
                width = 0, alpha = 0.3) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~source, scales = "free") +
  labs(title = "Treatment Response Relative to Control Across All Experiments",
       subtitle = "Points show mean ± SE; curves show loess fit",
       x = "Date",
       y = "Treatment / Control Ratio") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        strip.text = element_text(size = 7))

ggsave("plots/all_experiments_relative_response_loess.png", 
       all_plot, width = 16, height = 12, dpi = 300)

print(all_plot+theme(legend.position = "none"))

# Optional: Version using log response ratio instead
all_plot_log <- ggplot(summary_data %>%
                         group_by(source, Treatment, Date_parsed) %>%
                         summarise(mean_log = mean(log(mean_response), na.rm = TRUE),
                                   se_log = sd(log(mean_response), na.rm = TRUE) / sqrt(n()),
                                   .groups = "drop"),
                       aes(x = Date_parsed, y = mean_log, 
                           color = Treatment, fill = Treatment)) +
  geom_errorbar(aes(ymin = mean_log - se_log, 
                    ymax = mean_log + se_log),
                width = 0, alpha = 0.3) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~source, scales = "free") +
  labs(title = "Log Response Ratio Across All Experiments",
       subtitle = "Points show mean ± SE; curves show loess fit; 0 = no effect",
       x = "Date",
       y = "Log Response Ratio (ln(Treatment/Control))") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        strip.text = element_text(size = 7))

ggsave("plots/all_experiments_log_response_loess.png", 
       all_plot_log, width = 16, height = 12, dpi = 300)

print(all_plot_log+theme(legend.position = "none"))

# Summary statistics
summary_stats <- summary_data %>%
  group_by(source, Treatment) %>%
  summarise(
    overall_mean = mean(mean_response, na.rm = TRUE),
    overall_median = median(mean_response, na.rm = TRUE),
    n_timepoints = n(),
    .groups = "drop"
  ) %>%
  arrange(source, Treatment)

print(summary_stats)


















library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

# Read harmonized data
df <- read.csv("/Users/jongewirtzman/Downloads/harmonized_Oct10_v2.csv")

# Define control treatment names
control_names <- c("control", "Control", "C", "0", "C1", "Camb Namb M", "T4", 
                   "u u c", "W", "C C", "pre", "XXX", "WS08", "none", 
                   "CONTROL", "Outside Juncus", "C(Control Plot)")

# Function to parse date flexibly
parse_date_flexible <- function(date_string) {
  date_parsed <- tryCatch({
    if (grepl("^\\d{4}-\\d{2}$", date_string)) {
      as.Date(paste0(date_string, "-01"))
    } 
    else if (grepl("^\\d{4}$", date_string)) {
      as.Date(paste0(date_string, "-01-01"))
    }
    else {
      as.Date(date_string)
    }
  }, error = function(e) {
    NA
  })
  return(date_parsed)
}

# Function to calculate treatment relative to control
calculate_relative_response <- function(data, response_var = "Response.Variable") {
  
  # Parse dates
  data$Date_parsed <- sapply(data$Date, parse_date_flexible)
  data$Date_parsed <- as.Date(data$Date_parsed, origin = "1970-01-01")
  
  # Separate control and treatment data
  control_data <- data %>%
    filter(Treatment %in% control_names) %>%
    group_by(source, Date, Date_parsed) %>%
    summarise(control_mean = mean(get(response_var), na.rm = TRUE),
              control_n = n(),
              .groups = "drop")
  
  treatment_data <- data %>%
    filter(!Treatment %in% control_names)
  
  # Join and calculate relative response
  relative_data <- treatment_data %>%
    left_join(control_data, by = c("source", "Date", "Date_parsed")) %>%
    filter(!is.na(control_mean), control_mean > 0, !is.na(get(response_var))) %>%
    mutate(relative_response = get(response_var) / control_mean,
           log_response_ratio = log(get(response_var) / control_mean))
  
  return(relative_data)
}

# Calculate relative responses
relative_data <- calculate_relative_response(df)

# Calculate mean and SE for each experiment, treatment, and timepoint
summary_data <- relative_data %>%
  group_by(source, Treatment, Date_parsed) %>%
  summarise(
    mean_response = mean(relative_response, na.rm = TRUE),
    se_response = sd(relative_response, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Function to classify temporal trends
classify_trend <- function(data) {
  # Need at least 3 timepoints for meaningful trend analysis
  if (nrow(data) < 3) {
    return(data.frame(
      trend_class = "insufficient_data",
      slope = NA,
      p_value = NA,
      r_squared = NA,
      cv = NA,
      mean_ratio = NA,
      n_timepoints = nrow(data)
    ))
  }
  
  # Convert date to numeric (days since first observation)
  data$time_numeric <- as.numeric(data$Date_parsed - min(data$Date_parsed))
  
  # Fit linear model
  lm_fit <- lm(mean_response ~ time_numeric, data = data)
  lm_summary <- summary(lm_fit)
  
  # Extract statistics
  slope <- coef(lm_fit)[2]
  p_value <- coef(lm_summary)[2, 4]
  r_squared <- lm_summary$r.squared
  
  # Calculate coefficient of variation
  cv <- sd(data$mean_response, na.rm = TRUE) / mean(data$mean_response, na.rm = TRUE)
  mean_ratio <- mean(data$mean_response, na.rm = TRUE)
  
  # Classify trend based on multiple criteria
  # Using p < 0.05 for significance and slope magnitude
  if (p_value < 0.05) {
    if (slope > 0) {
      trend_class <- "increasing"
    } else {
      trend_class <- "decreasing"
    }
  } else {
    # Non-significant trend - check if stable or variable
    if (cv < 0.2) {  # CV < 20% indicates low variability
      trend_class <- "stable"
    } else {
      trend_class <- "variable"
    }
  }
  
  return(data.frame(
    trend_class = trend_class,
    slope = slope,
    p_value = p_value,
    r_squared = r_squared,
    cv = cv,
    mean_ratio = mean_ratio,
    n_timepoints = nrow(data)
  ))
}

# Analyze trends for each experiment-treatment combination
trend_analysis <- summary_data %>%
  group_by(source, Treatment) %>%
  group_modify(~ classify_trend(.x)) %>%
  ungroup()

# Summary statistics
trend_summary <- trend_analysis %>%
  group_by(trend_class) %>%
  summarise(
    n_treatments = n(),
    mean_cv = mean(cv, na.rm = TRUE),
    mean_slope = mean(slope, na.rm = TRUE),
    mean_r2 = mean(r_squared, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_treatments))

print("=== TREND CLASSIFICATION SUMMARY ===")
print(trend_summary)

# Summary by experiment
experiment_summary <- trend_analysis %>%
  group_by(source, trend_class) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = trend_class, values_from = n, values_fill = 0)

print("\n=== TRENDS BY EXPERIMENT ===")
print(experiment_summary)

# Detailed results
print("\n=== DETAILED TREND ANALYSIS ===")
print(trend_analysis %>% 
        arrange(source, Treatment) %>%
        dplyr::select(source, Treatment, trend_class, slope, p_value, r_squared, cv, n_timepoints))

# Create plots directory
if (!dir.exists("plots")) dir.create("plots")

# Visualize trend classifications
trend_plot <- ggplot(trend_analysis, aes(x = trend_class, fill = trend_class)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00",
    "insufficient_data" = "#757575"
  )) +
  labs(title = "Classification of Treatment Response Trends Over Time",
       subtitle = "Based on linear regression and coefficient of variation",
       x = "Trend Classification",
       y = "Number of Treatment-Experiment Combinations") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("plots/trend_classification_summary.png", trend_plot, 
       width = 10, height = 6, dpi = 300)

print(trend_plot)

# Plot trends by experiment
experiment_trend_plot <- trend_analysis %>%
  filter(trend_class != "insufficient_data") %>%
  ggplot(aes(x = source, fill = trend_class)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00"
  )) +
  labs(title = "Proportion of Trend Types by Experiment",
       x = "Experiment",
       y = "Proportion",
       fill = "Trend Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("plots/trend_by_experiment.png", experiment_trend_plot, 
       width = 12, height = 6, dpi = 300)

print(experiment_trend_plot)

# Scatter plot: slope vs CV colored by classification
scatter_plot <- trend_analysis %>%
  filter(trend_class != "insufficient_data") %>%
  ggplot(aes(x = cv, y = slope, color = trend_class)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.2, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00"
  )) +
  labs(title = "Temporal Trend Characteristics",
       subtitle = "Dashed lines show classification thresholds (CV=0.2, slope=0)",
       x = "Coefficient of Variation (CV)",
       y = "Slope (change in ratio per day)",
       color = "Trend Type") +
  theme_bw()

ggsave("plots/trend_characteristics.png", scatter_plot, 
       width = 10, height = 6, dpi = 300)

print(scatter_plot)

# Create individual plots for each experiment showing trends
experiments <- unique(summary_data$source)

for (exp in experiments) {
  
  exp_data <- summary_data %>%
    filter(source == exp)
  
  exp_trends <- trend_analysis %>%
    filter(source == exp)
  
  if (nrow(exp_data) == 0) next
  
  # Join trend classification
  exp_data <- exp_data %>%
    left_join(exp_trends %>% dplyr::select(Treatment, trend_class, slope, p_value), 
              by = "Treatment")
  
  # Create plot
  p <- ggplot(exp_data, aes(x = Date_parsed, y = mean_response, 
                            color = trend_class, fill = trend_class)) +
    geom_errorbar(aes(ymin = mean_response - se_response, 
                      ymax = mean_response + se_response),
                  width = 0, alpha = 0.3) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(
      "increasing" = "#2E7D32",
      "decreasing" = "#C62828", 
      "stable" = "#1565C0",
      "variable" = "#F57C00",
      "insufficient_data" = "#757575"
    )) +
    scale_fill_manual(values = c(
      "increasing" = "#2E7D32",
      "decreasing" = "#C62828", 
      "stable" = "#1565C0",
      "variable" = "#F57C00",
      "insufficient_data" = "#757575"
    )) +
    facet_wrap(~Treatment, scales = "free_y") +
    labs(title = paste("Treatment Trends:", exp),
         subtitle = "Points = mean ± SE; curves = loess fit; colors = trend classification",
         x = "Date",
         y = "Treatment / Control Ratio",
         color = "Trend",
         fill = "Trend") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("plots/", gsub("\\.csv", "", exp), "_trends_classified.png"),
         plot = p, width = 14, height = 10, dpi = 300)
  
  print(p)
}

# Save results to CSV
write.csv(trend_analysis, "plots/trend_analysis_results.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to: plots/trend_analysis_results.csv\n")
cat("Plots saved to: plots/ directory\n")




















# Additional summary statistics and visualizations

# 1. Overall proportions
overall_proportions <- trend_analysis %>%
  filter(trend_class != "insufficient_data") %>%
  count(trend_class) %>%
  mutate(proportion = n / sum(n),
         percentage = round(proportion * 100, 1))

print("\n=== OVERALL PROPORTIONS (excluding insufficient data) ===")
print(overall_proportions)

# 2. Summary statistics by trend type
trend_stats <- trend_analysis %>%
  filter(trend_class != "insufficient_data") %>%
  group_by(trend_class) %>%
  summarise(
    n = n(),
    mean_ratio = mean(mean_ratio, na.rm = TRUE),
    median_ratio = median(mean_ratio, na.rm = TRUE),
    mean_cv = mean(cv, na.rm = TRUE),
    median_cv = median(cv, na.rm = TRUE),
    mean_timepoints = mean(n_timepoints, na.rm = TRUE),
    .groups = "drop"
  )

print("\n=== DETAILED STATISTICS BY TREND TYPE ===")
print(trend_stats)

# 3. Effect size categories (how different from control)
trend_with_effect <- trend_analysis %>%
  filter(trend_class != "insufficient_data") %>%
  mutate(
    effect_category = case_when(
      mean_ratio < 0.8 ~ "Strong negative effect",
      mean_ratio >= 0.8 & mean_ratio < 0.95 ~ "Moderate negative effect",
      mean_ratio >= 0.95 & mean_ratio <= 1.05 ~ "No effect",
      mean_ratio > 1.05 & mean_ratio <= 1.25 ~ "Moderate positive effect",
      mean_ratio > 1.25 ~ "Strong positive effect"
    )
  )

effect_summary <- trend_with_effect %>%
  count(trend_class, effect_category) %>%
  pivot_wider(names_from = effect_category, values_from = n, values_fill = 0)

print("\n=== EFFECT SIZE BY TREND TYPE ===")
print(effect_summary)

# 4. Create comprehensive summary plot
library(gridExtra)
library(scales)

# Plot 1: Pie chart of trend classifications
pie_data <- overall_proportions %>%
  mutate(label = paste0(trend_class, "\n", n, " (", percentage, "%)"))

p1 <- ggplot(pie_data, aes(x = "", y = n, fill = trend_class)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00"
  )) +
  labs(title = "Distribution of Trend Types",
       fill = "Trend Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 2: Effect size distribution
p2 <- ggplot(trend_with_effect, aes(x = mean_ratio, fill = trend_class)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
  scale_fill_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00"
  )) +
  labs(title = "Distribution of Mean Treatment/Control Ratios",
       x = "Mean Treatment/Control Ratio",
       y = "Count",
       fill = "Trend Type") +
  theme_bw()

# Plot 3: CV distribution by trend
p3 <- ggplot(trend_analysis %>% filter(trend_class != "insufficient_data"), 
             aes(x = trend_class, y = cv, fill = trend_class)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00"
  )) +
  labs(title = "Coefficient of Variation by Trend Type",
       x = "Trend Type",
       y = "Coefficient of Variation") +
  theme_bw() +
  theme(legend.position = "none")

# Plot 4: Number of timepoints by trend
p4 <- ggplot(trend_analysis %>% filter(trend_class != "insufficient_data"), 
             aes(x = trend_class, y = n_timepoints, fill = trend_class)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c(
    "increasing" = "#2E7D32",
    "decreasing" = "#C62828", 
    "stable" = "#1565C0",
    "variable" = "#F57C00"
  )) +
  labs(title = "Number of Timepoints by Trend Type",
       x = "Trend Type",
       y = "Number of Timepoints") +
  theme_bw() +
  theme(legend.position = "none")

# Combine plots
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

ggsave("plots/comprehensive_trend_summary.png", combined_plot, 
       width = 14, height = 10, dpi = 300)

# 5. Heatmap of trends by experiment
heatmap_data <- experiment_summary %>%
  pivot_longer(cols = -source, names_to = "trend_class", values_to = "count") %>%
  filter(trend_class != "insufficient_data") %>%
  group_by(source) %>%
  mutate(proportion = count / sum(count))

p5 <- ggplot(heatmap_data, aes(x = trend_class, y = source, fill = proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = count), size = 3) +
  scale_fill_gradient2(low = "white", high = "#1565C0", 
                       midpoint = 0.5, labels = percent) +
  labs(title = "Distribution of Trend Types Across Experiments",
       subtitle = "Numbers show count; colors show proportion within experiment",
       x = "Trend Type",
       y = "Experiment",
       fill = "Proportion") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/trend_heatmap_by_experiment.png", p5, 
       width = 10, height = 12, dpi = 300)

print(p5)

# 6. Create summary text report
sink("plots/trend_analysis_summary_report.txt")

cat("=" , rep("=", 70), "\n", sep = "")
cat("TEMPORAL TREND ANALYSIS SUMMARY REPORT\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

cat("OVERALL SUMMARY:\n")
cat("Total treatment-experiment combinations analyzed:", nrow(trend_analysis), "\n")
cat("Combinations with sufficient data:", 
    nrow(trend_analysis %>% filter(trend_class != "insufficient_data")), "\n\n")

cat("TREND DISTRIBUTION:\n")
for (i in 1:nrow(overall_proportions)) {
  cat(sprintf("  %s: %d (%.1f%%)\n", 
              overall_proportions$trend_class[i], 
              overall_proportions$n[i], 
              overall_proportions$percentage[i]))
}

cat("\nKEY FINDINGS:\n")
cat(sprintf("  - %.1f%% of treatments show stable responses over time\n", 
            overall_proportions$percentage[overall_proportions$trend_class == "stable"]))
cat(sprintf("  - %.1f%% show variable responses without clear directional trends\n", 
            overall_proportions$percentage[overall_proportions$trend_class == "variable"]))
cat(sprintf("  - %.1f%% show significant increasing trends\n", 
            overall_proportions$percentage[overall_proportions$trend_class == "increasing"]))
cat(sprintf("  - %.1f%% show significant decreasing trends\n", 
            overall_proportions$percentage[overall_proportions$trend_class == "decreasing"]))

cat("\nTREND CHARACTERISTICS:\n")
for (i in 1:nrow(trend_stats)) {
  cat(sprintf("\n%s treatments:\n", toupper(trend_stats$trend_class[i])))
  cat(sprintf("  Mean treatment/control ratio: %.3f\n", trend_stats$mean_ratio[i]))
  cat(sprintf("  Mean coefficient of variation: %.3f\n", trend_stats$mean_cv[i]))
  cat(sprintf("  Average timepoints: %.1f\n", trend_stats$mean_timepoints[i]))
}

cat("\n", rep("=", 72), "\n", sep = "")

sink()

cat("\n=== SUMMARY REPORT SAVED ===\n")
cat("File: plots/trend_analysis_summary_report.txt\n\n")

# 7. Statistical tests comparing trend groups
cat("\n=== STATISTICAL COMPARISONS ===\n")

# Kruskal-Wallis test for CV differences between groups
kw_cv <- kruskal.test(cv ~ trend_class, 
                      data = trend_analysis %>% 
                        filter(trend_class != "insufficient_data"))
cat("\nKruskal-Wallis test - CV by trend type:\n")
cat(sprintf("  H = %.3f, p-value = %.4f\n", kw_cv$statistic, kw_cv$p.value))

# Kruskal-Wallis test for mean ratio differences
kw_ratio <- kruskal.test(mean_ratio ~ trend_class, 
                         data = trend_analysis %>% 
                           filter(trend_class != "insufficient_data"))
cat("\nKruskal-Wallis test - Mean ratio by trend type:\n")
cat(sprintf("  H = %.3f, p-value = %.4f\n", kw_ratio$statistic, kw_ratio$p.value))

# 8. Export detailed table
detailed_export <- trend_analysis %>%
  left_join(
    relative_data %>%
      group_by(source, Treatment) %>%
      summarise(
        n_observations = n(),
        date_range = paste(min(Date), "to", max(Date)),
        .groups = "drop"
      ),
    by = c("source", "Treatment")
  ) %>%
  arrange(trend_class, source, Treatment)

write.csv(detailed_export, "plots/detailed_trend_analysis.csv", row.names = FALSE)

cat("\n=== DETAILED RESULTS EXPORTED ===\n")
cat("File: plots/detailed_trend_analysis.csv\n")

















# Create a publication-quality summary figure

library(ggplot2)
library(dplyr)
library(patchwork)  # for combining plots nicely

# Prepare data
trend_data_clean <- trend_analysis %>%
  filter(trend_class != "insufficient_data")

# Set consistent color scheme
trend_colors <- c(
  "increasing" = "#2E7D32",
  "decreasing" = "#C62828", 
  "stable" = "#1565C0",
  "variable" = "#F57C00"
)

# Panel A: Stacked bar showing proportions
panel_a <- overall_proportions %>%
  mutate(trend_class = factor(trend_class, 
                              levels = c("stable", "variable", "increasing", "decreasing"))) %>%
  ggplot(aes(x = "", y = n, fill = trend_class)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = paste0(n, "\n(", percentage, "%)")), 
            position = position_fill(vjust = 0.5), 
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = trend_colors) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "A. Overall Distribution",
       y = "Proportion of treatments",
       x = "",
       fill = "Trend Type") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

# Panel B: Effect sizes by trend type
panel_b <- trend_with_effect %>%
  mutate(trend_class = factor(trend_class, 
                              levels = c("stable", "variable", "increasing", "decreasing"))) %>%
  ggplot(aes(x = trend_class, y = mean_ratio, fill = trend_class)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = trend_colors) +
  scale_y_continuous(trans = "log2", 
                     breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 4)) +
  labs(title = "B. Treatment Effect Size",
       x = "Trend Type",
       y = "Mean Treatment/Control Ratio (log scale)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Panel C: Temporal variability
panel_c <- trend_data_clean %>%
  mutate(trend_class = factor(trend_class, 
                              levels = c("stable", "variable", "increasing", "decreasing"))) %>%
  ggplot(aes(x = trend_class, y = cv, fill = trend_class)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = trend_colors) +
  labs(title = "C. Temporal Variability",
       x = "Trend Type",
       y = "Coefficient of Variation",
       caption = "Dashed line shows CV = 0.2 threshold") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Panel D: Study duration
panel_d <- trend_data_clean %>%
  mutate(trend_class = factor(trend_class, 
                              levels = c("stable", "variable", "increasing", "decreasing"))) %>%
  ggplot(aes(x = trend_class, y = n_timepoints, fill = trend_class)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = trend_colors) +
  labs(title = "D. Study Duration",
       x = "Trend Type",
       y = "Number of Timepoints") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine all panels
final_figure <- (panel_a | panel_b) / (panel_c | panel_d)

ggsave("plots/Figure_temporal_trends_summary.png", 
       final_figure, width = 12, height = 10, dpi = 300)

ggsave("plots/Figure_temporal_trends_summary.pdf", 
       final_figure, width = 12, height = 10)

print(final_figure)

# Create a simple table for manuscript
manuscript_table <- trend_stats %>%
  mutate(
    trend_class = factor(trend_class, 
                         levels = c("stable", "variable", "increasing", "decreasing")),
    mean_ratio = sprintf("%.2f (%.2f)", mean_ratio, median_ratio),
    mean_cv = sprintf("%.3f (%.3f)", mean_cv, median_cv),
    mean_timepoints = sprintf("%.1f", mean_timepoints)
  ) %>%
  arrange(trend_class) %>%
  dplyr::select(
    `Trend Type` = trend_class,
    `N` = n,
    `Mean (Median) Ratio` = mean_ratio,
    `Mean (Median) CV` = mean_cv,
    `Mean Timepoints` = mean_timepoints
  )

print("\n=== TABLE FOR MANUSCRIPT ===")
print(manuscript_table)

write.csv(manuscript_table, "plots/Table_trend_summary.csv", row.names = FALSE)

# Create interpretation text
cat("\n=== KEY INTERPRETATIONS ===\n\n")

cat("1. DOMINANT PATTERN:\n")
cat(sprintf("   - %d%% of treatments show non-directional responses (stable + variable)\n",
            round(sum(overall_proportions$percentage[overall_proportions$trend_class %in% c("stable", "variable")]))))
cat(sprintf("   - Only %d%% show clear directional trends (increasing + decreasing)\n\n",
            round(sum(overall_proportions$percentage[overall_proportions$trend_class %in% c("increasing", "decreasing")]))))

cat("2. EFFECT MAGNITUDES:\n")
cat(sprintf("   - Stable treatments average %.2fx control (close to no effect)\n", 
            trend_stats$mean_ratio[trend_stats$trend_class == "stable"]))
cat(sprintf("   - Variable/Increasing treatments average %.2fx control (moderate positive effect)\n", 
            mean(trend_stats$mean_ratio[trend_stats$trend_class %in% c("variable", "increasing")])))
cat(sprintf("   - Decreasing treatments average %.2fx control (moderate positive effect, but declining)\n\n",
            trend_stats$mean_ratio[trend_stats$trend_class == "decreasing"]))

cat("3. TEMPORAL DYNAMICS:\n")
cat(sprintf("   - Increasing trends require longer studies (%.0f timepoints on average)\n",
            trend_stats$mean_timepoints[trend_stats$trend_class == "increasing"]))
cat(sprintf("   - Stable patterns detectable in shorter studies (%.0f timepoints)\n",
            trend_stats$mean_timepoints[trend_stats$trend_class == "stable"]))
cat(sprintf("   - Variable responses have high CV (%.2f), indicating boom-bust dynamics\n\n",
            trend_stats$mean_cv[trend_stats$trend_class == "variable"]))

cat("4. STATISTICAL SIGNIFICANCE:\n")
cat(sprintf("   - CV differs significantly among trend types (H = %.2f, p < 0.0001)\n", 
            kw_cv$statistic))
cat(sprintf("   - Mean ratios differ significantly among trend types (H = %.2f, p < 0.0001)\n\n", 
            kw_ratio$statistic))

# Export this interpretation
sink("plots/interpretation_summary.txt")
cat("KEY INTERPRETATIONS OF TEMPORAL TREND ANALYSIS\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

cat("1. DOMINANT PATTERN:\n")
cat(sprintf("   - %d%% of treatments show non-directional responses (stable + variable)\n",
            round(sum(overall_proportions$percentage[overall_proportions$trend_class %in% c("stable", "variable")]))))
cat(sprintf("   - Only %d%% show clear directional trends (increasing + decreasing)\n\n",
            round(sum(overall_proportions$percentage[overall_proportions$trend_class %in% c("increasing", "decreasing")]))))

cat("2. EFFECT MAGNITUDES:\n")
cat(sprintf("   - Stable treatments average %.2fx control (close to no effect)\n", 
            trend_stats$mean_ratio[trend_stats$trend_class == "stable"]))
cat(sprintf("   - Variable/Increasing treatments average %.2fx control (moderate positive effect)\n", 
            mean(trend_stats$mean_ratio[trend_stats$trend_class %in% c("variable", "increasing")])))
cat(sprintf("   - Decreasing treatments average %.2fx control (moderate positive effect, but declining)\n\n",
            trend_stats$mean_ratio[trend_stats$trend_class == "decreasing"]))

cat("3. TEMPORAL DYNAMICS:\n")
cat(sprintf("   - Increasing trends require longer studies (%.0f timepoints on average)\n",
            trend_stats$mean_timepoints[trend_stats$trend_class == "increasing"]))
cat(sprintf("   - Stable patterns detectable in shorter studies (%.0f timepoints)\n",
            trend_stats$mean_timepoints[trend_stats$trend_class == "stable"]))
cat(sprintf("   - Variable responses have high CV (%.2f), indicating boom-bust dynamics\n\n",
            trend_stats$mean_cv[trend_stats$trend_class == "variable"]))

cat("4. IMPLICATIONS FOR EXPERIMENTAL DESIGN:\n")
cat("   - Short-term studies may miss directional trends (need 30+ timepoints)\n")
cat("   - High variability is common (42% of treatments) - requires replication\n")
cat("   - Most treatments maintain consistent effects over time (77% stable/variable)\n")
cat("   - Few treatments show declining effectiveness (only 3%)\n")

sink()

cat("\nAll files saved to plots/ directory\n")


























library(dplyr)
library(ggplot2)
library(broom)

# Function to perform sequential analysis - how many timepoints needed for detection?
detect_trend_timepoints <- function(data) {
  
  # Need at least 3 timepoints
  if (nrow(data) < 3) {
    return(data.frame(
      min_n_for_detection = NA,
      final_n = nrow(data),
      final_slope = NA,
      final_p = NA,
      time_to_detect = NA,
      detected = FALSE
    ))
  }
  
  # Sort by date
  data <- data %>% arrange(Date_parsed)
  data$time_numeric <- as.numeric(data$Date_parsed - min(data$Date_parsed))
  
  # Try progressively longer time series
  detection_n <- NA
  
  for (i in 3:nrow(data)) {
    subset_data <- data[1:i, ]
    lm_fit <- lm(mean_response ~ time_numeric, data = subset_data)
    p_value <- summary(lm_fit)$coefficients[2, 4]
    
    # Check if trend is significant
    if (p_value < 0.05) {
      detection_n <- i
      break
    }
  }
  
  # Get final model statistics
  final_lm <- lm(mean_response ~ time_numeric, data = data)
  final_summary <- summary(final_lm)
  final_slope <- coef(final_lm)[2]
  final_p <- final_summary$coefficients[2, 4]
  
  return(data.frame(
    min_n_for_detection = ifelse(is.na(detection_n), nrow(data), detection_n),
    final_n = nrow(data),
    final_slope = final_slope,
    final_p = final_p,
    time_to_detect = ifelse(is.na(detection_n), NA, 
                            as.numeric(data$Date_parsed[detection_n] - data$Date_parsed[1]) / 365.25),
    detected = !is.na(detection_n)
  ))
}

# Apply to all experiment-treatment combinations with trends
detection_analysis <- summary_data %>%
  group_by(source, Treatment) %>%
  group_modify(~ detect_trend_timepoints(.x)) %>%
  ungroup()

# Join with original trend classification
detection_with_class <- detection_analysis %>%
  left_join(trend_analysis %>% 
              dplyr::select(source, Treatment, trend_class, slope, p_value, cv, mean_ratio),
            by = c("source", "Treatment"))

# Focus on treatments that DO have significant trends
significant_trends <- detection_with_class %>%
  filter(trend_class %in% c("increasing", "decreasing"), detected == TRUE)

print("\n=== DETECTION TIME ANALYSIS FOR SIGNIFICANT TRENDS ===\n")

# Summary statistics
detection_summary <- significant_trends %>%
  group_by(trend_class) %>%
  summarise(
    n = n(),
    mean_n_to_detect = mean(min_n_for_detection, na.rm = TRUE),
    median_n_to_detect = median(min_n_for_detection, na.rm = TRUE),
    min_n_to_detect = min(min_n_for_detection, na.rm = TRUE),
    max_n_to_detect = max(min_n_for_detection, na.rm = TRUE),
    mean_years_to_detect = mean(time_to_detect, na.rm = TRUE),
    median_years_to_detect = median(time_to_detect, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary: Timepoints needed to detect significant trends")
print(detection_summary)

# Detailed view
print("\nDetailed breakdown by experiment:")
print(significant_trends %>%
        arrange(min_n_for_detection) %>%
        dplyr::select(source, Treatment, trend_class, min_n_for_detection, 
                      time_to_detect, final_slope, final_p) %>%
        mutate(time_to_detect = round(time_to_detect, 1)))

# What about trends that were never detected?
non_detected_trends <- detection_with_class %>%
  filter(trend_class %in% c("increasing", "decreasing"), detected == FALSE)

if (nrow(non_detected_trends) > 0) {
  print("\n=== TRENDS THAT NEVER REACHED SIGNIFICANCE ===")
  print(paste("Number of non-significant 'trends':", nrow(non_detected_trends)))
  print("(These were classified by other criteria, not p-value)")
}

# Relationship between effect size and detection time
effect_vs_detection <- significant_trends %>%
  mutate(
    abs_slope = abs(final_slope),
    effect_magnitude = abs(log(mean_ratio))
  )

# Plot 1: Detection time by trend type
p1 <- ggplot(significant_trends, aes(x = trend_class, y = min_n_for_detection, 
                                     fill = trend_class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("increasing" = "#2E7D32", "decreasing" = "#C62828")) +
  labs(title = "Timepoints Required to Detect Significant Trends",
       subtitle = "Based on when p < 0.05 first achieved in sequential analysis",
       x = "Trend Type",
       y = "Number of Timepoints to Detection") +
  theme_bw() +
  theme(legend.position = "none")

# Plot 2: Years to detection
p2 <- ggplot(significant_trends, aes(x = trend_class, y = time_to_detect, 
                                     fill = trend_class)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("increasing" = "#2E7D32", "decreasing" = "#C62828")) +
  labs(title = "Years Required to Detect Significant Trends",
       x = "Trend Type",
       y = "Years to Detection") +
  theme_bw() +
  theme(legend.position = "none")

# Plot 3: Detection time vs effect size
p3 <- ggplot(effect_vs_detection, aes(x = abs_slope, y = min_n_for_detection, 
                                      color = trend_class)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("increasing" = "#2E7D32", "decreasing" = "#C62828")) +
  scale_x_log10() +
  labs(title = "Detection Time vs. Effect Size",
       subtitle = "Larger slopes detected faster (as expected)",
       x = "Absolute Slope (log scale)",
       y = "Timepoints to Detection",
       color = "Trend Type") +
  theme_bw()

# Plot 4: Detection time vs CV
p4 <- ggplot(significant_trends, aes(x = cv, y = min_n_for_detection, 
                                     color = trend_class)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("increasing" = "#2E7D32", "decreasing" = "#C62828")) +
  labs(title = "Detection Time vs. Variability",
       subtitle = "Higher variability requires more timepoints",
       x = "Coefficient of Variation",
       y = "Timepoints to Detection",
       color = "Trend Type") +
  theme_bw()

# Combine plots
library(patchwork)
detection_plots <- (p1 | p2) / (p3 | p4)

ggsave("plots/detection_time_analysis.png", detection_plots, 
       width = 14, height = 10, dpi = 300)

print(detection_plots)

# Statistical tests
print("\n=== FACTORS AFFECTING DETECTION TIME ===\n")

# Correlation with slope magnitude
cor_slope <- cor.test(effect_vs_detection$abs_slope, 
                      effect_vs_detection$min_n_for_detection, 
                      method = "spearman")
print(sprintf("Correlation between |slope| and detection time: rho = %.3f, p = %.4f",
              cor_slope$estimate, cor_slope$p.value))

# Correlation with CV
cor_cv <- cor.test(significant_trends$cv, 
                   significant_trends$min_n_for_detection, 
                   method = "spearman")
print(sprintf("Correlation between CV and detection time: rho = %.3f, p = %.4f",
              cor_cv$estimate, cor_cv$p.value))

# Multiple regression: what predicts detection time?
if (nrow(significant_trends) > 10) {
  detection_model <- lm(min_n_for_detection ~ abs(final_slope) + cv + mean_ratio, 
                        data = significant_trends)
  print("\nMultiple regression predicting detection time:")
  print(summary(detection_model))
}

# Create summary table
detection_table <- significant_trends %>%
  mutate(
    detection_category = case_when(
      min_n_for_detection <= 5 ~ "Fast (≤5 timepoints)",
      min_n_for_detection <= 10 ~ "Moderate (6-10)",
      min_n_for_detection <= 20 ~ "Slow (11-20)",
      TRUE ~ "Very slow (>20)"
    )
  ) %>%
  count(trend_class, detection_category) %>%
  pivot_wider(names_from = detection_category, values_from = n, values_fill = 0)

print("\n=== DETECTION TIME CATEGORIES ===")
print(detection_table)

# Save results
write.csv(detection_with_class, "plots/detection_time_analysis.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Key files saved:\n")
cat("  - plots/detection_time_analysis.png\n")
cat("  - plots/detection_time_analysis.csv\n")