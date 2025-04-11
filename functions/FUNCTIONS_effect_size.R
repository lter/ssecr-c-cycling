
#=========================#
# EFFECT SIZE CALCULATION
#=========================#

# Function to calculate standardized effect sizes
calculate_effect_sizes <- function(data) {
  # Group by relevant variables
  grouped_data <- data %>%
    filter(!is.na(is_treatment) & !is.na(response_value)) %>%
    group_by(site, experiment_type, year, treatment_type, response_variable, unit) 
  
  # Calculate mean, SD, and N for control and treatment groups
  effect_sizes <- grouped_data %>%
    summarize(
      control_mean = mean(response_value[is_treatment == 0], na.rm = TRUE),
      treatment_mean = mean(response_value[is_treatment == 1], na.rm = TRUE),
      control_sd = sd(response_value[is_treatment == 0], na.rm = TRUE),
      treatment_sd = sd(response_value[is_treatment == 1], na.rm = TRUE),
      control_n = sum(!is.na(response_value[is_treatment == 0])),
      treatment_n = sum(!is.na(response_value[is_treatment == 1])),
      .groups = "drop"
    ) %>%
    # Filter for groups that have both control and treatment data
    filter(control_n > 0 & treatment_n > 0 & 
             !is.na(control_mean) & !is.na(treatment_mean) & 
             !is.na(control_sd) & !is.na(treatment_sd) &
             control_sd > 0 & treatment_sd > 0)  # Avoid division by zero
  
  # Calculate effect sizes
  effect_sizes <- effect_sizes %>%
    rowwise() %>%
    mutate(
      # Calculate pooled standard deviation
      pooled_sd = sqrt(((control_n - 1) * control_sd^2 + (treatment_n - 1) * treatment_sd^2) / 
                         (control_n + treatment_n - 2)),
      
      # Calculate standardized mean difference (SMD)
      smd = (treatment_mean - control_mean) / pooled_sd,
      
      # Apply small sample size correction factor (Hedges' g)
      j = 1 - (3 / (4 * (control_n + treatment_n - 2) - 1)),
      hedges_g = j * smd,
      
      # Calculate variance of Hedges' g
      var_g = (control_n + treatment_n) / (control_n * treatment_n) + 
        hedges_g^2 / (2 * (control_n + treatment_n)),
      
      # Calculate standard error of Hedges' g
      se_g = sqrt(var_g),
      
      # Calculate 95% confidence intervals
      ci_lower = hedges_g - 1.96 * se_g,
      ci_upper = hedges_g + 1.96 * se_g,
      
      # Calculate log response ratio (lnRR)
      lnRR = log(treatment_mean / control_mean),
      
      # Calculate variance of lnRR
      var_lnRR = (control_sd^2 / (control_n * control_mean^2)) + 
        (treatment_sd^2 / (treatment_n * treatment_mean^2)),
      
      # Calculate standard error of lnRR
      se_lnRR = sqrt(var_lnRR),
      
      # Calculate 95% confidence intervals for lnRR
      lnRR_ci_lower = lnRR - 1.96 * se_lnRR,
      lnRR_ci_upper = lnRR + 1.96 * se_lnRR
    ) %>%
    ungroup()
  
  return(effect_sizes)
}
