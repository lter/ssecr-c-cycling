#=========================#
# META-ANALYSIS FUNCTIONS
#=========================#

# Function to perform meta-analysis for each experiment type and response variable
meta_analysis <- function(effect_data) {
  # Create lists to store results
  meta_results <- list()
  
  # Group data by experiment type and response variable
  grouped_data <- effect_data %>%
    group_by(experiment_type, response_variable) %>%
    group_split()
  
  # Loop through each group and perform meta-analysis
  for(i in seq_along(grouped_data)) {
    group <- grouped_data[[i]]
    
    # Skip if group is too small
    if(nrow(group) < 3) next
    
    # Extract grouping variables
    exp_type <- unique(group$experiment_type)
    resp_var <- unique(group$response_variable)
    
    # Meta-analysis using Hedges' g
    meta_g <- try({
      rma(yi = hedges_g, vi = var_g, data = group,
          method = "REML", slab = paste(site, year, sep = "_"))
    }, silent = TRUE)
    
    # Meta-analysis using lnRR
    meta_lnRR <- try({
      rma(yi = lnRR, vi = var_lnRR, data = group,
          method = "REML", slab = paste(site, year, sep = "_"))
    }, silent = TRUE)
    
    # Store results if analyses were successful
    result <- list(
      experiment_type = exp_type,
      response_variable = resp_var,
      k = nrow(group),
      meta_g = if(!inherits(meta_g, "try-error")) meta_g else NULL,
      meta_lnRR = if(!inherits(meta_lnRR, "try-error")) meta_lnRR else NULL
    )
    
    meta_results[[length(meta_results) + 1]] <- result
  }
  
  return(meta_results)
}

# Function to extract and summarize meta-analysis results
summarize_meta_results <- function(meta_results) {
  result_df <- data.frame(
    experiment_type = character(),
    response_variable = character(),
    k = integer(),
    effect_measure = character(),
    overall_effect = numeric(),
    se = numeric(),
    pval = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    tau2 = numeric(),
    i2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(res in meta_results) {
    # Process Hedges' g results
    if(!is.null(res$meta_g)) {
      g_row <- data.frame(
        experiment_type = res$experiment_type,
        response_variable = res$response_variable,
        k = res$k,
        effect_measure = "Hedges_g",
        overall_effect = res$meta_g$b[1],
        se = res$meta_g$se,
        pval = res$meta_g$pval,
        ci_lower = res$meta_g$ci.lb,
        ci_upper = res$meta_g$ci.ub,
        tau2 = res$meta_g$tau2,
        i2 = res$meta_g$I2,
        stringsAsFactors = FALSE
      )
      result_df <- rbind(result_df, g_row)
    }
    
    # Process lnRR results
    if(!is.null(res$meta_lnRR)) {
      lnRR_row <- data.frame(
        experiment_type = res$experiment_type,
        response_variable = res$response_variable,
        k = res$k,
        effect_measure = "lnRR",
        overall_effect = res$meta_lnRR$b[1],
        se = res$meta_lnRR$se,
        pval = res$meta_lnRR$pval,
        ci_lower = res$meta_lnRR$ci.lb,
        ci_upper = res$meta_lnRR$ci.ub,
        tau2 = res$meta_lnRR$tau2,
        i2 = res$meta_lnRR$I2,
        stringsAsFactors = FALSE
      )
      result_df <- rbind(result_df, lnRR_row)
    }
  }
  
  return(result_df)
}

# Function to create forest plots for meta-analysis results
create_forest_plots <- function(effect_sizes, meta_results) {
  # Loop through meta-analysis results and create forest plots
  for(i in seq_along(meta_results)) {
    res <- meta_results[[i]]
    
    # Skip if no results or not significant
    if(is.null(res$meta_g) || res$meta_g$pval >= 0.05) next
    
    # Prepare data for forest plot
    exp_type <- res$experiment_type
    resp_var <- res$response_variable
    
    # Filter relevant effect sizes
    plot_data <- effect_sizes %>%
      filter(experiment_type == exp_type, response_variable == resp_var)
    
    # Create forest plot using ggplot2
    forest_g <- ggplot(plot_data, aes(x = hedges_g, y = paste(site, year))) +
      geom_point(aes(size = 1/sqrt(var_g))) +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = res$meta_g$b[1], color = "red") +
      geom_rect(aes(xmin = res$meta_g$ci.lb, xmax = res$meta_g$ci.ub, 
                    ymin = -Inf, ymax = Inf), 
                fill = "red", alpha = 0.1, inherit.aes = FALSE) +
      labs(
        title = paste("Forest Plot -", exp_type),
        subtitle = paste("Response Variable:", resp_var),
        x = "Hedges' g (95% CI)",
        y = "",
        caption = paste("Overall effect =", round(res$meta_g$b[1], 3), 
                        "(95% CI:", round(res$meta_g$ci.lb, 3), "to", 
                        round(res$meta_g$ci.ub, 3), "), p =", round(res$meta_g$pval, 4))
      ) +
      theme_bw() +
      theme(legend.position = "none")
    
    # Save forest plot
    filename <- paste0("forest_plot_", gsub(" ", "_", exp_type), "_", 
                       gsub(" ", "_", resp_var), ".png")
    
    # Save to the data directory
    ggsave(filename, forest_g, width = 10, height = 8)
  }
}
