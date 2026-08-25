# preprocess_runner.R
# Orchestrator: reads the registry, downloads raw data, and runs preprocessing functions
# Each dataset has a named preprocessing function in R/preprocess/preprocess_{site}.R

source("R/pipeline/registry.R")
source("R/pipeline/download.R")

# =============================================================================
# SOURCE ALL PREPROCESS FUNCTIONS
# =============================================================================

#' Load all preprocessing function files from R/preprocess/
#' @param preprocess_dir Directory containing preprocess_*.R files
load_preprocess_functions <- function(preprocess_dir = "R/preprocess") {
  files <- list.files(preprocess_dir, pattern = "^preprocess_.*\\.R$", full.names = TRUE)
  if (length(files) == 0) {
    warning("No preprocessing files found in ", preprocess_dir)
    return(invisible(NULL))
  }
  for (f in files) {
    source(f, local = FALSE)
    cat("  Loaded:", basename(f), "\n")
  }
  invisible(files)
}

# =============================================================================
# PREPROCESS A SINGLE DATASET
# =============================================================================

#' Run preprocessing for a single dataset
#' @param dataset_id The dataset_id from the registry
#' @param registry The full registry data.frame
#' @param raw_dir Base directory for raw downloads
#' @param ready_dir Output directory for preprocessed files
#' @return Path to the output file, or NA on failure
preprocess_one <- function(dataset_id, registry, raw_dir = "data/raw",
                           ready_dir = "data/ready") {

  entry <- registry[registry$dataset_id == dataset_id, ]
  if (nrow(entry) == 0) stop("Dataset not found in registry: ", dataset_id)

  func_name <- entry$preprocess_function[1]
  ready_filename <- entry$ready_filename[1]

  # Check if the function exists
 if (!exists(func_name, mode = "function")) {
    cat("  WARNING: Function", func_name, "not found. Skipping.\n")
    return(NA)
  }

  # Get raw file path(s)
  # (Supports datasets with multiple registry rows sharing one ready_filename;
  # no current dataset uses this, but the mechanism is kept for generality.)
  entries_for_output <- registry[registry$ready_filename == ready_filename, ]

  raw_paths <- vapply(entries_for_output$dataset_id, function(did) {
    tryCatch(
      get_raw_path(did, raw_dir),
      error = function(e) NA_character_
    )
  }, character(1))

  raw_paths <- raw_paths[!is.na(raw_paths)]

  if (length(raw_paths) == 0) {
    cat("  No raw files found for", dataset_id, ". Download first.\n")
    return(NA)
  }

  # Call the preprocessing function
  # Convention: preprocess functions take raw_path(s) and return a data.frame
  # For single-input datasets: func(raw_path)
  # For multi-input datasets: func(raw_path1, raw_path2, ...)
  cat("  Running", func_name, "...\n")

  result <- tryCatch({
    if (length(raw_paths) == 1) {
      do.call(func_name, list(raw_paths[1]))
    } else {
      do.call(func_name, as.list(raw_paths))
    }
  }, error = function(e) {
    cat("  ERROR in", func_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })

  if (is.null(result)) return(NA)

  # Write output
  if (!dir.exists(ready_dir)) dir.create(ready_dir, recursive = TRUE)
  output_path <- file.path(ready_dir, ready_filename)
  write.csv(result, output_path, row.names = FALSE)
  cat("  Wrote:", ready_filename, "(", nrow(result), "rows )\n")

  output_path
}

# =============================================================================
# PREPROCESS ALL DATASETS
# =============================================================================

#' Run preprocessing for all included datasets in the registry
#' @param registry_path Path to dataset_registry.csv
#' @param raw_dir Base directory for raw downloads
#' @param ready_dir Output directory for preprocessed files
#' @param datasets Optional character vector of dataset_ids to process (NULL = all)
#' @return data.frame with dataset_id, ready_filename, status
preprocess_all <- function(registry_path = "data/dataset_registry.csv",
                           raw_dir = "data/raw",
                           ready_dir = "data/ready",
                           datasets = NULL) {

  registry <- read_registry(registry_path, include_only = TRUE)

  # Deduplicate by ready_filename: process each output file only once
  # (in case multiple registry rows ever share one output file)
  unique_outputs <- unique(registry$ready_filename)

  if (!is.null(datasets)) {
    # Filter to only requested datasets, but still process all inputs for their outputs
    requested_outputs <- unique(registry$ready_filename[registry$dataset_id %in% datasets])
    unique_outputs <- unique_outputs[unique_outputs %in% requested_outputs]
  }

  cat("=== PREPROCESSING", length(unique_outputs), "DATASETS ===\n\n")

  results <- data.frame(
    ready_filename = character(), dataset_ids = character(),
    status = character(), stringsAsFactors = FALSE
  )

  for (i in seq_along(unique_outputs)) {
    rf <- unique_outputs[i]
    # Find the first dataset_id that maps to this output (the one with the function)
    entries <- registry[registry$ready_filename == rf, ]
    primary_id <- entries$dataset_id[1]

    cat(sprintf("[%d/%d] %s\n", i, length(unique_outputs), rf))

    status <- tryCatch({
      output_path <- preprocess_one(primary_id, registry, raw_dir, ready_dir)
      if (is.na(output_path)) "SKIPPED" else "OK"
    }, error = function(e) {
      cat("  FAILED:", conditionMessage(e), "\n")
      paste("FAILED:", conditionMessage(e))
    })

    results <- rbind(results, data.frame(
      ready_filename = rf,
      dataset_ids = paste(entries$dataset_id, collapse = " + "),
      status = status,
      stringsAsFactors = FALSE
    ))
    cat("\n")
  }

  # Summary
  n_ok <- sum(results$status == "OK")
  n_skip <- sum(results$status == "SKIPPED")
  n_fail <- nrow(results) - n_ok - n_skip
  cat("=== PREPROCESSING COMPLETE ===\n")
  cat("  Success:", n_ok, "/", nrow(results), "\n")
  if (n_skip > 0) cat("  Skipped:", n_skip, "\n")
  if (n_fail > 0) {
    cat("  Failed:\n")
    failed <- results[results$status != "OK" & results$status != "SKIPPED", ]
    for (j in seq_len(nrow(failed))) {
      cat("    ", failed$ready_filename[j], ":", failed$status[j], "\n")
    }
  }

  invisible(results)
}

# =============================================================================
# FULL PIPELINE: DOWNLOAD + PREPROCESS
# =============================================================================

#' Run the full pipeline: download all datasets, then preprocess all
#' @param registry_path Path to dataset_registry.csv
#' @param raw_dir Base directory for raw downloads
#' @param manifest_dir Directory for manifests
#' @param ready_dir Output directory for preprocessed files
#' @param datasets Optional character vector of dataset_ids (NULL = all)
#' @param force_download Re-download even if cached
#' @return List with download_results and preprocess_results
run_pipeline <- function(registry_path = "data/dataset_registry.csv",
                         raw_dir = "data/raw",
                         manifest_dir = "data/manifests",
                         ready_dir = "data/ready",
                         datasets = NULL,
                         force_download = FALSE) {

  cat("============================================================\n")
  cat("  SSECR C-CYCLING REPRODUCIBLE DATA PIPELINE\n")
  cat("============================================================\n\n")

  # Step 1: Load preprocessing functions
  cat("--- Loading preprocessing functions ---\n")
  load_preprocess_functions()
  cat("\n")

  # Step 2: Download
  cat("--- Downloading raw data ---\n")
  dl_results <- download_all(
    registry_path = registry_path,
    raw_dir = raw_dir,
    manifest_dir = manifest_dir,
    datasets = datasets,
    force = force_download
  )
  cat("\n")

  # Step 3: Preprocess
  cat("--- Preprocessing datasets ---\n")
  pp_results <- preprocess_all(
    registry_path = registry_path,
    raw_dir = raw_dir,
    ready_dir = ready_dir,
    datasets = datasets
  )
  cat("\n")

  cat("============================================================\n")
  cat("  PIPELINE COMPLETE\n")
  cat("============================================================\n")

  invisible(list(
    download_results = dl_results,
    preprocess_results = pp_results
  ))
}
