#' Simulate basket trial data
#'
#' @description
#' Generates simulated response data for a basket trial based on specified
#' response rates
#'
#' @param design A basket_design object or parameters for trial design
#' @param n_sims Integer. Number of simulation replicates
#' @param seed Integer. Random seed for reproducibility
#'
#' @return A list of basket_data objects, one for each simulation
#' @export
#'
#' @examples
#' design <- basket_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   response_rates = c(0.35, 0.35, 0.20, 0.20),
#'   null_response_rates = 0.20,
#'   design_type = "bma"
#' )
#' 
#' sims <- simulate_responses(design, n_sims = 100)
simulate_responses <- function(design, n_sims = 1000, seed = NULL) {
  
  if (!inherits(design, "basket_design")) {
    stop("design must be a basket_design object")
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generate simulation data
  sim_data <- lapply(seq_len(n_sims), function(i) {
    
    n_responses <- stats::rbinom(
      n = design$n_baskets,
      size = design$sample_sizes,
      prob = design$response_rates
    )
    
    basket_data(
      basket_names = design$basket_names,
      n_patients = design$sample_sizes,
      n_responses = n_responses
    )
  })
  
  # Add simulation metadata
  attr(sim_data, "n_sims") <- n_sims
  attr(sim_data, "design") <- design
  class(sim_data) <- c("basket_simulation", "list")
  
  return(sim_data)
}


#' Analyze simulated basket trial
#'
#' @description
#' Analyzes basket trial data using the specified design method
#'
#' @param data A basket_data object or list of basket_data objects
#' @param design A basket_design object specifying the analysis method
#' @param ... Additional arguments passed to the specific analysis function
#'
#' @return An analysis results object
#' @export
analyze_basket <- function(data, design, ...) {
  UseMethod("analyze_basket", design)
}


#' Basket design analyze method
#'
#' @param data A basket_data object
#' @param design A basket_design object
#' @param ... Additional arguments passed to method-specific functions
#' @export
analyze_basket.basket_design <- function(data, design, ...) {
  # Dispatch based on design_type
  switch(design$design_type,
    "cunanan" = analyze_basket.cunanan(data, design, ...),
    "bma" = analyze_basket.bma(data, design, ...),
    "mem" = analyze_basket.mem(data, design, ...),
    "bhm" = analyze_basket.bhm(data, design, ...),
    stop("No analysis method implemented for design_type: ", design$design_type)
  )
}

#' Default analyze_basket method
#'
#' @param data A basket_data object
#' @param design A design object
#' @param ... Additional arguments
#' @export
analyze_basket.default <- function(data, design, ...) {
  stop("No analysis method implemented for this design class")
}


#' Run complete simulation study
#'
#' @description
#' Runs a complete simulation study: generates data, analyzes with specified method,
#' and computes operating characteristics
#'
#' @param design A basket_design object
#' @param n_sims Integer. Number of simulation replicates
#' @param seed Integer. Random seed for reproducibility
#' @param alpha Numeric. Significance level for hypothesis testing
#' @param .parallelize Logical. If TRUE, use parallel processing via furrr/future.
#'   Default FALSE for backwards compatibility. Set a parallel plan with
#'   \code{future::plan()} before calling if TRUE (e.g., \code{future::plan(future::multisession)}).
#'   See \code{future} package documentation for details.
#' @param ... Additional arguments passed to analysis functions
#'
#' @return A simulation_results object containing operating characteristics
#' @export
#'
#' @examples
#' design <- basket_design(
#'   n_baskets = 4,
#'   basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
#'   sample_sizes = 25,
#'   response_rates = c(0.35, 0.35, 0.35, 0.35),
#'   null_response_rates = 0.20,
#'   design_type = "bma"
#' )
#' 
#' # Sequential execution (default)
#' # results <- simulate_basket_trial(design, n_sims = 1000)
#' 
#' # Parallel execution
#' # future::plan(future::multisession, workers = 4)
#' # results <- simulate_basket_trial(design, n_sims = 1000, .parallelize = TRUE)
#' # future::plan(future::sequential)  # Reset to sequential
#' 
#' # Enable progress reporting (works with both sequential and parallel)
#' # progressr::handlers(global = TRUE)  # Enable for all subsequent calls
#' # progressr::handlers("progress")     # Use progress bar handler
#' # results <- simulate_basket_trial(design, n_sims = 1000, .parallelize = TRUE)
#' # progressr::handlers(global = FALSE) # Disable progress reporting
simulate_basket_trial <- function(design, 
                                  n_sims = 1000, 
                                  seed = NULL,
                                  alpha = 0.05,
                                  .parallelize = FALSE,
                                  ...) {
  
  if (!inherits(design, "basket_design")) {
    stop("design must be a basket_design object")
  }
  
  # Check if this is a two-stage design (currently only Cunanan)
  is_twostage <- design$design_type == "cunanan"
  
  if (is_twostage) {
    # Use specialized two-stage simulation
    return(simulate_twostage_trial(design, n_sims, seed, alpha, .parallelize = .parallelize, ...))
  }
  
  # Single-stage simulation (original logic)
  # Generate simulated data
  message("Generating ", n_sims, " simulated trials...")
  sim_data <- simulate_responses(design, n_sims = n_sims, seed = seed)
  
  # Analyze each simulation
  message("Analyzing trials using ", design$design_type, " method...")
  
  # Set up progress reporting (only active if user has enabled handlers)
  p <- progressr::progressor(steps = n_sims)
  
  # Use parallel or sequential processing based on .parallelize flag
  if (.parallelize) {
    # Use furrr for parallel processing
    # Note: User must set future plan before calling (e.g., future::plan(future::multisession))
    results <- furrr::future_map(sim_data, function(dat) {
      result <- analyze_basket(dat, design, ...)
      p(sprintf("Completed simulation"))
      result
    }, .options = furrr::furrr_options(seed = TRUE))
  } else {
    # Sequential processing (backwards compatible)
    results <- lapply(sim_data, function(dat) {
      result <- analyze_basket(dat, design, ...)
      p(sprintf("Completed simulation"))
      result
    })
  }
  
  # Compute operating characteristics
  message("Computing operating characteristics...")
  oc <- compute_operating_characteristics(
    results = results,
    design = design,
    alpha = alpha
  )
  
  # Package results
  output <- structure(
    list(
      design = design,
      n_sims = n_sims,
      operating_characteristics = oc,
      raw_results = results,
      seed = seed
    ),
    class = "simulation_results"
  )
  
  return(output)
}


#' Simulate two-stage basket trial (e.g., Cunanan design)
#'
#' @description
#' Handles simulation for designs requiring two stages:
#' Stage 1: Initial enrollment and interim analysis
#' Stage 2: Conditional enrollment for continued baskets
#'
#' @param design A basket_design object (must be two-stage design)
#' @param n_sims Number of simulations
#' @param seed Random seed for reproducibility
#' @param alpha Significance level for hypothesis tests
#' @param .parallelize Logical. If TRUE, use parallel processing via furrr/future
#' @param ... Additional arguments passed to analyze_basket
#'
#' @return A simulation_results object
#' @keywords internal
simulate_twostage_trial <- function(design, n_sims, seed, alpha, .parallelize = FALSE, ...) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n_baskets <- design$n_baskets
  
  # For Cunanan, stage 1 is half the sample size
  stage1_n <- floor(design$sample_sizes / 2)
  stage2_n <- design$sample_sizes - stage1_n
  
  message("Generating ", n_sims, " simulated two-stage trials...")
  message("Stage 1: n = ", paste(stage1_n, collapse = ", "))
  message("Stage 2: n = ", paste(stage2_n, collapse = ", "), " (conditional)")
  
  # Set up progress reporting (only active if user has enabled handlers)
  p <- progressr::progressor(steps = n_sims)
  
  # Create a function for one simulation iteration
  simulate_one_trial <- function(i, design, n_baskets, stage1_n, stage2_n, p, ...) {
    # STAGE 1: Simulate initial enrollment
    stage1_data <- list(
      basket_names = design$basket_names,
      n_baskets = n_baskets,
      n_responses = rbinom(n_baskets, stage1_n, design$response_rates),
      n_patients = stage1_n
    )
    class(stage1_data) <- c("basket_data", "list")
    
    # Analyze stage 1 (interim)
    interim_result <- analyze_basket(stage1_data, design, ...)
    
    # Check if we have interim results (should have interim_stage = TRUE)
    # Note: If futility stopping occurs, interim_result will be a final result
    if (!is.null(interim_result$interim_stage) && interim_result$interim_stage) {
      
      # Determine which baskets continue
      continued <- interim_result$continued_baskets
      
      if (length(continued) == 0) {
        # No baskets continue - should not happen since futility returns final result
        return(interim_result)
      }
      
      # STAGE 2: Simulate additional enrollment for continued baskets only
      stage2_responses <- integer(n_baskets)
      stage2_responses[continued] <- rbinom(
        length(continued),
        stage2_n[continued],
        design$response_rates[continued]
      )
      
      # Combine stage 1 and stage 2 data
      final_data <- list(
        basket_names = design$basket_names,
        n_baskets = n_baskets,
        n_responses = stage1_data$n_responses + stage2_responses,
        n_patients = stage1_n + ifelse(seq_len(n_baskets) %in% continued, stage2_n, 0)
      )
      class(final_data) <- c("basket_data", "list")
      
      # Analyze final data (pass interim_data to indicate stage 2)
      final_result <- analyze_basket(final_data, design, interim_data = interim_result, ...)
      p(sprintf("Completed two-stage simulation"))
      return(final_result)
      
    } else {
      # Final result returned in stage 1 (e.g., futility stopping in homogeneous path)
      # This is correct behavior - use the result as-is
      p(sprintf("Completed two-stage simulation"))
      return(interim_result)
    }
  }
  
  # Run simulations in parallel or sequentially
  if (.parallelize) {
    results <- furrr::future_map(seq_len(n_sims), simulate_one_trial,
                                  design = design, n_baskets = n_baskets,
                                  stage1_n = stage1_n, stage2_n = stage2_n,
                                  p = p,
                                  ...,
                                  .options = furrr::furrr_options(seed = TRUE))
  } else {
    results <- lapply(seq_len(n_sims), simulate_one_trial,
                      design = design, n_baskets = n_baskets,
                      stage1_n = stage1_n, stage2_n = stage2_n,
                      p = p,
                      ...)
  }
  
  # Compute operating characteristics
  message("Computing operating characteristics...")
  oc <- compute_operating_characteristics(
    results = results,
    design = design,
    alpha = alpha
  )
  
  # Package results
  output <- structure(
    list(
      design = design,
      n_sims = n_sims,
      operating_characteristics = oc,
      raw_results = results,
      seed = seed
    ),
    class = "simulation_results"
  )
  
  return(output)
}


#' Compute operating characteristics from simulation results
#'
#' @description
#' Calculates type I error, power, FWER, etc. from simulation results
#'
#' @param results List of analysis results from simulations
#' @param design Original basket_design object
#' @param alpha Significance level
#'
#' @return A list containing operating characteristics
#' @export
compute_operating_characteristics <- function(results, design, alpha = 0.05) {
  
  n_baskets <- design$n_baskets
  
  # Determine which baskets are truly promising (alternative hypothesis)
  promising <- design$response_rates > design$null_response_rates
  
  # Extract rejection decisions from each simulation
  # This will be method-specific, so we'll create a generic extractor
  rejections <- extract_rejections(results, alpha)
  
  # Calculate basket-specific operating characteristics
  basket_oc <- lapply(seq_len(n_baskets), function(j) {
    basket_rejections <- sapply(rejections, function(r) r[j])
    
    if (promising[j]) {
      # Calculate power for promising baskets
      list(
        basket_name = design$basket_names[j],
        metric = "Power",
        value = mean(basket_rejections)
      )
    } else {
      # Calculate type I error for non-promising baskets
      list(
        basket_name = design$basket_names[j],
        metric = "Type I Error",
        value = mean(basket_rejections)
      )
    }
  })
  
  # Calculate family-wise operating characteristics
  fwer <- mean(sapply(rejections, function(r) any(r[!promising])))
  fwp_disjunctive <- mean(sapply(rejections, function(r) any(r[promising])))
  fwp_conjunctive <- mean(sapply(rejections, function(r) all(r[promising])))
  
  oc <- list(
    basket_specific = basket_oc,
    family_wise = list(
      fwer = fwer,
      fwp_disjunctive = fwp_disjunctive,
      fwp_conjunctive = fwp_conjunctive
    ),
    n_promising_baskets = sum(promising),
    n_nonpromising_baskets = sum(!promising)
  )
  
  return(oc)
}


#' Extract rejection decisions from analysis results
#'
#' @description
#' Generic function to extract which baskets reject the null hypothesis
#'
#' @param result Analysis result object (single result or list of results)
#' @param ... Additional arguments (not currently used)
#'
#' @return Logical vector indicating rejections for each basket, or list of such vectors
#' @export
extract_rejections <- function(result, ...) {
  UseMethod("extract_rejections")
}


#' Default extract_rejections method
#'
#' @param result Analysis result object
#' @param ... Additional arguments (not currently used)
#' @export
extract_rejections.default <- function(result, ...) {
  
  # Try to extract rejections field directly
  if (!is.null(result$rejections)) {
    return(result$rejections)
  }
  
  # Handle empty or NULL results
  if (is.null(result) || length(result) == 0) {
    return(NULL)
  }
  
  # If it's a list of results, apply recursively
  if (is.list(result)) {
    # Check if this looks like a single result object or a list of results
    if (!is.null(names(result)) && "rejections" %in% names(result)) {
      # Single result object with named components
      return(result$rejections)
    } else if (all(sapply(result, is.list))) {
      # List of results - extract from each
      return(lapply(result, extract_rejections))
    }
  }
  
  # Unable to extract - warn and return NULL
  warning("Cannot extract rejection decisions from results. Unknown result format.")
  return(NULL)
}


#' Print method for simulation_results
#'
#' @param x A simulation_results object
#' @param ... Additional arguments (not used)
#' @export
print.simulation_results <- function(x, ...) {
  cat("Basket Trial Simulation Results\n")
  cat("================================\n\n")
  cat("Design Type:", x$design$design_type, "\n")
  cat("Number of Simulations:", x$n_sims, "\n")
  cat("Number of Baskets:", x$design$n_baskets, "\n\n")
  
  cat("Operating Characteristics:\n")
  cat("--------------------------\n\n")
  
  cat("Basket-Specific:\n")
  for (i in seq_along(x$operating_characteristics$basket_specific)) {
    b <- x$operating_characteristics$basket_specific[[i]]
    cat(sprintf("  %s: %s = %.3f\n", b$basket_name, b$metric, b$value))
  }
  
  cat("\nFamily-Wise:\n")
  cat(sprintf("  FWER: %.3f\n", x$operating_characteristics$family_wise$fwer))
  cat(sprintf("  FWP (Disjunctive): %.3f\n", 
              x$operating_characteristics$family_wise$fwp_disjunctive))
  cat(sprintf("  FWP (Conjunctive): %.3f\n", 
              x$operating_characteristics$family_wise$fwp_conjunctive))
  
  invisible(x)
}
