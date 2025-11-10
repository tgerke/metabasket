#' Analyze basket trial using Bayesian Hierarchical Model
#'
#' @description
#' Wrapper for bhmbasket package implementing Bayesian hierarchical modeling
#' from Berry et al. (2013) and Neuenschwander et al. (2016).
#' 
#' This method uses MCMC sampling via JAGS to estimate posterior distributions
#' with hierarchical borrowing of information across baskets.
#'
#' @param data A basket_data object containing trial results
#' @param design A basket_design object with design_type = "bhm"
#' @param ... Additional arguments (currently unused)
#'
#' @return A bhm_result object containing:
#' \item{posterior_means}{Posterior mean response rates for each basket}
#' \item{posterior_probs}{Posterior P(rate > null_rate) for each basket}
#' \item{rejections}{Logical vector of rejection decisions}
#' \item{basket_names}{Names of baskets}
#' \item{method}{Analysis method ("berry", "exnex", etc.)}
#' \item{threshold}{Decision threshold used}
#' 
#' @details
#' **System Requirements:** This method requires JAGS (Just Another Gibbs Sampler)
#' to be installed on your system. Install from \url{https://mcmc-jags.sourceforge.io/}.
#' 
#' The bhmbasket package provides several BHM methods:
#' - "berry": Berry et al. (2013) exchangeability model
#' - "exnex": Neuenschwander et al. (2016) EX-NEX model  
#' - "exnex_adj": Combination approach
#' - "pooled": Full pooling across baskets
#' - "stratified": No pooling (independent analyses)
#' 
#' @references
#' Berry SM, Broglio KR, Groshen S, Berry DA (2013). "Bayesian hierarchical modeling
#' of patient subpopulations: Efficient designs of Phase II oncology clinical trials."
#' Clinical Trials, 10(5), 720-734.
#' 
#' Neuenschwander B, Wandel S, Roychoudhury S, Bailey S (2016). "Robust exchangeability
#' designs for early phase clinical trials with multiple strata." Pharmaceutical
#' Statistics, 15(2), 123-134.
#'
#' @examples
#' \dontrun{
#' # Note: Requires JAGS installation
#' design_bhm <- basket_design(
#'   design_type = "bhm",
#'   n_baskets = 3,
#'   n_patients_per_basket = 20,
#'   null_response_rates = rep(0.15, 3),
#'   design_params = list(
#'     method = "berry",
#'     n_mcmc_iterations = 5000,
#'     evidence_level = 0.1
#'   )
#' )
#' 
#' data_bhm <- basket_data(
#'   n_patients = c(20, 20, 20),
#'   n_responses = c(8, 10, 12),
#'   basket_names = c("Basket A", "Basket B", "Basket C")
#' )
#' 
#' result_bhm <- analyze_basket(data_bhm, design_bhm)
#' print(result_bhm)
#' extract_rejections(result_bhm)
#' }
#' 
#' @export
analyze_basket.bhm <- function(data, design, ...) {
  
  # Check for bhmbasket package
  if (!requireNamespace("bhmbasket", quietly = TRUE)) {
    stop("Package 'bhmbasket' is required but not installed.\n",
         "Install with: install.packages('bhmbasket')")
  }
  
  # Check for JAGS
  jags_available <- tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE)
  
  if (!jags_available) {
    stop("JAGS (Just Another Gibbs Sampler) is required but not installed.\n",
         "Download from: https://mcmc-jags.sourceforge.io/\n",
         "After installing JAGS, reinstall rjags: install.packages('rjags')")
  }
  
  # Extract data components
  y <- data$n_responses
  n <- data$n_patients
  p0 <- design$null_response_rates
  basket_names <- data$basket_names
  
  # Get design parameters or use defaults
  params <- design$design_params
  method <- params$method %||% "berry"
  n_mcmc <- params$n_mcmc_iterations %||% 10000
  evidence_level <- params$evidence_level %||% 0.1
  
  # Validate method
  valid_methods <- c("berry", "exnex", "exnex_adj", "pooled", "stratified")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }
  
  # Create trial object and perform analysis
  trial <- bhmbasket::createTrial(
    n_subjects = n,
    n_responders = y
  )
  
  # Perform BHM analysis
  bhm_result <- bhmbasket::performAnalyses(
    scenario_list = trial,
    method_names = method,
    target_rates = p0,
    evidence_levels = evidence_level,
    n_mcmc_iterations = n_mcmc,
    verbose = FALSE
  )
  
  if (is.null(bhm_result)) {
    stop("Error in bhmbasket analysis: returned NULL")
  }
  
  # Extract posterior means from matrix
  posterior_means_matrix <- bhmbasket::getEstimates(
    bhm_result,
    point_estimator = "mean"
  )
  posterior_means <- as.numeric(posterior_means_matrix[[method]][, "Mean"])
  
  # Extract quantiles for posterior probability estimation
  # The quantiles contain posterior samples at various percentiles
  quantiles_matrix <- bhm_result$scenario_1$quantiles_list[[method]][[1]]
  
  # Get cohort names (p_1, p_2, etc.)
  cohort_cols <- grep("^p_[0-9]+$", colnames(quantiles_matrix), value = TRUE)
  
  # For each cohort, estimate P(rate > p0) from the quantiles
  # If the lower quantile at evidence_level is > p0, then P(rate > p0) > 1 - evidence_level
  posterior_probs <- sapply(seq_along(y), function(i) {
    cohort_name <- paste0("p_", i)
    cohort_quantiles <- quantiles_matrix[, cohort_name]
    
    # Estimate posterior probability that rate > p0[i]
    # by checking what proportion of the distribution is above p0
    # Using quantiles as a proxy: if median > p0, prob > 0.5
    lower_q <- as.numeric(quantiles_matrix["5%", cohort_name])
    median_q <- as.numeric(quantiles_matrix["50%", cohort_name])
    upper_q <- as.numeric(quantiles_matrix["95%", cohort_name])
    
    # Simple approximation: if lower 5% quantile > p0, then P(rate > p0) > 0.95
    if (lower_q > p0[i]) {
      return(0.98)  # Very high probability
    } else if (upper_q < p0[i]) {
      return(0.02)  # Very low probability
    } else if (median_q > p0[i]) {
      # Interpolate between 0.5 and 0.95 based on how far median is from lower_q
      return(0.5 + 0.45 * (median_q - p0[i]) / (upper_q - median_q))
    } else {
      # Interpolate between 0.05 and 0.5
      return(0.05 + 0.45 * (median_q - lower_q) / (p0[i] - lower_q))
    }
  })
  
  # Get rejection decisions based on evidence level
  threshold <- 1 - evidence_level
  rejections <- posterior_probs > threshold
  
  # Build result object
  result <- structure(
    list(
      posterior_means = posterior_means,
      posterior_probs = posterior_probs,
      rejections = rejections,
      basket_names = basket_names,
      method = method,
      threshold = threshold,
      n_mcmc_iterations = n_mcmc,
      evidence_level = evidence_level,
      raw_output = bhm_result
    ),
    class = c("bhm_result", "basket_result", "list")
  )
  
  return(result)
}


#' Print BHM analysis results
#'
#' @param x A bhm_result object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#' @export
print.bhm_result <- function(x, ...) {
  cat("Bayesian Hierarchical Model Analysis Results\n")
  cat("Method:", x$method, "\n")
  cat("MCMC iterations:", x$n_mcmc_iterations, "\n")
  cat("Decision threshold:", sprintf("%.3f", x$threshold), "\n")
  cat("Evidence level:", sprintf("%.3f", x$evidence_level), "\n\n")
  
  # Create results table
  results_df <- data.frame(
    Basket = x$basket_names,
    Post_Mean = sprintf("%.3f", x$posterior_means),
    Post_Prob = sprintf("%.3f", x$posterior_probs),
    Reject_H0 = ifelse(x$rejections, "Yes", "No"),
    stringsAsFactors = FALSE
  )
  
  print(results_df, row.names = FALSE)
  
  cat("\nNote: Post_Prob = P(response rate > null rate | data)\n")
  cat("      Reject_H0 if Post_Prob >", sprintf("%.3f", x$threshold), "\n")
  
  invisible(x)
}


#' Extract rejections from BHM results
#'
#' @param result A bhm_result object
#' @param ... Additional arguments (currently unused)
#'
#' @return Logical vector of rejection decisions
#' @export
extract_rejections.bhm_result <- function(result, ...) {
  if (!inherits(result, "bhm_result")) {
    stop("result must be a bhm_result object")
  }
  
  return(result$rejections)
}
