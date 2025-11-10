#' Compare operating characteristics across designs
#'
#' @description
#' Compares operating characteristics from multiple simulation results
#'
#' @param ... simulation_results objects to compare
#' @param metrics Character vector of metrics to compare
#'
#' @return A comparison table
#' @export
compare_designs <- function(..., 
                           metrics = c("power", "type_i_error", "fwer")) {
  
  results_list <- list(...)
  
  # Validate inputs
  if (length(results_list) == 0) {
    stop("At least one simulation_results object must be provided")
  }
  
  for (i in seq_along(results_list)) {
    if (!inherits(results_list[[i]], "simulation_results")) {
      stop("All arguments must be simulation_results objects")
    }
  }
  
  # Extract design types
  design_types <- sapply(results_list, function(r) r$design$design_type)
  
  # Create comparison data frame
  comparison <- data.frame(
    Design = design_types,
    N_Baskets = sapply(results_list, function(r) r$design$n_baskets),
    N_Sims = sapply(results_list, function(r) r$n_sims)
  )
  
  # Add FWER
  comparison$FWER <- sapply(results_list, function(r) {
    r$operating_characteristics$family_wise$fwer
  })
  
  # Add FWP
  comparison$FWP_Disjunctive <- sapply(results_list, function(r) {
    r$operating_characteristics$family_wise$fwp_disjunctive
  })
  
  comparison$FWP_Conjunctive <- sapply(results_list, function(r) {
    r$operating_characteristics$family_wise$fwp_conjunctive
  })
  
  class(comparison) <- c("design_comparison", "data.frame")
  return(comparison)
}


#' Print method for design_comparison
#'
#' @param x A design_comparison object
#' @param ... Additional arguments (not used)
#' @export
print.design_comparison <- function(x, ...) {
  cat("Basket Trial Design Comparison\n")
  cat("==============================\n\n")
  
  print.data.frame(x, row.names = FALSE, digits = 3)
  
  invisible(x)
}


#' Create standard simulation scenarios
#'
#' @description
#' Creates pre-defined simulation scenarios commonly used in basket trial literature
#'
#' @param scenario Character. One of "global_null", "global_alternative", 
#'   "mixed_half", "mixed_heterogeneous"
#' @param n_baskets Integer. Number of baskets
#' @param null_rate Numeric. Null response rate
#' @param alt_rate Numeric. Alternative response rate
#'
#' @return A numeric vector of response rates
#' @export
#'
#' @examples
#' # Global null scenario (all baskets ineffective)
#' rates_null <- create_scenario("global_null", n_baskets = 4, null_rate = 0.20)
#' 
#' # Global alternative (all baskets effective)
#' rates_alt <- create_scenario("global_alternative", n_baskets = 4, 
#'                              null_rate = 0.20, alt_rate = 0.35)
create_scenario <- function(scenario = c("global_null", "global_alternative",
                                         "mixed_half", "mixed_heterogeneous"),
                           n_baskets = 4,
                           null_rate = 0.20,
                           alt_rate = 0.35) {
  
  scenario <- match.arg(scenario)
  
  rates <- switch(
    scenario,
    "global_null" = rep(null_rate, n_baskets),
    
    "global_alternative" = rep(alt_rate, n_baskets),
    
    "mixed_half" = c(
      rep(alt_rate, floor(n_baskets / 2)),
      rep(null_rate, ceiling(n_baskets / 2))
    ),
    
    "mixed_heterogeneous" = {
      promising <- floor(n_baskets / 2)
      c(
        seq(alt_rate, alt_rate * 1.3, length.out = promising),
        seq(null_rate * 0.5, null_rate, length.out = n_baskets - promising)
      )
    }
  )
  
  return(rates)
}


#' Calculate sample size for basket trial
#'
#' @description
#' Provides sample size recommendations based on desired operating characteristics
#'
#' @param n_baskets Integer. Number of baskets
#' @param null_rate Numeric. Null response rate
#' @param alt_rate Numeric. Alternative response rate  
#' @param power Numeric. Desired power (0-1)
#' @param alpha Numeric. Significance level
#' @param method Character. Design method to use
#'
#' @return Recommended sample size per basket
#' @export
#'
#' @examples
#' # Calculate sample size for BMA design
#' n <- calculate_sample_size(
#'   n_baskets = 4,
#'   null_rate = 0.20,
#'   alt_rate = 0.35,
#'   power = 0.80,
#'   alpha = 0.05,
#'   method = "bma"
#' )
calculate_sample_size <- function(n_baskets,
                                 null_rate,
                                 alt_rate,
                                 power = 0.80,
                                 alpha = 0.05,
                                 method = "bma") {
  
  # Simple heuristic for now
  # In practice, this would iterate simulations to find appropriate N
  
  # Effect size
  effect <- alt_rate - null_rate
  
  # Base calculation using single proportion test
  z_alpha <- stats::qnorm(1 - alpha)
  z_beta <- stats::qnorm(power)
  
  n_single <- ((z_alpha + z_beta)^2 * (alt_rate * (1 - alt_rate) + 
                                       null_rate * (1 - null_rate))) / effect^2
  
  # Adjust for borrowing (borrowing reduces required N)
  borrowing_factor <- switch(
    method,
    "bma" = 0.85,
    "mem" = 0.85,
    "bhm" = 0.80,
    "cunanan" = 0.90,
    1.0
  )
  
  n_recommended <- ceiling(n_single * borrowing_factor)
  
  message(sprintf(
    "Recommended sample size: %d patients per basket\n",
    n_recommended
  ))
  message(sprintf(
    "Total trial size: %d patients\n",
    n_recommended * n_baskets
  ))
  message(
    "Note: This is a preliminary estimate. ",
    "Conduct simulations to verify operating characteristics."
  )
  
  return(n_recommended)
}
