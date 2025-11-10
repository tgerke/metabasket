## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(metabasket)
library(future)

# Set up parallel processing for faster simulations
# Use all but 1 core (leave 1 for system)
plan(multisession, workers = max(1, availableCores() - 1))

## ----scenario_setup-----------------------------------------------------------
# Common parameters
n_baskets <- 4
sample_size_per_basket <- 20
p0 <- 0.20  # Null response rate (all baskets)
p1 <- 0.35  # Alternative response rate

# Scenario 1: Global Null
# All baskets non-promising
scenario_1 <- list(
  name = "Global Null",
  response_rates = rep(0.20, n_baskets),
  promising = rep(FALSE, n_baskets)
)

# Scenario 2: Global Alternative  
# All baskets promising
scenario_2 <- list(
  name = "Global Alternative",
  response_rates = rep(0.35, n_baskets),
  promising = rep(TRUE, n_baskets)
)

# Scenario 3: Mixed (1 null, 3 alternative)
scenario_3 <- list(
  name = "Mixed (1/4 null)",
  response_rates = c(0.20, 0.35, 0.35, 0.35),
  promising = c(FALSE, TRUE, TRUE, TRUE)
)

# Scenario 4: Mixed (2 null, 2 alternative)
scenario_4 <- list(
  name = "Mixed (2/4 null)",
  response_rates = c(0.20, 0.20, 0.35, 0.35),
  promising = c(FALSE, FALSE, TRUE, TRUE)
)

# Scenario 5: Mixed (gradient response rates)
scenario_5 <- list(
  name = "Mixed (gradient)",
  response_rates = c(0.10, 0.20, 0.30, 0.40),
  promising = c(FALSE, FALSE, TRUE, TRUE)
)

# Scenario 6: Mixed (1 alternative, 3 null)
scenario_6 <- list(
  name = "Mixed (1/4 alternative)",
  response_rates = c(0.20, 0.20, 0.20, 0.35),
  promising = c(FALSE, FALSE, FALSE, TRUE)
)

scenarios <- list(scenario_1, scenario_2, scenario_3, 
                  scenario_4, scenario_5, scenario_6)

## ----design_setup-------------------------------------------------------------
basket_names <- paste0("Basket", 1:n_baskets)

# Function to create design for a scenario
# Currently demonstrates Cunanan, Simon, and BMA (all implemented)
create_designs <- function(scenario) {
  list(
    cunanan = basket_design(
      n_baskets = n_baskets,
      basket_names = basket_names,
      sample_sizes = sample_size_per_basket,
      response_rates = scenario$response_rates,
      null_response_rates = p0,
      design_type = "cunanan"
    ),
    simon = simon_design(
      n_baskets = n_baskets,
      basket_names = basket_names,
      sample_sizes = sample_size_per_basket,
      null_response_rates = p0,
      alternative_response_rates = p1,
      alpha = 0.05 / n_baskets,  # Bonferroni correction for FWER control
      beta = 0.20
    ),
    bma = basket_design(
      n_baskets = n_baskets,
      basket_names = basket_names,
      sample_sizes = sample_size_per_basket,
      null_response_rates = p0,
      design_type = "bma",
      design_params = list(
        mu0 = 0.5,          # Prior mean
        phi0 = 1,           # Prior dispersion
        pmp0 = 1,           # Model probability parameter
        post_prob_threshold = 0.95
      )
    )
  )
}

# When MEM/BHM are integrated, add them:
# mem = basket_design(..., design_type = "mem"),
# bhm = basket_design(..., design_type = "bhm")

## ----design_comparison_example------------------------------------------------
# Common parameters
n_baskets <- 4
basket_names <- paste0("Basket", 1:4)

# Cunanan design (information borrowing via interim heterogeneity test)
design_cunanan <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  response_rates = rep(0.35, 4),  
  null_response_rates = 0.20,
  design_type = "cunanan"
)

# Simon design (independent analysis, no borrowing)
design_simon <- simon_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  null_response_rates = 0.20,
  alternative_response_rates = 0.35,
  alpha = 0.05 / 4,  # Bonferroni for FWER control
  beta = 0.20
)

# BMA design (Bayesian model averaging over partitions)
design_bma <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  null_response_rates = 0.20,
  design_type = "bma",
  design_params = list(
    mu0 = 0.5,  # Prior mean
    phi0 = 1,   # Prior dispersion
    post_prob_threshold = 0.95
  )
)

# MEM design (Multi-source exchangeability model)
design_mem <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  null_response_rates = 0.20,
  design_type = "mem",
  design_params = list(
    shape1 = 0.5,  # Beta prior shape1
    shape2 = 0.5,  # Beta prior shape2
    hpd_alpha = 0.05
  )
)

# BHM design (Bayesian hierarchical model - requires JAGS)
# Note: This will only run if JAGS is installed on your system
design_bhm <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  null_response_rates = 0.20,
  design_type = "bhm",
  design_params = list(
    method = "berry",          # Berry et al. (2013) model
    n_mcmc_iterations = 5000,  # Reduced for vignette speed
    evidence_level = 0.1
  )
)

# Show designs
print(design_cunanan)
print(design_simon)
print(design_bma)
print(design_mem)
print(design_bhm)

## ----simulate_example---------------------------------------------------------
# Generate some example data
set.seed(2024)

# Global alternative scenario (all baskets promising)
sims_global_alt <- simulate_responses(design_cunanan, n_sims = 5, seed = 100)

# Show first simulation
sims_global_alt[[1]]

# Mixed scenario (only 1 basket promising)
design_mixed <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  response_rates = c(0.20, 0.20, 0.20, 0.35),  # Only basket 4 promising
  null_response_rates = 0.20,
  design_type = "cunanan"
)

sims_mixed <- simulate_responses(design_mixed, n_sims = 5, seed = 200)

# Show first simulation from mixed scenario
sims_mixed[[1]]

## ----analyze_example----------------------------------------------------------
# Analyze one trial with Cunanan method
result_cunanan <- analyze_basket(sims_global_alt[[1]], design_cunanan)
print(result_cunanan)

# Analyze same data with Simon method
result_simon <- analyze_basket(sims_global_alt[[1]], design_simon)
print(result_simon)

# Analyze same data with BMA method
library(bmabasket)
result_bma <- analyze_basket(sims_global_alt[[1]], design_bma)
print(result_bma)

# Analyze same data with MEM method
library(basket)
result_mem <- analyze_basket(sims_global_alt[[1]], design_mem)
print(result_mem)

# Analyze same data with BHM method (if JAGS is available)
if (requireNamespace("rjags", quietly = TRUE)) {
  library(bhmbasket)
  result_bhm <- analyze_basket(sims_global_alt[[1]], design_bhm)
  print(result_bhm)
  rejections_bhm <- extract_rejections(result_bhm)
} else {
  rejections_bhm <- NA
}

# Extract rejection decisions
rejections_cunanan <- extract_rejections(result_cunanan)
rejections_simon <- extract_rejections(result_simon)
rejections_bma <- extract_rejections(result_bma)
rejections_mem <- extract_rejections(result_mem)

# Create table of rejection decisions
rejection_table <- data.frame(
  Method = c("Cunanan", "Simon", "BMA", "MEM"),
  Basket_1 = c(rejections_cunanan[1], rejections_simon[1], rejections_bma[1], rejections_mem[1]),
  Basket_2 = c(rejections_cunanan[2], rejections_simon[2], rejections_bma[2], rejections_mem[2]),
  Basket_3 = c(rejections_cunanan[3], rejections_simon[3], rejections_bma[3], rejections_mem[3]),
  Basket_4 = c(rejections_cunanan[4], rejections_simon[4], rejections_bma[4], rejections_mem[4])
)
if (!all(is.na(rejections_bhm))) {
  rejection_table <- rbind(rejection_table, 
    data.frame(Method = "BHM", Basket_1 = rejections_bhm[1], Basket_2 = rejections_bhm[2],
               Basket_3 = rejections_bhm[3], Basket_4 = rejections_bhm[4]))
}
knitr::kable(rejection_table, caption = "Rejection Decisions by Method (0=Do not reject H0, 1=Reject H0)")

## ----compute_ocs_small, message=FALSE, warning=FALSE--------------------------
# Example with 500 simulations (use n_sims >= 1000 for publication)
# Note: Smaller n_sims may show Monte Carlo variation in estimates
set.seed(2024)

# Create Cunanan design for global alternative
design_for_ocs <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  response_rates = rep(0.35, 4),  # All promising
  null_response_rates = 0.20,
  design_type = "cunanan",
  design_params = list(gamma = 0.52, alpha_s = 0.07, alpha_c = 0.05)
)

# Simulate trial data
# simulate_basket_trial automatically computes operating characteristics
sim_results <- suppressMessages(
  simulate_basket_trial(design_for_ocs, n_sims = 250, seed = 123, .parallelize = TRUE)
)

# Extract operating characteristics
ocs <- sim_results$operating_characteristics

# Display family-wise operating characteristics as a table
# Note: FWER is 0% here because all baskets are promising (global alternative)
# FWER only applies when there are non-promising baskets
fw_ocs <- data.frame(
  Metric = c("FWP-D (detect ≥1 promising)", "FWP-C (detect all promising)"),
  Value = sprintf("%.1f%%", c(
    ocs$family_wise$fwp_disjunctive * 100,
    ocs$family_wise$fwp_conjunctive * 100
  ))
)
knitr::kable(fw_ocs, caption = "Family-wise Power (Cunanan Design, Global Alternative)")

# Display basket-specific operating characteristics as a table
basket_ocs <- do.call(rbind, lapply(ocs$basket_specific, function(boc) {
  data.frame(
    Basket = boc$basket_name,
    Metric = boc$metric,
    Value = sprintf("%.1f%%", boc$value * 100)
  )
}))
knitr::kable(basket_ocs, caption = "Basket-specific Operating Characteristics (Cunanan Design)", row.names = FALSE)

## ----fwer_example, message=FALSE, warning=FALSE-------------------------------
# Global null: all baskets non-promising
design_null <- basket_design(
  n_baskets = 4,
  basket_names = basket_names,
  sample_sizes = 20,
  response_rates = rep(0.20, 4),  # All null
  null_response_rates = 0.20,
  design_type = "cunanan",
  design_params = list(gamma = 0.52, alpha_s = 0.07, alpha_c = 0.05)
)

sim_null <- suppressMessages(
  simulate_basket_trial(design_null, n_sims = 250, seed = 456, .parallelize = TRUE)
)

fwer_result <- data.frame(
  Scenario = "Global Null (4/4 non-promising)",
  FWER = sprintf("%.1f%%", sim_null$operating_characteristics$family_wise$fwer * 100),
  Target = "≤ 5.0%"
)
knitr::kable(fwer_result, caption = "FWER Control Assessment")

## ----all_methods_comparison, eval=FALSE---------------------------------------
# # Use global alternative scenario
# set.seed(2024)
# n_sims_demo <- 50  # Small for vignette speed
# 
# # Use the Cunanan design created earlier in this chunk
# sims_demo <- simulate_responses(design_cunanan_oc, n_sims = n_sims_demo, seed = 500)
# 
# all_methods_ocs <- list()
# 
# # Cunanan
# all_methods_ocs$Cunanan <- compute_rejection_rates(sims_demo, design_cunanan_oc, "Cunanan")
# 
# # Simon
# all_methods_ocs$Simon <- compute_rejection_rates(sims_demo, design_simon_oc, "Simon")
# 
# # BMA - create design
# suppressPackageStartupMessages(library(bmabasket))
# design_bma_oc <- basket_design(
#   n_baskets = 4,
#   basket_names = basket_names_oc,
#   sample_sizes = 20,
#   null_response_rates = 0.20,
#   design_type = "bma",
#   design_params = list(mu0 = 0.5, phi0 = 1, post_prob_threshold = 0.95)
# )
# all_methods_ocs$BMA <- compute_rejection_rates(sims_demo, design_bma_oc, "BMA")
# 
# # MEM - create design
# suppressPackageStartupMessages(library(basket))
# design_mem_oc <- basket_design(
#   n_baskets = 4,
#   basket_names = basket_names_oc,
#   sample_sizes = 20,
#   null_response_rates = 0.20,
#   design_type = "mem",
#   design_params = list(shape1 = 0.5, shape2 = 0.5, hpd_alpha = 0.05)
# )
# all_methods_ocs$MEM <- compute_rejection_rates(sims_demo, design_mem_oc, "MEM")
# 
# # BHM (if JAGS available) - create design
# if (requireNamespace("rjags", quietly = TRUE)) {
#   suppressPackageStartupMessages(library(bhmbasket))
#   design_bhm_oc <- basket_design(
#     n_baskets = 4,
#     basket_names = basket_names_oc,
#     sample_sizes = 20,
#     null_response_rates = 0.20,
#     design_type = "bhm",
#     design_params = list(method = "berry", n_mcmc_iterations = 1000, evidence_level = 0.1)
#   )
#   all_methods_ocs$BHM <- compute_rejection_rates(sims_demo, design_bhm_oc, "BHM")
# } else {
#   all_methods_ocs$BHM <- data.frame(
#     Method = "BHM (not run)",
#     Basket_1 = NA, Basket_2 = NA, Basket_3 = NA, Basket_4 = NA, FWER = NA
#   )
# }
# 
# # Combine into single table
# all_methods_table <- do.call(rbind, all_methods_ocs)
# rownames(all_methods_table) <- NULL
# 
# knitr::kable(
#   all_methods_table,
#   digits = 3,
#   caption = paste0("Five-Method Comparison: Global Alternative Scenario (n_sims = ", n_sims_demo, ")"),
#   col.names = c("Method", "Basket 1", "Basket 2", "Basket 3", "Basket 4", "FWER"),
#   align = c("l", rep("r", 5))
# )

## ----observed_ocs, message=FALSE, warning=FALSE-------------------------------
# Run simulations to demonstrate package functionality
# Note: Use n_sims >= 1000 for stable estimates; smaller values show Monte Carlo variation
set.seed(2024)
n_sims_verify <- 1000  # Sufficient for reasonable estimates

# Scenario 1: Global Alternative (all baskets promising)
design_global <- basket_design(
  n_baskets = 4, basket_names = basket_names, sample_sizes = 20,
  response_rates = rep(0.35, 4), null_response_rates = 0.20,
  design_type = "cunanan",
  design_params = list(gamma = 0.52, alpha_s = 0.07, alpha_c = 0.05)
)

# Scenario 2: Mixed (1/4 promising)
design_mixed <- basket_design(
  n_baskets = 4, basket_names = basket_names, sample_sizes = 20,
  response_rates = c(0.35, 0.20, 0.20, 0.20), null_response_rates = 0.20,
  design_type = "cunanan",
  design_params = list(gamma = 0.52, alpha_s = 0.07, alpha_c = 0.05)
)

# Simulate and extract OCs
sim_global <- suppressMessages(simulate_basket_trial(design_global, n_sims = n_sims_verify, seed = 100, .parallelize = TRUE))
sim_mixed <- suppressMessages(simulate_basket_trial(design_mixed, n_sims = n_sims_verify, seed = 200, .parallelize = TRUE))

# Extract power (for promising baskets) and FWER
ocs_global <- sim_global$operating_characteristics
ocs_mixed <- sim_mixed$operating_characteristics

# For global: average power across all baskets (FWER is N/A - no null baskets)
power_global <- mean(sapply(ocs_global$basket_specific, function(x) 
  if(x$metric == "Power") x$value else NA), na.rm = TRUE) * 100

# For mixed: power for first basket (promising) and Type I error for others
power_mixed <- ocs_mixed$basket_specific[[1]]$value * 100
type1_mixed <- mean(sapply(ocs_mixed$basket_specific[2:4], function(x) x$value)) * 100

observed_ocs <- data.frame(
  Scenario = c("Global Alternative (4/4 promising)", "Mixed (1/4 promising)"),
  Design = c("Cunanan", "Cunanan"),
  `Average Power` = sprintf("%.1f%%", c(power_global, power_mixed)),
  `FWER or Avg Type I Error` = c(
    "N/A (no null baskets)",
    sprintf("%.1f%% (FWER)", ocs_mixed$family_wise$fwer * 100)
  ),
  check.names = FALSE
)

knitr::kable(
  observed_ocs,
  caption = paste0("Observed Operating Characteristics from Package (n_sims = ", n_sims_verify, ", Cunanan design)"),
  align = c("l", "l", "r", "l")
)

## ----illustrative_table-------------------------------------------------------
# Illustrative comparison based on published results
illustrative_ocs <- data.frame(
  Scenario = rep(c("Global Alternative", "Mixed (1/4 promising)"), each = 5),
  Design = rep(c("Cunanan", "Simon", "BMA", "MEM", "BHM"), 2),
  `Expected Power` = c(
    "75-85%", "50-60%", "80-90%", "80-90%", "80-90%",  # Global alt: Bayesian methods & Cunanan benefit from borrowing
    "25-35%", "30-40%", "70-80%", "75-85%", "75-85%"   # Mixed: Bayesian methods flexible, Simon robust
  ),
  `Expected FWER` = c(
    "< 5%", "< 5%", "< 5%", "< 5%", "< 5%",      # All control FWER
    "< 5%", "< 5%", "< 5%", "< 5%", "< 5%"
  ),
  Notes = c(
    "Homogeneity test adapts", "No borrowing", "Model averaging flexible", "Learns exchangeability", "Hierarchical borrowing",
    "Test prevents inflation", "Independent analysis", "Partition-based borrowing", "Data-driven sharing", "Berry/ExNex models"
  ),
  check.names = FALSE
)

knitr::kable(
  illustrative_ocs,
  caption = "Expected Operating Characteristics (illustrative from literature)",
  align = c("l", "l", "r", "r", "l")
)

## ----full_framework, eval=FALSE-----------------------------------------------
# # Note: This code is for reference - eval=FALSE to avoid long computation
# 
# set.seed(2024)
# n_sims <- 1000  # Use >= 1000 for publication quality
# 
# # Store results for all scenarios and designs
# comparison_results <- list()
# 
# for (i in seq_along(scenarios)) {
#   scenario <- scenarios[[i]]
#   cat("Running scenario", i, ":", scenario$name, "\n")
# 
#   designs <- create_designs(scenario)
#   scenario_results <- list()
# 
#   for (design_name in names(designs)) {
#     cat("  Design:", design_name, "\n")
# 
#     # Simulate trials
#     sim_results <- simulate_basket_trial(
#       designs[[design_name]],
#       n_sims = n_sims,
#       seed = 1000 + i * 10 + match(design_name, names(designs))
#     )
# 
#     # Compute operating characteristics
#     ocs <- compute_operating_characteristics(sim_results,
#                                               designs[[design_name]],
#                                               alpha = 0.05)
#     scenario_results[[design_name]] <- ocs
#   }
# 
#   comparison_results[[scenario$name]] <- scenario_results
# }
# 
# # Save for later analysis
# saveRDS(comparison_results, "comparison_results.rds")

## ----full_oc_framework, eval=FALSE--------------------------------------------
# # Full simulation study (eval=FALSE - computationally intensive)
# set.seed(2024)
# n_sims <- 1000  # Use >= 1000 for publication quality
# 
# # Example for one scenario
# scenario <- scenarios[[2]]  # Global Alternative
# 
# # Create designs
# design_cunanan <- basket_design(
#   n_baskets = 4,
#   basket_names = basket_names,
#   sample_sizes = 20,
#   response_rates = scenario$response_rates,
#   null_response_rates = 0.20,
#   design_type = "cunanan"
# )
# 
# design_simon <- simon_design(
#   n_baskets = 4,
#   basket_names = basket_names,
#   sample_sizes = 20,
#   null_response_rates = 0.20,
#   alternative_response_rates = 0.35,
#   alpha = 0.05 / 4,
#   beta = 0.20
# )
# 
# # Generate simulations
# sims_cunanan_list <- simulate_responses(design_cunanan, n_sims = n_sims)
# sims_simon_list <- simulate_responses(design_simon, n_sims = n_sims)
# 
# # Analyze each
# results_cunanan <- lapply(sims_cunanan_list, function(sim_data) {
#   analyze_basket(sim_data, design_cunanan)
# })
# 
# results_simon <- lapply(sims_simon_list, function(sim_data) {
#   analyze_basket(sim_data, design_simon)
# })
# 
# # Compute OCs using package function
# ocs_cunanan <- compute_operating_characteristics(results_cunanan, design_cunanan, alpha = 0.05)
# ocs_simon <- compute_operating_characteristics(results_simon, design_simon, alpha = 0.05)
# 
# # Format results
# # Extract power values (for promising baskets)
# power_cunanan <- sapply(ocs_cunanan$basket_specific, function(x)
#   if(x$metric == "Power") x$value else NA)
# power_simon <- sapply(ocs_simon$basket_specific, function(x)
#   if(x$metric == "Power") x$value else NA)
# 
# results_df <- data.frame(
#   Design = c("Cunanan", "Simon"),
#   `Avg Power` = sprintf("%.1f%%",
#                         c(mean(power_cunanan, na.rm = TRUE),
#                           mean(power_simon, na.rm = TRUE)) * 100),
#   `FWP-D` = sprintf("%.1f%%",
#                     c(ocs_cunanan$family_wise$fwp_disjunctive,
#                       ocs_simon$family_wise$fwp_disjunctive) * 100),
#   `FWP-C` = sprintf("%.1f%%",
#                     c(ocs_cunanan$family_wise$fwp_conjunctive,
#                       ocs_simon$family_wise$fwp_conjunctive) * 100),
#   check.names = FALSE
# )
# 
# knitr::kable(results_df, caption = paste0("Operating Characteristics: ", scenario$name))

## ----power_vs_type1, eval=FALSE-----------------------------------------------
# library(ggplot2)
# 
# # Extract average power and FWER by design
# plot_data <- data.frame()
# 
# for (scenario_name in names(comparison_results)) {
#   scenario_results <- comparison_results[[scenario_name]]
# 
#   for (design_name in names(scenario_results)) {
#     ocs <- scenario_results[[design_name]]
# 
#     avg_power <- mean(ocs$power, na.rm = TRUE)
# 
#     plot_data <- rbind(plot_data, data.frame(
#       Scenario = scenario_name,
#       Design = design_name,
#       FWER = ocs$fwer,
#       AvgPower = avg_power
#     ))
#   }
# }
# 
# # Plot
# ggplot(plot_data, aes(x = FWER, y = AvgPower,
#                       color = Design, shape = Scenario)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_line(aes(group = Design), alpha = 0.3) +
#   scale_x_continuous(labels = scales::percent_format()) +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(
#     title = "Power vs Family-Wise Error Rate Trade-off",
#     subtitle = "Comparing basket trial designs across scenarios",
#     x = "Family-Wise Error Rate (FWER)",
#     y = "Average Power",
#     caption = paste0("Based on ", n_sims, " simulations per scenario/design")
#   ) +
#   theme_minimal() +
#   theme(legend.position = "right")

## ----scenario_comparison, eval=FALSE------------------------------------------
# # Reshape for plotting
# plot_data_long <- plot_data %>%
#   tidyr::pivot_longer(cols = c(FWER, AvgPower),
#                       names_to = "Metric",
#                       values_to = "Value")
# 
# ggplot(plot_data_long, aes(x = Design, y = Value, fill = Design)) +
#   geom_col(position = "dodge") +
#   facet_grid(Metric ~ Scenario, scales = "free_y") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(
#     title = "Design Performance Across Scenarios",
#     x = NULL,
#     y = "Probability"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none")

## ----protocol_specification, eval=FALSE---------------------------------------
# # Example protocol text generation
# design_chosen <- designs$bma  # Or other design
# 
# protocol <- generate_protocol_language(
#   design_chosen,
#   include_statistical_details = TRUE,
#   include_borrowing_rationale = TRUE,
#   include_oc_table = TRUE
# )
# 
# # Specify decision criteria
# cat("
# Statistical Decision Criteria:
# - Posterior probability threshold: 0.97 (calibrated for weak FWER ≤ 5%)
# - FWER control: Weak (global null scenario)
# - Planned interim analysis: After 50% enrollment
# - Futility stopping: P(promising | data) < 0.05
# ")

## ----custom_scenario----------------------------------------------------------
#' User-friendly function to compare designs with custom assumptions
#'
#' @param n_baskets Number of baskets
#' @param sample_size Sample size per basket
#' @param p0 Null response rate (scalar or vector)
#' @param scenarios List of response rate vectors to evaluate
#' @param n_sims Number of simulations
#' @param designs Which designs to compare (default: all)
#' @return Comparison results
compare_designs_custom <- function(n_baskets,
                                   sample_size,
                                   p0,
                                   scenarios,
                                   n_sims = 1000,
                                   designs = c("bma", "mem", "bhm", 
                                              "cunanan", "simon")) {
  
  # Ensure p0 is vector
  if (length(p0) == 1) p0 <- rep(p0, n_baskets)
  
  results <- list()
  
  for (i in seq_along(scenarios)) {
    scenario_rates <- scenarios[[i]]
    scenario <- list(
      name = paste("Scenario", i),
      response_rates = scenario_rates,
      promising = scenario_rates > p0
    )
    
    # Create designs
    basket_names <- paste0("Basket", 1:n_baskets)
    design_list <- list()
    
    if ("bma" %in% designs) {
      design_list$bma <- basket_design(
        n_baskets = n_baskets,
        basket_names = basket_names,
        sample_sizes = sample_size,
        response_rates = scenario_rates,
        null_response_rates = p0,
        design_type = "bma"
      )
    }
    
    if ("mem" %in% designs) {
      design_list$mem <- basket_design(
        n_baskets = n_baskets,
        basket_names = basket_names,
        sample_sizes = sample_size,
        response_rates = scenario_rates,
        null_response_rates = p0,
        design_type = "mem"
      )
    }
    
    if ("bhm" %in% designs) {
      design_list$bhm <- basket_design(
        n_baskets = n_baskets,
        basket_names = basket_names,
        sample_sizes = sample_size,
        response_rates = scenario_rates,
        null_response_rates = p0,
        design_type = "bhm"
      )
    }
    
    if ("cunanan" %in% designs) {
      design_list$cunanan <- basket_design(
        n_baskets = n_baskets,
        basket_names = basket_names,
        sample_sizes = sample_size,
        response_rates = scenario_rates,
        null_response_rates = p0,
        design_type = "cunanan"
      )
    }
    
    if ("simon" %in% designs) {
      design_list$simon <- simon_design(
        n_baskets = n_baskets,
        basket_names = basket_names,
        sample_sizes = sample_size,
        null_response_rates = p0,
        alternative_response_rates = 0.35,  # Can be customized
        alpha = 0.05 / n_baskets,
        beta = 0.20
      )
    }
    
    # Simulate each design
    scenario_results <- list()
    for (design_name in names(design_list)) {
      sim_results <- simulate_basket_trial(
        design_list[[design_name]], 
        n_sims = n_sims
      )
      ocs <- compute_operating_characteristics(sim_results, 
                                                design_list[[design_name]], 
                                                alpha = 0.05)
      scenario_results[[design_name]] <- ocs
    }
    
    results[[scenario$name]] <- scenario_results
  }
  
  return(results)
}

## ----example_custom, eval=FALSE-----------------------------------------------
# # User specifies their Phase 2 trial assumptions
# my_comparison <- compare_designs_custom(
#   n_baskets = 5,
#   sample_size = 25,
#   p0 = 0.15,  # Standard of care response rate
#   scenarios = list(
#     c(0.15, 0.15, 0.15, 0.15, 0.15),  # All null
#     c(0.45, 0.45, 0.45, 0.45, 0.45),  # All promising
#     c(0.15, 0.45, 0.45, 0.45, 0.45),  # 1 null
#     c(0.15, 0.15, 0.45, 0.45, 0.45),  # 2 null
#     c(0.15, 0.15, 0.15, 0.15, 0.45)   # 1 promising
#   ),
#   n_sims = 1000,
#   designs = c("bma", "cunanan", "simon")
# )
# 
# # Generate comparison table
# comparison_table <- format_oc_table(my_comparison)
# knitr::kable(comparison_table,
#              caption = "Custom Phase 2 Basket Trial Comparison")
# 
# # Summary recommendation
# cat("
# Based on your assumptions (n=25 per basket, p0=0.15, p1=0.45):
# 
# If you expect homogeneous effects:
#   → Use BMA for 30-40% power increase over Simon
# 
# If you expect heterogeneous effects:
#   → Use Cunanan or Simon for better FWER control
# 
# Expected sample size:
#   → BMA/Cunanan: ~125 patients total (may stop early)
#   → Simon: ~125 patients total (fixed two-stage per basket)
# ")

## -----------------------------------------------------------------------------
sessionInfo()

## ----cleanup, include=FALSE---------------------------------------------------
# Reset to sequential processing
future::plan(future::sequential)

