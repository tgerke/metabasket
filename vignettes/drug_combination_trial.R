## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load_packages, message=FALSE, warning=FALSE------------------------------
library(metabasket)
library(knitr)
library(future)

# Set up parallel processing for faster simulations
# Use all but 1 core (leave 1 for system)
plan(multisession, workers = max(1, availableCores() - 1))

# Set seed for reproducibility
set.seed(2024)

# Define basket characteristics
basket_names <- c("NSCLC", "CRC", "PC", "GC")
n_baskets <- length(basket_names)
sample_size <- 25
null_rate <- 0.15
promising_rate <- 0.35

## ----bma_design---------------------------------------------------------------
# Create BMA design
bma_design <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = rep(promising_rate, n_baskets),
  null_response_rates = null_rate,
  design_type = "bma",
  design_params = list(
    mu0 = 0.25,                  # Prior mean for response rate
    phi0 = 1,                    # Prior precision
    pmp0 = 1,                    # Prior model weight parameter
    post_prob_threshold = 0.95   # Posterior probability threshold for rejection
  )
)

print(bma_design)

## ----bma_ocs, message=FALSE, warning=FALSE------------------------------------
# Scenario 1: All baskets promising
bma_global_alt <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = rep(0.35, n_baskets),
  null_response_rates = null_rate,
  design_type = "bma",
  design_params = list(mu0 = 0.25, phi0 = 1, post_prob_threshold = 0.95)
)

sim_bma_alt <- simulate_basket_trial(bma_global_alt, n_sims = 250, seed = 201, .parallelize = TRUE)

# Scenario 2: Mixed
bma_mixed <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = c(0.35, 0.35, 0.15, 0.15),
  null_response_rates = null_rate,
  design_type = "bma",
  design_params = list(mu0 = 0.25, phi0 = 1, post_prob_threshold = 0.95)
)

sim_bma_mixed <- simulate_basket_trial(bma_mixed, n_sims = 250, seed = 202, .parallelize = TRUE)

# Scenario 3: Global null
bma_null <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = rep(0.15, n_baskets),
  null_response_rates = null_rate,
  design_type = "bma",
  design_params = list(mu0 = 0.25, phi0 = 1, post_prob_threshold = 0.95)
)

sim_bma_null <- simulate_basket_trial(bma_null, n_sims = 250, seed = 203, .parallelize = TRUE)

# Extract key metrics
bma_results <- data.frame(
  Scenario = c("Global Alternative (4/4 at 35%)", 
               "Mixed (2 at 35%, 2 at 15%)",
               "Global Null (4/4 at 15%)"),
  `Average Power` = c(
    sprintf("%.1f%%", mean(sapply(sim_bma_alt$operating_characteristics$basket_specific, function(x) x$value)) * 100),
    sprintf("%.1f%%", mean(sapply(sim_bma_mixed$operating_characteristics$basket_specific[1:2], function(x) x$value)) * 100),
    "N/A (no alternative)"
  ),
  FWER = c(
    "N/A (no null)",
    sprintf("%.1f%%", sim_bma_mixed$operating_characteristics$family_wise$fwer * 100),
    sprintf("%.1f%%", sim_bma_null$operating_characteristics$family_wise$fwer * 100)
  ),
  check.names = FALSE
)

knitr::kable(bma_results, 
             caption = "BMA Design: Simulated Operating Characteristics (250 simulations)")

## ----mem_design---------------------------------------------------------------
# Create MEM design
mem_design <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = rep(promising_rate, n_baskets),
  null_response_rates = null_rate,
  design_type = "mem",
  design_params = list(
    alpha0 = 1,      # Prior shape parameter
    beta0 = 1,       # Prior shape parameter
    tau = 0.5        # Similarity threshold
  )
)

print(mem_design)

## ----mem_ocs, message=FALSE, warning=FALSE------------------------------------
# Scenario 1: All baskets promising
mem_global_alt <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = rep(0.35, n_baskets),
  null_response_rates = null_rate,
  design_type = "mem",
  design_params = list(alpha0 = 1, beta0 = 1, tau = 0.5)
)

sim_mem_alt <- simulate_basket_trial(mem_global_alt, n_sims = 250, seed = 301, .parallelize = TRUE)

# Scenario 2: Mixed
mem_mixed <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = c(0.35, 0.35, 0.15, 0.15),
  null_response_rates = null_rate,
  design_type = "mem",
  design_params = list(alpha0 = 1, beta0 = 1, tau = 0.5)
)

sim_mem_mixed <- simulate_basket_trial(mem_mixed, n_sims = 250, seed = 302, .parallelize = TRUE)

# Scenario 3: Global null
mem_null <- basket_design(
  n_baskets = n_baskets,
  basket_names = basket_names,
  sample_sizes = sample_size,
  response_rates = rep(0.15, n_baskets),
  null_response_rates = null_rate,
  design_type = "mem",
  design_params = list(alpha0 = 1, beta0 = 1, tau = 0.5)
)

sim_mem_null <- simulate_basket_trial(mem_null, n_sims = 250, seed = 303, .parallelize = TRUE)

# Extract key metrics
mem_results <- data.frame(
  Scenario = c("Global Alternative (4/4 at 35%)", 
               "Mixed (2 at 35%, 2 at 15%)",
               "Global Null (4/4 at 15%)"),
  `Average Power` = c(
    sprintf("%.1f%%", mean(sapply(sim_mem_alt$operating_characteristics$basket_specific, function(x) x$value)) * 100),
    sprintf("%.1f%%", mean(sapply(sim_mem_mixed$operating_characteristics$basket_specific[1:2], function(x) x$value)) * 100),
    "N/A (no alternative)"
  ),
  FWER = c(
    "N/A (no null)",
    sprintf("%.1f%%", sim_mem_mixed$operating_characteristics$family_wise$fwer * 100),
    sprintf("%.1f%%", sim_mem_null$operating_characteristics$family_wise$fwer * 100)
  ),
  check.names = FALSE
)

knitr::kable(mem_results, 
             caption = "MEM Design: Simulated Operating Characteristics (250 simulations)")

