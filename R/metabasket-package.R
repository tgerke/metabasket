#' metabasket: Meta-Analysis and Comparison Framework for Basket Trial Designs
#'
#' @description
#' The metabasket package provides a unified interface for designing, simulating,
#' and analyzing basket trials using multiple established statistical methods.
#' Basket trials evaluate a single therapy across multiple cancer types or
#' subtypes simultaneously.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{basket_design}}}{Create a basket trial design specification}
#'   \item{\code{\link{basket_data}}}{Create a basket trial data object}
#'   \item{\code{\link{simulate_responses}}}{Simulate basket trial data}
#'   \item{\code{\link{simulate_basket_trial}}}{Run complete simulation study}
#'   \item{\code{\link{analyze_basket}}}{Analyze basket trial data}
#'   \item{\code{\link{generate_protocol_language}}}{Generate protocol text}
#' }
#'
#' @section Supported Methods:
#' \itemize{
#'   \item Bayesian Model Averaging (BMA) - Psioda et al. (2021)
#'   \item Multi-source Exchangeability Model (MEM) - Hobbs & Landin (2018)
#'   \item Bayesian Hierarchical Model (BHM) - Berry et al. (2013)
#'   \item Efficient Basket Design - Cunanan et al. (2017)
#' }
#'
#' @section Package Philosophy:
#' The package follows test-driven development principles, with comprehensive
#' tests comparing results to published benchmarks. The consistent interface
#' across methods facilitates fair comparisons of operating characteristics.
#'
#' @docType package
#' @name metabasket-package
#' @aliases metabasket
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
