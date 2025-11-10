#' Analyze basket trial using parallel Simon two-stage designs
#'
#' Implements independent Simon optimal two-stage designs for each basket. This
#' serves as a reference design for comparison with basket trial methods that
#' borrow information across baskets. Each basket is analyzed separately without
#' any information sharing.
#'
#' @param data A `basket_data` object containing the trial data
#' @param design A `simon_design` object created by `simon_design()`
#' @param ... Additional arguments (not currently used, for S3 consistency)
#' @param alpha Significance level for each individual basket. If not provided,
#'   uses alpha from design object. For family-wise error rate control at level
#'   epsilon across K baskets, set alpha = epsilon/K in the design.
#' @param beta Type II error rate (1 - power) for each basket. If not provided,
#'   uses beta from design object. Default is 0.20 (80% power).
#' @param design_type Character string indicating Simon design type. If not provided,
#'   uses design_type from design object.
#' @param interim_data Optional `simon_result` object with stage 1 results. If NULL,
#'   assumes `data` represents stage 1.
#'
#' @return A list with class "simon_result" containing:
#'   * `rejections`: Logical vector indicating rejected baskets
#'   * `p_values`: P-values for each basket (from exact binomial test)
#'   * `design_params`: Simon design parameters for each basket (r1, n1, r, n)
#'   * `stage1_continue`: Logical vector indicating which baskets continued to stage 2
#'   * `futility_stopped`: Logical vector indicating which baskets stopped for futility
#'
#' @details
#' The Simon two-stage design (Simon 1989) is the standard approach for single-arm
#' phase II trials. For basket trials, this corresponds to running K independent
#' Simon designs in parallel, one per basket.
#'
#' **Stage 1**: Enroll n1 patients. If <= r1 responses observed, stop for futility.
#' Otherwise continue to stage 2.
#'
#' **Stage 2**: Enroll additional n2 = n - n1 patients. Reject null hypothesis
#' if total responses across both stages > r.
#'
#' The design parameters (r1, n1, r, n) are chosen to:
#' - Control type I error at alpha under null response rate (p0)
#' - Achieve power (1 - beta) under alternative response rate (p1)
#' - Minimize expected sample size under H0 (optimal design)
#' - OR minimize maximum sample size n (minimax design)
#'
#' This implementation uses the clinfun::ph2simon algorithm for parameter selection.
#'
#' @references
#' Simon R (1989). Optimal two-stage designs for phase II clinical trials.
#' *Controlled Clinical Trials*, 10(1):1-10.
#'
#' Jung SH, Lee T, Kim K, George SL (2004). Admissible two-stage designs for
#' phase II cancer clinical trials. *Statistics in Medicine*, 23(4):561-569.
#'
#' @export
#' @examples
#' \dontrun{
#' # Design with 4 cohorts using parallel Simon designs
#' design <- simon_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   null_response_rates = 0.20,
#'   alternative_response_rates = 0.40,
#'   alpha = 0.05 / 4  # Bonferroni correction
#' )
#'
#' # Stage 1 data
#' stage1 <- basket_data(
#'   basket_names = paste0("Basket", 1:4),
#'   n_patients = c(12, 12, 12, 12),  # Stage 1 sample sizes
#'   n_responses = c(5, 2, 4, 1)
#' )
#'
#' # Interim analysis
#' interim <- analyze_basket(stage1, design, alpha = 0.05/4)  # Bonferroni
#' print(interim)
#'
#' # After stage 2 data collected for continuing baskets...
#' # full_data <- basket_data(...)
#' # final <- analyze_basket(full_data, design, interim_data = interim)
#' }
analyze_basket.simon_design <- function(data, design, ...,
                                        alpha = NULL,
                                        beta = NULL,
                                        design_type = NULL,
                                        interim_data = NULL) {

  # Use design parameters if not overridden
  if (is.null(alpha)) alpha <- design$alpha
  if (is.null(beta)) beta <- design$beta
  if (is.null(design_type)) design_type <- design$design_type
  
  design_type <- match.arg(design_type, c("optimal", "minimax"))

  # Extract data
  n_baskets <- length(data$basket_names)
  n_responses <- data$n_responses
  n_patients <- data$n_patients

  # Get null and alternative response rates
  p0 <- design$null_response_rates
  p1 <- design$alternative_response_rates

  # Check if this is stage 1 (interim) or full data
  is_stage1 <- is.null(interim_data)

  if (is_stage1) {
    # Stage 1: Calculate Simon design parameters and check futility

    # Calculate Simon design for representative basket
    # (assumes all baskets have similar sample sizes and response rates)
    simon_params <- calculate_simon_design(
      p0 = p0[1],
      p1 = p1[1],
      alpha = alpha,
      beta = beta,
      design_type = design_type
    )

    # Check each basket against futility rule
    stage1_continue <- rep(FALSE, n_baskets)
    futility_stopped <- rep(FALSE, n_baskets)

    for (k in seq_len(n_baskets)) {
      # Check if basket had approximately stage 1 sample size
      # and apply futility rule
      r1 <- simon_params$r1
      n1 <- simon_params$n1

      # If basket has at least n1 patients, apply futility rule
      if (n_patients[k] >= n1) {
        if (n_responses[k] > r1) {
          stage1_continue[k] <- TRUE
        } else {
          futility_stopped[k] <- TRUE
        }
      } else {
        # Not enough patients yet - continue
        stage1_continue[k] <- TRUE
      }
    }

    return(structure(
      list(
        interim_stage = TRUE,
        design_params = simon_params,
        stage1_continue = stage1_continue,
        futility_stopped = futility_stopped,
        stage1_responses = n_responses,
        stage1_patients = n_patients,
        alpha = alpha,
        beta = beta
      ),
      class = c("simon_result", "list")
    ))

  } else {
    # Stage 2 / Final analysis

    simon_params <- interim_data$design_params
    futility_stopped <- interim_data$futility_stopped

    # Test each basket independently using exact binomial test
    rejections <- rep(FALSE, n_baskets)
    p_values <- rep(1, n_baskets)

    for (k in seq_len(n_baskets)) {
      if (futility_stopped[k]) {
        # Basket stopped at interim - not rejected
        rejections[k] <- FALSE
        p_values[k] <- 1
      } else {
        # Perform exact binomial test
        # H0: p <= p0 vs H1: p > p0
        binom_test <- stats::binom.test(
          x = n_responses[k],
          n = n_patients[k],
          p = p0[k],
          alternative = "greater"
        )

        p_values[k] <- binom_test$p.value
        rejections[k] <- p_values[k] <= alpha

        # Also check against Simon critical value
        # Reject if total responses > r
        simon_reject <- n_responses[k] > simon_params$r
        rejections[k] <- rejections[k] && simon_reject
      }
    }

    return(structure(
      list(
        rejections = rejections,
        p_values = p_values,
        design_params = simon_params,
        stage1_continue = !futility_stopped,
        futility_stopped = futility_stopped,
        alpha = alpha,
        beta = beta,
        test_statistics = list(
          responses_per_basket = n_responses,
          patients_per_basket = n_patients
        )
      ),
      class = c("simon_result", "list")
    ))
  }
}


#' Calculate Simon two-stage design parameters
#'
#' Uses the algorithm from Simon (1989) to find optimal or minimax two-stage
#' designs. This is a simplified implementation; for production use, consider
#' using clinfun::ph2simon.
#'
#' @param p0 Null response rate
#' @param p1 Alternative response rate (should be > p0)
#' @param alpha Significance level (type I error)
#' @param beta Type II error rate (1 - power)
#' @param design_type "optimal" or "minimax"
#'
#' @return List with Simon design parameters: r1, n1, r, n, EN (expected sample size)
#'
#' @keywords internal
calculate_simon_design <- function(p0, p1, alpha = 0.05, beta = 0.20,
                                   design_type = c("optimal", "minimax")) {

  design_type <- match.arg(design_type)

  # Try to use clinfun if available
  if (requireNamespace("clinfun", quietly = TRUE)) {
    # clinfun::ph2simon returns a matrix with columns:
    # r1, n1, r, n, EN(p0), PET(p0)
    result <- clinfun::ph2simon(
      pu = p0,
      pa = p1,
      ep1 = alpha,
      ep2 = beta
    )

    if (design_type == "optimal") {
      # First row is optimal design
      params <- result$out[1, ]
    } else {
      # Second row is minimax design
      params <- result$out[2, ]
    }

    return(list(
      r1 = params[1],
      n1 = params[2],
      r = params[3],
      n = params[4],
      EN = params[5],
      PET = params[6],
      design_type = design_type
    ))

  } else {
    # Fallback: Use simple heuristic if clinfun not available
    warning("clinfun package not available. Using simplified Simon design calculation.")

    # Approximate using normal approximation
    # This is a rough approximation for illustration
    z_alpha <- stats::qnorm(1 - alpha)
    z_beta <- stats::qnorm(1 - beta)

    # Total sample size (single stage)
    n_single <- ceiling(
      ((z_alpha * sqrt(p0 * (1 - p0)) + z_beta * sqrt(p1 * (1 - p1)))^2) /
        (p1 - p0)^2
    )

    # Stage 1: roughly half of single-stage sample size
    n1 <- ceiling(n_single / 2)
    r1 <- floor(p0 * n1)

    # Stage 2: similar total as single-stage
    n <- n_single
    r <- floor(p0 * n + 1)

    # Expected sample size under H0
    PET <- stats::pbinom(r1, n1, p0)
    EN <- n1 + (1 - PET) * (n - n1)

    return(list(
      r1 = r1,
      n1 = n1,
      r = r,
      n = n,
      EN = EN,
      PET = PET,
      design_type = design_type
    ))
  }
}


#' Extract rejection decisions from Simon analysis
#'
#' @param result A `simon_result` object from `analyze_basket.simon()`
#' @param ... Additional arguments (not currently used)
#'
#' @return Logical vector indicating which baskets were rejected (declared active)
#' @export
extract_rejections.simon_result <- function(result, ...) {
  if (!is.null(result$interim_stage) && result$interim_stage) {
    warning("This is an interim result. Final rejections not yet available.")
    return(NULL)
  }
  result$rejections
}


#' Print method for Simon results
#'
#' @param x A `simon_result` object
#' @param ... Additional arguments (not used)
#'
#' @export
print.simon_result <- function(x, ...) {
  cat("Simon Two-Stage Design (Parallel Independent Analyses)\n")
  cat("=======================================================\n\n")

  cat("Design Type:", x$design_params$design_type, "\n")
  cat("Alpha:", x$alpha, "\n")
  cat("Beta:", x$beta, "\n\n")

  cat("Design Parameters:\n")
  cat(sprintf("  Stage 1: n1 = %d, r1 = %d (stop if <= %d responses)\n",
              x$design_params$n1, x$design_params$r1, x$design_params$r1))
  cat(sprintf("  Stage 2: n = %d, r = %d (reject if > %d responses total)\n",
              x$design_params$n, x$design_params$r, x$design_params$r))
  cat(sprintf("  Expected N under H0: %.1f\n", x$design_params$EN))
  cat(sprintf("  Probability of early termination (PET): %.3f\n\n",
              x$design_params$PET))

  if (!is.null(x$interim_stage) && x$interim_stage) {
    cat("INTERIM ANALYSIS (Stage 1)\n")
    cat("--------------------------\n")
    cat("Baskets continuing to Stage 2:",
        sum(x$stage1_continue), "of", length(x$stage1_continue), "\n")
    cat("Baskets stopped for futility:",
        sum(x$futility_stopped), "of", length(x$futility_stopped), "\n\n")

    for (i in seq_along(x$stage1_continue)) {
      status <- if (x$futility_stopped[i]) {
        "STOPPED (futility)"
      } else {
        "Continue"
      }
      cat(sprintf("  Basket %d: %s (%d/%d responses)\n",
                  i, status, x$stage1_responses[i], x$stage1_patients[i]))
    }

  } else {
    cat("FINAL ANALYSIS\n")
    cat("--------------\n")
    cat("Rejection decisions (each basket analyzed independently):\n")

    for (i in seq_along(x$rejections)) {
      if (x$futility_stopped[i]) {
        status <- "Stopped at interim (futility)"
      } else {
        status <- ifelse(x$rejections[i], "ACTIVE", "Not active")
      }

      cat(sprintf("  Basket %d: %s (p = %.4f, %d/%d responses)\n",
                  i, status, x$p_values[i],
                  x$test_statistics$responses_per_basket[i],
                  x$test_statistics$patients_per_basket[i]))
    }

    cat("\nNote: No information borrowing across baskets (independent analyses)\n")
  }

  invisible(x)
}
