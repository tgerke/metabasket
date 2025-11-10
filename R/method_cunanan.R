#' Analyze basket trial using Cunanan et al. efficient design
#'
#' Implements the efficient basket trial design from Cunanan et al. (2017) that
#' uses an interim assessment of heterogeneity via Fisher's exact test to determine
#' whether to pool baskets for increased power or analyze them separately.
#'
#' @param data A `basket_data` object containing the trial data
#' @param design A `basket_design` object containing the design specifications
#' @param ... Additional arguments (not currently used, for S3 consistency)
#' @param gamma Tuning parameter for heterogeneity assessment (0-1). Higher values
#'   favor heterogeneous design path. Default is 0.52 based on K=5 baskets.
#' @param alpha_s Significance level for separate analyses in heterogeneous path,
#'   before correction for multiple comparisons. Default is 0.07.
#' @param alpha_c Significance level for combined analysis in homogeneous path.
#'   Default is 0.05.
#' @param interim_data Optional `basket_data` object with stage 1 results. If NULL,
#'   assumes `data` represents stage 1.
#'
#' @return A list with class "cunanan_result" containing:
#'   * `rejections`: Logical vector indicating rejected baskets
#'   * `p_values`: P-values for each basket
#'   * `toh_pvalue`: P-value from test of homogeneity (Fisher's exact test)
#'   * `design_path`: Either "heterogeneous" or "homogeneous"
#'   * `continued_baskets`: Baskets that continued to stage 2
#'   * `test_statistics`: Additional test statistics
#'
#' @details
#' The Cunanan design has two stages:
#'
#' **Stage 1**: Enroll n1 patients per basket. Conduct Fisher's exact test on the
#' K x 2 contingency table of responses/non-responses across baskets.
#'
#' **Decision at interim**:
#' * If p-value <= gamma: Evidence of heterogeneity → heterogeneous path
#'   - Continue only baskets with >= 1 responder
#'   - Analyze each basket separately at end with Bonferroni correction (alpha_s/K*)
#' * If p-value > gamma: Evidence of homogeneity → homogeneous path
#'   - Continue all baskets if total responses >= K (approximately 1 per basket)
#'   - Analyze pooled data at end with one-sample test (alpha_c)
#'
#' **Stage 2**: Additional patients enrolled according to path chosen
#'
#' @references
#' Cunanan KM, Iasonos A, Shen R, Begg CB, Gönen M (2017). An efficient basket
#' trial design. *Statistics in Medicine*, 36(10):1568-1579.
#' doi:10.1002/sim.7227
#'
#' @export
#' @examples
#' \dontrun{
#' # Design for 5 baskets
#' design <- basket_design(
#'   n_baskets = 5,
#'   sample_sizes = c(7, 7, 7, 7, 7),  # Stage 1
#'   design_type = "cunanan"
#' )
#'
#' # Stage 1 data
#' stage1 <- basket_data(
#'   basket_names = paste0("Basket", 1:5),
#'   n_patients = c(7, 7, 7, 7, 7),
#'   n_responses = c(3, 2, 3, 1, 4)
#' )
#'
#' # Analyze
#' result <- analyze_basket(stage1, design)
#' }
analyze_basket.cunanan <- function(data, design, ...,
                                   gamma = 0.52,
                                   alpha_s = 0.07,
                                   alpha_c = 0.05,
                                   interim_data = NULL) {

  # Extract data
  n_baskets <- length(data$basket_names)
  n_responses <- data$n_responses
  n_patients <- data$n_patients

  # Check if this is stage 1 (interim) or full data
  is_stage1 <- is.null(interim_data)

  if (is_stage1) {
    # Stage 1: Test of homogeneity using Fisher's exact test

    # Create contingency table: responses vs non-responses for each basket
    contingency_table <- rbind(
      n_responses,
      n_patients - n_responses
    )

    # Fisher's exact test for homogeneity
    # Use hybrid approach if any responses present (Cunanan recommendation)
    if (sum(n_responses) == 0) {
      toh_result <- stats::fisher.test(contingency_table)
    } else {
      # Use simulation for hybrid p-value when responses present
      toh_result <- tryCatch(
        stats::fisher.test(contingency_table, hybrid = TRUE,
                          simulate.p.value = TRUE, B = 10000),
        error = function(e) stats::fisher.test(contingency_table)
      )
    }

    toh_pvalue <- toh_result$p.value

    # Determine design path
    if (toh_pvalue <= gamma) {
      design_path <- "heterogeneous"

      # Heterogeneous path: identify baskets with evidence of response
      # Default: continue baskets with >= 1 responder
      r_s <- design$design_params$r_s %||% 1
      continued <- n_responses >= r_s
      k_star <- which(continued)

      # Return interim results - need stage 2 data to complete analysis
      return(structure(
        list(
          interim_stage = TRUE,
          design_path = design_path,
          toh_pvalue = toh_pvalue,
          continued_baskets = k_star,
          stage1_responses = n_responses,
          stage1_patients = n_patients,
          gamma = gamma,
          alpha_s = alpha_s,
          alpha_c = alpha_c
        ),
        class = c("cunanan_result", "list")
      ))

    } else {
      design_path <- "homogeneous"

      # Homogeneous path: check if total responses sufficient to continue
      r_c <- design$design_params$r_c %||% n_baskets
      total_responses <- sum(n_responses)

      if (total_responses < r_c) {
        # Futility: stop trial
        return(structure(
          list(
            rejections = rep(FALSE, n_baskets),
            p_values = rep(1, n_baskets),
            toh_pvalue = toh_pvalue,
            design_path = design_path,
            futility_stopped = TRUE,
            continued_baskets = integer(0),
            test_statistics = list(
              total_responses = total_responses,
              total_patients = sum(n_patients)
            )
          ),
          class = c("cunanan_result", "list")
        ))
      }

      # Continue to stage 2 with all baskets
      return(structure(
        list(
          interim_stage = TRUE,
          design_path = design_path,
          toh_pvalue = toh_pvalue,
          continued_baskets = seq_len(n_baskets),
          stage1_responses = n_responses,
          stage1_patients = n_patients,
          gamma = gamma,
          alpha_s = alpha_s,
          alpha_c = alpha_c
        ),
        class = c("cunanan_result", "list")
      ))
    }

  } else {
    # Stage 2 / Final analysis with full data
    # interim_data contains the stage 1 results

    design_path <- interim_data$design_path
    toh_pvalue <- interim_data$toh_pvalue
    k_star <- interim_data$continued_baskets

    # Get null response rate
    theta_0 <- design$null_response_rates[1]

    if (design_path == "heterogeneous") {
      # Separate analysis for each continuing basket
      # Use one-sided binomial exact test with Bonferroni correction

      rejections <- rep(FALSE, n_baskets)
      p_values <- rep(1, n_baskets)

      for (k in k_star) {
        # One-sided test: H0: p <= theta_0 vs H1: p > theta_0
        binom_test <- stats::binom.test(
          x = n_responses[k],
          n = n_patients[k],
          p = theta_0,
          alternative = "greater"
        )

        p_values[k] <- binom_test$p.value

        # Bonferroni correction: alpha_s / K*
        adjusted_alpha <- alpha_s / length(k_star)
        rejections[k] <- p_values[k] <= adjusted_alpha
      }

      return(structure(
        list(
          rejections = rejections,
          p_values = p_values,
          toh_pvalue = toh_pvalue,
          design_path = design_path,
          continued_baskets = k_star,
          adjusted_alpha = alpha_s / length(k_star),
          test_statistics = list(
            responses_per_basket = n_responses,
            patients_per_basket = n_patients
          )
        ),
        class = c("cunanan_result", "list")
      ))

    } else {
      # Homogeneous path: one-sample test on pooled data
      # All baskets declared active or none

      total_responses <- sum(n_responses)
      total_patients <- sum(n_patients)

      # One-sided binomial test
      pooled_test <- stats::binom.test(
        x = total_responses,
        n = total_patients,
        p = theta_0,
        alternative = "greater"
      )

      # All baskets have same decision
      all_active <- pooled_test$p.value <= alpha_c
      rejections <- rep(all_active, n_baskets)
      p_values <- rep(pooled_test$p.value, n_baskets)

      return(structure(
        list(
          rejections = rejections,
          p_values = p_values,
          toh_pvalue = toh_pvalue,
          design_path = design_path,
          continued_baskets = k_star,
          pooled_test = TRUE,
          test_statistics = list(
            total_responses = total_responses,
            total_patients = total_patients,
            pooled_pvalue = pooled_test$p.value
          )
        ),
        class = c("cunanan_result", "list")
      ))
    }
  }
}


#' Extract rejection decisions from Cunanan analysis
#'
#' @param result A `cunanan_result` object from `analyze_basket.cunanan()`
#' @param ... Additional arguments (not currently used)
#'
#' @return Logical vector indicating which baskets were rejected (declared active)
#' @export
extract_rejections.cunanan_result <- function(result, ...) {
  if (!is.null(result$interim_stage) && result$interim_stage) {
    warning("This is an interim result. Final rejections not yet available.")
    return(NULL)
  }
  result$rejections
}


#' Print method for Cunanan results
#'
#' @param x A `cunanan_result` object
#' @param ... Additional arguments (not used)
#'
#' @export
print.cunanan_result <- function(x, ...) {
  cat("Cunanan Efficient Basket Trial Design\n")
  cat("=====================================\n\n")

  if (!is.null(x$interim_stage) && x$interim_stage) {
    cat("INTERIM ANALYSIS (Stage 1)\n")
    cat("--------------------------\n")
    cat("Test of Homogeneity p-value:", round(x$toh_pvalue, 4), "\n")
    cat("Design path selected:", x$design_path, "\n")
    cat("Baskets continuing to Stage 2:", paste(x$continued_baskets, collapse = ", "), "\n\n")

    if (x$design_path == "heterogeneous") {
      cat("Stage 2 will analyze baskets separately with alpha =",
          round(x$alpha_s / length(x$continued_baskets), 4), "\n")
    } else {
      cat("Stage 2 will pool all baskets with alpha =", x$alpha_c, "\n")
    }

  } else {
    cat("FINAL ANALYSIS\n")
    cat("--------------\n")
    cat("Design path:", x$design_path, "\n")
    cat("Test of Homogeneity p-value:", round(x$toh_pvalue, 4), "\n\n")

    if (!is.null(x$futility_stopped) && x$futility_stopped) {
      cat("Trial stopped for futility at interim\n")
      cat("No baskets declared active\n")
    } else {
      cat("Rejection decisions:\n")
      for (i in seq_along(x$rejections)) {
        status <- ifelse(x$rejections[i], "ACTIVE", "Not active")
        cat(sprintf("  Basket %d: %s (p = %.4f)\n",
                    i, status, x$p_values[i]))
      }

      if (x$design_path == "heterogeneous") {
        cat("\nAdjusted alpha:", round(x$adjusted_alpha, 4), "\n")
      } else {
        cat("\nPooled analysis alpha:", x$test_statistics$pooled_pvalue, "\n")
      }
    }
  }

  invisible(x)
}


# Helper for NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
