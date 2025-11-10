#' Generate comprehensive analysis report
#'
#' @description
#' Creates a detailed analysis report with tables, confidence intervals,
#' and adjusted statistics for basket trial results. Similar to rpact's
#' analysis reporting functionality.
#'
#' @param result An analysis result object from analyze_basket()
#' @param design The basket_design object used
#' @param data The basket_data object analyzed
#' @param format Character. Output format: "text", "markdown", "html", or "latex"
#' @param include_interim Logical. Include interim analysis results if available
#' @param confidence_level Numeric. Confidence level for intervals (default 0.95)
#'
#' @return A formatted report string
#' @importFrom stats binom.test rbinom
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' \dontrun{
#' design <- basket_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   design_type = "cunanan"
#' )
#' 
#' data <- basket_data(
#'   n_patients = c(25, 25, 25, 25),
#'   n_responses = c(8, 12, 6, 10)
#' )
#' 
#' result <- analyze_basket(data, design)
#' report <- generate_analysis_report(result, design, data)
#' cat(report)
#' }
generate_analysis_report <- function(result,
                                    design,
                                    data,
                                    format = c("text", "markdown", "html", "latex"),
                                    include_interim = TRUE,
                                    confidence_level = 0.95) {
  
  format <- match.arg(format)
  
  # Build report sections
  sections <- list()
  
  # Header
  sections$header <- generate_report_header(design, format)
  
  # Design summary
  sections$design <- generate_design_summary(design, format)
  
  # Analysis results
  sections$results <- generate_results_table(result, data, confidence_level, format)
  
  # Decision summary
  sections$decisions <- generate_decision_summary(result, format)
  
  # Interim results if available
  if (include_interim && !is.null(result$interim_stage) && result$interim_stage) {
    sections$interim <- generate_interim_summary(result, format)
  }
  
  # Combine sections
  report <- paste(sections, collapse = "\n\n")
  
  return(report)
}


#' Generate report header
#' @keywords internal
generate_report_header <- function(design, format) {
  
  title <- "Basket Trial Analysis Report"
  date <- format(Sys.Date(), "%B %d, %Y")
  
  if (format == "markdown") {
    return(sprintf("# %s\n\n**Date:** %s\n**Design:** %s", 
                   title, date, design$design_type))
  } else if (format == "latex") {
    return(sprintf("\\section{%s}\n\\textbf{Date:} %s \\\\\n\\textbf{Design:} %s",
                   title, date, design$design_type))
  } else if (format == "html") {
    return(sprintf("<h1>%s</h1>\n<p><strong>Date:</strong> %s<br>\n<strong>Design:</strong> %s</p>",
                   title, date, design$design_type))
  } else {
    return(sprintf("%s\n%s\n\nDate: %s\nDesign: %s",
                   title, strrep("=", nchar(title)), date, design$design_type))
  }
}


#' Generate design summary section
#' @keywords internal
generate_design_summary <- function(design, format) {
  
  text <- sprintf(
    "Number of baskets: %d\nTotal sample size: %d\nNull response rate: %.2f",
    design$n_baskets,
    sum(design$sample_sizes),
    mean(design$null_response_rates)
  )
  
  if (format == "markdown") {
    return(sprintf("## Design Summary\n\n%s", text))
  } else if (format == "html") {
    return(sprintf("<h2>Design Summary</h2>\n<pre>%s</pre>", text))
  } else {
    return(sprintf("Design Summary\n%s\n\n%s", strrep("-", 14), text))
  }
}


#' Generate results table with confidence intervals
#' @keywords internal
generate_results_table <- function(result, data, confidence_level, format) {
  
  n_baskets <- length(data$basket_names)
  
  # Calculate point estimates and confidence intervals
  response_rates <- data$n_responses / data$n_patients
  
  # Exact binomial confidence intervals
  ci_list <- lapply(seq_len(n_baskets), function(i) {
    binom::binom.confint(
      x = data$n_responses[i],
      n = data$n_patients[i],
      conf.level = confidence_level,
      methods = "exact"
    )
  })
  
  ci_lower <- sapply(ci_list, function(x) x$lower)
  ci_upper <- sapply(ci_list, function(x) x$upper)
  
  # Build results table
  results_df <- data.frame(
    Basket = data$basket_names,
    N = data$n_patients,
    Responses = data$n_responses,
    Rate = sprintf("%.3f", response_rates),
    CI_Lower = sprintf("%.3f", ci_lower),
    CI_Upper = sprintf("%.3f", ci_upper),
    P_value = if (!is.null(result$p_values)) sprintf("%.4f", result$p_values) else rep("--", n_baskets),
    Decision = ifelse(result$rejections, "REJECT H0", "Fail to reject"),
    stringsAsFactors = FALSE
  )
  
  # Format table based on output format
  if (format == "markdown") {
    header <- "## Analysis Results\n\n"
    table <- knitr::kable(results_df, format = "markdown", align = "lrrrrrrr")
    return(paste0(header, table))
  } else if (format == "latex") {
    header <- "\\subsection{Analysis Results}\n\n"
    table <- knitr::kable(results_df, format = "latex", align = "lrrrrrrr")
    return(paste0(header, table))
  } else if (format == "html") {
    header <- "<h2>Analysis Results</h2>\n"
    table <- knitr::kable(results_df, format = "html", align = "lrrrrrrr")
    return(paste0(header, table))
  } else {
    header <- paste0("Analysis Results\n", strrep("-", 16), "\n\n")
    return(paste0(header, paste(capture.output(print(results_df, row.names = FALSE)), collapse = "\n")))
  }
}


#' Generate decision summary
#' @keywords internal
generate_decision_summary <- function(result, format) {
  
  n_rejected <- sum(result$rejections)
  n_total <- length(result$rejections)
  
  text <- sprintf(
    "Rejected baskets: %d out of %d\nOverall rejection rate: %.1f%%",
    n_rejected, n_total, 100 * n_rejected / n_total
  )
  
  # Add design-specific details
  if (inherits(result, "cunanan_result")) {
    text <- paste0(text, sprintf(
      "\nDesign path: %s\nTest of homogeneity p-value: %.4f",
      result$design_path, result$toh_pvalue
    ))
  }
  
  if (format == "markdown") {
    return(sprintf("## Decision Summary\n\n%s", text))
  } else {
    return(sprintf("Decision Summary\n%s\n\n%s", strrep("-", 16), text))
  }
}


#' Generate interim analysis summary
#' @keywords internal
generate_interim_summary <- function(result, format) {
  
  if (inherits(result, "cunanan_result")) {
    text <- sprintf(
      "Interim Analysis:\nContinuing baskets: %s\nDesign path: %s",
      paste(result$continued_baskets, collapse = ", "),
      result$design_path
    )
  } else if (inherits(result, "simon_result")) {
    n_continue <- sum(result$stage1_continue)
    n_futility <- sum(result$futility_stopped)
    text <- sprintf(
      "Interim Analysis:\nBaskets continuing: %d\nBaskets stopped for futility: %d",
      n_continue, n_futility
    )
  } else {
    text <- "Interim analysis details not available for this design type."
  }
  
  if (format == "markdown") {
    return(sprintf("## Interim Analysis\n\n%s", text))
  } else {
    return(sprintf("Interim Analysis\n%s\n\n%s", strrep("-", 16), text))
  }
}


#' Calculate conditional power for adaptive decisions
#'
#' @description
#' Calculates the conditional power given observed interim data,
#' assuming a specified effect size for stage 2. Similar to rpact's
#' conditional power functionality.
#'
#' @param interim_result Interim analysis result from analyze_basket()
#' @param design The basket_design object
#' @param interim_data The basket_data from stage 1
#' @param n2_planned Vector of planned stage 2 sample sizes per basket
#' @param assumed_rates Vector of assumed response rates for stage 2
#'
#' @return A list with conditional power for each basket
#' @export
#'
#' @examples
#' \dontrun{
#' # After interim analysis
#' cp <- calculate_conditional_power(
#'   interim_result = interim_result,
#'   design = design,
#'   interim_data = stage1_data,
#'   n2_planned = c(15, 15, 15, 15),
#'   assumed_rates = c(0.40, 0.40, 0.40, 0.40)
#' )
#' print(cp)
#' }
calculate_conditional_power <- function(interim_result,
                                       design,
                                       interim_data,
                                       n2_planned,
                                       assumed_rates) {
  
  # Check if this is an interim result
  if (is.null(interim_result$interim_stage) || !interim_result$interim_stage) {
    stop("interim_result must be from an interim analysis (stage 1)")
  }
  
  n_baskets <- length(interim_data$basket_names)
  
  # Calculate conditional power for each basket
  conditional_power <- numeric(n_baskets)
  
  for (k in seq_len(n_baskets)) {
    
    # Check if basket is continuing
    if (inherits(interim_result, "cunanan_result")) {
      if (!(k %in% interim_result$continued_baskets)) {
        conditional_power[k] <- 0
        next
      }
    } else if (inherits(interim_result, "simon_result")) {
      if (!interim_result$stage1_continue[k]) {
        conditional_power[k] <- 0
        next
      }
    }
    
    # Observed data from stage 1
    x1 <- interim_data$n_responses[k]
    n1 <- interim_data$n_patients[k]
    
    # Stage 2 assumptions
    n2 <- n2_planned[k]
    p2 <- assumed_rates[k]
    
    # Total sample size
    n_total <- n1 + n2
    
    # Get decision threshold
    threshold <- get_decision_threshold(interim_result, design, k)
    
    # Monte Carlo simulation of stage 2 outcomes
    n_sims <- 10000
    x2_sim <- rbinom(n_sims, n2, p2)
    x_total <- x1 + x2_sim
    
    # Calculate proportion that would lead to rejection
    if (inherits(interim_result, "cunanan_result")) {
      if (interim_result$design_path == "heterogeneous") {
        # One-sided binomial test
        p_vals <- sapply(x_total, function(x) {
          binom.test(x, n_total, p = design$null_response_rates[k], 
                    alternative = "greater")$p.value
        })
        conditional_power[k] <- mean(p_vals <= threshold)
      } else {
        # Pooled test - more complex, use approximation
        conditional_power[k] <- NA  # Would need full pooled simulation
      }
    } else if (inherits(interim_result, "simon_result")) {
      # Simon design: need total responses > r
      r <- interim_result$design_params$r
      conditional_power[k] <- mean(x_total > r)
    }
  }
  
  result <- list(
    basket_names = interim_data$basket_names,
    conditional_power = conditional_power,
    n2_planned = n2_planned,
    assumed_rates = assumed_rates
  )
  
  class(result) <- "conditional_power_result"
  return(result)
}


#' Get decision threshold for a basket
#' @keywords internal
get_decision_threshold <- function(interim_result, design, basket_idx) {
  
  if (inherits(interim_result, "cunanan_result")) {
    if (interim_result$design_path == "heterogeneous") {
      # Bonferroni corrected alpha
      k_star <- length(interim_result$continued_baskets)
      return(interim_result$alpha_s / k_star)
    } else {
      return(interim_result$alpha_c)
    }
  } else if (inherits(interim_result, "simon_result")) {
    return(interim_result$alpha)
  }
  
  return(0.05)  # default
}


#' Print conditional power results
#' @param x A conditional_power_result object
#' @param ... Additional arguments (not used)
#' @export
print.conditional_power_result <- function(x, ...) {
  cat("Conditional Power Analysis\n")
  cat("==========================\n\n")
  
  df <- data.frame(
    Basket = x$basket_names,
    N2_Planned = x$n2_planned,
    Assumed_Rate = sprintf("%.3f", x$assumed_rates),
    Conditional_Power = sprintf("%.3f", x$conditional_power),
    stringsAsFactors = FALSE
  )
  
  print(df, row.names = FALSE)
  
  cat("\nNote: Conditional power is the probability of rejecting H0 at final analysis\n")
  cat("      given observed stage 1 data and assumed stage 2 response rates.\n")
  
  invisible(x)
}


#' Generate pre-registration document
#'
#' @description
#' Creates a structured pre-registration document suitable for
#' submission to registries like OSF, ClinicalTrials.gov, etc.
#'
#' @param design A basket_design object
#' @param study_info List with study metadata (title, PI, institution, etc.)
#' @param format Character. Output format: "markdown" or "text"
#'
#' @return A pre-registration document string
#' @export
#'
#' @examples
#' \dontrun{
#' design <- basket_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   design_type = "cunanan"
#' )
#' 
#' study_info <- list(
#'   title = "Phase II Basket Trial of Drug X",
#'   pi = "Dr. Jane Smith",
#'   institution = "Academic Medical Center",
#'   sponsor = "Pharmaceutical Company"
#' )
#' 
#' prereg <- generate_preregistration(design, study_info)
#' writeLines(prereg, "preregistration.md")
#' }
generate_preregistration <- function(design,
                                    study_info = list(),
                                    format = c("markdown", "text")) {
  
  format <- match.arg(format)
  
  sections <- list()
  
  # Title and metadata
  sections$title <- sprintf(
    "# Pre-registration: %s\n\n**Principal Investigator:** %s\n**Institution:** %s\n**Date:** %s",
    study_info$title %||% "Basket Trial Study",
    study_info$pi %||% "[TO BE SPECIFIED]",
    study_info$institution %||% "[TO BE SPECIFIED]",
    format(Sys.Date(), "%B %d, %Y")
  )
  
  # Study design
  sections$design <- sprintf(
    "\n\n## Study Design\n\n**Design type:** %s basket trial\n**Number of baskets:** %d\n**Total sample size:** %d patients\n**Enrollment:** %s\n\n**Basket definitions:**\n%s",
    design$design_type,
    design$n_baskets,
    sum(design$sample_sizes),
    if (length(unique(design$sample_sizes)) == 1) {
      sprintf("%d patients per basket", unique(design$sample_sizes))
    } else {
      "Variable enrollment per basket"
    },
    paste(sprintf("- %s: N = %d", design$basket_names, design$sample_sizes), 
          collapse = "\n")
  )
  
  # Statistical analysis plan
  sections$sap <- generate_sap_section(design)
  
  # Hypotheses
  sections$hypotheses <- sprintf(
    "\n\n## Hypotheses\n\nFor each basket j (j = 1, ..., %d):\n\n- **H0:** Response rate \u2264 %.2f (null hypothesis)\n- **H1:** Response rate > %.2f (alternative hypothesis)",
    design$n_baskets,
    mean(design$null_response_rates),
    mean(design$null_response_rates)
  )
  
  # Decision rules
  sections$decisions <- "\n\n## Decision Rules\n\nPre-specified decision rules and thresholds are documented in the Statistical Analysis Plan. All analyses will be conducted as specified, without modification based on observed data patterns."
  
  # Data monitoring
  sections$monitoring <- sprintf(
    "\n\n## Data Monitoring\n\n%s\n\nAll interim analyses are pre-specified and documented herein.",
    if (design$design_type %in% c("cunanan", "simon")) {
      "This study includes planned interim analyses for futility/efficacy assessment."
    } else {
      "This study will analyze data at a single timepoint after complete enrollment."
    }
  )
  
  # Combine sections
  prereg <- paste(sections, collapse = "")
  
  return(prereg)
}


#' Generate Statistical Analysis Plan section
#' @keywords internal
generate_sap_section <- function(design) {
  
  method_desc <- switch(
    design$design_type,
    "cunanan" = "The Cunanan efficient design uses an interim test of homogeneity to determine whether to pool baskets (homogeneous path) or analyze separately (heterogeneous path).",
    "simon" = "Independent Simon two-stage designs for each basket, with Bonferroni correction for family-wise error control.",
    "bma" = "Bayesian Model Averaging across all possible partitions of baskets.",
    "mem" = "Multi-source Exchangeability Model with data-driven information borrowing.",
    "bhm" = "Bayesian Hierarchical Model with MCMC-based posterior estimation."
  )
  
  sprintf(
    "\n\n## Statistical Analysis Plan\n\n**Primary analysis method:** %s\n\n**Description:** %s\n\n**Multiple testing adjustment:** %s",
    design$design_type,
    method_desc,
    if (design$design_type == "simon") {
      "Bonferroni correction applied to control family-wise error rate"
    } else {
      "Controlled through Bayesian decision thresholds / adaptive design features"
    }
  )
}


# Helper operator
`%||%` <- function(a, b) if (is.null(a)) b else a
`%s%` <- function(a, b) paste0(a, b)
