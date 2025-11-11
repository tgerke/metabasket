#' Generate protocol language for basket trial design
#'
#' @description
#' Generates standardized protocol text describing a basket trial design,
#' suitable for inclusion in study protocols and regulatory submissions
#'
#' @param design A basket_design or simon_design object
#' @param include_statistical_details Logical. Whether to include technical details
#' @param include_references Logical. Whether to include literature references
#'
#' @return A character string containing protocol language
#' @export
#'
#' @examples
#' design <- basket_design(
#'   n_baskets = 4,
#'   basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
#'   sample_sizes = 25,
#'   response_rates = 0.35,
#'   null_response_rates = 0.20,
#'   design_type = "bma"
#' )
#' 
#' protocol_text <- generate_protocol_language(design)
#' cat(protocol_text)
generate_protocol_language <- function(design, 
                                      include_statistical_details = TRUE,
                                      include_references = TRUE) {
  UseMethod("generate_protocol_language")
}

#' @export
generate_protocol_language.basket_design <- function(design, 
                                      include_statistical_details = TRUE,
                                      include_references = TRUE) {
  
  # Introduction
  intro <- sprintf(
    "This is a basket trial designed to evaluate the efficacy of [DRUG NAME] across %d cancer types or subtypes. The trial will enroll patients into %d distinct baskets based on their cancer diagnosis:\n\n",
    design$n_baskets, design$n_baskets
  )
  
  # Basket descriptions
  basket_desc <- paste0(
    sprintf("%d. %s (N = %d patients)\n", 
            seq_along(design$basket_names),
            design$basket_names,
            design$sample_sizes),
    collapse = ""
  )
  
  # Sample size justification
  sample_size_text <- sprintf(
    "\nThe total sample size for this trial is %d patients (%s per basket). This sample size was selected to provide adequate power to detect clinically meaningful treatment effects while accounting for information borrowing across baskets.\n\n",
    sum(design$sample_sizes),
    if (length(unique(design$sample_sizes)) == 1) {
      paste(unique(design$sample_sizes), "patients")
    } else {
      "varying numbers of patients"
    }
  )
  
  # Statistical approach
  stat_approach <- generate_statistical_approach_text(design, include_statistical_details)
  
  # Hypothesis testing
  hypothesis_text <- generate_hypothesis_text(design)
  
  # Decision criteria
  decision_text <- generate_decision_criteria_text(design)
  
  # Operating characteristics
  oc_text <- "\n\n## Operating Characteristics\n\nThe design has been calibrated through simulation studies to maintain appropriate Type I error control while maximizing power to detect treatment effects. Detailed operating characteristics are available in the Statistical Analysis Plan.\n"
  
  # References
  ref_text <- ""
  if (include_references) {
    ref_text <- generate_references(design)
  }
  
  # Combine all sections
  protocol_text <- paste0(
    "# Basket Trial Design\n\n",
    "## Trial Overview\n\n",
    intro,
    basket_desc,
    sample_size_text,
    "## Statistical Methodology\n\n",
    stat_approach,
    hypothesis_text,
    decision_text,
    oc_text,
    ref_text
  )
  
  return(protocol_text)
}


#' Generate statistical approach text
#'
#' @param design A basket_design object
#' @param include_details Logical
#'
#' @keywords internal
generate_statistical_approach_text <- function(design, include_details) {
  
  method_name <- switch(
    design$design_type,
    "bma" = "Bayesian Model Averaging",
    "mem" = "Multi-source Exchangeability Model",
    "bhm" = "Bayesian Hierarchical Model",
    "cunanan" = "Efficient Basket Trial Design",
    "simon" = "Simon Two-Stage Design (Parallel Independent Analyses)"
  )
  
  method_desc <- switch(
    design$design_type,
    "bma" = "The Bayesian model averaging (BMA) approach considers multiple possible partitions of the baskets, where baskets within the same partition are assumed to have identical response rates. The posterior probability of treatment efficacy in each basket is computed as a weighted average across all partitions, where the weights correspond to the posterior probabilities of the partitions given the observed data. This approach provides adaptive borrowing of information across baskets based on the similarity of their observed response rates.",
    
    "mem" = "The multi-source exchangeability model (MEM) uses a Bayesian hierarchical framework that allows for dynamic borrowing of information across baskets. The degree of information sharing between any two baskets depends on the similarity of their observed response rates, with the exchangeability structure learned from the data. This approach is robust to heterogeneity across baskets while still gaining efficiency through information borrowing when appropriate.",
    
    "bhm" = "The Bayesian hierarchical model (BHM) assumes that the response rates across baskets arise from a common distribution, enabling information borrowing through shrinkage toward an overall mean response rate. The degree of shrinkage is determined by a variance parameter that controls the heterogeneity across baskets. This approach uses Markov chain Monte Carlo (MCMC) methods to sample from the posterior distribution of the response rates.",
    
    "cunanan" = "This design uses a two-stage approach with an interim analysis to assess homogeneity of treatment effects across baskets. If substantial heterogeneity is not detected, data are pooled across baskets for increased efficiency. Otherwise, each basket is analyzed independently to avoid inappropriate borrowing of information.",
    
    "simon" = "This trial uses independent Simon optimal two-stage designs conducted in parallel for each basket. Each basket is analyzed separately without borrowing information across baskets. This approach provides a conservative reference standard for comparison with information-borrowing methods. To control the family-wise error rate across multiple baskets, the significance level for each individual basket is adjusted using Bonferroni correction."
  )
  
  text <- sprintf(
    "The trial will use a %s for the analysis of treatment efficacy. %s\n\n",
    method_name,
    method_desc
  )
  
  if (include_details) {
    details <- generate_technical_details(design)
    text <- paste0(text, details)
  }
  
  return(text)
}


#' Generate hypothesis testing text
#'
#' @param design A basket_design object
#'
#' @keywords internal
generate_hypothesis_text <- function(design) {
  
  null_rates <- if (length(unique(design$null_response_rates)) == 1) {
    sprintf("%.1f%%", unique(design$null_response_rates) * 100)
  } else {
    "basket-specific reference rates"
  }
  
  text <- sprintf(
    "### Hypothesis Testing\n\nFor each basket j (j = 1, ..., %d), we will test the following hypotheses:\n\nH0: pi_j <= %s (the treatment is not efficacious)\nH1: pi_j > %s (the treatment is efficacious)\n\nwhere pi_j represents the true response rate for basket j.\n\n",
    design$n_baskets,
    null_rates,
    null_rates
  )
  
  return(text)
}


#' Generate decision criteria text
#'
#' @param design A basket_design object
#'
#' @keywords internal
generate_decision_criteria_text <- function(design) {
  
  text <- paste0(
    "### Decision Criteria\n\n",
    "At the completion of enrollment, the posterior probability that H1 is true will be computed for each basket using the observed data and the specified statistical model. A basket will be declared promising (i.e., H0 will be rejected) if the posterior probability exceeds a prespecified threshold gamma.\n\n",
    "The threshold gamma will be calibrated through simulation studies to achieve desired operating characteristics (e.g., family-wise error rate <= 5%, adequate power). The final threshold will be specified in the Statistical Analysis Plan prior to unblinding.\n\n"
  )
  
  return(text)
}


#' Generate technical details
#'
#' @param design A basket_design object
#'
#' @keywords internal
generate_technical_details <- function(design) {
  
  details <- switch(
    design$design_type,
    "bma" = "Prior distributions will be placed on the response rates within each partition, and a prior will be specified over the space of all possible partitions. The posterior distribution will be computed using analytical or numerical methods, and decisions will be based on the posterior probability that the response rate exceeds the null rate in each basket.",
    
    "mem" = "The model includes parameters that quantify the exchangeability between each pair of baskets. Prior distributions are specified for the basket-specific response rates and the exchangeability parameters. Posterior inference is conducted using Markov chain Monte Carlo sampling.",
    
    "bhm" = "The hierarchical model uses a normal distribution for the logit-transformed response rates, with hyperparameters governing the mean and variance. A half-t or half-normal prior is placed on the between-basket standard deviation parameter. Posterior samples are obtained using MCMC with appropriate convergence diagnostics.",
    
    "cunanan" = "The interim analysis uses Fisher's exact test to assess homogeneity. The critical value for the test statistic is chosen to optimize operating characteristics through simulation. The final analysis threshold is adjusted based on whether pooling or stratified analysis is used.",
    
    "simon" = "Each basket uses an optimal Simon two-stage design chosen to minimize expected sample size under the null hypothesis while maintaining the specified Type I error and power. At the interim analysis, baskets with <= r1 responses in n1 patients stop for futility. Baskets that continue enroll additional patients. At final analysis, each basket is tested independently using an exact binomial test. The significance level alpha for each basket is set at epsilon/K (Bonferroni correction) to control the family-wise error rate at epsilon across K baskets."
  )
  
  return(paste0("#### Technical Details\n\n", details, "\n\n"))
}


#' Generate references
#'
#' @param design A basket_design object
#'
#' @keywords internal
generate_references <- function(design) {
  
  refs <- list(
    general = c(
      "Zhou T, Ji Y. Bayesian Methods for Information Borrowing in Basket Trials: An Overview. Cancers. 2024;16(2):251.",
      "Hobbs BP, Pestana RC, Zabor EC, Kaizer AM, Hong DS. Basket trials: Review of current practice and innovations for future trials. J Clin Oncol. 2022;40(28):3520-3528."
    ),
    bma = "Psioda MA, Xu J, Jiang Q, Ke C, Yang Z, Ibrahim JG. Bayesian adaptive basket trial design using model averaging. Biostatistics. 2021;22(1):19-34.",
    mem = c(
      "Hobbs BP, Landin R. Bayesian basket trial design with exchangeability monitoring. Stat Med. 2018;37(25):3557-3572.",
      "Kaizer AM, Koopmeiners JS, Hobbs BP. Bayesian hierarchical modeling based on multisource exchangeability. Biostatistics. 2018;19(2):169-184."
    ),
    bhm = c(
      "Berry SM, Broglio KR, Groshen S, Berry DA. Bayesian hierarchical modeling of patient subpopulations: Efficient designs of phase II oncology clinical trials. Clin Trials. 2013;10(5):720-734.",
      "Neuenschwander B, Wandel S, Roychoudhury S, Bailey S. Robust exchangeability designs for early phase clinical trials with multiple strata. Pharm Stat. 2016;15(2):123-134."
    ),
    cunanan = "Cunanan KM, Iasonos A, Shen R, Begg CB, Gonen M. An efficient basket trial design. Stat Med. 2017;36(10):1568-1579.",
    simon = c(
      "Simon R. Optimal two-stage designs for phase II clinical trials. Control Clin Trials. 1989;10(1):1-10.",
      "Jung SH, Lee T, Kim K, George SL. Admissible two-stage designs for phase II cancer clinical trials. Stat Med. 2004;23(4):561-569."
    )
  )
  
  # Select relevant references
  selected_refs <- c(
    refs$general,
    refs[[design$design_type]]
  )
  
  ref_text <- paste0(
    "\n\n## References\n\n",
    paste0(seq_along(selected_refs), ". ", selected_refs, collapse = "\n")
  )
  
  return(ref_text)
}


#' Export protocol language to file
#'
#' @param design A basket_design object
#' @param file Character. Output file path. Extension determines format:
#'   .md = markdown, .html = HTML, .tex = LaTeX, .docx = Word,
#'   .txt = plain text, .Rmd = R Markdown with embedded R code
#' @param ... Additional arguments passed to generate_protocol_language()
#'
#' @return Invisibly returns the protocol text
#' @export
#' @examples
#' \dontrun{
#' design <- basket_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   design_type = "cunanan"
#' )
#' 
#' # Export as markdown
#' export_protocol_language(design, "protocol.md")
#' 
#' # Export as HTML
#' export_protocol_language(design, "protocol.html")
#' 
#' # Export as Word document
#' export_protocol_language(design, "protocol.docx")
#' 
#' # Export as R Markdown (includes reproducible R code)
#' export_protocol_language(design, "protocol.Rmd")
#' }
export_protocol_language <- function(design, file, ...) {
  
  # Determine format from file extension
  ext <- tolower(tools::file_ext(file))
  
  if (ext == "rmd") {
    # Generate R Markdown with embedded R code
    text <- generate_rmarkdown_protocol(design, ...)
    writeLines(text, file)
  } else if (ext == "html") {
    # Generate markdown first, then convert to HTML
    md_text <- generate_protocol_language(design, ...)
    text <- markdown_to_html(md_text)
    writeLines(text, file)
  } else if (ext == "tex") {
    # Generate LaTeX format
    text <- generate_latex_protocol(design, ...)
    writeLines(text, file)
  } else if (ext == "docx") {
    # Generate Word document using officer package
    generate_word_protocol(design, file, ...)
    message("Protocol language exported to: ", file)
    return(invisible(NULL))
  } else {
    # Default: markdown or plain text
    text <- generate_protocol_language(design, ...)
    writeLines(text, file)
  }
  
  message("Protocol language exported to: ", file)
  
  invisible(text)
}


#' Generate R Markdown protocol with reproducible code
#' @keywords internal
generate_rmarkdown_protocol <- function(design, ...) {
  
  # YAML header
  yaml <- "---
title: \"Basket Trial Protocol\"
author: \"Generated by metabasket\"
date: \"`r Sys.Date()`\"
output: 
  html_document:
    toc: true
    toc_float: true
---

"
  
  # Setup chunk
  setup <- "```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(metabasket)
```

"
  
  # Design specification chunk
  design_code <- sprintf("```{r design}
# Basket trial design
design <- basket_design(
  n_baskets = %d,
  basket_names = c(%s),
  sample_sizes = c(%s),
  response_rates = c(%s),
  null_response_rates = c(%s),
  design_type = \"%s\"
)

print(design)
```

",
    design$n_baskets,
    paste(sprintf("\"%s\"", design$basket_names), collapse = ", "),
    paste(design$sample_sizes, collapse = ", "),
    paste(design$response_rates, collapse = ", "),
    paste(design$null_response_rates, collapse = ", "),
    design$design_type
  )
  
  # Main protocol text
  protocol_text <- generate_protocol_language(design, ...)
  
  # Simulation example chunk
  sim_chunk <- "
## Operating Characteristics

```{r simulation, eval=FALSE}
# Simulate trial operating characteristics
set.seed(123)
sim_results <- simulate_basket_trial(
  design = design,
  n_sims = 1000,
  true_response_rates = design$response_rates
)

# View results
print(sim_results$operating_characteristics)
```
"
  
  # Combine all parts
  rmd <- paste0(yaml, setup, design_code, "\n", protocol_text, sim_chunk)
  
  return(rmd)
}


#' Generate LaTeX protocol
#' @keywords internal
generate_latex_protocol <- function(design, ...) {
  
  # Get markdown version
  md_text <- generate_protocol_language(design, ...)
  
  # Basic markdown to LaTeX conversion
  # (For production use, consider using pandoc via rmarkdown::pandoc_convert)
  
  latex_text <- md_text
  
  # Convert headers
  latex_text <- gsub("^# (.*)$", "\\\\section{\\1}", latex_text, perl = TRUE)
  latex_text <- gsub("^## (.*)$", "\\\\subsection{\\1}", latex_text, perl = TRUE)
  latex_text <- gsub("^### (.*)$", "\\\\subsubsection{\\1}", latex_text, perl = TRUE)
  
  # Convert bold
  latex_text <- gsub("\\*\\*(.+?)\\*\\*", "\\\\textbf{\\1}", latex_text, perl = TRUE)
  
  # Convert italic
  latex_text <- gsub("\\*(.+?)\\*", "\\\\textit{\\1}", latex_text, perl = TRUE)
  
  # Add document structure
  header <- "\\documentclass{article}
\\usepackage{hyperref}
\\usepackage{geometry}
\\geometry{margin=1in}

\\title{Basket Trial Protocol}
\\author{Generated by metabasket}
\\date{\\today}

\\begin{document}
\\maketitle

"
  
  footer <- "

\\end{document}"
  
  latex_text <- paste0(header, latex_text, footer)
  
  return(latex_text)
}


#' Convert markdown to HTML
#' @keywords internal
markdown_to_html <- function(md_text) {
  
  # Basic markdown to HTML conversion
  # (For production use, consider using markdown::markdownToHTML or pandoc)
  
  html_text <- md_text
  
  # Convert headers
  html_text <- gsub("^# (.*)$", "<h1>\\1</h1>", html_text, perl = TRUE)
  html_text <- gsub("^## (.*)$", "<h2>\\1</h2>", html_text, perl = TRUE)
  html_text <- gsub("^### (.*)$", "<h3>\\1</h3>", html_text, perl = TRUE)
  
  # Convert bold
  html_text <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", html_text, perl = TRUE)
  
  # Convert italic
  html_text <- gsub("\\*(.+?)\\*", "<em>\\1</em>", html_text, perl = TRUE)
  
  # Wrap paragraphs
  lines <- strsplit(html_text, "\n")[[1]]
  lines <- sapply(lines, function(line) {
    if (nchar(trimws(line)) > 0 && !grepl("^<", line)) {
      paste0("<p>", line, "</p>")
    } else {
      line
    }
  })
  html_text <- paste(lines, collapse = "\n")
  
  # Add HTML structure
  header <- "<!DOCTYPE html>
<html>
<head>
<meta charset=\"utf-8\">
<title>Basket Trial Protocol</title>
<style>
body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
h1 { color: #333; border-bottom: 2px solid #333; }
h2 { color: #666; border-bottom: 1px solid #999; margin-top: 30px; }
h3 { color: #888; margin-top: 20px; }
</style>
</head>
<body>

"
  
  footer <- "

</body>
</html>"
  
  html_text <- paste0(header, html_text, footer)
  
  return(html_text)
}


#' Generate Word document protocol
#' @keywords internal
generate_word_protocol <- function(design, file, ...) {
  
  # Check if officer package is available
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required for Word export but not installed.\n",
         "Install with: install.packages('officer')")
  }
  
  # Generate protocol text
  protocol_text <- generate_protocol_language(design, ...)
  
  # Create Word document
  doc <- officer::read_docx()
  
  # Parse markdown-like text and add to document
  lines <- strsplit(protocol_text, "\n")[[1]]
  
  for (line in lines) {
    # Skip empty lines at start
    if (nchar(trimws(line)) == 0) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
      next
    }
    
    # Headers
    if (grepl("^# ", line)) {
      header_text <- gsub("^# ", "", line)
      doc <- officer::body_add_par(doc, header_text, style = "heading 1")
    } else if (grepl("^## ", line)) {
      header_text <- gsub("^## ", "", line)
      doc <- officer::body_add_par(doc, header_text, style = "heading 2")
    } else if (grepl("^### ", line)) {
      header_text <- gsub("^### ", "", line)
      doc <- officer::body_add_par(doc, header_text, style = "heading 3")
    } else {
      # Regular text - handle bold formatting
      text <- line
      
      # Simple bold formatting (convert **text** to bold)
      if (grepl("\\*\\*", text)) {
        # For simplicity, just remove the ** markers
        # (officer requires more complex handling for inline formatting)
        text <- gsub("\\*\\*", "", text)
      }
      
      doc <- officer::body_add_par(doc, text, style = "Normal")
    }
  }
  
  # Write document
  print(doc, target = file)
  
  invisible(NULL)
}


#' @export
generate_protocol_language.simon_design <- function(design, 
                                                   include_statistical_details = TRUE,
                                                   include_references = TRUE) {
  
  # Introduction - differs for single vs multiple cohorts
  if (design$n_baskets == 1) {
    intro <- sprintf(
      "This trial uses a Simon two-stage design, the gold standard approach for single-arm phase II trials. The %s design has been selected to %s while maintaining the specified Type I error rate and power.\n\n",
      toupper(design$design_type),
      if (design$design_type == "optimal") "minimize the expected sample size under the null hypothesis" else "minimize the maximum sample size"
    )
  } else {
    intro <- sprintf(
      "This trial uses independent Simon two-stage designs conducted in parallel across %d cancer types or subtypes. Each cohort will be analyzed separately without borrowing information across cohorts, providing a conservative approach that protects against inappropriate pooling of heterogeneous treatment effects. The %s design has been selected to %s.\n\n",
      design$n_baskets,
      toupper(design$design_type),
      if (design$design_type == "optimal") "minimize the expected sample size under the null hypothesis" else "minimize the maximum sample size"
    )
  }
  
  # Cohort descriptions - only show section header for multiple cohorts
  if (design$n_baskets > 1) {
    cohort_desc <- "## Trial Cohorts\n\n"
    cohort_desc <- paste0(cohort_desc,
      paste0(sprintf("%d. %s (N = %d patients)\n", 
                     seq_along(design$basket_names),
                     design$basket_names,
                     design$sample_sizes),
             collapse = "")
    )
  } else {
    cohort_desc <- sprintf("## Trial Population\n\n%s (Maximum N = %d patients)\n", 
                          design$basket_names[1], 
                          design$sample_sizes[1])
  }
  
  # Sample size justification
  if (design$n_baskets == 1) {
    sample_size_text <- sprintf(
      "\nThe maximum sample size for this trial is %d patients. The trial uses a Simon two-stage design optimized for single-arm phase II trials.\n\n",
      design$sample_sizes[1]
    )
  } else {
    sample_size_text <- sprintf(
      "\nThe maximum total sample size for this trial is %d patients across %d independent cohorts (%s per cohort). Each cohort uses a Simon two-stage design optimized for single-arm phase II trials.\n\n",
      sum(design$sample_sizes),
      design$n_baskets,
      if (length(unique(design$sample_sizes)) == 1) {
        paste(unique(design$sample_sizes), "patients")
      } else {
        "varying numbers of patients"
      }
    )
  }
  
  # Statistical approach
  stat_approach <- "## Statistical Methodology\n\n"
  if (design$n_baskets == 1) {
    stat_approach <- paste0(stat_approach,
      "This trial uses a Simon two-stage design, the most widely accepted approach for single-arm phase II trials in oncology. The design allows for early stopping if the treatment shows insufficient activity, thereby minimizing patient exposure to ineffective therapy while maintaining appropriate statistical properties.\n\n"
    )
  } else {
    stat_approach <- paste0(stat_approach,
      "This trial uses independent Simon two-stage designs conducted in parallel for each cohort. Each cohort is analyzed separately without borrowing information across cohorts. This approach provides a conservative reference standard and is appropriate when treatment effects may be heterogeneous across cancer types.\n\n"
    )
  }
  
  if (include_statistical_details) {
    stat_approach <- paste0(stat_approach,
      sprintf("### Two-Stage Design\n\nEach cohort follows a two-stage design:\n\n**Stage 1**: Enroll n1 patients and observe the number of responses r1.\n- If responses <= r1, stop for futility (cohort is not promising)\n- If responses > r1, continue to Stage 2\n\n**Stage 2**: Enroll additional patients to reach total sample size n.\n- At final analysis, if total responses > r, reject H0 (cohort is promising)\n- Otherwise, fail to reject H0\n\nThe design parameters (n1, r1, n, r) are calculated using the Simon (1989) method with the following specifications:\n- Null response rate (H0): %s\n- Alternative response rate (H1): %s\n- Type I error rate (per cohort): %.3f\n- Type II error rate (per cohort): %.3f\n- Optimization criterion: %s\n\n",
        if (length(unique(design$null_response_rates)) == 1) 
          sprintf("%.1f%%", unique(design$null_response_rates) * 100)
        else "varies by cohort",
        if (length(unique(design$alternative_response_rates)) == 1) 
          sprintf("%.1f%%", unique(design$alternative_response_rates) * 100)
        else "varies by cohort",
        design$alpha,
        design$beta,
        ifelse(design$design_type == "optimal", 
               "Minimize expected sample size under H0",
               "Minimize maximum sample size")
      )
    )
  }
  
  # Hypothesis testing
  if (design$n_baskets == 1) {
    hypothesis_text <- sprintf(
      "### Hypothesis Testing\n\nWe test:\n\nH0: π ≤ %.2f (the treatment is not efficacious)\nH1: π > %.2f (the treatment is efficacious)\n\nwhere π represents the true response rate.\n\n",
      design$null_response_rates[1],
      design$null_response_rates[1]
    )
  } else {
    hypothesis_text <- sprintf(
      "### Hypothesis Testing\n\nFor each cohort j (j = 1, ..., %d), we test:\n\nH0: π_j ≤ p0 (the treatment is not efficacious)\nH1: π_j > p0 (the treatment is efficacious)\n\nwhere π_j represents the true response rate for cohort j and p0 is the null response rate.\n\n",
      design$n_baskets
    )
  }
  
  # Multiple testing adjustment - only for multiple cohorts
  if (design$n_baskets > 1) {
    # Infer intended FWER from the per-test alpha
    # If user already applied Bonferroni (small alpha), describe it
    # If alpha is large (e.g., 0.05-0.10), assume it's per-test without correction
    if (design$alpha <= 0.025) {
      # User likely already applied Bonferroni
      multiple_testing_text <- sprintf(
        "### Multiple Testing Adjustment\n\nTo control the family-wise error rate (FWER) across %d cohorts, a Bonferroni correction is applied. Each individual cohort is tested at significance level α = %.4f. This ensures that the probability of falsely rejecting H0 for at least one cohort is controlled when all null hypotheses are true.\n\n",
        design$n_baskets,
        design$alpha
      )
    } else {
      # Describe that testing is at the stated alpha without adjustment
      multiple_testing_text <- sprintf(
        "### Type I Error Control\n\nEach cohort is tested at significance level α = %.3f. With %d independent cohorts, the family-wise error rate (probability of at least one false positive when all nulls are true) is approximately %.3f under independence.\n\nNote: If strict family-wise error rate control is required, consider using α = %.4f per cohort (Bonferroni correction).\n\n",
        design$alpha,
        design$n_baskets,
        1 - (1 - design$alpha)^design$n_baskets,
        design$alpha / design$n_baskets
      )
    }
  } else {
    # Single cohort - just describe the Type I error rate
    multiple_testing_text <- sprintf(
      "### Type I Error Control\n\nThe design is calibrated to achieve a Type I error rate of α = %.3f, meaning there is at most a %.1f%% probability of concluding the treatment is efficacious when the true response rate is at or below the null hypothesis value.\n\n",
      design$alpha,
      design$alpha * 100
    )
  }
  
  # Decision criteria - detailed operational language
  if (design$n_baskets == 1) {
    decision_text <- sprintf(
      "### Decision Criteria\n\nThis study aims to identify an improvement in response rate from %.1f%% (null hypothesis) to %.1f%% (alternative hypothesis) with treatment. Using a two-stage Simon %s design with %.1f%% power and %.1f%% alpha (one-sided), participants will be recruited in two stages as follows:\n\n",
      design$null_response_rates[1] * 100,
      design$alternative_response_rates[1] * 100,
      design$design_type,
      (1 - design$beta) * 100,
      design$alpha * 100
    )
    
    decision_text <- paste0(decision_text,
      "**Stage 1 Enrollment and Interim Analysis:**\n\n",
      "- Enroll **n1 participants** to the first stage\n",
      "- If **(r1 + 1) or more** participants meet the primary endpoint, proceed to Stage 2\n",
      "- If **(r1 + 1) or more** participants meet the endpoint prior to enrollment of n1 participants, accrual will continue uninterrupted from Stage 1 to Stage 2\n",
      "- If **(r1 + 1) or more** participants have not met the endpoint at the time n1 participants have been enrolled, accrual will be held until:\n",
      "  - **(r1 + 1) or more** participants have met the endpoint (proceed to Stage 2), OR\n",
      "  - **n1 - (r1 + 1) or more** participants have failed to meet the endpoint (stop for futility)\n",
      "- If an insufficient number of participants meet the endpoint to proceed to Stage 2, the trial will be stopped for futility at the conclusion of Stage 1\n\n"
    )
    
    decision_text <- paste0(decision_text,
      "**Stage 2 Enrollment and Final Analysis:**\n\n",
      "- If Stage 2 is opened, enroll additional participants to reach a maximum total of **n participants**\n",
      "- Success is declared when **(r + 1) or more** participants meet the primary endpoint\n",
      "- If fewer than (r + 1) participants meet the endpoint, fail to reject the null hypothesis\n\n",
      "*Note: The specific design parameters (n1, r1, n, r) will be calculated using the Simon (1989) method based on the above specifications and documented in the Statistical Analysis Plan.*\n\n"
    )
  } else {
    decision_text <- "### Decision Criteria\n\n**Stage 1 Decision (Futility)**:\nFor each cohort, if the number of responses in the first n1 patients is ≤ r1, enrollment to that cohort stops and the treatment is declared not promising for that cohort. Cohorts may continue accrual uninterrupted if (r1 + 1) or more responses are observed before reaching n1 patients.\n\n**Final Decision (Efficacy)**:\nFor cohorts that continue to Stage 2, if the total number of responses is (r + 1) or more across all n patients, the treatment is declared promising for that cohort (H0 is rejected).\n\n"
  }
  
  # Operating characteristics
  oc_text <- "## Operating Characteristics\n\nThe design has been calibrated to achieve:\n"
  if (design$n_baskets == 1) {
    oc_text <- paste0(oc_text, sprintf(
      "- Type I error rate: α = %.3f (%.1f%% probability of false positive)\n",
      design$alpha,
      design$alpha * 100
    ))
    oc_text <- paste0(oc_text, sprintf(
      "- Power: ≥ %.1f%% (at alternative response rate of %.1f%%)\n",
      (1 - design$beta) * 100,
      design$alternative_response_rates[1] * 100
    ))
    oc_text <- paste0(oc_text, sprintf(
      "- Expected sample size: Minimized under H0 (%s design)\n",
      design$design_type
    ))
    oc_text <- paste0(oc_text, "\n\nDetailed design parameters (n1, r1, n, r) are calculated using the Simon (1989) method to achieve these operating characteristics.\n\n")
  } else {
    oc_text <- paste0(oc_text, sprintf(
      "- Type I error rate per cohort: α = %.4f\n",
      design$alpha
    ))
    oc_text <- paste0(oc_text, sprintf(
      "- Power per cohort: ≥ %.1f%% (at alternative response rate)\n",
      (1 - design$beta) * 100
    ))
    oc_text <- paste0(oc_text, sprintf(
      "- Expected sample size: Minimized under H0 (%s design)\n\n",
      design$design_type
    ))
  }
  
  # References
  ref_text <- ""
  if (include_references) {
    ref_text <- "\n## References\n\n"
    ref_text <- paste0(ref_text,
      "1. Simon R. Optimal two-stage designs for phase II clinical trials. Control Clin Trials. 1989;10(1):1-10.\n",
      "2. Jung SH, Lee T, Kim K, George SL. Admissible two-stage designs for phase II cancer clinical trials. Stat Med. 2004;23(4):561-569.\n",
      "3. Korn EL, Freidlin B, Abrams JS, Halabi S. Design issues in randomized phase II/III trials. J Clin Oncol. 2012;30(6):667-671.\n"
    )
  }
  
  # Combine all sections
  title <- if (design$n_baskets == 1) {
    "# Simon Two-Stage Design\n\n"
  } else {
    "# Simon Two-Stage Design for Multi-Cohort Trial\n\n"
  }
  
  protocol_text <- paste0(
    title,
    "## Trial Overview\n\n",
    intro,
    cohort_desc,
    sample_size_text,
    stat_approach,
    hypothesis_text,
    multiple_testing_text,
    decision_text,
    oc_text,
    ref_text
  )
  
  return(protocol_text)
}
