#' Generate protocol language for basket trial design
#'
#' @description
#' Generates standardized protocol text describing a basket trial design,
#' suitable for inclusion in study protocols and regulatory submissions
#'
#' @param design A basket_design object
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
  
  if (!inherits(design, "basket_design")) {
    stop("design must be a basket_design object")
  }
  
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
