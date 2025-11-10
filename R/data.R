#' Imatinib trial in sarcomas
#'
#' Data from a phase II multicenter basket trial of imatinib mesylate in 10 
#' histologic subtypes of sarcoma using a Bayesian hierarchical model. The trial
#' enrolled 179 patients and observed 28 responses (overall RR = 15.6%).
#'
#' @format A `basket_data` object (list) with 4 components:
#' \describe{
#'   \item{basket_names}{Character vector of 10 sarcoma subtype names}
#'   \item{n_patients}{Integer vector of sample sizes per subtype (total = 179)}
#'   \item{n_responses}{Integer vector of responses per subtype (total = 28)}
#'   \item{response_rates}{Numeric vector of observed response rates}
#' }
#'
#' @source 
#' Chugh R, Wathen JK, Maki RG, et al. Phase II multicenter trial of imatinib 
#' in 10 histologic subtypes of sarcoma using a Bayesian hierarchical statistical 
#' model. J Clin Oncol. 2009;27(19):3148-3153. doi:10.1200/JCO.2008.20.9054
#'
#' @examples
#' data(imatinib_trial)
#' print(imatinib_trial)
#' 
#' # Summary statistics
#' sum(imatinib_trial$n_patients)    # 179
#' sum(imatinib_trial$n_responses)   # 28
"imatinib_trial"


#' Vemurafenib trial in BRAF V600 cancers
#'
#' Data from a phase II basket trial of vemurafenib in 6 non-melanoma cancer 
#' types with BRAF V600 mutations. The trial enrolled 84 patients and observed
#' 18 responses (overall RR = 21.4%).
#'
#' @format A `basket_data` object (list) with 4 components:
#' \describe{
#'   \item{basket_names}{Character vector of 6 cancer type names}
#'   \item{n_patients}{Integer vector of sample sizes per type (total = 84)}
#'   \item{n_responses}{Integer vector of responses per type (total = 18)}
#'   \item{response_rates}{Numeric vector of observed response rates}
#' }
#'
#' @source 
#' Hyman DM, Puzanov I, Subbiah V, et al. Vemurafenib in multiple nonmelanoma
#' cancers with BRAF V600 mutations. N Engl J Med. 2015;373(8):726-736. 
#' doi:10.1056/NEJMoa1502309
#'
#' @examples
#' data(vemurafenib_trial)
#' print(vemurafenib_trial)
#' 
#' # Summary statistics  
#' sum(vemurafenib_trial$n_patients)   # 84
#' sum(vemurafenib_trial$n_responses)  # 18
"vemurafenib_trial"
