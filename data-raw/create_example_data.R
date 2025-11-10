# Script to create example datasets for metabasket package
# Run this script to regenerate data/imatinib_trial.rda and data/vemurafenib_trial.rda

# Imatinib trial data (Chugh et al. 2009)
# Source: Table 1 in Zhou & Ji (2024) review paper
imatinib_trial <- structure(
  list(
    basket_names = c(
      "Angiosarcoma", "Ewing", "Fibrosarcoma", "Leiomyosarcoma",
      "Liposarcoma", "MFH", "Osteosarcoma", "MPNST",
      "Rhabdomyosarcoma", "Synovial"
    ),
    n_patients = c(15, 13, 12, 28, 29, 29, 26, 5, 2, 20),
    n_responses = c(2, 0, 1, 6, 7, 3, 5, 1, 0, 3),
    response_rates = c(2/15, 0/13, 1/12, 6/28, 7/29, 3/29, 5/26, 1/5, 0/2, 3/20)
  ),
  class = c("basket_data", "list")
)

# Vemurafenib trial data (Hyman et al. 2015)
# Source: Table 2 in Zhou & Ji (2024) review paper
vemurafenib_trial <- structure(
  list(
    basket_names = c("ATC", "ECD/LCH", "CCA", "CRC-V", "CRC-VC", "NSCLC"),
    n_patients = c(7, 14, 8, 26, 10, 19),
    n_responses = c(2, 6, 1, 1, 0, 8),
    response_rates = c(2/7, 6/14, 1/8, 1/26, 0/10, 8/19)
  ),
  class = c("basket_data", "list")
)

# Save to data directory using usethis::use_data()
# This follows R packages best practices and handles compression/location automatically
usethis::use_data(imatinib_trial, overwrite = TRUE, compress = "bzip2")
usethis::use_data(vemurafenib_trial, overwrite = TRUE, compress = "bzip2")

message("Example datasets created successfully!")
message("- data/imatinib_trial.rda")
message("- data/vemurafenib_trial.rda")
