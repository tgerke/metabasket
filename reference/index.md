# Package index

## Design Functions

Functions for creating basket trial designs

- [`basket_design()`](https://tgerke.github.io/metabasket/reference/basket_design.md)
  : Create a basket trial design specification
- [`simon_design()`](https://tgerke.github.io/metabasket/reference/simon_design.md)
  : Create Simon two-stage design specification
- [`basket_data()`](https://tgerke.github.io/metabasket/reference/basket_data.md)
  : Create a basket trial data object
- [`print(`*`<basket_design>`*`)`](https://tgerke.github.io/metabasket/reference/print.basket_design.md)
  : Print method for basket_design
- [`print(`*`<basket_data>`*`)`](https://tgerke.github.io/metabasket/reference/print.basket_data.md)
  : Print method for basket_data
- [`calculate_sample_size()`](https://tgerke.github.io/metabasket/reference/calculate_sample_size.md)
  : Calculate sample size for basket trial

## Simulation Functions

Functions for simulating basket trials

- [`simulate_responses()`](https://tgerke.github.io/metabasket/reference/simulate_responses.md)
  : Simulate basket trial data
- [`simulate_basket_trial()`](https://tgerke.github.io/metabasket/reference/simulate_basket_trial.md)
  : Run complete simulation study
- [`compute_operating_characteristics()`](https://tgerke.github.io/metabasket/reference/compute_operating_characteristics.md)
  : Compute operating characteristics from simulation results
- [`extract_rejections()`](https://tgerke.github.io/metabasket/reference/extract_rejections.md)
  : Extract rejection decisions from analysis results
- [`compare_designs()`](https://tgerke.github.io/metabasket/reference/compare_designs.md)
  : Compare operating characteristics across designs
- [`create_scenario()`](https://tgerke.github.io/metabasket/reference/create_scenario.md)
  : Create standard simulation scenarios
- [`print(`*`<design_comparison>`*`)`](https://tgerke.github.io/metabasket/reference/print.design_comparison.md)
  : Print method for design_comparison
- [`print(`*`<simulation_results>`*`)`](https://tgerke.github.io/metabasket/reference/print.simulation_results.md)
  : Print method for simulation_results

## Analysis Functions

Functions for analyzing basket trial data

- [`analyze_basket()`](https://tgerke.github.io/metabasket/reference/analyze_basket.md)
  : Analyze simulated basket trial
- [`analyze_basket(`*`<basket_design>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.basket_design.md)
  : Basket design analyze method
- [`analyze_basket(`*`<bhm>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.bhm.md)
  : Analyze basket trial using Bayesian Hierarchical Model
- [`analyze_basket(`*`<bma>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.bma.md)
  : Bayesian Model Averaging Analysis Wrapper
- [`analyze_basket(`*`<cunanan>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.cunanan.md)
  : Analyze basket trial using Cunanan et al. efficient design
- [`analyze_basket(`*`<default>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.default.md)
  : Default analyze_basket method
- [`analyze_basket(`*`<mem>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.mem.md)
  : Multi-source Exchangeability Model (MEM) Analysis Wrapper
- [`analyze_basket(`*`<simon_design>`*`)`](https://tgerke.github.io/metabasket/reference/analyze_basket.simon_design.md)
  : Analyze basket trial using parallel Simon two-stage designs
- [`extract_rejections(`*`<bhm_result>`*`)`](https://tgerke.github.io/metabasket/reference/extract_rejections.bhm_result.md)
  : Extract rejections from BHM results
- [`extract_rejections(`*`<bma_result>`*`)`](https://tgerke.github.io/metabasket/reference/extract_rejections.bma_result.md)
  : Extract rejection decisions from BMA results
- [`extract_rejections(`*`<cunanan_result>`*`)`](https://tgerke.github.io/metabasket/reference/extract_rejections.cunanan_result.md)
  : Extract rejection decisions from Cunanan analysis
- [`extract_rejections(`*`<default>`*`)`](https://tgerke.github.io/metabasket/reference/extract_rejections.default.md)
  : Default extract_rejections method
- [`extract_rejections(`*`<mem_result>`*`)`](https://tgerke.github.io/metabasket/reference/extract_rejections.mem_result.md)
  : Extract rejection decisions from MEM results
- [`extract_rejections(`*`<simon_result>`*`)`](https://tgerke.github.io/metabasket/reference/extract_rejections.simon_result.md)
  : Extract rejection decisions from Simon analysis
- [`print(`*`<cunanan_result>`*`)`](https://tgerke.github.io/metabasket/reference/print.cunanan_result.md)
  : Print method for Cunanan results
- [`print(`*`<simon_result>`*`)`](https://tgerke.github.io/metabasket/reference/print.simon_result.md)
  : Print method for Simon results
- [`print(`*`<bma_result>`*`)`](https://tgerke.github.io/metabasket/reference/print.bma_result.md)
  : Print method for BMA results
- [`print(`*`<mem_result>`*`)`](https://tgerke.github.io/metabasket/reference/print.mem_result.md)
  : Print method for MEM results
- [`print(`*`<bhm_result>`*`)`](https://tgerke.github.io/metabasket/reference/print.bhm_result.md)
  : Print BHM analysis results

## Protocol Functions

Functions for generating protocol language

- [`generate_protocol_language()`](https://tgerke.github.io/metabasket/reference/generate_protocol_language.md)
  : Generate protocol language for basket trial design
- [`export_protocol_language()`](https://tgerke.github.io/metabasket/reference/export_protocol_language.md)
  : Export protocol language to file

## Reporting Functions

Functions for trial reporting and analysis

- [`generate_analysis_report()`](https://tgerke.github.io/metabasket/reference/generate_analysis_report.md)
  : Generate comprehensive analysis report
- [`calculate_conditional_power()`](https://tgerke.github.io/metabasket/reference/calculate_conditional_power.md)
  : Calculate conditional power for adaptive decisions
- [`generate_preregistration()`](https://tgerke.github.io/metabasket/reference/generate_preregistration.md)
  : Generate pre-registration document
- [`print(`*`<conditional_power_result>`*`)`](https://tgerke.github.io/metabasket/reference/print.conditional_power_result.md)
  : Print conditional power results

## Example Datasets

Published basket trial data

- [`imatinib_trial`](https://tgerke.github.io/metabasket/reference/imatinib_trial.md)
  : Imatinib trial in sarcomas
- [`vemurafenib_trial`](https://tgerke.github.io/metabasket/reference/vemurafenib_trial.md)
  : Vemurafenib trial in BRAF V600 cancers
