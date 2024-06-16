## README

# Land Transfer Duty Forecasting Project

Welcome to the Land Transfer Duty Forecasting Project repository! This project aims to improve the forecast accuracy of *land transfer duty* (**LTD**) for the *Department of Treasury and Finance* (**DTF**) using advanced forecasting methodologies, including forecast reconciliation. This README provides a detailed overview of the project, its methodology, results, and instructions on how to reproduce the results and apply the techniques to your own needs.

## Repository Structure

- **Function**: Contains user-defined functions created for the project.
- **Report**: Contains the final report and all relevant materials documenting the project.
- **analysis01_ltd**: Includes the initial steps of data analysis, handling missing values, summary statistics, and distribution for all variables. This is not the main focus for detailed analysis.
- **analysis02_time_series**: Focuses on time series data analysis with time plots, seasonal plots, subseries plots, decomposition for all variables, and initial cross-sectional forecast reconciliation attempts. This file is also not the main focus.
- **analysis03_vecm_train_test**: Contains R scripts for training and testing the VECM model:
  - `demo_cross_temporal_reco.R`: Applies the full forecasting and cross-temporal reconciliation procedure to a subset of data.
  - `plot.R`: Plots one year worth of forecasts for all temporal frequencies, from monthly to annually.
- **analysis04_cross_validation**: The main directory containing all files related to performing cross-validation for 10 folds of training sets with a thorough forecast reconciliation procedure. This includes RMSE and MAPE computation and plotting for 12-step-ahead forecasts.
- **data**: Contains all initial and generated data used in the project.
- **presentation**: Contains files related to the presentation about the project.
- **FINAL_FORECAST**: Contains the final forecast data for each type of reconciliation, along with the related files used to produce these forecasts.

## Project Overview

The property sector is a cornerstone of Australia's economy, contributing approximately 13% to the Gross Domestic Product (GDP) and indirectly supporting one in four jobs. This project aims to enhance the accuracy of LTD forecasts using advanced statistical methods.

### Methodology

The project employs two primary models:
1. **Vector Error Correction Model (VECM)**: Captures both short-term dynamics and long-term equilibrium relationships among time series.
2. **Autoregressive Integrated Moving Average (ARIMA)**: A traditional time series forecasting model used for comparison purposes.

#### Forecast Reconciliation

To improve forecast accuracy, the project utilizes forecast reconciliation techniques:
- **Cross-sectional Reconciliation**: Ensures coherence among different levels of data aggregation.
- **Temporal Reconciliation**: Ensures coherence across different temporal frequencies.
- **Cross-temporal Reconciliation**: Combines both cross-sectional and temporal reconciliation for optimal accuracy.

### Results

Empirical results demonstrate that VECM, particularly when coupled with forecast reconciliation techniques, consistently outperforms ARIMA. Temporal hierarchies play a more significant role in enhancing forecast accuracy than cross-sectional hierarchies. Reconciled forecasts exhibit lower RMSE compared to those of the DTF, validating the proposed methodology's efficacy.

## Reproducing the Results

### Prerequisites

Ensure you have the following software and packages installed:
- **R** (version 4.0 or higher)
- **RStudio** (optional but recommended)
- **Required R packages**: `dplyr`, `ggplot2`, `forecast`, `vars`, `tsibble`, `FoReco`, `tseries`, `urca`, `kableExtra`, `quarto`, `fpp3`

### Steps to Reproduce

1. **Clone the Repository**:
   ```sh
   git clone https://github.com/justin-git01/ltd_forecasting.git
   cd ltd_forecasting
   ```

2. **Prepare the Data**:
   - Ensure all data files are in the `data` directory.
   - Run the data preprocessing scripts in `analysis01_ltd` to handle missing values and compute summary statistics.

3. **Time Series Analysis**:
   - Execute the scripts in `analysis02_time_series` for initial time series analysis and plotting.

4. **Train and Test VECM**:
   - Run `demo_cross_temporal_reco.R` and `plot.R` in `analysis03_vecm_train_test` to apply forecasting and cross-temporal reconciliation to a subset of data.

5. **Cross-validation and Evaluation**:
   - Execute the scripts in `analysis04_cross_validation` to perform cross-validation, compute RMSE and MAPE, and generate plots for the 10 folds of 12-step-ahead forecasts.

### Applying the Methodology

You can use the provided scripts and functions as references to apply the forecast reconciliation methodology to your own datasets. Modify the data loading and preprocessing steps as needed, and adjust the model parameters to fit your specific requirements.

## References

For citing this project, please use the following BibTeX entry:
```bibtex
@misc{github_repo_2024,
  author = {Hoang Do},
  title = {LTD Forecasting Project},
  year = {2024},
  url = {https://github.com/justin-git01/ltd_forecasting},
  note = {Accessed: 2024-06-03}
}
```

We hope this project provides valuable insights and tools for improving forecast accuracy in the property sector. For any questions or further assistance, please feel free to contact us through the GitHub repository.