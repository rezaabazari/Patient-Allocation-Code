Two R Markdown notebooks included in this repository were directly used in the research study:

SIR_Estimation_Florida.Rmd
This notebook estimates the epidemiological parameters β (transmission rate) and γ (recovery rate) for each Florida county using the SIR model. The estimated parameters are exported and used as fixed inputs in the optimization model.

Time-Series Forecasting.Rmd
This notebook forecasts the hospitalized-patient ratio αₜ (hospital bed demand per infection) using both univariate and hierarchical ARIMA time-series models. These forecasts supply the demand-related inputs to the patient allocation optimization model.

The notebooks use the following data sources included in the repository:

countyLevelFL_DailyCovid_2020-2021.csv — Daily COVID-19 confirmed case counts for all Florida counties (used for SIR parameter estimation).

County.csv — County names and identifiers.

final_df.csv — Historical hospitalization data used to estimate and forecast the hospitalization ratio αₜ.
