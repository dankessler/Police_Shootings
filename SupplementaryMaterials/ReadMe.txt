The Data File Folder Contains:
MapFile - A RData file containing the shapefile of the USA, which can be linked to the raw county-level data in the following file...
MapFileData-WithCountyResultsAndCovariates - A CSV file containing the raw county-level data used in analysis, and the county-level outcomes of model fitting.
RacismData_Google-Stephens-Davidowitz - This is the racism proxy from Google search data, released by S. Stephens-Davidowitz.
U.S. Police Shootings Data (Cleaned) - A CSV file, which can be loaded using the attached model code, so that readers can replicate the analyses presented in the main text.
U.S. Police Shootings Data (Cleaned) - An Excel version of the above file, so that readers can find hyperlinks and evaluate the quality of the raw data.

The Model File Folder Contains:
1.IndividualDataToCountySums - R code to translate the individual-level U.S. Police Shootings Data (Cleaned) into county-level sums.
2.BayesianModelOfPoliceShooting - Stan code to model relative risk of being the victim of police shootings at the county-level.
3.BayesianModelOfPreditorsOfRacialBias - Stan code to model county-level relative risk ratios as a function of county-level properties.
4.MapPlotting - R code to visualize results.

The Results File Folder Contains:
StanResults-PredictorsOfRacialBias - The Stan results with convergence diagnostics for the county-level predictors of racial bias models.
StanResults-RelativeRiskModel - The Stan results with convergence diagnostics for the relative risk models.

Please check the author's GitHub account, www.github.com/ctross, for updated models and data.