Analyze the Impacts of Urbanization and Climate on Diversions in the Lower Boise River Basin

Steps to running diversion analysis: 

Python scripts are basically all the data preprocessing for what goes into the models, and the R scripts are all the modeling. 

Python 

1) diversion_timeseries.ipynb : This script takes daily irrigation season flow values for the diversions and calculates 1) total water diverted, 2)  start day of irrigation season, 3) end day of irrigation season, 4) length of irrigation season. It corrects for diversions that would go beyond the bounds of a true irrigation season (e.g., February).
2) The following scripts can be done in any order: 
a. extract_gridment.ipynb: This script calculates zonal stats for each POU from Daymet and SSEBop data. 
b. subset_LULCC.ipynb: This script uses LCMAP data to calculate annual percent of each land class from 1987 to 2020 for each POU.
c. hydromet_data.ipynb: This script calculates the reservoir carryover and the maximum fill for the 3 reservoirs (Anderson Ranch, Arrowrock, Lucky Peak) in the Lower Boise River Basin. 
3) data_compilation.ipynb: This compiles climate, land use, and flow outputs and puts them in a usable, long-format csv file to import into R. 

R 

borah_dataprep.R: This takes full csv outputs from the Python output and puts it in the usable format for both the Generalized Linear Mixed Model with and without the ARMA. This exports 2 different csv files to be put into the respective models.
div_mixed_model_borah.R : Runs GLMMs with priors and model specifications and outputs .RDS files
borah_epreds.R : Uses model outputs to run expected predictions based on posterior outputs. This saves csv files for each of the variables that had a non-zero effect. These predictions are used to create marginal effects figures.
ind_mlr_brms.R: Runs individual GLMs for each diversion and summarized effect sizes. Outputs a csv file with the effect size for each variable and the uncertainty around it. 
trend_plots.R: Runs Mann Kendall test and outputs plots for diversions with trends
thesis_figures.R: This script creates all the figures in my thesis from model outputs besides the Mann Kendall analysis. Long script broken into sections based on the figures being made for each analysis

