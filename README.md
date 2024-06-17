# Mortality Caused by Tropical Cyclones in the United States
Program: ReadMe
 
 Authors: Rachel Young and Solomon Hsiang
 
 Date: 6/14/2024

## Set Up

Scripts provided are written in Stata 14, Matlab 2019a, and R 4.2.0. Note that you will need a Stata 14, R (or Rstudio) version 4.2.0, and Matlab R2019a or higher license to fully replicate the analysis. Throughout this ReadMe, when indicating paths to code and data, it is assumed that you’ll execute scripts from the repo root directory.

To estimate run all the scripts, you will need several packages installed in Stata. To add them, launch Stata and run:

ssc install rsource, replace
ssc install outreg2, replace
net install grc1leg, from(http://www.stata.com/users/vwiggins/) 

In addition you will need sevaral pacakges intstalled in R. To add them, launch R and run:

install.packages(c("data.table", "dplyr", "magrittr", "reshape2", "ggplot2", "gridExtra", "fields", "akima", "sf", "RColorBrewer", "terra", "raster", "spatstat", "R.matlab", "matlib", "cowplot", "readr", "devtools", "stringr"))

If you do not run from the master do program you will have to run the following commands to create all the subdirectories:

mkdir output

mkdir output/randomization 

mkdir output/beta

mkdir figures

mkdir figures/appendix

Note, set up should only take a few minutes to complete.


## File Structure

/data 		:		Stores the data needed for the analysis

/scripts	:		Stores all the Stata and R scripts

/output		:		All the data generated by the scripts goes here	

/output/randomization	:	Results from the randomization test goes here

/output/beta	:		All the coefficients from the regressions go here

/figures	:		All main figures go here

/figures/appendix	:	All the Supplemental figures and tables go here

/attribution  	:		Matlab script and data to generate Figure 3 here (also where Figure 3 output is stored)

/plotting_maps	:		Matlab script and data to generate Figure 1a here (also where Figure 1a output is stored)





## Data Documentation

A detailed description of the data used in this analysis can be found in the Supplementary Information associated with the article.


## Replication Steps

There are five stages to our analysis:

1. Data collection and processing (this step takes a few minutes to run on a normal desktop computer)
2. Regression model estimation (this step takes 3-5 hours to run on a normal desktop computer)
3. Randomization test (this step takes up to a week to run on a normal desktop computer)
4. Mortality burden prediction (this step takes a few minutes to run on a normal desktop computer)
5. Figure creation (this whole step takes around 12-24 hours run on a normal desktop computer)

### Run the full pipeline:

The entire Stata and R pipeline (except the Stata randomization step) can be run by calling:

stata -b do scripts/master.do. 

To run the Matlab scripts that create Figure 1a, Figure SI1 and Figure 3 see instructions below. 

If you would rather do it step by step, see the description of each stage below. 




### 1. Data collection and processing:

All data is available for download here: https://zenodo.org/records/11662454. 

The full data processing code is not included but the collected full data set is provided in DATA_hurricane_mortality_temp_month_state_19302015.dta .

We also provide the follow datasets required for the analysis:

- Shapefiles of the U.S. states : cb_2016_us_state_20m
- LICRICE generated TC wind speed and pddi by state and month : panel_by_storm__NA_USA_density_8_yr_1930_2018.csv 
- NOAA TC direct deaths : directdeaths.csv
- Nordhaus TC damages and LICRICE, national : nordhaus_LICRICE_USA_merged.dta
- TC rainfall data : rainfall_idw_state_storm.csv
- CDC mortality data for all states : mortality_19002015.dta (needed for SI figures)
- Counties on the coastline : coastline-counties-list.xlsx (needed for SI figures)
- County population by age : us.1969_2020.19ages.adjusted.txt (needed for SI figures)
- List of TC names : storm_list.txt
- Wind speed and population by pixel : wind_state_pop_export.csv (needed for Figure SI13e)
- LICRICE all storms pixel-level : NA_USA_density_8_yr_1930_2018_storm_specific.mat (needed for Figure SI6)
- Shapefile of US states : plotting_maps/s_11au16/s_11au16.shp (needed for Figure SI1)
- Hurricane direct death data : directdeaths.csv (needed for Figure 3)
- TC unique serial number and storm name : storm_id_name_raw.mat (needed for Figure 3)
- TC unique serial number : stormnamelist.mat (needed for Figure 3)

Preparation of variables (such as creating fixed effects and leads) for the regressions can be generated using the following command:

stata -b do scripts/01-prep-lags.do

We also provide the assembled data needed to run scripts to create the main results (output/full_data_for_regression.dta), including the output of the randomization test (output/randomization). 

### 2. Regression model estimation:

Once data is processed, you can estimate each regression model for all mortality outcomes using the following command:

stata -b do scripts/02-run-regressions.do



### 3. Randomization test

Once the data is processed you can perform the randomization procedure using the following command:

stata -b do scripts/randomization.do

NOTE: This program takes multiple days to run in its entirety. Therefore, we provide the results of the randomization in the output/randomization directory for your convenience. 



### 4. Mortality burden prediction

Once the regression coefficients have been estimated in the above models, you can generate the necessary predictions using 

stata -b do scripts/03-mortality-predictions.do



### 5. Figure creation 

To generate the four figures in the paper, run the following scripts:

matlab -nodesktop -nosplash -r "plotting_maps; exit;" (note: you need to change the cd path)

stata -b do scripts/04-main-figure1.do

stata -b do scripts/05-main-figure2.do

stata -b do scripts/06-main-figure3.do

matlab -nodesktop -nosplash -r "plotting_impulse; exit;" (note: you need to change the cd path)

stata -b do scripts/07-main-figure4.do

R CMD BATCH scripts/08-excess-mortality-maps.R

stata -b do scripts/09-appendix-figures.do

R CMD BATCH scripts/10-appendix-rain-wind-maps.R


You can also run the scripts interactively by opening the corresponding programs and scripts. 



Figure 1 requires the data collection step and the randomization step to be complete.

- panel a : created in created in plotting_maps/plotting_maps.m.
- panel b - g : figures/figure1.pdf ; created in scripts/04-main-figure1.do



Figure 2 requires the data collection and randomization step to be complete. 

- panel a - f : figures/figure2.pdf ; created in scripts/04-main-figure2.do



Figure 3 requires the data collection and the regression steps to be complete.

- read in data:
	- attribution/panel_by_storm_state_collapsed.csv ; created in scripts/06-main-figure3.do
	- attribution/mortality_predict_storm_pooled_adaptation.csv ; created in scripts/06-main-figure3.do
- panel a : attribution/figure3_attribution_windspeed.pdf ; 
- panel b : attribution/figure3_attribution_cubic.pdf ; 
- panel c : attribution/figure3_attribution_directdeath.pdf ; 


Figure 4 requires the data collection, regression, and projection step to be complete. Also need to run Figure 1 code. 
- panel a, c-f: created in scripts/08-excess-mortality-maps.R
- panel b, g, k: figures/appendix/figure4_state_mortality_withoutTCs.pdf ;
- panel h-j: figures/appendix/figure4_pre_post_2001_count_windspeed_population.pdf ;




ED Figures and Tables, and SI Figures requires data collection, regression, and projection steps to be complete. 

Extended data figures and tables:

- Figure ED1 : figures/appendix/figureED1_windspeed_allstates.pdf ; created in 09-appendix-figures.do
- Figure ED3 : figures/appendix/figureED3_mortality_allstates.pdf ; created in 09-appendix-figures.do
- Figure ED4 : figures/appendix/figureED4_examining_model_fit.pdf ; created in 09-appendix-figures.do
- Figure ED5 : figures/appendix/figureED5_randomization_distributions_combined.pdf ; created in 04-main-figure1.do
- Figure ED6 : figures/appendix/figureED6_model_adapt_time_weight_combined.pdf ; created in 09-appendix-figures.do note: Figure ED6c : figures/appendix/APPENDIX_est_adaptation.gph ; created in 05-main-figure2.do
- Figure ED7 : figures/appendix/figureED7_est_age_race_stacked.pdf ; created in 05-main-figure2.do
- Figure ED8 : figures/appendix/figureED8_est_cubic_adaptation_combine.pdf ; created in 09-appendix-figures.do
- Table ED1 : figures/appendix/TableED1_all_models_predicted_deaths.csv and figures/total_direct_deaths.csv; created in 09-appendix-figures.do
- Table ED2 : figures/appendix/TableED2_all_outcomes_predicted_deaths.csv ; created in 09-appendix-figures.do

Supplemental Information figures:

- Figure SI1 : created in plotting_maps/plotting_maps.m
- Figure SI2 : figures/appendix/figureSI2_USA_damage_maxs.pdf ; created in 04-main-figure1.do
- Figure SI3 : figures/appendix/rain_wind_DOLLY_1968.pdf, figures/appendix/rain_wind_DAVID_1979.pdf, figures/appendix/rain_wind_ANDREW_1992.pdf, figures/appendix/rain_wind_KATE_1985.pdf, figures/appendix/rain_wind_IKE_2008.pdf, figures/appendix/rain_wind_IRENE_2011.pdf ; created in 10-appendix-rain-wind-maps.R and combined mannually
- Figure SI4 : figures/appendix/figureSI4_maxs_histcdf.pdf ; created in 09-appendix-figures.do
- Figure SI5 : figures/appendix/figureSI5_residual_combined.pdf ; created in 09-appendix-figures.do
- Figure SI6 : figures/appendix/figureSI6_temperature_death_rate_state.pdf ; created in 09-appendix-figures.do

Note Figure ED2 and Figure SI7 are not generated programatically.

