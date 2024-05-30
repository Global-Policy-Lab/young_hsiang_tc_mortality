//-------------------------------------------------------------------------------	
// A Large Undocumented Mortality Burden Caused by the Tropical Cyclone Climate of the United States
// Authors: Rachel Young & Sol Hsiang
// Program: master.do
// Date: 1-4-2024
//-------------------------------------------------------------------------------

cd "/Users/rachelyoung/Dropbox/test/YoungHsiang_replication" /* <----- set your path here */

*mkdir output /*  <----- This is provided because we are giving the results of the randomization */
*mkdir output/randomization  /*  <--- we provide the results of the randomization with seed 12345. */
mkdir output/beta
mkdir figures
mkdir figures/appendix

ssc install rsource, replace
ssc install outreg2, replace
net install grc1leg, from(http://www.stata.com/users/vwiggins/) 

//---------------------------------------------------------------------------------
// 00-attribute-program.do
// Program Description: Define the attribute program which is needed to create
//						Table SI1, Table SI2, and Figure 4.
//---------------------------------------------------------------------------------
do "scripts/00-attribute-program.do"


//---------------------------------------------------------------------------------
// 01-prep-lags.do
// Program Description: Prep Data for Anlaysis
//---------------------------------------------------------------------------------
do "scripts/01-prep-lags.do"


//---------------------------------------------------------------------------------
// randomization.do
// Program Description: The program performs the 4 randomization tests. 
//
// NOTE: This program takes over a week to run, so we provided the results of the 
//		randomization in output/randomization
//---------------------------------------------------------------------------------
//do scripts/randomization.do


//---------------------------------------------------------------------------------
// 02-run-regressions.do
// Program Description: Runs all regression specifications and stores .ster results
//---------------------------------------------------------------------------------
do "scripts/02-run-regressions.do"


//---------------------------------------------------------------------------------
// 03-mortality-predictions
// Program Description: Performs the predictions of excess mortlaity based on the 
//						regression analysis and the attribute program. 
//---------------------------------------------------------------------------------
do "scripts/03-mortality-predictions.do"



//---------------------------------------------------------------------------------
// 04-main-figure1.do
// Program Description: Creates Figure 1 and Appendix Figure SI7
//
// NOTE: The do program creates panel b - d
//			The matlab program creates the maps (panel a)
//---------------------------------------------------------------------------------
do "scripts/04-main-figure1.do"

cd "plotting_maps"
//matlab -nodesktop -nosplash -r "plotting_maps; exit;" 
cd ..


//---------------------------------------------------------------------------------
// 05-main-figure2.do
// Program Description: Creates Figure 2 and Appendix Figure SI9 and SI10c
//---------------------------------------------------------------------------------
do "scripts/05-main-figure2.do"



//---------------------------------------------------------------------------------
// 06-main-figure3.do
// Program Description:  Creates Figure 3
//
// NOTE: The do program generates the underlying data needed to run the Matlab script.
//		The matlab program creates figure 3. 
//---------------------------------------------------------------------------------
do "scripts/06-main-figure3.do"

cd "attribution"
//matlab -nodesktop -nosplash -r "plotting_impulse; exit;" 
cd ..


//---------------------------------------------------------------------------------
// 07-main-figure4.do
// Program Description: Creates Figure 4
//
// NOTE: The do program creates the figure 4b, 4h-k, and the data needed to create 
//		Figure 4a and 4c-f. 
//		The R program creates Figure 4a and 4c-f
//---------------------------------------------------------------------------------
do "scripts/07-main-figure4.do"

rsource using "scripts/08-excess-mortality-maps.R", rpath("/usr/local/bin/R") roptions(`"--vanilla"')

 
//---------------------------------------------------------------------------------
// 08-appendix-figures.do
// Program Description: Creates all reminaing appendix figures not generated 
//						in the above programs
//---------------------------------------------------------------------------------
do "scripts/09-appendix-figures.do"


rsource using "scripts/10-appendix-rain-wind-maps.R", rpath("/usr/local/bin/R") roptions(`"--vanilla"')
