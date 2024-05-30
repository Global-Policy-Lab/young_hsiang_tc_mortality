

//-------------------------------------------------------------------------------	
//--------------------------------------------------------------------- APPENDIX
//-------------------------------------------------------------------------------
disp("-------------------------STARTING APPENDIX-----------------------")

use output/full_data_for_regression.dta, clear

//---------------------------------------------------------------------------------
//------------------------------------------ Figure SI4: all state mortality
//---------------------------------------------------------------------------------
qui areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
predict y_imt_TC3 if e(sample), xbd

gen modate_lbl = ym(year, month)
format modate_lbl %tm

tw (line tdths_ttpop modate_lbl, color(orange*2%50) subtitle(,size(small)) by(adm_name , note(""))) (sc y_imt_TC3 modate_lbl, msize(vtiny) color(cranberry%50) by(adm_name , note(""))) if year>=1950, xlabel(-120(240)660,format(%tmCY)) ylabel(50(25)125) legend(order(1 "Measured" 2 "Predicted") size(small)) xtitle("")  ytitle("Monthly All Cause Mortality Rate (per 100,000)", size(small)) ysize(10.5) xsize(9) 
graph export figures/appendix/figureSI4_mortality_allstates.pdf, replace 

//---------------------------------------------------------------------------------
//------------------------------------------ Figure SI2: all state wind speeds
//---------------------------------------------------------------------------------
tw bar maxs modate_lbl if year>=1950, by(adm_name , note("")) subtitle(,size(small)) color(blue) xlabel(-120(240)660,format(%tmCY)) xtitle("") ytitle("Monthly Maximum Windspeed (m/s)", size(small)) ysize(10) xsize(9) 
graph export figures/appendix/figureSI2_windspeed_allstates.pdf, replace 






//---------------------------------------------------------------------------------	
//-------------------------------------- Figure SI10: Plotting temperature
//---------------------------------------------------------------------------------

use output/full_data_for_regression.dta, clear

*areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate)
est use output/beta/m_linear.ster

gen temp_deaths_rate_predict = .
gen temp_deaths_predict = .
gen temp_deaths_min_T=.
gen temp_deaths_predict_hot=.
gen temp_deaths_predict_cold=.

forvalues i = 1/55 { //these are state fips (need to go up to 55 to include VA, WA, WV and WI)
capture{
* Death rate Prediction
replace temp_deaths_rate_predict = _b[_iT_stfXtavg_`i']*tavg + _b[_iT_stfXtavga`i']*tavg_2 if stfips == `i'
sum temp_deaths_rate_predict if stfips == `i'
replace temp_deaths_rate_predict = temp_deaths_rate_predict - r(min) if stfips == `i'

* Total death prediction
replace temp_deaths_predict = totalpop/100000*_b[_iT_stfXtavg_`i']*tavg + totalpop/100000*_b[_iT_stfXtavga`i']*tavg_2 if stfips == `i'
sum temp_deaths_predict if stfips == `i'
replace temp_deaths_predict = temp_deaths_predict - r(min) if stfips == `i'

* each state's temperature that causes min excess deaths
replace temp_deaths_min_T = -(_b[_iT_stfXtavg_`i'])/(_b[_iT_stfXtavga`i']*2) if stfips == `i'

* splitting deaths from cold and deaths from hot
replace temp_deaths_predict_cold = totalpop/100000*_b[_iT_stfXtavg_`i']*tavg + totalpop/100000*_b[_iT_stfXtavga`i']*tavg_2 if stfips == `i' & tavg <=temp_deaths_min_T
sum temp_deaths_predict_cold if stfips == `i'
replace temp_deaths_predict_cold = temp_deaths_predict_cold - r(min) if stfips == `i' & tavg <=temp_deaths_min_T

replace temp_deaths_predict_hot = totalpop/100000*_b[_iT_stfXtavg_`i']*tavg + totalpop/100000*_b[_iT_stfXtavga`i']*tavg_2 if stfips == `i' & tavg >temp_deaths_min_T
sum temp_deaths_predict_hot if stfips == `i'
replace temp_deaths_predict_hot = temp_deaths_predict_hot - r(min) if stfips == `i' & tavg >temp_deaths_min_T
}
}
sort adm_id tavg
tw line temp_deaths_rate_predict tavg, by(adm_name, note(" ")) xtitle("Average Temperature") ytitle("Predicted Death per 100,000") 
graph export figures/appendix/figureSI10_temperature_death_rate_state.pdf, replace



//---------------------------------------------------------------------------------	
//-------------------------------------------------  Figure SI8: Checking model fit
//---------------------------------------------------------------------------------
use output/full_data_for_regression.dta, clear

*visually inspect model fit when no TC forcing

areg tdths_ttpop _iy_* _imt*, absorb(_im_)
predict y_imt_noTC if e(sample), xbd

mkdir figures/appendix/no_TC_in_model_plots
cd figures/appendix/no_TC_in_model_plots
*local graphs " "
gen date = ym( year, month)
format date %tm

preserve
drop if date<-120
foreach i in NJ FL {
	tw (line tdths_ttpop date) (sc y_imt_noTC date, msize(tiny) mcolor(black)) if state_abbrev == "`i'", title("State `i'") xlabel(-120(120)660,format(%tmCY)) /*name(state_`i'_pred, replace)*/ xtitle("") legend(order(1 "Measured" 2 "Predicted")) ytitle("Monthly All Cause Mortality Rate (per 100,000)")
	graph export state_`i'_pred.png, replace
	graph save state_`i'_pred.gph, replace
	}
restore

cd ../../..

graph combine figures/appendix/no_TC_in_model_plots/state_NJ_pred.gph figures/appendix/no_TC_in_model_plots/state_FL_pred.gph, col(1)
graph save figures/appendix/no_TC_in_model_plots/no_TC_in_model_plots.gph , replace

*visually inspect model fit when including TC forcing and temp (no leads, years 1950-2012)

areg tdths_ttpop _iy_* _imt* _y_* _iT_* _L*_maxs, absorb(_im_)
predict y_imt_TC1 if e(sample), xbd

capture: mkdir figures/appendix/TC_temp_in_model_plots
cd figures/appendix/TC_temp_in_model_plots
preserve
drop if date<-120
foreach i in NJ FL {
	tw (line tdths_ttpop date) (sc y_imt_TC1 date, msize(tiny) mcolor(black)) if state_abbrev == "`i'", title("state `i' years 1950-2015 w/ TCs")  xlabel(-120(120)660,format(%tmCY)) xtitle("") legend(order(1 "Measured" 2 "Predicted")) ytitle("Monthly All Cause Mortality Rate (per 100,000)")
	graph export state_`i'_pred.png, replace
	graph save state_`i'_pred.gph, replace
	}
restore
cd ../../..

graph combine figures/appendix/TC_temp_in_model_plots/state_NJ_pred.gph figures/appendix/TC_temp_in_model_plots/state_FL_pred.gph, col(1)
graph save figures/appendix/TC_temp_in_model_plots/TC_temp_in_model_plots.gph , replace


*visually inspect model fit when including TC forcing (include 3 yr leads, years 1950-2012)

areg tdths_ttpop _iy_* _imt* _F*_maxs _L*_maxs , absorb(_im_)
predict y_imt_TC2 if e(sample), xbd

mkdir figures/appendix/TCwLeads_in_model_plots
cd figures/appendix/TCwLeads_in_model_plots
preserve
drop if date<-120
foreach i in NJ FL {
	tw (line tdths_ttpop date) (sc y_imt_TC2 date, msize(tiny) mcolor(black)) if state_abbrev == "`i'", title("state `i' years 1950-2012 w/ TCs + leads") xlabel(-120(120)660,format(%tmCY)) xtitle("") legend(order(1 "Measured" 2 "Predicted")) ytitle("Monthly All Cause Mortality Rate (per 100,000)")
	graph export state_`i'_pred.png, replace
	graph save state_`i'_pred.gph, replace
	}
restore
cd ../../..

*graph combine figures/appendix/TC_wMonthFE_in_model_plots/state_NJ_pred.gph figures/appendix/TC_wMonthFE_in_model_plots/state_FL_pred.gph



*visually inspect model fit when including TC forcing + TEMP + Month FE (include 3 yr leads, years 1950-2012)

areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*_maxs, absorb(modate)
predict y_imt_TC3 if e(sample), xbd

mkdir figures/appendix/TC_wMonthFE_in_model_plots
cd figures/appendix/TC_wMonthFE_in_model_plots
preserve
drop if date<-120
foreach i in NJ FL {
	tw (line tdths_ttpop date) (sc y_imt_TC3 date, msize(tiny) mcolor(black)) if state_abbrev == "`i'", title("state `i' years 1950-2012 w/ TCs + leads + monthFE") xlabel(-120(120)660,format(%tmCY)) xtitle("") legend(order(1 "Measured" 2 "Predicted")) ytitle("Monthly All Cause Mortality Rate (per 100,000)")
	graph export state_`i'_pred.png, replace
	graph save state_`i'_pred.gph, replace
	}
restore
cd ../../..

graph combine figures/appendix/TC_wMonthFE_in_model_plots/state_NJ_pred.gph figures/appendix/TC_wMonthFE_in_model_plots/state_FL_pred.gph, col(1) 
graph save figures/appendix/TC_wMonthFE_in_model_plots/TC_wMonthFE_in_model_plots.gph , replace


graph combine figures/appendix/no_TC_in_model_plots/no_TC_in_model_plots.gph  figures/appendix/TC_temp_in_model_plots/TC_temp_in_model_plots.gph figures/appendix/TC_wMonthFE_in_model_plots/TC_wMonthFE_in_model_plots.gph, col(3)  ysize(5.25) xsize(10)
graph export figures/appendix/figureSI8_examining_model_fit.pdf, replace







//---------------------------------------------------------------------------------
//---------------------- Figure SI7: Density and cumulative density wind speed
//---------------------------------------------------------------------------------



tw (histogram maxs) , xtitle("wind speed (m/s)") ///
	legend(ring(0) position(1)  col(1) size(small) order(1 "wind speed, including 0" 2 "wind speed, excluding 0"))
grinset b=20 l=60, size(50 35) scale(1) : tw (histogram maxs if maxs>0, color(blue%30)), xtitle("")
graph save "figures/appendix/hist_maxs_inset.gph", replace

cumul maxs if maxs>0, gen(cummaxs_no0)
cumul maxs, gen(cummaxs)
sort cummaxs

tw (line cummaxs maxs, color(gray%30)) (line cummaxs_no0 maxs, color(blue%30)), ytitle("cumulative density") xtitle("wind speed (m/s)") ///
legend(ring(0) position(5)  col(1) size(small) order(1 "wind speed, including 0" 2 "wind speed, excluding 0"))
graph save "figures/appendix/cdf_maxs.gph", replace

graph combine "figures/appendix/hist_maxs_inset.gph" "figures/appendix/cdf_maxs.gph" , ysize(4) xsize(7)
graph export "figures/appendix/figureSI7_maxs_histcdf.pdf" , replace





//---------------------------------------------------------------------------------
//---------------------- Figure SI9: Distribution of the residuals
//---------------------------------------------------------------------------------


qui areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
capture: predict y_imt_TC3 if e(sample), xbd
predict resid, residuals
hist resid ,  normal
graph save "figures/appendix/residual_hist.gph", replace


preserve
collapse 	(mean) mean_resid = resid ///
			(p2) p2_resid = resid ///
			(p25) p25_resid = resid ///
			(p50) p50_resid = resid ///
			(p75) p75_resid = resid ///
			(p98) p98_resid = resid ///
			, by(modate year month)
drop if mean_resid==.
gen modate_lbl = ym(year, month)
format modate_lbl %tm
tw (line p2_resid modate_lbl , lcolor(gs14)) (line p25_resid modate_lbl, lcolor(gs10)) (line p50_resid modate_lbl, lcolor(gs6)) (line p75_resid modate_lbl, lcolor(gs10)) (line p98_resid modate_lbl, lcolor(gs14)), yline(0) ytitle("residual") xtitle("") legend(ring(0) position(11)  col(1) size(small) order(1 "2 percentile" 2 "25 percentile" 3 "median" 4 "75 percentile" 5 "98 percentile"))
graph save "figures/appendix/residual_modate.gph", replace
restore

graph combine "figures/appendix/residual_hist.gph" "figures/appendix/residual_modate.gph" , ysize(4) xsize(7)
graph export "figures/appendix/figureSI9_residual_combined.pdf" , replace





//---------------------------------------------------------------------------------	
//--------Table SI1: Store out summary statistics all cause outcome
//---------------------------------------------------------------------------------	


use "./data/DATA_hurricane_mortality_temp_month_state_19302015.dta", clear
collapse (sum) dths_tot, by(year month)
drop if year<1930
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"

tempfile totaldeaths
save `totaldeaths'


//make table
loc outcome_list "tdths_ttpop"
foreach Y of loc outcome_list {
di "`Y'"
use "output/mort_TC_date_allmodels_`Y'.dta", clear

merge 1:1 modate using `totaldeaths', nogen keep(1 3)

estpost summarize dths_tot mort_m_linear mort_m_nonlinear mort_m_cubic mort_m_linear_leads mort_m_linear_adapt_4 mort_m_linear_adapt_pool24 mort_m_nonlinear_adapt_4 mort_m_nonlinear_adapt_pool24 mort_m_cubic_adapt_pool24 p_total_m_linear p_total_m_nonlinear p_total_m_linear_leads p_total_m_linear_adapt_4 p_total_m_linear_adapt_pool24 p_total_m_nonlinear_adapt_4 p_total_m_nonlinear_adapt_pool24 p_total_m_cubic_adapt_pool24 p_total_m_cubic, listwise 
estimates store total_`Y'
local storelist = "`storelist' total_`Y'"
}
esttab `storelist' using figures/appendix/TableSI1_all_models_predicted_deaths.csv, cells("mean(fmt(3)) sum(fmt(0))") mtitle("Average Deaths/Month (1950 - 2015)" "Total TC Deaths (1950 - 2015)") replace

preserve
foreach Y of loc outcome_list {
di "`Y'"
use "output/mort_TC_date_allmodels_`Y'.dta", clear
keep if year>=2000
estpost summarize dths_tot mort_m_linear mort_m_nonlinear mort_m_cubic mort_m_linear_leads mort_m_linear_adapt_4 mort_m_linear_adapt_pool24 mort_m_nonlinear_adapt_4 mort_m_nonlinear_adapt_pool24 mort_m_cubic_adapt_pool24, listwise 
estimates store total_`Y'
local storelist = "`storelist' total_`Y'"
}
esttab `storelist' using figures/appendix/TableSI1_all_models_predicted_deaths.csv, cells("mean(fmt(3))") mtitle("Average Deaths/Month (2000 - 2015)") append
restore


use "output/mort_TC_date_allmodels_tdths_ttpop.dta", clear
collapse (sum) mort_m_linear mort_m_nonlinear mort_m_cubic mort_m_linear_leads mort_m_linear_adapt_4 mort_m_linear_adapt_pool24 mort_m_nonlinear_adapt_4 mort_m_nonlinear_adapt_pool24 mort_m_cubic_adapt_pool24
gen storm_count = 501
local model_list "m_linear m_nonlinear m_linear_leads m_linear_adapt_4 m_linear_adapt_pool24 m_nonlinear_adapt_4 m_nonlinear_adapt_pool24 m_cubic_adapt_pool24 m_cubic"
foreach m of loc model_list {
	
	gen meanTC_`m' = mort_`m'/storm_count
	
}


use "output/mort_TC_state_total_allmodels_tdths_ttpop.dta", clear
gen prop_total_cubic = mort_m_cubic_adapt_pool24 / dths_tot
order adm_nam prop_total_cubic
tab prop_total_cubic





//---------------------------------------------------------------------------------	
//--------Table SI2: Summary statistics for multiple outcomes
//---------------------------------------------------------------------------------	

loc outcome_list "_0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop"
foreach Y of loc outcome_list {
di "`Y'"
use "output/mort_TC_date_allmodels_`Y'.dta", clear
estpost summarize  mort_m_linear mort_m_cubic_adapt_pool24 p_total_m_cubic_adapt_pool24 p_total_m_linear, listwise 
estimates store total_`Y'
local storelist = "`storelist' total_`Y'"
}
esttab `storelist' using figures/appendix/TableSI2_all_outcomes_predicted_deaths.csv, cells("mean(fmt(3)) sum(fmt(0))") replace


//--------Additional summary statistics for multiple outcomes

use data/DATA_hurricane_mortality_temp_month_state_19302015.dta, clear
drop if stfips==2 | stfips==15
keep if year>=1960
drop if year>2015
collapse (sum) _0000_apop=dths_0000 _0144_apop=dths_0144 _4564_apop=dths_4564 _6599_apop =dths_6599 _black_bpop =dths_black _white_wpop=dths_white pop_0000 pop_0144 pop_4564 pop_6599 pop_black pop_white (mean) dths_0000_mean = dths_0000 dths_0144_mean = dths_0144 dths_4564_mean = dths_4564 dths_6599_mean = dths_6599 dths_black_mean = dths_black dths_white_mean = dths_white, by(adm_name)
gen adm_name_t = strupper(adm_name)
drop adm_name
rename adm_name_t adm_name
tempfile alldths
save `alldths'


use output/full_data_for_regression.dta, clear

drop modate
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1960 | year>2015

//------by date

collapse (sum) _0000_apop=dths_0000 _0144_apop=dths_0144 _4564_apop=dths_4564 _6599_apop =dths_6599 _black_bpop =dths_black _white_wpop=dths_white  (mean) dths_0000_mean = dths_0000 dths_0144_mean = dths_0144 dths_4564_mean = dths_4564 dths_6599_mean = dths_6599 dths_black_mean = dths_black dths_white_mean = dths_white, by(modate)

foreach m in _0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop {

	merge 1:1 modate using output/mort_TC_date_total_m_linear_`m'.dta, nogen
	*rename mort_m_cubic_adapt_pool24 mort_m_cubic_`m'
	rename mort mort_m_linear_`m'

	gen p_`m'_TC = mort_m_linear_`m' / `m' 
	replace p_`m'_TC = 0 if p_`m'_TC== .
	label variable p_`m'_TC "proportion deaths from TCs by age group"
	
}

preserve 
collapse (sum) _0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop  mort_m_linear__0000_apop mort_m_linear__0144_apop mort_m_linear__4564_apop mort_m_linear__6599_apop mort_m_linear__black_bpop mort_m_linear__white_wpop 
restore

preserve
collapse (mean) mort_m_linear__0000_apop mort_m_linear__0144_apop mort_m_linear__4564_apop mort_m_linear__6599_apop mort_m_linear__black_bpop mort_m_linear__white_wpop p__0000_apop_TC p__0144_apop_TC p__4564_apop_TC p__6599_apop_TC p__black_bpop_TC p__white_wpop_TC (sum) sum_mort_m_linear__0000_apop = mort_m_linear__0000_apop sum_mort_m_linear__0144_apop  =mort_m_linear__0144_apop sum_mort_m_linear__4564_apop = mort_m_linear__4564_apop sum_mort_m_linear__6599_apop  = mort_m_linear__6599_apop sum_mort_m_linear__black_bpop  =mort_m_linear__black_bpop sum_mort_m_linear__white_wpop = mort_m_linear__white_wpop
restore

//------by state

use output/full_data_for_regression.dta, clear

drop modate
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1960 | year>2015

preserve
collapse (sum) _0000_apop=dths_0000 _0144_apop=dths_0144 _4564_apop=dths_4564 _6599_apop =dths_6599 _black_bpop =dths_black _white_wpop=dths_white  (mean) dths_0000_mean = dths_0000 dths_0144_mean = dths_0144 dths_4564_mean = dths_4564 dths_6599_mean = dths_6599 dths_black_mean = dths_black dths_white_mean = dths_white, by(adm_name adm_id)

merge 1:1 adm_name using `alldths' 

foreach m in _0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop {

	merge 1:1 adm_name using output/mort_TC_state_total_m_linear_`m'.dta, nogen
	*rename mort_m_cubic_adapt_pool24 mort_m_cubic_`m'
	rename mort mort_m_linear_`m'

	gen p_`m'_TC = mort_m_linear_`m' / `m' 
	replace p_`m'_TC = 0 if p_`m'_TC== .
	label variable p_`m'_TC "proportion deaths from TCs by age group"
	
	gen diff_TC_total_mort_`m' =   `m' - mort_m_linear_`m'
	replace diff_TC_total_mort_`m'= `m' if diff_TC_total_mort_`m'==.
	label variable diff_TC_total_mort_`m' "difference total deaths from TCs by age group"
	
}

gen total_dths_rate_0000 = (_0000_apop / pop_0000) *100000
label variable total_dths_rate_0000 "Total mortality per 100,000"
gen TC_dths_rate_0000 = (mort_m_linear__0000_apop / pop_0000) *100000
label variable TC_dths_rate_0000 "TC mortality per 100,000"
gen diff_TC_total_rate_0000= total_dths_rate_0000 - TC_dths_rate_0000
replace diff_TC_total_rate_0000 = total_dths_rate_0000 if diff_TC_total_rate_0000==.

order adm_name p_* diff_TC_total_mort_* dths_* mort_m_linear_*
drop if adm_name==""
save output/linear_state_mort_age_race.dta, replace
restore


/*
foreach m of loc model_list {
rm output/mort_TC_date_total_`m'_tdths_ttpop.dta
rm output/mort_TC_state_total_`m'_tdths_ttpop.dta
}

*/


//---------------------------------------------------------------------------------
//--------------------------------------------------- Figure SI13b: time trends
//---------------------------------------------------------------------------------
// Use the 4 bin version
use output/full_data_for_regression.dta, clear

xtile time_4bins = year, n(4) 
tab year time_4bins

areg tdths_ttpop _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.time_4bins, absorb(modate)

est save "output/beta/m_timetrend_4bins.ster", replace

**** Create dataset of the coefficients *****
clear
loc x = _N+240+1
set obs `x'
gen xaxis=_n -1

est use "output/beta/m_timetrend_4bins.ster"

forvalue q = 1/4 {
	*** get summed coefficients from event-study models
	gen Bs_`q' = .
	gen B_`q' = .
	
	if `q'==1 {
		loc varlistsum "0"
		forvalues i = 0/240 {
			replace B_`q' = _b[_LL`i'_maxs] if xaxis == `i'
			quietly {
			loc varlistsum "`varlistsum' + _b[_LL`i'_maxs]"
			lincom `varlistsum'
			replace Bs_`q' = r(estimate) if xaxis == `i'
			}
		} //end loop over 240 lags	
	}
		
	else {
		loc varlistsum "0"
		forvalues i = 0/238 { 
			quietly{
			replace B_`q' = _b[`q'.time_4bins#c._LL`i'_maxs] if xaxis == `i'
			loc varlistsum "`varlistsum' + _b[_LL`i'_maxs] + _b[`q'.time_4bins#c._LL`i'_maxs]"
			lincom `varlistsum'
			replace Bs_`q' = r(estimate) if xaxis == `i'
			}
		} //end loop over 240 lags
	}
}

est use "output/beta/m_linear.ster"

gen B_main = .
gen CI1_s_main=.
gen CI2_s_main=.
		local varlistsum "0"
		forvalues i = 0/238 { 
		quietly{
		loc varlistsum "`varlistsum' + _b[_LL`i'_maxs]"
		lincom `varlistsum'
		replace B_main = r(estimate) if xaxis == `i'
		replace CI1_s_main = r(estimate) - 1.96 * r(se) if xaxis == `i'
		replace CI2_s_main = r(estimate) + 1.96 * r(se) if xaxis == `i'
		}
		}

		
		
tw (rarea CI1_s_main CI2_s_main xaxis, fcolor(gs14%75) lwidth(none)) ///
	(line B_main xaxis, lcolor(black))  ///
	(line Bs_* xaxis, lpattern(dash dot longdash_dot shortdash)) ///
	, yline(0) xtitle("Months Since Tropical Cyclone") legend(ring(0) position(11) col(2) size(small) )  xlabel(0(24)240) ytitle("Cummulative All Cause Mortality (per 100,000 per m/s)", size(small)) ///
   legend(order(1 "CI Main Linear" 2 "Main Linear Model" 3 "1930-1951" 4 "1952-1973" 5 "1974-1994" 6 "1995-2015")) 
graph save figures/appendix/est_by_timetrend_4bins.gph, replace
*graph export figures/appendix/est_by_timetrend_4bins.pdf, replace


//---------------------------------------------------------------------------------
//--------------------------------------------------- weighted version of the linear model
//---------------------------------------------------------------------------------
	use output/full_data_for_regression.dta, clear 
	
	gen lag = _n - _N + 240 
	replace lag = . if lag < 0

	capture: gen B_linear = .
	lab var B_linear "Total Cause Linear model"
	capture: gen CI1_linear = .
	capture: gen CI2_linear = .
	
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
	
	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		lincom `varlistsum'
		replace B_linear = r(estimate) if lag == `i'
		replace CI1_linear = r(estimate) - 1.96 * r(se) if lag == `i'
		replace CI2_linear = r(estimate) + 1.96 * r(se) if lag == `i'
		}
	}
	
	
	capture: gen B_linear_w = .
	lab var B_linear_w "Total Cause Linear model (weights)"
	capture: gen CI1_linear_w = .
	capture: gen CI2_linear_w = .
	
	
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs [aweight = totalpop], absorb(modate) 
	
	
	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		lincom `varlistsum'
		replace B_linear_w = r(estimate) if lag == `i'
		replace CI1_linear_w = r(estimate) - 1.96 * r(se) if lag == `i'
		replace CI2_linear_w = r(estimate) + 1.96 * r(se) if lag == `i'
		}
	}
	
	order lag B_linear CI1_linear CI2_linear B_linear_w CI1_linear_w CI2_linear_w
	
/*tw (rarea CI1_linear CI2_linear lag, fcolor(red*2%20) lwidth(none)) (line B_linear lag, lcolor(red*2)) ///
   (rarea CI1_linear_w CI2_linear_w lag, fcolor(ebblue*2%20) lwidth(none)) (line B_linear_w lag, lcolor(ebblue*2)) ///
	, yline(0)  xtitle("Months Since Tropical Cyclone") legend(ring(0) position(11) col(1) size(small) bmargin(tiny) rowgap(0) colgap(0)) xlabel(0(24)240)  ///
   legend(order(2 "Linear Model" 4 "Linear Model, Pop Weighted") symxsize(*.5))  ///
   ytitle("Cumulative All Cause Mortality (per 100,000 per m/s)" , size(small))
   graph save figures/appendix/est_linear_weights.gph, replace
   *graph export figures/appendix/est_linear_weights.png, replace
	
*/

//---------------------------------------------------------------------------------
//--------------------------------------------------- weighted version of the linear model
//---------------------------------------------------------------------------------
/*-------- Poisson -------*/

use output/full_data_for_regression.dta, clear 


poisson tdths_ttpop _m_* _iy_* _imt* _mi_* _iT_* _LL*_maxs

	capture: gen lag = _n - _N + 240
	capture: replace lag = . if lag < 0

	capture: gen b =.
	capture: gen ci1 =.
	capture: gen ci2 =.
	capture: gen B_leads = .
	lab var B_leads "cumulative effect with leads"
	capture: gen CI1_leads = .
	capture: gen CI2_leads = .

	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		lincom _b[_LL`i'_maxs] ,  level(95)
		replace b = r(estimate) if lag==`i'
		replace ci1 = r(lb) if lag==`i'
		replace ci2 = r(ub)  if lag==`i'
		loc varlistsum "`varlistsum' + (_LL`i'_maxs)"
		lincom `varlistsum',  level(95)
		replace B_leads = r(estimate) if lag == `i'
		replace CI1_leads = r(lb) if lag == `i'
		replace CI2_leads = r(ub) if lag == `i'
		}
	}
	
egen mean_mort_pop = mean(tdths_ttpop)
gen B_poisson_mp = B_leads * mean_mort_pop


	capture: gen B_linear = .
	lab var B_linear "Total Cause Linear model"
	capture: gen CI1_linear = .
	capture: gen CI2_linear = .
	
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
	
	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		lincom `varlistsum'
		replace B_linear = r(estimate) if lag == `i'
		replace CI1_linear = r(estimate) - 1.96 * r(se) if lag == `i'
		replace CI2_linear = r(estimate) + 1.96 * r(se) if lag == `i'
		}
	}
	
	
	capture: gen B_linear_w = .
	lab var B_linear_w "Total Cause Linear model (weights)"
	capture: gen CI1_linear_w = .
	capture: gen CI2_linear_w = .
	
	
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs [aweight = totalpop], absorb(modate) 
	
	
	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		lincom `varlistsum'
		replace B_linear_w = r(estimate) if lag == `i'
		replace CI1_linear_w = r(estimate) - 1.96 * r(se) if lag == `i'
		replace CI2_linear_w = r(estimate) + 1.96 * r(se) if lag == `i'
		}
	}
	
	order lag B_linear CI1_linear CI2_linear B_linear_w CI1_linear_w CI2_linear_w
	
/*-------------regional--------------*/
	capture: gen B_regional = .
	capture: gen CI1_regional = .
	capture: gen CI2_regional = .
	drop if region==0
	
	*replace region = 1 if region==2
	*replace region = 2 if region==3 /*limit the regions to north and south*/
	capture: xi i.region*i.modate_1, noomit pref(_mr_)

	reg tdths_ttpop _iy_* _imt* _mi_* _iT_* _mr_* _LL*_maxs 
	est save "./output/beta/m_linear_region", replace
	
	drop _mr_*

	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		lincom `varlistsum'
		replace B_regional = r(estimate) if lag == `i'
		replace CI1_regional = r(estimate) - 1.96 * r(se) if lag == `i'
		replace CI2_regional = r(estimate) + 1.96 * r(se) if lag == `i'
		} 
	}	
	
tw (rarea CI1_linear CI2_linear lag, lwidth(none) fcolor(maroon%25)) ///
   (rarea CI1_linear_w CI2_linear_w lag, fcolor(ebblue*2%20) lwidth(none)) ///
   (line B_linear_w lag, lcolor(ebblue*2)) ///
	(line B_linear lag, lcolor(maroon))	///
	(line B_poisson_mp lag, lcolor(gray) lpattern(dash)) ///
	(line B_regional lag ,  lcolor(forest_green)) ///
	(rarea CI1_regional CI2_regional lag, fcolor(forest_green%20) lwidth(none)) ///
	, yline(0, lcolor(black)) ytitle("Cumulative All Cause Mortality (per 100,000 per m/s)" , size(small)) text(10.5 172 "172 month", size(small)) xtitle("Months Since Tropical Cyclone")  xlabel(0(24)240) ///
	legend(pos(11) ring(0) col(1) size(small) symxsize(*.5) order(5 "Poisson model" 4 "Linear model" 3 "Linear model, population weighted" 6 "Linear model, regional controls" 1 "95% CI")) 
	graph save figures/appendix/est_linear_weights.gph, replace


	
	

//---------------------------------------------------------------------------------
//--------------------------------------------------- Figure SI13d: coastal population
//---------------------------------------------------------------------------------

import excel using "./data/coastline-counties-list.xlsx",  first clear

duplicates drop stfips county_fips, force

tempfile coastal
save `coastal'


import delimited using "./data/us.1969_2020.19ages.adjusted.txt", clear
	
gen year = substr(v1,1,4)
gen state_abbrev = substr(v1,5,2)
gen stfips = substr(v1,7,2)
gen county_fips = substr(v1,9,3)
gen registry = substr(v1,12,2)
gen race = substr(v1,14,1)
gen origin = substr(v1,15,1)
gen sex = substr(v1,16,1)
gen age = substr(v1,17,2)
gen population = substr(v1,19,8)
destring(population), replace

collapse (sum) population, by(year state_abbrev stfips county_fips)	


merge m:1 stfips county_fips using `coastal', nogen

replace coastal_index = 0 if coastal_index==.

drop FIPS STATE_NAME COUNTY_NAME 

gen coastal_pop = population if coastal_index ==1
replace coastal_pop = 0 if coastal_pop==.

collapse (sum) population coastal_pop , by(year state_abbrev stfips)	

drop if year==""
drop if state_abbrev=="AK" | state_abbrev=="HI"

gen frac_coastal_pop = coastal_pop/population
destring(year), replace


destring(stfips), replace

save output/coastal_pop_19692020.dta, replace


use "./output/full_data_for_regression.dta", clear

merge m:1 stfips year using output/coastal_pop_19692020.dta , keep(1 3) nogen
/*not matched from master is the years before 1970*/

egen frac_coastal_popmean_st = mean(frac_coastal_pop), by(stfips)
xtile pop_pct_4 = frac_coastal_popmean_st, n(4)
replace pop_pct_4 = 1 if pop_pct_4==2

**** coastal popualtion quartile with 4 bins
areg tdths_ttpop _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pop_pct_4, absorb(modate)
est save "output/beta/m_coastalpop_linear_groups4", replace

**** Create dataset of the coefficients *****
clear
loc x = _N+240+1
set obs `x'
gen xaxis=_n -1

est use "./output/beta/m_coastalpop_linear_groups4.ster"

forvalue q = 1/4 {
	*** get summed coefficients from event-study models
	gen Bs_`q' = .
	gen B_`q' = .
	
	if `q'==2 {
		continue
	}
	
	if `q'==1 {
		loc varlistsum "0"
		forvalues i = 0/240 {
			replace B_`q' = _b[_LL`i'_maxs] if xaxis == `i'
			quietly {
			loc varlistsum "`varlistsum' + _b[_LL`i'_maxs]"
			lincom `varlistsum'
			replace Bs_`q' = r(estimate) if xaxis == `i'
			}
		} //end loop over 240 lags	
	}

	else {
		loc varlistsum "0"
		forvalues i = 0/238 { 
			quietly{
			replace B_`q' = _b[`q'.pop_pct_4#c._LL`i'_maxs] if xaxis == `i'
			loc varlistsum "`varlistsum' + _b[_LL`i'_maxs] + _b[`q'.pop_pct_4#c._LL`i'_maxs]"
			lincom `varlistsum'
			replace Bs_`q' = r(estimate) if xaxis == `i'
			}
		} //end loop over 240 lags
	}
}

est use "./output/beta/m_linear.ster"

gen B_main = .
gen CI1_s_main=.
gen CI2_s_main=.
		local varlistsum "0"
		forvalues i = 0/238 { 
		quietly{
		loc varlistsum "`varlistsum' + _b[_LL`i'_maxs]"
		lincom `varlistsum'
		replace B_main = r(estimate) if xaxis == `i'
		replace CI1_s_main = r(estimate) - 1.96 * r(se) if xaxis == `i'
		replace CI2_s_main = r(estimate) + 1.96 * r(se) if xaxis == `i'
		}
		}

		
		
tw (rarea CI1_s_main CI2_s_main xaxis, fcolor(gs14%75) lwidth(none)) ///
	(line B_main xaxis, lcolor(black))  ///
	(line Bs_1 Bs_3  Bs_4 xaxis,  lcolor(lavender*1.5 lavender*1 lavender*.5)) ///
	, yline(0) xtitle("Months Since Tropical Cyclone") legend(ring(0) position(11) col(2) size(small) )  xlabel(0(24)240) ytitle("Cummulative All Cause Mortality (per 100,000 per m/s)", size(small)) ///
   legend(order(2 "Main Linear Model" 3 "1 & 2 quartile coastal" 4 "3 quartile coastal" 5 "4 quartile coastal") col(1) size(small) region(lwidth(none))) 
  graph export ./figures/est_coastal_pop.png, replace   
   graph save ./figures/est_coastal_pop.gph, replace   
   

graph combine figures/appendix/est_linear_weights.gph figures/appendix/est_by_timetrend_4bins.gph figures/appendix/APPENDIX_est_adpatation.gph figures/est_coastal_pop.gph, col(2) ysize(5.25) xsize(10)
graph export figures/appendix/figureSI13_model_adapt_time_weight_combined.pdf, replace

rm figures/appendix/est_linear_weights.gph 
rm figures/appendix/est_by_timetrend_4bins.gph 
rm figures/appendix/APPENDIX_est_adpatation.gph


//---------------------------------------------------------------------------------
//----------------------column of direct "offical" deaths in appendix table 1
//---------------------------------------------------------------------------------


//----------------------prep data of total deaths, deaths from temp, TC (predicted), TC Direct
use output/mort_TC_date_total_m_nonlinear_adapt_pool24_tdths_ttpop.dta, clear
collapse (sum) mort, by(modate)
tempfile tcmort
save `tcmort'


import delimited using data/natural-disasters-directdeath.csv, varn(1) clear
keep year entity numberofdeathsfromstorms deathratesfromstorms injuryratesfromstorms
keep if entity=="United States"
keep if year>1930

tempfile directdeathyear
save `directdeathyear'

use "data/DATA_hurricane_mortality_temp_month_state_19302015.dta", clear
collapse (sum) dths_tot, by(year month)
drop if year<1930
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"

tempfile totaldeaths
save `totaldeaths'

use data/directdeaths_19302015.dta, clear
duplicates drop
collapse (sum) deaths_direct, by(year month modate)

merge 1:1 modate using `tcmort', keep(2 3)  nogen

merge 1:1 modate using `totaldeaths', keep(3) nogen

keep if modate>=608 & modate<=1392

preserve
egen katrina = max(deaths_direct)
gen direct_death_noKatrina = deaths_direct
replace direct_death_noKatrina  = . if direct_death_noKatrina == katrina

collapse (sum) dths_tot deaths_direct direct_death_noKatrina (mean) deaths_direct_mean = deaths_direct direct_death_noKatrina_mean = direct_death_noKatrina

gen p_direct_total = deaths_direct/dths_tot
gen avg_direct_storm = deaths_direct/501
gen avg_direct_death_noKatrina = direct_death_noKatrina/501

outsheet using figures/total_direct_deaths.csv, comma replace 
restore



/**** Generate the month - state- storm numbers ****/

use output/mortality_predict_m_cubic_adapt_pool24_tdths_ttpop, clear
//month - state- storm 
gen storm_month_counter = 0
replace storm_month_counter = 0 if mort>0

// STORM BY MONTH
collapse (sum) mort, by(storm_id modate)
replace mort = . if mort==0
sort modate
by modate: egen total_month_mort = sum(mort)
gen p_month_mort_storm = mort / total_month_mort
sum p_month_mort_storm, d



use output/full_data_for_regression.dta, clear
keep if year>=1950
drop if year>2015
collapse (sum) totalpop, by(year)
tempfile totalpop
save `totalpop'

use "./output/mort_TC_date_allmodels_tdths_ttpop.dta", clear
collapse (sum) dths_tot mort_m_linear mort_m_nonlinear mort_m_linear_leads mort_m_linear_adapt_4  mort_m_linear_adapt_pool24 mort_m_nonlinear_adapt_4 mort_m_nonlinear_adapt_pool24 mort_m_cubic_adapt_pool24 mort_m_cubic , by(year)

merge 1:1 year using `totalpop'
gen mort_m_linear_rate = mort_m_linear/totalpop*100000
gen mort_m_cubic_2groups_rate = mort_m_cubic_adapt_pool24/totalpop*100000






//---------------------------------------------------------------------------------
//---------------------- Figure SI15: Cubic model with adaptation (pooled 2-4 bins)
//---------------------------------------------------------------------------------


/*-------------spline model --------*/

use output/full_data_for_regression.dta, clear

egen maxs_mean_st = mean(maxs), by(stfips)
xtile pct = maxs_mean_st, n(4)
tab pct, gen(maxs_q)
tab stfips pct
tab stfips maxs_mean_st
replace pct=2 if pct>2

gen lag = _n - _N + 240
replace lag = . if lag < -72

*drop maxs_spline1*
mkspline maxs_spline1=maxs, knots(1 8 25 40) cubic displayknots
mat knots = r(knots)

tsset stfips modate
forvalues j = 1/3 {
forvalues i = 0/240 {
	quietly{
	capture: gen _LL`i'_maxs_spline1`j'= L`i'.maxs_spline1`j' 
	}
}
}
gen counter = _n - _N + 18
replace counter = . if counter <= 0

forvalues a = 1/2 {
preserve
	if `a'==1 {
		keep if pct==1
	}
	if `a'==2 {
		keep if pct==2
	}

drop maxs_spline1*
mkspline maxs_spline1=maxs, knots(1 8 25 40) cubic displayknots
mat knots1=r(knots)

areg tdths_ttpop _iy_* _imt* _mi_* _iT_*  _LL*_maxs_spline1* , absorb(modate)
est save "output/beta/m_spline_`a'", replace
 
drop _iy_* _imt* _mi_* _iT_* //to make room for all the spline computations 

forvalues i = 0/180 {
di as error `i' 
xbrcspline _LL`i'_maxs_spline1 , values(.25 .5 1 2 4 6 8 10 12 14 16 18 20 24 28 30 35) matknots(knots1) gen(wsm`i' xb`i' lb`i' ub`i' )
}
egen xbsum`a' = rowtotal(xb*)
save "output/spline_adapt`a'.dta", replace

restore


}


use  "output/spline_adapt1.dta", clear
keep xbsum1 wsm1 adm_name stfips modate
tempfile spline1
save `spline1'

use  "output/spline_adapt2.dta", clear
keep xbsum2 wsm2 adm_name stfips modate
tempfile spline2
save `spline2'


use output/full_data_for_regression.dta, clear
gen lag = _n - _N + 240
replace lag = . if lag < -72
egen maxs_mean_st = mean(maxs), by(stfips)
xtile pct = maxs_mean_st, n(4)
tab pct, gen(maxs_q)
tab stfips pct
tab stfips maxs_mean_st
replace pct=2 if pct>2

merge 1:1 adm_name stfips modate using `spline1', nogen
merge 1:1 adm_name stfips modate using `spline2', nogen

forvalues a = 1/2 {
preserve
	if `a'==1{
		keep if pct==1
	}
	if `a'==2 {
		keep if pct==2
	}

	drop _L*
	
	tsset stfips modate
	forvalues i = 0/179 {
		quietly{
		capture: gen _L`i'D_maxs= L`i'.D.maxs
		capture: gen _L`i'D_maxs_2= L`i'.D.maxs_2
		capture: gen _L`i'D_maxs_3= L`i'.D.maxs_3
		}
	}

	capture: gen _L180D_maxs = L180.maxs
	capture: gen _L180D_maxs_2 = L180.maxs_2
	capture: gen _L180D_maxs_3 = L180.maxs_3
	
	forvalues i = 181/240 {
		quietly{
		capture: gen _LL`i'_maxs= L`i'.maxs
		capture: gen _LL`i'_maxs_2= L`i'.maxs_2
		capture: gen _LL`i'_maxs_3= L`i'.maxs_3
		}
	}
	
*.25 .5 1 2 4 6 8 10 12 14 16 18 20 24 28 30 35


areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*D_maxs _L*D_maxs_2 _L*D_maxs_3 _LL*_maxs _LL*_maxs_2 _LL*_maxs_3, absorb(modate)
restore

	sort wsm`a'
	replace wsm`a' = 40 in 18
	
	capture: gen ci1_`a' = .
	capture: gen ci2_`a' = . 
	
		lincom _b[_L180D_maxs]*.25+ _b[_L180D_maxs_2]*0.0625 + _b[_L180D_maxs_3]*0.015625, level(95)
		replace ci1_`a' =  r(lb) if wsm`a' == .25
		replace ci2_`a' =  r(ub) if wsm`a' == .25
		lincom _b[_L180D_maxs]*.5+ _b[_L180D_maxs_2]*.25 + _b[_L180D_maxs_3]*.125, level(95)
		replace ci1_`a' =  r(lb)  if wsm`a' ==.5
		replace ci2_`a' =  r(ub)  if wsm`a' ==.5		
		lincom _b[_L180D_maxs]*1+ _b[_L180D_maxs_2]*1 + _b[_L180D_maxs_3]*1, level(95)
		replace ci1_`a' =  r(lb)  if wsm`a' ==1
		replace ci2_`a' =  r(ub)  if wsm`a' ==1
		lincom _b[_L180D_maxs]*2+ _b[_L180D_maxs_2]*4 + _b[_L180D_maxs_3]*8, level(95)
		replace ci1_`a' =  r(lb)  if wsm`a' ==2
		replace ci2_`a' =  r(ub)  if wsm`a' ==2
		lincom _b[_L180D_maxs]*4+ _b[_L180D_maxs_2]*16 + _b[_L180D_maxs_3]*64, level(95)
		replace ci1_`a' = r(lb) if wsm`a' ==4
		replace ci2_`a'=  r(ub)  if wsm`a' ==4
		lincom _b[_L180D_maxs]*6+ _b[_L180D_maxs_2]*36 + _b[_L180D_maxs_3]*216, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==6
		replace ci2_`a' =  r(ub) if wsm`a' ==6
		lincom _b[_L180D_maxs]*8+ _b[_L180D_maxs_2]*64 + _b[_L180D_maxs_3]*512, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==8
		replace ci2_`a'=  r(ub) if wsm`a' ==8
		lincom _b[_L180D_maxs]*10+ _b[_L180D_maxs_2]*100 + _b[_L180D_maxs_3]*1000, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==10
		replace ci2_`a'=  r(ub) if wsm`a' ==10
		lincom _b[_L180D_maxs]*12+ _b[_L180D_maxs_2]*144 + _b[_L180D_maxs_3]*1728, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==12
		replace ci2_`a' =  r(ub) if wsm`a' ==12
		lincom _b[_L180D_maxs]*14+ _b[_L180D_maxs_2]*196 + _b[_L180D_maxs_3]*2744, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==14
		replace ci2_`a' =  r(ub) if wsm`a' ==14
		lincom _b[_L180D_maxs]*16+ _b[_L180D_maxs_2]*256 + _b[_L180D_maxs_3]*4096, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==16
		replace ci2_`a' =  r(ub) if wsm`a' ==16
		lincom _b[_L180D_maxs]*18+ _b[_L180D_maxs_2]*324 + _b[_L180D_maxs_3]*5832, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==18
		replace ci2_`a' =  r(ub) if wsm`a' ==18
		lincom _b[_L180D_maxs]*20+ _b[_L180D_maxs_2]*400 + _b[_L180D_maxs_3]*8000, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==20
		replace ci2_`a'=  r(ub) if wsm`a' ==20
		lincom _b[_L180D_maxs]*24+ _b[_L180D_maxs_2]*576 + _b[_L180D_maxs_3]*13824, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==24
		replace ci2_`a' =  r(ub) if wsm`a' ==24
		lincom _b[_L180D_maxs]*28+ _b[_L180D_maxs_2]*784 + _b[_L180D_maxs_3]*21952, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==28
		replace ci2_`a' =  r(ub) if wsm`a' ==28		
		lincom _b[_L180D_maxs]*30+ _b[_L180D_maxs_2]*900 + _b[_L180D_maxs_3]*27000, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==30
		replace ci2_`a' =  r(ub) if wsm`a' ==30		
		lincom _b[_L180D_maxs]*35+ _b[_L180D_maxs_2]*1225 + _b[_L180D_maxs_3]*42875, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' ==35
		replace ci2_`a' =  r(ub) if wsm`a' ==35
		lincom _b[_L180D_maxs]*40+ _b[_L180D_maxs_2]*1600 + _b[_L180D_maxs_3]*64000, level(95)
		replace ci1_`a'=  r(lb) if wsm`a' == 40
		replace ci2_`a' =  r(ub) if wsm`a' == 40	


capture: gen xb`a'_cubic = _b[_L180D_maxs]*wsm`a' + _b[_L180D_maxs_2]*wsm`a'^2  + _b[_L180D_maxs_3]*wsm`a'^3
	
	
	
	tsset stfips modate
	forvalues i = 0/179 {
		quietly{
		capture: gen _L`i'D_maxs= L`i'.D.maxs
		}
	}

	capture: gen _L180D_maxs = L180.maxs
	
	forvalues i = 181/240 {
		quietly{
		capture: gen _LL`i'_maxs= L`i'.maxs
		}
	}
	
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*D_maxs _LL*_maxs, absorb(modate)

	capture: gen B_linear_`a' = .
	capture: gen ci1_linear_`a' = .
	capture: gen ci2_linear_`a' = .

	foreach m in .25 .5 1 2 4 6 8 10 12 14 16 18 20 24 28 30 35 40 {
		quietly{
		lincom _b[_L180D_maxs]*`m', level(95)
		replace B_linear_`a' = r(estimate) if wsm`a'==`m'
		replace ci1_linear_`a' = r(lb) if wsm`a'==`m'
		replace ci2_linear_`a'= r(ub) if wsm`a'==`m'
		}
	} 	

}

tw (line xbsum1 xb1_cubic B_linear_1 wsm1 if wsm1<=30, lpattern(dash solid solid)) ///
(rarea ci1_1 ci2_1 wsm1 if wsm1<=30, lwidth(none) fcolor(gs14%50) yline(0)) ///
(rarea ci1_linear_1 ci2_linear_1 wsm1 if wsm1<=30, lwidth(none) fcolor(gs10%25) yline(0)) ///
, legend(order( 2 "Cubic Model" 4 "Cubic Model 95% CI" 3 "Linear Model" 5 "Linear Model 95% CI" 1 "Cubic Spline Model") pos(11) ring(0) col(1))  xlabel(0(10)30) ///
xtitle("wind speed (m/s)") ytitle("All Cause Cumulative Mortality (per 100,000)") title("Low Average Exposure" "After 180 Months", size(medium))
graph save "figures/appendix/spline_adapt1.gph", replace

sort wsm2
replace xbsum2 = . if wsm2==40
tw (line xbsum2 xb2_cubic B_linear_2 wsm2, lpattern(dash solid solid))  ///
(rarea ci1_2 ci2_2 wsm2, lwidth(none) fcolor(gs14%50) yline(0)) ///
(rarea ci1_linear_2 ci2_linear_2 wsm2, lwidth(none) fcolor(gs10%25) yline(0)) ///
, xtitle("wind speed (m/s)") legend(off) /*legend(order( 2 "Cubic Model" 4 "Cubic Model 95% CI" 3 "Linear Model" 5 "Linear Model 95% CI" 1 "Cubic Spline Model") pos(11) ring(0) col(1))  xlabel(0(10)30) */ ///
 ytitle("All Cause Cumulative Mortality (per 100,000)") title("High Average Exposure" "After 180 Months", size(medium))
graph save "figures/appendix/spline_adapt2.gph", replace



graph combine figures/appendix/spline_adapt1.gph figures/appendix/windspeed_kernel_density_adapt1.gph, col(1) xcommon
graph save figures/appendix/est_a1_nonlinear3_180_v2.gph , replace

graph combine figures/appendix/spline_adapt2.gph figures/appendix/windspeed_kernel_density_adapt2.gph, col(1) xcommon
graph save figures/appendix/est_a2_nonlinear3_180_v2.gph , replace

graph combine figures/appendix/est_a1_nonlinear3_180_v2.gph figures/appendix/est_a2_nonlinear3_180_v2.gph, col(2) 
graph export figures/appendix/figureSI15_est_cubic_adaptation_combine.pdf, replace



rm figures/appendix/est_by_adapt1_nonlinear3_180_v2.gph 
rm figures/appendix/windspeed_kernel_density_adapt1.gph
rm figures/appendix/est_a1_nonlinear3_180_v2.gph
rm figures/appendix/est_by_adapt2_nonlinear3_180_v2.gph 
rm figures/appendix/windspeed_kernel_density_adapt2.gph
rm figures/appendix/est_a2_nonlinear3_180_v2.gph
rm figures/appendix/windspeed_kernel_density.gph




   


disp("-------------------------APPENDIX FIGURES DONE-----------------------")

