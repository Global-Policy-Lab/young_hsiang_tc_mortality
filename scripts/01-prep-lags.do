//------------------------------------------------------------------------------
//--------------------------------------------------- prepping temp and lag variables
//------------------------------------------------------------------------------

//monthly data during 1851-2015
use data/DATA_hurricane_mortality_temp_month_state_19302015.dta, clear

//----------------------------------------- constructing normal lags
forvalues i = 72(-1)1 {
	quietly{
	gen _FF`i'_maxs= F`i'.maxs
	}
}

forvalues i = 0/240 {
	quietly{
	gen _LL`i'_maxs= L`i'.maxs
	}
}

// nonlinearity
gen maxs_2 = maxs^2

forvalues i = 0/240 {
	quietly{
	gen _LL`i'_maxs_2= L`i'.maxs_2
	}
}

gen maxs_3 = maxs^3

forvalues i = 0/240 {
	quietly{
	gen _LL`i'_maxs_3= L`i'.maxs_3
	}
}



//---------------------------------------------- creating fixed effects

*centering modate on sample
sum modate if year > 1949
replace modate = modate - round(r(mean))

forvalues i = 1/8 {
	gen modate_`i' = modate^`i'
}

*temperature nonlinearity
gen tavg_2 = tavg^2
gen tavg_3 = tavg^3

egen _im_ = group(stfips month)

*month-by-state fixed effects
quietly: xi i.stfips*i.month, noomit pref(_mi_)

*state-specific 8th order polynomial
quietly: xi i.stfips*modate_1 i.stfips*modate_2 i.stfips*modate_3 i.stfips*modate_4 i.stfips*modate_5 i.stfips*modate_6  i.stfips*modate_7 i.stfips*modate_8  , noomit pref(_iy_)

*state-by-month specific linear trend (need this to account for changes in seasonality over time
quietly: xi i._im_*modate_1 , noomit pref(_imt)

*state-specific cubic in temperature
quietly: xi i.stfips*tavg i.stfips*tavg_2 , noomit pref(_iT_)

*month-of-sample fixed effects
quietly: xi i.modate_1, noomit pref(_m_)

*year fixed effects
quietly: xi i.year, noomit pref(_y_)

drop _imt_im__* // since already encoded in _im_
drop _iy_stfips_* // since already encoded in _im_ 
drop _iT_stfips_* // since already encoded in _im_ 



save output/full_data_for_regression.dta, replace /* THIS IS THE MAIN DATASET FOR ALL REGRESSIONS */



//-------------------------------------------------------------saving population data for merging into plotting data set
use data/DATA_hurricane_mortality_temp_month_state_19302015.dta, clear
collapse (sum) totalpop , by(modate year)
collapse (mean) totalpop, by(year)
keep if year>=1950 & year<=2015
tsset year
gen t_p_change_1950_2015 =(totalpop - L65.totalpop) / L65.totalpop
gsort -totalpop
carryforward t_p_change_1950_2015, gen(p_change_1950_2015)
drop t_p_change_1950_2015
tempfile pchange
save `pchange'

use "output/full_data_for_regression.dta", clear

keep month year adm_name totalpop pop_black pop_white pop_0000 pop_0144 pop_4564 pop_6599

gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1930

replace adm_name = upper(adm_name)
sort adm_name modate

rename pop_black _black_bpop
rename pop_white _white_wpop

foreach x in  0000 0144 4564 6599 {
	rename pop_`x' _`x'_apop
}
drop if adm_name==""

merge m:1 year using `pchange', nogen

save "output/population_data_for_plotting.dta", replace



//---------------------------------------------------------computing and storm-specific mortality attribution

//-------------------------------------------------------------saving state quartile 

use "output/full_data_for_regression.dta", clear
egen maxs_mean_st = mean(maxs), by(stfips)

xtile pct = maxs_mean_st, n(4)
collapse (mean) pct, by(adm_name)
save output/pct_maxs.dta, replace


//monthly data during 1851-2015

insheet using "data/panel_by_storm__NA_USA_density_8_yr_1930_2018.csv", clear
drop pddi

gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1930 | year > 2015

rename adm_name adm_name_cap
gen adm_name = upper(adm_name_cap)
drop adm_name_cap

merge m:1 adm_name using output/pct_maxs.dta, keep(3) nogen //just keep the matches

gen firstdate_flag = .
egen min_date = min(modate)
sort firstdate_flag modate
bysort pct modate : gen seq=_n
replace firstdate_flag = 1 if modate==min_date
replace firstdate_flag = . if seq!=firstdate_flag

drop if maxs == 0 & firstdate_flag!=1 
drop firstdate_flag seq min_date
*replace pct = 2 if pct>=2 //try attribution plots pooling pct 2 - 4 together 5/10/2022

save output/plotting_deaths.dta, replace


