
//monthly data during 1851-2015

use data/DATA_hurricane_mortality_temp_month_state_18512015.dta, clear

//make the fixed effects
egen _im_ = group(stfips month)

forvalues i = 36(-1)1 {
	quietly{
	gen _F`i'D_maxs= F`i'.D.maxs
	
	gen sim_coeff_F`i'D_maxs = .
	lab var sim_coeff_F`i'D_maxs "simulation results: coeff on F`i'D_maxs"
	}
}

forvalues i = 0/239 {
	quietly{
	gen _L`i'D_maxs= L`i'.D.maxs
	
	gen sim_coeff_L`i'D_maxs = .
	lab var sim_coeff_L`i'D_maxs "simulation results: coeff on L`i'D_maxs"
	}
}

gen _L240_maxs = L240.maxs
gen sim_coeff_L240_maxs = .
lab var sim_coeff_L240_maxs "simulation results: coeff on L240_maxs"


//centering modate on sample
sum modate if year > 1949
replace modate = modate - round(r(mean))

forvalues i = 1/8 {
	gen modate_`i' = modate^`i'
}

*month-by-state fixe effects
quietly: xi i.stfips*i.month, noomit pref(_mi_)

*state-by-month specific linear trend (need this to account for changes in seasonality over time
quietly: xi i.stfips*modate_1 i.stfips*modate_2 i.stfips*modate_3 i.stfips*modate_4 i.stfips*modate_5 i.stfips*modate_6  i.stfips*modate_7 i.stfips*modate_8  , noomit pref(_iy_)

*state-by-month specific linear trend (need this to account for changes in seasonality over time
quietly: xi i._im_*modate_1 i._im_*modate_2, noomit pref(_imt)

*state-specific cubic in temperature
quietly: xi i.stfips*tavg i.stfips*tavg_2 , noomit pref(_iT_)

*month-of-sample fixed effects
quietly: xi i.modate_1, noomit pref(_m_)

*year fixed effects
quietly: xi i.year, noomit pref(_y_)

drop _imt_im__* // since already encoded in _im_
drop _iy_stfips_* // since already encoded in _im_ 
drop _iT_stfips_* // since already encoded in _im_ 

encode adm_name, gen(adm_id)

drop _L*_maxs _F*D_maxs

tsset
gen obsN = _n

encode adm_name, gen(adm_ID)
sum adm_ID
loc total_adm_count = r(max)

sum modate
gen month_ID = modate - r(min) + 1
sum month_ID
loc total_period_count = r(max)

*loc total_simulations = 1000
loc total_simulations = 1

//*
//----------------------------------------WHITHIN MONTH-YEAR RANDOMIZATION
	
set seed 12345
	
forvalues x = 1/`total_simulations'{
	
	disp("simulation run: `x'/`total_simulations'")
	quietly{
	
	capture: drop fake_sub_index fake_pre_index fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs
	
	//sample TC data uniformly within month-year across panel units with replacement 
	
	sort month_ID adm_ID
	gen fake_sub_index = runiform(1, `total_adm_count')
	gen fake_pre_index = (month_ID-1)*`total_adm_count'
	gen fake_index = fake_pre_index + fake_sub_index
	gen maxs_rnd = maxs[fake_index]
	tsset

	*check randomization looks right:
	*Should be the same (approx):
	//plot maxs modate
	//plot maxs_rnd modate
	
	*Should be different:
	//plot maxs adm_ID
	//plot maxs_rnd adm_ID

	
	//construct lags using the fake data
	*forvalues i = 36(-1)1 {
	*	gen _F`i'D_maxs= F`i'.D.maxs_rnd
	*}

	forvalues i = 0/239 {
		gen _L`i'D_maxs= L`i'.D.maxs_rnd
	}
	
	gen _L240_maxs = L240.maxs_rnd

	//run the model with the fake date
	//areg tdths_ttpop _iy_*  _F*_maxs _L*_maxs, absorb(_im_)
	//areg tdths_ttpop _iy_* _imt* _y_* _iT_* _L*_maxs, absorb(_im_)
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*_maxs, absorb(modate) // this runs!

	//store coeffs in the xth obs for each run
	*forvalues i = 36(-1)1 {
	*	replace sim_coeff_F`i'D_maxs = _b[_F`i'D_maxs] if _n == `x'
	*}

	forvalues i = 0/239 {
		replace sim_coeff_L`i'D_maxs = _b[_L`i'D_maxs] if _n == `x'
	}

	replace sim_coeff_L240_maxs =_b[_L240_maxs] if _n == `x'
	}
	
}

	capture: drop fake_sub_index fake_pre_index fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs

	capture: mkdir "output/randomization/whithin_monthyear_randomization_output"
	
	tsset
	
	save "output/randomization/whithin_monthyear_randomization_output/whithin_monthyear_`total_simulations'_simulation_output", replace
	outsheet sim_coeff_* using "output/randomization/whithin_monthyear_randomization_output/whithin_monthyear_`total_simulations'_simulation_output.csv" if _n<=`total_simulations', comma replace 
	
	

	
//----------------------------------------CROSS-PANEL RANDOMIZATION
//*
set seed 12345
	
forvalues x = 1/`total_simulations'{
	
	disp("simulation run: `x'/`total_simulations'")
	quietly{
	
	capture: drop fake_sub_index fake_pre_index fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs
	
	//block sample TC data holding time structure fixed but swap across panel units with replacement 
	
	sort adm_ID month_ID
	
	gen fake_sub_index = month_ID
	
	gen fake_pre_index_temp = .
	replace fake_pre_index = floor(runiform(0, `total_adm_count'))*`total_period_count' if month_ID == 1
	bysort adm_ID: egen fake_pre_index = max(fake_pre_index_temp)
	
	gen fake_index = fake_pre_index + fake_sub_index
	gen maxs_rnd = maxs[fake_index]
	tsset
	
	capture: drop fake_pre_index_temp

	*check randomization looks right:
	*Should be the same (approx):
	//plot maxs modate
	//plot maxs_rnd modate
	
	*Should be different:
	//plot maxs adm_ID
	//plot maxs_rnd adm_ID

	
	//construct lags using the fake data
	*forvalues i = 36(-1)1 {
	*	gen _F`i'D_maxs= F`i'.D.maxs_rnd
	*}

	forvalues i = 0/239 {
		gen _L`i'D_maxs= L`i'.D.maxs_rnd
	}
	
	gen _L240_maxs = L240.maxs_rnd

	//run the model with the fake date
	//areg tdths_ttpop _iy_*  _F*_maxs _L*_maxs, absorb(_im_)
	//areg tdths_ttpop _iy_* _imt* _iT_* _L*_maxs, absorb(_im_)
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*_maxs, absorb(modate) // this runs!

	//store coeffs in the xth obs for each run
	*forvalues i = 36(-1)1 {
	*	replace sim_coeff_F`i'D_maxs = _b[_F`i'D_maxs] if _n == `x'
	*}

	forvalues i = 0/239 {
		replace sim_coeff_L`i'D_maxs = _b[_L`i'D_maxs] if _n == `x'
	}

	replace sim_coeff_L240_maxs =_b[_L240_maxs] if _n == `x'
	}
	
}

	capture: drop fake_sub_index fake_pre_index fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs

	capture: mkdir "output/randomization/cross_panel_randomization_output"
	
	tsset
	
	save "output/randomization/cross_panel_randomization_output/cross_panel_`total_simulations'_simulation_output", replace
	outsheet sim_coeff_* using "output/randomization/cross_panel_randomization_output/cross_panel_`total_simulations'_simulation_output.csv" if _n<=`total_simulations', comma replace 
	
	
//----------------------------------------WHITHIN PANEL RANDOMIZATION

//*
set seed 12345

forvalues x = 1/`total_simulations'{
	
	disp("simulation run: `x'/`total_simulations'")
	quietly{
	
	capture: drop fake_sub_index fake_pre_index fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs
	
	//sample TC data uniformly within a panel-unit time series with replacement 

	sort adm_ID month_ID
	gen fake_sub_index = runiform(1, `total_period_count')
	gen fake_pre_index = (adm_ID-1)*`total_period_count'
	gen fake_index = fake_pre_index + fake_sub_index
	gen maxs_rnd = maxs[fake_index]
	tsset
	
	*check randomization looks right:
	*Should be the same (approx):
	//plot maxs adm_ID
	//plot maxs_rnd adm_ID
	
	*Should be different:
	//plot maxs modate
	//plot maxs_rnd modate

	
	//construct lags using the fake data
	*forvalues i = 36(-1)1 {
	*	gen _F`i'D_maxs= F`i'.D.maxs_rnd
	*}

	forvalues i = 0/239 {
		gen _L`i'D_maxs= L`i'.D.maxs_rnd
	}
	
	gen _L240_maxs = L240.maxs_rnd

	//run the model with the fake date
	*areg tdths_ttpop _iy_*  _iT_* _F*_maxs _L*_maxs, absorb(_im_)
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*_maxs, absorb(modate) // this runs!

	//store coeffs in the xth obs for each run
	*forvalues i = 36(-1)1 {
	*	replace sim_coeff_F`i'D_maxs = _b[_F`i'D_maxs] if _n == `x'
	*}

	forvalues i = 0/239 {
		replace sim_coeff_L`i'D_maxs = _b[_L`i'D_maxs] if _n == `x'
	}

	replace sim_coeff_L240_maxs =_b[_L240_maxs] if _n == `x'
	}
	
}

	capture: drop fake_sub_index fake_pre_index fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs

	capture: mkdir "output/randomization/whithin_panel_randomization_output"
	
	tsset
	
	save "output/randomization/whithin_panel_randomization_output/whithin_panel_`total_simulations'_simulation_output", replace
	outsheet sim_coeff_* using "output/randomization/whithin_panel_randomization_output/whithin_panel_`total_simulations'_simulation_output.csv" if _n<=`total_simulations', comma replace 
	

//----------------------------------------TOTAL RANDOMIZATION
//*

set seed 12345

forvalues x = 1/`total_simulations'{
	
	disp("simulation run: `x'/`total_simulations'")
	quietly{
	
	tsset
	capture: drop fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs
	
	//sample TC data uniformly with replacement 
	gen fake_index = runiform(1,_N)
	gen maxs_rnd = maxs[fake_index]

	
	//construct lags using the fake data
	*forvalues i = 36(-1)1 {
	*	gen _F`i'D_maxs= F`i'.D.maxs_rnd
	*}

	forvalues i = 0/239 {
		gen _L`i'D_maxs= L`i'.D.maxs_rnd
	}
	
	gen _L240_maxs = L240.maxs_rnd

	//run the model with the fake date
	//areg tdths_ttpop _iy_*  _F*_maxs _L*_maxs, absorb(_im_)
	//areg tdths_ttpop _iy_* _imt* _iT_* _L*_maxs, absorb(_im_)
	areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _L*_maxs, absorb(modate) // this runs!

	//store coeffs in the xth obs for each run
	*forvalues i = 36(-1)1 {
	*	replace sim_coeff_F`i'D_maxs = _b[_F`i'D_maxs] if _n == `x'
	*}

	forvalues i = 0/239 {
		replace sim_coeff_L`i'D_maxs = _b[_L`i'D_maxs] if _n == `x'
	}

	replace sim_coeff_L240_maxs =_b[_L240_maxs] if _n == `x'
	}
	
}

	capture: drop fake_index
	capture: drop maxs_rnd
	capture: drop _L*_maxs //_F*D_maxs

	capture: mkdir "output/randomization/total_randomization_output"
	
	tsset
	
	save "output/randomization/total_randomization_output/total_`total_simulations'_simulation_output", replace
	outsheet sim_coeff_* using "output/randomization/total_randomization_output/total_`total_simulations'_simulation_output.csv" if _n<=`total_simulations', comma replace 
*/
	
disp("-------------------------DONE-----------------------")










