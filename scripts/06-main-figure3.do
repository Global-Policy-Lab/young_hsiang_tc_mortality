
	
	
//--------------------------------------------------------------------- Figure 3
disp("-------------------------STARTING FIGURE 3-----------------------")

use output/full_data_for_regression.dta, clear

/* Attribution with cubic and with 2 adaptation bins version of the model */
egen maxs_mean_st = mean(maxs), by(stfips)
xtile pct = maxs_mean_st, n(4)
tab pct, gen(maxs_q)
tab stfips pct
tab stfips maxs_mean_st
replace pct=2 if pct>2

qui areg tdths_ttpop _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct c.(_LL*_maxs_2)##i.pct c.(_LL*_maxs_3)##i.pct, absorb(modate)

est save "output/beta/m_adapt_pooled24_cubic.ster", replace

preserve
est use "output/beta/m_adapt_pooled24_cubic.ster"

forvalues q=1/2 {

	*** get coefficients from models
	quietly {
	if `q'==1 {
		forvalues t=0/240 {
		gen beta_L`t'_maxs = .
		gen beta_L`t'_maxs_2 = .
		gen beta_L`t'_maxs_3 = .
		replace beta_L`t'_maxs=_b[_LL`t'_maxs] 
		replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] 
		replace beta_L`t'_maxs_3=_b[_LL`t'_maxs_3] 
		}
		}
		else {
		forvalues t=0/240 {
		replace beta_L`t'_maxs=_b[_LL`t'_maxs] + _b[`q'.pct#_LL`t'_maxs] if pct==`q'
		replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] + _b[`q'.pct#_LL`t'_maxs_2] if pct==`q'
		replace beta_L`t'_maxs_3=_b[_LL`t'_maxs_3] + _b[`q'.pct#_LL`t'_maxs_3] if pct==`q'
		} /* end loop over t */
		} 
	} /* end quietly */ 	

}

drop modate
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1930

drop pct //recreate pct to make it easier to do the prediction
xtile pct = maxs_mean_st, n(4)

keep adm_name modate beta_L*_maxs beta_L*_maxs_2  beta_L*_maxs_3 pct

save "output/beta_data_for_plotting_cubic_adaptation_binpooled24.dta", replace

restore

 

//---------------------------------------------------------computing and storm-specific mortality attribution



//loop over each quartile to make the program run faster
forvalues q = 1/4 {

use output/plotting_deaths.dta, clear

keep if pct==`q'

egen event_id = group(storm_serial_id adm_name)
lab var event_id "unique state-by-storm identifier"

tsset event_id  modate 
tsfill, full

replace maxs = 0 if maxs == .

egen storm = group(year month storm_serial_id)
bysort event_id: egen storm_id = max(storm)
drop storm

encode storm_serial_id, gen(storm_name_id)
bysort event_id: egen x = max(storm_name_id)
label values x storm_name_id
decode x, gen(storm_name_2)
drop x
replace storm_serial_id = storm_name_2
drop storm_name_2

encode adm_name, gen(adm_id)
bysort event_id: egen x = max(adm_id)
label values x adm_id
decode x, gen(adm_name_2)
drop x
replace adm_name = adm_name_2
drop adm_name_2

drop adm_id
drop storm_name_id
//drop year month



// --------------------------- MERGE IN POPULATION DATA

order adm_name modate
sort adm_name modate
merge m:1 adm_name modate using output/population_data_for_plotting.dta
plot event_id modate if _merge == 3
keep if _merge == 3 | _merge == 1 // population data is not available prior to 1930
drop _merge

// --------------------------- MERGE IN BETA DATA

order adm_name modate
sort adm_name modate
merge m:1 adm_name modate using output/beta_data_for_plotting_cubic_adaptation_binpooled24.dta
plot event_id modate if _merge == 3

keep if _merge == 3 | _merge == 1 // since beta data is not available prior to 1950
drop _merge

gen maxs_2 = maxs^2
gen maxs_3 = maxs^3


//--------------------------- CONSTRUCT PREDICTIONS 
di as err "Begin constructing predictions for `q'"
tsset

// REPLACE WITH MORE SOPHISTICATED PREDICTION IF NONLINEAR OR CHANGING OVER TIME
gen mort = totalpop/100000*L0.maxs*beta_L0_maxs + totalpop/100000*L0.maxs^2*beta_L0_maxs_2 + totalpop/100000*L0.maxs^3*beta_L0_maxs_3

forvalues i = 1/240 {
	qui replace mort = mort + totalpop/100000*L`i'.maxs*beta_L`i'_maxs + totalpop/100000*L`i'.maxs_2*beta_L`i'_maxs_2 + totalpop/100000*L`i'.maxs_3*beta_L`i'_maxs_3
	}


replace mort = . if modate < 601 // since have incomplete history of storms for dates before 1950

//------------Collapse by storm
preserve
di as err "collapsing mortality by serial id `q'"

collapse (sum) mort, by(storm_serial_id  modate)

save output/mortality_predict_storm_`q'.dta, replace

restore

//------------Collapse by state
preserve
di as err "collapsing mortality by state"

collapse (sum) mort, by(adm_name)

save output/mortality_predict_state_`q'.dta, replace
restore
}

//------------Append the storm data together
use output/mortality_predict_storm_1.dta, clear 

forvalue q = 2/4 {
	append using output/mortality_predict_storm_`q'.dta
	rm output/mortality_predict_storm_`q'.dta
	}
rm output/mortality_predict_storm_1.dta

collapse (sum) mort, by(storm_serial_id  modate)
rename mort mortality_
reshape wide mortality_, i(modate) j(storm_serial_id) string
outsheet using attribution/mortality_predict_storm_pooled_adaptation.csv, comma replace //this is what is used for the MATLAB program to make the attribution figure


egen mort_TC_total = rowtotal(mortality_*) 
keep modate mort_TC_total

save "output/mort_TC_total_cubic.dta", replace



//------------Append the state data together
use output/mortality_predict_state_1.dta, clear 

forvalue q = 2/4 {
	append using output/mortality_predict_state_`q'.dta
	rm output/mortality_predict_state_`q'.dta
}
rm output/mortality_predict_state_1.dta

outsheet using "output/mort_TC_state_total_cubic.csv", comma replace //this is used to make the map in R code



//--------Create sum max, state month csv for Figure 3


insheet using "data/panel_by_storm__NA_USA_density_8_yr_1930_2018.csv", clear
drop pddi


drop if year < 1950 | year > 2015
gen modate = (year-1900)*12 + month
keep if maxs>0

collapse (sum) maxs, by(modate year month)

export delimited using attribution/panel_by_storm_state_collapsed.csv, replace //this is used to make the Figure 3 in Matlab





