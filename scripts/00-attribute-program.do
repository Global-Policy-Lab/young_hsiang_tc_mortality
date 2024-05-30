//-------------------------------------------------------------DEFINE PROGRAM TO PREDICT TC DEATHS


program define attribute , nclass
args Y m fixedpop

//loop over each quartile because the data is large 
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
if "`Y'"=="tdths_ttpop" {
merge m:1 adm_name modate using output/beta/`m'_`Y'.dta
}
else {
merge m:1 adm_name modate using output/beta/`m'_dths`Y'.dta	
}
plot event_id modate if _merge == 3

keep if _merge == 3 | _merge == 1 // since beta data is not available prior to 1950
drop _merge

gen maxs_2 = maxs^2
gen maxs_3 = maxs^3

//--------------------------- CONSTRUCT PREDICTIONS without population change (1950 population)
if "`fixedpop'" == "1" {
di as err "Begin constructing predictions for `Y' `m' `q' with 1950 population" 	
	
	drop year month
	gen year = int(modate/12 +1900)
	gen month = modate - ((year-1900)*12)

	gen t_pop_1950 = totalpop if year==1950
	sort adm_name modate
	by adm_name : carryforward t_pop_1950 , gen(pop_1950)
	drop t_pop_1950
	
	if "`m'" =="m_cubic_adapt_pool24" | "`m'" =="m_cubic" {
		di "`m' `Y' `fixedpop'"
		tsset
		capture confirm variable mort
		if !_rc {
                       di in red "mort exists"
					   drop mort
               }
               else {
                       di in red "mort does not exist"
               }
		gen mort = pop_1950/100000*L0.maxs*beta_L0_maxs + pop_1950/100000*L0.maxs^2*beta_L0_maxs_2 + pop_1950/100000*L0.maxs^3*beta_L0_maxs_3
		forvalues i = 1/240 {
		replace mort = mort + pop_1950/100000*L`i'.maxs*beta_L`i'_maxs + pop_1950/100000*L`i'.maxs_2*beta_L`i'_maxs_2 + pop_1950/100000*L`i'.maxs_3*beta_L`i'_maxs_3
		}
	}
	else {
		gen mort = pop_1950/100000*L0.maxs*beta_L0_maxs 
		forvalues i = 1/240 {
		qui {
			replace mort = mort + pop_1950/100000*L`i'.maxs*beta_L`i'_maxs
		}
		}
	}
	
	replace mort = 0 if mort<0 // Trucate the negatives
	replace mort = . if modate < 601 // since have incomplete history of storms for dates before 1950

	//------------Collapse by date
	preserve
	di as err "collapsing mortality by state"
	
	collapse (sum) mort, by(modate)

	save output/mortality_predict_date_`q'_1950pop.dta, replace
	restore
} // end of "`fixedpop'" == "1"

//--------------------------- CONSTRUCT PREDICTIONS without population change (1950 population)
if "`fixedpop'" == "2" {
di as err "Begin constructing predictions for `Y' `m' `q' with 2015 population" 	
	
	drop year month
	gen year = int(modate/12 +1900)
	gen month = modate - ((year-1900)*12)
	gen t_pop_2015 = totalpop if year==2015
	gen int negmodate = -modate
	bys adm_name (negmodate): carryforward t_pop_2015, gen(pop_2015)
	drop t_pop_2015
	
	if "`m'" =="m_cubic_adapt_pool24" | "`m'" =="m_cubic" {
		tsset
		gen mort = pop_2015/100000*L0.maxs*beta_L0_maxs + pop_2015/100000*L0.maxs^2*beta_L0_maxs_2 + pop_2015/100000*L0.maxs^3*beta_L0_maxs_3
		forvalues i = 1/240 {
		replace mort = mort + pop_2015/100000*L`i'.maxs*beta_L`i'_maxs + pop_2015/100000*L`i'.maxs_2*beta_L`i'_maxs_2 + pop_2015/100000*L`i'.maxs_3*beta_L`i'_maxs_3
		}
	}
	else {
		gen mort = pop_2015/100000*L0.maxs*beta_L0_maxs 
		forvalues i = 1/240 {
		qui {
			replace mort = mort + pop_2015/100000*L`i'.maxs*beta_L`i'_maxs
		}
		}
	}
	
	replace mort = 0 if mort<0 // Trucate the negatives
	replace mort = . if modate < 601 // since have incomplete history of storms for dates before 1950

	//------------Collapse by date
	preserve
	di as err "collapsing mortality by state"
	
	collapse (sum) mort, by(modate)

	save output/mortality_predict_date_`q'_2015pop.dta, replace
	restore
} // end of "`fixedpop'" == "2"

//--------------------------- CONSTRUCT PREDICTIONS with 2015 spatial distribution but 1950 levels
if "`fixedpop'" == "3" {

	drop year month
	gen year = int(modate/12 +1900)
	gen month = modate - ((year-1900)*12)
	gen t_pop_2015 = totalpop if year==2015
	gen int negmodate = -modate
	bys adm_name (negmodate): carryforward t_pop_2015, gen(pop_2015)
	drop t_pop_2015
	gen deflated_pop_2015 = pop_2015/(1+p_change_1950_2015)
	
	if "`m'" =="m_cubic_adapt_pool24" | "`m'" =="m_cubic" {
		tsset
		gen mort = deflated_pop_2015/100000*L0.maxs*beta_L0_maxs + deflated_pop_2015/100000*L0.maxs^2*beta_L0_maxs_2 + deflated_pop_2015/100000*L0.maxs^3*beta_L0_maxs_3
		forvalues i = 1/240 {
		replace mort = mort + deflated_pop_2015/100000*L`i'.maxs*beta_L`i'_maxs + deflated_pop_2015/100000*L`i'.maxs_2*beta_L`i'_maxs_2 + deflated_pop_2015/100000*L`i'.maxs_3*beta_L`i'_maxs_3
		}
	}
	else {
		gen mort = deflated_pop_2015/100000*L0.maxs*beta_L0_maxs 
		forvalues i = 1/240 {
		qui {
			replace mort = mort + deflated_pop_2015/100000*L`i'.maxs*beta_L`i'_maxs
		}
		}
	}
	
	replace mort = 0 if mort<0 // Trucate the negatives
	replace mort = . if modate < 601 // since have incomplete history of storms for dates before 1950
	
	//------------Collapse by date
	preserve
	di as err "collapsing mortality by state"
	
	collapse (sum) mort, by(modate)

	save output/mortality_predict_date_`q'_deflated2015pop.dta, replace
	restore		
} // end of "`fixedpop'" == "3"

if "`fixedpop'" == "4" { 
//--------------------------- CONSTRUCT PREDICTIONS WITH ACTUAL POPULATION

if "`Y'"=="tdths_ttpop" {
	di as err "Begin constructing predictions for `Y' `m' `q'"
	tsset
	if "`m'" =="m_nonlinear" | "`m'" =="m_nonlinear_adapt_4" | "`m'" =="m_nonlinear_adapt_pool24" {
		gen mort = totalpop/100000*L0.maxs*beta_L0_maxs + totalpop/100000*L0.maxs^2*beta_L0_maxs_2

		forvalues i = 1/240 {
		qui {
			replace mort = mort + totalpop/100000*L`i'.maxs*beta_L`i'_maxs + totalpop/100000*L`i'.maxs_2*beta_L`i'_maxs_2
		}
		}
	}

	if "`m'" =="m_cubic_adapt_pool24" | "`m'" =="m_cubic" {
		gen mort = totalpop/100000*L0.maxs*beta_L0_maxs + totalpop/100000*L0.maxs^2*beta_L0_maxs_2 + totalpop/100000*L0.maxs^3*beta_L0_maxs_3

		forvalues i = 1/240 {
		qui {
			replace mort = mort + totalpop/100000*L`i'.maxs*beta_L`i'_maxs + totalpop/100000*L`i'.maxs_2*beta_L`i'_maxs_2 + totalpop/100000*L`i'.maxs_3*beta_L`i'_maxs_3
		}
		}
	}
	if "`m'" =="m_linear" |  "`m'" == "m_linear_leads" |  "`m'" == "m_linear_adapt_4" |  "`m'" == "m_linear_adapt_pool24" {
		gen mort = totalpop/100000*L0.maxs*beta_L0_maxs 
		forvalues i = 1/240 {
		qui {
			replace mort = mort + totalpop/100000*L`i'.maxs*beta_L`i'_maxs
		}
		}
	}
} // "`Y'"=="tdths_ttpop"

else { // all "`Y'"!="tdths_ttpop"
	di as err "Begin constructing predictions for `Y' `m' `q'"
	drop if year<1960 // beta not avaliable prior to 1960 for age and race specific mortality
	tsset
	if "`m'" =="m_linear" {
		gen mort = `Y'/100000*L0.maxs*beta_L0_maxs 
		forvalues i = 1/240 {
		qui {
			replace mort = mort + `Y'/100000*L`i'.maxs*beta_L`i'_maxs
		}
		}
	}
	if "`m'" =="m_cubic_adapt_pool24" {
		gen mort = `Y'/100000*L0.maxs*beta_L0_maxs + `Y'/100000*L0.maxs^2*beta_L0_maxs_2 + `Y'/100000*L0.maxs^3*beta_L0_maxs_3

		forvalues i = 1/240 {
		qui {
			replace mort = mort + `Y'/100000*L`i'.maxs*beta_L`i'_maxs + `Y'/100000*L`i'.maxs_2*beta_L`i'_maxs_2 + `Y'/100000*L`i'.maxs_3*beta_L`i'_maxs_3
		}
		}
	}	
}
} // end of "`fixedpop'" == "4"

replace mort = 0 if mort<0 // Trucate the negatives
replace mort = . if modate < 601 // since have incomplete history of storms for dates before 1950

preserve 
drop beta_L*
save output/mortality_predict_`m'_`Y'_`q'.dta, replace //THIS MAKES THE month-state-storm numbers in the paper
restore

/*/------------Collapse by storm
preserve
di as err "collapsing mortality by serial id `q'"

collapse (sum) mort, by(storm_serial_id  modate)

save output/mortality_predict_storm_`q'.dta, replace

restore
*/
//------------Collapse by state
preserve
di as err "collapsing mortality by state"

//truncate at 0
replace mort = 0 if mort<0
collapse (sum) mort, by(adm_name)

save output/mortality_predict_state_`q'.dta, replace
restore


//------------Collapse by date
preserve
di as err "collapsing mortality by date"

collapse (sum) mort, by(modate)

save output/mortality_predict_date_`q'.dta, replace
restore

} // end loop over q (subset of the state/storms)
 

if  "`fixedpop'" == "1" {

//------------Append the date data together
	use output/mortality_predict_date_1_1950pop.dta, clear 

	forvalue q = 2/4 {
		append using output/mortality_predict_date_`q'_1950pop.dta
	}
	collapse (sum) mort, by(modate)
	save  "output/mort_TC_date_total_`m'_`Y'_1950pop.dta",  replace
}
if  "`fixedpop'" == "2" {

//------------Append the date data together
	use output/mortality_predict_date_1_2015pop.dta, clear 

	forvalue q = 2/4 {
		append using output/mortality_predict_date_`q'_2015pop.dta
	}
	collapse (sum) mort, by(modate)
	save  "output/mort_TC_date_total_`m'_`Y'_2015pop.dta",  replace
}
if  "`fixedpop'" == "3" {

//------------Append the date data together
	use output/mortality_predict_date_1_deflated2015pop.dta, clear 

	forvalue q = 2/4 {
		append using output/mortality_predict_date_`q'_deflated2015pop.dta
	}
	collapse (sum) mort, by(modate)
	save  "output/mort_TC_date_total_`m'_`Y'_deflated2015pop.dta",  replace
}
else {
//------------Append the state data together
use output/mortality_predict_state_1.dta, clear 

forvalue q = 2/4 {
	append using output/mortality_predict_state_`q'.dta
	rm output/mortality_predict_state_`q'.dta
}
save  "output/mort_TC_state_total_`m'_`Y'.dta",  replace
rm output/mortality_predict_state_1.dta


//------------Append the date data together
use output/mortality_predict_date_1.dta, clear 

forvalue q = 2/4 {
	append using output/mortality_predict_date_`q'.dta
	rm output/mortality_predict_date_`q'.dta
}
collapse (sum) mort, by(modate)
save  "output/mort_TC_date_total_`m'_`Y'.dta",  replace
rm output/mortality_predict_date_1.dta
}


end
