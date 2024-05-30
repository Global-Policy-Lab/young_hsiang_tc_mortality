//---------------------------------------------------------------------------------
//03-mortality-predictions
//---------------------------------------------------------------------------------


//------------- Create excess deaths data for Figure 4b - g
loc outcome_list "_0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop"
loc model_list "m_linear m_cubic_adapt_pool24"
foreach n of loc model_list {
foreach z of loc outcome_list {
	attribute `z' `n' 4
}
}

//------------- Create excess deaths data for Figure 4a (also need it for Table SI1)
local model_list "m_linear m_nonlinear m_linear_leads m_linear_adapt_4 m_linear_adapt_pool24 m_nonlinear_adapt_4 m_nonlinear_adapt_pool24 m_cubic_adapt_pool24 m_cubic"
foreach n of loc model_list {
	attribute tdths_ttpop `n' 4
}

//------------- Create excess deaths data for Figure 4k
forvalues j = 1/4 {
	di `j'
	attribute tdths_ttpop m_cubic_adapt_pool24 `j'
}


//-------------Creates the month-state-storm
use output/mortality_predict_m_cubic_adapt_pool24_tdths_ttpop_1.dta, clear 
forvalue q = 2/4 {
	append using output/mortality_predict_m_cubic_adapt_pool24_tdths_ttpop_`q'.dta
}

save output/mortality_predict_m_cubic_adapt_pool24_tdths_ttpop.dta, replace




//------------- collapse and combine the prediction data based on date and state
loc outcome_list "tdths_ttpop _0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop"
foreach Y of loc outcome_list {

if "`Y'"=="tdths_ttpop" {
	local model_list "m_linear m_nonlinear m_linear_leads m_linear_adapt_4 m_linear_adapt_pool24 m_nonlinear_adapt_4 m_nonlinear_adapt_pool24 m_cubic_adapt_pool24 m_cubic"
}
else {
	local model_list "m_linear m_cubic_adapt_pool24"
}

foreach m of loc model_list { 

//------------Merge the date data for each model together

foreach m of loc model_list {
	use "output/mort_TC_date_total_`m'_`Y'.dta", clear
	collapse (sum) mort (sd) mort_sd = mort (count) mort_n=mort, by(modate)
	save  "output/mort_TC_date_total_`m'_`Y'.dta",  replace
}

use "output/mort_TC_date_total_m_linear_`Y'.dta" , clear
*rename mort mort_m_linear


foreach m of loc model_list {
	
	merge 1:1 modate using "output/mort_TC_date_total_`m'_`Y'.dta", nogen
	
	rename mort mort_`m'
}
save "output/mort_TC_date_allmodels_`Y'.dta", replace


use output/full_data_for_regression.dta, clear
collapse (sum) dths_tot, by(year month)
drop if year<1930
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"

merge 1:1 modate using "output/mort_TC_date_allmodels_`Y'.dta", keep(3)

drop if year<1950

foreach m of loc model_list {
	replace mort_`m'=. if mort_`m'==0
	
	gen p_total_`m' = mort_`m'/dths_tot
	
}

local mortmodel ""
foreach n in `model_list' {
	local mortmodel "`mortmodel' mort_`n' "
	
}

preserve 
collapse (sum) dths_tot `mortmodel'

foreach m of loc model_list {
	
	gen p_total_`m' = mort_`m'/dths_tot
	
}

sum p_total_*
sum mort_*
restore


preserve
collapse (mean) dths_tot `mortmodel'

sum mort_*
restore

preserve
keep if year>=2000
collapse (mean) dths_tot `mortmodel'

sum mort_*
restore

save output/mort_TC_date_allmodels_`Y'.dta, replace

//------------Merge the state data for each model together

*use "output/mort_TC_state_total_m_linear_`Y'.dta" , clear
*rename mort mort_m_linear
use output/full_data_for_regression.dta, clear
collapse (sum) dths_tot, by(adm_name)

foreach m of loc model_list {

	merge 1:1 adm_name using "output/mort_TC_state_total_`m'_`Y'.dta", nogen
	
	rename mort mort_`m'
	
}
save  "output/mort_TC_state_total_allmodels_`Y'.dta",  replace

preserve

collapse (sum) dths_tot `mortmodel'
sum mort_*

restore

save "output/mort_TC_state_allmodels_`Y'.dta", replace

}
}




