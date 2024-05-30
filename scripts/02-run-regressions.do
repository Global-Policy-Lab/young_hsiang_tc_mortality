//---------------------------------------------------------------------------------
//02-run-regressions.do
//---------------------------------------------------------------------------------



//-------------------------------------------------------------Run regressions for all outcomes and all versions of the model

use output/full_data_for_regression.dta, clear

egen maxs_mean_st = mean(maxs), by(stfips)

xtile pct_4 = maxs_mean_st, n(4)
xtile pct_2 = maxs_mean_st, n(4)
replace pct_2 = 2 if pct_2>=2 


loc outcome_list "tdths_ttpop dths_0144_pop dths_4564_pop dths_6599_pop dths_0000_apop dths_0144_apop dths_4564_apop dths_6599_apop dths_black_bpop dths_white_wpop dths_0000_pop dths_0144_pop dths_4564_pop dths_6599_pop dths_black_pop dths_white_pop"
foreach Y of loc outcome_list {
	
// Linear
	areg `Y' _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
	est save "output/beta/m_linear_`Y'", replace
	
// Linear with weights
	areg `Y' _iy_* _imt* _mi_* _iT_* _LL*_maxs [aweight = totalpop], absorb(modate) 
	est save "output/beta/m_linear_weighted_`Y'", replace
	
// Quadratic
	areg `Y'  _iy_* _imt* _mi_* _iT_* _LL*_maxs _LL*_maxs_2, absorb(modate) 
	est save "output/beta/m_nonlinear_`Y'", replace

// Leads
	areg `Y'  _iy_* _imt* _mi_* _iT_* _FF*_maxs _LL*_maxs, absorb(modate) 
	est save "output/beta/m_linear_leads_`Y'", replace
	
// cubic
	areg `Y' _iy_* _imt* _mi_* _iT_* _LL*_maxs _LL*_maxs_2 _LL*_maxs_3, absorb(modate) 
	est save "output/beta/m_cubic_`Y'", replace

// Adpatation (4) linear
	areg `Y'  _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_4, absorb(modate) 
	est save "output/beta/m_linear_adapt_4_`Y'", replace

// Adpatation (2) linear
	areg `Y'  _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_2, absorb(modate) 
	est save "output/beta/m_linear_adapt_pool24_`Y'", replace

// adpatation (4) nonlinear
	areg `Y'  _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_4 c.(_LL*_maxs_2)##i.pct_4, absorb(modate) 
	est save "output/beta/m_nonlinear_adapt_4_`Y'", replace

// adpatation (2) nonlinear
	areg `Y' _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_2 c.(_LL*_maxs_2)##i.pct_2, absorb(modate) 
	est save "output/beta/m_nonlinear_adapt_pool24_`Y'", replace

// adpatation (2) cubic
	areg `Y' _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_2 c.(_LL*_maxs_2)##i.pct_2 c.(_LL*_maxs_3)##i.pct_2, absorb(modate) 
	est save "output/beta/m_cubic_adapt_pool24_`Y'", replace

local model_list "m_linear m_nonlinear m_cubic m_linear_leads m_linear_adapt_4 m_linear_adapt_pool24 m_nonlinear_adapt_4 m_nonlinear_adapt_pool24 m_cubic_adapt_pool24"
*local model_list "m_linear m_cubic_adapt_pool24"
foreach m of loc model_list {
	
	//---- make beta for attribution
	preserve
	
	di "`m'_`Y'.ster"

	est use "output/beta/`m'_`Y'.ster"
	
	matrix list e(b)
	
	if "`m'"=="m_linear_adapt_4" | "`m'" == "m_nonlinear_adapt_4" {
		local z = 4
		di "model `m', quartile `z'"
	}	
	if "`m'"=="m_linear_adapt_pool24" | "`m'" == "m_nonlinear_adapt_pool24" | "`m'"=="m_cubic_adapt_pool24"{
		local z = 2
	}
	if "`m'"=="m_linear" | "`m'" == "m_nonlinear" | "`m'" == "m_linear_leads" | "`m'"=="m_cubic" {
		local z = 1
		gen pct_1 = 1
		
		di "model `z'"
	}
	
	keep adm_name modate pct_* year month

	forvalues q=1/`z' {

		*** get coefficients from models
		*quietly {
		if `q'==1 {
			di "`q'"
			forvalues t=0/240 {
				
			gen beta_L`t'_maxs = .
			replace beta_L`t'_maxs=_b[_LL`t'_maxs] if pct_`z'==`q'
			
			di "pct_`z'=`q'"
			
			if "`m'" == "m_nonlinear" | "`m'" == "m_nonlinear_adapt_4" | "`m'" == "m_nonlinear_adapt_pool24" {
			gen beta_L`t'_maxs_2 = .
			replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] if pct_`z'==`q'
			}
			
			if "`m'" == "m_cubic" | "`m'" == "m_cubic_adapt_pool24" {
			gen beta_L`t'_maxs_2 = .
			replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] if pct_`z'==`q'
			gen beta_L`t'_maxs_3 = .
			replace beta_L`t'_maxs_3=_b[_LL`t'_maxs_3] if pct_`z'==`q'
			}
			} /* end loop over t */
		} /* end q=1 */
		if `q'>1 {
			di "`q'>1"
			di "pct_`z'=`q'"

			forvalues t=0/240 {
			if "`m'"=="m_linear" | "`m'" == "m_nonlinear" | "`m'" == "m_linear_leads" {
			replace beta_L`t'_maxs=_b[_LL`t'_maxs] + _b[`q'.pct_`z'#_LL`t'_maxs] if pct_`z'==`q' 
			}
			
			if "`m'"=="m_linear_adapt_4" | "`m'"=="m_linear_adapt_pool24" {
			replace beta_L`t'_maxs=_b[_LL`t'_maxs] + _b[`q'.pct_`z'#c._LL`t'_maxs] if pct_`z'==`q' 				
			}
			
			if "`m'" == "m_nonlinear_adapt_4" {
			replace beta_L`t'_maxs=_b[_LL`t'_maxs] + _b[`q'.pct_`z'#c._LL`t'_maxs] if pct_`z'==`q' 				
			replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] + _b[`q'.pct_`z'#c._LL`t'_maxs_2] if pct_`z'==`q'
			}
			
			if "`m'" == "m_nonlinear" | "`m'" == "m_nonlinear_adapt_pool24" {
			replace beta_L`t'_maxs=_b[_LL`t'_maxs] + _b[`q'.pct_`z'#_LL`t'_maxs] if pct_`z'==`q' 
			replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] + _b[`q'.pct_`z'#_LL`t'_maxs_2] if pct_`z'==`q'
			}
			
			if "`m'" == "m_cubic" | "`m'" == "m_cubic_adapt_pool24" {
			replace beta_L`t'_maxs=_b[_LL`t'_maxs] + _b[`q'.pct_`z'#_LL`t'_maxs] if pct_`z'==`q' 
			replace beta_L`t'_maxs_2=_b[_LL`t'_maxs_2] + _b[`q'.pct_`z'#_LL`t'_maxs_2] if pct_`z'==`q'
			replace beta_L`t'_maxs_3=_b[_LL`t'_maxs_3] + _b[`q'.pct_`z'#_LL`t'_maxs_3] if pct_`z'==`q'
			}
			} /* end loop over t */
		} /* end else */ 
		*}
	}
	
	drop modate
	gen modate = (year-1900)*12 + month
	lab var modate "months since jan 1900 = 1"
	drop if year < 1930

	save "output/beta/`m'_`Y'.dta", replace

	restore

}
}


