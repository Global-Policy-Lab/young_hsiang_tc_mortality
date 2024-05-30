//------------------------------------------------------------------------------
//--------------------------------------------------------- Figure 2
//--------------------------------------------------------- scripts/04-main-figure2.do
//------------------------------------------------------------------------------


disp("-------------------------STARTING FIGURE 2-----------------------")


//-----------------------------------------TOTAL RANDOMIZATION

use "output/randomization/total_randomization_output/total_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .

collapse (mean) sim_coeff_L*_maxs
gen i = 1
reshape long sim_coeff_L@D_maxs , i(i) j(lag)
rename sim_coeff_LD_maxs sim_coeff_LD_maxs_total_rand

tempfile total
save `total'

//------------------------------------------ WITHIN  PANEL
use "output/randomization/whithin_panel_randomization_output/whithin_panel_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .

collapse (mean) sim_coeff_L*_maxs
gen i = 1
reshape long sim_coeff_L@D_maxs , i(i) j(lag)
rename sim_coeff_LD_maxs sim_coeff_LD_maxs_within_panel

tempfile within_panel
save `within_panel'

//------------------------------------------ WITHIN MONTH
use "output/randomization/whithin_monthyear_randomization_output/whithin_monthyear_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .


collapse (mean) sim_coeff_L*_maxs
gen i = 1
reshape long sim_coeff_L@D_maxs , i(i) j(lag)
rename sim_coeff_LD_maxs sim_coeff_LD_maxs_within_month

tempfile within_month
save `within_month'

//------------------------------------------ CROSS PANEL

use "output/randomization/cross_panel_randomization_output/cross_panel_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .

collapse (mean) sim_coeff_L*_maxs
gen i = 1
reshape long sim_coeff_L@D_maxs , i(i) j(lag)
rename sim_coeff_LD_maxs sim_coeff_LD_maxs_cross_panel

merge 1:1 lag using `total' , nogen
merge 1:1 lag using `within_panel' , nogen
merge 1:1 lag using `within_month' , nogen

save data/randomization_mean.dta, replace



//-------------------------------------------------- Main Result with leads
use output/full_data_for_regression.dta, clear

*log using ./output/main_analysis
// lag is a var just for indexing coeffs for plotting
gen lag = _n - _N + 240
replace lag = . if lag < -72

areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _FF*_maxs _LL*_maxs, absorb(modate) 

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
		if `i' == 172 {
			di r(p)
			di as error "CUMULATIVE 172 LAGS, LINEAR MODEL, ALL CAUSE:"
			di "ESTIMATE:"
			di "`r(estimate)'"
			di "SE:"
			di "`r(se)' "
			di "t-stat:"
			di "`r(t)'"
			di "DF:"
			di "`r(df)'"
		}
		if `i' == 180 {
			di r(p)
		}	
	}
	loc varlistsum "0"
	forvalues i = 1/72 {
		quietly{
		lincom _b[_FF`i'_maxs],  level(95)
		replace b =  r(estimate) if lag == -`i'
		replace ci1 = r(lb) if lag==-`i'
		replace ci2 = r(ub) if lag==-`i'
		loc varlistsum "`varlistsum' + (_FF`i'_maxs)"
		lincom -(`varlistsum'),  level(95)
		replace B_leads = r(estimate) if lag == -`i'
		replace CI1_leads = r(lb) if lag == -`i'
		replace CI2_leads = r(ub) if lag == -`i'
		}

	}

gen lag2 = lag^2	
reg b lag lag2 if lag>0 & lag<172

merge m:1 lag using data/randomization_mean.dta, nogen


tw (rarea CI1_leads CI2_leads lag, lwidth(none) fcolor(emerald%25)) ///
	(line B_leads lag, lcolor(emerald)) ///
	(line sim_coeff_LD_maxs_cross_panel lag, lcolor(dkorange) lpattern(dash)) ///
	(line sim_coeff_LD_maxs_total_rand lag, lcolor(dkorange) lpattern(dot)) ///
	(line sim_coeff_LD_maxs_within_panel lag, lcolor(dkorange) lpattern(shortdash_dot)) ///
	(line sim_coeff_LD_maxs_within_month lag, lcolor(dkorange) lpattern(longdash_dot)) ///
	,  xline(0, lcolor(black)) ytitle("Cumulative All Cause Mortality (per 100,000 per m/s)" " " , size(small)) xline(172 , lwidth(thin) lcolor(gs8)) text(10.5 172 "172 month", size(small)) xtitle("Months Since Tropical Cyclone")  xlabel(-72(24)240) ///
	legend(ring(0) position(11) col(1) size(small) symxsize(*.5) order( 2 "Summed All Cause Effect" 1 "95% CI" 4 "Total Randomization" 3 "Across State"  5 "Within State" 6 "Within Month" ) bmargin(tiny) rowgap(0) colgap(0) forces) saving(figures/est_by_all_withleads.gph, replace)

tw (rspike ci1 ci2 lag, lcolor(emerald%25)) (line b lag , lcolor(emerald) yline(0)) ///
 (qfit b lag if lag>=0 & lag<=172, lcolor(red*2) lpattern(longdash)) , ///
 ytitle("Monthly All Cause Mortality Rate (per 100,000 per m/s)" , size(small)) /// 
 xlabel(-72(24)240) /*fysize(.75)*/ xtitle("Months Since Tropical Cyclone") legend(off) xline(0 , lcolor(gs6)) ///
 xline(172 , lwidth(thin) lcolor(gs8)) text(0.12 172 "172 month", size(small)) ///
 saving(figures/est_by_all_withleads_b.gph, replace)
 
graph combine figures/est_by_all_withleads_b.gph figures/est_by_all_withleads.gph , col(1) fxsize(65)
graph save figures/est_all_leads_main.gph, replace

//for appendix
tw (rarea CI1_leads CI2_leads lag, lwidth(none) fcolor(gs14) yline(0)) (line B_leads lag, lcolor(black)), ///
 xtitle("")  xlabel(-72(24)240) legend(off) saving(figures/appendix/est_by_all_withleads.gph, replace)


//------------------------GRAPHING CUMULATIVE EFFECTS (ALL OUTCOMES)

log using log_tstats, replace
loc outcome_list "tdths_ttpop dths_cvd_pop dths_neo_pop dths_rpd_pop dths_ifd_pop dths_mva_pop dths_other_pop dths_0000_pop dths_0144_pop dths_4564_pop dths_6599_pop dths_0000_apop dths_0144_apop dths_4564_apop dths_6599_apop dths_black_pop dths_white_pop dths_black_bpop dths_white_wpop"
foreach Y of loc outcome_list {
	di "`Y'"
	
	capture: gen B_`Y' = .
	lab var B_`Y' "`Y'"
	capture: gen CI1_`Y' = .
	capture: gen CI2_`Y' = .
	
	order lag B_`Y' CI1_`Y' CI2_`Y'
	
	quietly{ 
		areg `Y' _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
	}
	
	loc varlistsum "0"
	forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		lincom `varlistsum'
		replace B_`Y' = r(estimate) if lag == `i'
		replace CI1_`Y' = r(estimate) - 1.96 * r(se) if lag == `i'
		replace CI2_`Y' = r(estimate) + 1.96 * r(se) if lag == `i'
		}
		if `i' == 172 {
			di as error "CUMULATIVE 172 LAGS, LINEAR MODEL, `Y':"
			di "ESTIMATE:"
			di "`r(estimate)'"
			di "SE:"
			di "`r(se)'"
			di "t-stat:"
			di "`r(t)'"
			di "Degree Freedom:"
			di "`r(df)'"
		}
	}
}
log close

save output/full_data_for_regression_with_betas.dta, replace

drop if lag<0
//Age
tw (rarea CI1_dths_0000_apop CI2_dths_0000_apop lag, fcolor(ebblue*2%20) lwidth(none)) ///
	(rarea CI1_dths_0144_apop CI2_dths_0144_apop lag, fcolor(red*2%20)  lwidth(none)) ///
	(rarea CI1_dths_4564_apop CI2_dths_4564_apop lag, fcolor(red*.5%20) lwidth(none)) ///
	(rarea CI1_dths_6599_apop CI2_dths_6599_apop lag, fcolor(ebblue*.5%20) lwidth(none))  ///
	(line B_dths_0000_apop B_dths_0144_apop B_dths_4564_apop B_dths_6599_apop lag ,lcolor(ebblue*2 red*2 red*.5 ebblue*.5) /*lpattern(solid dash dot shortdash)*/ ) ///
	, yline(0) xlabel(0(24)240) ///
	legend(ring(0) position(11) col(1) size(small) symxsize(*.5) order(5 "<1 yrs" 6 "1-44 yrs" 7 "45-64 yrs" 8 "65-99 yrs") forces bmargin(tiny) rowgap(0) colgap(0))  xtitle("Months Since Tropical Cyclone")  /// 
	ytitle("Age Specific Cumulative Mortality (per 100,000 per m/s)" , size(small)) ///
	xline(172 , lwidth(thin) lcolor(gs14)) text(84 172 "172 month", size(small)) ///
	saving(figures/est_by_age.gph, replace)
	*graph export figures/est_by_age.png, replace

//Race
tw (rarea CI1_dths_black_bpop CI2_dths_black_bpop lag, fcolor(ebblue*2%20) lwidth(none)) ///
	(rarea CI1_dths_white_wpop CI2_dths_white_wpop lag, fcolor(red*2%20) lwidth(none)) ///
	(line B_dths_black_bpop B_dths_white_wpop lag, /*lpattern(solid dash)*/ lcolor(ebblue*2 red*2 )) , xlabel(0(24)240) ///
	legend(ring(0) position(11) col(1) size(small) symxsize(*.5) order(3 "Black" 4 "White") bmargin(tiny) rowgap(0) colgap(0) forces) yline(0)  xtitle("Months Since Tropical Cyclone")  ///
	ytitle("Race Specific Cumulative Mortality (per 100,000 per m/s)" , size(small)) ///
	xline(172 , lwidth(thin) lcolor(gs14)) text(31 172 "172 month", size(small)) ///
	saving(figures/est_by_race.gph, replace) 
	*graph export figures/est_by_race.png, replace

//Cause
tw (rarea CI1_dths_other_pop CI2_dths_other_pop  lag, fcolor(red*2%20) lwidth(none)) ///
(rarea CI1_dths_cvd_pop CI2_dths_cvd_pop lag, fcolor(red*.5%20)  lwidth(none)) ///
(rarea CI1_dths_neo_pop CI2_dths_neo_pop  lag, fcolor(ebblue*2%25) lwidth(none)) ///
(rarea CI1_dths_mva_pop CI2_dths_mva_pop  lag, fcolor(ebblue*.5%25) lwidth(none))  ///
(rarea CI1_dths_ifd_pop CI2_dths_ifd_pop lag, fcolor(green*2%20) lwidth(none))  ///
(rarea CI1_dths_rpd_pop CI2_dths_rpd_pop lag, fcolor(green*.5%20) lwidth(none))  ///
(line B_dths_other_pop B_dths_cvd_pop B_dths_neo_pop B_dths_mva_pop B_dths_ifd_pop B_dths_rpd_pop lag,  ///
/* lpattern(dash dot shortdash longdash_dot dash_dot solid)  */ lcolor(red*2 red*.5 ebblue*2 ebblue*.5 green*2 green*.5)) ///
	, xlabel(0(24)240) yline(0) ylabel(-1(1)4.5)  ///
	legend(ring(0) position(11) col(1) size(small) order(7 "Other" 8 "Cardiovascular" 9 "Neoplasm" 10 "Motor Vehicle" 11 "Infectious" 12 "Respiratory") symxsize(*.5) bmargin(tiny) rowgap(0) colgap(0) forces)  ///
	ytitle("Cause Specific Cumulative Mortality (per 100,000 per m/s)" , size(small)) ///
	xline(172 , lwidth(thin) lcolor(gs14)) text(4.65 172 "172 month", size(small)) ///
	xtitle("Months Since Tropical Cyclone")  ///
	saving(figures/est_by_disease.gph, replace)
	
	
//in the appendix
tw (rarea CI1_tdths_ttpop CI2_tdths_ttpop lag, fcolor(gs14) lwidth(none))(line B_tdths_ttpop lag, lcolor(black)), ///
	legend(off) xlabel(0(24)240) yline(0)  xtitle("")  ///
	saving(figures/appendix/est_by_all.gph, replace)


************ Create stacked plots for age and race (in Figure SI14)
drop if lag<0
gen stack_B_dths_0000_pop = B_dths_0000_pop
gen stack_B_dths_0144_pop = stack_B_dths_0000_pop + B_dths_0144_pop
gen stack_B_dths_4564_pop = stack_B_dths_0144_pop + B_dths_4564_pop
gen stack_B_dths_6599_pop = stack_B_dths_4564_pop + B_dths_6599_pop

gen stack_B_dths_black_pop = B_dths_black_pop
gen stack_B_dths_white_pop = stack_B_dths_black_pop + B_dths_white_pop

tw area stack_B_dths_6599_pop stack_B_dths_4564_pop stack_B_dths_0144_pop stack_B_dths_0000_pop    lag, ///
color(midblue%75 red cranberry blue) /*lpattern(solid dash dot shortdash)*/ ///
legend(ring(0) position(11) col(1) size(small) order(4 "<1 yrs"  3 "1-44 yrs"  2  "45-64 yrs" 1 "65-99 yrs"  )) ///
xlabel(0(24)245) xtitle("Months Since Tropical Cyclone") yline(0) ///
ytitle("Stacked Age Specific Cumulative Mortality" "(per 100,000 per m/s)") ///
text(0.4 248 "14%", size(small)) ///
text(1.75 248 "32%", size(small)) ///
text(2.8 248 "8%", size(small)) ///
text(4.1 248 "46%", size(small)) ///
saving(figures/appendix/est_by_age_totalpop.gph, replace)

tw area stack_B_dths_white_pop stack_B_dths_black_pop lag, color(cranberry%50 midblue ) yline(0) xtitle("Months Since Tropical Cyclone") xlabel(0(24)245) legend(ring(0) position(11) col(1) order(1 "White" 2 "Black" )) ///
ytitle("Stacked Race Specific Cumulative Mortality" "(per 100,000 per m/s)" ) ///
text(1 248 "34%", size(small)) ///
text(3.8 248 "66%", size(small)) ///
 saving(figures/appendix/est_by_race_totalpop.gph, replace)
 
 
gen stack_B_cause =  B_dths_other_pop+ B_dths_mva_pop+ B_dths_ifd_pop+ B_dths_rpd_pop+ B_dths_neo_pop+ B_dths_cvd_pop
gen other_percent = B_dths_other_pop / stack_B_cause
gen mva_percent = B_dths_mva_pop / stack_B_cause
gen ifd_percent = B_dths_ifd_pop / stack_B_cause
gen rpd_percent = B_dths_rpd_pop / stack_B_cause
gen neo_percent = B_dths_neo_pop / stack_B_cause
gen cvd_percent = B_dths_cvd_pop / stack_B_cause

 //----------------------------------------For Figure SI14: stacked race and age graphs

graph combine figures/appendix/est_by_age_totalpop.gph  figures/appendix/est_by_race_totalpop.gph , ysize(5) xsize(8)
*graph export figures/appendix/est_age_race_stacked.png, replace
graph export figures/appendix/figureSI14_est_age_race_stacked.pdf, replace

rm figures/appendix/est_by_age_totalpop.gph  
rm figures/appendix/est_by_race_totalpop.gph

//----------------------------------------------Adaptation 

use output/full_data_for_regression.dta, clear


egen maxs_mean_st = mean(maxs), by(stfips)

xtile pct_4 = maxs_mean_st, n(4)
xtile pct_2 = maxs_mean_st, n(4)
replace pct_2 = 2 if pct_2>=2 


**** Adaptation with 4 bins
areg tdths_ttpop _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_4, absorb(modate)
est save "output/beta/m_adapt_linear_groups4", replace

**** Adaptation with 2 bins (pool bin 2 - 4 from previous model)
*replace pct = 2 if pct>=2
areg tdths_ttpop _iy_* _imt* _mi_* _iT_* c.(_LL*_maxs)##i.pct_2, absorb(modate)
est save "output/beta/m_adapt_linear_groups2", replace

preserve
clear
loc x = _N+240+1
set obs `x'
gen xaxis=_n - 1

foreach i in 4 2 {

**** Create dataset of the coefficients *****
	//------------- 4 bin model
	if `i'==4 {
		est use "output/beta/m_adapt_linear_groups4.ster"
		
		forvalues q = 1/4 {
		*** get summed coefficients from event-study models
		gen B_`q' = .
		
		if `q'==1 {
		gen CI1_s_1=.
		gen CI2_s_1=.
		
		loc varlistsum "0"
		forvalues i = 0/240 {
		quietly{
		loc varlistsum "`varlistsum' + _b[_LL`i'_maxs]"
		lincom `varlistsum'
		replace B_`q' = r(estimate) if xaxis == `i'
		replace CI1_s_`q' = r(estimate) - 1.96 * r(se) if xaxis == `i'
		replace CI2_s_`q' = r(estimate) + 1.96 * r(se) if xaxis == `i'
		}
		if `i' == 172 {
			di as error "CUMULATIVE 172 LAGS, LINEAR MODEL, `Y':"
			di "ESTIMATE:"
			di "`r(estimate)'"
			di "SE:"
			di "`r(se)'"
			di "t-stat:"
			di "`r(t)'"
			di "Degree Freedom:"
			di "`r(df)'"
		}
		} //end loop over 240 lags	
		} //end if q=1
		else {
		loc varlistsum "0"
		forvalues i = 0/238 { //ran into an error saying the sum was too long 
		quietly{
		loc varlistsum "`varlistsum' + _b[_LL`i'_maxs] + _b[`q'.pct#c._LL`i'_maxs]"
		lincom `varlistsum'
		replace B_`q' = r(estimate) if xaxis == `i'
		*replace CI1_s_`q' = r(estimate) - 1.96 * r(se) if xaxis == `i'
		*replace CI2_s_`q' = r(estimate) + 1.96 * r(se) if xaxis == `i'
		}
		if `i' == 172 {
			di as error "CUMULATIVE 172 LAGS, LINEAR MODEL, `Y':"
			di "ESTIMATE:"
			di "`r(estimate)'"
			di "SE:"
			di "`r(se)'"
			di "t-stat:"
			di "`r(t)'"
			di "Degree Freedom:"
			di "`r(df)'"
		}
		} //end loop over 240 lags
		} //end else
		} //end loop over quartile
	}
		
	//-------------- 2 bin model
	else {
			
		est use "output/beta/m_adapt_linear_groups2.ster"
		
		forvalues q = 1/2 {
		gen B_pooled_`q' = .
		gen CI1_pooled_`q'=.
		gen CI2_pooled_`q'=.
		
		if `q'==1 {
		loc varlistsum "0"
		forvalues i = 0/238 {
		quietly{
		loc varlistsum "`varlistsum' + _b[_LL`i'_maxs]"
		lincom `varlistsum'
		replace B_pooled_`q' = r(estimate) if xaxis == `i'
		replace CI1_pooled_`q' = r(estimate) - 1.96 * r(se) if xaxis == `i'
		replace CI2_pooled_`q' = r(estimate) + 1.96 * r(se) if xaxis == `i'
		}
		if `i' == 172 {
			di as error "CUMULATIVE 172 LAGS, LINEAR MODEL, `Y':"
			di "ESTIMATE:"
			di "`r(estimate)'"
			di "SE:"
			di "`r(se)'"
			di "t-stat:"
			di "`r(t)'"
			di "Degree Freedom:"
			di "`r(df)'"
		}
		} //end loop over 240 lags	
		} //end if 1
		else {
		loc varlistsum "0"
		forvalues i = 0/238 { //ran into an error saying the sum was too long 
		quietly{
		loc varlistsum "`varlistsum' + _b[_LL`i'_maxs] + _b[`q'.pct#c._LL`i'_maxs]"
		lincom `varlistsum'
		replace B_pooled_`q' = r(estimate) if xaxis == `i'
		replace CI1_pooled_`q' = r(estimate) - 1.96 * r(se) if xaxis == `i'
		replace CI2_pooled_`q' = r(estimate) + 1.96 * r(se) if xaxis == `i'
		}
		if `i' == 172 {
			di as error "CUMULATIVE 172 LAGS, LINEAR MODEL, `Y':"
			di "ESTIMATE:"
			di "`r(estimate)'"
			di "SE:"
			di "`r(se)'"
			di "t-stat:"
			di "`r(t)'"
			di "Degree Freedom:"
			di "`r(df)'"
		}
		} //end loop over 240 lags
		} //end else
		}
	}
}
save output/est_adaptation.dta, replace

drop if xaxis<0
tw (rarea CI1_pooled_1 CI2_pooled_1 xaxis, fcolor(red*2%20) lwidth(none)) (line B_pooled_1 xaxis, lcolor(red*2)) ///
   (rarea CI1_pooled_2 CI2_pooled_2 xaxis, fcolor(ebblue*2%20) lwidth(none)) (line B_pooled_2 xaxis, lcolor(ebblue*2)) ///
	, yline(0)  xtitle("Months Since Tropical Cyclone")  ///
	legend(ring(0) position(11) col(1) size(small) bmargin(tiny) rowgap(0) colgap(0)) xlabel(0(24)240)  ///
   legend(order(2 "Low Average Exposure" 4 "High Average Exposure") symxsize(*.5))  ///
   xline(172 , lwidth(thin) lcolor(gs14)) text(22 172 "172 month", size(small)) ///
   ytitle("Cumulative All Cause Mortality (per 100,000 per m/s)" , size(small))
graph save figures/est_adpatation.gph, replace
*graph export figures/est_adaptation.png, replace	

//in the appendix
tw (rarea CI1_pooled_2 CI2_pooled_2 xaxis, fcolor(gs14%75) lwidth(none)) (line B_pooled_2 xaxis, lcolor(black)) ///
	(line B_1 B_2 B_3 B_4 xaxis,  lcolor(red eltblue*.5 eltblue ebblue))  ///
	,  yline(0)  xtitle("Months Since Tropical Cyclone") ///
	ytitle("Cumulative All Cause Mortality (per 100,000 per m/s)" , size(small)) legend(ring(0) position(11) col(2) size(small) ) xlabel(0(24)240)  ///
   legend(order(1 "Pooled CI" 2 "Pooled 2-4" 3 "1 quartile" 4 "2 quartile" 5 "3 quartile" 6 "4 quartile" ) ) 
graph save figures/appendix/APPENDIX_est_adpatation.gph, replace
*graph export figures/appendix/APPENDIX_est_adaptation.png, replace	
	
restore	

graph combine  figures/est_by_age.gph figures/est_by_race.gph figures/est_by_disease.gph figures/est_adpatation.gph , col(2)
graph save figures/est_combined.gph, replace



graph combine figures/est_all_leads_main.gph figures/est_combined.gph , ysize(5.25) xsize(10)
graph export figures/figure2.pdf, replace	/* THIS IS THE MAIN FIGURE */


rm figures/est_combined.gph
rm figures/est_adpatation.gph
rm figures/est_by_age.gph
rm figures/est_by_race.gph
rm figures/est_by_disease.gph
rm figures/est_by_all_withleads.gph
rm figures/est_by_all_withleads_b.gph	
rm figures/est_all_leads_main.gph
	
	
	
	
	
	
	
	
	
