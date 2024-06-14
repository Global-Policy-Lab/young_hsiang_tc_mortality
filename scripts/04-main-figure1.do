//------------------------------------------------------------------------------
//--------------------------------------------------------- Figure 1
//--------------------------------------------------------- scripts/04-main-figure1.do
//------------------------------------------------------------------------------

//----------------------------------------- RANDOMIZATION
//NOTE: need to run randomization code to generate the simulation outputs. 
*do code/randomization.do <- THIS WILL TAKE A LONG TIME TO RUN (MULTIPE DAYS)



use output/full_data_for_regression.dta, clear


disp("------------------------- LINEAR MODEL -----------------------")

qui areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 
est save "output/beta/m_linear.ster", replace



//--------------------------------------------------------------------- Figure 1
disp("-------------------------STARTING FIGURE 1-----------------------")

//-----------------plot prediction against truth 

* FL and NJ figures
qui areg tdths_ttpop _iy_* _imt* _mi_* _iT_* _LL*_maxs, absorb(modate) 

predict y_imt_TC3 if e(sample), xbd

gen modate_lbl = ym(year, month)
format modate_lbl %tm

tw (line tdths_ttpop modate_lbl, color(orange*2%50)) (sc y_imt_TC3 modate_lbl, msize(tiny) color(cranberry%50)) if state_abbrev=="NJ" & year>=1950, xlabel(-120(120)660,format(%tmCY)) legend(order(1 "Measured" 2 "Predicted") size(small)) xtitle(" ")  ytitle("Monthly All Cause Mortality Rate (per 100,000)", size(small))
	graph save figures/state_NJ_pred.gph, replace
	
tw (line tdths_ttpop modate_lbl, color(orange*2%50)) (sc y_imt_TC3 modate_lbl, msize(tiny) color(cranberry%50)) if state_abbrev=="FL" & year>=1950, xlabel(-120(120)660,format(%tmCY)) xtitle(" ") legend(order(1 "Measured" 2 "Predicted") size(small)) ytitle("Monthly All Cause Mortality Rate (per 100,000)", size(small))
	graph save figures/state_FL_pred.gph, replace
	
tw (bar maxs modate_lbl if state_abbrev=="NJ" & year>=1950, color(blue)), xlabel(-120(120)660,format(%tmCY)) xtitle(" ") ytitle("Monthly Maximum Windspeed (m/s)", size(small)) title("New Jersey") 
graph save figures/time_NJ_maxs.gph, replace

tw bar maxs modate_lbl if state_abbrev=="FL" & year>=1950, color(blue) xlabel(-120(120)660,format(%tmCY)) xtitle(" ") ytitle("Monthly Maximum Windspeed (m/s)", size(small)) title("Florida") 
graph save figures/time_FL_maxs.gph, replace 

grc1leg figures/time_FL_maxs.gph figures/time_NJ_maxs.gph figures/state_FL_pred.gph figures/state_NJ_pred.gph  , col(2) legendfrom(figures/state_FL_pred.gph)
graph save figures/state_NJ_FL_pred.gph, replace
graph export figures/state_NJ_FL_pred.png, replace

rm figures/time_FL_maxs.gph 
rm figures/time_NJ_maxs.gph 
rm figures/state_FL_pred.gph 
rm figures/state_NJ_pred.gph




* predictions
preserve
collapse (mean) tdths_ttpop y_imt_TC3 [aw=totalpop], by(year)

rename tdths_ttpop tdths_ttpop_year
rename y_imt_TC3 y_imt_TC3_year

tempfile collapse
save `collapse'
restore

merge m:1 year using `collapse', nogen

bys year: replace y_imt_TC3_year =. if (_n!=1 ) 
bys year: replace tdths_ttpop_year =. if (_n!=1 ) 

local r2: display %5.3f e(r2)
tw (scatter tdths_ttpop y_imt_TC3,  color(cranberry%10)) (lfit tdths_ttpop y_imt_TC3)  ///
, legend(pos(11) ring(0) col(1) lab(1 "State-Month") lab(2 "45 degree line") ) xtitle("Predicted Mortality Rate (per 100,000)") ytitle("True Mortality Rate (per 100,000)") note(model R-squared=`r2')
graph save figures/prediction_true_plot.gph, replace
graph export figures/prediction_true_plot.png, replace




//--------------------------------------------------------graphing randomization results as placebo runs

	**** Create dataset of the coefficients *****
	clear
	loc x = _N+240+1
	set obs `x'
	gen lag=_n-1
	
		est use "output/beta/m_linear.ster"

		*** get coefficients from event-study models
		gen B = .
		gen B_s = .
		
		loc varlistsum "0"
		forvalues i = 0/240 {
		quietly{
		replace B = _b[_LL`i'_maxs] if lag == `i'
		
		lincom `varlistsum'
		loc varlistsum "`varlistsum' + _LL`i'_maxs"
		replace B_s = r(estimate) if lag == `i'
		}
		}
		
save output/m_linear_beta.dta, replace




est use "output/beta/m_linear.ster"

//-----------------------------------------TOTAL RANDOMIZATION

use "output/randomization/total_randomization_output/total_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .
gen simulation = _n

reshape long sim_coeff_L@D_maxs, i(simulation) j(lag)

merge m:1 lag using output/m_linear_beta.dta, nogen


//---------------Make the p-value figure	
gen p_indicator = 0
replace p_indicator = 1 if B_s < abs(sim_coeff_LD_maxs) 

//----------- "Joint" significance 
preserve
keep if lag>=1 & lag<=172
sum p_indicator
local rp: display %10.1e r(mean)
di `rp'
restore

//Make the p-value figure
preserve
collapse (mean) p_indicator, by(lag)
replace p_indicator = 0 if lag==0
gen l_p_value = log(p_indicator)
tw line p_indicator lag , ///
  ytitle("p-value", size(small)) xsize(4) ysize(8) fxsize(25) ///
  ylabel(0(0.05).15) ///
  xtitle("Months Since Tropical Cyclone", size(small)) ///
  yline(0.01, lp(dash) lc(gs12)) ///
  yline(0.05, lp(dash) lc(gs12)) ///
  yline(0.1, lp(dash) lc(gs12)) ///
  text(0.075 82 "Joint Significance" "P<`rp'" "(months 1 - 172)", size(small)) ///
  saving(figures/appendix/total_rand_pvalue.gph, replace)
restore



//Make the randomization figure	

collapse 	(mean) mean_maxs = sim_coeff_LD_maxs ///
			(p1) p1_maxs = sim_coeff_LD_maxs ///
			(p2) p2_maxs = sim_coeff_LD_maxs ///
			(p5) p5_maxs = sim_coeff_LD_maxs ///
			(p10) p10_maxs = sim_coeff_LD_maxs ///
			(p25) p25_maxs = sim_coeff_LD_maxs ///
			(p50) p50_maxs = sim_coeff_LD_maxs ///
			(p75) p75_maxs = sim_coeff_LD_maxs ///
			(p90) p90_maxs = sim_coeff_LD_maxs ///
			(p95) p95_maxs = sim_coeff_LD_maxs ///
			(p98) p98_maxs = sim_coeff_LD_maxs ///
			(p99) p99_maxs = sim_coeff_LD_maxs ///
			(mean) main_linear_model = B_s  ///
			, by(lag)
			
tw 	(rarea p1_maxs p99_maxs lag, lwidth(none) fcolor(gs14)) ///
	(rarea p2_maxs p98_maxs lag, lwidth(none) fcolor(gs12)) ///
	(rarea p5_maxs p95_maxs lag, lwidth(none) fcolor(gs10)) ///
	(rarea p10_maxs p90_maxs lag, lwidth(none) fcolor(gs8)) ///
	(rarea p25_maxs p75_maxs lag, lwidth(none) fcolor(gs6)) ///
	(line main_linear_model lag, lcolor(black)) ///
	(line mean_maxs lag, lcolor(black)), ylabel(-5(5)5) ///
	legend(off) xtitle("Months Since Tropical Cyclone", size(small)) ///
	ytitle("All Cause Cumulative Mortality (per 100,000 per m/s)" ,size(small)) ///
	saving(figures/appendix/total_rand.gph, replace)

graph combine figures/appendix/total_rand.gph figures/appendix/total_rand_pvalue.gph, title("Total Randomization", size(small)) saving(figures/appendix/total_rand.gph, replace)

rm figures/appendix/total_rand_pvalue.gph

keep if lag==5 | lag==10 | lag==15 | lag==20
rename mean_maxs mean_maxs_total
rename p*_maxs p*_maxs_total
tempfile totalrandom
save `totalrandom'


//------------------------------------------ WHITHIN  PANEL

use "output/randomization/whithin_panel_randomization_output/whithin_panel_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .
gen simulation = _n
reshape long sim_coeff_L@D_maxs, i(simulation) j(lag)

merge m:1 lag using output/m_linear_beta.dta

//Make the p-value figure	
gen p_indicator = 0
replace p_indicator = 1 if B_s < abs(sim_coeff_LD_maxs) 

//----------- "Joint" significance 
preserve
keep if lag>=1 & lag<=172
sum p_indicator
local rp: display %10.1e r(mean)
di `rp'
restore

preserve
collapse (mean) p_indicator, by(lag)
replace p_indicator = 0 if lag==0
gen l_p_value = log(p_indicator)
tw line p_indicator lag , ///
  ytitle("p-value", size(small)) xsize(4) ysize(8) fxsize(25) ///
  ylabel(0(0.05).15) ///
  xtitle("Months Since Tropical Cyclone", size(small)) ///
  yline(0.01, lp(dash) lc(gs12)) ///
  yline(0.05, lp(dash) lc(gs12)) ///
  yline(0.1, lp(dash) lc(gs12)) ///
  text(0.075 82 "Joint Significance" "P<`rp'" "(months 1 - 172)", size(small)) ///
  saving(figures/appendix/within_panel_pvalue.gph, replace)
restore

collapse 	(mean) mean_maxs = sim_coeff_LD_maxs ///
			(p1) p1_maxs = sim_coeff_LD_maxs ///
			(p2) p2_maxs = sim_coeff_LD_maxs ///
			(p5) p5_maxs = sim_coeff_LD_maxs ///
			(p10) p10_maxs = sim_coeff_LD_maxs ///
			(p25) p25_maxs = sim_coeff_LD_maxs ///
			(p50) p50_maxs = sim_coeff_LD_maxs ///
			(p75) p75_maxs = sim_coeff_LD_maxs ///
			(p90) p90_maxs = sim_coeff_LD_maxs ///
			(p95) p95_maxs = sim_coeff_LD_maxs ///
			(p98) p98_maxs = sim_coeff_LD_maxs ///
			(p99) p99_maxs = sim_coeff_LD_maxs ///
			(mean) main_linear_model = B_s  ///
			, by(lag)
			
tw 	(rarea p1_maxs p99_maxs lag, lwidth(none) fcolor(gs14)) ///
	(rarea p2_maxs p98_maxs lag, lwidth(none) fcolor(gs12)) ///
	(rarea p5_maxs p95_maxs lag, lwidth(none) fcolor(gs10)) ///
	(rarea p10_maxs p90_maxs lag, lwidth(none) fcolor(gs8)) ///
	(rarea p25_maxs p75_maxs lag, lwidth(none) fcolor(gs6)) ///
	(line main_linear_model lag, lcolor(black)) ///
	(line  mean_maxs lag, lcolor(black)), ///
	xtitle("Months Since Tropical Cyclone", size(small)) ///
	ytitle("All Cause Cumulative Mortality (per 100,000 per m/s)", size(small)) ///
	legend(off) saving(figures/appendix/within_panel.gph, replace)

graph combine figures/appendix/within_panel.gph figures/appendix/within_panel_pvalue.gph, title("Within State Randomization", size(small)) saving(figures/appendix/within_panel.gph, replace)
rm figures/appendix/within_panel_pvalue.gph

keep if lag==5 | lag==10 | lag==15 | lag==20
rename mean_maxs mean_maxs_panel
rename p*_maxs p*_maxs_panel
tempfile withinpanel
save `withinpanel' 



//------------------------------------------ WHITHIN MONTH

use "output/randomization/whithin_monthyear_randomization_output/whithin_monthyear_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .
gen simulation = _n
reshape long sim_coeff_L@D_maxs, i(simulation) j(lag)

merge m:1 lag using output/m_linear_beta.dta

//Make the p-value figure	
gen p_indicator = 0
replace p_indicator = 1 if B_s < abs(sim_coeff_LD_maxs) 
//----------- "Joint" significance 
preserve
keep if lag>=1 & lag<=172
sum p_indicator
local rp: display %10.1e r(mean)
di `rp'
restore

preserve
collapse (mean) p_indicator, by(lag)
replace p_indicator = 0 if lag==0
gen l_p_value = log(p_indicator)
tw line p_indicator lag , ///
  ytitle("p-value", size(small)) xsize(4) ysize(8) fxsize(25) ///
  ylabel(0(0.05).15) ///
  xtitle("Months Since Tropical Cyclone", size(small)) ///
  yline(0.01, lp(dash) lc(gs12)) ///
  yline(0.05, lp(dash) lc(gs12)) ///
  yline(0.1, lp(dash) lc(gs12)) ///
  text(0.075 82 "Joint Significance" "P<`rp'" "(months 1 - 172)", size(small)) ///
  saving(figures/appendix/within_month_pvalue.gph, replace)
restore


collapse 	(mean) mean_maxs = sim_coeff_LD_maxs ///
			(p1) p1_maxs = sim_coeff_LD_maxs ///
			(p2) p2_maxs = sim_coeff_LD_maxs ///
			(p5) p5_maxs = sim_coeff_LD_maxs ///
			(p10) p10_maxs = sim_coeff_LD_maxs ///
			(p25) p25_maxs = sim_coeff_LD_maxs ///
			(p50) p50_maxs = sim_coeff_LD_maxs ///
			(p75) p75_maxs = sim_coeff_LD_maxs ///
			(p90) p90_maxs = sim_coeff_LD_maxs ///
			(p95) p95_maxs = sim_coeff_LD_maxs ///
			(p98) p98_maxs = sim_coeff_LD_maxs ///
			(p99) p99_maxs = sim_coeff_LD_maxs ///
			(mean) main_linear_model = B_s  ///
			, by(lag)
			
tw 	(rarea p1_maxs p99_maxs lag, lwidth(none) fcolor(gs14)) ///
	(rarea p2_maxs p98_maxs lag, lwidth(none) fcolor(gs12)) ///
	(rarea p5_maxs p95_maxs lag, lwidth(none) fcolor(gs10)) ///
	(rarea p10_maxs p90_maxs lag, lwidth(none) fcolor(gs8)) ///
	(rarea p25_maxs p75_maxs lag, lwidth(none) fcolor(gs6)) ///
	(line main_linear_model lag, lcolor(black)) ///
	(line  mean_maxs lag, lcolor(black)), ///
	xtitle("Months Since Tropical Cyclone" , size(small)) ///
	ytitle("All Cause Cumulative Mortality (per 100,000 per m/s)", size(small)) ///
	legend(off) saving(figures/appendix/within_month.gph, replace)
	
graph combine figures/appendix/within_month.gph figures/appendix/within_month_pvalue.gph, title("Within Month Randomization", size(small))  saving(figures/appendix/within_month.gph, replace)
rm figures/appendix/within_month_pvalue.gph
	
keep if lag==5 | lag==10 | lag==15 | lag==20
rename mean_maxs mean_maxs_month
rename p*_maxs p*_maxs_month

tempfile withinmonth
save `withinmonth' 

//------------------------------------------ CROSS PANEL

use "output/randomization/cross_panel_randomization_output/cross_panel_1000_simulation_output.dta", clear
ren sim_coeff_L240_maxs sim_coeff_L240D_maxs
keep sim_coeff_L*D_maxs
drop if sim_coeff_L1D_maxs == .
gen simulation = _n
reshape long sim_coeff_L@D_maxs, i(simulation) j(lag)

merge m:1 lag using output/m_linear_beta.dta

//Make the p-value figure	
gen p_indicator = 0
replace p_indicator = 1 if B_s < abs(sim_coeff_LD_maxs) 
//----------- "Joint" significance 
preserve
keep if lag>=1 & lag<=172
sum p_indicator
local rp: display %10.1e r(mean)
di `rp'
restore

preserve
collapse (mean) p_indicator, by(lag)
replace p_indicator = 0 if lag==0
gen l_p_value = log(p_indicator)
tw line p_indicator lag , ///
  ytitle("p-value", size(small)) xsize(4) ysize(8) fxsize(25) ///
  ylabel(0(0.05).15) ///
  xtitle("Months Since Tropical Cyclone", size(small)) ///
  yline(0.01, lp(dash) lc(gs12)) ///
  yline(0.05, lp(dash) lc(gs12)) ///
  yline(0.1, lp(dash) lc(gs12)) ///
  text(0.075 82 "Joint Significance" "P<`rp'" "(months 1 - 172)", size(small)) ///
  saving(figures/appendix/cross_panel_pvalue.gph, replace)
restore


collapse 	(mean) mean_maxs = sim_coeff_LD_maxs ///
			(p1) p1_maxs = sim_coeff_LD_maxs ///
			(p2) p2_maxs = sim_coeff_LD_maxs ///
			(p5) p5_maxs = sim_coeff_LD_maxs ///
			(p10) p10_maxs = sim_coeff_LD_maxs ///
			(p25) p25_maxs = sim_coeff_LD_maxs ///
			(p50) p50_maxs = sim_coeff_LD_maxs ///
			(p75) p75_maxs = sim_coeff_LD_maxs ///
			(p90) p90_maxs = sim_coeff_LD_maxs ///
			(p95) p95_maxs = sim_coeff_LD_maxs ///
			(p98) p98_maxs = sim_coeff_LD_maxs ///
			(p99) p99_maxs = sim_coeff_LD_maxs ///
			(mean) main_linear_model = B_s  ///
			, by(lag)
			
tw 	(rarea p1_maxs p99_maxs lag, lwidth(none) fcolor(gs14)) ///
	(rarea p2_maxs p98_maxs lag, lwidth(none) fcolor(gs12)) ///
	(rarea p5_maxs p95_maxs lag, lwidth(none) fcolor(gs10)) ///
	(rarea p10_maxs p90_maxs lag, lwidth(none) fcolor(gs8)) ///
	(rarea p25_maxs p75_maxs lag, lwidth(none) fcolor(gs6)) ///
	(line main_linear_model lag, lcolor(black)) ///
	(line  mean_maxs lag, lcolor(black)), ///
	xtitle("Months Since Tropical Cyclone", size(small)) ///
	ytitle("All Cause Cumulative Mortality (per 100,000 per m/s)", size(small)) ///
	 legend(off) saving(figures/appendix/cross_panel.gph, replace)
	 
graph combine figures/appendix/cross_panel.gph figures/appendix/cross_panel_pvalue.gph, title("Block Randomization by State", size(small))  saving(figures/appendix/cross_panel.gph, replace)
rm figures/appendix/cross_panel_pvalue.gph
	
keep if lag==5 | lag==10 | lag==15 | lag==20
rename mean_maxs mean_maxs_cross
rename p*_maxs p*_maxs_cross

tempfile crosspanel
save `crosspanel' 

//---------------------------------------- 

merge 1:1 lag using `withinmonth' , nogen
merge 1:1 lag using `withinpanel' , nogen
merge 1:1 lag using `totalrandom', nogen

reshape long p@_maxs_total p@_maxs_panel p@_maxs_month p@_maxs_cross, i(lag) j(p)

graph hbox p_maxs_total p_maxs_panel p_maxs_month p_maxs_cross, over(lag) nofill asyvars bar(1, color(midblue%50)) bar(2, color(blue%50)) bar(3, color(cranberry%50)) bar(4, color(red%50)) legend(/*ring(0) position(11)*/ position(6) col(1) size(small) order(1 "Total Randomization" 2 "Within State" 3 "Within Month" 4 "Across State")) xsize(4) ysize(8) fxsize(25) 
graph save figures/randomization_boxplot_combined.gph, replace
graph export figures/randomization_boxplot_combined.png, replace

//this is in the appendix
graph combine figures/appendix/cross_panel.gph figures/appendix/within_month.gph  figures/appendix/within_panel.gph figures/appendix/total_rand.gph,  xcommon col(2) xsize(8) ysize(5)
*graph export figures/appendix/randomization_distributions_combined.png, replace
graph export figures/appendix/figureED5_randomization_distributions_combined.pdf, replace

rm figures/appendix/cross_panel.gph 
rm figures/appendix/within_month.gph 
rm figures/appendix/within_panel.gph
rm figures/appendix/total_rand.gph




///------------------------------------------- damage ~ wind

use "data/nordhaus_LICRICE_USA_merged.dta", clear


//find break point

forvalues cutpoint = .1(.1)2.5 {

	loc C = round(`cutpoint'*10)
	gen maxs_wedge_`C' = 0
	replace maxs_wedge_`C' = maxs - `cutpoint' if maxs > `cutpoint'
	/*
	reg l_n_damage maxs maxs_wedge_`C', robust
	outreg2 using tables/cutoff_searches/cutoff_search1, excel
	reg l_n_damage maxs maxs_wedge_`C' max_wind_ms, robust
	outreg2 using tables/cutoff_searches/cutoff_search2, excel
	reg l_n_damage maxs maxs_wedge_`C' min_pressure_mb, robust
	outreg2 using tables/cutoff_searches/cutoff_search3, excel
	*/
	// in simple model, bet cutoff is 1.2 m/s in maxs
}
replace maxs = 1.2-0.0001 if _n == 89
replace maxs = 1.2+0.0001 if _n == 90
replace maxs_wedge_12 = 0.0001 if _n == 90

reg l_n_damage maxs maxs_wedge_12
predict y_hat
predict error, stdp
generate lb = y_hat - invnormal(0.975)*error
generate ub = y_hat + invnormal(0.975)*error


tw (sc l_n_damage maxs, mlab(name_plot) m(none) mlabpos(0) /* fcolor(black%25) */) ///
   (lfit y_hat maxs if maxs <= 1.2, lcolor(black%75)) ///
   (lfit y_hat maxs if maxs > 1.2, lcolor(black%75)) ///
   (qfit lb maxs if maxs <= 1.2,  lcolor(gs6%50)) ///
   (qfit ub maxs if maxs <= 1.2,  lcolor(gs6%50)) ///
   (qfit lb maxs if maxs > 1.2,  lcolor(gs6%50)) ///
   (qfit ub maxs if maxs > 1.2,  lcolor(gs6%50)) ///
   , xline(1.2, lcolor(black)) legend(off) xtitle("national avg. maximum wind speed (m/s)") ytitle("Log(Damage/GDP)") /*title(USA 1950-2005)*/ text(-15 2.5 "r{sup:2}=0.39") saving(figures/appendix/USA_damage_kinked_maxs_15_SI.gph, replace)
   
preserve
collapse (sum) gdp_billions damage_millions (max) maxs, by(year)
forvalues cutpoint = .1(.1)2.5 {

	loc C = round(`cutpoint'*10)
	gen maxs_wedge_`C' = 0
	replace maxs_wedge_`C' = maxs - `cutpoint' if maxs > `cutpoint'
	/*
	reg l_n_damage maxs maxs_wedge_`C', robust
	outreg2 using tables/cutoff_searches/cutoff_search1, excel
	reg l_n_damage maxs maxs_wedge_`C' max_wind_ms, robust
	outreg2 using tables/cutoff_searches/cutoff_search2, excel
	reg l_n_damage maxs maxs_wedge_`C' min_pressure_mb, robust
	outreg2 using tables/cutoff_searches/cutoff_search3, excel
	*/
	// in simple model, bet cutoff is 1.2 m/s in maxs
}
replace maxs = 1.2-0.0001 if _n == 89
replace maxs = 1.2+0.0001 if _n == 90
replace maxs_wedge_12 = 0.0001 if _n == 90

gen l_n_damage = log(damage_millions/gdp_billions)
gen l_maxs = log(maxs)
reg l_n_damage maxs maxs_wedge_12
predict y_hat
predict error, stdp
generate lb = y_hat - invnormal(0.975)*error
generate ub = y_hat + invnormal(0.975)*error


tw (sc l_n_damage maxs,  mlab(year) m(none) mlabpos(0)) ///
   (lfit y_hat maxs if maxs <= 1.2, lcolor(black%75)) ///
   (lfit y_hat maxs if maxs > 1.2, lcolor(black%75)) ///
   (qfit lb maxs if maxs <= 1.2,  lcolor(gs6%50)) ///
   (qfit ub maxs if maxs <= 1.2,  lcolor(gs6%50)) ///
   (qfit lb maxs if maxs > 1.2,  lcolor(gs6%50)) ///
   (qfit ub maxs if maxs > 1.2,  lcolor(gs6%50)) , ///
   legend(off) text(-8 2.5 "r{sup:2}=0.32") ytit(Log(Damage/GDP)) xtit(national avg. maximum wind speed (m/s))  xline(1.2, lcolor(black)) saving(figures/appendix/USA_damage_maxs_year_SI.gph, replace)
   

restore


graph combine figures/appendix/USA_damage_kinked_maxs_15_SI.gph  figures/appendix/USA_damage_maxs_year_SI.gph , xsize(11) ysize(5)
graph export figures/appendix/figureSI2_USA_damage_maxs.pdf, replace /* NEW SI FIGURE */

  
  
**** this is the version in figure1 
drop if maxs>1.5
reg l_n_damage maxs
tw (lfitci l_n_damage maxs, lcolor(white)) ///
(sc l_n_damage maxs, /*mlab(name_plot) m(none) mlabpos(0) */ fcolor(black%25) msymbol(O)) ///
   , legend(off) xtitle("national avg. maximum wind speed (m/s)") ytitle("Log(Damage/GDP)") /*title(USA 1950-2005)*/ text(-15 1.25 "slope=4.87" "r{sup:2}=0.36") saving(figures/USA_damage_kinked_maxs.gph, replace)









///------------------------------------------- rain ~ wind


***--------------storm name data
import delim "data/storm_list.txt", delim(",") clear

rename v21 ep_name
rename v1 storm_name
rename v9 year
rename v2 stusps
drop v4 v5 v6 v7 v11 v14 v15 v16 v17 v20 v18  v3
drop if year==.
replace ep_name = subinstr(ep_name, " ", "",.) 
replace stusps = subinstr(stusps, " ", "",.) 


tempfile stormnames
save `stormnames'

import delimited "data/rainfall_idw_state_storm.csv", varnames(1) clear
drop if v1==1

gen storm_name1 = substr(storm_name,1,length(storm_name) - 5) 
replace storm_name1 = subinstr(storm_name1, " ", "_",.) 
replace storm_name1 = strupper(storm_name1)
replace storm_name1 = subinstr(storm_name1, "TROPICAL_DEPRESSION_", "",.) 
replace storm_name = subinstr(storm_name, " ", "",.)
rename storm_name filename
rename storm_name1 ep_name 

gen year = substr(filename,length(filename)-3,4) 
rename state_name adm_name 
drop name v1 lsad awater statens

destring year, replace
replace average_rain = "" if average_rain=="NA"
replace total_rain = "" if total_rain=="NA"
destring average_rain total_rain, replace

merge m:1 year ep_name using `stormnames', keep(1 3) nogen 

drop v13 v12 v19 v10 v8

replace storm_name = ep_name if storm_name==""
replace storm_name = subinstr(storm_name, " ", "",.) 
replace adm_name = subinstr(adm_name, " ", "",.) 
*replace storm_name = "NOT_NAMED" if storm_name=="UNNAMED"


tempfile rainfall
save `rainfall'


***--------------licrice data

import delimited "data/panel_by_storm__NA_USA_density_8_yr_1930_2018.csv", varnames(1) clear

duplicates drop storm_name adm_name year maxs month, force 
duplicates tag storm_name adm_name year, gen(tag)
drop if (tag>0 & maxs==0)
replace storm_name = subinstr(storm_name, " ", "",.) 
replace adm_name = subinstr(adm_name, " ", "",.) 


merge m:m storm_name adm_name year using `rainfall'

keep if _merge==3
replace average_rain=0 if average_rain==.

reg average_rain maxs
tw (scatter average_rain maxs, mcolor(lavender%15)) (lfitci average_rain maxs) , legend(off) text(10 25 "slope = 0.116" "r{sup:2} = 0.307" , place(se)) ytit(average rainfall (inches)) xtit(maximum wind speed (m/s))
graph save  "figures/scatter_rain_wind_full.gph", replace





///------------------------------------------- Combine figures to make full Figure 1

graph combine figures/time_FL_maxs.gph figures/time_NJ_maxs.gph figures/state_FL_pred.gph figures/state_NJ_pred.gph  figures/USA_damage_kinked_maxs.gph scatter_rain_wind_full.gph  figures/prediction_true_plot.gph figures/randomization_boxplot_combined.gph , col(2) ysize(9) xsize(5)
graph export figures/figure1.pdf, replace  /* THIS IS THE MAIN FIGURE */








