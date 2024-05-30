



//--------------------------------------------------------------------- Figure 4
disp("-------------------------STARTING FIGURE 4-----------------------")

use data/mortality_19002015.dta, clear
keep year totalpop month stfips stateabrev adm_name
gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1950 | year > 2015
rename adm_name adm_name_cap
gen adm_name = upper(adm_name_cap)
drop adm_name_cap
duplicates drop 
drop if month==.
tempfile population
save `population'

insheet using "data/panel_by_storm__NA_USA_density_8_yr_1930_2018.csv", clear
drop pddi

gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1950 | year > 2015

rename adm_name adm_name_cap
gen adm_name = upper(adm_name_cap)
drop adm_name_cap

collapse (max) maxs , by(adm_name modate year month)

merge m:1 adm_name modate using `population'

collapse (mean) maxs totalpop, by(adm_name year stateabrev)

gen post = 0
replace post = 1 if year>=2001

*save output/joint_max_population.dta, replace
by post, sort: egen percent_totalpop_bypost = pc(totalpop)
egen maxscat = cut(maxs), at(0,2,4,6,8,10,12,14,16)
collapse (sum) percent_totalpop_bypost , by(maxscat post)
gen p_totalpop_bypost = percent_totalpop_bypost/100

tw (bar p_totalpop_bypost maxscat if post==0, color(green%50) barwidth(2)) ///
 (bar p_totalpop_bypost maxscat if post==1, color(emerald%50) barwidth(2)) , ///
    legend(order(1 "Pre 2001" 2 "Post 2001")) xtitle("Average Annual Wind Speed (m/s)") ytitle("Fraction of CONUS Population")
graph save figures/pre_post_2001_population.gph, replace



insheet using "data/panel_by_storm__NA_USA_density_8_yr_1930_2018.csv", clear
drop pddi

gen modate = (year-1900)*12 + month
lab var modate "months since jan 1900 = 1"
drop if year < 1930 | year > 2015

rename adm_name adm_name_cap
gen adm_name = upper(adm_name_cap)
drop adm_name_cap

collapse (mean) maxs_mean = maxs (max) maxs_max = maxs , by(year storm_serial_id)

gen count = 1

collapse (mean) maxs_mean (max) maxs_max (sum) count, by(year)
 
gen post = 0
replace post = 1 if year>=2001


hist maxs_max if post==1, title("Post 2001")
hist maxs_max if post==0 , title("Pre 2001")

twoway (histogram maxs_max if post==1, frac width(5) color(blue%30)) ///        
       (histogram maxs_max if post==0, frac width(5) color(ebblue%30)), ///   
       legend(order(2 "Pre 2001" 1 "Post 2001"  )) xtitle("Max Annual Wind Speed (m/s)")
graph save figures/pre_post_2001_windspeed.gph, replace
twoway (histogram count if post==1,  frac width(5) color(blue%30)) ///        
       (histogram count if post==0,  frac width(5) color(ebblue%30)), ///   
       legend(order( 2 "Pre 2001" 1 "Post 2001"))  xtitle("Number of Tropical Cyclones")
graph save figures/pre_post_2001_count.gph, replace

graph combine figures/pre_post_2001_count.gph figures/pre_post_2001_windspeed.gph figures/pre_post_2001_population.gph , col(3) xsize(8.5) ysize(5)
graph export figures/figure4_pre_post_2001_count_windspeed_population.pdf, replace


twoway (histogram maxs_mean if post==1, frac  color(red%30)) ///        
       (histogram maxs_mean if post==0, frac  color(green%30)), ///   
       legend(order(1 "Post 2001" 2 "Pre 2001" ))

rm figures/pre_post_2001_count.gph 
rm figures/pre_post_2001_windspeed.gph 
rm figures/pre_post_2001_population.gph	   


	   
//------------- Prep data to plot deaths by race and age
 
use data/mortality_19002015.dta, clear
*use output/full_data_for_regression.dta, clear
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

collapse (sum) _0000_apop=dths_0000 _0144_apop=dths_0144 _4564_apop=dths_4564 _6599_apop =dths_6599 _black_bpop =dths_black _white_wpop=dths_white  (mean) dths_0000_mean = dths_0000 dths_0144_mean = dths_0144 dths_4564_mean = dths_4564 dths_6599_mean = dths_6599 dths_black_mean = dths_black dths_white_mean = dths_white maxs, by(adm_name adm_id)

merge 1:1 adm_name using `alldths' 

replace maxs = 0 if _merge==2	



foreach m in _0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop {
merge 1:1 adm_name using output/mort_TC_state_total_m_linear_`m'.dta, nogen
*rename mort_m_cubic_adapt_pool24 mort_m_cubic_`m'
rename mort mort_m_linear_`m'

	label variable `m' "Total deaths by age group"
	
	gen p_`m'_TC = mort_m_linear_`m' / `m' 
	replace p_`m'_TC = 0 if p_`m'_TC== .
	label variable p_`m'_TC "proportion deaths from TCs by age group"
	
	gen diff_TC_total_mort_`m' =   `m' - mort_m_linear_`m'
	replace diff_TC_total_mort_`m'= `m' if diff_TC_total_mort_`m'==.
	label variable diff_TC_total_mort_`m' "difference total deaths from TCs by age group"
	
}

foreach age in 0000 0144 4564 6599 {
di "`age'"

gen total_dths_rate_`age' = (_`age'_apop / pop_`age') *100000
label variable total_dths_rate_`age' "Total mortality per 100,000"
gen TC_dths_rate_`age' = (mort_m_linear__`age'_apop / pop_`age') *100000
label variable TC_dths_rate_`age' "TC mortality per 100,000"
gen diff_TC_total_rate_`age' = total_dths_rate_`age' - TC_dths_rate_`age'
replace diff_TC_total_rate_`age' = total_dths_rate_`age' if diff_TC_total_rate_`age'==.

drop if adm_name==""

}

save output/FORMAPS_linear_state_mort_age_race.dta, replace /* This is read into the R code to create the maps (Figure 4a and 4c-f) */


//----------------------------------------------------------------- Figure 4g

keep total_dths_rate_* diff_TC_total_rate_* adm_name maxs

 gen TC = 0

 replace TC = 1 if maxs>0


reshape wide total_dths_rate_* diff_TC_total_rate_*, i(adm_name) j(TC) 

gen age1 = 1
gen age2 = 2 
gen age3 = 3

foreach age in 0000 0144 4564 6599 {

	egen mean_total_noTC = mean(total_dths_rate_`age'0)
	egen median_total_noTC = median(total_dths_rate_`age'0)
    egen upq_total_noTC = pctile(total_dths_rate_`age'0), p(75) 
    egen loq_total_noTC = pctile(total_dths_rate_`age'0), p(25) 
	egen iqr_total_noTC = iqr(total_dths_rate_`age'0)
	egen upper_total_noTC = pctile(total_dths_rate_`age'0), p(90)
	egen lower_total_noTC = pctile(total_dths_rate_`age'0), p(10)
	*egen upper_total_noTC = max(total_dths_rate_`age'0)
	*egen lower_total_noTC = min(total_dths_rate_`age'0)
	
	egen mean_total_withTC = mean(total_dths_rate_`age'1)
	egen median_total_withTC = median(total_dths_rate_`age'1)
    egen upq_total_withTC = pctile(total_dths_rate_`age'1), p(75) 
    egen loq_total_withTC = pctile(total_dths_rate_`age'1), p(25) 
	egen iqr_total_withTC = iqr(total_dths_rate_`age'1)
	egen upper_total_withTC  = pctile(total_dths_rate_`age'1), p(90) 
	egen lower_total_withTC  = pctile(total_dths_rate_`age'1), p(10) 
	*egen upper_total_withTC  = max(min(total_dths_rate_`age'1, upq_total_withTC  + 1.5 * iqr_total_withTC ))
	*egen lower_total_withTC  = min(max(total_dths_rate_`age'1, loq_total_withTC  - 1.5 * iqr_total_withTC ))
	
	egen mean_total_minusTC = mean(diff_TC_total_rate_`age'1)
	egen median_total_minusTC = median(diff_TC_total_rate_`age'1)
    egen upq_total_minusTC = pctile(diff_TC_total_rate_`age'1), p(75) 
    egen loq_total_minusTC = pctile(diff_TC_total_rate_`age'1), p(25) 
	egen iqr_total_minusTC = iqr(diff_TC_total_rate_`age'1)
	egen upper_total_minusTC = pctile(diff_TC_total_rate_`age'1), p(90)
	egen lower_total_minusTC = pctile(diff_TC_total_rate_`age'1), p(10)
	*egen upper_total_minusTC = max(min(diff_TC_total_rate_`age'1, upq_total_minusTC + 1.5 * iqr_total_minusTC))
	*egen lower_total_minusTC = min(max(diff_TC_total_rate_`age'1, loq_total_minusTC - 1.5 * iqr_total_minusTC))

	sum total_dths_rate_`age'0
	local noTC = round(r(mean), 1)
	
	sum total_dths_rate_`age'1
	local withTC = round(r(mean),1)
	local min = r(max) + 0.1*r(min)
	
	sum diff_TC_total_rate_`age'1
	local minusTC = round(r(mean), 1)
	
	*loc age "0000"
	twoway (rbar median_total_noTC upq_total_noTC age1 , pstyle(p1) blc(gs15) bfc(midblue%50) barw(0.35)) ///
    (rbar median_total_noTC loq_total_noTC age1, pstyle(p1) blc(gs15) bfc(midblue%50) barw(0.35) ) ///
    (rspike upq_total_noTC upper_total_noTC age1, pstyle(p1) lcolor(midblue)) ///
    (rspike loq_total_noTC lower_total_noTC age1, pstyle(p1) lcolor(midblue)) ///
	(rcap upper_total_noTC upper_total_noTC age1, pstyle(p1) msize(*2) lcolor(midblue)) ///
	(rcap lower_total_noTC lower_total_noTC age1, pstyle(p1) msize(*2) lcolor(midblue)) ///
	(scatter total_dths_rate_`age'0 age1 if !inrange(total_dths_rate_`age'0, lower_total_noTC, upper_total_noTC), ms(Oh) color(midblue)) ///
	(scatter mean_total_noTC age1, ms(T) color(midblue*2)) ///
(rbar median_total_withTC upq_total_withTC age2, pstyle(p1) blc(gs15) bfc(cranberry%50) barw(0.35)) ///
    (rbar median_total_withTC loq_total_withTC age2, pstyle(p1) blc(gs15) bfc(cranberry%50) barw(0.35) ) ///
    (rspike upq_total_withTC upper_total_withTC age2, pstyle(p1) lcolor(cranberry)) ///
    (rspike loq_total_withTC lower_total_withTC age2, pstyle(p1) lcolor(cranberry)) ///
	(rcap upper_total_withTC upper_total_withTC age2, pstyle(p1) msize(*2) lcolor(cranberry)) ///
	(rcap lower_total_withTC lower_total_withTC age2, pstyle(p1) msize(*2) lcolor(cranberry)) ///
	(scatter total_dths_rate_`age'1 age2 if !inrange(total_dths_rate_`age'1, lower_total_withTC, upper_total_withTC), color(cranberry) ms(Oh)) ///
	(scatter mean_total_withTC age2, ms(T) color(cranberry*2)) ///
(rbar median_total_minusTC upq_total_minusTC age3, pstyle(p1) blc(gs15) bfc(red%50) barw(0.35)) ///
    (rbar median_total_minusTC loq_total_minusTC age3, pstyle(p1) blc(gs15) bfc(red%50) barw(0.35)) ///
    (rspike upq_total_minusTC upper_total_minusTC age3, pstyle(p1) lcolor(red)) ///
    (rspike loq_total_minusTC lower_total_minusTC age3, pstyle(p1) lcolor(red)) ///
	(rcap upper_total_minusTC upper_total_minusTC age3, pstyle(p1) msize(*2) lcolor(red)) ///
	(rcap lower_total_minusTC lower_total_minusTC age3, pstyle(p1) msize(*2) lcolor(red)) ///
	(scatter mean_total_minusTC age3, ms(T) color(red*2)) ///
	(scatter diff_TC_total_rate_`age'1 age3 if !inrange(diff_TC_total_rate_`age'1, lower_total_minusTC, upper_total_minusTC), color(red) ms(Oh)) ///
	, legend(order(1 "non-TC states" 9 "TC states (actual)" 17 "TC states without TCs") col(3)) xlabel(1(1)3, nolabels notick)  ///
 ytitle("Death per 100,000") ///
	text(`noTC' 1.5  "`noTC'", size(small) color(midblue*2)) ///
	text(`withTC' 2.5  "`withTC'", size(small) color(cranberry*2)) ///
	text(`minusTC' 3.5  "`minusTC'", size(small) color(red*1.5))
	graph save figures/box_TC_`age'.gph, replace
	
	drop iqr_total_noTC lower_total_noTC loq_total_withTC iqr_total_withTC lower_total_withTC loq_total_minusTC iqr_total_minusTC lower_total_minusTC mean_total_noTC median_total_noTC mean_total_withTC median_total_withTC mean_total_minusTC median_total_minusTC upq_total_noTC upper_total_noTC upq_total_withTC upper_total_withTC upq_total_minusTC upper_total_minusTC loq_total_noTC

}

grc1leg figures/box_TC_0000.gph figures/box_TC_0144.gph figures/box_TC_4564.gph figures/box_TC_6599.gph, col(4) legendfrom(figures/box_TC_0000.gph)

graph export figures/figure4_state_mortality_withoutTCs.pdf, replace

//---------------------------------------------------------------------------------
//------------------------------------Figure 4b: Bar Chart of summary statistics for multiple outcomes
//---------------------------------------------------------------------------------
use "output/full_data_for_regression.dta", clear
collapse (sum) dths_0000 dths_0144 dths_4564 dths_6599 dths_black dths_white
gen id = 1
reshape long dths_@ , i(id) j(age) string

gen age_string = ""
replace age_string = "_0000_apop" if age=="0000"
replace age_string = "_0144_apop" if age=="0144"
replace age_string = "_4564_apop" if age=="4564"
replace age_string = "_6599_apop" if age=="6599"
replace age_string = "_black_bpop" if age=="black"
replace age_string = "_white_wpop" if age=="white"
drop age
rename age_string age
tempfile age_dths
save `age_dths'

loc outcome_list "_0000_apop _0144_apop _4564_apop _6599_apop _black_bpop _white_wpop"
foreach Y of loc outcome_list {
di "`Y'"
use "output/mort_TC_date_total_m_linear_`Y'.dta", clear
collapse (sum) mort
gen age = "`Y'"
tempfile total_`Y'
save `total_`Y''
}
loc outcome_list "_0000_apop _0144_apop _4564_apop _6599_apop _black_bpop"
foreach Y of loc outcome_list {
append using `total_`Y''
}
merge 1:1 age using `age_dths', keep(3)
gen proportion = mort/dths_
gen total = 1
encode age, gen(agenum) label(age)

gen TC = string(mort , "%9.0fc") + " (" + string(proportion*100, "%9.0f") + "%)"
gen all =  string(dths_ - mort, "%10.0fc")
gen proportionlbl = string(proportion, "%9.2f")
gen x = total - .2
gen yaxis1 = agenum +.15
gen yaxis2 = agenum -.15
gen yaxis3 = agenum +.25
gen dths_xaxis = dths_*.9

gen mort_per_year = mort/55

twoway (bar mort_per_year  yaxis2, yaxis(1) ytitle("Excess Deaths per year", axis(1)) barw(0.3) bcolor(red) base(0)) ///
(bar proportion yaxis1, yaxis(2) ytitle("Proportion of Total Deaths", axis(2)) barw(0.3) base(0)) ///
, xla(1 "Age <1" 2 "Age 1 - 44" 3 "Age 45 - 64" 4 "Age 65+" 5 "Black" 6 "White" , angle(45)) xt("") legend(order(1 "Excess" "Deaths from TCs" 2 "Proportion of Total" "Deaths from TCs") size(small)) 
graph export figures/figure4_mort_age_race_percent_total.pdf, replace





//---------------------------------------------------------------------------------
//----------------------Figure 4k : plot attribution cubic models (with fixed population)
//---------------------------------------------------------------------------------


use output/mort_TC_date_total_m_cubic_adapt_pool24_tdths_ttpop_1950pop.dta, clear

rename mort mort_pop1950

merge 1:1 modate using output/mort_TC_date_total_m_cubic_adapt_pool24_tdths_ttpop_2015pop.dta, nogen
rename mort mort_pop2015

merge 1:1 modate using output/mort_TC_date_total_m_cubic_adapt_pool24_tdths_ttpop_deflated2015pop.dta
rename mort mort_deflatedpop2015

merge 1:1 modate using output/mort_TC_date_total_m_cubic_adapt_pool24_tdths_ttpop.dta, nogen

drop if mort==0
gen year = int(modate/12 +1900)
gen month = modate - (year-1900)*12 +1
gen date = ym(year, month)
format date %tm


tw (lpoly mort  date, color(orange%75)) (lfit mort date, color(orange%50) lpattern(dash) lwidth(thin)) ///
(lpoly  mort_pop1950 date, color(cranberry%75)) (lfit mort_pop1950  date, color(cranberry%50) lpattern(dash) lwidth(thin)) ///
(lpoly  mort_pop2015 date, color(red%75)) (lfit mort_pop2015  date, color(red%50) lpattern(dash) lwidth(thin)) ///
(lpoly  mort_deflatedpop2015 date, color(gold%75)) (lfit mort_deflatedpop2015  date, color(gold%50) lpattern(dash) lwidth(thin)), ///
legend(order(5 "Fixed 2015 Population" 1 "Actual" 7 "2015 Population Distribution," "Deflated to 1950 Levels"  3 "Fixed 1950 Population"  2 "Best Fit Lines") ring(0) position(11) col(1) size(small)) ///
ytitle("Excess Deaths per Month") xlabel(,format(%tmCY)) xtitle("") ///
tlabel(1950m10 1960m1 1970m1 1980m1 1990m1 2000m1 2010m1)  ylabel(0(2000)18000)
graph export figures/figure4_total_deaths.pdf,replace



rm figures/box_TC_0000.gph 
rm figures/box_TC_0144.gph 
rm figures/box_TC_4564.gph 
rm figures/box_TC_6599.gph

disp("-------------------------MAIN FIGURES DONE-----------------------")


