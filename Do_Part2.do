************************************************************************
//																	  //
//							TOP INCOMES IN CHILE					  //
//																	  //
//								  Part II							  //
//																	  //
************************************************************************


clear all
cd "~/Dropbox/GitHub/Top_incomes_Chile"	

// 0. LOCALS AND TEMPORARY FILES----------------------------------------------//

local inc_concepts "fiscal fiscal_ss"
local groups "t10 t1 t01 t001"
local var_a "topsh thr b topavg"
local taxtype "C GC"
local start_pos=91
local start_freq=0.1
local cpi "cpi_base2016"
local usd_ppa_2013=409.1
foreach g in `groups' {
		local `g'_pos=`start_pos'
		local `g'_freq=`start_freq'
		local start_pos=`start_pos'+9
		local start_freq=`start_freq'/10
		di "Info for `g' in _n = [``g'_pos'] (in annual sheets)" ///
			", it gathers ``g'_freq' of adult population"
}

tempfile tf tf_C  tf_GC

foreach totinc in `inc_concepts' {
	// 1. COMBINE RESULTS FROM BOTH SERIES----------------------------------------//

	//Import Gpinter results and change name of variables 
	foreach ty in `taxtype' {
		quietly import excel "Data/Gpinter/output/`ty'_`totinc'.xlsx" ///
			,sheet("series") clear firstrow	
		quietly destring Year, replace
		if ("`ty'"=="GC") {
			//drop weird years
			drop if Year==1975 | Year==1972
		}
		quietly ds Year Country Component, not
		local vars "`r(varlist)'"
		foreach v in `vars' {
			rename `v' `v'_`ty'
		}
		//Get info from annual sheets
		quietly levelsof Year, local(allyears)
		preserve
			foreach yr in `allyears'{
				quietly import excel "Data/Gpinter/output/`ty'_`totinc'.xlsx" ///
				,sheet("total, CHI, `yr'") clear firstrow 
				foreach g in `groups' {
					foreach v in `var_a' {
						local `v'_`g'_`yr'_`ty'=`v'[``g'_pos']
					}
				}
			}
		restore
		//Write on main dataset
		foreach g in `groups'{
			foreach v in `var_a'{
				quietly gen `g'_`v'_`ty'=.
				foreach yr in `allyears'{
					quietly replace `g'_`v'_`ty'=``v'_`g'_`yr'_`ty'' ///
						if Year==`yr'
				}
			}
		}
		//Save	
		quietly save `tf_`ty'', replace 	
	}	

	//Merge and Save unified dataset 
	quietly use `tf_GC', clear 
	quietly merge 1:1 Year using `tf_C', nogenerate 
	quietly save `tf', replace 

	//2. PREPARE ADJUSTMENT FOR IUSC (SCALING) --------------------------------//

	//Ratio of estimates from different Series
	foreach g in `groups' {
		tempvar aux_`g'
		quietly gen `aux_`g''=`g'_topavg_C/`g'_topavg_GC
		quietly sum `aux_`g'' if !missing(`aux_`g'') & Year<=2011
		local scal1_`g'_`totinc'=r(mean)
	}

	//Graph and Save
	graph twoway (line `aux_t10' Year, lcolor(edkblue)) ///
		(line `aux_t1' Year, lcolor(maroon)) ///
		(line  `aux_t01' Year, lcolor(sand)) (line `aux_t001' Year, lcolor(olive)) ///
		if Year>=2005 ///
		, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Average-income ratio") ///
		yline(1, lcolor(black) lpattern(dot)) ///
		xlabel(2005(2)2017, labsize(medium) grid labels) ///
		ylabel(0.6(0.2)1.4, labsize(medium) angle(horizontal) format(%2.1f) grid labels) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(1.25 2008  "Top 10%", color(edkblue)) text(1.05  2008  "Top 1%", color(maroon)) ///
		text(1.15 2008 "Top 0.1%", color(sand)) text(0.95 2008  "Top 0.01%", color(olive)) ///
		legend(off)
		quietly graph export "Figures/topavg_ratio.pdf", replace 
		
	//Save and export to table of scaling factors
	quietly save `tf', replace 	
	clear 
	set obs 1
	foreach g in `groups'{
		quietly gen `g'_`totinc'=`scal1_`g'_`totinc''
	}
	quietly export excel using "Data/auxi.xlsx" ///
		, firstrow(variables) sheet("scal_`totinc'") sheetreplace

	// 3. PREPARE ADJUSTMENT FOR SOCIAL SECURITY CONTRIBUTIONS -------------------//

	//Retrieve labor decomposition
	forvalues y=2005(1)2009 {
		quietly import excel "Data/tab_gc_1998_2009.xlsx" ///
			,sheet("`y'") clear firstrow cellrange(B4:G47) 
		quietly rename Mayor thr 
		quietly keep thr NumbContribWage
		quietly gsort -thr 
			quietly gen cum_n_wage=sum(NumbContribWage)
		quietly sort thr
		foreach g in `groups'{
			preserve 
				//macro dir
				keep if thr>=`thr_`g'_`y'_C'
				local dep_n_`g'_`y'=cum_n_wage[1]
			restore
		}
	}

	//Import Population, UF and Social Security variables
	local db "totpop_wb UF social_sec"
	foreach d in `db' {
		quietly import excel "Data/`d'.xlsx" ///
			,sheet("Hoja1") clear firstrow
		if ("`d'"=="totpop_wb") {
			rename year Year 
		}
		if ("`d'"=="social_sec"){
			//Linear interpolation for social contrib rates
			quietly ipolate tasa_tot_empart Year ///
				if Year<1980, gen(ip_tasa_tot_empart) ///
				epolate
		}
		tempfile tf_`d'
		quietly save `tf_`d'', replace
	}

	//Bring sueldos vitales 
	quietly import excel "Data/tab_gc_1963_1981.xlsx" ///
		,sheet("tab") clear firstrow 
	quietly collapse (firstnm) s_vital, by(year)
	quietly rename year Year
	quietly keep if !missing(s_vital)
	tempfile tf_svit
	quietly save `tf_svit', replace

	//Merge with main data
	quietly use `tf', clear
	local db "totpop_wb UF svit social_sec"
	foreach d in `db'{
		merge 1:m Year using `tf_`d'', nogenerate
		quietly keep if !missing(Country)
	}

	//Write share of wage earners in each top group
	foreach g in `groups' {
		quietly gen n_workers_`g'=.
		forvalues yr=2005(1)2009{
			quietly replace n_workers_`g' = `dep_n_`g'_`yr'' / (``g'_freq' * totpop) ///
				if Year==`yr'
		}
		quietly sum n_workers_`g' if !missing(n_workers_`g')
		local freq_workers_`g'=r(mean)
	}

	//Estimate maximum contribution to social security	
	quietly gen max_contrib_ss = tope_imp_afp * 12 * UF * (tasa_salud + tasa_pensiones) ///
		+  tope_imp_cesa * 12 * UF * tasa_cesantia 
	quietly replace max_contrib_ss = tope_imp_empart * s_vital * (ip_tasa_tot_empart / 100) ///
		if Year < 1980
	quietly gen max_bimp_ss = tope_imp_afp * 12 * UF	
	quietly replace max_bimp_ss = tope_imp_empart * s_vital ///
		if Year < 1980
	quietly save `tf', replace 	
	
	// 5. ADJUSTMETNTS  ----------------------------------------------------//	
		
	//Import scaling factors from Part 1 (deductions)
	quietly import excel "Data/auxi.xlsx" ///
		, sheet("avg_deduc") clear firstrow 
	foreach g in `groups'{
		//scaling factor with 1973's profile (taxes already paid)
		quietly sum avg_taxded_`g' if Year==1973
		local scal_taxded_1973_`g'=r(mean)/100
		//scaling factor in the 1990's (taxes paid + allowances)
		quietly sum total_`g' if Year!=1973
		local scal_deds_1990s_`g'=r(mean)/100
	}
	
	//Import alternative scaling factors (aggregated f22 data)
	quietly import excel "Data/f22tot.xlsx" ///
		, sheet("Datos") clear firstrow
	quietly levelsof year, local(f22years)
 	foreach yr in `f22years' {
		quietly sum ded_rate if year == `yr'
		local ded_r_`yr' = r(sum)
	} 
	
	//Apply adjustments
	quietly use `tf', clear
	foreach g in `groups' {
		foreach v in "topsh" "topavg" "thr" {
			//Use consolidados series when available
			quietly gen `g'_`v'_adj = `g'_`v'_GC
			quietly replace `g'_`v'_adj = `g'_`v'_C if Year >= 2005
			//Adjustment for C/GC
			quietly replace `g'_`v'_adj=`g'_`v'_adj*`scal1_`g'_`totinc'' ///
				if Year < 2005 & Year >= 1972			
			//Adjustment for deductions
			quietly replace `g'_`v'_adj=`g'_`v'_adj*(1+`scal_taxded_1973_`g'') ///
			if Year<1990
			quietly replace `g'_`v'_adj=`g'_`v'_adj*(1+`scal_deds_1990s_`g'') ///
			if Year>=1990
			local auxi1_`g'=1+`scal_taxded_1973_`g''
			local auxi2_`g'=1+`scal_deds_1990s_`g''
			if ("`v'"=="topsh"){
				di "`g' : Scaling factor for deductions in 1973 (only taxes previously paid): `auxi1_`g''." ///
				" for deductions in the 1990s (paid taxes + allowances): `auxi2_`g''"
			}	
			//Create alternative variable for 2001-2016	
			quietly gen `g'_`v'_alt = `g'_`v'_adj
			//quietly replace `g'_`v'_alt = `g'_`v'_adj * 1.03 if Year >= 2001
			quietly gen aux_ded_`g'_`v' = 1.02465828755 if Year == 2001
			quietly replace aux_ded_`g'_`v' = 1.03698743132 if Year == 2017
			quietly ipolate aux_ded_`g'_`v' Year ///
				if Year>=1964 & Year <= 2017, gen(aux_ded2_`g'_`v') 
			quietly replace `g'_`v'_alt = `g'_`v'_adj * aux_ded2_`g'_`v'  ///
				if Year >= 2001
			//Adjustment for social security contributions (from 1980)	
			if ("`v'"=="topavg" & "`totinc'"=="fiscal_ss"){
				//After 1980
				quietly gen aux_ss_`g' = (max_contrib_ss * `freq_workers_`g'') / `g'_`v'_adj
				quietly replace `g'_`v'_adj = `g'_`v'_adj + (max_contrib_ss * `freq_workers_`g'') ///
					if !missing(max_contrib_ss)
				//Before 1980 (Fill the blanks)	
				quietly sum aux_ss_`g' if Year<1980
				quietly replace aux_ss_`g' = r(mean) ///
					if missing(aux_ss_`g')  & Year < 1980
			}	
			
		}
		if ("`totinc'"=="fiscal_ss"){
			quietly replace `g'_topsh_adj = `g'_topsh_adj * (1 + aux_ss_`g') ///
				if !missing(aux_ss_`g')
		}

		foreach t in "`g'_topsh_C" "`g'_topsh_adj" "`g'_topsh_GC" "`g'_topsh_alt" {
			quietly replace `t' = `t' * 100
		}
	quietly gen beta_`g' = `g'_topavg_adj / `g'_thr_adj	
	}

	//Other paper's results
	preserve
		tempfile tf_tdr tf_fj tf_sm
	//Import Farifield & Jorratt (2016) data
		quietly import excel "Data/F&J2016.xlsx", ///
			sheet("Hoja1") clear firstrow
		quietly save `tf_fj', replace 	
	//Import Sanhueza & Mayer (2011) data	
		quietly import excel "Data/S&M2011.xlsx", ///
			sheet("Data") clear firstrow
		quietly save `tf_sm', replace	
	restore
	quietly merge 1:1 Year using `tf_fj', nogenerate
	quietly merge 1:1 Year using `tf_sm', nogenerate 
	
	if ("`totinc'"=="fiscal_ss"){
		foreach g in "t10" "t1" "t01" {
			//TESTEANDO
			quietly gen testing_`g'=FJ_`g'_Rlzd/`g'_topsh_adj
			quietly sum testing_`g'
			local testing_`g'=r(mean)
			//quietly replace `g'_topsh_adj=`g'_topsh_adj*`testing_`g'' if Year>=2001
			di "`g': Difference btw `totinc' series and F&J estimates: `testing_`g''"
		}
	}
	
//Beta coefficients

graph twoway (connected beta_t1 Year, lcolor(black) msize(small) mcolor(black) mstyle(O) mfcolor(black*0.3)) ///
	, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
	ytitle("Inverted Beta Coefficient") xtitle("") ///
	xlabel(1960(10)2020, labsize(medium) grid labels) ///
	ylabel(1(0.5)3.5, labsize(medium) angle(horizontal) format(%2.1g) grid labels) ///
	scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
	text(2.1 2017  "Top 1%", color(black)) ///
	legend(off)
quietly graph export "Figures/betas_`totinc'.pdf", replace 

	
//6. DECLARED INCOME SERIES --------------------------------------------------//
	
	//Graph Fiscal top shares 
	
	if ("`totinc'"=="`totinc'"){
		// Top 10
		graph twoway (connected t10_topsh_adj Year, lcolor(olive) ///
			mcolor(olive) msize(medium) msymbol(X)) ///
			(connected t1_topsh_adj Year, lcolor(edkblue) ///
			mcolor(edkblue) mfcolor(white) msize(small) msymbol(T)) ///
			if Year >= 2005 & Year <= 2017 ///
			, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Share of `totinc' income (%)") xtitle("") ///
			xlabel(2005(2)2017, labsize(medium) grid labels) ///
			ylabel(0(10)60, labsize(medium) angle(horizontal) format(%2.1g) grid labels) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(20 2016  "Top 1%", color(edkblue)) text(53  2016  "Top 10%", color(olive)) ///
			legend(off)
		quietly graph export "Figures/topsh_t10_`totinc'.pdf", replace 
		
		// Top 1
		graph twoway (connected t1_topsh_adj Year, lcolor(edkblue) ///
			mcolor(edkblue) mfcolor(white) msize(small) msymbol(T)) ///
			(connected t01_topsh_adj Year, lcolor(maroon) mcolor(maroon) mfcolor(white) msize(small) msymbol(D) ) ///
			(connected t001_topsh_adj Year, lcolor(sand) mcolor(sand ) msize(small) msymbol(+) ) ///
			if Year <= 2017 ///
			, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Share of `totinc' income (%)") xtitle("") ///
			xlabel(1960(10)2020, labsize(medium) grid labels) ///
			ylabel(0(2)18, labsize(medium) angle(horizontal) format(%2.1g) grid) ///
			xmtick(1960(5)2020, grid) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(17 2015  "Top 1%", color(edkblue)) text(5.5  2015  "Top 0.1%", color(maroon)) ///
			text(2  2015  "Top 0.01%", color(sand)) ///
			legend(off)
		quietly graph export "Figures/topsh_t1_`totinc'.pdf", replace 
		
		// Top 1 - Alternative
		graph twoway (line t1_topsh_alt Year, lcolor(maroon) lpattern(dash)) ///
			(line t1_topsh_adj Year, lcolor(edkblue)) if Year <= 2017 ///
			, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Share of `totinc' income (%)") xtitle("") ///
			xlabel(1960(10)2020, labsize(medium) grid labels) ///
			ylabel(8(2)20, labsize(medium) angle(horizontal) format(%2.1g) grid) ///
			xmtick(1960(5)2020, grid) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(13.5 2012  "Benchmark", color(edkblue)) ///
			text(17 2012  "Alternative", color(maroon)) ///
			legend(off)
		quietly graph export "Figures/topsh_t1alt_`totinc'.pdf", replace 
		
		quietly sort Year 
		quietly drop if Year<1964
		
		graph twoway (connected t1_topsh_adj Year, lcolor(edkblue) ///
			mcolor(edkblue) mfcolor(white) msize(small) msymbol(T)) ///
			(line t1_SM Year, lcolor(forest_green)) ///
			(scatter FJ_t1_Rlzd Year, lcolor(red) mcolor(red) msize(small) msymbol(X) ) ///
			if Year <= 2017 ///
			, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Share of `totinc' income (%)") xtitle("") ///
			xlabel(1960(10)2020, labsize(medium) grid labels) ///
			ylabel(0(2)18, labsize(medium) angle(horizontal) format(%2.1g) grid) ///
			xmtick(1960(5)2020, grid) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(17 2015  "Fiscal", color(edkblue)) text(7  2010  "S&M2011", color(forest_green)) ///
			text(14  2011  "F&J2016", color(red)) ///
			legend(off)
		quietly graph export "Figures/topsh_t1_`totinc'_FJSM.pdf", replace 
	}
	
// 7. ADJUSTMENT FOR UNDISTRIBUTED PROFITS ----------------------------------//
	
	//Import data from National Accounts
	preserve
		quietly import excel "Data/na.xls", ///
			clear firstrow sheet("TOT-FiscInc")
		quietly keep UndProfits_gdp FISCAL_SSGDP year
		drop if year=="AVG"
		destring year, replace
		quietly rename year Year
		tempfile tf_up
		quietly save `tf_up', replace
	restore
	
	//Merge with main dataset
	quietly merge 1:1 Year using `tf_up', nogenerate
	
	//Undistributed profits (total)
	quietly sum UndProfits_gdp if Year == 1996
	quietly replace UndProfits_gdp = r(max) ///
	if Year < 1996 & Year >= 1990
	//Prepare for hgher and lower bounds
	local bounds "lower upper"
	local groups2 "t10 t1 t01"
	foreach g in `groups2' {
		foreach b in `bounds' {
			quietly gen UP_`b'_`g' = FJ_`g'_UndProfits
			quietly sum FJ_`g'_UndProfits if Year == 2005
			quietly replace UP_`b'_`g' = r(max) ///
			if missing(UP_`b'_`g') & Year < 2005 & Year >= 1990
			if "`b'"=="upper" {
				quietly sum FJ_`g'_UndProfits if Year == 2009
				quietly replace UP_`b'_`g' = r(max) ///
				if missing(UP_`b'_`g') & Year > 2009 
			}
			quietly ipolate UP_`b'_`g' Year ///
				if Year>=1990, gen(UP_`b'_ip_`g') ///
				epolate	
		}
	}
	quietly save `tf', replace 			
				
	//Import World Bank data (dor GDP, CPI and POP)
	local wbd "gdp totpop cpi"
	foreach f in `wbd' {
		quietly import excel "Data/`f'_wb.xlsx" ///
			,sheet("Hoja1") clear firstrow	
		if ("`f'"=="gdp") {
			//Convert to escudos until 1973
			quietly replace gdp=gdp*1000 if year<=1973
			quietly label var ///
				gdp "Escudos before 1973, Pesos after"
		}
		if ("`f'"=="cpi"){
			keep year `cpi'
		}
		quietly rename year Year
		tempfile tf_`f'	
		quietly save `tf_`f'', replace	
	}

	//Merge everything
	quietly use `tf', clear
	foreach db in "gdp" "totpop" "cpi" {
		quietly merge m:1 Year using `tf_`db'', nogenerate 
	}
				
	//Lower and upper bounds
	foreach g in "t10" "t1" "t01" { 
		foreach b in `bounds' {
			local mult=1
			if ("`b'"=="lower"){
				local mult=2/3
			}
			quietly gen `g'_avg_UndProf_`b' = `g'_topavg_adj + ///
			(UndProfits_gdp * gdp / (``g'_freq' * totpop) * (UP_`b'_ip_`g'  * `mult') ///
			/ 100)
			quietly gen `g'_share_UndProf_`b' = `g'_avg_UndProf_`b' * ``g'_freq' / ///
			(Average_GC + UndProfits_gdp * gdp / totpop) * 100
		}
		
		//Central trend
		foreach v in "avg" "share" {
			quietly egen `g'_`v'_UndProf= ///
				rowmean(`g'_`v'_UndProf_lower `g'_`v'_UndProf_upper)
		}
	}
	quietly gen avg_UndProf = (Average_GC + UndProfits_gdp * gdp / totpop) * 100
	
	//Undistributed Profits as share of Fiscal Income 
	quietly gen test1=Average_GC*totpop/gdp
	quietly gen test2=UndProfits_gdp/test1

	//Top 1% and 0.1%
	graph twoway (rarea t1_share_UndProf_upper t1_share_UndProf_lower Year, color(edkblue*0.5)) ///
	(line t1_share_UndProf Year, lcolor(edkblue)) ///
	(rarea t01_share_UndProf_upper t01_share_UndProf_lower Year, color(maroon*0.5)) ///
	(line t01_share_UndProf Year, lcolor(maroon) lpattern(dash)) ///
	(scatter FJ_t1_AccrdProf Year, msymbol(O) mcolor(edkblue) mfcolor(edkblue*0.5)) ///
	(scatter FJ_t01_AccrdProf Year, msymbol(O) mcolor(maroon) mfcolor(maroon*0.5)) ///
		if Year>=1990 ///
		, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Share of total income (%)") xtitle("") ///
		xlabel(1990(5)2017, labsize(medium) grid labels) ///
		ylabel(5(5)25, labsize(medium) angle(horizontal) format(%2.1g) grid) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(26 2015  "Top 1%", color(edkblue)) ///
		text(11 2015  "Top 0.1%", color(maroon)) ///
		legend(off)
	quietly graph export "Figures/topsh_UndisProf_`totinc'.pdf", replace
	
	//Top 10%
	graph twoway (rarea t10_share_UndProf_upper t10_share_UndProf_lower Year, color(olive*0.5)) ///
	(line t10_share_UndProf Year, lcolor(olive)) ///
	(scatter FJ_t10_AccrdProf Year, mcolor(maroon) msymbol(X) msize(medium)) ///
		if Year>=2005 ///
		, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Share of total income (%)") xtitle("") ///
		xlabel(2005(5)2017, labsize(medium) grid labels) ///
		ylabel(40(5)65, labsize(medium) angle(horizontal) format(%2.1g) grid) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(56.5 2015  "Top 10%", color(olive)) ///
		text(56.5 2005.5  "F&J", color(maroon)) ///
		legend(off)
	quietly graph export "Figures/t10_UProf_`totinc'.pdf", replace
	
	//Undistributed Profits vs CTI
	quietly gen CTI_aux = Average_GC * totpop / gdp * 100
	quietly replace UndProfits_gdp = UndProfits_gdp * 100
	
	graph twoway ///
	(connected UndProfits_gdp Year, lcolor(black) mcolor(black) mfcolor(black) msymbol(O) yaxis(1)) ///
	(connected CTI_aux Year, lcolor(black) mcolor(black) mfcolor(white) msymbol(S) yaxis(2) lpattern(dash)) ///
		if Year>=1996 ///
		, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Undistributed Profits (%GDP)") xtitle("") ///
		ytitle("Total Fiscal Income (%GDP)", axis(2)) ///
		xlabel(1995(5)2017, labsize(medium) grid labels) ///
		ylabel(0(5)15, labsize(medium) angle(horizontal) format(%2.1g) grid) ///
		ylabel(40(5)55, labsize(medium) angle(horizontal) format(%2.1g) grid axis(2)) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(2 2000  "Undistrib." "Profits", color(black)) ///
		text(13 2000  "Fiscal" "Income", color(black)) ///
		legend(off)
	quietly graph export "Figures/CTI_UndisProf_`totinc'.pdf", replace

// 8. AVERAGE INCOMES -------------------------------------------------------//

//Get Bottom 99%
quietly gen bottom99_avg = (1 - t1_topsh_adj / 100) * Average_GC / 0.99
quietly gen bottom99_UndProf_avg = (1 - t1_share_UndProf / 100) * avg_UndProf / 0.99
//Get P99-PP.9
quietly gen next09_avg = (t1_topsh_adj / 100 - t01_topsh_adj / 100) * ///
	Average_GC/0.009
quietly gen next09_UndProf_avg=(t1_share_UndProf/100-t01_share_UndProf/100) * ///
	avg_UndProf/0.009

//Get real (base 2016)
ds *topavg_adj
local avg_vars "`r(varlist)'"
ds *_UndProf_avg *_avg_UndProf
local undprof_avg_vars "`r(varlist)'"
local avg_vars "`avg_vars' `undprof_avg_vars' bottom99_avg next09_avg Average_GC"
di "`avg_vars'"

foreach v in `avg_vars'{
	//convert to pesos before 1973 (from escudos)
	quietly replace `v' = `v' / 1000 if Year <= 1973
	//deflate
	quietly gen `v'_rclp = `v' / `cpi' * 100
	//index base 1964
	if (inlist("`v'","bottom99_UndProf_avg","next09_UndProf_avg","t01_avg_UndProf")) {
		local startyr = 1990
	}
	else {
		local startyr = 1964
	}
	quietly sum `v'_rclp if Year == `startyr'
	quietly gen `v'_idx = `v'_rclp / r(max) * 100
}

//Check growth btw 1973-1981

quietly gen gdp_rclp = gdp / `cpi' * 100
quietly replace gdp_rclp = gdp_rclp * 1000 if Year > 1973
foreach v in "t1_topavg_adj" "t01_topavg_adj" "next09_avg" "bottom99_avg"  "gdp"{
	quietly sum `v'_rclp if Year == 1973
	quietly gen `v'_idx_73 = `v'_rclp / r(max) * 100
}

graph twoway (connected t1_topavg_adj_idx_73 gdp_idx_73 Year) ///
	if Year >= 1973 & Year <= 1981

graph twoway (connected t01_topavg_adj_idx Year, color(maroon) msymbol(D) mcolor(maroon) msize(small)) ///
	(connected next09_avg_idx Year, color(edkblue) msymbol(T) mcolor(edkblue) mfcolor(white) msize(small)) ///
	(connected bottom99_avg_idx Year if Year<=2017, color(gs8) msymbol(S) mcolor(gs8) mfcolor(white) msize(small)) ///
	, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
	ytitle("Real income growth index (100 in 1964)") xtitle("") ///
	xlabel(1960(10)2020, labsize(medium) grid labels) ///
	ylabel(100(200)1300, labsize(medium) angle(horizontal) format(%6.0g) grid) ///
	scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
	text(400 1986  "Top 0.1%", color(maroon)) ///
	text(650 1986  "P99-P99.9", color(edkblue)) ///
	text(200 1986  "Bottom 99%", color(gs8)) ///
	legend(off)
	quietly graph export "Figures/avginc1964_`totinc'.pdf", replace

graph twoway (connected t01_avg_UndProf_idx Year, color(maroon) ///
	msymbol(D) mcolor(maroon) msize(small)) ///
	(connected next09_UndProf_avg_idx Year, ///
	color(edkblue) msymbol(T) mcolor(edkblue) mfcolor(white) msize(small)) ///
	(connected bottom99_UndProf_avg_idx Year, color(gs8) msymbol(S) mcolor(gs8) mfcolor(white) msize(small)) ///
	if Year>1990 ///
	, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
	ytitle("Real income growth index (100 in 1990)") xtitle("") ///
	xlabel(1990(10)2023, labsize(medium) grid labels) ///
	ylabel(100(50)300, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
	scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
	text(275 2020  "Top 0.1%", color(maroon)) ///
	text(255 2020  "P99-P99.9", color(edkblue)) ///
	text(295 2020  "Bottom 99%", color(gs8)) ///
	legend(off)	
	quietly graph export "Figures/avginc1990_`totinc'.pdf", replace

// 9. INTERNATIONAL COMPARISONS (WID.WORLD DATA)-----------------------------//
preserve
	//Prepare Chilean data
	foreach per in "p99p100" "p99.9p100" {
		if ("`per'"=="p99p100") {
			local topg "t1"
			tempfile randomname 
			quietly save `randomname',replace
		}
		else {
			local topg "t01"
			quietly use `randomname', clear
		}
		quietly keep Country Year `topg'_share_UndProf `topg'_topsh_adj 
		quietly rename Country country
		quietly rename Year year
		quietly rename `topg'_topsh_adj `topg'1
		quietly rename `topg'_share_UndProf `topg'2
		drop if missing(country)
		quietly reshape long `topg', i(year) j(type)
		quietly drop if missing(`topg')
		quietly replace country="CL" if type==1
		quietly replace country="CL_UP" if type==2
		sort type year
		tempfile tf_aux
		quietly save `tf_aux', replace
		//locals
		local countries_latam "CO AR UY BR"
		local countries_dev "US SE"
		local countries "`countries_latam' `countries_dev'"
		local variables "sfiinc"

		//Retrieve WID data
		wid, indicators (`variables') areas (`countries') ///
			perc(`per') ages(992) population() clear
		rename value `topg'
			quietly replace variable = subinstr(variable, "sfiinc992", "",.) 
		//Select pertinent series
		egen ctry_var=concat(country variable)
		quietly drop if ctry_var=="USt" | year<1964
		keep country ctry_var year `topg'
		
		quietly append using `tf_aux'
		quietly keep country year `topg'
		quietly replace `topg'=`topg'*100 if `topg'<1
		quietly sort country year
		
		if ("`per'"=="p99p100") {
			graph twoway ///
			(connected `topg' year if country=="BR" ///
			, lcolor(olive_teal) mcolor(olive_teal) msize(small) msymbol(O) mfcolor(white)) ///
			(connected `topg' year if country=="CO" ///
			, lcolor(sand) mcolor(sand) msize(small) msymbol(S) mfcolor(sand*0.5)) ///
			(connected `topg' year if country=="AR" ///
			, lcolor(ltblue) mcolor(ltblue) msize(small) msymbol(D) mfcolor(white)) ///
			(connected `topg' year if country=="UY" ///
			, lcolor(edkblue) mcolor(edkblue) msize(small) msymbol(O) mfcolor(white) lpattern(dot)) ///
			(connected `topg' year if country=="CL_UP" ///
			, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(black)) ///
			(connected `topg' year if country=="CL" ///
			, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(white)) ///
			if year>=1990 & year <=2017 ///
			,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Top 1% income share (%)") xtitle("") ///		
			xlabel(1990(5)2020, labsize(medium) grid labels) ///
			ylabel(0(5)30, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(20 2018  "CHL*", color(black)) ///
			text(15 2018  "CHL", color(black)) ///
			text(11 2001  "ARG", color(ltblue)) ///
			text(18 2007  "COL", color(sand)) ///
			text(13 2010  "URY", color(edkblue)) ///
			text(23 2018 "BRA", color(olive_teal)) ///
			legend(off)	
			quietly graph export "Figures/comp_latam_`totinc'.pdf", replace
			
			graph twoway ///
			(connected `topg' year if country=="US", ///
			lcolor(olive_teal) mcolor(olive_teal) msize(small) msymbol(O) mfcolor(white)) ///
			(connected `topg' year if country=="SE", lcolor(sand) mcolor(sand) msize(small) msymbol(S) mfcolor(sand*0.5)) ///
			(connected `topg' year if country=="CL_UP", lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(black)) ///
			(connected `topg' year if country=="CL", lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(white) lpattern(dash)) ///
			if year <= 2017 ///
			,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Top 1% income share (%)") xtitle("") ///
			xlabel(1960(10)2020, labsize(medium) grid labels) ///
			ylabel(0(5)30, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(26 2010  "CHL*", color(black)) ///
			text(19 1981  "CHL", color(black)) ///
			text(8 2017  "SWE", color(sand)) ///
			text(15 1999  "USA", color(olive_teal)) ///
			legend(off)	
			quietly graph export "Figures/comp_dev_`totinc'.pdf", replace
		}
		if ("`per'"=="p99.9p100"){
			graph twoway ///
			(connected `topg' year if country=="BR" ///
				, lcolor(olive_teal) mcolor(olive_teal) msize(small) msymbol(O) mfcolor(white)) ///
			(connected `topg' year if country=="CO" ///
				, lcolor(sand) mcolor(sand) msize(small) msymbol(S) mfcolor(sand*0.5)) ///
			(connected `topg' year if country=="AR" ///
				, lcolor(ltblue) mcolor(ltblue) msize(small) msymbol(D) mfcolor(white)) ///
			(connected `topg' year if country=="UY" ///
				, lcolor(edkblue) mcolor(edkblue) msize(small) msymbol(O) mfcolor(white) lpattern(dot)) ///
			(connected `topg' year if country=="CL_UP" ///
				, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(black)) ///
			(connected `topg' year if country=="CL" ///
				, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(white)) ///
			if year>=1990 & year <=2017 ///
			,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Top 0.1% income share (%)") xtitle("") ///
			xlabel(1990(5)2020, labsize(medium) grid labels) ///
			ylabel(0(5)15, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(8.2 2018  "CHL*", color(black)) ///
			text(4 2018  "CHL", color(black)) ///
			text(5 2004  "ARG", color(ltblue)) ///
			text(9 1993  "COL", color(sand)) ///
			text(6 2010  "URY", color(edkblue)) ///
			text(11 2018 "BRA", color(olive_teal)) ///
			legend(off)	
			quietly graph export "Figures/comp_latam_t01_`totinc'.pdf", replace
		}
	}
restore

//10. COMPARE WITH SURVEYS ---------------------------------------------------//
preserve
	quietly import excel "Data/surveys.xlsx" , ///
		sheet("Data") clear firstrow
	tempfile tf_svy
	quietly save `tf_svy', replace
restore

quietly merge 1:1 Year using `tf_svy', nogenerate

foreach v in "t1_topavg_svypost" "t1_topavg_svypre" {
	quietly replace `v'=`v'/`usd_ppa_2013'/1000
}
local usd_ppa_2013=409.1
quietly gen t1_topavg_ppa = t1_topavg_adj_rclp / `usd_ppa_2013' / 1000

//Generate index base 100 in 1990 for P50 (CASEN)
quietly sum p50_thr_svypost if Year == 1990
quietly gen p50_casen_idx = p50_thr_svypost / r(sum) * 100

//Graph top shares
graph twoway ///
	(connected t1_topsh_svypost Year, lcolor(maroon) mcolor(maroon) msize(small) msymbol(O) mfcolor(maroon*0.5)) ///
	(connected t1_topsh_svypre Year, lcolor(sand) mcolor(sand) msize(small) msymbol(S) mfcolor(sand*0.5)) ///
	(connected t1_topsh_adj Year, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(white)) ///
	if Year>=1990 & Year <= 2017 ///
	,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
	ytitle("Top 1% income share (%)") xtitle("") ///
	xlabel(1990(5)2017, labsize(medium) grid labels) ///
	ylabel(10(2)20, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
	scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
	text(17 2016  "Fiscal", color(black)) ///
	text(17.5 2009  "CASEN" "pre-tax", color(sand)) ///
	text(12 2015  "CASEN" "post-tax", color(maroon)) ///
	legend(off)	
	quietly graph export "Figures/comp_topsh_svy_`totinc'.pdf", replace

//Graph top averages	
graph twoway ///
	(connected t1_topavg_svypost Year, lcolor(maroon) mcolor(maroon) msize(small) msymbol(O) mfcolor(maroon*0.5)) ///
	(connected t1_topavg_svypre Year, lcolor(sand) mcolor(sand) msize(small) msymbol(S) mfcolor(sand*0.5)) ///
	(connected t1_topavg_ppa Year, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(white)) ///
	if Year>=1990 & Year <= 2017 ///
	,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
	ytitle("Real average income (thsd. USD PPP 2013)") xtitle("") ///
	xlabel(1990(5)2017, labsize(medium) grid labels) ///
	ylabel(0(50)270, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
	scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
	text(260 2015  "Fiscal", color(black)) ///
	text(160 2015  "CASEN" "pre-tax", color(sand)) ///
	text(100 2015  "CASEN" "post-tax", color(maroon)) ///
	legend(off)	
	quietly graph export "Figures/comp_topavg_svy_`totinc'.pdf", replace	
	
//Graph P50	
graph twoway ///
	(connected p50_casen_idx Year, lcolor(maroon) mcolor(maroon) msize(small) msymbol(O) mfcolor(maroon*0.5)) ///
	(connected t01_avg_UndProf_idx Year, lcolor(black) mcolor(black) msize(small) msymbol(T) mfcolor(black*0.5)) ///
	if Year >= 1990 & Year <= 2017 ///
	,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
	ytitle("Real income growth index (100 in 1990)") xtitle("") ///
	xlabel(1990(5)2017, labsize(medium) grid labels) ///
	ylabel(100(50)300, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
	scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
	text(220 2015.5  "Top 0.1%""", color(black)) ///
	text(180 2000  "Median" "CASEN", color(maroon)) ///
	legend(off)	
	quietly graph export "Figures/comp50_svy_`totinc'.pdf", replace
	
//Compare topavg with survey	
gen v1=t1_topavg_ppa/t1_topavg_svypost
gen v2=t1_topavg_ppa/t1_topavg_svypre	
sum v1 v2 if Year>=2009

if ("`totinc'"=="fiscal"){
	quietly levelsof Year, local(allyrs)
	foreach yr in `allyrs' {
		quietly sum t1_topsh_adj if Year==`yr'
		local t1_`yr'=r(max)
	}
}

// 10. COMPARE WITH FISCAL INCOME WITH SSC ------------------------------------//
	//Save fiscal results for comparison
	if ("`totinc'"=="fiscal"){
		preserve 
			keep Year t1_topsh_adj
			quietly rename t1_topsh_adj t1_topsh_fiscal
			tempfile tf_fiscal
			quietly save `tf_fiscal', replace
		restore
	}

	if ("`totinc'"=="fiscal_ss"){
		quietly merge 1:1 Year using `tf_fiscal', nogenerate 
	}
	sort Year

	if ("`totinc'"=="fiscal_ss"){
		graph twoway (connected t1_topsh_fiscal Year, lcolor(edkblue) ///
			mcolor(edkblue) msymbol(T) msize(vsmall)) ///
			(connected t1_topsh_adj Year, lcolor(maroon) ///
			mfcolor(white) mcolor(maroon) msize(vsmall) msymbol(S)) ///
			(scatter FJ_t1_Rlzd Year, msymbol(O) mcolor(sand) msize(medium) mfcolor(white)) ///
			if Year <= 2017 ///
			, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Share of total income (%)") xtitle("") ///
			xlabel(1960(10)2020, labsize(medium) grid labels) ///
			ylabel(0(2)20, labsize(medium) angle(horizontal) format(%2.1g) grid labels) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(19 1985  "Fiscal", color(edkblue)) ///
			text(14 1985  "Fiscal" "(SSC)", color(maroon)) ///
			text(17 2008  "F&J", color(sand)) ///
			legend(off)
		quietly graph export "Figures/topsh_`totinc'_comp.pdf", replace 
	}
//Save some results for Part 2
preserve
	quietly gen totinc="`totinc'"
	quietly keep totinc Year *_adj UndProf `cti'
	quietly export excel using ///
		"Data/someresults.xlsx" ///
		, firstrow(variables) sheet("`totinc'") sheetreplace			
restore		
}
	
// 11. OTHER GRAPHS ---------------------------------------------------------//	
//preserve
	//Top Marginal Tax rates
	quietly import excel using "Data/tmtr.xlsx", ///
		sheet("Data") firstrow clear 
		
	graph twoway (connected top_mtr year, lcolor(black) ///
		msize(small) mcolor(black) mfcolor(white) msymbol(D)) ///	
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Top Marginal Tax Rate (%)") xtitle("") ///
		xlabel(1960(10)2020, labsize(medium) grid labels) ///
		ylabel(30(10)70, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		legend(off)	
		quietly graph export "Figures/tmtr.pdf", replace	
	
	quietly gen t1=.
	quietly levelsof year, local(allyrs2)
	foreach yr in `allyrs2' {
		quietly replace t1 = `t1_`yr'' if year==`yr'
	}
		
	graph twoway (scatter t1 top_mtr if year<=1980, mcolor(edkblue) mfcolor(white) msize(small)) ///
		(scatter t1 top_mtr if year>=1980, mcolor(maroon) mfcolor(white) msize(small)) ///
		(lfit t1 top_mtr, lcolor(black)) ///
		(lfit t1 top_mtr if year>=1980, lcolor(maroon) lpattern(dot)) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Top 1% share of fiscal income (%)") xtitle("Top marginal tax rate (%)") ///
		xlabel(30(5)65, labsize(medium) grid labels) ///
		ylabel(8(2)20, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		legend(off)
		quietly graph export "Figures/reg_tmtr.pdf", replace

	//Evasion rates in literature	
	cd "~/Dropbox/ForGitHub/Top_incomes_Chile"	
	quietly import excel using "Data/evasion.xlsx", ///
		sheet("Data") firstrow clear 

	graph twoway (scatter Foxley year, msize(small) mcolor(black) msymbol(T)) ///	
		(connected Serra year, lcolor(black) lpattern(dash) msize(small) mcolor(black) msymbol(O) mfcolor(black*0.3)) ///
		(connected Yanez year, lcolor(black) msize(small) mcolor(black) msymbol(S) mfcolor(black)) ///
		(connected Jorratt year, lcolor(black) msize(small) mcolor(black) msymbol(D) mfcolor(white)) ///
		(connected Figueroa year, lcolor(black) lpattern(dot) msize(small) mcolor(black) msymbol(T) mfcolor(black*0.3)) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Evasion Rate, estimate (%)") xtitle("") ///
		xlabel(1960(10)2020, labsize(medium) grid labels) ///
		ylabel(0(10)70, labsize(medium) angle(horizontal) format(%5.0g) grid) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(67 1969  "Foxley et " "al. (1980)", color(black)) ///
		text(50 2003  "Yañez" "(2015)", color(black)) ///
		text(66 1990  "Serra" "(2000)", color(black)) ///
		text(16 2003  "Jorratt" "(2013)", color(black)) ///
		text(16 2015  "López et" "al. (2016)", color(black)) ///
		legend(off)	
		quietly graph export "Figures/evasion.pdf", replace	
//restore
 	
