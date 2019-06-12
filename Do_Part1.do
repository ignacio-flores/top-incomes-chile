
************************************************************************
//																	  //
//							TOP INCOMES IN CHILE					  //
//															 		  //
//									Part I							  //
//																	  //
************************************************************************

clear all
cd "~/Dropbox/GitHub/Top_incomes_Chile"

// 0. LOCALS ---------------------------------------------------------------//

//local gc_vars "year tramo tramo_clp tasa Y_GC N_GC Y_GC_gross"
local ti_vars "FISCALGDP FISCAL_SSGDP"
local inc_concepts "fiscal fiscal_ss"
local totinc_main "fiscal"
local totinc_main2 "FISCAL_SSGDP"
local taxtype "C GC"
local cpi "cpi_base2016"
local usd_ppa_2013 = 409.1

local consolidados "tab_consolidados_2005_2017"
local gcomp "tab_gc_1963_1981 tab_gc_1990_1999 tab_gc_1998_2009"
local alltabs "`consolidados' `gcomp'"

local groups "t10 t1 t01 t001"
local start_pos=91
local start_freq=0.1
foreach g in `groups' {
		local `g'_pos=`start_pos'
		local `g'_freq=`start_freq'
		local start_pos=`start_pos'+9
		local start_freq=`start_freq'/10
		di "Info for `g' in _n = [``g'_pos'] (in annual sheets)" ///
			", it gathers ``g'_freq' of adult population"
}

tempfile tf tf_na tf_ci tf_ts tf_fi

foreach totinc in `inc_concepts' {

// 1. PREPARE DATA -----------------------------------------------------//

	//Merge tax declarations (tabulated data) 
	local iter=1
	foreach t in `alltabs' {
		quietly import excel "Data/`t'.xlsx", sheet(tab) clear firstrow
		if ("`t'"=="tab_gc_1998_2009") {
			quietly drop if year<2000 & year!=1998	 
			quietly drop if year>2004 
		} 
		if (`iter'==0){
			quietly merge m:m year using `tf', nogenerate
			quietly keep if !missing(year)
			quietly sort year tramo
		}
		quietly save `tf', replace
		local iter=0
	}

	//National Accounts data (for total income control)
	quietly import excel "Data/na.xls" ///
		,sheet("TOT-FiscInc") clear firstrow
	quietly drop if year=="AVG"
	quietly destring year, replace	
	quietly keep  year `ti_vars'
	quietly save `tf_na', replace 

	//World Bank data (dor GDP, CPI and POP)
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
		tempfile tf_`f'	
		quietly save `tf_`f'', replace	
	}

	//UTA  (for tax brackets)
	quietly import excel "Data/uta.xlsx" ///
		,sheet("uta") clear firstrow 
	quietly keep year uta	
	quietly drop if missing(uta)
	tempfile tf_uta
	quietly save `tf_uta', replace	

	//Marginal Tax rates  (only for selected years)
	quietly import excel "Data/tablas.xlsx" ///
		,sheet("MTR") clear firstrow
	quietly replace uta=uta*10	
	local mtr_years "1973 1980 1990 1996 2003 2013"	
	foreach y in `mtr_years'{
		preserve 
			quietly keep uta v`y'
			quietly drop if missing(v`y')
			quietly levelsof uta, local(uta_`y')
			foreach l in `uta_`y''{
				quietly sum v`y' if uta==`l'
				local mtr_`l'_`y'=r(max)
			}
		restore
	}	

	//Merge everything
	quietly use `tf', clear
	foreach db in "na" "gdp" "totpop" "uta" "cpi" {
		quietly merge m:1 year using `tf_`db'', nogenerate 
	}

	//Fill missing variables in some obs
	quietly replace UTA=uta if !missing(uta)
	quietly replace tramo=tramo_clp/UTA ///
		if missing(tramo) & !missing(tramo_clp)
	quietly rename tasa MTR
	tempvar aux1
	quietly gen `aux1'=1 if missing(MTR)
	foreach y in `mtr_years' {
		foreach l in `uta_`y'' {
			local l2=`l'/10
			quietly replace MTR=`mtr_`l'_`y'' ///
				if `aux1'==1 & year>=`y' & tramo>=`l2'
		}
	}

	//Put observations of same year in same unit 
	foreach y in `taxtype' {
		foreach var in "Y" "I" {
			//tabulations in billions
			quietly replace `var'_`y'=`var'_`y' * 1000000 ///
				if year >= 2005
			}
		}
		//tabulations in thousands
		foreach var in "Y" "I" {
			quietly replace `var'_GC = `var'_GC * 1000 ///
				if year < 1990
		}	
		foreach inc in "Y_GC_gross" "t_tax" "ipc_tax" {
			quietly replace `inc' = `inc' * 1000 if year < 1990
	}

// 2. DATA TRANSFORMATION --------------------------------------------//

// 2.1 Get cumulative distributions
	foreach y in `taxtype' {
		quietly gsort year -tramo -tramo_clp
		quietly gen freq_`y'=N_`y'/totpop
		by year: gen cumfreq_`y'=sum(freq_`y')
		by year: gen cum_decl_`y'=sum(Y_`y')
		by year: gen cum_decl_`y'_gdp=sum(Y_`y')/gdp
		quietly sort year tramo
		quietly egen `y'_decl=sum(Y_`y'), by(year)
		quietly egen `y'_decl_gdp=max(cum_decl_`y'_gdp) ///
			,by(year)
		quietly egen `y'_n_decl=sum(N_`y'), by(year)
	}

// 2.2 Total Fiscal income control with Social Sec. Contributions (SSC)
	//(scaled to 2009 of Fairfield & Jorratt (2015))
	quietly sum FISCAL_SSGDP if year == 2009
	local scalingf = 0.501 / r(max)
	quietly gen fiscal_ss_TI = FISCAL_SSGDP * `scalingf'
	quietly sum fiscal_ss_TI
	quietly replace fiscal_ss_TI = r(mean) ///
		if missing(fiscal_ss_TI)
	quietly label var fiscal_ss_TI ///
		"Fiscal Inc. (incl. SSC) from NA, scaled to F&J2015 level of 2009"	

// 2.3 Create control income total with fiscal income definition (w/o SSC)
	//Use data from national accounts directly
	quietly gen fiscal_TI = FISCALGDP
	quietly sum FISCALGDP if !missing(FISCALGDP)
	quietly replace fiscal_TI = r(mean) ///
		if missing(fiscal_TI)

// Estimate average tax rate for top 1%
preserve 
	quietly keep year I_* tramo* s_vital MTR Y_* cumfreq* freq*
	quietly keep if !missing(tramo)
	quietly levelsof year, local(years)
	foreach t in "GC" "C" {
		quietly gen istop_`t' = 1 if cumfreq_`t' <= 0.01 & !missing(Y_`t' )
		quietly levelsof year if !missing(I_`t'), local(yrs)
		quietly gen aux3_`t' = .
		quietly gen Y_new_`t'=Y_`t'
		quietly gen I_new_`t'=I_`t'
		foreach y in `yrs' {
			quietly sum cumfreq_`t' if year == `y' & missing(istop_`t' )
			quietly replace istop_`t' = 2 if cumfreq_`t' == r(min) & year == `y'
			quietly sum freq_`t' if istop_`t' == 1 & year == `y'
			quietly replace aux3_`t' = (0.01 - r(sum)) / freq_`t' ///
				if year == `y' & istop_`t' == 2
			quietly replace Y_new_`t'= Y_new_`t' * aux3_`t' if istop_`t' == 2	
			quietly replace I_new_`t'= I_new_`t' * aux3_`t' if istop_`t' == 2	
		}
	}

	quietly collapse (sum) Y_new* I_new*  if istop_GC == 1 | istop_GC ==2, by(year)
	quietly gen t1_etr_GC = I_new_GC / Y_new_GC * 100
	quietly replace t1_etr_GC = . if t1_etr_GC == 0
	graph twoway (connected t1_etr_GC year, mstyle (O) mcolor(edkblue) ///
		msize(small) mfcolor(white)) if year != 1967
restore
	
// 2.4 Other
	//Allocate non-declarants (nº) to 1st bracket
	foreach y in `taxtype'{
		quietly gen n_miss_`y'=totpop-`y'_n_decl
		quietly replace N_`y'=N_`y'+n_miss_`y' if tramo==0
		quietly replace cumfreq_`y'=0 if tramo==0
	}
		
	//Clean & Sort
	quietly drop if year<1964
	quietly drop if missing(Y_GC)
	quietly sort year tramo tramo_clp	
	
// 3.COMPARE TOTAL INCOME DEFINITIONS --------------------------------------//

	//Bring survey totals
	preserve
		quietly import excel "Data/surveys.xlsx" , ///
			sheet("Data") clear firstrow
		tempfile tf_svy
		quietly rename Year year
		quietly keep year tot_svy_gdp tot_svy2_gdp
		quietly keep if !missing(tot_svy_gdp)
		quietly save `tf_svy', replace
	restore

	quietly merge m:1 year using `tf_svy', nogenerate

	preserve
		quietly collapse (firstnm) fiscal_ss_TI fiscal_TI FISCAL_SSGDP ///
			FISCALGDP gdp cum_decl_C_gdp cum_decl_GC_gdp `cpi' ///
			totpop tot_svy_gdp tot_svy2_gdp ///
			,by(year)
	
		//graph twoway (line TAXABLEGDP year) (line FISCALGDP year, yaxis(2)) if year>=1996
	
		quietly gen gdp_ppa=gdp/totpop/`cpi'*100/`usd_ppa_2013'/1000
		quietly replace gdp_ppa=gdp_ppa/1000 if year<=1973
		quietly label var gdp_ppa ///
			"Real GDP in USD PPP 2013, using CPI as deflator" 
		quietly ds year gdp gdp_ppa `cpi' totpop, not	
		local vars "`r(varlist)'"
		foreach v in `vars' {
			quietly gen `v'_ppa=`v'*gdp_ppa
			quietly replace `v'=`v'*100
		}
		quietly replace cum_decl_C_gdp_ppa=. if cum_decl_C_gdp_ppa==0
		quietly replace cum_decl_C_gdp=. if cum_decl_C_gdp==0
		
		//Graph averages
		graph twoway (line gdp_ppa year, lcolor(black) lpattern(dash)) ///
		(line fiscal_TI_ppa year, lcolor(black)) ///
		(line cum_decl_C_gdp_ppa year) ///
		(line cum_decl_GC_gdp_ppa year) ///
		(line tot_svy_gdp_ppa year) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("USD PPP 2013 per adult (thousands)") xtitle("") ///
		xlabel(1960(10)2020, labsize(medium) grid labels) ///
		ylabel(0(5)35, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(29 2016 "GDP", color(gs5)) ///
		text(19 2016 "Fiscal" "NA", color(gs5)) ///
		text(5 2016 "Global" "tax", color(gs5)) ///
		text(14 2016 "Tot. tax", color(gs5)) ///
		text(3.5 1985 "CASEN" "post-tax", color(gs5)) ///
		legend(off)
		quietly graph export "Figures/totincs_avg.pdf", replace
		
		//Graph average & CPI
		graph twoway ///
		(connected `cpi' year, lcolor(black) lpattern(dot) msymbol(D) mcolor(black) mfcolor(white) yscale(log) msize(small)) ///
		(connected fiscal_TI_ppa year, lcolor(black) msymbol(O) mcolor(black) msize(small) mfcolor(black*0.2) yaxis(2)) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Consumer Price Index, 100 in 2016 (log)") ///
		ytitle("Average Real Income (thsd. USD PPP 2013)", axis(2)) ///
		xtitle("") ///
		xlabel(1960(10)2020, labsize(medium) grid labels) ///
		ylabel(0.0001 0.001 0.01 0.1 1 10 100, labsize(medium) angle(horizontal) nogrid labels) ///
		ylabel(0(5)20, labsize(medium) angle(horizontal) format(%2.0f) nogrid labels axis(2)) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(10 1975 "CPI" "(left)", color(gs5)) ///
		text(1 2000 "Average" "(right)", color(gs5)) ///
		legend(off)
		quietly graph export "Figures/avgcpi.pdf", replace
	
		//Graph shares
		graph twoway ///
		(line fiscal_TI year, lcolor(black)) ///
		(line fiscal_ss_TI year, lcolor(black) lpattern(dot)) ///
		(line cum_decl_C_gdp year) ///
		(line cum_decl_GC_gdp year) ///
		(line tot_svy_gdp year) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Share of GDP (%)") xtitle("") ///
		xlabel(1960(10)2020, labsize(medium) grid labels) ///
		ylabel(0(10)70, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(47 1965 "Fiscal", color(gs5)) ///
		text(61 1965 "Fiscal" "SSC", color(gs5)) ///
		text(5 1965 "Global" "tax", color(gs5)) ///
		text(36 1985 "CASEN" "post-tax", color(gs5)) ///
		text(44 2017 "Total" "decl.", color(gs5)) ///
		legend(off)
		quietly graph export "Figures/totincs_topsh.pdf", replace
		
		//Compare totals with survey
		collapse (firstnm) fiscal_TI tot_svy_gdp tot_svy2_gdp, by(year)	
		gen v1=fiscal_TI/tot_svy_gdp
		gen v2=fiscal_TI/tot_svy2_gdp
		sum v1 v2 if year>=2009	
		
	restore

	//Save
	quietly save `tf', replace

// 4. DEDUCTIONS ----------------------------------------------------------//

	//Harmonize definition of gross income
	quietly keep if !missing(Y_GC_gross)
	quietly replace Y_GC_gross=Y_GC_gross+ipc_tax+t_tax ///
			if year!=1973
	//drop first bracket (bc it includes all non-filers)
	quietly drop if tramo==0
	quietly levelsof year, local(years)
	//Define a minimum size for brackets
	local numb=0.0005
	quietly count if (freq_GC<`numb')
		while (r(N) > 0) {
			quietly gsort year cumfreq_GC
			tempvar bracket newbracket queue
			by year: generate `queue' = sum(freq_GC<`numb')
			by year: generate `bracket'=_n
			// We choose to group the bracket with the one just below
			by year: generate `newbracket' = `bracket'[_n + 1] ///
				if (freq_GC<`numb')
			// We have to make an exception for the last bracket,
			//which we group with the one just above
			by year: replace `newbracket' = `bracket'[_n - 1] ///
				if (freq_GC<`numb') & (_n == _N)
			by year: replace `bracket' = `newbracket' ///
				if (`queue' == 1) & (freq_GC<`numb')
			quietly collapse (sum) freq_GC Y_GC Y_GC_gross ipc_tax t_tax (max) ///
			tramo cumfreq_GC ,by(year `bracket')
			quietly count if (freq_GC<`numb')		
		}	
	//Sort and define percentiles
	quietly gsort year -cumfreq_GC
	quietly gen p=(1-cumfreq_GC)*100
	
	//Get deductions for Top groups
	quietly gen taxdeds = t_tax + ipc_tax if year==1996 | year==1999
	quietly replace taxdeds = Y_GC_gross - Y_GC if year==1973
	quietly gen allowdeds = Y_GC_gross - Y_GC - taxdeds ///
		if year==1996 | year==1999
	foreach yr in `years' {
		local list "Y_GC Y_GC_gross taxdeds allowdeds"
		if ("`yr'" == "1973") {
			local list "Y_GC Y_GC_gross taxdeds"
		}
		foreach g in `groups'{
			foreach x in `list' {
				local perc_`g' = round((1-``g'_freq') * 100 , 0.001)
				quietly sum `x' if year == `yr' &  p >= `perc_`g''
				local tot_`x'_`yr'_`g' = r(sum)
				//di "Hola `x' `yr', `g': `tot_`x'_`yr'_`g''"
				if (`tot_`x'_`yr'_`g''==0 & "`g'"=="t001") {
					quietly sum p if year == `yr' 
					local max_p_`yr' = r(max)
					quietly sum `x' if year == `yr' &  p >= `max_p_`yr''
					local tot_`x'_`yr'_`g' = r(sum)
					//di "Chao `x' `yr' `g': `tot_`x'_`yr'_`g''"
				}

			}	
			local avg_taxdeds_`yr'_`g' = ///
				`tot_taxdeds_`yr'_`g'' / `tot_Y_GC_gross_`yr'_`g'' * 100
			if ("`yr'"!="1973") {
				local avg_allowdeds_`yr'_`g' = ///
					`tot_allowdeds_`yr'_`g'' / `tot_Y_GC_gross_`yr'_`g'' * 100
				di "AVG allowdeds `yr', `g': `avg_allowdeds_`yr'_`g''"	
			}
		}
	}

	// 4.1. DEDUCTIONS FOR TAXES ALREADY PAID (TERRITORIAL+CATEGORIES) ----------//	
		
	//Deductions for taxes paid (total)
	quietly gen tax2=(t_tax+ipc_tax)/Y_GC_gross*100 if year==1996 | year==1999
	quietly replace tax2=(Y_GC_gross-Y_GC)/Y_GC_gross*100 if year==1973
		
	graph twoway (connected tax2 p if year==1996 ///
		,lcolor(edkblue) mcolor(edkblue) msize(vsmall)) ///
		(connected tax2 p if year==1999,lcolor(maroon) mcolor(maroon) msize(vsmall)) ///
		(connected tax2 p if year==1973,lcolor(sand) mcolor(sand) msize(vsmall)) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Deduction of paid-taxes (% Fiscal Income)") xtitle("Percentile") ///
		yline(`avg_taxdeds_1973_t1', lcolor(sand) lpattern(dot)) ///
		yline(`avg_taxdeds_1996_t1', lcolor(edkblue) lpattern(dot)) ///
		yline(`avg_taxdeds_1999_t1', lcolor(maroon) lpattern(dot)) ///
		xlabel(93(1)100, labsize(medium) grid labels) ///
		ylabel(0(2)8, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(3.5 94  "1996", color(edkblue)) text(2.3 94  "1999", color(maroon)) ///
		text(3 99.5 "1973", color(sand)) ///
		legend(off)
		quietly graph export "Figures/taxdeds.pdf", replace 	
		
	//Study decomposition of deductions for taxes paid (only 1990's)
	quietly gen tax1=t_tax/Y_GC_gross*100

	foreach yr in "1996" "1999" {
		graph twoway (area tax2 p, color(edkblue)) (area tax1 p, color(maroon)) ///
			if year==`yr' ///
			,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
			ytitle("Deductions (% of Fiscal Income)") xtitle("Percentile") ///
			xlabel(93(1)100, labsize(medium) grid labels) ///
			ylabel(0(2)8, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
			scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
			text(0.3 96.5  "Territorial Tax", color(gs15)) ///
			text(1.5 96.5  "1st category tax", color(gs15)) ///
			legend(off)
			quietly graph export "Figures/dec_taxded`yr'.pdf", replace 
	}
		
	// 4.2. ALLOWANCES -----------------------------------------------------------//

	quietly gen allow=(Y_GC_gross-Y_GC-t_tax-ipc_tax)/Y_GC_gross*100

	graph twoway (connected allow p if year==1996, lcolor(edkblue) mcolor(edkblue) msize(vsmall)) ///
		(connected allow p if year==1999, lcolor(maroon) mcolor(maroon) msize(vsmall)) ///
		,graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
		ytitle("Deductions (% of Fiscal Income)") xtitle("Percentile") ///
		yline(`avg_allowdeds_1996_t1', lcolor(edkblue) lpattern(dot)) ///
		yline(`avg_allowdeds_1999_t1', lcolor(maroon) lpattern(dot)) ///
		xlabel(93(1)100, labsize(medium) grid labels) ///
		ylabel(0(2)8, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
		scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
		text(0.3 96.5  "1996", color(edkblue)) text(1.5 96.5  "1999", color(maroon)) ///
		legend(off)
		quietly graph export "Figures/allow.pdf", replace 

	// 5. DECLARED INCOME --------------------------------------------------//

	//Declared and taxable income over CTI
	use `tf', clear
	quietly sort year tramo tramo_clp
	quietly gen taxable=. 
	quietly replace taxable=1 if MTR!=0
	foreach y in `taxtype'{
		quietly gen `y'_pct_decl=(`y'_decl_gdp/`totinc_main'_TI)*100
		quietly gen `y'_pct_taxd=.
		quietly gen `y'_taxd_gdp=.
		quietly levelsof year, local(allyears)
		foreach yr in `allyears' {
			quietly sum Y_`y' if year==`yr' & taxable==1 
			quietly replace `y'_taxd_gdp=r(sum)/gdp if year==`yr'
		}
		quietly replace `y'_pct_taxd=`y'_taxd_gdp/`totinc_main'_TI*100 
		quietly replace `y'_pct_taxd=. if `y'_pct_taxd==0
		//impute missing income to 1st bracket
		quietly gen y_missing_`y'=`totinc'_TI*gdp-`y'_decl
		quietly replace Y_`y'=Y_`y'+y_missing_`y' if tramo==0
		sort year tramo	
	quietly save `tf', replace

	preserve
		if ("`totinc'" == "`totinc_main'") {
			quietly collapse (max) `y'_pct_decl `y'_pct_taxd, by(year)
				if ("`y'"=="C") {
					graph twoway (line `y'_pct_decl year, lcolor(edkblue)) ///
						(line  `y'_pct_taxd year, lcolor(maroon)) ///
						if year>=2005 ///
						, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
						ytitle("Share of Control Income Total (%)") xtitle("") ///
						xlabel(2005(2)2017, labsize(medium) grid labels) ///
						ylabel(0(20)100, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
						scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
						text(88 2015  "Declared", color(edkblue)) ///
						text(61 2015  "Taxable", color(maroon)) ///
						legend(off)
						quietly graph export "Figures/inc_`y'.pdf", replace 
				}
				if ("`y'"=="GC") {
					graph twoway (line `y'_pct_decl year, lcolor(edkblue)) ///
						(line `y'_pct_taxd year, lcolor(maroon)) ///
						if year>=1964 & !missing(`y'_pct_decl, `y'_pct_taxd) ///
						, graphregion(color(white)) plotregion(lcolor(bluishgray)) scale(1.2) ///
						ytitle("Share of Control Income Total (%)") xtitle("") ///
						xlabel(1960(10)2020, labsize(medium) grid labels) ///
						ylabel(0(20)100, labsize(medium) angle(horizontal) format(%2.0f) grid labels) ///
						scheme(s1color) subtitle(,fcolor(white) lcolor(bluishgray)) ///
						text(60 2015 "Declared", color(edkblue)) ///
						text(30 2015 "Taxable", color(maroon)) ///
						legend(off)
						quietly graph export "Figures/inc_`y'.pdf", replace 
				}
			}
	restore	
	}

	// 6. PREPARE FOR GPINTER --------------------------------------------//
	foreach y in `taxtype'{
		quietly use `tf', clear
		quietly keep if !missing(Y_`y')
		quietly gen country="CHI"
		quietly gen component="total"
		quietly rename totpop popsize
		quietly rename tramo thr
		quietly rename cumfreq_`y' p
		quietly replace p=1-p if p!=0
		quietly gen average=`totinc'_TI*gdp/popsize 	
		quietly gen bracketavg=Y_`y'/N_`y'
		quietly replace thr=thr*UTA if year>=1975 & !missing(UTA)
		quietly replace thr=thr*s_vital if year<1975

	//Check consistency (collapse if inconsistent)
	quietly count if (bracketavg>=thr[_n+1] & year[_n+1]==year) | bracketavg<thr
		while (r(N) > 0){
			tempvar bracket newbracket queue weight nweight
			by year: generate `queue' = sum(bracketavg>=thr[_n+1]| bracketavg<thr)
			by year: generate `bracket'=_n
			//We group the bracket with the one just above
			by year: generate `newbracket' = `bracket'[_n + 1] ///
				if (bracketavg>=thr[_n+1] | bracketavg<thr)
			by year: replace `bracket' = `newbracket' ///
				if (`queue' == 1) & (bracketavg>=thr[_n+1] | bracketavg<thr)
			//weight brackets before collapsing
			by year: gen `weight'=p[_n+1] - p
			quietly replace `weight' = 1 - p if missing(`weight')
			quietly gen `nweight'=popsize*`weight'
			quietly collapse  (firstnm) country component popsize average (min) p thr ///
				(mean) bracketavg [w=`nweight'],by(year `bracket')
			quietly count if (bracketavg >= thr[_n+1] & year[_n+1] == year) | bracketavg<thr
			di "`r(N)'"
		}

		//Export tables
		local vars "year country component popsize average p thr bracketavg"
		local delvar_st "country component"
		local delvar_nu "year average popsize"

		keep `vars'
		order `vars'
		quietly drop if missing(thr)
		quietly levelsof year, local(allyears)
		foreach t in `allyears' {
			preserve
				quietly keep if year==`t'
				foreach v in `delvar_st'{
					quietly replace `v'="" if _n!=1
				}
				foreach v in `delvar_nu'{
					quietly replace `v'=. if _n!=1
				}
				quietly export excel using ///
					"Data/Gpinter/input/gpinter_tables_`y'_`totinc'.xlsx" ///
					, firstrow(variables) sheet("`t'") sheetreplace
			restore		
		}
	}
}

//Save some info for Part 2
clear
set obs 3
quietly gen Year=.
foreach g in `groups'{
	quietly gen avg_taxded_`g'=.
	quietly gen avg_allow_`g'=.
	local iter=1
	foreach yr in "1973" "1996" "1999" {
		quietly replace Year=`yr' in `iter'
		quietly replace avg_taxded_`g'=`avg_taxdeds_`yr'_`g'' ///
			if Year==`yr'
		if ("`yr'"!="1973"){
			quietly replace avg_allow_`g'=`avg_allowdeds_`yr'_`g'' ///
				if Year==`yr'
	}	
	local iter=`iter'+1
	}
	quietly egen total_`g'=rowtotal(avg_taxded_`g' avg_allow_`g')
}
quietly export excel "Data/auxi.xlsx", ///
	sheet("avg_deduc") firstrow(variables) sheetreplace
	
