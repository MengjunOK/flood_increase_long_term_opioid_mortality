/*
================================================================================
    04_migration_robustness.do

    Consolidated migration, composition stability, and demographic controls
    robustness tests for PNAS replication.

    Part 1: Migration flow test
            — pop_allpop, log_pop, rnetmig
            — With joint F-tests

    Part 2: Composition stability test (standard binned, same as main)
            — 8 demographic shares + edupct_leh (edu restricted 2008+)
            — With joint F-tests

    Part 3: Main analysis with demographic composition controls
            — Main: 4 outcomes (allpop, allpop_t, allpop_rate, allpop_rate_t)
            — Subgroup: allpop_t × 3 pairs (Male/Female, White/Nonwhite, Low/High Edu)
            — Subsample: allpop_t × 2 pairs (Distressed/Non-distressed, Low/High MDI)
            — No F-tests

    Globals required: $data, $tables, $logs (set by master.do)
    Output: $tables/coefdata_migration_robustness.csv
================================================================================
*/


	capture log close
	log using "$logs/04_migration_robustness.log", replace


** Set up unified postfile for all parts
tempname coefhandle
tempfile coeffile
postfile `coefhandle' str30 variable int period double(coeff se ci_lo ci_hi) ///
	double(avg_post_coeff avg_post_se avg_post_p avg_pre_coeff avg_pre_se avg_pre_p joint_f_pre_p joint_f_post_p) ///
	str30 part using `coeffile', replace



*======================================================================*
*  PART 1: Migration Flow Test                                        *
*  Variables: pop_allpop, log_pop, rnetmig                             *
*  Zeros F_FH_15–18 & L_FH_15–17, bins ±10–14 into ±10+              *
*======================================================================*

di as text _newline "========================================"
di as text "PART 1: Migration Flow Test"
di as text "========================================"

use "$data/floodopioid_demo.dta", clear

	** Dependent Variables
	foreach drug in alldrug opioid pre {
	foreach pop in allpop male female white nonwhite leh hh {
		gen death_`drug'_`pop'_rate = (death_`drug'_`pop'/pop_`pop')*100000
		gen death_`drug'_`pop'_t = asinh(death_`drug'_`pop')
		gen death_`drug'_`pop'_rate_t = asinh(death_`drug'_`pop'_rate)
	}
	}

	** Sample Selection: rural counties, no pre-2000 floods
	drop if all_fefl_fl1h_1st_year <= 1999
	keep if ruralregion == 1

	** County subsamples
	gen distressed = (econcate2002=="Distressed" | econcate2003=="Distressed" | econcate2004=="Distressed")
	gen lowmdi = (pctnation2000_medianhhi <= 30251)

	** Drop obs after 2nd flood
	drop if year >= fefl_fl1h_2nd_year

	** State x Year FE
	encode state, gen(stateencode)
	gen stateyearstr = state + yearstr
	encode stateyearstr, gen(stateyearencode)

	gen F_FH_1 = 0
	gen log_pop = log(pop_allpop)

	** Controls
	global controlok povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3

	** Event Study time Indicators: bin into ±10+
	egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14)
	egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14)
	drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
	drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
	rename F_FH_10p F_FH_10
	rename L_FH_10p L_FH_10

	global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 F_FH_1

eststo clear

foreach var in pop_allpop log_pop rnetmig {

	di as text _newline "--- Part 1: `var' ---"

	eventstudyinteract `var' $timeind ///
		L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Avg pre-trend (excl t-1)
	lincom (F_FH_2 + F_FH_3 + F_FH_4 + F_FH_5 + F_FH_6 + F_FH_7 + F_FH_8 + F_FH_9 + F_FH_10) / 9
	scalar lincom1_estimate = r(estimate)
	scalar lincom1_se = r(se)
	scalar lincom1_p = r(p)

	// Joint pre-trend F-test
	test (F_FH_2 = 0) (F_FH_3 = 0) (F_FH_4 = 0) (F_FH_5 = 0) (F_FH_6 = 0) (F_FH_7 = 0) (F_FH_8 = 0) (F_FH_9 = 0) (F_FH_10 = 0)
	scalar ktest_p = r(p)

	// Avg post-treatment effect (periods 0 to 10+)
	lincom (L_FH_0 + L_FH_1 + L_FH_2 + L_FH_3 + L_FH_4 + L_FH_5 + L_FH_6 + L_FH_7 + L_FH_8 + L_FH_9 + L_FH_10) / 11
	scalar lincom2_estimate = r(estimate)
	scalar lincom2_se = r(se)
	scalar lincom2_p = r(p)

	// Joint post-treatment F-test
	test (L_FH_0 = 0) (L_FH_1 = 0) (L_FH_2 = 0) (L_FH_3 = 0) (L_FH_4 = 0) (L_FH_5 = 0) (L_FH_6 = 0) (L_FH_7 = 0) (L_FH_8 = 0) (L_FH_9 = 0) (L_FH_10 = 0)
	scalar ktest_post_p = r(p)

	// Store for esttab
	estadd scalar lincom1_estimate = lincom1_estimate
	estadd scalar lincom1_se = lincom1_se
	estadd scalar lincom1_p = lincom1_p
	estadd scalar lincom2_estimate = lincom2_estimate
	estadd scalar lincom2_se = lincom2_se
	estadd scalar lincom2_p = lincom2_p
	estadd scalar ktest_p = ktest_p
	estadd scalar ktest_post_p = ktest_post_p

	// Post reference period (t = -1)
	post `coefhandle' ("`var'") (-1) (0) (0) (0) (0) ///
		(lincom2_estimate) (lincom2_se) (lincom2_p) ///
		(lincom1_estimate) (lincom1_se) (lincom1_p) (ktest_p) (ktest_post_p) ("migration")

	// Post period-by-period coefficients
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("`var'") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(lincom2_estimate) (lincom2_se) (lincom2_p) ///
				(lincom1_estimate) (lincom1_se) (lincom1_p) (ktest_p) (ktest_post_p) ("migration")
		}
	}

	eststo `var'
}

** Summary table: Migration Flow
esttab pop_allpop log_pop rnetmig ///
	using "$tables/migration_summary.csv", replace ///
	cells(none) ///
	stats(lincom1_estimate lincom1_se lincom1_p ktest_p lincom2_estimate lincom2_se lincom2_p ktest_post_p N, ///
		labels("Avg Pre (excl t-1)" "SE (Pre)" "p-value (Pre)" "Joint F Pre p" "Avg Post" "SE (Post)" "p-value (Post)" "Joint F Post p" "N") ///
		fmt(%9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
	mtitle("Total Pop" "Log(Pop)" "Net Mig Rate") ///
	nonumber



*======================================================================*
*  RELOAD DATA for Standard Binning (Parts 2 & 3)                      *
*======================================================================*

di as text _newline "========================================"
di as text "Reloading data for standard binning (Parts 2 & 3)"
di as text "========================================"

use "$data/floodopioid_demo.dta", clear

	** Dependent Variables
	foreach drug in alldrug opioid pre {
	foreach pop in allpop male female white nonwhite leh hh {
		gen death_`drug'_`pop'_rate = (death_`drug'_`pop'/pop_`pop')*100000
		gen death_`drug'_`pop'_t = asinh(death_`drug'_`pop')
		gen death_`drug'_`pop'_rate_t = asinh(death_`drug'_`pop'_rate)
	}
	}

	** Sample Selection
	drop if all_fefl_fl1h_1st_year <= 1999
	keep if ruralregion == 1

	** County subsamples
	gen distressed = (econcate2002=="Distressed" | econcate2003=="Distressed" | econcate2004=="Distressed")
	gen lowmdi = (pctnation2000_medianhhi <= 30251)

	** Drop obs after 2nd flood
	drop if year >= fefl_fl1h_2nd_year

	** State x Year FE
	encode state, gen(stateencode)
	gen stateyearstr = state + yearstr
	encode stateyearstr, gen(stateyearencode)

	** Standard binning: all periods into ±10+
	egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
	egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)

	drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
	drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26

	rename F_FH_10p F_FH_10
	rename L_FH_10p L_FH_10

	gen F_FH_1 = 0

	** Controls
	global controlok povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
	global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 F_FH_1
	global timeaft L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10

	** Generate population shares (for composition tests)
	global pop_structure pop_allpop pop_nonwhite pop_white pop_male pop_female pop_less25 pop_2544 pop_4564 pop_over65

	foreach i in $pop_structure {
		gen `i'_pct = (`i'/pop_allpop)*100
	}

	** Education pct (already in percentage form)
	gen edupct_leh_pct = edupct_leh

	** Generate demographic control variables (for Part 3)
	gen pct_white = pop_white_pct
	gen pct_male = pop_male_pct
	gen pct_2544 = pop_2544_pct
	gen pct_4564 = pop_4564_pct
	gen pct_over65 = pop_over65_pct

	global democontrol rnetmig pct_white pct_male edupct_hh pct_2544 pct_4564 pct_over65



*======================================================================*
*  PART 2: Composition Stability Test (Standard Binned)                *
*  8 demographic shares + edupct_leh (education restricted 2008+)      *
*  Controls: $controlok + pop_allpop                                   *
*======================================================================*

di as text _newline "========================================"
di as text "PART 2: Composition Stability (Standard Binned)"
di as text "========================================"

eststo clear

** 2a: Demographic shares (full sample)
global demo_test pop_nonwhite pop_white pop_male pop_female pop_less25 pop_2544 pop_4564 pop_over65

foreach var in $demo_test {

	di as text _newline "--- Part 2a: `var' ---"

	eventstudyinteract `var'_pct $timeind L_FH_*, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok pop_allpop)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Avg pre-trend (excl t-1)
	lincom (F_FH_2 + F_FH_3 + F_FH_4 + F_FH_5 + F_FH_6 + F_FH_7 + F_FH_8 + F_FH_9 + F_FH_10) / 9
	scalar lincom1_estimate = r(estimate)
	scalar lincom1_se = r(se)
	scalar lincom1_p = r(p)

	// Joint pre-trend F-test
	test (F_FH_2 = 0) (F_FH_3 = 0) (F_FH_4 = 0) (F_FH_5 = 0) (F_FH_6 = 0) (F_FH_7 = 0) (F_FH_8 = 0) (F_FH_9 = 0) (F_FH_10 = 0)
	scalar ktest_p = r(p)

	// Avg post-treatment effect
	lincom (L_FH_0 + L_FH_1 + L_FH_2 + L_FH_3 + L_FH_4 + L_FH_5 + L_FH_6 + L_FH_7 + L_FH_8 + L_FH_9 + L_FH_10) / 11
	scalar lincom2_estimate = r(estimate)
	scalar lincom2_se = r(se)
	scalar lincom2_p = r(p)

	// Joint post-treatment F-test
	test (L_FH_0 = 0) (L_FH_1 = 0) (L_FH_2 = 0) (L_FH_3 = 0) (L_FH_4 = 0) (L_FH_5 = 0) (L_FH_6 = 0) (L_FH_7 = 0) (L_FH_8 = 0) (L_FH_9 = 0) (L_FH_10 = 0)
	scalar ktest_post_p = r(p)

	// Store for esttab
	estadd scalar lincom1_estimate = lincom1_estimate
	estadd scalar lincom1_se = lincom1_se
	estadd scalar lincom1_p = lincom1_p
	estadd scalar lincom2_estimate = lincom2_estimate
	estadd scalar lincom2_se = lincom2_se
	estadd scalar lincom2_p = lincom2_p
	estadd scalar ktest_p = ktest_p
	estadd scalar ktest_post_p = ktest_post_p

	// Post reference period
	post `coefhandle' ("`var'") (-1) (0) (0) (0) (0) ///
		(lincom2_estimate) (lincom2_se) (lincom2_p) ///
		(lincom1_estimate) (lincom1_se) (lincom1_p) (ktest_p) (ktest_post_p) ("composition")

	// Post period-by-period coefficients
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("`var'") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(lincom2_estimate) (lincom2_se) (lincom2_p) ///
				(lincom1_estimate) (lincom1_se) (lincom1_p) (ktest_p) (ktest_post_p) ("composition")
		}
	}

	eststo `var'
}


** 2b: Education composition (restricted to 2008+, ACS annual data only)
di as text _newline "--- Part 2b: edupct_leh (2008+ only) ---"

preserve
keep if year >= 2008

foreach var in edupct_leh {

	eventstudyinteract `var'_pct $timeind L_FH_*, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok pop_allpop)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Avg pre-trend
	lincom (F_FH_2 + F_FH_3 + F_FH_4 + F_FH_5 + F_FH_6 + F_FH_7 + F_FH_8 + F_FH_9 + F_FH_10) / 9
	scalar lincom1_estimate = r(estimate)
	scalar lincom1_se = r(se)
	scalar lincom1_p = r(p)

	// Joint pre-trend F-test
	test (F_FH_2 = 0) (F_FH_3 = 0) (F_FH_4 = 0) (F_FH_5 = 0) (F_FH_6 = 0) (F_FH_7 = 0) (F_FH_8 = 0) (F_FH_9 = 0) (F_FH_10 = 0)
	scalar ktest_p = r(p)

	// Avg post-treatment
	lincom (L_FH_0 + L_FH_1 + L_FH_2 + L_FH_3 + L_FH_4 + L_FH_5 + L_FH_6 + L_FH_7 + L_FH_8 + L_FH_9 + L_FH_10) / 11
	scalar lincom2_estimate = r(estimate)
	scalar lincom2_se = r(se)
	scalar lincom2_p = r(p)

	// Joint post-treatment F-test
	test (L_FH_0 = 0) (L_FH_1 = 0) (L_FH_2 = 0) (L_FH_3 = 0) (L_FH_4 = 0) (L_FH_5 = 0) (L_FH_6 = 0) (L_FH_7 = 0) (L_FH_8 = 0) (L_FH_9 = 0) (L_FH_10 = 0)
	scalar ktest_post_p = r(p)

	// Store
	estadd scalar lincom1_estimate = lincom1_estimate
	estadd scalar lincom1_se = lincom1_se
	estadd scalar lincom1_p = lincom1_p
	estadd scalar lincom2_estimate = lincom2_estimate
	estadd scalar lincom2_se = lincom2_se
	estadd scalar lincom2_p = lincom2_p
	estadd scalar ktest_p = ktest_p
	estadd scalar ktest_post_p = ktest_post_p

	// Post reference period
	post `coefhandle' ("`var'") (-1) (0) (0) (0) (0) ///
		(lincom2_estimate) (lincom2_se) (lincom2_p) ///
		(lincom1_estimate) (lincom1_se) (lincom1_p) (ktest_p) (ktest_post_p) ("composition_edu")

	// Post period-by-period
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("`var'") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(lincom2_estimate) (lincom2_se) (lincom2_p) ///
				(lincom1_estimate) (lincom1_se) (lincom1_p) (ktest_p) (ktest_post_p) ("composition_edu")
		}
	}

	eststo `var'
}

restore

** Summary table: Composition Stability
esttab pop_nonwhite pop_white pop_male pop_female pop_less25 pop_2544 pop_4564 pop_over65 edupct_leh ///
	using "$tables/composition_stability_summary.csv", replace ///
	cells(none) ///
	stats(lincom1_estimate lincom1_se lincom1_p ktest_p lincom2_estimate lincom2_se lincom2_p ktest_post_p N, ///
		labels("Avg Pre (excl t-1)" "SE (Pre)" "p-value (Pre)" "Joint F Pre p" "Avg Post" "SE (Post)" "p-value (Post)" "Joint F Post p" "N") ///
		fmt(%9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.0f)) ///
	mtitle("Nonwhite%" "White%" "Male%" "Female%" "<25%" "25-44%" "45-64%" "65+%" "LowEdu%") ///
	nonumber



*======================================================================*
*  PART 3: Robustness with Demographic Composition Controls            *
*  Additional controls: $democontrol on top of $controlok              *
*  Main: 4 outcomes; Subgroup: allpop_t; Subsample: allpop_t           *
*  No F-tests                                                          *
*======================================================================*

di as text _newline "========================================"
di as text "PART 3: Robustness with Demographic Controls"
di as text "========================================"

** 3a: Main — all 4 outcomes
global main_robust allpop allpop_t allpop_rate allpop_rate_t

eststo clear

foreach var in $main_robust {

	di as text _newline "--- Part 3a main: `var' ---"

	eventstudyinteract death_opioid_`var' $timeind L_FH_*, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok $democontrol)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Post reference period (no F-tests, post missing for aggregate stats)
	post `coefhandle' ("`var'") (-1) (0) (0) (0) (0) ///
		(.) (.) (.) (.) (.) (.) (.) (.) ("robust_main")

	// Post period-by-period
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("`var'") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(.) (.) (.) (.) (.) (.) (.) (.) ("robust_main")
		}
	}

	eststo `var'
}

** Summary table: Main with demographic controls
esttab allpop allpop_t allpop_rate allpop_rate_t ///
	using "$tables/robust_demogcontrol_summary.csv", replace ///
	cells(b(star fmt(4)) se(par fmt(4))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("Count" "IHS(Count)" "Rate" "IHS(Rate)") ///
	nonumber


** 3b: Subgroup — IHS(count) only × 3 pairs
global subpop_robust male_t female_t white_t nonwhite_t leh_t hh_t

eststo clear

foreach var in $subpop_robust {

	di as text _newline "--- Part 3b subgroup: `var' ---"

	eventstudyinteract death_opioid_`var' $timeind L_FH_*, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok $democontrol)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Post reference period
	post `coefhandle' ("`var'") (-1) (0) (0) (0) (0) ///
		(.) (.) (.) (.) (.) (.) (.) (.) ("robust_subgroup")

	// Post period-by-period
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("`var'") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(.) (.) (.) (.) (.) (.) (.) (.) ("robust_subgroup")
		}
	}

	eststo `var'
}

** Summary table: Subgroup with demographic controls
esttab male_t female_t white_t nonwhite_t leh_t hh_t ///
	using "$tables/robust_subgroup_summary.csv", replace ///
	cells(b(star fmt(4)) se(par fmt(4))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("Male IHS" "Female IHS" "White IHS" "Nonwhite IHS" "LowEdu IHS" "HighEdu IHS") ///
	nonumber


** 3c: Subsample — IHS(count) only × 2 pairs
global condition distressed lowmdi

eststo clear

foreach cond in $condition {

	** Condition == 1
	di as text _newline "--- Part 3c subsample: allpop_t, `cond'==1 ---"

	eventstudyinteract death_opioid_allpop_t $timeind L_FH_* if `cond'==1, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok $democontrol)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Post reference period
	post `coefhandle' ("p_t`cond'1") (-1) (0) (0) (0) (0) ///
		(.) (.) (.) (.) (.) (.) (.) (.) ("robust_subsample")

	// Post period-by-period
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("p_t`cond'1") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(.) (.) (.) (.) (.) (.) (.) (.) ("robust_subsample")
		}
	}

	eststo p_t`cond'1


	** Condition == 0
	di as text _newline "--- Part 3c subsample: allpop_t, `cond'==0 ---"

	eventstudyinteract death_opioid_allpop_t $timeind L_FH_* if `cond'==0, ///
		vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
		cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
		covariates($controlok $democontrol)
	matrix b = e(b_iw)
	matrix V = e(V_iw)
	ereturn post b V

	// Post reference period
	post `coefhandle' ("p_t`cond'0") (-1) (0) (0) (0) (0) ///
		(.) (.) (.) (.) (.) (.) (.) (.) ("robust_subsample")

	// Post period-by-period
	foreach t in F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 ///
				 L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10 {
		local per = .
		if "`t'" == "F_FH_10" local per = -10
		if "`t'" == "F_FH_9"  local per = -9
		if "`t'" == "F_FH_8"  local per = -8
		if "`t'" == "F_FH_7"  local per = -7
		if "`t'" == "F_FH_6"  local per = -6
		if "`t'" == "F_FH_5"  local per = -5
		if "`t'" == "F_FH_4"  local per = -4
		if "`t'" == "F_FH_3"  local per = -3
		if "`t'" == "F_FH_2"  local per = -2
		if "`t'" == "L_FH_0"  local per = 0
		if "`t'" == "L_FH_1"  local per = 1
		if "`t'" == "L_FH_2"  local per = 2
		if "`t'" == "L_FH_3"  local per = 3
		if "`t'" == "L_FH_4"  local per = 4
		if "`t'" == "L_FH_5"  local per = 5
		if "`t'" == "L_FH_6"  local per = 6
		if "`t'" == "L_FH_7"  local per = 7
		if "`t'" == "L_FH_8"  local per = 8
		if "`t'" == "L_FH_9"  local per = 9
		if "`t'" == "L_FH_10" local per = 10
		capture local b_val = _b[`t']
		capture local se_val = _se[`t']
		if _rc == 0 {
			post `coefhandle' ("p_t`cond'0") (`per') (`b_val') (`se_val') ///
				(`b_val' - 1.96*`se_val') (`b_val' + 1.96*`se_val') ///
				(.) (.) (.) (.) (.) (.) (.) (.) ("robust_subsample")
		}
	}

	eststo p_t`cond'0
}

** Summary table: Subsample with demographic controls
esttab p_tdistressed1 p_tdistressed0 p_tlowmdi1 p_tlowmdi0 ///
	using "$tables/robust_subsample_summary.csv", replace ///
	cells(b(star fmt(4)) se(par fmt(4))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	mtitle("Distressed" "Non-distressed" "Low MDI" "High MDI") ///
	nonumber



*======================================================================*
*  Export unified coefficient data for R plotting                       *
*======================================================================*

postclose `coefhandle'

preserve
use `coeffile', clear
export delimited using "$tables/coefdata_migration_robustness.csv", replace
restore


di as text _newline "========================================"
di as text "All parts complete."
di as text "Output: $tables/coefdata_migration_robustness.csv"
di as text "========================================"

log close
