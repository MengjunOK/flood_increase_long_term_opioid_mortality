
********************************************************************************
* 03_robustness_checks.do
*
* Robustness Checks — 5 Analyses, Each Producing 4-Panel Figure
*
* Figure S5-2: Alternative flood definition (NOAA deaths)
* Figure S5-3: Prescription drug overdose deaths
* Figure S5-4: Overall drug-related deaths
* Figure S5-6: Including both urban and rural Appalachian counties
* Figure S5-7: Excluding Alabama and Mississippi
*
* Each analysis: 4 outcomes (count, IHS count, rate, IHS rate) for allpop
* Exports CSVs with coefficients + SEs for R plotting
*
* Requires: eventstudyinteract, estout packages
********************************************************************************


	capture log close _all
	log using "$logs/03_robustness_checks.log", replace name(robust)


*======================================================================*
*  Figure S5-3: Prescription Drug Overdose Deaths
*======================================================================*

	di _n "=============================================="
	di    "  Figure S5-3: Prescription Drug Deaths"
	di    "=============================================="

	use $data/floodopioid_demo.dta, clear

		foreach pop in allpop {
			gen death_pre_`pop'_rate = (death_pre_`pop'/pop_`pop')*100000
			gen death_pre_`pop'_t = asinh(death_pre_`pop')
			gen death_pre_`pop'_rate_t = asinh(death_pre_`pop'_rate)
		}

		egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
		egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)
		drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
		drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
		rename F_FH_10p F_FH_10
		rename L_FH_10p L_FH_10
		gen F_FH_1 = 0

		drop if all_fefl_fl1h_1st_year <= 1999
		keep if ruralregion == 1
		drop if year >= fefl_fl1h_2nd_year

		encode state, gen(stateencode)
		gen stateyearstr = state + yearstr
		encode stateyearstr, gen(stateyearencode)

		global controlok pop_allpop povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
		global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 F_FH_1

	eststo clear
	foreach var in allpop allpop_t allpop_rate allpop_rate_t {
		eventstudyinteract death_pre_`var' $timeind L_FH_*, ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
			covariates($controlok)
		matrix b = e(b_iw)
		matrix V = e(V_iw)
		ereturn post b V
		lincom (F_FH_2+F_FH_3+F_FH_4+F_FH_5+F_FH_6+F_FH_7+F_FH_8+F_FH_9+F_FH_10)/10
		estadd scalar lincom1_estimate = r(estimate)
		estadd scalar lincom1_p = r(p)
		test (F_FH_2=0)(F_FH_3=0)(F_FH_4=0)(F_FH_5=0)(F_FH_6=0)(F_FH_7=0)(F_FH_8=0)(F_FH_9=0)(F_FH_10=0)
		estadd scalar ktest_p = r(p)
		lincom (L_FH_0+L_FH_1+L_FH_2+L_FH_3+L_FH_4+L_FH_5+L_FH_6+L_FH_7+L_FH_8+L_FH_9+L_FH_10)/11
		estadd scalar lincom2_estimate = r(estimate)
		estadd scalar lincom2_p = r(p)
		eststo pre_`var'
	}

	esttab pre_allpop pre_allpop_t pre_allpop_rate pre_allpop_rate_t ///
		using "$tables/esttab_robust_predrug.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)


*======================================================================*
*  Figure S5-4: Overall Drug-Related Deaths
*======================================================================*

	di _n "=============================================="
	di    "  Figure S5-4: All Drug Deaths"
	di    "=============================================="

	use $data/floodopioid_demo.dta, clear

		foreach pop in allpop {
			gen death_alldrug_`pop'_rate = (death_alldrug_`pop'/pop_`pop')*100000
			gen death_alldrug_`pop'_t = asinh(death_alldrug_`pop')
			gen death_alldrug_`pop'_rate_t = asinh(death_alldrug_`pop'_rate)
		}

		egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
		egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)
		drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
		drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
		rename F_FH_10p F_FH_10
		rename L_FH_10p L_FH_10
		gen F_FH_1 = 0

		drop if all_fefl_fl1h_1st_year <= 1999
		keep if ruralregion == 1
		drop if year >= fefl_fl1h_2nd_year

		encode state, gen(stateencode)
		gen stateyearstr = state + yearstr
		encode stateyearstr, gen(stateyearencode)

		global controlok pop_allpop povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
		global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 F_FH_1

	eststo clear
	foreach var in allpop allpop_t allpop_rate allpop_rate_t {
		eventstudyinteract death_alldrug_`var' $timeind L_FH_*, ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
			covariates($controlok)
		matrix b = e(b_iw)
		matrix V = e(V_iw)
		ereturn post b V
		lincom (F_FH_2+F_FH_3+F_FH_4+F_FH_5+F_FH_6+F_FH_7+F_FH_8+F_FH_9+F_FH_10)/10
		estadd scalar lincom1_estimate = r(estimate)
		estadd scalar lincom1_p = r(p)
		test (F_FH_2=0)(F_FH_3=0)(F_FH_4=0)(F_FH_5=0)(F_FH_6=0)(F_FH_7=0)(F_FH_8=0)(F_FH_9=0)(F_FH_10=0)
		estadd scalar ktest_p = r(p)
		lincom (L_FH_0+L_FH_1+L_FH_2+L_FH_3+L_FH_4+L_FH_5+L_FH_6+L_FH_7+L_FH_8+L_FH_9+L_FH_10)/11
		estadd scalar lincom2_estimate = r(estimate)
		estadd scalar lincom2_p = r(p)
		eststo all_`var'
	}

	esttab all_allpop all_allpop_t all_allpop_rate all_allpop_rate_t ///
		using "$tables/esttab_robust_alldrug.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)


*======================================================================*
*  Figure S5-6: Including Both Urban and Rural Counties
*======================================================================*

	di _n "=============================================="
	di    "  Figure S5-6: Urban + Rural"
	di    "=============================================="

	use $data/floodopioid_demo.dta, clear

		foreach drug in opioid {
		foreach pop in allpop {
			gen death_`drug'_`pop'_rate = (death_`drug'_`pop'/pop_`pop')*100000
			gen death_`drug'_`pop'_t = asinh(death_`drug'_`pop')
			gen death_`drug'_`pop'_rate_t = asinh(death_`drug'_`pop'_rate)
		}
		}

		egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
		egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)
		drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
		drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
		rename F_FH_10p F_FH_10
		rename L_FH_10p L_FH_10
		gen F_FH_1 = 0

		drop if all_fefl_fl1h_1st_year <= 1999
		** NOTE: No ruralregion filter — this is the key difference
		drop if year >= fefl_fl1h_2nd_year

		encode state, gen(stateencode)
		gen stateyearstr = state + yearstr
		encode stateyearstr, gen(stateyearencode)

		global controlok pop_allpop povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
		global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 F_FH_1

	eststo clear
	foreach var in allpop allpop_t allpop_rate allpop_rate_t {
		eventstudyinteract death_opioid_`var' $timeind L_FH_*, ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
			covariates($controlok)
		matrix b = e(b_iw)
		matrix V = e(V_iw)
		ereturn post b V
		lincom (F_FH_2+F_FH_3+F_FH_4+F_FH_5+F_FH_6+F_FH_7+F_FH_8+F_FH_9+F_FH_10)/10
		estadd scalar lincom1_estimate = r(estimate)
		estadd scalar lincom1_p = r(p)
		test (F_FH_2=0)(F_FH_3=0)(F_FH_4=0)(F_FH_5=0)(F_FH_6=0)(F_FH_7=0)(F_FH_8=0)(F_FH_9=0)(F_FH_10=0)
		estadd scalar ktest_p = r(p)
		lincom (L_FH_0+L_FH_1+L_FH_2+L_FH_3+L_FH_4+L_FH_5+L_FH_6+L_FH_7+L_FH_8+L_FH_9+L_FH_10)/11
		estadd scalar lincom2_estimate = r(estimate)
		estadd scalar lincom2_p = r(p)
		eststo urban_`var'
	}

	esttab urban_allpop urban_allpop_t urban_allpop_rate urban_allpop_rate_t ///
		using "$tables/esttab_robust_urbanrural.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)


*======================================================================*
*  Figure S5-7: Excluding Alabama and Mississippi
*======================================================================*

	di _n "=============================================="
	di    "  Figure S5-7: Exclude AL & MS"
	di    "=============================================="

	use $data/floodopioid_demo.dta, clear

		foreach drug in opioid {
		foreach pop in allpop {
			gen death_`drug'_`pop'_rate = (death_`drug'_`pop'/pop_`pop')*100000
			gen death_`drug'_`pop'_t = asinh(death_`drug'_`pop')
			gen death_`drug'_`pop'_rate_t = asinh(death_`drug'_`pop'_rate)
		}
		}

		egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
		egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)
		drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
		drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
		rename F_FH_10p F_FH_10
		rename L_FH_10p L_FH_10
		gen F_FH_1 = 0

		drop if all_fefl_fl1h_1st_year <= 1999
		keep if ruralregion == 1
		** NOTE: Exclude Alabama and Mississippi
		drop if state == "AL" | state == "MS"
		drop if year >= fefl_fl1h_2nd_year

		encode state, gen(stateencode)
		gen stateyearstr = state + yearstr
		encode stateyearstr, gen(stateyearencode)

		global controlok pop_allpop povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
		global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 F_FH_4 F_FH_3 F_FH_2 F_FH_1

	eststo clear
	foreach var in allpop allpop_t allpop_rate allpop_rate_t {
		eventstudyinteract death_opioid_`var' $timeind L_FH_*, ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
			covariates($controlok)
		matrix b = e(b_iw)
		matrix V = e(V_iw)
		ereturn post b V
		lincom (F_FH_2+F_FH_3+F_FH_4+F_FH_5+F_FH_6+F_FH_7+F_FH_8+F_FH_9+F_FH_10)/10
		estadd scalar lincom1_estimate = r(estimate)
		estadd scalar lincom1_p = r(p)
		test (F_FH_2=0)(F_FH_3=0)(F_FH_4=0)(F_FH_5=0)(F_FH_6=0)(F_FH_7=0)(F_FH_8=0)(F_FH_9=0)(F_FH_10=0)
		estadd scalar ktest_p = r(p)
		lincom (L_FH_0+L_FH_1+L_FH_2+L_FH_3+L_FH_4+L_FH_5+L_FH_6+L_FH_7+L_FH_8+L_FH_9+L_FH_10)/11
		estadd scalar lincom2_estimate = r(estimate)
		estadd scalar lincom2_p = r(p)
		eststo dropALMS_`var'
	}

	esttab dropALMS_allpop dropALMS_allpop_t dropALMS_allpop_rate dropALMS_allpop_rate_t ///
		using "$tables/esttab_robust_dropALMS.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)


*======================================================================*
*  Figure S5-2: Alternative Flood Definition (NOAA Deaths)
*======================================================================*

	di _n "=============================================="
	di    "  Figure S5-2: NOAA Deaths Definition"
	di    "=============================================="

	use $data/floodopioid_demo.dta, clear

		foreach pop in allpop {
			gen death_opioid_`pop'_rate = (death_opioid_`pop'/pop_`pop')*100000
			gen death_opioid_`pop'_t = asinh(death_opioid_`pop')
			gen death_opioid_`pop'_rate_t = asinh(death_opioid_`pop'_rate)
		}

		keep if ruralregion == 1
		drop if fl1d_1st_year <= 1999
		drop if all_fl1d_1st_year <= 1999
		drop if year >= fl1d_2nd_year

		** Event study indicators: bin into +/-10+
		egen F_D_10p = rowmax(F_D_10 F_D_11 F_D_12 F_D_13 F_D_14 F_D_15 F_D_16 F_D_17 F_D_18)
		egen L_D_10p = rowmax(L_D_10 L_D_11 L_D_12 L_D_13 L_D_14 L_D_15 L_D_16 L_D_17)
		drop F_D_10 F_D_11 F_D_12 F_D_13 F_D_14 F_D_15 F_D_16 F_D_17 F_D_18 F_D_19 F_D_20 F_D_21 F_D_22 F_D_23 F_D_24 F_D_25 F_D_26
		drop L_D_10 L_D_11 L_D_12 L_D_13 L_D_14 L_D_15 L_D_16 L_D_17 L_D_18 L_D_19 L_D_20 L_D_21 L_D_22 L_D_23 L_D_24 L_D_25 L_D_26
		rename F_D_10p F_D_10
		rename L_D_10p L_D_10
		gen F_D_1 = 0

		encode state, gen(stateencode)
		gen stateyearstr = state + yearstr
		encode stateyearstr, gen(stateyearencode)

		global controlok pop_allpop povertypct medianhhi unemployrate fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
		global timeind F_D_10 F_D_9 F_D_8 F_D_7 F_D_6 F_D_5 F_D_4 F_D_3 F_D_2 F_D_1

	eststo clear
	foreach var in allpop allpop_t allpop_rate allpop_rate_t {
		eventstudyinteract death_opioid_`var' $timeind L_D_*, ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fl1d_1st_year) control_cohort(never_fl1d) ///
			covariates($controlok)
		matrix b = e(b_iw)
		matrix V = e(V_iw)
		ereturn post b V
		lincom (F_D_2+F_D_3+F_D_4+F_D_5+F_D_6+F_D_7+F_D_8+F_D_9+F_D_10)/10
		estadd scalar lincom1_estimate = r(estimate)
		estadd scalar lincom1_p = r(p)
		test (F_D_2=0)(F_D_3=0)(F_D_4=0)(F_D_5=0)(F_D_6=0)(F_D_7=0)(F_D_8=0)(F_D_9=0)(F_D_10=0)
		estadd scalar ktest_p = r(p)
		lincom (L_D_0+L_D_1+L_D_2+L_D_3+L_D_4+L_D_5+L_D_6+L_D_7+L_D_8+L_D_9+L_D_10)/11
		estadd scalar lincom2_estimate = r(estimate)
		estadd scalar lincom2_p = r(p)
		eststo noaa_`var'
	}

	esttab noaa_allpop noaa_allpop_t noaa_allpop_rate noaa_allpop_rate_t ///
		using "$tables/esttab_robust_noaa.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)


	di _n "03_robustness_checks.do complete. CSVs exported to: $tables"

	log close robust
