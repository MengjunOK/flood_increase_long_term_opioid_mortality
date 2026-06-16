
********************************************************************************
* 01_main_analysis.do
*
* Main event study analysis using Sun and Abraham (2021) estimator
*
* Produces:
*   - Regression estimates for 4 main outcomes (Fig 2)
*   - Subpopulation analysis: 6 groups x 4 outcomes (Fig 3 + S4)
*   - Subsample analysis: distressed/lowmdi x 4 outcomes (Fig 4 + S4)
*   - CSV exports to output/tables/ for R plotting
*
* Requires: eventstudyinteract, estout packages
********************************************************************************


*----------------------------------------------------------------------
* Data Preparation
*----------------------------------------------------------------------

	use $data/floodopioid_demo.dta, clear

	** Dependent Variables
	foreach drug in alldrug opioid pre {
	foreach pop in allpop male female white nonwhite leh hh {
		gen death_`drug'_`pop'_rate = (death_`drug'_`pop'/pop_`pop')*100000
		gen death_`drug'_`pop'_t = asinh(death_`drug'_`pop')
		gen death_`drug'_`pop'_rate_t = asinh(death_`drug'_`pop'_rate)
	}
	}

	** Event Study time Indicators: bin into +/-10+
	egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
	egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)
	drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
	drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
	rename F_FH_10p F_FH_10
	rename L_FH_10p L_FH_10
	gen F_FH_1 = 0

	** Sample Selection
	drop if all_fefl_fl1h_1st_year <= 1999
	keep if ruralregion == 1

	** County subsample indicators
	gen distressed = (econcate2002=="Distressed" | econcate2003=="Distressed" | econcate2004=="Distressed")
	gen lowmdi = (pctnation2000_medianhhi <= 30251)

	** Drop obs after 2nd flood
	drop if year >= fefl_fl1h_2nd_year

	** State x Year FE
	encode state, gen(stateencode)
	gen stateyearstr = state + yearstr
	encode stateyearstr, gen(stateyearencode)


*----------------------------------------------------------------------
* Globals
*----------------------------------------------------------------------

	global controlok pop_allpop povertypct medianhhi unemployrate ///
		fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
	global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 ///
		F_FH_4 F_FH_3 F_FH_2 F_FH_1

	global main    allpop allpop_rate allpop_t allpop_rate_t
	global subpop  male male_rate male_t male_rate_t ///
		female female_rate female_t female_rate_t ///
		white white_rate white_t white_rate_t ///
		nonwhite nonwhite_rate nonwhite_t nonwhite_rate_t ///
		leh leh_rate leh_t leh_rate_t ///
		hh hh_rate hh_t hh_rate_t
	global new        p p_rate p_t p_rate_t
	global condition  distressed lowmdi


*----------------------------------------------------------------------
* Event Study Regressions: Main + Subpopulations
*----------------------------------------------------------------------

	foreach var in $main $subpop {
		eventstudyinteract death_opioid_`var' $timeind L_FH_*, ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
			covariates($controlok)
		matrix b = e(b_iw)
		matrix V = e(V_iw)
		ereturn post b V

		lincom (F_FH_2+F_FH_3+F_FH_4+F_FH_5+F_FH_6+F_FH_7+F_FH_8+F_FH_9+F_FH_10)/10
		estadd scalar lincom1_estimate = r(estimate)
		estadd scalar lincom1_se = r(se)
		estadd scalar lincom1_p = r(p)

		test (F_FH_2=0)(F_FH_3=0)(F_FH_4=0)(F_FH_5=0)(F_FH_6=0)(F_FH_7=0)(F_FH_8=0)(F_FH_9=0)(F_FH_10=0)
		estadd scalar ktest_stat = r(chi2)
		estadd scalar ktest_p = r(p)
		estadd scalar ktest_df1 = r(df)

		lincom (L_FH_0+L_FH_1+L_FH_2+L_FH_3+L_FH_4+L_FH_5+L_FH_6+L_FH_7+L_FH_8+L_FH_9+L_FH_10)/11
		estadd scalar lincom2_estimate = r(estimate)
		estadd scalar lincom2_se = r(se)
		estadd scalar lincom2_p = r(p)

		eststo `var'
	}


*----------------------------------------------------------------------
* Event Study Regressions: Subsamples (distressed/lowmdi)
*----------------------------------------------------------------------

	foreach var in $new {
	foreach condition in $condition {

		** Condition == 1
		eventstudyinteract death_opioid_allpo`var' $timeind L_FH_* if `condition'==1, ///
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
		eststo `var'`condition'1

		** Condition == 0
		eventstudyinteract death_opioid_allpo`var' $timeind L_FH_* if `condition'==0, ///
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
		eststo `var'`condition'0

	}
	}


*----------------------------------------------------------------------
* Export CSV tables for R plotting
*----------------------------------------------------------------------

	** Figure 2: Main (4 outcomes)
	esttab allpop allpop_t allpop_rate allpop_rate_t ///
		using "$tables/esttab_main.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Figure 3: Subpop IHS count
	esttab leh_t hh_t male_t female_t white_t nonwhite_t ///
		using "$tables/esttab_subpop_ihscount.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Figure 4: Subsample IHS count
	esttab p_tdistressed1 p_tdistressed0 p_tlowmdi1 p_tlowmdi0 ///
		using "$tables/esttab_subsample_ihscount.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Supplementary: Subpop count
	esttab leh hh male female white nonwhite ///
		using "$tables/esttab_subpop_count.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Supplementary: Subpop rate
	esttab leh_rate hh_rate male_rate female_rate white_rate nonwhite_rate ///
		using "$tables/esttab_subpop_rate.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Supplementary: Subpop IHS rate
	esttab leh_rate_t hh_rate_t male_rate_t female_rate_t white_rate_t nonwhite_rate_t ///
		using "$tables/esttab_subpop_ihsrate.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Supplementary: Subsample count
	esttab pdistressed1 pdistressed0 plowmdi1 plowmdi0 ///
		using "$tables/esttab_subsample_count.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Supplementary: Subsample rate
	esttab p_ratedistressed1 p_ratedistressed0 p_ratelowmdi1 p_ratelowmdi0 ///
		using "$tables/esttab_subsample_rate.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	** Supplementary: Subsample IHS rate
	esttab p_rate_tdistressed1 p_rate_tdistressed0 p_rate_tlowmdi1 p_rate_tlowmdi0 ///
		using "$tables/esttab_subsample_ihsrate.csv", replace ///
		cells(b(fmt(6)) se(fmt(6))) nostar nomtitles nonumber plain ///
		scalars(lincom1_estimate lincom1_p lincom2_estimate lincom2_p ktest_p)

	di _n "01_main_analysis.do complete. CSVs exported to: $tables"
