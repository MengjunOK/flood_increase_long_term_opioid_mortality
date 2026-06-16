
********************************************************************************
* 02_subgroup_comparison.do
*
* Formal subgroup heterogeneity tests using Sun & Abraham (2021) estimator
* with State x Year x Group fixed effects
*
* Tests:
*   1. Male vs. Female (reshape long)
*   2. White vs. Nonwhite (reshape long)
*   3. Low-education vs. High-education (reshape long)
*   4. Distressed vs. Non-distressed counties (county-level split)
*   5. Low vs. High median household income (county-level split)
*
* Each comparison: 4 DVs (Count, Rate, IHS Count, IHS Rate)
* Clustering: State x Group
*
* Requires: eventstudyinteract, estout packages
********************************************************************************


*----------------------------------------------------------------------
* Data Preparation (identical to 01_main_analysis.do)
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
	global timeaft L_FH_0 L_FH_1 L_FH_2 L_FH_3 L_FH_4 L_FH_5 ///
		L_FH_6 L_FH_7 L_FH_8 L_FH_9 L_FH_10


*----------------------------------------------------------------------
* Log
*----------------------------------------------------------------------

	capture log close _all
	log using "$logs/02_subgroup_comparison.log", replace name(subcomp)


*======================================================================*
*  COMPARISON 1: Male vs. Female
*  g0 = male, g1 = female
*======================================================================*

	preserve

		rename death_opioid_male         dv_count0
		rename death_opioid_female       dv_count1
		rename death_opioid_male_rate    dv_rate0
		rename death_opioid_female_rate  dv_rate1
		rename death_opioid_male_t       dv_ihsc0
		rename death_opioid_female_t     dv_ihsc1
		rename death_opioid_male_rate_t  dv_ihsr0
		rename death_opioid_female_rate_t dv_ihsr1

		reshape long dv_count dv_rate dv_ihsc dv_ihsr, i(county_fips year) j(female)

		egen county_group    = group(county_fips female)
		egen stateyear_group = group(stateyearencode female)
		egen state_group     = group(stateencode female)

		foreach k in 10 9 8 7 6 5 4 3 2 {
			gen F_FH_`k'_g0 = F_FH_`k' * (female == 0)
			gen F_FH_`k'_g1 = F_FH_`k' * (female == 1)
		}
		gen F_FH_1_g0 = 0
		gen F_FH_1_g1 = 0

		foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
			gen L_FH_`k'_g0 = L_FH_`k' * (female == 0)
			gen L_FH_`k'_g1 = L_FH_`k' * (female == 1)
		}

		foreach dv in dv_count dv_rate dv_ihsc dv_ihsr {

			if "`dv'" == "dv_count" local dvlabel "Count"
			if "`dv'" == "dv_rate"  local dvlabel "Rate"
			if "`dv'" == "dv_ihsc"  local dvlabel "IHS(Count)"
			if "`dv'" == "dv_ihsr"  local dvlabel "IHS(Rate)"

			foreach clust in state_group {

				local clustlabel "State x Group"

				eventstudyinteract `dv' ///
					F_FH_10_g0 F_FH_9_g0 F_FH_8_g0 F_FH_7_g0 F_FH_6_g0 F_FH_5_g0 F_FH_4_g0 F_FH_3_g0 F_FH_2_g0 F_FH_1_g0 ///
					F_FH_10_g1 F_FH_9_g1 F_FH_8_g1 F_FH_7_g1 F_FH_6_g1 F_FH_5_g1 F_FH_4_g1 F_FH_3_g1 F_FH_2_g1 F_FH_1_g1 ///
					L_FH_*_g0 L_FH_*_g1, ///
					vce(cluster `clust') ///
					absorb(i.county_group i.stateyear_group) ///
					cohort(fefl_fl1h_1st_year) ///
					control_cohort(never_fefl_fl1h) ///
					covariates($controlok)

				matrix b = e(b_iw)
				matrix V = e(V_iw)
				ereturn post b V

				di ""
				di "============================================================"
				di "  TEST 1: Male vs Female — DV: `dvlabel', Cluster: `clustlabel'"
				di "============================================================"

				lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
				di "  Male avg post-treatment:  " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
				di "  Female avg post-treatment: " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11 ///
					  - (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
				di "  DIFFERENCE (Male - Female): " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				** Period-by-period tests
				di ""
				di "  Period-by-period difference tests (Male - Female):"
				di "  -------------------------------------------------"
				foreach k in 10 9 8 7 6 5 4 3 2 {
					lincom F_FH_`k'_g0 - F_FH_`k'_g1
					di "  t = -`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
				}
				foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
					lincom L_FH_`k'_g0 - L_FH_`k'_g1
					di "  t = +`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
				}

				** Joint F-test
				test (L_FH_0_g0 - L_FH_0_g1 = 0) (L_FH_1_g0 - L_FH_1_g1 = 0) (L_FH_2_g0 - L_FH_2_g1 = 0) ///
					 (L_FH_3_g0 - L_FH_3_g1 = 0) (L_FH_4_g0 - L_FH_4_g1 = 0) (L_FH_5_g0 - L_FH_5_g1 = 0) ///
					 (L_FH_6_g0 - L_FH_6_g1 = 0) (L_FH_7_g0 - L_FH_7_g1 = 0) (L_FH_8_g0 - L_FH_8_g1 = 0) ///
					 (L_FH_9_g0 - L_FH_9_g1 = 0) (L_FH_10_g0 - L_FH_10_g1 = 0)
				di "  Joint F-test (all post-treatment diffs = 0): chi2 = " r(chi2) "  p = " r(p)
			}
		}

	restore


*======================================================================*
*  COMPARISON 2: White vs. Nonwhite
*  g0 = white, g1 = nonwhite
*======================================================================*

	preserve

		rename death_opioid_white         dv_count0
		rename death_opioid_nonwhite      dv_count1
		rename death_opioid_white_rate    dv_rate0
		rename death_opioid_nonwhite_rate dv_rate1
		rename death_opioid_white_t       dv_ihsc0
		rename death_opioid_nonwhite_t    dv_ihsc1
		rename death_opioid_white_rate_t  dv_ihsr0
		rename death_opioid_nonwhite_rate_t dv_ihsr1

		reshape long dv_count dv_rate dv_ihsc dv_ihsr, i(county_fips year) j(nonwhite)

		egen county_group    = group(county_fips nonwhite)
		egen stateyear_group = group(stateyearencode nonwhite)
		egen state_group     = group(stateencode nonwhite)

		foreach k in 10 9 8 7 6 5 4 3 2 {
			gen F_FH_`k'_g0 = F_FH_`k' * (nonwhite == 0)
			gen F_FH_`k'_g1 = F_FH_`k' * (nonwhite == 1)
		}
		gen F_FH_1_g0 = 0
		gen F_FH_1_g1 = 0

		foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
			gen L_FH_`k'_g0 = L_FH_`k' * (nonwhite == 0)
			gen L_FH_`k'_g1 = L_FH_`k' * (nonwhite == 1)
		}

		foreach dv in dv_count dv_rate dv_ihsc dv_ihsr {

			if "`dv'" == "dv_count" local dvlabel "Count"
			if "`dv'" == "dv_rate"  local dvlabel "Rate"
			if "`dv'" == "dv_ihsc"  local dvlabel "IHS(Count)"
			if "`dv'" == "dv_ihsr"  local dvlabel "IHS(Rate)"

			foreach clust in state_group {

				local clustlabel "State x Group"

				eventstudyinteract `dv' ///
					F_FH_10_g0 F_FH_9_g0 F_FH_8_g0 F_FH_7_g0 F_FH_6_g0 F_FH_5_g0 F_FH_4_g0 F_FH_3_g0 F_FH_2_g0 F_FH_1_g0 ///
					F_FH_10_g1 F_FH_9_g1 F_FH_8_g1 F_FH_7_g1 F_FH_6_g1 F_FH_5_g1 F_FH_4_g1 F_FH_3_g1 F_FH_2_g1 F_FH_1_g1 ///
					L_FH_*_g0 L_FH_*_g1, ///
					vce(cluster `clust') ///
					absorb(i.county_group i.stateyear_group) ///
					cohort(fefl_fl1h_1st_year) ///
					control_cohort(never_fefl_fl1h) ///
					covariates($controlok)

				matrix b = e(b_iw)
				matrix V = e(V_iw)
				ereturn post b V

				di ""
				di "============================================================"
				di "  TEST 2: White vs Nonwhite — DV: `dvlabel', Cluster: `clustlabel'"
				di "============================================================"

				lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
				di "  White avg post-treatment:    " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
				di "  Nonwhite avg post-treatment: " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11 ///
					  - (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
				di "  DIFFERENCE (White - Nonwhite): " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				di ""
				di "  Period-by-period difference tests (White - Nonwhite):"
				di "  -----------------------------------------------------"
				foreach k in 10 9 8 7 6 5 4 3 2 {
					lincom F_FH_`k'_g0 - F_FH_`k'_g1
					di "  t = -`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
				}
				foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
					lincom L_FH_`k'_g0 - L_FH_`k'_g1
					di "  t = +`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
				}

				test (L_FH_0_g0 - L_FH_0_g1 = 0) (L_FH_1_g0 - L_FH_1_g1 = 0) (L_FH_2_g0 - L_FH_2_g1 = 0) ///
					 (L_FH_3_g0 - L_FH_3_g1 = 0) (L_FH_4_g0 - L_FH_4_g1 = 0) (L_FH_5_g0 - L_FH_5_g1 = 0) ///
					 (L_FH_6_g0 - L_FH_6_g1 = 0) (L_FH_7_g0 - L_FH_7_g1 = 0) (L_FH_8_g0 - L_FH_8_g1 = 0) ///
					 (L_FH_9_g0 - L_FH_9_g1 = 0) (L_FH_10_g0 - L_FH_10_g1 = 0)
				di "  Joint F-test (all post-treatment diffs = 0): chi2 = " r(chi2) "  p = " r(p)
			}
		}

	restore


*======================================================================*
*  COMPARISON 3: Low-education (leh) vs. High-education (hh)
*  g0 = low-education, g1 = high-education
*======================================================================*

	preserve

		rename death_opioid_leh         dv_count0
		rename death_opioid_hh          dv_count1
		rename death_opioid_leh_rate    dv_rate0
		rename death_opioid_hh_rate     dv_rate1
		rename death_opioid_leh_t       dv_ihsc0
		rename death_opioid_hh_t        dv_ihsc1
		rename death_opioid_leh_rate_t  dv_ihsr0
		rename death_opioid_hh_rate_t   dv_ihsr1

		reshape long dv_count dv_rate dv_ihsc dv_ihsr, i(county_fips year) j(higheduc)

		egen county_group    = group(county_fips higheduc)
		egen stateyear_group = group(stateyearencode higheduc)
		egen state_group     = group(stateencode higheduc)

		foreach k in 10 9 8 7 6 5 4 3 2 {
			gen F_FH_`k'_g0 = F_FH_`k' * (higheduc == 0)
			gen F_FH_`k'_g1 = F_FH_`k' * (higheduc == 1)
		}
		gen F_FH_1_g0 = 0
		gen F_FH_1_g1 = 0

		foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
			gen L_FH_`k'_g0 = L_FH_`k' * (higheduc == 0)
			gen L_FH_`k'_g1 = L_FH_`k' * (higheduc == 1)
		}

		foreach dv in dv_count dv_rate dv_ihsc dv_ihsr {

			if "`dv'" == "dv_count" local dvlabel "Count"
			if "`dv'" == "dv_rate"  local dvlabel "Rate"
			if "`dv'" == "dv_ihsc"  local dvlabel "IHS(Count)"
			if "`dv'" == "dv_ihsr"  local dvlabel "IHS(Rate)"

			foreach clust in state_group {

				local clustlabel "State x Group"

				eventstudyinteract `dv' ///
					F_FH_10_g0 F_FH_9_g0 F_FH_8_g0 F_FH_7_g0 F_FH_6_g0 F_FH_5_g0 F_FH_4_g0 F_FH_3_g0 F_FH_2_g0 F_FH_1_g0 ///
					F_FH_10_g1 F_FH_9_g1 F_FH_8_g1 F_FH_7_g1 F_FH_6_g1 F_FH_5_g1 F_FH_4_g1 F_FH_3_g1 F_FH_2_g1 F_FH_1_g1 ///
					L_FH_*_g0 L_FH_*_g1, ///
					vce(cluster `clust') ///
					absorb(i.county_group i.stateyear_group) ///
					cohort(fefl_fl1h_1st_year) ///
					control_cohort(never_fefl_fl1h) ///
					covariates($controlok)

				matrix b = e(b_iw)
				matrix V = e(V_iw)
				ereturn post b V

				di ""
				di "============================================================"
				di "  TEST 3: Low-educ vs High-educ — DV: `dvlabel', Cluster: `clustlabel'"
				di "============================================================"

				lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
				di "  Low-educ avg post-treatment:  " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
				di "  High-educ avg post-treatment: " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11 ///
					  - (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
				di "  DIFFERENCE (Low-educ - High-educ): " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

				di ""
				di "  Period-by-period difference tests (Low-educ - High-educ):"
				di "  ----------------------------------------------------------"
				foreach k in 10 9 8 7 6 5 4 3 2 {
					lincom F_FH_`k'_g0 - F_FH_`k'_g1
					di "  t = -`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
				}
				foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
					lincom L_FH_`k'_g0 - L_FH_`k'_g1
					di "  t = +`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
				}

				test (L_FH_0_g0 - L_FH_0_g1 = 0) (L_FH_1_g0 - L_FH_1_g1 = 0) (L_FH_2_g0 - L_FH_2_g1 = 0) ///
					 (L_FH_3_g0 - L_FH_3_g1 = 0) (L_FH_4_g0 - L_FH_4_g1 = 0) (L_FH_5_g0 - L_FH_5_g1 = 0) ///
					 (L_FH_6_g0 - L_FH_6_g1 = 0) (L_FH_7_g0 - L_FH_7_g1 = 0) (L_FH_8_g0 - L_FH_8_g1 = 0) ///
					 (L_FH_9_g0 - L_FH_9_g1 = 0) (L_FH_10_g0 - L_FH_10_g1 = 0)
				di "  Joint F-test (all post-treatment diffs = 0): chi2 = " r(chi2) "  p = " r(p)
			}
		}

	restore


*======================================================================*
*  COMPARISON 4: Distressed vs. Non-distressed counties
*  g0 = non-distressed, g1 = distressed
*======================================================================*

	** State-year x group FE and clustering var
	egen stateyear_distressed = group(stateyearencode distressed)
	egen state_group_dist     = group(stateencode distressed)

	foreach k in 10 9 8 7 6 5 4 3 2 {
		gen F_FH_`k'_g0 = F_FH_`k' * (distressed == 0)
		gen F_FH_`k'_g1 = F_FH_`k' * (distressed == 1)
	}
	gen F_FH_1_g0 = 0
	gen F_FH_1_g1 = 0

	foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
		gen L_FH_`k'_g0 = L_FH_`k' * (distressed == 0)
		gen L_FH_`k'_g1 = L_FH_`k' * (distressed == 1)
	}

	foreach dv in death_opioid_allpop death_opioid_allpop_rate death_opioid_allpop_t death_opioid_allpop_rate_t {

		if "`dv'" == "death_opioid_allpop"        local dvlabel "Count"
		if "`dv'" == "death_opioid_allpop_rate"    local dvlabel "Rate"
		if "`dv'" == "death_opioid_allpop_t"       local dvlabel "IHS(Count)"
		if "`dv'" == "death_opioid_allpop_rate_t"  local dvlabel "IHS(Rate)"

		foreach clust in state_group_dist {

			local clustlabel "State x Group"

			eventstudyinteract `dv' ///
				F_FH_10_g0 F_FH_9_g0 F_FH_8_g0 F_FH_7_g0 F_FH_6_g0 F_FH_5_g0 F_FH_4_g0 F_FH_3_g0 F_FH_2_g0 F_FH_1_g0 ///
				F_FH_10_g1 F_FH_9_g1 F_FH_8_g1 F_FH_7_g1 F_FH_6_g1 F_FH_5_g1 F_FH_4_g1 F_FH_3_g1 F_FH_2_g1 F_FH_1_g1 ///
				L_FH_*_g0 L_FH_*_g1, ///
				vce(cluster `clust') ///
				absorb(i.county_fips i.stateyear_distressed) ///
				cohort(fefl_fl1h_1st_year) ///
				control_cohort(never_fefl_fl1h) ///
				covariates($controlok)

			matrix b = e(b_iw)
			matrix V = e(V_iw)
			ereturn post b V

			di ""
			di "============================================================"
			di "  TEST 4: Distressed vs Non-distressed — DV: `dvlabel', Cluster: `clustlabel'"
			di "============================================================"

			lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
			di "  Distressed avg post-treatment:     " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

			lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
			di "  Non-distressed avg post-treatment: " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

			lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11 ///
				  - (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
			di "  DIFFERENCE (Distressed - Non-distressed): " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

			di ""
			di "  Period-by-period difference tests (Distressed - Non-distressed):"
			di "  ----------------------------------------------------------------"
			foreach k in 10 9 8 7 6 5 4 3 2 {
				lincom F_FH_`k'_g1 - F_FH_`k'_g0
				di "  t = -`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
			}
			foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
				lincom L_FH_`k'_g1 - L_FH_`k'_g0
				di "  t = +`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
			}

			test (L_FH_0_g1 - L_FH_0_g0 = 0) (L_FH_1_g1 - L_FH_1_g0 = 0) (L_FH_2_g1 - L_FH_2_g0 = 0) ///
				 (L_FH_3_g1 - L_FH_3_g0 = 0) (L_FH_4_g1 - L_FH_4_g0 = 0) (L_FH_5_g1 - L_FH_5_g0 = 0) ///
				 (L_FH_6_g1 - L_FH_6_g0 = 0) (L_FH_7_g1 - L_FH_7_g0 = 0) (L_FH_8_g1 - L_FH_8_g0 = 0) ///
				 (L_FH_9_g1 - L_FH_9_g0 = 0) (L_FH_10_g1 - L_FH_10_g0 = 0)
			di "  Joint F-test (all post-treatment diffs = 0): chi2 = " r(chi2) "  p = " r(p)
		}
	}

	drop F_FH_*_g0 F_FH_*_g1 L_FH_*_g0 L_FH_*_g1
	drop stateyear_distressed state_group_dist


*======================================================================*
*  COMPARISON 5: Low vs. High median household income
*  g0 = high income, g1 = low income
*======================================================================*

	** State-year x group FE and clustering var
	egen stateyear_lowmdi = group(stateyearencode lowmdi)
	egen state_group_mdi  = group(stateencode lowmdi)

	foreach k in 10 9 8 7 6 5 4 3 2 {
		gen F_FH_`k'_g0 = F_FH_`k' * (lowmdi == 0)
		gen F_FH_`k'_g1 = F_FH_`k' * (lowmdi == 1)
	}
	gen F_FH_1_g0 = 0
	gen F_FH_1_g1 = 0

	foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
		gen L_FH_`k'_g0 = L_FH_`k' * (lowmdi == 0)
		gen L_FH_`k'_g1 = L_FH_`k' * (lowmdi == 1)
	}

	foreach dv in death_opioid_allpop death_opioid_allpop_rate death_opioid_allpop_t death_opioid_allpop_rate_t {

		if "`dv'" == "death_opioid_allpop"        local dvlabel "Count"
		if "`dv'" == "death_opioid_allpop_rate"    local dvlabel "Rate"
		if "`dv'" == "death_opioid_allpop_t"       local dvlabel "IHS(Count)"
		if "`dv'" == "death_opioid_allpop_rate_t"  local dvlabel "IHS(Rate)"

		foreach clust in state_group_mdi {

			local clustlabel "State x Group"

			eventstudyinteract `dv' ///
				F_FH_10_g0 F_FH_9_g0 F_FH_8_g0 F_FH_7_g0 F_FH_6_g0 F_FH_5_g0 F_FH_4_g0 F_FH_3_g0 F_FH_2_g0 F_FH_1_g0 ///
				F_FH_10_g1 F_FH_9_g1 F_FH_8_g1 F_FH_7_g1 F_FH_6_g1 F_FH_5_g1 F_FH_4_g1 F_FH_3_g1 F_FH_2_g1 F_FH_1_g1 ///
				L_FH_*_g0 L_FH_*_g1, ///
				vce(cluster `clust') ///
				absorb(i.county_fips i.stateyear_lowmdi) ///
				cohort(fefl_fl1h_1st_year) ///
				control_cohort(never_fefl_fl1h) ///
				covariates($controlok)

			matrix b = e(b_iw)
			matrix V = e(V_iw)
			ereturn post b V

			di ""
			di "============================================================"
			di "  TEST 5: Low vs High MDI — DV: `dvlabel', Cluster: `clustlabel'"
			di "============================================================"

			lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11
			di "  Low-income avg post-treatment:  " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

			lincom (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
			di "  High-income avg post-treatment: " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

			lincom (L_FH_0_g1 + L_FH_1_g1 + L_FH_2_g1 + L_FH_3_g1 + L_FH_4_g1 + L_FH_5_g1 + L_FH_6_g1 + L_FH_7_g1 + L_FH_8_g1 + L_FH_9_g1 + L_FH_10_g1) / 11 ///
				  - (L_FH_0_g0 + L_FH_1_g0 + L_FH_2_g0 + L_FH_3_g0 + L_FH_4_g0 + L_FH_5_g0 + L_FH_6_g0 + L_FH_7_g0 + L_FH_8_g0 + L_FH_9_g0 + L_FH_10_g0) / 11
			di "  DIFFERENCE (Low-income - High-income): " r(estimate) " (SE = " r(se) ", p = " r(p) ")"

			di ""
			di "  Period-by-period difference tests (Low-income - High-income):"
			di "  --------------------------------------------------------------"
			foreach k in 10 9 8 7 6 5 4 3 2 {
				lincom F_FH_`k'_g1 - F_FH_`k'_g0
				di "  t = -`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
			}
			foreach k in 0 1 2 3 4 5 6 7 8 9 10 {
				lincom L_FH_`k'_g1 - L_FH_`k'_g0
				di "  t = +`k':  diff = " %7.3f r(estimate) "  SE = " %7.3f r(se) "  p = " %5.3f r(p)
			}

			test (L_FH_0_g1 - L_FH_0_g0 = 0) (L_FH_1_g1 - L_FH_1_g0 = 0) (L_FH_2_g1 - L_FH_2_g0 = 0) ///
				 (L_FH_3_g1 - L_FH_3_g0 = 0) (L_FH_4_g1 - L_FH_4_g0 = 0) (L_FH_5_g1 - L_FH_5_g0 = 0) ///
				 (L_FH_6_g1 - L_FH_6_g0 = 0) (L_FH_7_g1 - L_FH_7_g0 = 0) (L_FH_8_g1 - L_FH_8_g0 = 0) ///
				 (L_FH_9_g1 - L_FH_9_g0 = 0) (L_FH_10_g1 - L_FH_10_g0 = 0)
			di "  Joint F-test (all post-treatment diffs = 0): chi2 = " r(chi2) "  p = " r(p)
		}
	}


	di _n "02_subgroup_comparison.do complete."

	log close subcomp
