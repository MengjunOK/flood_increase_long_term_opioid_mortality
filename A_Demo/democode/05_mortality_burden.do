
********************************************************************************
* 05_mortality_burden.do
*
* Mortality Burden Calculation — IHS(Count) Only
*
* Method: Exact inverse of IHS transformation, t=-1 baseline
*   excess_ik = [e^(delta_k + IHS(y_{-1})) - e^(-(delta_k + IHS(y_{-1})))] / 2 - y_{-1}
*
* Subgroups:
*   Subpop:    allpop, male, female, white, nonwhite, leh, hh
*   Subsample: distressed=1/0, lowmdi=1/0
*
* Includes repeat flood stacking (2nd, 3rd, 4th floods)
* Delta-method CIs using full VCV (Norton 2022; Bellemare & Wichman 2020)
*
* Outputs:
*   $tables/burden_ihsc_*.dta          — county-level burden by subgroup
*   $tables/burden_ihsc_all_subgroups.dta — combined county-level burden
*   $tables/burden_ihsc_summary.csv    — summary with 95% CIs
*
* Requires: eventstudyinteract package
********************************************************************************

	cap log close
	log using "$logs/05_mortality_burden.log", replace


*----------------------------------------------------------------------
* STEP 1: Prepare data
*----------------------------------------------------------------------

	use $data/floodopioid_demo.dta, clear

	** IHS-transformed death counts
	foreach pop in allpop male female white nonwhite leh hh {
		gen death_opioid_`pop'_t = asinh(death_opioid_`pop')
	}

	** Event Study time Indicators: bin into ±10+
	egen F_FH_10p = rowmax(F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18)
	egen L_FH_10p = rowmax(L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17)
	drop F_FH_10 F_FH_11 F_FH_12 F_FH_13 F_FH_14 F_FH_15 F_FH_16 F_FH_17 F_FH_18 F_FH_19 F_FH_20 F_FH_21 F_FH_22 F_FH_23 F_FH_24 F_FH_25 F_FH_26
	drop L_FH_10 L_FH_11 L_FH_12 L_FH_13 L_FH_14 L_FH_15 L_FH_16 L_FH_17 L_FH_18 L_FH_19 L_FH_20 L_FH_21 L_FH_22 L_FH_23 L_FH_24 L_FH_25 L_FH_26
	rename F_FH_10p F_FH_10
	rename L_FH_10p L_FH_10
	gen F_FH_1 = 0

	** Sample
	drop if all_fefl_fl1h_1st_year <= 1999
	keep if ruralregion == 1

	** Subsample indicators
	gen distressed = (econcate2002=="Distressed" | econcate2003=="Distressed" | econcate2004=="Distressed")
	gen lowmdi = (pctnation2000_medianhhi <= 30251)

	** Event time (1st flood)
	gen event_time = year - fefl_fl1h_1st_year

	** Construct repeat flood years
	bysort county_fips: gen numorder_fefl_fl1h = sum(fefl_fl1h)
	cap confirm variable fefl_fl1h_2nd_year
	if _rc != 0 {
		bysort county_fips: egen fefl_fl1h_2nd_year = ///
			min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 2, year, .))
	}
	cap confirm variable fefl_fl1h_3rd_year
	if _rc != 0 {
		bysort county_fips: egen fefl_fl1h_3rd_year = ///
			min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 3, year, .))
	}
	cap confirm variable fefl_fl1h_4th_year
	if _rc != 0 {
		bysort county_fips: egen fefl_fl1h_4th_year = ///
			min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 4, year, .))
	}
	drop numorder_fefl_fl1h

	** Event times for repeat floods
	foreach fl in 2nd 3rd 4th {
		qui count if fefl_fl1h_`fl'_year != .
		if r(N) > 0 {
			gen event_time_`fl' = year - fefl_fl1h_`fl'_year ///
				if fefl_fl1h_`fl'_year != .
		}
	}

	** Save full dataset
	tempfile fulldata
	save `fulldata'


*----------------------------------------------------------------------
* STEP 2: Run IHS(count) regressions (regression sample)
*----------------------------------------------------------------------

	drop if year >= fefl_fl1h_2nd_year

	encode state, gen(stateencode)
	gen stateyearstr = state + yearstr
	encode stateyearstr, gen(stateyearencode)

	global controlok pop_allpop povertypct medianhhi unemployrate ///
		fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3
	global timeind F_FH_10 F_FH_9 F_FH_8 F_FH_7 F_FH_6 F_FH_5 ///
		F_FH_4 F_FH_3 F_FH_2 F_FH_1

	** Subgroup definitions
	local grplist  "allpop male female white nonwhite leh hh dist1 dist0 lmdi1 lmdi0"
	local poplist  "allpop male female white nonwhite leh hh allpop allpop allpop allpop"
	local condlist "none none none none none none none distressed distressed lowmdi lowmdi"
	local vallist  "0 0 0 0 0 0 0 1 0 1 0"

	local ng : word count `grplist'

	di _n "=================================================================="
	di    "  STEP 2: Running IHS(count) regressions for `ng' subgroups"
	di    "=================================================================="

	forvalues g = 1/`ng' {
		local grp  : word `g' of `grplist'
		local pop  : word `g' of `poplist'
		local cond : word `g' of `condlist'
		local val  : word `g' of `vallist'

		if "`cond'" == "none" {
			local ifcond ""
		}
		else {
			local ifcond "if `cond'==`val'"
		}

		di _n "--- [`g'/`ng'] `grp': IHS(death_opioid_`pop') `ifcond' ---"

		qui eventstudyinteract death_opioid_`pop'_t $timeind L_FH_* `ifcond', ///
			vce(cluster county_fips) absorb(i.county_fips i.stateyearencode) ///
			cohort(fefl_fl1h_1st_year) control_cohort(never_fefl_fl1h) ///
			covariates($controlok)
		matrix coef_`grp' = e(b_iw)
		matrix vcov_`grp' = e(V_iw)

		di "  Done: `grp'"
	}

	di _n "All `ng' regressions complete."


*----------------------------------------------------------------------
* STEP 3: Compute burden for each subgroup
*----------------------------------------------------------------------

	di _n "=================================================================="
	di    "  STEP 3: Computing IHS(count) burden for all subgroups"
	di    "=================================================================="

	forvalues g = 1/`ng' {
		local grp  : word `g' of `grplist'
		local pop  : word `g' of `poplist'
		local cond : word `g' of `condlist'
		local val  : word `g' of `vallist'

		local death_var "death_opioid_`pop'"

		di _n "--- [`g'/`ng'] Burden for: `grp' ---"

		** Reload full data
		use `fulldata', clear

		** Apply subsample restriction
		if "`cond'" != "none" {
			keep if `cond' == `val'
		}

		** ---- Extract coefficients ----
		forvalues k = 0/9 {
			local ck = colnumb(coef_`grp', "L_FH_`k'")
			scalar b_`k' = coef_`grp'[1, `ck']
		}
		* 10+ bin: use L_FH_10 coefficient for k = 10,...,17
		forvalues k = 10/17 {
			local ck = colnumb(coef_`grp', "L_FH_10")
			scalar b_`k' = coef_`grp'[1, `ck']
		}

		** ---- Compute t=-1 baseline (y_{-1}) ----
		bysort county_fips: egen ybar_m1 = ///
			mean(cond(event_time == -1, `death_var', .))

		** Keep only flooded counties
		keep if never_fefl_fl1h == 0

		** ---- IHS of baseline ----
		gen ihs_ybar_m1 = ln(ybar_m1 + sqrt(ybar_m1^2 + 1))

		** ---- Repeat flood t=-1 baselines ----
		foreach fl in 2nd 3rd 4th {
			cap confirm variable event_time_`fl'
			if _rc != 0 continue

			bysort county_fips: egen ybar_m1_`fl' = ///
				mean(cond(event_time_`fl' == -1, `death_var', .))
			gen ihs_ybar_m1_`fl' = ///
				ln(ybar_m1_`fl' + sqrt(ybar_m1_`fl'^2 + 1))
		}

		** ---- Keep post-flood years ----
		keep if event_time >= 0

		** ---- Assign betas to observations ----
		gen beta = .
		forvalues k = 0/17 {
			replace beta = b_`k' if event_time == `k'
		}

		* Repeat flood betas
		foreach fl in 2nd 3rd 4th {
			cap confirm variable event_time_`fl'
			if _rc != 0 continue

			gen beta_`fl' = .
			forvalues k = 0/17 {
				replace beta_`fl' = b_`k' if event_time_`fl' == `k'
			}
		}


		** ============================================================
		** Compute excess deaths: IHS(count) exact inverse
		**
		**   excess_ik = [e^(delta_k + IHS(y_{-1})) - e^(-(delta_k + IHS(y_{-1})))] / 2
		**               - y_{-1}
		** ============================================================

		gen excess = (exp(ihs_ybar_m1 + beta) ///
			- exp(-(ihs_ybar_m1 + beta))) / 2 ///
			- ybar_m1


		** ============================================================
		** Delta-method weights for 1st flood
		**
		**   w_k = sum_i [ e^(delta_k + IHS(y_{i,-1})) + e^(-(delta_k + IHS(y_{i,-1}))) ] / 2
		**
		**   This is the derivative of excess w.r.t. delta_k
		**   (same formula as excess but with PLUS instead of MINUS)
		** ============================================================

		forvalues kb = 0/10 {
			if `kb' < 10 {
				gen _tmp_w = ((exp(ihs_ybar_m1 + beta) ///
					+ exp(-(ihs_ybar_m1 + beta))) / 2) ///
					if event_time == `kb' & beta != .
			}
			else {
				gen _tmp_w = ((exp(ihs_ybar_m1 + beta) ///
					+ exp(-(ihs_ybar_m1 + beta))) / 2) ///
					if event_time >= 10 & beta != .
			}
			qui sum _tmp_w
			local w_`kb' = r(sum)
			drop _tmp_w
		}

		** Build 11x1 weight vector
		matrix w = J(11, 1, 0)
		forvalues kb = 0/10 {
			local row = `kb' + 1
			matrix w[`row', 1] = `w_`kb''
		}

		** Extract 11x11 VCV submatrix for L_FH_0,...,L_FH_10
		matrix V = J(11, 11, 0)
		forvalues r = 0/10 {
			local cr = colnumb(coef_`grp', "L_FH_`r'")
			forvalues c = 0/10 {
				local cc = colnumb(coef_`grp', "L_FH_`c'")
				matrix V[`r'+1, `c'+1] = vcov_`grp'[`cr', `cc']
			}
		}

		** Compute Var(burden) = w'Vw for 1st flood
		matrix var_b = w' * V * w
		scalar se_`grp' = sqrt(var_b[1,1])

		di "  1st-flood SE: " %7.0f se_`grp'


		** ============================================================
		** Repeat flood weights (add to 1st flood weights)
		** ============================================================

		matrix w_tot = w

		foreach fl in 2nd 3rd 4th {
			cap confirm variable event_time_`fl'
			if _rc != 0 continue

			forvalues kb = 0/10 {
				if `kb' < 10 {
					cap gen _tmp_rw = ((exp(ihs_ybar_m1 + beta_`fl') ///
						+ exp(-(ihs_ybar_m1 + beta_`fl'))) / 2) ///
						if event_time_`fl' == `kb' & beta_`fl' != .
				}
				else {
					cap gen _tmp_rw = ((exp(ihs_ybar_m1 + beta_`fl') ///
						+ exp(-(ihs_ybar_m1 + beta_`fl'))) / 2) ///
						if event_time_`fl' >= 10 & beta_`fl' != .
				}

				local row = `kb' + 1
				qui sum _tmp_rw
				matrix w_tot[`row', 1] = w_tot[`row', 1] + r(sum)
				cap drop _tmp_rw
			}
		}

		** Total Var(burden) = w_tot' V w_tot
		matrix var_t = w_tot' * V * w_tot
		scalar se_`grp'_total = sqrt(var_t[1,1])

		di "  Total SE:     " %7.0f se_`grp'_total


		** ============================================================
		** Repeat flood excess deaths
		** ============================================================

		gen excess_rpt = 0

		foreach fl in 2nd 3rd 4th {
			cap confirm variable event_time_`fl'
			if _rc != 0 continue

			replace excess_rpt = excess_rpt ///
				+ (exp(ihs_ybar_m1_`fl' + beta_`fl') ///
				   - exp(-(ihs_ybar_m1_`fl' + beta_`fl'))) / 2 ///
				- ybar_m1_`fl' ///
				if event_time_`fl' >= 0 & beta_`fl' != . ///
				& ybar_m1_`fl' != .
		}

		* Total = 1st flood + repeat
		gen excess_total = excess + excess_rpt


		** ============================================================
		** Aggregate and store results
		** ============================================================

		* Count flooded counties
		qui egen tag_cty = tag(county_fips)
		qui count if tag_cty == 1
		scalar s_`grp'_ncty = r(N)

		* Store aggregate burden
		qui sum excess
		scalar s_`grp'_burden = r(sum)
		qui sum excess_total
		scalar s_`grp'_burden_total = r(sum)

		* Save county-level burden
		collapse (sum) excess excess_rpt excess_total ///
			(first) fefl_fl1h_1st_year state, ///
			by(county_fips)
		gen subgroup = "`grp'"

		save "$tables/burden_ihsc_`grp'.dta", replace

		di "  `grp': " %3.0f s_`grp'_ncty " counties, " ///
			"burden=" %7.0f s_`grp'_burden ///
			" (total=" %7.0f s_`grp'_burden_total ")"
	}


*----------------------------------------------------------------------
* STEP 4: Combine county-level files
*----------------------------------------------------------------------

	clear
	foreach grp in `grplist' {
		append using "$tables/burden_ihsc_`grp'.dta"
	}
	save "$tables/burden_ihsc_all_subgroups.dta", replace


*----------------------------------------------------------------------
* STEP 5: Summary table
*----------------------------------------------------------------------

	di _n "======================================================================"
	di    "  MORTALITY BURDEN: IHS(Count) — All Subgroups"
	di    "======================================================================"

	di _n "  Subgroup  | Cnty |  1st Flood  |    Total    |   SE (total)"
	di    "  ----------|------|-------------|-------------|-------------"

	foreach grp in `grplist' {
		di "  " %9s "`grp'" " | " %3.0f s_`grp'_ncty "  | " ///
			%10.0f s_`grp'_burden "  | " ///
			%10.0f s_`grp'_burden_total "  | " ///
			%10.0f se_`grp'_total
	}


*----------------------------------------------------------------------
* STEP 6: Export CSV with 95% CIs
*----------------------------------------------------------------------

	** Reload raw data for observed OOD
	use $data/floodopioid_demo.dta, clear
	drop if all_fefl_fl1h_1st_year <= 1999
	keep if ruralregion == 1
	gen distressed = (econcate2002=="Distressed" | econcate2003=="Distressed" | econcate2004=="Distressed")
	gen lowmdi = (pctnation2000_medianhhi <= 30251)
	gen flooded = (fefl_fl1h_1st_year != .)

	local grplist   "allpop male female white nonwhite leh hh dist1 dist0 lmdi1 lmdi0"
	local deathlist "allpop male female white nonwhite leh hh allpop allpop allpop allpop"
	local condlist  "none none none none none none none distressed distressed lowmdi lowmdi"
	local vallist   "0 0 0 0 0 0 0 1 0 1 0"
	local ng : word count `grplist'

	forvalues i = 1/`ng' {
		local grp  : word `i' of `grplist'
		local dpop : word `i' of `deathlist'
		local cond : word `i' of `condlist'
		local val  : word `i' of `vallist'

		if "`cond'" == "none" {
			qui sum death_opioid_`dpop' if flooded == 1
		}
		else {
			qui sum death_opioid_`dpop' if flooded == 1 & `cond' == `val'
		}
		scalar ood_flooded_`grp' = r(sum)

		if "`cond'" == "none" {
			qui sum death_opioid_`dpop'
		}
		else {
			qui sum death_opioid_`dpop' if `cond' == `val'
		}
		scalar ood_all_`grp' = r(sum)
	}

	** ---- Write CSV ----
	capture file close csvout
	file open csvout using "$tables/burden_ihsc_summary.csv", write replace

	** Header
	file write csvout ///
		"Flood Scope,Subgroup,Counties," ///
		"IHS Count Burden,IHS Count 95% CI," ///
		"Observed OOD (all)" _n

	** Readable subgroup names
	local lbl_allpop   "All Population"
	local lbl_male     "Male"
	local lbl_female   "Female"
	local lbl_white    "White"
	local lbl_nonwhite "Non-White"
	local lbl_leh      "Low Education"
	local lbl_hh       "High Education"
	local lbl_dist1    "Distressed"
	local lbl_dist0    "Non-Distressed"
	local lbl_lmdi1    "Low MDI"
	local lbl_lmdi0    "High MDI"

	foreach grp in allpop male female white nonwhite leh hh dist1 dist0 lmdi1 lmdi0 {
		local oof : di %10.0f ood_flooded_`grp'
		local ooa : di %10.0f ood_all_`grp'
		local oof = strtrim("`oof'")
		local ooa = strtrim("`ooa'")
		local nc  : di %3.0f s_`grp'_ncty
		local nc  = strtrim("`nc'")

		** ---- 1st flood row ----
		local bdn : di %8.0f s_`grp'_burden
		local lo  : di %8.0f (s_`grp'_burden - 1.96 * se_`grp')
		local hi  : di %8.0f (s_`grp'_burden + 1.96 * se_`grp')
		foreach x in bdn lo hi {
			local `x' = strtrim("``x''")
		}

		file write csvout ///
			`"1st Flood Only,`lbl_`grp'',`nc',"' ///
			`"`bdn',"[`lo'; `hi']","' ///
			`"`ooa'"' _n

		** ---- Total row ----
		local bdn : di %8.0f s_`grp'_burden_total
		local lo  : di %8.0f (s_`grp'_burden_total - 1.96 * se_`grp'_total)
		local hi  : di %8.0f (s_`grp'_burden_total + 1.96 * se_`grp'_total)
		foreach x in bdn lo hi {
			local `x' = strtrim("``x''")
		}

		file write csvout ///
			`"Total (1st + Repeat),`lbl_`grp'',`nc',"' ///
			`"`bdn',"[`lo'; `hi']","' ///
			`"`ooa'"' _n
	}

	file close csvout
	di _n "CSV written to: $tables/burden_ihsc_summary.csv"

	log close
