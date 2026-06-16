

*********************************
* Combine Data Together
*
* Merges non-flood data (C_noflooddata_clean.do output) with
* flood event data (B_floodevent_clean.do output), plus
* additional county-level covariates.
*
* Final output: floodopioid_demo.dta (analysis dataset)
*********************************

	global  path         "Store Data Path in your computer"
	global  input 	     "$path/build/input"
	global  output       "$path/build/output"
	global  temp         "$path/build/temp"

	local timewindow     1999_2017

	use $output/reog_combine_noflood.dta, clear

	*** Merge with flooding data
	merge m:1  year  county_fips  using   $input/reog_NOAAFLOOD_data_regclean_`timewindow'.dta

	keep if _merge==3
	drop _merge


	*** Merge decennial population
	merge m:1  county_fips  using	$input/pop_dec.dta
	drop _merge

	*** Economy indicators
	foreach  econ  in  medianhhi  unemployrate  povertypct {
	bysort county_fips: egen  mean`econ' = mean(`econ')

	sum   mean`econ' , detail
	gen  high`econ' = 1  if  mean`econ'>=  r(p50)
	replace  high`econ' = 0 if  mean`econ' <r(p50)
	gen  low`econ'= 1 if   mean`econ' <r(p50)
	replace  low`econ'=0 if mean`econ' >= r(p50)
	}

	gen nocoalcounty = 1 if coalcounty==0
	replace nocoalcounty=0 if coalcounty==1

	gen centralappa=1 if appasubregion=="Central" | appasubregion=="North Central" | appasubregion=="South Central"
	replace centralappa = 0 if centralappa==.

	gen nocentralappa = 1 if centralappa==0
	replace nocentralappa=0 if centralappa==1

	*** Merge year-2000 national economic level comparisons
	merge m:1 county_fips  using   $input/nationeconomiclevel_compare.dta, nogen

	*** Keep Appalachian counties only
	keep if apparegion==1

	*** Merge FEMA other natural disaster controls
	merge m:1  county_fips year using   $input/femacontrol_othernaturaldisaster.dta ,nogen

	keep if apparegion ==1

	*** Select variables for analysis dataset
	keep year yearstr state state_fips county_fips ///
		pop_allpop pop_nonwhite pop_white pop_male pop_female ///
		pop_less25 pop_2544 pop_4564 pop_over65 pop_leh pop_hh ///
		death_alldrug_* death_opioid_* death_pre_* ///
		unemployrate povertypct medianhhi ///
		ruralurbancode appasubregion ruralregion ///
		fema_allother fema_allother_last1 fema_allother_last2 fema_allother_last3 ///
		fefl fl1d fl1h fefl_fl1h ///
		all_fl1d_1st_year all_fefl_fl1h_1st_year ///
		fl1d_1st_year fefl_fl1h_1st_year ///
		fl1d_2nd_year fefl_fl1h_2nd_year ///
		never_fl1d never_fefl_fl1h ///
		F_D_* F_FH_* L_D_* L_FH_* ///
		pctnation2000_medianhhi pctnation2000_unemploy pctnation2000_poverty ///
		econcate2002 econcate2003 econcate2004 ///
		rnetmig domesticmig rdomesticmig popest ///
		edupct_leh edupct_hh edupct_lehpp edupct_hhpp

	order year yearstr state state_fips county_fips ///
		pop_allpop pop_nonwhite pop_white pop_male pop_female ///
		pop_less25 pop_2544 pop_4564 pop_over65

	*** Save analysis dataset
	save $output/floodopioid_demo.dta, replace

	di _n "=== floodopioid_demo.dta saved ==="
	di "  Obs: " _N
	distinct county_fips
	tab year

