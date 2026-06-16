
global  path         "Store Data Path in your computer"
global  input 	     "$path/build/input"
global  output       "$path/build/output"
global  temp         "$path/build/temp"
global  countyyear   "$path/build/input/countybyyear"

cd  "$input"


***************************************
* Other Data (Not the Flooding Data ) * 
***************************************



global  path        "Store Data Path in your computer"
global  input 	     "$path/build/input"
global  output       "$path/build/output"
global  temp         "$path/build/temp"
global  countyyear   "$path/build/input/countybyyear"

cd  "$input"


** Add the population data: pop-non-white ** 


***************************************
* Other Data (Not the Flooding Data ) * 
***************************************

	/* This part follows: datacombine_clean_floodtime0316_merge.do */

		********* Population Data *********
		** Contains: pop_allpop, pop by age/sex/race, rnetmig, domesticmig, etc.

		use population_new.dta, clear
		keep if year <= 2017 & year >= 1999 
		drop if state == "AK" | state =="HI" | state =="PR" | state =="VI" | state =="AS" | state=="GU" | state=="MP" | state=="TT" 

		********* Merge Deaths Data********** 

		   *** Merge opioid overdose deaths data 
			merge m:1 year county_fips  using county_9917_oddcount.dta
			tab _merge 
			drop if _merge ==2  // These not merged one is the state in AK or HI . The state_fips is 15 and 2. So it is only AK and HI. 
			foreach var of varlist deat*  con_* {
			replace `var' = 0 if `var' == . & _merge == 1
			}   
			// For deaths that only the 
			drop _merge      

			*** Merge suicide data ***
			merge m:1 year county_fips  using county_9917_suicide.dta
			tab _merge 
			drop if _merge ==2  // These not merged one is the state in AK or HI . The state_fips is 15 and 2. So it is only AK and HI. 
			foreach var of varlist deat*   {
			replace `var' = 0 if `var' == . & _merge == 1
			}   
			drop _merge  

			*** Merge alcohol data *** 
			merge m:1 year county_fips  using county_9917_alcohol.dta
			keep if year <= 2017 
			tab _merge 
			drop if _merge ==2  // These not merged one is the state in AK or HI . The state_fips is 15 and 2. So it is only AK and HI. 
			foreach var of varlist deat*   {
			replace `var' = 0 if `var' == . & _merge == 1
			}   
			drop _merge


			*** Merge alcohol data *** 
			merge m:1 year county_fips  using county_9917_alcoholp.dta     // this is alcohol overdose deaths data  //
			keep if year <= 2017 
			tab _merge 
			drop if _merge ==2  // These not merged one is the state in AK or HI . The state_fips is 15 and 2. So it is only AK and HI. 
			foreach var of varlist deat*   {
			replace `var' = 0 if `var' == . & _merge == 1
			}   
			// For deaths that only the 
			drop _merge

			******** Merge Deaths Data (greater than 25 years old by Education) *** 
			merge m:1  year county_fips  using  edu4level_greater25_wide.dta 
			drop if _merge ==2 
			foreach var of varlist deat*  con_* {
			replace `var' = 0 if `var' == . & _merge == 1
			}  
			drop _merge  

			*** merge alcohol data ***
			merge m:1  year county_fips  using  edu4level_greater25_alcohol.dta 
			drop if _merge ==2 
			foreach var of varlist deat*   {
			replace `var' = 0 if `var' == . & _merge == 1
			}  
			drop _merge  

			*** merge alcohol data ***
			merge m:1  year county_fips  using  edu4level_greater25_alcoholp.dta 
			drop if _merge ==2 
			foreach var of varlist deat*   {
			replace `var' = 0 if `var' == . & _merge == 1
			}  
			drop _merge  

			*** merge suicide data ***
			merge m:1  year county_fips  using  edu4level_greater25_suicide.dta 
			drop if _merge ==2 
			foreach var of varlist deat*   {
			replace `var' = 0 if `var' == . & _merge == 1
			}  
			drop _merge  

		******** Merge with Control Data *****
			merge m:1 year county_fips using laucnty_99_20.dta   // For the data that did not matched. For the using data, year after 2017 and state // 
			keep if _merge==3 
			drop _merge 

			merge m:1 year county_fips using povertymedianhh.dta   // I don't have the data from 2000, 2001, and 2002. I only have the year from 2003. 
			drop if _merge==2 
			drop _merge 

			merge m:1 year county_fips  using cleaned_personalincome.dta 
			drop if _merge==2 
			drop _merge 

			merge m:1 year county_fips  using acs_06_20_control.dta 
			drop if _merge==2 
			drop _merge   // For the control variable, it only has the data from 2017 ... 

		*** Merge with APPA geographic definition data ***
			merge m:1   county_fips  using  countyruralurbancode_plusappa.dta
			keep if _merge==3 
			drop _merge 

		**** Merge with community zone ***
			merge m:1  county_fips  using  cz2000.dta
			keep if _merge==3 
			drop _merge 

		**** Merge with spatial long and lat information *** 
			merge m:1 county_fips  using Final_county_spatialinformation.dta  	
			keep if _merge==3 
			drop _merge 
        
        *** Standard population 
			gen  stdp1019 = 39877 
			gen  stdp2029 = 35979 
			gen  stdp3039 = 41691 
			gen  stdp4049 = 42285 
			gen  stdp5059 = 30531 
			gen  stdp6069 = 20064 
			gen  stdp7079 = 16141 
			gen  stdpover80 = 9159
			gen  stdptotal = stdp1019 + stdp2029 + stdp3039 + stdp4049 + stdp5059 + stdp6069 + stdp7079 + stdpover80 

		   sort county_fips  year 

       	keep if year <= 2017 & year >= 1999 

   **********************************************
   **** Missing Values  for control variables ***
   **********************************************

	   //povertypct medianhhi

		sort county_fips  year  // 2000, 2001, 2002, 2003, 2004, 2005, 2006 
		* Missing values: popdensity occhouseunit occhouseunit_rentpct avghousesize edupct_lesshigh edupct_higherhigh edupct_somecollege edupct_higherbachelor edupct_highermaster edupct_higherprofessional edupct_higherphd laborforcepct nolaborforcepct workconstructionpct workmanufacturingpct housenotele
		global  varlist  popdensity occhouseunit occhouseunit_rentpct avghousesize edupct_lesshigh edupct_highschool edupct_somecollege edupct_higherbachelor edupct_highermaster edupct_higherprofessional edupct_higherphd laborforcepct nolaborforcepct workconstructionpct workmanufacturingpct housenotele

        foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}

		foreach i in  $varlist {
		bysort county_fips: replace `i'=`i'[_n+1] if missing(`i')
		}


		*** Other control variables *** 
	    rename  pop_all  pop_allpop

		gen pct_over65 = (pop_over65/pop_allpop)*100  
		gen pct_race_other = (pop_other/pop_allpop)*100 
		gen pct_race_black = (pop_black/pop_allpop)*100 
		gen pct_male = (pop_male/pop_allpop)*100 

	***************
	* Deaths Data *
	***************

		drop con_* 

		//  con_pre_her_allpop con_pre_syt_allpop con_pre_hersyt_allpop con_opi_sti_allpop con_opi_coc_allpop con_opi_cocsti_allpop con_2drug_allpop con_1drug_allpop
		global  typeodd   allpop male  female  white  black  other 2564 less25  2544 4564  over65 less10  1019 2029 3039 4049 5059 6069 7079 over80 
		global  typedrug  all  opioid  heroin  natural  methdone  cocaine  sythetic  stimulant pre  ictop suicide  alcohol  alcoholp
/* 		global  typecon   /* pre_her  pre_syt pre_hersyt  opi_sti  opi_coc   opi_cocsti */ 2drug 1drug 
 */     
        gen pop_less20 = pop_less10 + pop_1019 
		gen pop_over60 = pop_6069 + pop_7079 + pop_over80 
		gen pop_nonwhite = pop_other + pop_black 
		gen pop_2564 = pop_2544 + pop_4564 


		// Generate non-white ; 10 years age groups //
		foreach i in $typedrug {
		gen death_`i'_nonwhite = death_`i'_black + death_`i'_other 
		gen death_`i'_less20 = death_`i'_less10 + death_`i'_1019 
		gen death_`i'_over60 = death_`i'_6069 + death_`i'_7079+death_`i'_over80
		gen death_`i'_2564  = death_`i'_2544 + death_`i'_4564 
		}	
		

		global  typeodd  allpop male  female  white  nonwhite  black  other  2564   less25  2544 4564  over65 less10  1019 2029 3039 4049 5059 6069 7079 over80 less20  over60 
		// Deaths rate 
		foreach i in $typeodd  {
		foreach z in $typedrug {
		gen death_`z'_`i'_rate = (death_`z'_`i'/pop_`i')*100000 
		}
		}

        **** Standard deaths rate *** 
		foreach  z  in   $typedrug {
		foreach i in 1019 2029 3039 4049 5059 6069 7079 over80 {
			gen death_`z'_`i'e = death_`z'_`i'_rate*(stdp`i'/100000)
		}
		}

		foreach  z  in   $typedrug   {
		gen death_`z'_allpope = death_`z'_1019e +death_`z'_2029e +death_`z'_3039e +death_`z'_4049e +death_`z'_5059e +death_`z'_6069e +death_`z'_7079e +death_`z'_over80e
		gen death_`z'_allpop_stdrate = (death_`z'_allpope/stdptotal)*100000
		}


		// Deaths by Education 
		sum   edupct*

		** Do it until here again** 
		global  typedrug all  opioid  heroin  natural  methdone  cocaine  sythetic  stimulant pre   suicide  alcohol  alcoholp

		*** Use the deaths by education and that age greater than 25 years old ** 
		foreach z in $typedrug{
		gen  death_`z'_lessequalhigh = death_`z'_p_lesshigh +  death_`z'_p_highschool
		gen  death_`z'_higherhigh = death_`z'_p_college + death_`z'_p_higherba 
		}
	

		sum  edupct_lesshigh edupct_highschool edupct_somecollege edupct_higherbachelor edupct_highermaster edupct_higherprofessional edupct_higherphd
		gen  edupct_leh  =  edupct_lesshigh +edupct_highschool 
		gen  edupct_hh = edupct_somecollege + edupct_higherbachelor + edupct_highermaster +  edupct_higherprofessional + edupct_higherphd

		gen  edupct_lehpp =  (edupct_leh * (pop_allpop-pop_less25)) /100
		gen  edupct_hhpp =  (edupct_hh * (pop_allpop-pop_less25))/100 


		*** Not have high school education level ** 
		foreach z in $typedrug{
		gen  death_`z'_leh_rate = (death_`z'_lessequalhigh / edupct_lehpp)*100000 
		gen  death_`z'_hh_rate =  (death_`z'_higherhigh/edupct_hhpp)*100000 
		}


	************************
	* Other new variables  * 
	************************
		// Generate some more variables ***
		encode state, gen(stateencode)
		tostring year , gen(yearstr)
		gen stateyearstr = state+yearstr
		encode stateyearstr,gen(stateyearencode)


		tostring cz2000, gen(cz2000str)
		gen czyearstr = cz2000str+yearstr 
		encode czyearstr ,gen(czyearencode)


		//Rural vs Urban ****** 
		gen ruralregion = 1  if ruralurbancode==5 | ruralurbancode==6 
		replace ruralregion = 0  if ruralurbancode==1|ruralurbancode==2|ruralurbancode==3|ruralurbancode==4 

		gen urbanregion = 1  if ruralregion==0 
		replace urbanregion = 0 if ruralregion==1 

		// Combine with the coal county **** 
		merge m:1  county_fips  using  coalcounty.dta
		drop _merge 
		// For the coal county, the merged data is not the full dataset that encompassed all the county, so some merge equals to 2 // 

		merge m:1 county_fips  year using  coalcounty_byyear.dta 
		drop _merge 

		foreach i in coal_production  active_mine   active_mine_s    coal_worker {
		bysort county_fips : replace  `i'= `i'[_n+1]  if 	missing(`i')
		bysort county_fips : replace  `i'= `i'[_n+1]  if 	missing(`i')
		}
		save $output/reog_combine_noflood.dta, replace  




		use  $output/reog_combine_noflood.dta, clear 
		keep county_fips  year  medianhhi  unemployrate  povertypct  apparegion 

		keep if year == 2000 
		keep if apparegion == 1 

		collapse (mean) medianhhi unemployrate  povertypct  , by(county_fips  apparegion)

		rename  medianhhi      pctnation2000_medianhhi
		rename  unemployrate   pctnation2000_unemploy
		rename  povertypct     pctnation2000_poverty

		merge m:1 county_fips using $input/econ2000status.dta 

		drop _merge 

		keep  county_fips  pctnation2000_medianhhi  pctnation2000_unemploy  pctnation2000_poverty  econcate2002  econcate2003  econcate2004

		save  $input/nationeconomiclevel_compare.dta, replace 


		// Medicaid eligibility at 138% of FPL  // 



