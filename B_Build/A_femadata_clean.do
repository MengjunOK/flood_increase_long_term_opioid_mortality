

*********************************
* Previous Clean for FEMA DATA **
*********************************


global  path         "Store Data Path in your computer"

cd     "$path/Flooding/FloodingDownload/FEMA_disasterder/raw"

global input  "$path/Flooding/FloodingDownload/FEMA_disasterder/build/input"
global output "$path/Flooding/FloodingDownload/FEMA_disasterder/build/output"
global temp   "$path/Flooding/FloodingDownload/FEMA_disasterder/build/temp" 


*******************************
* Deal with the main dataset  * 
*******************************

	**************** Use the Data and Clean the Data and Get the number of the Flooding Data **************** 

	use $input/disastersummary.dta, clear 

	gen disasteryear = substr(incidentbegindate,1,4) 
	destring disasteryear, replace 

	gen disastermonth = substr(incidentbegindate,6,2)
	destring disastermonth, replace 

	drop if  disasteryear<= 1969 | disasteryear==2024  

	// For incident type, get the flooding incident 
	tab incidenttype   // For different incident, I think these types are different. 


	*** I need to add the duration for this data. *** I need to follow the study for NOAA data ***
	/* for this data, I can also add the duration. I think it is good to add the duration for the data*/ 
	//incidentbegindate 
	//incidentenddate
	gen begindate = substr(incidentbegindate,1,10)
	gen enddate = substr(incidentenddate,1,10)

	gen begindate_d =  date(begindate,"YMD")
	gen enddate_d = date(enddate,"YMD")

	gen durationdays = enddate_d - begindate_d

	*** Flooding related data *** 
	//	gen flooding = 1  if     incidenttype =="Flood" | incidenttype=="Coastal Storm" | incidenttype=="Severe Storm" | incidenttype=="Hurricane" | incidenttype=="Typhoon" 
	/* 	gen flooding = 1  if     incidenttype=="Dam/Levee Break" |incidenttype=="Tornado"| incidenttype=="Tropical Storm"| /* incidenttype=="Tsunami" | */ incidenttype =="Flood" | incidenttype=="Coastal Storm" | incidenttype=="Severe Storm" | incidenttype=="Hurricane" | incidenttype=="Typhoon"        
	*/	
	/*     gen flooding = 1  if     incidenttype=="Dam/Levee Break" | incidenttype=="Tropical Storm"| /* incidenttype=="Tsunami" | */ incidenttype =="Flood" | incidenttype=="Coastal Storm" | incidenttype=="Severe Storm" | incidenttype=="Hurricane" | incidenttype=="Typhoon"        
	*/

    gen flooding = 1  if   incidenttype == "Flood" | incidenttype =="Hurricane" | incidenttype=="Severe Storm"    | incidenttype =="Typhoon"       | incidenttype == "Tornado"        
	

	// don't include the tornado // 



	gen strom = 1  if     incidenttype=="Tropical Storm" | incidenttype=="Coastal Storm"   | incidenttype=="Severe Storm" 
	gen hurricane = 1 if  incidenttype=="Hurricane" | incidenttype=="Typhoon"
	gen pureflood = 1 if  incidenttype=="Flood"

	gen drought = 1 if incidenttype=="Drought"
	gen winterwhether = 1 if incidenttype=="Freezing" | incidenttype=="Severe Ice Storm"  | incidenttype=="Snowstorm" | incidenttype=="Winter Storm"
	gen biochemical = 1 if incidenttype=="Biological" | incidenttype=="Chemical" | incidenttype=="Toxic Substances"
	gen fire = 1 if incidenttype=="Fire"
	gen earthquakevocanic = 1 if incidenttype=="Earthquake" | incidenttype=="Volcanic Eruption"  | incidenttype=="Tsunami"
	gen otherdd = 1 if /* /* incidenttype=="Fishing Losses" |  */  incidenttype=="Human Cause"  /*  | incidenttype=="Mud/Landslide" *//*     */  |  incidenttype=="Terrorist"  | incidenttype=="Other"  */  declarationtitle == "POWER OUTAGE"

	foreach var in flooding  strom  hurricane pureflood  drought  winterwhether biochemical fire earthquakevocanic otherdd {
	replace `var' = 0 if `var' ==. 
	}

	gen otherdisaster = 1 if flooding == 0  
	replace otherdisaster = 0 if flooding ==1 

	gen totaldisaster = 1  

	// For the flooding, there is large flooding and small flooding 
	tab declarationtype
	gen majorflooding =  1 if flooding==1 & declarationtype=="DR" 
	replace majorflooding = 0 if majorflooding==. 


	//format begindate_d	 %td
	//format enddate_d	 %td
	gen flood_duration_01 = 1 if flooding==1  & durationdays>=1 
	gen flood_duration_03 = 1 if flooding==1  & durationdays>=3  
	gen flood_duration_05 = 1 if flooding ==1 & durationdays>=5
	gen flood_duration_10 = 1 if flooding==1  & durationdays>=10 
	gen flood_duration_15 = 1 if flooding==1  & durationdays>=15  
	gen flood_duration_20 = 1 if flooding==1  & durationdays>=20
	gen flood_duration_25 = 1 if flooding==1  & durationdays>=25    
	gen flood_duration_30 = 1 if flooding==1  & durationdays>=30     

	gen otherevent_duration_01 = 1 if flooding==0  & durationdays>=1 
	gen otherevent_duration_03 = 1 if flooding==0  & durationdays>=3  
	gen otherevent_duration_05 = 1 if flooding==0  & durationdays>=5
	gen otherevent_duration_10 = 1 if flooding==0  & durationdays>=10 
	gen otherevent_duration_15 = 1 if flooding==0  & durationdays>=15 
	gen otherevent_duration_20 = 1 if flooding==0  & durationdays>=20 
	gen otherevent_duration_25 = 1 if flooding==0  & durationdays>=25 
	gen otherevent_duration_30 = 1 if flooding==0  & durationdays>=30 

	gen femf_duration_01 = 1 if majorflooding==1  & durationdays>=1 
	gen femf_duration_03 = 1 if majorflooding==1  & durationdays>=3  
	gen femf_duration_05 = 1 if majorflooding==1  & durationdays>=5
	gen femf_duration_10 = 1 if majorflooding==1  & durationdays>=10 
	gen femf_duration_15 = 1 if majorflooding==1  & durationdays>=15  
	gen femf_duration_20 = 1 if majorflooding==1  & durationdays>=20
	gen femf_duration_25 = 1 if majorflooding==1  & durationdays>=25    
	gen femf_duration_30 = 1 if majorflooding==1  & durationdays>=30     


	tostring fipsstatecode , gen(fipsstatecodestr) 
	tostring fipscountycode, gen (fipscountycodestr)

	forvalues i = 1/9 {
	replace fipsstatecodestr = "0`i'" if fipsstatecodestr=="`i'"
	}

	forvalues i = 0/9 {
	replace  fipscountycodestr = "00`i'" if fipscountycodestr=="`i'"
	}

	forvalues i=10/99{
	replace fipscountycodestr = "0`i'" if  fipscountycodestr=="`i'"
	}

	gen county_fips_str = fipsstatecodestr + fipscountycodestr

	destring county_fips_str, gen(county_fips)

	save $output/flood_FEMA_declare_nocollapse.dta, replace 



	preserve 
	collapse  (sum) flooding strom hurricane pureflood drought  winterwhether biochemical fire earthquakevocanic otherdd  otherdisaster totaldisaster  majorflooding   flood_durat*  femf_dura*  otherevent_durat*    , by(county_fips state disasteryear ) 
	rename flooding dr_flooding  
	rename otherdisaster dr_other 
	rename totaldisaster dr_total 
	rename majorflooding  dr_floodingmajor 
	rename disasteryear  year 
	rename flood_durat*  dr_dura*_flood 
	rename otherevent_durat* dr_dura*_other 
	tabstat dr_flooding dr_other dr_total  dr_floodingmajor , by(year)
	save $input/floodingfema_collapse.dta, replace 
	restore 

	preserve 
	rename disasteryear  year 
	collapse (sum) durationdays if flooding==1 , by(county_fips state year)
	rename  durationdays  durationdays_flooding 
	save $input/floodingfema_collapse_duration_flooding.dta, replace 
	restore 

	preserve 
	rename disasteryear  year 
	collapse (sum) durationdays if flooding==0 , by(county_fips state year)
	rename  durationdays  durationdays_other 
	save $input/floodingfema_collapse_duration_other.dta, replace 
	restore 

	use $input/floodingfema_collapse.dta, clear 
	merge m:1 county_fips state year   using  $input/floodingfema_collapse_duration_flooding.dta
	drop _merge  
	merge m:1 county_fips state  year  using $input/floodingfema_collapse_duration_other.dta 
	drop _merge 

	replace durationdays_flooding = 0  if durationdays_flooding==. 
	replace  durationdays_other = 0 if durationdays_other==. 
	duplicates drop county_fips  year , force 

	save $output/flooding_FEMA_declare.dta, replace 

	save $input/flooding_FEMA_declare.dta, replace



*******************************
******* Define the path *******
*******************************

global  path        "Store Data Path in your computer"
global  input 	     "$path/build/input"
global  output       "$path/build/output"
global  temp         "$path/build/temp"
global  countyyear   "$path/build/input/countybyyear"

cd  "$input"

************************************************
* Flooding data: FEMA flooding data 1990_2023  * 
************************************************

		* Step 1 : FEMA PANEL DATA 
		use  countyfipsonly_year_1960_2024.dta, clear 
		merge m:1 county_fips  year using  flooding_FEMA_declare.dta 
		drop if state == "AK" | state =="HI" | state =="PR" | state =="VI" | state =="AS" | state=="GU" | state=="MP" | state=="TT" 

		tab _merge 
		drop if _merge ==2 

		rename _merge  _mergefema 

		local floodfema  dr_*   durationdays_*  drought winterwhether biochemical fire earthquakevocanic otherdd hurricane pureflood strom 
		foreach var of varlist  `floodfema' {
		replace `var' = 0 if `var' ==. & _mergefema==1 
		}

		drop _mergefema 
		drop state

		* Step 2: Generate Dummy for Regressions 
		** Whether to define flooding as the flooding has at least 1 days? 
		** how to measure the severity of the flooding? 
		** 1. flooding vs major flooding  (whether has the major flooding this year)
		** 2. Duration of the flooding  (whether the year has the flooding event that greater than xx days; or whether the year has the sum of flooding days less or greater than some days )
		local   othervar  drought  winterwhether  biochemical  fire  earthquakevocanic  otherdd  hurricane  pureflood  strom 
		global  floodvar    `othervar'   dr_flooding dr_other dr_total dr_floodingmajor   dr_duraion_01_flood   dr_duraion_03_flood  dr_duraion_05_flood dr_duraion_10_flood dr_duraion_15_flood dr_duraion_20_flood dr_duraion_25_flood dr_duraion_30_flood dr_duraion_03_other dr_duraion_05_other dr_duraion_10_other dr_duraion_15_other dr_duraion_20_other dr_duraion_25_other dr_duraion_30_other  
		foreach i in $floodvar {
		gen `i'_dummy = 1 if `i'>=1 
		replace `i'_dummy = 0 if `i'==0 
		}


		** Indicator of less than or greater than  these days flooding 
		foreach i in  01  03  05  10  15  20  25  30  {
		gen dr_duraion_`i'_flood_no = 1  if dr_flooding_dummy==1 & dr_duraion_`i'_flood_dummy==0 
		replace dr_duraion_`i'_flood_no=0 if  dr_flooding_dummy==0 | dr_duraion_`i'_flood_dummy==1
		}

		rename  dr_flooding_dummy    fefl  
		label var fefl  "Whether the county year has the flooding event (FEMA)"

		rename  dr_floodingmajor_dummy  femf   
		label var  femf "Whether the county year has the major flooding event (FEMA)"

		foreach i in   01   03   05  10   15   20   25   30  {
		rename dr_duraion_`i'_flood_dummy  fefl_dg`i' 
		label var   fefl_dg`i'   "Whether the county year  has the flooding that duration last greater than `i' days"

		rename  dr_duraion_`i'_flood_no  fefl_dl`i'  
		label var  fefl_dl`i'  "whther the county year  has the  flooding that duration less than `i' days"   
		}




		* Step 3 : other durartion dummy (use the total duration days, not the single event duration days)
		foreach du in 1 3 5 10 15 20 25 30 {
		gen du_g`du' = 1 if durationdays_flooding>= `du'  & fefl==1
		replace du_g`du' = 0  if (durationdays_flooding<`du' & fefl==1) | fefl==0 

		gen du_l`du'=1 if durationdays_flooding< `du' & fefl==1 
		replace du_l`du'=0 if (durationdays_flooding>= `du' & fefl== 1) | fefl== 0 
		}

		// 0-5 days ; 5-10 days; 10+ days variables (all flooding)
		gen du_g0_10 = 1  if durationdays_flooding <= 10 & fefl ==1 
		replace du_g0_10 = 0  if (durationdays_flooding >10 & fefl==1) | fefl==0 

		gen du_g10_20 =1 if  durationdays_flooding>10 & durationdays_flooding<=20  & fefl==1 
		replace du_g10_20 = 0 if (durationdays_flooding>20 & fefl==1) | (durationdays_flooding<=10 & fefl==1) | fefl==0 

		// 10_35 days ; 35 days + variables 
		gen du_g10_30 = 1  if (durationdays_flooding < 30 & durationdays_flooding>= 10)  & fefl ==1 
		replace du_g10_30 = 0  if du_g10_30 == . 

		****** femf ****** 

		foreach du in 1 3 5 10 15 20 25 30 {
		gen femf_du_g`du' = 1 if durationdays_flooding>= `du'  & femf==1
		replace femf_du_g`du' = 0  if durationdays_flooding<`du' | femf==0 

		gen femf_du_l`du'=1 if durationdays_flooding< `du' & femf==1 
		replace femf_du_l`du'=0 if durationdays_flooding>= `du' | femf== 0 
		}

		// 0-5 days; 5-10 days; 10+ days variable (major flooding)
		gen femf_du_g0_10 = 1  if durationdays_flooding <= 10 & femf ==1 
		replace femf_du_g0_10 = 0  if (durationdays_flooding >10 & fefl==1) | femf ==0 

		gen femf_du_g10_20 =1 if  durationdays_flooding>10 & durationdays_flooding<=20  & femf==1 
		replace femf_du_g10_20 = 0 if (durationdays_flooding>20 & fefl==1) | (durationdays_flooding<=10 & femf==1) | femf==0 



		* Step 4: other nature disasters
		rename drought_dummy            d_drought 
		rename winterwhether_dummy      d_winter
		rename biochemical_dummy        d_bio
		rename fire_dummy               d_fire 
		rename earthquakevocanic_dummy  d_earth
		rename otherdd_dummy            d_otherdd

		rename hurricane_dummy          d_hurricane 
		rename pureflood_dummy          d_pureflood 
		rename strom_dummy              d_strom 
		rename dr_other_dummy           d_allother 
		rename dr_total_dummy           d_total 

		save  reog_flooding_FEMA_declare_countybalancepanel.dta, replace 




		use  reog_flooding_FEMA_declare_countybalancepanel.dta, clear 

		merge m:1 county_fips using countyruralurbancode_plusappa.dta

		keep if _merge == 3 
		drop _merge 
		keep if apparegion==1 

		tabstat d_allother
		tabstat fefl
		tabstat  d_drought  d_winter  d_bio d_fire d_earth  

		tab fefl  
		tab femf  
		tab du_g10  
		tab femf_du_g10  

		save FEMA_countyyear_balance_appa.dta, replace 



