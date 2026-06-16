



global  path         "Store Data Path in your computer"
global  input 	     "$path/build/input"
global  output       "$path/build/output"
global  temp         "$path/build/temp"
global  countyyear   "$path/build/input/countybyyear"

cd  "$input"


****************************
* FEMA Data 
****************************

	* Data 1 : Individual-level data (APPA Region)
		//  fema_individual_appa.dta 
	* Data 2 : County-year balance panel data (APPA Region)
		// FEMA_countyyear_balance_appa.dta 


****************************
* NOAA Data 
****************************

	* Data 1 : Individual-level data (APPA Region)
		//  NOAA_countyyear_balance_appa.dta 
	* Data 2 : County-year balance panel data (APPA Region)
		// noaa_individual_appa.dta 

	use  fema_individual_appa.dta  , clear 

	tab incidenttype 

	use  FEMA_countyyear_balance_appa.dta , clear 


	use  NOAA_countyyear_balance_appa.dta, clear 

	use noaa_individual_appa.dta,clear 

	tab event_type 


	/*

	                EVENT_TYPE |      Freq.     Percent        Cum.
	---------------------------+-----------------------------------
	             Coastal Flood |          1        0.00        0.00
	               Debris Flow |        171        0.17        0.17
	               Flash Flood |     15,476       15.23       15.40
	                     Flood |      8,111        7.98       23.38
	                Heavy Rain |      2,034        2.00       25.38
	       Hurricane (Typhoon) |        116        0.11       25.50
	           Lakeshore Flood |         20        0.02       25.52
	          Marine High Wind |          1        0.00       25.52
	               Strong Wind |      2,519        2.48       28.00
	         Thunderstorm Wind |     69,007       67.91       95.91
	                   Tornado |      3,441        3.39       99.29
	       Tropical Depression |         65        0.06       99.36
	            Tropical Storm |        654        0.64      100.00
	---------------------------+-----------------------------------
	                     Total |    101,616      100.00
	*/


**************************************************************
*  For the Yearly Level Data, define the flooding incidents  * 
**************************************************************

  	use  FEMA_countyyear_balance_appa.dta , clear 

  	merge m:1  county_fips  year  using  NOAA_countyyear_balance_appa.dta 

  	drop _merge 


   	keep if year >= 1996 & year <= 2023 
 

  	*** For floods related events *** 
  	//fefl 
    // femf 
    //fl1d 
    // fl1h 

    * Define the strict flooding events 
    
    gen fefl_fl1d  = 1  if  fefl == 1 & fl1d ==1 
    replace  fefl_fl1d = 0  if fefl_fl1d==. 

    gen femf_fl1d  = 1  if  femf==1 & fl1d==1 
    replace  femf_fl1d = 0 if femf_fl1d ==. 

    gen fefl_fl1h = 1 if  fefl==1  & fl1h ==1 
    replace  fefl_fl1h = 0  if  fefl_fl1h ==. 

    gen femf_fl1h = 1 if  femf==1 &  fl1h==1 
    replace  femf_fl1h = 0  if femf_fl1h==. 


    label  var  fefl_fl1d  "Has FEMA declaratiin and also 1 deaths"
    label  var  femf_fl1d  "Has FEMA major disaster declaratiin and also 1 deaths"
    label  var  fefl_fl1h  "Has FEMA declaratiin and also 1 harms"
    label  var  femf_fl1h  "Has FEMA major declaratiin and also 1 harms"
    
    rename   flooding        noaa_fld 
    rename   totaldeaths     noaa_deaths
    rename   totalinjury     noaa_injury 
    rename   totalharm       noaa_harms 
    rename   durationdays    noaa_durations 
    rename   dr_flooding     fema_fld 
    rename   dr_floodingmajor   fema_majorfld 

    rename   strom               fema_strom 
    rename   hurricane           fema_hurricane 
    rename   pureflood           fema_flood 
    rename   drought             fema_drought 
    rename   winterwhether       fema_winter 
    rename   biochemical         fema_biochemical 
    rename   fire                fema_fire 
    rename   earthquakevocanic   fema_earth
    rename   dr_other            fema_allother
    rename   dr_total            fema_total 

    rename   durationdays_flooding  fema_durations 
    rename   durationdays_other     fema_durations_other 
    

    keep county county_fips year fema_fld fema_strom fema_hurricane fema_flood fema_drought fema_winter fema_biochemical fema_fire fema_earth   ///
    	 fema_allother fema_total fema_majorfld    fema_durations fema_durations_other    ///
    	 fefl   femf  ruralurbancode appasubregion apparegion noaa_fld noaa_deaths noaa_injury noaa_harms noaa_durations flna fl1d fl1h fefl_fl1d femf_fl1d fefl_fl1h femf_fl1h

    save  FEMA_NOAA_countyyear_balance.dta, replace 



 ***************************
 * New county year balance * 
 ***************************
   use  FEMA_NOAA_countyyear_balance.dta , clear 
   keep county_fips  year    fema_fld    fema_allother   ruralurbancode appasubregion apparegion 
   xtset county_fips year


   label var  fema_allother   "Number of other natural disasters in the year" 


   sort county_fips  year 
   bysort county_fips: gen fema_allother_last1 = L1.fema_allother
   bysort  county_fips: gen  fema_allother_last2 = L2.fema_allother  
   bysort  county_fips: gen  fema_allother_last3 = L3.fema_allother 

   keep if year >= 1999 & year <= 2017 
   keep if apparegion ==1 

   keep county_fips  year  fema_allother   fema_allother_last1 fema_allother_last2 fema_allother_last3 

   save  femacontrol_othernaturaldisaster.dta,replace 




*****************************************************
* Based on the new Data, Construct Relative Years   * 
*****************************************************
	
	* It is better to count after years 1999 * 

  	use  FEMA_NOAA_countyyear_balance.dta, clear 
  

/*     use FEMA_countyyear_balance_appa_withdeath1009.dta , clear 
  */
 
/* 	keep if year >= 1999 

	I think I need to drop the county has been hit by flood in 1996,1997 and 1998 
 */

	sort county_fips  year 

	bysort county_fips :  egen total_fefl= total(fefl)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl= sum(fefl)   // The number of the change of flooding 


	bysort county_fips :  egen total_femf = total(femf)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf = sum(femf)   // The number of the change of flooding 


	bysort county_fips :  egen total_fl1d = total(fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fl1d = sum(fl1d)   // The number of the change of flooding 


	bysort county_fips :  egen total_fl1h = total(fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fl1h = sum(fl1h)   // The number of the change of flooding 


	bysort county_fips :  egen total_fefl_fl1d = total(fefl_fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl_fl1d = sum(fefl_fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf_fl1d = total(femf_fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf_fl1d = sum(femf_fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_fefl_fl1h = total(fefl_fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl_fl1h = sum(fefl_fl1h)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf_fl1h = total(femf_fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf_fl1h = sum(femf_fl1h)   // The number of the change of flooding 


	bysort county_fips: egen all_fefl_1st_year = min(cond(fefl == 1 & numorder_fefl == 1, year, .))

	bysort county_fips: egen all_femf_1st_year = min(cond(femf == 1 & numorder_femf == 1, year, .))

	bysort county_fips: egen all_fl1h_1st_year = min(cond(fl1h == 1 & numorder_fl1h == 1, year, .))

	bysort county_fips: egen all_fl1d_1st_year = min(cond(fl1d == 1 & numorder_fl1d == 1, year, .))


	bysort county_fips: egen all_fefl_fl1d_1st_year = min(cond(fefl_fl1d == 1 & numorder_fefl_fl1d == 1, year, .))

	bysort county_fips: egen all_femf_fl1d_1st_year = min(cond(femf_fl1d == 1 & numorder_femf_fl1d == 1, year, .))

	bysort county_fips: egen all_fefl_fl1h_1st_year = min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 1, year, .))

	bysort county_fips: egen all_femf_fl1h_1st_year = min(cond(femf_fl1h == 1 & numorder_femf_fl1h == 1, year, .))

	drop   total_fefl  numorder_fefl  total_femf  numorder_femf   total_fl1d  numorder_fl1d   total_fl1h  numorder_fl1h     total_fefl_fl1d  numorder_fefl_fl1d  total_femf_fl1d   numorder_femf_fl1d   total_fefl_fl1h   numorder_fefl_fl1h   total_femf_fl1h   numorder_femf_fl1h   


	keep if year >= 1999

	bysort county_fips :  egen total_fefl= total(fefl)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl= sum(fefl)   // The number of the change of flooding 


	bysort county_fips :  egen total_femf = total(femf)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf = sum(femf)   // The number of the change of flooding 


	bysort county_fips :  egen total_fl1d = total(fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fl1d = sum(fl1d)   // The number of the change of flooding 


	bysort county_fips :  egen total_fl1h = total(fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fl1h = sum(fl1h)   // The number of the change of flooding 


	bysort county_fips :  egen total_fefl_fl1d = total(fefl_fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl_fl1d = sum(fefl_fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf_fl1d = total(femf_fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf_fl1d = sum(femf_fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_fefl_fl1h = total(fefl_fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl_fl1h = sum(fefl_fl1h)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf_fl1h = total(femf_fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf_fl1h = sum(femf_fl1h)   // The number of the change of flooding 

	bysort county_fips: egen all99_fefl_1st_year = min(cond(fefl == 1 & numorder_fefl == 1, year, .))

	bysort county_fips: egen all99_femf_1st_year = min(cond(femf == 1 & numorder_femf == 1, year, .))

	bysort county_fips: egen all99_fl1h_1st_year = min(cond(fl1h == 1 & numorder_fl1h == 1, year, .))

	bysort county_fips: egen all99_fl1d_1st_year = min(cond(fl1d == 1 & numorder_fl1d == 1, year, .))


	bysort county_fips: egen all99_fefl_fl1d_1st_year = min(cond(fefl_fl1d == 1 & numorder_fefl_fl1d == 1, year, .))

	bysort county_fips: egen all99_femf_fl1d_1st_year = min(cond(femf_fl1d == 1 & numorder_femf_fl1d == 1, year, .))

	bysort county_fips: egen all99_fefl_fl1h_1st_year = min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 1, year, .))

	bysort county_fips: egen all99_femf_fl1h_1st_year = min(cond(femf_fl1h == 1 & numorder_femf_fl1h == 1, year, .))

	drop   total_fefl  numorder_fefl  total_femf  numorder_femf   total_fl1d  numorder_fl1d   total_fl1h  numorder_fl1h     total_fefl_fl1d  numorder_fefl_fl1d  total_femf_fl1d   numorder_femf_fl1d   total_fefl_fl1h   numorder_fefl_fl1h   total_femf_fl1h   numorder_femf_fl1h   


*****************************
* Time Window 
******************************

	global  timewindow    1999_2017        

	foreach i in $timewindow{
	gen fl_`i'= "`i'"
	}

	foreach win in $timewindow{
	preserve 
	gen yearbegin = substr(fl_`win',1,4)
	gen yearend = substr(fl_`win',6,4) 

	destring yearend, replace 
	destring yearbegin, replace 

	keep if year<=yearend & year>=yearbegin 

	** Use fl1d to generate the treatments ** 
	sort county_fips  year 

	bysort county_fips :  egen total_fefl= total(fefl)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl= sum(fefl)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf = total(femf)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf = sum(femf)   // The number of the change of flooding 

	bysort county_fips :  egen total_fl1d = total(fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fl1d = sum(fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_fl1h = total(fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fl1h = sum(fl1h)   // The number of the change of flooding 


	bysort county_fips :  egen total_fefl_fl1d = total(fefl_fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl_fl1d = sum(fefl_fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf_fl1d = total(femf_fl1d)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf_fl1d = sum(femf_fl1d)   // The number of the change of flooding 

	bysort county_fips :  egen total_fefl_fl1h = total(fefl_fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_fefl_fl1h = sum(fefl_fl1h)   // The number of the change of flooding 

	bysort county_fips :  egen total_femf_fl1h = total(femf_fl1h)   // For each county, during 2000-2017, total of the flooding 
	bysort county_fips :  gen  numorder_femf_fl1h = sum(femf_fl1h)   // The number of the change of flooding 

   
   //  br total_fefl_fl1h    numorder_fefl_fl1h   fefl_fl1h_1st_year   fefl_fl1h_2nd_year   fefl_fl1h_3rd_year    fefl_fl1h_4th_year
    ** Generate the Hit Year ** 
    bysort county_fips: egen fefl_1st_year = min(cond(fefl == 1 & numorder_fefl == 1, year, .))
    
    bysort county_fips: egen femf_1st_year = min(cond(femf == 1 & numorder_femf == 1, year, .))
    
    bysort county_fips: egen fl1h_1st_year = min(cond(fl1h == 1 & numorder_fl1h == 1, year, .))
    
    bysort county_fips: egen fl1d_1st_year = min(cond(fl1d == 1 & numorder_fl1d == 1, year, .))


    bysort county_fips: egen fefl_fl1d_1st_year = min(cond(fefl_fl1d == 1 & numorder_fefl_fl1d == 1, year, .))
    
    bysort county_fips: egen femf_fl1d_1st_year = min(cond(femf_fl1d == 1 & numorder_femf_fl1d == 1, year, .))
    
    bysort county_fips: egen fefl_fl1h_1st_year = min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 1, year, .))
    
    bysort county_fips: egen femf_fl1h_1st_year = min(cond(femf_fl1h == 1 & numorder_femf_fl1h == 1, year, .))

    ** Generate the second hit year ** 
    bysort county_fips: egen fefl_2nd_year = min(cond(fefl == 1 & numorder_fefl == 2, year, .))
    
    bysort county_fips: egen femf_2nd_year = min(cond(femf == 1 & numorder_femf == 2, year, .))
    
    bysort county_fips: egen fl1h_2nd_year = min(cond(fl1h == 1 & numorder_fl1h == 2, year, .))
    
    bysort county_fips: egen fl1d_2nd_year = min(cond(fl1d == 1 & numorder_fl1d == 2, year, .))


    bysort county_fips: egen fefl_fl1d_2nd_year = min(cond(fefl_fl1d == 1 & numorder_fefl_fl1d == 2, year, .))
    
    bysort county_fips: egen femf_fl1d_2nd_year = min(cond(femf_fl1d == 1 & numorder_femf_fl1d == 2, year, .))
    
    bysort county_fips: egen fefl_fl1h_2nd_year = min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 2, year, .))
    
    bysort county_fips: egen femf_fl1h_2nd_year = min(cond(femf_fl1h == 1 & numorder_femf_fl1h == 2, year, .))

    ** Generate the third hit 

    bysort county_fips: egen fefl_3rd_year = min(cond(fefl == 1 & numorder_fefl == 3, year, .))
    
    bysort county_fips: egen femf_3rd_year = min(cond(femf == 1 & numorder_femf == 3, year, .))
    
    bysort county_fips: egen fl1h_3rd_year = min(cond(fl1h == 1 & numorder_fl1h == 3, year, .))
    
    bysort county_fips: egen fl1d_3rd_year = min(cond(fl1d == 1 & numorder_fl1d == 3, year, .))


    bysort county_fips: egen fefl_fl1d_3rd_year = min(cond(fefl_fl1d == 1 & numorder_fefl_fl1d == 3, year, .))
    
    bysort county_fips: egen femf_fl1d_3rd_year = min(cond(femf_fl1d == 1 & numorder_femf_fl1d == 3, year, .))
    
    bysort county_fips: egen fefl_fl1h_3rd_year = min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 3, year, .))
    
    bysort county_fips: egen femf_fl1h_3rd_year = min(cond(femf_fl1h == 1 & numorder_femf_fl1h == 3, year, .))

    ** Generate the forth hit

    bysort county_fips: egen fefl_4th_year = min(cond(fefl == 1 & numorder_fefl == 4, year, .))
    
    bysort county_fips: egen femf_4th_year = min(cond(femf == 1 & numorder_femf == 4, year, .))
   
    bysort county_fips: egen fl1h_4th_year = min(cond(fl1h == 1 & numorder_fl1h == 4, year, .))
    
    bysort county_fips: egen fl1d_4th_year = min(cond(fl1d == 1 & numorder_fl1d == 4, year, .))


    bysort county_fips: egen fefl_fl1d_4th_year = min(cond(fefl_fl1d == 1 & numorder_fefl_fl1d == 4, year, .))
    
    bysort county_fips: egen femf_fl1d_4th_year = min(cond(femf_fl1d == 1 & numorder_femf_fl1d == 4, year, .))
    
    bysort county_fips: egen fefl_fl1h_4th_year = min(cond(fefl_fl1h == 1 & numorder_fefl_fl1h == 4, year, .))
    
    bysort county_fips: egen femf_fl1h_4th_year = min(cond(femf_fl1h == 1 & numorder_femf_fl1h == 4, year, .))

    ** Generate the hardest hit
    bysort  county_fips: egen  noaaharmmost = max(noaa_harms) if  fefl_fl1h == 1
    bysort  county_fips: gen   mostyear = year if noaaharmmost == noaa_harms
    bysort  county_fips: egen  fefl_fl1h_most_year = min(mostyear)

	*** Relative Year *** 
	gen fefl_1st_year_ry  =  year   -  fefl_1st_year
	gen femf_1st_year_ry  =  year   -  femf_1st_year 
	gen fl1h_1st_year_ry  =  year   -  fl1h_1st_year 
	gen fl1d_1st_year_ry  =  year   -  fl1d_1st_year 


	gen fefl_fl1d_1st_year_ry  =  year   -  fefl_fl1d_1st_year
	gen femf_fl1d_1st_year_ry  =  year   -  femf_fl1d_1st_year 
	gen fefl_fl1h_1st_year_ry  =  year   -  fefl_fl1h_1st_year 
	gen femf_fl1h_1st_year_ry  =  year   -  femf_fl1h_1st_year 

	gen fefl_fl1h_most_year_ry = year - fefl_fl1h_most_year

	*** Never Treated Unit *** 
    gen never_fefl = (  fefl_1st_year ==.   )
	gen never_femf= (  femf_1st_year ==.   )
	gen never_fl1h = (  fl1h_1st_year ==.   )
	gen never_fl1d = (  fl1d_1st_year ==.   )


	gen never_fefl_fl1d = (  fefl_fl1d_1st_year ==.   )
	gen never_femf_fl1d = (  femf_fl1d_1st_year ==.   )
	gen never_fefl_fl1h = (  fefl_fl1h_1st_year ==.   )
	gen never_femf_fl1h = (  femf_fl1h_1st_year ==.   )


	*** Leads and Lags *** 
	// 1st 
	forvalues k = 26(-1)2 {

  gen F_F_`k' = fefl_1st_year_ry == -`k'
	gen F_M_`k' = femf_1st_year_ry == -`k'
	gen F_H_`k' = fl1h_1st_year_ry == -`k'
	gen F_D_`k' = fl1d_1st_year_ry == -`k'

	gen F_FD_`k' = fefl_fl1d_1st_year_ry == -`k'
	gen F_MD_`k' = femf_fl1d_1st_year_ry == -`k'
	gen F_FH_`k' = fefl_fl1h_1st_year_ry == -`k'
	gen F_MH_`k' = femf_fl1h_1st_year_ry == -`k'

	gen F_most_`k' =  fefl_fl1h_most_year_ry == -`k'

	}


	forvalues k = 0/26 {

	gen L_F_`k' = fefl_1st_year_ry == `k'
	gen L_M_`k' = femf_1st_year_ry == `k'
	gen L_H_`k' = fl1h_1st_year_ry == `k'
	gen L_D_`k' = fl1d_1st_year_ry == `k'


	gen L_FD_`k' = fefl_fl1d_1st_year_ry == `k'
	gen L_MD_`k' = femf_fl1d_1st_year_ry == `k'
	gen L_FH_`k' = fefl_fl1h_1st_year_ry == `k'
	gen L_MH_`k' = femf_fl1h_1st_year_ry == `k'

	gen L_most_`k' =  fefl_fl1h_most_year_ry == `k'


	}

	save  reog_NOAAFLOOD_data_regclean_`win'.dta , replace  
	restore 
	}




