

		*********************
		** Define the Path **
		*********************

        global  path         "Store Data Path in your computer"

		cd     "$path/Flooding/FloodingDownload/NOAA_stromevent/Build/input/details"

		global input  "$path/Flooding/FloodingDownload/NOAA_stromevent/Build/input/details"
		global output "$path/Flooding/FloodingDownload/NOAA_stromevent/Build/output"


		*********************
		* Combine the data  * 
		*********************
		/* use  detail_1996.dta, clear 
		append  using  detail_1997.dta 
		append  using  detail_1998.dta 
		append  using  detail_1999.dta  
		append  using  detail_2000.dta 
		append  using  detail_2001.dta   
		append  using  detail_2002.dta 
		append  using  detail_2003.dta 
		append  using  detail_2004.dta 
		append  using  detail_2005.dta
		append  using  detail_2006.dta ,force 
		append  using  detail_2007.dta ,force 
		append  using  detail_2008.dta  , force 
		append  using  detail_2009.dta , force 
		append  using  detail_2010.dta ,force 
		append  using  detail_2011.dta ,force 
		append  using  detail_2012.dta ,force 
		append  using  detail_2013.dta ,force 
		append  using  detail_2014.dta ,force 
		append  using  detail_2015.dta ,force 
		append  using  detail_2016.dta ,force 
		append  using  detail_2017.dta ,force 
		append  using  detail_2018.dta ,force 
		append  using  detail_2019.dta ,force 
		append  using  detail_2020.dta ,force 
		append  using  detail_2021.dta ,force 
		append  using  detail_2022.dta ,force 
		append  using  detail_2023.dta ,force 
		save  $input/detail_allcombine.dta, replace  */


		*****************************
		* Define the flooding types * 
		*****************************

		use    $input/detail_allcombine.dta ,clear 

		// Define the Flood Types  
		tab event_type 

		/*
					                EVENT_TYPE |      Freq.     Percent        Cum.
			---------------------------+-----------------------------------
			     Astronomical Low Tide |        665        0.04        0.04
			                 Avalanche |        795        0.05        0.09
			                  Blizzard |     15,966        0.98        1.07
			             Coastal Flood |      3,874        0.24        1.30
			           Cold/Wind Chill |     16,199        0.99        2.30
			               Debris Flow |      2,163        0.13        2.43
			                 Dense Fog |     15,778        0.97        3.40
			               Dense Smoke |        147        0.01        3.40
			                   Drought |     73,288        4.49        7.89
			                Dust Devil |        245        0.02        7.91
			                Dust Storm |      1,644        0.10        8.01
			            Excessive Heat |     16,656        1.02        9.03
			   Extreme Cold/Wind Chill |     15,678        0.96        9.99
			               Flash Flood |     99,980        6.12       16.11
			                     Flood |     64,950        3.98       20.09
			              Freezing Fog |        445        0.03       20.12
			              Frost/Freeze |     14,420        0.88       21.00
			              Funnel Cloud |      9,469        0.58       21.58
			                      Hail |    319,443       19.56       41.14
			                      Heat |     28,707        1.76       42.90
			                Heavy Rain |     29,889        1.83       44.73
			                Heavy Snow |     70,815        4.34       49.07
			                 High Surf |     10,425        0.64       49.71
			                 High Wind |     86,759        5.31       55.02
			                 Hurricane |        274        0.02       55.04
			       Hurricane (Typhoon) |      1,799        0.11       55.15
			                 Ice Storm |     12,166        0.75       55.89
			          Lake-Effect Snow |      2,537        0.16       56.05
			           Lakeshore Flood |        357        0.02       56.07
			                 Lightning |     17,684        1.08       57.15
			          Marine Dense Fog |         17        0.00       57.15
			               Marine Hail |        809        0.05       57.20
			          Marine High Wind |        793        0.05       57.25
			  Marine Hurricane/Typhoon |         98        0.01       57.26
			          Marine Lightning |          2        0.00       57.26
			        Marine Strong Wind |        157        0.01       57.27
			  Marine Thunderstorm Wind |     38,584        2.36       59.63
			Marine Tropical Depression |         29        0.00       59.63
			     Marine Tropical Storm |        511        0.03       59.66
			           Northern Lights |          8        0.00       59.66
			               Rip Current |      1,735        0.11       59.77
			                    Seiche |         71        0.00       59.77
			                     Sleet |        838        0.05       59.83
			               Sneakerwave |         25        0.00       59.83
			          Storm Surge/Tide |      1,580        0.10       59.92
			               Strong Wind |     25,512        1.56       61.49
			         Thunderstorm Wind |    408,434       25.01       86.50
			                   Tornado |     38,939        2.38       88.88
			       Tropical Depression |        513        0.03       88.92
			            Tropical Storm |      6,592        0.40       89.32
			                   Tsunami |         42        0.00       89.32
			              Volcanic Ash |         70        0.00       89.33
			          Volcanic Ashfall |         77        0.00       89.33
			                Waterspout |      5,884        0.36       89.69
			                  Wildfire |      8,624        0.53       90.22
			              Winter Storm |     84,890        5.20       95.42
			            Winter Weather |     74,800        4.58      100.00
			---------------------------+-----------------------------------
			                     Total |  1,632,851      100.00

		*/

		* Flooding related events types 

/* 		global flooddefine event_type=="Coastal Flood" | event_type=="Debris Flow" | event_type =="Flash Flood"  |  ///
		event_type=="Flood" | event_type=="Heavy Rain" | event_type =="Hurricane"    |  ///
		event_type=="Hurricane (Typhoon)" | event_type=="Lakeshore Flood" | event_type =="Marine High Wind"   |  ///
		event_type=="Marine Hurricane/Typhoon" | event_type=="Marine Strong Wind" | event_type =="Marine Thunderstorm Wind"  |  ///
		event_type=="Marine Tropical Depression" | event_type=="Marine Tropical Storm" | event_type =="Strong Wind"   |  ///
		event_type=="Thunderstorm Wind" | event_type=="Tornado" | event_type =="Tropical Depression"  |   ///
		event_type=="Tropical Storm"   */

       


 		global flooddefine event_type=="Coastal Flood"       | event_type=="Debris Flow"             | event_type =="Flash Flood"    |  ///
						   event_type=="Flood"               | event_type=="Heavy Rain"              | event_type =="Hurricane"      | ///
						   event_type=="Hurricane (Typhoon)" |  event_type=="Lakeshore Flood"        |  event_type =="Strong Wind"   |  ///
						   event_type=="Thunderstorm Wind"   |  event_type =="Tropical Depression"   |   ///
						   event_type=="Tropical Storm"      |  event_type=="Tornado"                    // not include tornado and tsunami 
					 




    /* 		global flooddefine event_type=="Coastal Flood" | event_type=="Flash Flood" | event_type =="Flood" | event_type=="Heavy Rain" | event_type=="Hurricane" | event_type=="Hurricane (Typhoon)" |     ///
			               event_type=="Lakeshore Flood" | event_type=="Marine Hurricane/Typhoon" | event_type=="Marine Tropical Depression" |       ///
			               event_type=="Marine Tropical Storm" | event_type=="Tropical Depression" | event_type=="Tropical Storm"   
   */


		gen flooding = 1 if  $flooddefine

		tab event_type if flooding==1 


		tab  event_type 


		// Clean the Data  

		/* 	keep if flooding == 1 
		*/

		tostring begin_yearmonth , replace 
		tostring begin_day , replace 

		tostring end_yearmonth, replace 
		tostring end_day, replace 

		gen begin_month = substr(begin_yearmonth,5,2)
		gen end_month = substr(end_yearmonth,5,2)

		replace begin_day = "01"  if begin_day == "1"
		replace begin_day = "02"  if begin_day == "2"
		replace begin_day = "03"  if begin_day == "3"
		replace begin_day = "04"  if begin_day == "4"
		replace begin_day = "05"  if begin_day == "5"
		replace begin_day = "06"  if begin_day =="6"
		replace begin_day = "07"  if begin_day =="7"
		replace begin_day = "08"  if begin_day =="8"
		replace begin_day = "09"  if begin_day =="9"

		replace end_day = "01"  if end_day == "1"
		replace end_day = "02"  if end_day == "2"
		replace end_day = "03"  if end_day == "3"
		replace end_day = "04"  if end_day == "4"
		replace end_day = "05"  if end_day == "5"
		replace end_day = "06"  if end_day =="6"
		replace end_day = "07"  if end_day =="7"
		replace end_day = "08"  if end_day =="8"
		replace end_day = "09"  if end_day =="9"

		gen begindate = begin_yearmonth+begin_day
		gen enddate = end_yearmonth+end_day 

		gen begindate_d =  date(begindate,"YMD")
		gen enddate_d = date(enddate,"YMD")
		gen durationdays = enddate_d - begindate_d 
		format begindate_d %td
		format enddate_d %td

		gen totaldeaths = deaths_direct+deaths_indirect
		gen totalinjury = injuries_direct + injuries_indirect 
		gen totalharm = totaldeaths+totalinjury  

		keep episode_id event_id state state_fips year event_type cz_type cz_fips cz_name   ///
		begin_date_time cz_timezone end_date_time   ///
		injuries_direct injuries_indirect deaths_direct deaths_indirect damage_property damage_crops source   ///
		episode_narrative event_narrative data_source  ///
		flooding begin_month end_month begindate enddate begindate_d enddate_d durationdays totaldeaths totalinjury totalharm

		save  $input/detail_allcombine_individualclean.dta, replace 

		*************************************
		* Combine the flooding to the zone  * 
		*************************************

		
		****************************
		** For the cz_type is "C"  * 
		****************************

		use  $input/detail_allcombine_individualclean.dta  , clear 

		keep if cz_type == "C" 

		tostring state_fips, gen(state_fips_str)
		tostring cz_fips , gen (cz_fips_str)

		forvalues i = 1/9 {
		replace state_fips_str = "0`i'" if state_fips_str == "`i'"
		}

		forvalues i = 1/9 {
		replace cz_fips_str = "00`i'" if cz_fips_str == "`i'"
		}

		forvalues i = 10/99 {
		replace cz_fips_str = "0`i'" if cz_fips_str == "`i'"
		}

		gen county_fips_str = state_fips_str + cz_fips_str

		destring county_fips_str, gen(county_fips)

		save  $input/detail_allcombine_individualclean_Cpart.dta , replace 

		*****************************
		** For the cz_type  is "Z"  * 
		*****************************

		use  $input/detail_allcombine_individualclean.dta  , clear 
		keep if cz_type == "Z"    // 44603 (total)   40662 observations has been merged
		drop if state_fips > 78 
		save  $input/detail_allcombine_Z.dta , replace


		forvalues  i = 1(1)10 {
		use $input/detail_allcombine_Z.dta  , clear
		merge m:1 cz_fips state_fips  using  $input/zonecounty_connection_`i'.dta 
		keep if _merge == 3 
		save  $input/noaa_Z_combine_`i'.dta , replace 
		} 

		use               $input/noaa_Z_combine_1.dta , clear 
		append using      $input/noaa_Z_combine_2.dta
		append using      $input/noaa_Z_combine_3.dta
		append using      $input/noaa_Z_combine_4.dta
		append using      $input/noaa_Z_combine_5.dta
		append using      $input/noaa_Z_combine_6.dta
		append using      $input/noaa_Z_combine_7.dta
		append using      $input/noaa_Z_combine_8.dta
		append using      $input/noaa_Z_combine_9.dta
		append using      $input/noaa_Z_combine_10.dta

		drop m orderm _merge
		rename county_fips  county_fips_str 

		destring county_fips_str, gen(county_fips)

		save   $input/detail_allcombine_individualclean_Zpart.dta , replace 


		*****************************************
		* Combine C part and Z part together    * 
		*****************************************

		use    $input/detail_allcombine_individualclean_Cpart.dta , clear 
		append using  $input/detail_allcombine_individualclean_Zpart.dta    

				keep if flooding == 1 

		save  $output/detail_individual_clean.dta, replace 


		*************************************** 
		* Appalachian Region Data             *
		***************************************
		use  $output/detail_individual_clean.dta , clear

		tab event_type  if year >= 1999 & year <= 2012 

		keep if flooding == 1 

		merge m:1 county_fips  using  $input/noaa_detail_othercountycontrol.dta  

		keep if _merge == 3 
		drop _merge 

		keep if apparegion == 1 

		tab totalharm  event_type      if ruralurbancode ==5 | ruralurbancode==6 

		tab appasubregion 

		tab event_type  if  (year >= 1999 & year <= 2012) &  (appasubregion== "Central" | appasubregion=="North Central" | appasubregion=="South Central" ) 


		save    $output/detail_individual_clean_apparegion.dta , replace 
		save    $output/noaa_individual_appa.dta , replace 
		save    $input/noaa_individual_appa.dta, replace


		************************************************
		* County-year level data    APPA Region        * 
		************************************************

		use     $output/detail_individual_clean_apparegion.dta , clear 

		collapse (sum)  flooding  totaldeaths totalinjury totalharm , by(county_fips  state_fips  year apparegion  )

		save     $output/detail_countyyear_clean_apparegion.dta , replace 


		****************************************** 
		* County-year level data all counties    * 
		******************************************
		use  $output/detail_individual_clean.dta , clear

		collapse (sum)  flooding  totaldeaths totalinjury totalharm  durationdays , by(county_fips  state_fips  year   ) 
		save     $output/detail_countyyear_clean.dta , replace 


		******************************************
		* For every year level 
		*******************************************

		use  $output/noaa_flooddata_clean_update.dta,  clear  

		drop  flooding  state_fips county_fips_str    durationdays totaldeaths totalinjury totalharm   flooding_dy durationdays_dy totaldeaths_dy totalinjury_dy totalharm_dy

		merge m:1  county_fips year using    $output/detail_countyyear_clean.dta 

		replace flooding = 0      if   _merge ==1 
		replace totaldeaths = 0   if   _merge ==1 
		replace totalinjury = 0   if   _merge ==1 
		replace totalharm = 0     if   _merge ==1 
		replace durationdays = 0  if _merge ==1 

		keep if _merge ==1  | _merge==3 

		drop _merge 

		keep county_fips  year   county   flooding totaldeaths totalinjury totalharm  durationdays

		save   $output/noaa_flooddata_clean_update_0828.dta , replace 
		save   $input/noaa_flooddata_clean_update_0828.dta, replace


*********************************
* Generate the Data for results *
*********************************

		global  path        "Store Data Path in your computer"
		global  input 	     "$path/build/input"
		global  output       "$path/build/output"
		global  temp         "$path/build/temp"
		global  countyyear   "$path/build/input/countybyyear"

		cd  "$input"

		******* NOAA Balanced Data ***** 

		use noaa_flooddata_clean_update_0828.dta , clear 
		gen flna = flooding >= 1
		gen fl1d = totaldeaths >= 1 
		gen fl1h = totalharm >=1 

		label  var  flna   "Whether the year has the flooding event (NOAA)"
		label  var  fl1d   "Whether the year has the flooding with at least 1 deaths (NOAA)"
		label  var  fl1h   "Whether the year has the flooding with at least 1 harm (NOAA) "

		merge m:1 county_fips using countyruralurbancode_plusappa.dta
		keep if _merge == 3 
		drop _merge 
		keep if apparegion==1 

		save NOAA_countyyear_balance_appa.dta, replace


