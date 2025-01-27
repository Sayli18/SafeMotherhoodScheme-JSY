**********************************************************************************************************
*In this do file,
**********************************************************************************************************

clear*
set more off

global rain = "C:\Users\ssj18\Dropbox\JavadekarSaxena2021"
*Import rainfall data that varies by month
import excel "C:\Users\ssj18\Dropbox\JavadekarSaxena2021\raw data\rainfall_trials_.xlsx", sheet("india_monthly_rainfall_1981_201") firstrow

destring rain8101- rain1706,replace force

encode District, gen(dist)
encode State, gen(st)
drop State District

gen one = "1"
gen zero= "0"
gen st1=st
gen dist1=dist
egen UID  = concat(one  st dist)
bys UID: gen dup=cond(_N==1,0,_n)
drop if dup>1
destring UID, replace
format %20.0g UID

order UID st dist
drop rain17*
drop  one-dup

*reshape wide to long
reshape long rain00 rain01 rain02 rain03 rain04 rain05 rain06  rain07 rain08 ///
rain09 rain10 rain11 rain12 rain13 rain14 rain15 rain16 rain81 rain82 rain83 ///
rain84 rain85 rain86 rain87 rain88 rain89 rain90 rain91 rain92 rain93 rain94 ///
rain95 rain96 rain97 rain98 rain99  , i(UID) j(month) string
egen UIDm  = concat(UID month)
destring UIDm, replace
format %20.0g UIDm

reshape long rain  , i(UIDm) j(year) string

encode(year), gen(y)
gen twen="20" if y <=17
replace twen="19" if y >=18
egen y1=concat(twen year)

encode(y1), gen(YEAR)
encode(month),gen(MONTH)

order UID st dist YEAR MONTH rain
sort UID YEAR MONTH

drop UIDm- y1
save "C:\Users\ssj18\Dropbox\JavadekarSaxena2021\dta files\rain_long.dta",replace


decode(YEAR), gen(y)
destring(y),replace force

*Rainfall between june-oct taken as annual rainfall in each state
keep if MONTH>=6 & MONTH<=10
bys st dist y: gen dup=cond(_N==1,0,_n)
bys  st dist y: egen annual_rain=sum(rain)
drop if dup>1

* Avergare rainfall for 16 years 1981-1997
bys  st dist : egen rain_mean=mean(annual_rain) if y<1998 
replace rain_mean=0 if missing(rain_mean)
bys st dist: egen rain_hist=max(rain_mean) /*  long run mean*/
* Std dev in rainfall for 16 years 1981-1997
bys st dist: egen sd_rain=sd(annual_rain) if y <1998 
replace sd_rain=0 if missing(sd_rain)
bys st dist: egen sd_rain_hist=max(sd_rain) /*  long run sd*/
*Z-score in rainfall for 1998-2016 for each state
bys st dist : gen rain_z= (annual_rain - rain_hist)/sd_rain_hist if y>=1998
keep if rain_z!=.

order UID st dist YEAR y rain_z
sort UID YEAR 

drop MONTH - sd_rain_hist

label var rain_z "Z-score for rainfall"
** get the state dist from here
 
rename rain_z zrain_year
drop UID
decode(st), gen(STATE)
decode(dist), gen(DIST)
replace STATE = upper(STATE)
replace DIST = upper(DIST)
encode(STATE), gen(state)
encode(DIST), gen(District)
sort st dist y

*lag in z score for each year from 2000
bys st dist : gen z1=zrain_year[_n-1]
bys st dist: gen z2=zrain_year[_n-2]

label var z1 "1 year lagged rainfall z score"
label var z2 "2 year lagged rainfall z score"

order  st dist YEAR y  zrain_year z1 z2
drop if y<=1999
sort st dist y
save "C:\Users\ssj18\Dropbox\JavadekarSaxena2021\dta files\st-dist_rain11.dta",replace























********************************************************************************************************************************************************













/*
















decode(YEAR), gen(y)
destring(y),replace force
keep if MONTH>=5 & MONTH<=10

bys UID st dist MONTH: egen sr=mean(rain) if y <1998 & (MONTH>=5 & MONTH<=10)
replace sr=0 if missing(sr)
*bys UID st dist : egen srm=max(sr)
bys UID st dist MONTH: egen srsd=sd(rain) if y <1998 & (MONTH>=5 & MONTH<=10)
replace srsd=0 if missing(srsd)

bys st dist MONTH: egen mean_hist=max(sr)
bys st dist MONTH: egen sd_hist=max(srsd)
sort st dist y MONTH

save "C:\Users\ssj18\Dropbox\JSY paper update\Rain\hist_month",replace


*bys UID st dist : egen srsdm=max(srsd)

bys UID st dist : gen zrain_mon=(rain-mean_hist)/sd_hist if y >=1998 
order UID st dist YEAR MONTH rain zrain_mon
sort UID YEAR MONTH
 

*rain00 rain01 rain02 rain03 rain04 rain05 rain06  rain07 rain08 rain09 rain10 rain11 rain12 rain13 rain14 rain15 rain16 rain81 rain82 rain83 rain84 rain85 rain86 rain87 rain88 rain89 rain90 rain91 rain92 rain93 rain94 rain95 rain96 rain97 rain98 rain99  

drop if y<=1997
drop sr- sd_hist


save "C:\Users\ssj18\Dropbox\JSY paper update\Rain\rain_long_final.dta",replace



** get the state dist from here


bys dist: gen dup=cond(_N==1,0,_n)
drop if dup>1
keep st dist
save "C:\Users\ssj18\Dropbox\JSY paper update\Rain\st-dist_rain.dta",replace
*********************************************************************************


*Variation by year
*********************************************************************************

clear*
set more off

use"C:\Users\ssj18\Dropbox\JSY paper update\Rain\rain_long.dta",clear

*drop  rain8101- rain9912
*drop rain0601-rain1612

decode(YEAR), gen(y)
destring(y),replace force

keep if MONTH>=5 & MONTH<=10

bys UID st dist: egen sr=mean(rain) if y <1998 
bys UID st dist: egen srsd=sd(rain) if y <1998

bys UID st dist YEAR: egen srr=sum(rain) if y >=1998 
replace sr=0 if missing(sr)
bys UID st dist : egen srm=max(sr)
replace srsd=0 if missing(srsd)
bys UID st dist : egen srsdm=max(srsd)

bys UID st dist : gen zra=(srr-srm) if y >=1998 


bys UID st dist : gen zrain_year=(srr-srm)/srsdm if y >=1998 
drop if zrain_year==.
order UID st dist YEAR MONTH rain zrain_year
sort UID YEAR MONTH
drop y- srsdm 

*rain00 rain01 rain02 rain03 rain04 rain05 rain06  rain07 rain08 rain09 rain10 rain11 rain12 rain13 rain14 rain15 rain16 rain81 rain82 rain83 rain84 rain85 rain86 rain87 rain88 rain89 rain90 rain91 rain92 rain93 rain94 rain95 rain96 rain97 rain98 rain99  
bys UID st dist YEAR:gen dup=cond(_N==1,0,_n)
drop if dup>1
drop MONTH dup
save "C:\Users\ssj18\Dropbox\JSY paper update\Rain\rain_long_final_year.dta",replace



** get the state dist from here
 

drop UID
decode(st), gen(STATE)
decode(dist), gen(DIST)
replace STATE = upper(STATE)
replace DIST = upper(DIST)
encode(STATE), gen(state)
encode(DIST), gen(District)
bys st dist : gen z1=zrain_year[_n-1]
order  st dist YEAR  rain zrain_year z1
bys st dist: gen z2=zrain_year[_n-2]
order  st dist YEAR  rain zrain_year z1 z2
drop if YEAR<=19
sort st dist YEAR
save "C:\Users\ssj18\Dropbox\JSY paper update\Rain\st-dist_rain1.dta",replace
