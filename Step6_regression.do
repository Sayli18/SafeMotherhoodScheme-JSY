**************************************************************************************************
* In this do file, we run  the final health worker regression.
* asha_data_use_this.dta contains information from ASHA, DHS and 2011 census merged.
**************************************************************************************************
* MECHANISM - HEALTH WORKER
clear*
set more off

global ash = "C:\Users\ssj18\Dropbox\JavadekarSaxena2021"

use "$ash\dta files\asha_data_use_this",clear

set more off

*public
gen hw_pub=asha_pub_m/10000

reghdfe child_sex  c.hw_pub##i.treat st_year  if  rural==1 & mtemp==1 ,cl(v024) absorb( UID b2 bord)
eststo r1


*private
gen hw_pri=asha_pri_m/10000
reghdfe child_sex c.hw_pri treat c.hw_pri#1b.treat   if  rural==1 & mtemp==1 ,cl(v024) absorb( UID b2 bord)
eststo r1_


esttab r1


