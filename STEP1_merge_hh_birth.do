
clear all
set more off
set maxvar 11000
global js21 = "C:\Users\ssj18\Dropbox\JavadekarSaxena2021"


capture log close
log using "$js21\dataset.log", replace text

*************************************************************************************************************************
*************************************************************************************************************************
******* This file creates main data sets used in the analysis of Javadekar, Saxena (2021) *******************************
********************* "The seen and the unseen: Impact of a conditional cash transfer ***********************************
*******************************  program on prenatal sex selection" *****************************************************
*************************************************************************************************************************
*************************************************************************************************************************

*To create the working dataset we merge birth recode and household recode from DHS 2015/16

use "$js21\raw data\IABR74FL",clear
gen hv001=v001
gen hv002=v002
merge m:1 hv001 hv002 using "$js21\raw data\IAHR74FL", gen(_merge1)

*save fullmerge,replace
keep if _merge1==3
save "$js21\dta files\mer_hh_birth",replace

use "C:\Users\ssj18\Dropbox\JavadekarSaxena2021\dta files\mer_hh_birth",clear
keep v* bord b* sh* hw* s* sdistri m15 m14

save "$js21\dta files\finaldata",replace

