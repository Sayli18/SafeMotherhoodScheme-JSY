
clear
set more off

*global child= "C:\Users\ssj18\Dropbox\JavadekarSaxena2021"
global child= "/Users/kritika/Dropbox/JavadekarSaxena2021/"


********************************************************************************
* HAZ and WAZ
********************************************************************************

* Computing Z Scores
* Merging DHS3
* With birth recode for mother level variables

use "$child/raw data/IABR52FL.dta"
keep v001 v002 v003 v004 v005 v133 v012 v715 v138 v130 v212 bord
save "$child/dta files/b2005.dta",replace
clear
*DHS 2005/06 children recode
use "$child\raw data\IAKR52FL.dta"
merge m:1 v001 v002 v003 bord using "$child\dta files\b2005.dta", gen(_merge1)
keep if _merge1==3
drop _merge1

*Create imporatnt vars

//hh belongs to SC or ST
gen sc_st = 1 if s46 <=2  
replace sc_st = 0 if s46>=3 
replace sc_st = 0 if s45==3 & sc_st==.  //v131==993 is no caste no tribe
gen child_sex=1 if b4==2 /*fem*/
replace child_sex=0 if child_sex==.

gen fg_=1 if bord==1 & child_sex==1/*first child is a girl*/
replace fg_=0 if bord==1 & child_sex==0

gen rural = 1 if v025==2
replace rural =0 if missing(rural)

gen lps=1 if v024==4 | v024==5 | v024==7 | v024==14  | v024==15   | v024==19 | v024==26 | v024==29 | v024==33 | v024==34 
replace lps=0 if lps==.

label define lps 1"LPS" 0"HPS"


gen bpl=1 if s66==1
replace bpl=0 if missing(bpl)

gen treat=1 if bpl==0 & sc_st==0 & lps==1
replace treat=0 if bpl==0 & sc_st==0 & lps==0


gen mom_edu=v133
gen mom_age=v012
gen dad_edu=v715
clonevar tot_women=v138
clonevar religion= v130
clonevar first_age= v212
gen mom_age2= mom_age*mom_age
save "$child\dta files\chil_dhs3n4", replace


* Generating Z scores for DLHS 3
cd "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/who_reference"

adopath + "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/who_reference/"

drop weight



gen str80 reflib= "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/who_reference"
 lab var reflib "Directory of reference tables"
 gen str80 datalib= "`who_reference'"
 lab var datalib "Directory for datafiles"
 gen str30 datalab="mysurvey"
 lab var datalab "Working file"
 gen age=hw1 // consistent with WHO nomenclature: the month represents completed months did not subtract 1 
 gen str6 ageunit="months" 
 lab var ageunit "=months"
 gen sex=b4 
 gen height=hw3/10 if hw3>0
 gen weight=hw2/10 if hw2>0 
 gen measure=""
 replace measure="l" if hw15==1
 replace measure="h" if hw15==2
 gen str1 oedema="n"
 gen sw=1
 gen hc=.
 gen armc=.
 gen tris=.
 gen subs=.
    

keep if rural==1 //we are only interested in rural sample
igrowup_standard reflib datalib datalab sex age ageunit weight height measure oedema hc armc tris subs oedema sw 

*Removing outliers
clonevar haz= _zlen 
replace haz =. if haz<=-6 
replace haz =. if haz>=6 

clonevar waz= _zwei 
replace waz =. if waz<=-6  
replace waz =. if waz>=5 

clonevar whz= _zwfl 
replace whz =. if whz<=-5 
replace whz =. if whz>=5 

clonevar bmiz= _zbmi 
replace bmiz =. if bmiz<=-5 
replace bmiz =. if bmiz>=5 

drop _zwei _zlen _zbmi _zwfl _merge


* Generating CDF
qplot haz, recast(line) ylab(,angle(0)) xlab(0(0.1)1) yline(-3)

* Running quantile reg
* Plot coefficients for each regressor by quantiles
qui qreg haz child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women, q(.50) nolog
grqreg child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women, cons ci ols olsci reps(40)



* Test for heteroskedasticity
quietly reg haz child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women
estat hettest child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women, iid


*Merging DLHS4
* Merging birth data with child recode

use "/Volumes/Kritika WD Passport/Data/DHS-India/2015-2016/Birth Recode/IABR71FL.DTA"
keep v001 v002 v003 v004 v005 v133 v012 v715 v138 v212 b0 bord caseid
save "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/b2015.dta"
clear
use "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/child_2015-16.dta"
merge m:1 v001 v002 v003 bord using "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/b2015.dta", gen(_merge1)
keep if _merge1==3
drop _merge1
save, replace
merge m:1 v001 v002 using "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/hh15.dta", gen(_merge1)
keep if _merge1==3
drop _merge1
save, replace

gen sc_st = 1 if s116 <=2 //hh belongs to SC or ST
replace sc_st = 0 if s116>=3 
replace sc_st = 0 if v131==993 & sc_st==.  //v131==993 is no caste no tribe



gen bpl=1 if sh58==1 
replace bpl=0 if bpl==. 

* Generating Z scores for DLHS 4
cd "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/who_reference"

adopath + "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/who_reference/"

drop weight



gen str80 reflib= "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/who_reference"
 lab var reflib "Directory of reference tables"
 gen str80 datalib= "`who_reference'"
 lab var datalib "Directory for datafiles"
 gen str30 datalab="mysurvey"
 lab var datalab "Working file"
 gen age=hw1 // consistent with WHO nomenclature: the month represents completed months did not subtract 1 
 gen str6 ageunit="months" 
 lab var ageunit "=months"
 gen sex=b4 
 gen height=hw3/10 if hw3>0
 gen weight=hw2/10 if hw2>0 
 gen measure=""
 replace measure="l" if hw15==1
 replace measure="h" if hw15==2
 gen str1 oedema="n"
 gen sw=1
 gen hc=.
 gen armc=.
 gen tris=.
 gen subs=.
    

keep if rural==1 //we are only interested in rural sample
igrowup_standard reflib datalib datalab sex age ageunit weight height measure oedema hc armc tris subs oedema sw 

*Removing outliers
clonevar haz= _zlen 
replace haz =. if haz<=-6 
replace haz =. if haz>=6 

clonevar waz= _zwei 
replace waz =. if waz<=-6  
replace waz =. if waz>=5 

clonevar whz= _zwfl 
replace whz =. if whz<=-5 
replace whz =. if whz>=5 

clonevar bmiz= _zbmi 
replace bmiz =. if bmiz<=-5 
replace bmiz =. if bmiz>=5 

drop _zwei _zlen _zbmi _zwfl _merge



* Generating CDF
qplot haz, recast(line) ylab(,angle(0)) xlab(0(0.1)1) yline(-3)

* Running quantile reg
* Plot coefficients for each regressor by quantiles
qui qreg haz child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women, q(.50) nolog
grqreg child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women, cons ci ols olsci reps(40)



* Test for heteroskedasticity
quietly reg haz child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women
estat hettest child_sex mom_age mom_age2 mom_edu dad_edu first_age tot_women, iid
save, replace


* Appending dlhs3 and dlhs4
use "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/child_2005-06.dta"
append using "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/child_2015-16.dta"
save "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/Anthro comb.dta"



* Table on Child Anthoprometrics
clear
use "/Volumes/Kritika WD Passport/Data/MPT paper 1/DLHS/Anthro comb.dta"

qui reghdfe haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 strend if dlhs==4, cl(v024) absorb(b2 bord v024)
est store haz
qui reghdfe waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 strend if dlhs==4, cl(v024) absorb(b2 bord v024) //doesn't estimate WAZ
est store waz
qui reghdfe whz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 strend if dlhs==4, cl(v024) absorb(b2 bord v024)
est store whz
*qui reghdfe bmiz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(v024) absorb(b2 bord v024)
*est store bmiz
esttab haz waz whz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes)
esttab haz waz whz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes) tex

* Regressions 
* with treat
qui reghdfe haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(UID) absorb(b2 bord v024)
est store haz
qui reghdfe waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(UID) absorb(b2 bord v024) //doesn't estimate WAZ
est store waz
qui reghdfe whz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(UID) absorb(b2 bord v024)
est store whz
qui reghdfe bmiz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(UID) absorb(b2 bord v024)
est store bmiz
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes)
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes) tex

qui reghdfe haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(v024) absorb(b2 bord v024)
est store haz
qui reghdfe waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(v024) absorb(b2 bord v024) //doesn't estimate WAZ
est store waz
qui reghdfe whz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(v024) absorb(b2 bord v024)
est store whz
qui reghdfe bmiz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, cl(v024) absorb(b2 bord v024)
est store bmiz
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes)
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes) tex

* With bord>1
qui reghdfe haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>1, cl(UID) absorb(b2 bord UID)
est store haz
qui reghdfe waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>1, cl(UID) absorb(b2 bord UID) //doesn't estimate WAZ
est store waz
qui reghdfe whz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>1, cl(UID) absorb(b2 bord UID)
est store whz
qui reghdfe bmiz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>1, cl(UID) absorb(b2 bord UID)
est store bmiz
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes)
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes) tex


* With bord>2
qui reghdfe haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>2, cl(UID) absorb(b2 bord UID)
est store haz
qui reghdfe waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>2, cl(UID) absorb(b2 bord UID) //doesn't estimate WAZ
est store waz
qui reghdfe whz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>2, cl(UID) absorb(b2 bord UID)
est store whz
qui reghdfe bmiz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4 & bord>2, cl(UID) absorb(b2 bord UID)
est store bmiz
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes)
esttab haz waz whz bmiz, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes) tex






*Quantile Reg HAZ
qui qreg haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.1) nolog
est store qreg1
qui qreg haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.25) nolog
est store qreg25
qui qreg haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.5) nolog
est store qreg5
qui qreg haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.75) nolog
est store qreg75
qui qreg haz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.9) nolog
est store qreg09
esttab qreg1 qreg25 qreg5 qreg75 qreg09, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric outcomes) 
esttab qreg1 qreg25 qreg5 qreg75 qreg09, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child anthropometric quantile outcomes) tex

*Quantile Reg WAZ
qui qreg waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.1) nolog
est store qreg1
qui qreg waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.25) nolog
est store qreg25
qui qreg waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.5) nolog
est store qreg5
qui qreg waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.75) nolog
est store qreg75
qui qreg waz child_sex##treat mom_age mom_age2 mom_edu first_age tot_women v190 i.b2 i.bord i.v024 if dlhs==4, q(.9) nolog
est store qreg9
esttab qreg1 qreg25 qreg5 qreg75 qreg9, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child WAZ quantile outcomes) 
esttab qreg1 qreg25 qreg5 qreg75 qreg9, replace b(%10.4f)se scalars(N  r2) star(* 0.10 ** 0.05 *** 0.01) mtitles long title(Child WAZ quantile outcomes) tex


clonevar wealth = v190

qui qreg haz child_sex treat mom_age mom_age2 mom_edu first_age tot_women wealth if dlhs==4, nolog
grqreg child_sex treat mom_age mom_age2 mom_edu first_age tot_women wealth, cons ci ols olsci reps(40)

qui qreg waz child_sex treat mom_age mom_age2 mom_edu first_age tot_women wealth if dlhs==4, nolog
grqreg child_sex treat mom_age mom_age2 mom_edu first_age tot_women wealth, cons ci ols olsci reps(40)


bsqreg haz child_sex treat mom_age mom_age2 mom_edu first_age tot_women v190 if dlhs==4, reps(40) 
grqreg child_sex treat mom_age mom_age2 mom_edu first_age tot_women v190, cons ci ols olsci reps(40)
