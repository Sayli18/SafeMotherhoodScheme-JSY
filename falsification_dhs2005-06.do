set more off

global dhs3 = "C:\Users\ssj18\Dropbox\JavadekarSaxena2021"

use "dhs3\raw data\IAHR52FL.dta" 
gen v001 = hv001
gen v002=hv002
save "dhs3\raw data\IAHR52FL.dta", replace


capture log close
log using "$dhs3\falsification.log", replace text

*************************************************************************************************************************
*************************************************************************************************************************
******* This file creates falsification test usesing DHS3 for Javadekar, Saxena (2021) *******************************
********************* "The seen and the unseen: Impact of a conditional cash transfer ***********************************
*******************************  program on prenatal sex selection" *****************************************************
*************************************************************************************************************************

* Falsification Test with DHS - 2005/06

use "$dhs3\raw data\IABR52FL",replace

merge m:1 v001 v002 using "$dhs3\raw data\IAHR52FL", gen(_merge1) //didn't match 33,031   obs from using
keep if _merge1==3
drop _merge1
save, replace

* Generate a Unique Identifier
gen str6 v001id = string(v001,"%06.0f")
gen str2 v002id = string(v002,"%02.0f")
gen str2 v003id = string(v003,"%02.0f")

gen one = "1"
*egen UIDBO = concat(one stateid distid psuid hhid hhsplitid personid boid)
egen UID  = concat(one v001id v002id v003id)
duplicates tag UID bord , gen(dup1)
tab dup1 //no duplicates
destring UID, replace
format %20.0g UID


* Rename and generate required variables
clonevar bpl= s66

gen sc_st = 1 if s46 <=2  //hh belongs to SC or ST
replace sc_st = 0 if s46>=3 
replace sc_st = 0 if s45==3 & sc_st==.  //v131==993 is no caste no tribe

gen lps=1 if v024==4 | v024==5 | v024==7 | v024==14  | v024==15   | v024==19 | v024==26 | v024==29 | v024==33 | v024==34 
replace lps=0 if lps==.

* Treatment dummy
gen treat=1 if bpl==0 & sc_st==0 & lps==1
replace treat=0 if bpl==0 & sc_st==0 & lps==0

* Child sex variable
gen child_sex=1 if b4==2 /*fem*/
replace child_sex=0 if child_sex==.

* Variable for rural
gen rural =1 if v025==2
replace rural =0 if missing(rural)

* Mother's chr
gen mom_edu=v133
gen mom_age=v012
gen dad_edu=v715
clonevar tot_women=v138
clonevar religion= v130
clonevar first_age= v212
gen mom_age2= mom_age*mom_age

* Sample restricted to 1990 onwards
bys UID: gen tempo=1 if b2>=1990 & bord==1
replace tempo=0 if tempo==.
bys UID: egen mtemp=max(tempo)

* First child's sex
gen fg_=1 if bord==1 & child_sex==1/*first child is a girl*/
replace fg_=0 if bord==1 & child_sex==0
bys UID :egen fg=max(fg_)

* Post variables
bys UID: gen p05=1 if b2>1995 & b2<=2000
replace p05=2 if b2>2000 & b2<=2005
replace p05=0 if b2>=1990 & b2<=1995

bys UID: gen p00=1 if b2>2000 & b2<=2005
replace p00=0 if b2>1995 & b2<= 2000

bys UID: gen post=1 if b2>1995 & b2<=2005
replace post =0 if b2>=1990 & b2<=1995


* Falsification with Triple Diff considering 1995 and 2000 as program years
qui reghdfe child_sex treat##i.p05##fg  if rural==1 & mtemp==1 & bord>=2, cl(v024) absorb(b2 bord UID)
est store M1
qui reghdfe child_sex treat##post##fg  if rural==1 & mtemp==1 & bord>=2, cl(v024) absorb(b2 bord UID)
est store M2
qui reghdfe child_sex treat##i.p00##fg  if rural==1 & mtemp==1 & bord>=2, cl(v024) absorb(b2 bord UID)
est store M3


esttab M1 M2 M3 using $dhs3\Results\fal_dhs3.tex, replace mtitles("Girl" "Girl" "Girl" ) /// 
keep( 1.treat#1.post#1.fg 1.treat#1.p05#1.fg 1.treat#2.p05#1.fg 1.treat#1.p00#1.fg  )  ///
coeflabels( 1.treat#1.post#1.fg "Treat*Post1995-05*First_Girl" 1.treat#1.p05#1.fg "Treat*Post1996-00*First_Girl" 1.treat#2.p05#1.fg "Treat*Post2001-05*First_Girl" 1.treat#1.p00#1.fg "Treat*Post2001-05*First_Girl" ) collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Falsification Test: Triple Difference Estimation using DHS 2005-06\label{tab:fal_tddhs3}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports the triple difference results for the falsification tests using DHS 2005-06 data collected prior to the implemetation of the program. We consider 1995 and 2000 as `program years' and estimate the triple difference on the likelihood of observing a female birth. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, $FG$ is an indicator for if the woman's first born child was a girl. $Post$ compares the assumed `post program years' (1996-2005) to the assumed `pre program years' (1990-1995). $Post_I$ and $Post_{II}$ are the early (1996-2000) and late diffusion (2001-2005) periods of the program. $Post00$ asuumes 2001-2005 as post program years and 1996-2000 as preprogram years. $FE$ contains mother, birth and year fixed effects. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Standard errors in parentheses are clustered at the state level. " ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 





 *The table reports the triple difference results for the falsification tests using DHS 2005-06 data collected prior to the implementation of the program. We consider 1995 and 2000 as  and estimate the triple difference on the likelihood of observing a female birth. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, $First_Girl$ is an indicator for if the woman's first born child was a girl. $Post$ compares the assumed  (1996-2005) to the assumed  (1990-1995). $Post_I$ and $Post_{II}$ are the early (1996-2000) and late diffusion (2001-2005) periods of the program. $Post00$ assumes 2001-2005 as post program years and 1996-2000 as pre program years. $FE$ contains mother, birth and year fixed effects. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Standard errors in parentheses are clustered at the state level. 



