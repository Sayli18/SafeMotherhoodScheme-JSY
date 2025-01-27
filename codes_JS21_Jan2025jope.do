****without month of birth fe in every reg

set more off

*Sayli
global js21 = "C:\Users\ssj18\Dropbox\JavadekarSaxena2021"

*Kritika
global js21 = "/Users/kritzz/Library/CloudStorage/Dropbox/JavadekarSaxena2021"

capture log close
log using "$js21/jope/analysis_withoutmonthofbirth.log", replace text

*************************************************************************************************************************
*************************************************************************************************************************
******* This file creates main tables used in the analysis of Javadekar, Saxena (2021) *******************************
********************* "The seen and the unseen: Impact of a conditional cash transfer ***********************************
*******************************  program on prenatal sex selection" *****************************************************
*************************************************************************************************************************




use "$js21/dta files/finaldata",replace

* Unique identifier for each woman

gen str6 v001id = string(v001,"%06.0f")
gen str2 v002id = string(v002,"%02.0f")
gen str2 v003id = string(v003,"%02.0f")

gen one = "1"
egen UID  = concat(one v001id v002id v003id)
duplicates tag UID bord , gen(dup1)
destring UID, replace
format %20.0g UID



* Creating key variables

gen child_sex=1 if b4==2 /*fem*/
replace child_sex=0 if child_sex==.

gen fg_=1 if bord==1 & child_sex==1/*first child is a girl*/
replace fg_=0 if bord==1 & child_sex==0
bys UID :egen fg=max(fg_)

gen rural = 1 if v025==2
replace rural =0 if missing(rural)

gen lps=1 if v024==4 | v024==5 | v024==7 | v024==14  | v024==15   | v024==19 | v024==26 | v024==29 | v024==33 | v024==34 
replace lps=0 if lps==.

label define lps 1"LPS" 0"HPS"
label val lps lps

rename sh58 bpl

gen sc_st = 1 if s116 <=2 //hh belongs to SC or ST only
replace sc_st = 0 if s116>=3
replace sc_st = 0 if v131==993 & sc_st==. //v131==993 is no caste no tribe


gen treat=1 if bpl==0 & sc_st==0 & lps==1
replace treat=0 if bpl==0 & sc_st==0 & lps==0

bys UID :gen post =1 if b2 >2005 & b2<=2015
replace post =0 if b2>=2000 & b2<=2005

bys UID :gen p05 =1 if b2 >2005 & b2<=2010 
replace p05=2 if b2>2010 & b2<=2015
replace p05 =0 if b2>=2000 & b2<=2005


bys UID :gen tempo=1 if b2>=2000 & bord==1 // families with first birth after 2000
replace tempo=0 if tempo==.
bys UID :egen mtempo=max(tempo) 


egen st_year = group(v024 b2)


* Key variables for mortality

gen dead =1 if b5==0
replace dead=0 if missing(dead)

gen child_age=v007-b2 //v007 gives year of interview
*drop if mtempo==0
*************************************************************************************************************************
* Balance test
*************************************************************************************************************************


gen sc= 1 if s116==1
gen st=1 if s116==2
gen obc=1 if s116==3
gen fcaste=1 if s116==4
replace sc=0 if missing(sc)
replace st=0 if missing(st)
replace obc=0 if missing(obc)
replace fcaste=0 if missing(fcaste)

gen hindu=1 if v130==1
gen musl=1 if v130==2
gen chris=1 if v130==3
replace hindu=0 if missing(hindu)
replace musl=0 if missing(musl)
replace chris=0 if missing(chris)
gen oth_rel=1 if v130>3
replace oth_rel=0 if v130<=3


gen momedu=v133
gen noelig=v138
gen shhead=v151
gen ahhead=v152
gen totchil=v201
gen ultra=s236

gen poorest=1 if s190rs==1
gen poorer=1 if s190rs==2
gen middle=1 if s190rs==3
gen richer=1 if s190rs==4
gen richest=1 if s190rs==5

replace poorest=0 if missing(poorest)
replace poorer=0 if missing(poorer)
replace middle=0 if missing(middle)
replace richer=0 if missing(richer)
replace richest=0 if missing(richest)

gen elec=v119
gen radio=v120
gen tv=v121
gen fridge=v122
gen cycle=v123
gen scoo=v124
gen truck=v125

bys UID: egen tot_girls=sum(child_sex)
gen prop_g=tot_girls/totchil


bys UID: egen tot_chil05=max(bord) if b2<=2005
bys UID: egen tot_g05=total(child_sex) if b2<=2005
gen cs1=child_sex if bord==1
gen cs2=child_sex if bord==2
gen cs3=child_sex if bord==3
gen cs4=child_sex if bord>=4


 
gen ultr =s236

lab var prop_g "Prop of girls"
lab var totchil "Total childern"
lab var hindu "Hindu"
lab var musl "Muslim"
lab var fcaste "Forward Caste"
lab var obc "OBC"
lab var momedu "Mother's education"
lab var shhead "Sex of household head"
lab var ahhead "Age of household head"
lab var poorest "Poorest"
lab var poorer "Poorer"
lab var middle "Middle"
lab var richer "Richer"
lab var richest "Richest"
lab var elec "Electricity"
lab var truck "Truck"
lab var fridge "Fridge"
lab var cycle "Cycle"
lab var tv "TV"
lab var radio "Radio"
lab var ultr "Self reported ultrasound use"
lab var cs1 "Prop Girl parity 1"
lab var cs2 "Prop Girl parity 2"
lab var cs3 "Prop Girl parity 3"
lab var cs4 "Prop Girl parity 4"


*since we want only ytansitional mothers
*drop only child mothers
gen only1child=1 if v201==1
replace only1child=0 if only1child==.
*drop if only1child==1
*drop mothers who completed fertility before 2005
gen maxbord=1 if bord == v201 & b2<=2005
replace maxbord=0 if missing(maxbord)
bys UID: egen maxbord1=max(maxbord)
*drop if maxbord1==1
*drop maxbord maxbord1
*drop mothers who start fertility after 2005
gen maxbord2=1 if bord == 1 & b2>2005
replace maxbord2=0 if missing(maxbord2)
bys UID: egen maxbord21=max(maxbord2)
*drop if maxbord1==1
*drop maxbord maxbord1

gen transitional =1 if maxbord1==0 & maxbord21==0 & only1child==0

/*******************************************************************

Balance test for all

********************************************************************/
eststo clear
estpost summarize prop_g totchil hindu musl fcaste obc momedu shhead ahhead ultr poorest poorer middle richer richest elec truck fridge cycle tv radio  if  rural==1 & b2>=2000 & b2 <=2005 & bord==1
esttab using "$js21\jope\KS\table1.tex", replace b(%12.3f) label substitute(# \#) ///
    fragment nomtitle nonumber noobs cells("count(fmt(a2)) mean sd min max")
		

global DESCVARS prop_g totchil hindu musl fcaste obc momedu shhead ahhead ultr poorest poorer middle richer richest elec truck fridge cycle tv radio 
mata: mata clear

* First test of differences
local i = 1

foreach var in $DESCVARS {
    reg `var' treat if  rural==1  & bord==1 & b2>=2000 & b2 <=2005 ,  cl( v024)
    outreg, keep(treat)  rtitle("`: var label `var''") stats(b)  ///
        noautosumm store(row`i')  starlevels(10 5 1) starloc(1)
    outreg, replay(diff) append(row`i') ctitles("",Difference )  ///
        store(diff) note("")
    local ++i
}


		

* Then Summary statistics
local count: word count $DESCVARS
mat sumstat = J(`count',6,.)

local i = 1
foreach var in $DESCVARS {
    quietly: summarize `var' if treat==1 & rural==1 & b2>=2000 & b2 <=2005 & bord==1
    mat sumstat[`i',1] = r(N)
    mat sumstat[`i',2] = r(mean)
    mat sumstat[`i',3] = r(sd)
    quietly: summarize `var' if treat==0 &  rural==1 & b2>=2000 & b2 <=2005 & bord==1
    mat sumstat[`i',4] = r(N)
    mat sumstat[`i',5] = r(mean)
    mat sumstat[`i',6] = r(sd)
    local i = `i' + 1
}
frmttable, statmat(sumstat) store(sumstat) sfmt(g,f,f,g,f,f)

*And export
outreg using "$js21\jope\KS\balance.tex", ///
    replay(sumstat) merge(diff) tex nocenter note("") fragment plain replace ///
    ctitles("", Treatment, "", "", Control, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 
	

***Proportion of girls at every birth order prior to 2005	



eststo clear 



estpost summarize cs1 cs2 cs3 cs4 if  rural==1 & b2>=2000 & b2 <=2005
esttab using "$js21\jope\KS\tablebord.tex", replace b(%12.3f) label substitute(# \#) ///
    fragment nomtitle nonumber noobs cells("count(fmt(a2)) mean sd min max")
	
global DESCVARS cs1 cs2 cs3 cs4
mata: mata clear

	
* First test of differences
local i = 1

foreach var in $DESCVARS {
    reg `var' treat if  rural==1  & b2>=2000 & b2 <=2005 ,  cl( v024)
    outreg, keep(treat)  rtitle("`: var label `var''") stats(b)  ///
        noautosumm store(row`i')  starlevels(10 5 1) starloc(1)
    outreg, replay(diff) append(row`i') ctitles("",Difference )  ///
        store(diff) note("")
    local ++i
}


		

* Then Summary statistics
local count: word count $DESCVARS
mat sumstat = J(`count',6,.)

local i = 1
foreach var in $DESCVARS {
    quietly: summarize `var' if treat==1 & rural==1 & b2>=2000 & b2 <=2005
    mat sumstat[`i',1] = r(N)
    mat sumstat[`i',2] = r(mean)
    mat sumstat[`i',3] = r(sd)
    quietly: summarize `var' if treat==0 &  rural==1 & b2>=2000 & b2 <=2005
    mat sumstat[`i',4] = r(N)
    mat sumstat[`i',5] = r(mean)
    mat sumstat[`i',6] = r(sd)
    local i = `i' + 1
}
frmttable, statmat(sumstat) store(sumstat) sfmt(g,f,f,g,f,f)

*And export
outreg using "$js21\jope\KS\balancebord.tex", ///
    replay(sumstat) merge(diff) tex nocenter note("") fragment plain replace ///
    ctitles("", Treatment, "", "", Control, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 
	

	
	
	
	
/*******************************************************************

Balance test for transitional mothers

********************************************************************/
eststo clear
estpost summarize prop_g totchil hindu musl fcaste obc momedu shhead ahhead ultr poorest poorer middle richer richest elec truck fridge cycle tv radio if  rural==1 & transitional==1 & bord==1
esttab using "$js21\jope\KS\table1tran.tex", replace b(%12.3f) label substitute(# \#) ///
    fragment nomtitle nonumber noobs cells("count(fmt(a2)) mean sd min max")
	
	

global DESCVARS prop_g totchil hindu musl fcaste obc momedu shhead ahhead ultr poorest poorer middle richer richest elec truck fridge cycle tv radio
mata: mata clear

* First test of differences
local i = 1

foreach var in $DESCVARS {
    reg `var' treat if  rural==1  & bord==1 & transitional==1,  cl( v024)
    outreg, keep(treat)  rtitle("`: var label `var''") stats(b)  ///
        noautosumm store(row`i')  starlevels(10 5 1) starloc(1)
    outreg, replay(diff) append(row`i') ctitles("",Difference )  ///
        store(diff) note("")
    local ++i
}
outreg, replay(diff)

* Then Summary statistics
local count: word count $DESCVARS
mat sumstat = J(`count',6,.)

local i = 1
foreach var in $DESCVARS {
    quietly: summarize `var' if treat==1 & rural==1 & transitional==1 & bord==1
    mat sumstat[`i',1] = r(N)
    mat sumstat[`i',2] = r(mean)
    mat sumstat[`i',3] = r(sd)
    quietly: summarize `var' if treat==0 &  rural==1 & transitional==1 & bord==1
    mat sumstat[`i',4] = r(N)
    mat sumstat[`i',5] = r(mean)
    mat sumstat[`i',6] = r(sd)
    local i = `i' + 1
}
frmttable, statmat(sumstat) store(sumstat) sfmt(g,f,f,g,f,f)

*And export
outreg using "$js21\jope\KS\jope\balancetran.tex", ///
    replay(sumstat) merge(diff) tex nocenter note("") fragment plain replace ///
    ctitles("", Treatment, "", "", Control, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 
	
	
***Proportion of girls at every birth order for transitional mothers



eststo clear 



estpost summarize cs1 cs2 cs3 cs4 if  rural==1 & transitional==1
esttab using "$js21\jope\KS\jope\tablebordtr.tex", replace b(%12.3f) label substitute(# \#) ///
    fragment nomtitle nonumber noobs cells("count(fmt(a2)) mean sd min max")
	
global DESCVARS cs1 cs2 cs3 cs4
mata: mata clear

	
* First test of differences
local i = 1

foreach var in $DESCVARS {
    reg `var' treat if  rural==1  & transitional==1 ,  cl( v024)
    outreg, keep(treat)  rtitle("`: var label `var''") stats(b)  ///
        noautosumm store(row`i')  starlevels(10 5 1) starloc(1)
    outreg, replay(diff) append(row`i') ctitles("",Difference )  ///
        store(diff) note("")
    local ++i
}


		

* Then Summary statistics
local count: word count $DESCVARS
mat sumstat = J(`count',6,.)

local i = 1
foreach var in $DESCVARS {
    quietly: summarize `var' if treat==1 & rural==1 &  transitional==1
    mat sumstat[`i',1] = r(N)
    mat sumstat[`i',2] = r(mean)
    mat sumstat[`i',3] = r(sd)
    quietly: summarize `var' if treat==0 &  rural==1 & transitional==1
    mat sumstat[`i',4] = r(N)
    mat sumstat[`i',5] = r(mean)
    mat sumstat[`i',6] = r(sd)
    local i = `i' + 1
}
frmttable, statmat(sumstat) store(sumstat) sfmt(g,f,f,g,f,f)

*And export
outreg using "$js21\jope\KS\jope\balancebordtrans.tex", ///
    replay(sumstat) merge(diff) tex nocenter note("") fragment plain replace ///
    ctitles("", Treatment, "", "", Control, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 
	


	
	
	
**********************************************************************************************
*** first girl first boy

**********************************************************************************************

eststo clear
estpost summarize  hindu musl fcaste obc momedu shhead ahhead ultr poorest poorer middle richer richest elec truck fridge cycle tv radio if  rural==1 & b2>=2000 & b2 <=2005 & bord==1
esttab using "$js21/jope/KS/jope/table2.tex", replace b(%12.3f) label substitute(# \#) ///
    fragment nomtitle nonumber noobs cells("count(fmt(a2)) mean sd min max")
	
	

global DESCVARS  hindu musl fcaste obc momedu shhead ahhead ultr poorest poorer middle richer richest elec truck fridge cycle tv radio
mata: mata clear

* First test of differences
local i = 1

foreach var in $DESCVARS {
    reg `var' fg if  rural==1 & b2>=2000 & b2 <=2005 & bord==1 ,  cl( v024)
    outreg, keep(fg)  rtitle("`: var label `var''") stats(b)  ///
        noautosumm store(row`i')  starlevels(10 5 1) starloc(1)
    outreg, replay(diff) append(row`i') ctitles("",Difference )  ///
        store(diff) note("")
    local ++i
}
outreg, replay(diff)

* Then Summary statistics
local count: word count $DESCVARS
mat sumstat = J(`count',6,.)

local i = 1
foreach var in $DESCVARS {
    quietly: summarize `var' if fg==1 & rural==1 & b2>=2000 & b2 <=2005 & bord==1
    mat sumstat[`i',1] = r(N)
    mat sumstat[`i',2] = r(mean)
    mat sumstat[`i',3] = r(sd)
    quietly: summarize `var' if fg==0 &  rural==1 & b2>=2000 & b2 <=2005 & bord==1
    mat sumstat[`i',4] = r(N)
    mat sumstat[`i',5] = r(mean)
    mat sumstat[`i',6] = r(sd)
    local i = `i' + 1
}
frmttable, statmat(sumstat) store(sumstat) sfmt(g,f,f,g,f,f)

*And export
outreg using "$js21/jope/KS/balancefgfb.tex", ///
    replay(sumstat) merge(diff) tex nocenter note("") fragment plain replace ///
    ctitles("", First Girl Families, "", "", First Boy Families, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 


	
**********************************************************************************************

***parallel trends
**********************************************************************************************

set seed 12345

* Set the number of bootstrap replications (e.g., 1000)
local B=1000

* Perform the wild bootstrap with clustering using bsample
bootstrap, cluster(v024) reps(`B'): ///
    reg child_sex treat##i.b2 if rural == 1 & mtempo == 1 & b2 <= 2005

eststo para1
coefplot (para1,  keep(*#* )),  levels( 95) vertical yline(0) xlabel(,angle(90)) msymbol(S)  coeflabels(1.treat#2001.b2="2001" 1.treat#2002.b2="2002" 1.treat#2003.b2="2003" 1.treat#2004.b2="2004" 1.treat#2005.b2="2005" ) xtitle("Treat*Year") ytitle("Likelihood of girl birth") graphregion(fcolor(white))

* Set the number of bootstrap replications (e.g., 1000)
local B=1000

* Perform the wild bootstrap with clustering using bsample
bootstrap, cluster(v024) reps(`B'): ///
reg child_sex treat##i.b2##fg if  rural==1 & mtempo==1 & b2<=2005 & bord>1,  cl( v024)
eststo para2

coefplot (para2,  keep(1.treat#2001.b2#1.fg 1.treat#2003.b2#1.fg 1.treat#2004.b2#1.fg 1.treat#2005.b2#1.fg 1.treat#2002.b2#1.fg   )), levels( 95) vertical yline(0) xlabel(,angle(90)) msymbol(S) coeflabels( 1.treat#2001.b2#1.fg ="2001" 1.treat#2003.b2#1.fg ="2003" 1.treat#2004.b2#1.fg ="2004" 1.treat#2005.b2#1.fg ="2005" 1.treat#2002.b2#1.fg ="2002" ) xtitle("Treat*Year*First_Girl") ytitle("Likelihood of girl birth") graphregion(fcolor(white))


*************************************************************************************************************************
* DiD, TD
*************************************************************************************************************************
* Difference-in-difference

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if  rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2   )
eststo did1

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05 if  rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2  )
eststo did2

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if  rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2  b1 )
eststo did3

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05 if  rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2 b1 )
eststo did4

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if  rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2#b1 )
eststo did5

* Triple difference

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID bord b2   )
eststo td1

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID bord b2   )
eststo td2

*st -fe

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID st_year b2   bord )
eststo td3

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID st_year b2   bord )
eststo td4

* st -trend

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID i.v024#c.b2 b2   bord )
eststo td5

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID i.v024#c.b2 b2   bord )
eststo td6

/*
* st bord - fe

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID i.v024#i.bord b2   bord )
eststo td7


*st bord fe

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID i.v024#i.bord b2   bord )
eststo td8

*/

*month of birth fe

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID bord b2 b1  )
eststo td9

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID bord b2  b1 )
eststo td10

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID bord b2#b1 )
eststo td11


/*
esttab did1 did2 td1 td2 td3 td4 td5 td6 td7 td8 using $js21/jope/KS/table1.tex, replace se b(%12.3f) mtitles("Girl" "Girl" "Girl" "Girl" "Girl" "Girl"  "Girl" "Girl" "Girl" "Girl") /// 
keep(1.treat#1.post 1.treat#1.p05 1.treat#2.p05 1.treat#1.post#1.fg 1.treat#1.p05#1.fg 1.treat#2.p05#1.fg )  ///
coeflabels(1.treat#1.post  "Treat*Post" 1.treat#1.p05 "Treat*Post2006-10" 1.treat#2.p05 "Treat*Post2011-15" 1.treat#1.post#1.fg "Treat*Post*FirstGirl" 1.treat#1.p05#1.fg "Treat*Post2006-10*FirstGirl" 1.treat#2.p05#1.fg "Treat*Post2011-15*First_Girl") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Main Results:  Estimation Results for Difference in Difference and Triple Difference Estimation\label{tab:DDTDD}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports difference in difference and  triple difference coefficient of the impact of JSY on the likelihood of observing the child born to be a girl. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, $FG$ is an indicator for if the woman's first born child was a girl. $Post$ compares post program years (2006-2015) to the pre program years (2000-2005). $Post_I$ and $Post_{II}$ are the early (2006-2010) and late diffusion (2011-2015) periods of the program. $FE$ contains mother, birth and year fixed effects. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Standard errors in parentheses are clustered at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 
*/


*DiD results
esttab did1 did2 did3 did4 did5 using $js21/jope/KS/tableDiD.tex, replace se b(%12.3f) mtitles("" "" "Dep Var: Girl" ""  "") /// 
keep(1.treat#1.post 1.treat#1.p05 1.treat#2.p05 )  ///
coeflabels(1.treat#1.post  "Treat*Post" 1.treat#1.p05 "Treat*Post2006-10" 1.treat#2.p05 "Treat*Post2011-15") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Main Results:  Estimation Results for Difference in Difference Estimation\label{tab:DiD}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports difference in difference estimation coefficient of the impact of JSY on the likelihood of observing the child born to be a girl. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. $Post$ compares post program years (2006-2015) to the pre program years (2000-2005). $Treat*Post2006-10$ and $Treat*Post2011-15$ are the early (2006-2010) and late diffusion (2011-2015) periods of the program. $FE$ contains mother, birth order, and year fixed effects as indicated. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. Season FE is birth month fixed effects and Season Year FE are month and year specific fixed effects. All standard errors are clustered bootstraped (with 1000 reps) at the state level and reported in parentheses." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 

*Triple Difference results 
esttab td1 td2 td3 td4 td5 td6 td9 td10 td11 using $js21/jope/KS/tableDDD.tex, replace se b(%12.3f) mtitles("" "" "" "" "Dep Var: Girl" "" "" "" "" "" "") /// 
keep(1.treat#1.post 1.treat#1.p05 1.treat#2.p05 1.treat#1.post#1.fg 1.treat#1.p05#1.fg 1.treat#2.p05#1.fg 1.post#1.fg)  ///
coeflabels(1.treat#1.post  "Treat*Post" 1.treat#1.p05 "Treat*Post2006-10" 1.treat#2.p05 "Treat*Post2011-15" 1.post#1.fg "Post*FirstGirl" 1.treat#1.post#1.fg "Treat*Post*FirstGirl" 1.treat#1.p05#1.fg "Treat*Post2006-10*FirstGirl" 1.treat#2.p05#1.fg "Treat*Post2011-15*First_Girl") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Main Results:  Estimation Results for Triple Difference Estimation\label{tab:DDD}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports triple difference estimation coefficient of the impact of JSY on the likelihood of observing the child born to first girl families is a girl. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, $FG$ is an indicator for if the woman's first born child was a girl.  $Post$ compares post program years (2006-2015) to the pre program years (2000-2005). $Treat*Post2006-10$ and $Treat*Post2011-15$ are the early (2006-2010) and late diffusion (2011-2015) periods of the program. $FE$ contains mother, birth order, and year fixed effects as indicated. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. Season FE is birth month fixed effects and Season Year FE are month and year specific fixed effects. All standard errors are clustered bootstraped (with 1000 reps) at the state level and reported in parentheses." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 

*Triple diff with inheritence law

gen iht=.
replace iht =1 if v024==17 | v024==2 | v024 ==16 | v024==20 | v024==31
replace iht=0 if iht==.
replace iht=1 if b2>=2005



local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg iht if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID b2   i.v024#c.b2 bord )
eststo inhe3

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg iht if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID b2   i.v024#c.b2 bord )
eststo inhe3_

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##fg fg##iht if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID b2   i.v024#c.b2 bord )
eststo inhe4_

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg iht if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID bord b2#b1)
eststo inhe5

esttab inhe3 inhe3_ inhe4_ inhe5 using $js21/jope/KS/table_inh.tex, replace se b(%12.3f) mtitles("" "" "Dep Var: Girl" "") /// 
keep(1.treat#1.post#1.fg 1.treat#1.p05#1.fg 1.treat#2.p05#1.fg iht 1.fg#1.iht)  ///
coeflabels( 1.treat#1.post#1.fg "Treat*Post*First_Girl" 1.treat#1.p05#1.fg "Treat*Post2006-10*First_Girl" 1.treat#2.p05#1.fg "Treat*Post2011-15*First_Girl" iht "Inheritance Law" 1.fg#1.iht "Inheritance Law*First_Girl") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Main Results:  Estimation Results for Triple Difference with Inheritance Law\label{tab:DDTDD}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports triple difference coefficient of the impact of JSY on the likelihood of observing the child born to be a girl controlling for the change in Inheritance law. $Treat$ is the dummy variable that takes the value 1 if the mother is from our treatment group. Similarly, $FG$ is an indicator for if the woman's first born child was a girl. $Post$ compares post program years (2006-2015) to the pre program years (2000-2005). $Post_I$ and $Post_{II}$ are the early (2006-2010) and late diffusion (2011-2015) periods of the program. Main FE contains mother, birth and year fixed effects. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. Season Year FE is birth month and year fixed effects All triple difference estimates are for the sex of the child born at birth order 2 or higher. Standard errors in parentheses are clustered at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 


*************************************************************************************************************************
* Robustness - non-differential pretrends
*************************************************************************************************************************
****bootstrap not working
local B=1000

bootstrap, cluster(v024) reps(2): ///
reghdfe child_sex treat##i.b2 if  rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2  )
eststo ro 


* Ftest
 test _b[1.treat#2001.b2]=_b[1.treat#2002.b2]=_b[1.treat#2003.b2]=_b[1.treat#2004.b2]=_b[1.treat#2005.b2]=0

local B=1000

bootstrap, cluster(v024) reps(`B'): /// 

areg child_sex treat##i.b2##fg  i.b2 i.bord if  rural==1 & mtempo==1 & bord>=2,  cl( v024) absorb(UID )
eststo rob2

*Ftest 
test _b[1.treat#2001.b2#1.fg]=_b[1.treat#2002.b2#1.fg]=_b[1.treat#2003.b2#1.fg]=_b[1.treat#2004.b2#1.fg]=_b[1.treat#2005.b2#1.fg]=0

coefplot (ro ,  keep(*#* )),  levels( 95) vertical yline(0) xlabel(,angle(90)) msymbol(S)  coeflabels(1.treat#2001.b2="2001" 1.treat#2002.b2="2002" 1.treat#2003.b2="2003" 1.treat#2004.b2="2004" 1.treat#2005.b2="2005" 1.treat#2006.b2="2006" 1.treat#2007.b2="2007" 1.treat#2008.b2="2008" 1.treat#2009.b2="2009" 1.treat#2010.b2="2010" 1.treat#2011.b2="2011" 1.treat#2012.b2="2012" 1.treat#2013.b2="2013" 1.treat#2014.b2="2014" 1.treat#2015.b2="2015" 1.treat#2016.b2="2016") xtitle("Treat*Year") ytitle("Likelihood of girl birth") graphregion(fcolor(white))

coefplot (rob2,  keep(1.treat#2001.b2#1.fg 1.treat#2003.b2#1.fg 1.treat#2004.b2#1.fg 1.treat#2005.b2#1.fg 1.treat#2006.b2#1.fg 1.treat#2007.b2#1.fg 1.treat#2008.b2#1.fg 1.treat#2009.b2#1.fg 1.treat#2010.b2#1.fg 1.treat#2011.b2#1.fg 1.treat#2012.b2#1.fg 1.treat#2013.b2#1.fg 1.treat#2014.b2#1.fg 1.treat#2015.b2#1.fg 1.treat#2016.b2#1.fg  )), levels( 95) vertical yline(0) xlabel(,angle(90)) msymbol(S) coeflabels( 1.treat#2001.b2#1.fg ="2001" 1.treat#2003.b2#1.fg ="2003" 1.treat#2004.b2#1.fg ="2004" 1.treat#2005.b2#1.fg ="2005" 1.treat#2006.b2#1.fg ="2006" 1.treat#2007.b2#1.fg ="2007" 1.treat#2008.b2#1.fg="2008" 1.treat#2009.b2#1.fg ="2009" 1.treat#2010.b2#1.fg ="2010" 1.treat#2011.b2#1.fg ="2011" 1.treat#2012.b2#1.fg ="2012" 1.treat#2013.b2#1.fg ="2013" 1.treat#2014.b2#1.fg ="2014" 1.treat#2015.b2#1.fg ="2015" 1.treat#2016.b2#1.fg ="2016") xtitle("Treat*Year*First_Girl") ytitle("Likelihood of girl birth") graphregion(fcolor(white))

local B=1000

bootstrap, cluster(v024) reps(2): ///
reghdfe child_sex treat##ib2005.b2 if  rural==1 & mtempo==1,  vce(robust) absorb(UID bord b2)
eststo ro1 

coefplot (ro1 ,  keep(*#* )),  levels( 95) vertical yline(0) xlabel(,angle(90)) msymbol(S)  coeflabels(1.treat#2001.b2="2001" 1.treat#2002.b2="2002" 1.treat#2003.b2="2003" 1.treat#2004.b2="2004" 1.treat#2005.b2="2005" 1.treat#2006.b2="2006" 1.treat#2007.b2="2007" 1.treat#2008.b2="2008" 1.treat#2009.b2="2009" 1.treat#2010.b2="2010" 1.treat#2011.b2="2011" 1.treat#2012.b2="2012" 1.treat#2013.b2="2013" 1.treat#2014.b2="2014" 1.treat#2015.b2="2015" 1.treat#2016.b2="2016") xtitle("Treat*Year") ytitle("Likelihood of girl birth") graphregion(fcolor(white)) 


 
*************************************************************************************************************************
* Mortality
*************************************************************************************************************************

*one year
gen dead1=1 if b6<=212 | b6==301
replace dead1=0 if dead1==.
*under5
gen dead5=1 if b6<=305
replace dead5=0 if dead5==.

* All kids

* Under 1

local B=1000

bootstrap, cluster(v024) reps(`B'): /// 
reghdfe dead1 treat##i.p05##child_sex  if rural==1  & mtempo==1 , cl(v024) absorb(  bord b2   UID  )
eststo m1

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##post##child_sex   if rural==1   & mtempo==1 , cl(v024) absorb(  bord b2   UID  )
eststo m2

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##post##child_sex   if rural==1   & mtempo==1 , cl(v024) absorb(  bord b2#b1   UID  )
eststo m2a
*/

* Under 5

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##i.p05##child_sex   if rural==1  & mtempo==1 , cl(v024) absorb(  bord b2   UID  )
eststo m3

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##post##child_sex   if rural==1   & mtempo==1 , cl(v024) absorb(  bord b2   UID  )
eststo m4

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##post##child_sex   if rural==1   & mtempo==1 , cl(v024) absorb(  bord b2#b1  UID  )
eststo m4a
*/

* Birth order >1 i.e parity 2 onwards

* 1 year

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##i.p05##child_sex   if rural==1  & mtempo==1 & bord>1, cl(v024) absorb(  bord b2   UID  )
eststo m5

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##post##child_sex   if rural==1   & mtempo==1 & bord>1, cl(v024) absorb(  bord b2   UID  )
eststo m6

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##post##child_sex   if rural==1   & mtempo==1 & bord>1, cl(v024) absorb(  bord b2#b1   UID  )
eststo m6a
*/

*Under 5

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##i.p05##child_sex   if rural==1  & mtempo==1 & bord>1, cl(v024) absorb(  bord b2   UID  )
eststo m7

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##post##child_sex   if rural==1   & mtempo==1 & bord>1, cl(v024) absorb(  bord b2   UID  )
eststo m8

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##post##child_sex   if rural==1   & mtempo==1 & bord>1, cl(v024) absorb(  bord b2#b1  UID  )
eststo m8a
*/

* Birth order >2 i.e parity 3 onwards

* 1 year

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##i.p05##child_sex   if rural==1  & mtempo==1 & bord>2, cl(v024) absorb(  bord b2   UID  )
eststo m9

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead1 treat##post##child_sex   if rural==1   & mtempo==1 & bord>2, cl(v024) absorb(  bord b2   UID  )
eststo m10

local B=1000



* Under 5

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##i.p05##child_sex   if rural==1  & mtempo==1 & bord>2, cl(v024) absorb(  bord b2   UID  )
eststo m11

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe dead5 treat##post##child_sex   if rural==1   & mtempo==1 & bord>2, cl(v024) absorb(  bord b2   UID  )
eststo m12



esttab m2 m1 m6 m5 m10 m9 using $js21/jope/KS/mort1.tex, replace se b(%12.3f) mtitles("" "" "Dep Var: Dead" "" "" "") /// 
keep(1.treat#1.post#1.child_sex 1.treat#1.p05#1.child_sex 1.treat#2.p05#1.child_sex  )  ///
coeflabels(1.treat#1.post#1.child_sex  "Treat*Post*Girl" 1.treat#1.p05#1.child_sex "Treat*Post2006-10*Girl" 1.treat#2.p05#1.child_sex "Treat*Post2011-15*Girl") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{  Estimation Results for Mortality for children under 1 year{tab:mort1}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports the mortality outcomes for children below age 1. Columns 1 and 2 record the likelihood of girls dying before reaching age 1.Columns 3 and 4 record the likelihood of girls born at parity 2 and above dying before reaching age 1. Columns 5 and 6 record the likelihood of girls born at parity 3 and above dying before reaching age 1. Treat that takes the value 0 if the mother is from our treatment group. Post compares post program years (2006-2015) to the pre program years (2000-2005). Post2006-10 and Post2011-15 are the early (2006-2010) and late diffusion (2011-2015) periods of the program. Main FE contains mother, birth and year fixed effects. Standard errors in parentheses are clustered bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 


esttab m4  m3 m8 m7 m11 m12  using $js21/jope/KS/mort5.tex, replace se b(%12.3f) mtitles("Dead") /// 
keep(1.treat#1.post#1.child_sex 1.treat#1.p05#1.child_sex 1.treat#2.p05#1.child_sex  )  ///
coeflabels(1.treat#1.post#1.child_sex  "Treat*Post*Girl" 1.treat#1.p05#1.child_sex "Treat*Post2006-10*Girl" 1.treat#2.p05#1.child_sex "Treat*Post2011-15*Girl") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{  Estimation Results for Mortality for children under 5 year{tab:mort5}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\   The table reports the likelihood of a girl in treatment group dying before she reaches age 5.Columns 3 and 4 record the likelihood of girls born at parity 2 and above dying before reaching age 5. Columns 5 and 6 record the likelihood of girls born at parity 3 and above dying before reaching age 5. Treat that takes the value 0 if the mother is from our treatment group. Post compares post program years (2006-2015) to the pre program years (2000-2005). Post2006-10 and Post2011-15 are the early (2006-2010) and late diffusion (2011-2015) periods of the program. Main FE contains mother, birth and year fixed effects. Standard errors in parentheses are clustered bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 


*************************************************************************************************************************
* Falsification test plots for DiD 
*************************************************************************************************************************
* DiD
// assuming years from 1990 to 2004 to be program years
forval i = 1990/2000 {

bys UID :gen temp`i'=1 if b2>=`i' & bord==1
replace temp`i'=0 if temp`i'==.
bys UID :egen mtemp`i'=max(temp`i') 

bys UID :gen k`i' =1 if b2 >`i' & b2<=`i'+5
replace k`i' =0 if b2>=`i'-5 & b2<=`i'

}

forval i = 2001/2004 {

bys UID :gen temp`i'=1 if b2>=`i' & bord==1
replace temp`i'=0 if temp`i'==.
bys UID :egen mtemp`i'=max(temp`i') 

bys UID :gen k`i' =1 if b2 >`i' & b2<=2005
replace k`i' =0 if b2>=`i'-5 & b2<=`i'

}


*Bootstrap issues in 1995
forval i = 1990/2004 {

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
quietly reghdfe child_sex treat##k`i'   if  rural==1 & mtemp`i'==1,  cl( v024) absorb(UID b2   bord v024)
eststo o`i'
}

*Ttest
forval i = 1990/2004 {

reghdfe child_sex treat##i0b.k`i'   if  rural==1 & mtemp`i'==1,  cl( v024) absorb(UID b2   bord v024)
eststo o`i'
}

*Ftest
test _b[1.treat#2001.b2#1.fg]=_b[1.treat#2002.b2#1.fg]=_b[1.treat#2003.b2#1.fg]=_b[1.treat#2004.b2#1.fg]=_b[1.treat#2005.b2#1.fg]=0



coefplot (o1990,  keep(*#* ) )(o1991,  keep(*#* ) )(o1992, ciopts(pstyle(p3)))(o1993,  ciopts(pstyle(p3)) ) (o1994,  ciopts(pstyle(p3)) ) (o1995,  ciopts(pstyle(p3)) keep(*#* ) )(o1996, ciopts(pstyle(p3)) ) (o1997,  ciopts(pstyle(p3)) keep(*#* )) (o1998,  ciopts(pstyle(p3)) ) (o1999,  ciopts(pstyle(p3)) )(o2000, ciopts(pstyle(p3))) (o2001, keep(*#* )) (o2002, keep(*#* ) ) (o2003,  keep(*#* ) ) (o2004, keep(*#* ) ), vertical yline(0) xlabel(,angle(90)) msymbol(S) offset(.1) coeflabels(1.treat#1.k1990="1990" 1.treat#1.k1991="1991" 1.treat#1.k1992="1992" 1.treat#1.k1993="1993" 1.treat#1.k1994=" 1994" 1.treat#1.k1995=" 1995" 1.treat#1.k1996=" 1996"1.treat#1.k1997=" 1997" 1.treat#1.k1998=" 1998" 1.treat#1.k1999=" 1999" 1.treat#1.k2000=" 2000" 1.treat#1.k2001=" 2001" 1.treat#1.k2002="2002" 1.treat#1.k2003="2003" 1.treat#1.k2004="2004") xtitle(Treat*Assumed_Program_year) ytitle(Likelihood of girl birth) graphregion(fcolor(white)) drop(_cons) xline(0)

graph export "$js21/jope/KS/DDfal90_04.png",replace
 





*************************************************************************************************************************
* Mechanisms 
*************************************************************************************************************************

*ULTRASOUND

sort UID v021 bord b2 


*Children born in the PSU from 2010 onwards 
gen chil2010=1 if b2>=2010
bys v021: egen nchil2010 = count(chil2010) if b2>=2010
replace nchil2010=0 if missing(nchil2010)
gen pnsd = s220b
replace pnsd=0 if missing(pnsd)
*Total children borm since  2010 in the Primary sampling unit or neighborhood
bys v021: egen tot_nchil2010= max( nchil2010)


*no of births in the psu that used pnsd since  2010
bys v021: egen max_pnsd=total(pnsd)
bys v021: gen psu_use10=(max_pnsd-pnsd)/tot_nchil2010

*likelihood of uultrasound
bys v021:gen lius10=1-psu_use10

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##c.lius10  if rural==1 & mtempo==1 & bord>1, cl(v024) absorb(b2   bord UID)

*reghdfe child_sex treat##c.lius10##fg  if rural==1 & mtempo==1 & bord>1, cl(v024) absorb(b2 bord UID)
eststo reg1

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##s220b   if rural==1 & mtempo==1 , cl(v024) absorb(b2   bord UID)
eststo reg2


esttab reg1 reg2 using $js21/jope/KS/ultra.tex, replace se b(%12.3f) mtitles("Dep Var:" "Girl"  )starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Mechanism:  Ultrasound\label{ultra}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports coefficients of the impact of the likelihood of ultrasound avaibility in the neighbourhood on the likelihood of observing the child born to be a girl. The likelihood of ultrasound avaibility data is observed from 2010 onwards. Treat is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, First Girl is an indicator for if the woman's first born child was a girl. Post compares post program years (2006-2015) to the pre program years (2000-2005). Post2006-10 and Post2011-15 are the early (2006-2010) and late diffusion (2011-2015) periods of the program. Main FE contains mother, birth and year fixed effects. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Column 1 reports results using liklihood of ultrasound and column 2 reports results from reported ultrasound usage. Standard errors in parentheses are clustered Bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 
 
/*
*ULTRASOUND with only mothers of boys

sort UID v021 bord b2

/*
*Children born in the PSU from 2010 onwards
gen chil2010=1 if b2>=2010
bys v021: egen nchil2010 = count(chil2010) if b2>=2010
replace nchil2010=0 if missing(nchil2010)
gen pnsd = s220b
*replace pnsd=0 if missing(pnsd)
*Total children borm since  2010 in the Primary sampling unit or neighborhood
bys v021: egen tot_nchil2010= max( nchil2010)

*no of births in the psu that used pnsd since  2010
bys v021: egen max_pnsd=total(pnsd)
bys v021: gen psu_use10=(max_pnsd-pnsd)/tot_nchil2010
*likelihood of uultrasound
bys v021:gen lius10=1-psu_use10
*/



*no of births in the psu that used pnsd since  2010 for boyss
gen pnsd_b=s220b if b4==1
bys v021: egen max_pnsd_b=total(pnsd_b)
bys v021: gen psu_use10_b=(max_pnsd_b-pnsd)/tot_nchil2010
bys UID: egen max_psu_use10_b = max(psu_use10_b)
replace max_psu_use10_b = psu_use10_b if psu_use10_b!=.

*likelihood of uultrasound for boys
bys v021:gen lius10_b=1-psu_use10_b

local B=10

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##c.lius10_b##fg  if rural==1 & mtempo==1 & bord>=2, cl(v024) absorb(b2   bord UID)
eststo reg1
reghdfe child_sex treat##c.lius10##fg  if rural==1 & mtempo==1 & bord>=2, cl(v024) absorb(b2   bord UID)
reghdfe child_sex treat##c.lius10_b   if rural==1 & mtempo==1 , cl(v024) absorb(b2   bord UID)
eststo reg2


*esttab reg1 reg2 , b(%10.4f)se scalars(N r2 F) star(* 0.10 ** 0.05 *** 0.01) mtitles long tex
esttab reg1 reg2 using $js21\jope\ultraboy.tex, replace se b(%12.3f) mtitles("Girl" "Girl"  )starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Mechanism:  Ultrasound with only mothers of boys\label{ultra}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports coefficients of the impact of the likelihood of ultrasound avaibility in the neighbourhood on the likelihood of observing the child born to be a girl. The likelihood of ultrasound avaibility data is observed from 2010 onwards. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, $FG$ is an indicator for if the woman's first born child was a girl. $Post$ compares post program years (2006-2015) to the pre program years (2000-2005). $Post_I$ and $Post_{II}$ are the early (2006-2010) and late diffusion (2011-2015) periods of the program. $FE$ contains mother, birth and year fixed effects. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Standard errors in parentheses are clustered at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 
 */

*INCOME EFFECT USING RAINFALL INFORMATION

gen dist_dhs=sdistri
sort dist_dhs b2

egen UID_m  = concat(dist_dhs b2)
destring UID_m, replace

merge m:m UID_m using "/Users/kritzz/Library/CloudStorage/Dropbox/JavadekarSaxena2021/dta files/rain_use1.dta",gen(m2)
*keep if m2==3

*Rainfall shock 1st and 2nd lag depending on the month of birth of the child. 
*Child born after June, 1st lag; child born before May, 2nd lag
gen rain=0
replace rain=z1 if  z1>=6
replace rain=z2 if  z2<=5
 

 *Rainfall below long run mean
 
gen drys=1 if rain<0
replace drys =0 if missing(drys) & rain!=.

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##post##drys   if  rural==1 & mtempo==1,  cl( v024) absorb(UID   b2 bord )
eststo d1

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##drys   if  rural==1 & mtempo==1,  cl( v024) absorb(UID   b2 bord )
eststo d2
*/

drop drys


 *Rainfall 1sd below long run mean (need to re-estimate this)
gen drys=1 if rain<-1
replace drys=0 if missing(drys) & rain!=.


local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##post##drys   if  rural==1 & mtempo==1,  cl( v024) absorb(UID b2   bord )
eststo d3

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##drys   if  rural==1 & mtempo==1,  cl( v024) absorb(UID b2   bord )
eststo d4
*/

drop drys


 *Rainfall 2 sd below long run mean
gen drys=1 if rain<-2
replace drys=0 if missing(drys) & rain!=.


local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##post##drys   if  rural==1 & mtempo==1,  cl( v024) absorb(UID b2   bord )
eststo d5

/*
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.p05##drys   if  rural==1 & mtempo==1,  cl( v024) absorb(UID b2   bord )
eststo d6
*/

drop drys


esttab d1  d3  d5  using $js21/jope/KS/rainfall.tex, replace se b(%12.3f) mtitles("Girl") /// 
keep(1.treat#1.post#1.drys  1.treat#1.drys 1.post#1.drys 1.treat#1.post  )  ///
coeflabels(1.treat#1.post#1.drys  "Treat*Post*Rain_Shock" 1.treat#1.drys "Treat*Rain_Shock" 1.post#1.drys "Post*Rain_Shock" 1.treat#1.post "Treat*Post") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Mechanism:  Income Effect}\label{rain}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\   The table reports coefficients of an income shock represented by rainfall below long run mean on the likelihood of observing the child born to be a girl. Columns 1, 2 and 3 record the effect of rainfall below long run mean, rainfall 1 and 2 standard deviations below long run  mean respectively . Treat is the dummy variable that takes the value 0 if the mother is from our treatment group. Post compares post program years (2006-2015) to the pre program years (2000-2005). Main FE contains mother, birth order and year fixed effects. $Rain\_Shock$ is a dummy variable if the rainfall was below long run mean in column 1, below 1 standard deviation in columns 2; and below 2 standard deviations in column 3. Standard errors in parentheses are clustered bootstrapped (1000 resp) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 


*WEALTH EFFECT


gen wealth=s190r
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##post##ib5.wealth   if  rural==1 & mtempo==1,  cl( v024) absorb(UID b2 bord  )
eststo w1



esttab w1 using $js21/jope/KS/wealth.tex, replace se b(%12.3f) mtitles("Girl") /// 
keep(1.treat#1.post#1.wealth 1.treat#1.post#4.wealth  1.treat#1.post#3.wealth   1.treat#1.post#2.wealth 1.post#1.wealth 1.post#2.wealth 1.post#3.wealth 1.post#4.wealth)  ///
coeflabels(1.treat#1.post#4.wealth "Treat*Post*Richer" 1.treat#1.post#3.wealth "Treat*Post*Middle" 1.treat#1.post#2.wealth "Treat*Post*Poorer"  1.treat#1.post#1.wealth "Treat*Post*Poorest" 1.post#1.wealth "Post*Poorest" 1.post#2.wealth "Post*Poorer" 1.post#3.wealth "Post*Middle" 1.post#4.wealth "Post*Richer") collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Mechanism:  Wealth Effect}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\   The table reports coefficients of the effect of health workers per 10,000 women on the likelihood of observing the child born to be a girl. The data on health workers is available from 2008 on wards. In column (1) and (2), we use the data on the number of health workers who were paid JSY incentives for deliveries in public institutions and private institutions, respectively.  Treat is the dummy variable that takes the value 1 if the mother is from our treatment group.  Main FE contains mother, birth order and year fixed effects. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. Standard errors are clustered bootstrapped (1000 reps) at the state level.." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 



*HEALTH WORKERS - ASHA
*cen_ash_dhs.dta is obtained from Step4_merge_census_asha_dhs.py

gen y=b2

/*MERGE with asha infor*/
merge m:m  sdistri y using "$js21/dta files/cen_ash_dhs",gen(merge1)   
gen asha=asha_pub_m + asha_pri_m

*public
gen hw=asha_pub_m/10000
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex  c.hw#1b.treat treat c.hw  if  rural==1 & mtempo==1 ,cl(v024) absorb( UID b2   bord)
*reghdfe child_sex  c.hw_pub##i.treat st_year  if  rural==1 & mtemp==1 ,cl(v024) absorb( UID b2 bord)
eststo hw1

drop hw

*private
gen hw=asha_pri_m/10000

local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex c.hw treat c.hw#1b.treat   if  rural==1 & mtempo==1 ,cl(v024) absorb( UID b2   bord)
eststo hw2


esttab hw1 hw2  using $js21/jope/KS/hw.tex, replace se b(%12.3f) mtitles("Girl") /// 
keep(1.treat#c.hw hw)  ///
coeflabels(1.treat#c.hw "Treat*Health_Worker" hw )collabels(none) ///
starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Mechanism:  Health Workers Effect}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\   The table reports coefficients of the effect of health workers per 10,000 women on the likelihood of observing the child born to be a girl. The reference is the poorest category given in the DHS data. $Treat$ is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, $FG$ is an indicator for if the woman's first born child was a girl. $Post$ compares post program years (2006-2015) to the pre program years (2000-2005). $Post_I$ and $Post_{II}$ are the early (2006-2010) and late diffusion (2011-2015) periods of the program. $FE$ contains mother, birth and year fixed effects. State Year Trend is the state specific time trend and State Year FE is the State Year specific fixed effect. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Standard errors in parentheses are clustered at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 



********************************************************************************
*additional regressions for the reviweres - for Appendix
********************************************************************************

*Urban sample regression

*DiD - full sample
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if mtempo==1,  cl( v024) absorb(UID bord b2 )
eststo u1
*DiD - rural sample
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if rural==1 & mtempo==1,  cl( v024) absorb(UID bord b2 )
eststo u2
*DiD - Urban sample
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if rural==0 & mtempo==1,  cl( v024) absorb(UID bord b2 )
eststo u3

*DDD - full sample
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if   bord>=2 & mtempo==1,  cl( v024) absorb(UID st_year b2   bord )
eststo u4

* DDD - rural sample
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==1 & bord>=2 & mtempo==1,  cl( v024) absorb(UID st_year b2   bord )
eststo u5

*DDD - urban sample
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post##fg  if  rural==0 & bord>=2 & mtempo==1,  cl( v024) absorb(UID st_year b2   bord )
eststo u6
*without mtempo
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe child_sex treat##i.post if  rural==1,  cl( v024) absorb(UID bord b2   )
eststo u7

esttab u1 u2 u3 u4 u5 u6 u7 using $js21/jope/KS/urban.tex, replace se b(%12.3f) mtitles("Dep Var:" "Girl"  )starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Robustness: Urban Sample}\label{urban}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports coefficients of the difference in difference and triple difference estimators on the likelihood of observing the child born to be a girl. Treat is the dummy variable that takes the value 1 if the mother is from our treatment group. Similarly, First Girl is an indicator for if the woman's first born child was a girl. Post compares post program years (2006-2015) to the pre program years (2000-2005). Main FE contains mother, birth and year fixed effects. State Year FE includes State year fixed effects. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Column 1 and 4 report results for full sample that includes all urban and rural regions. Column 2 and 5 report results for rural sample and columns 3 and 6 report results only for the urban sample. Column 7 reports the difference in difference estimation including rural women who statrted their fertility before the year 2000. Standard errors in parentheses are clustered bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 
 


***Supply side differences

gen unmetneed =1 if v626a >=1 & v626a<=4
replace unmetneed=0 if unmetneed==.
local B=1000

bootstrap, cluster(v024) reps(`B'): ///
reghdfe unmetneed treat##fg, absorb( sdistri) cl(v024)
eststo u8


esttab u8 using $js21/jope/KS/unmet.tex, replace se b(%12.3f) mtitles("Unmet need for Contraception")starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Robustness: Urban Sample}\label{urban}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports coefficients for proxy for supply side differences. Treat is the dummy variable that takes the value 1 if the mother is from our treatment group. Similarly, First Girl is an indicator for if the woman's first born child was a girl. Dependent variable is unmet need for contraception which is women who do not want more children or get pregnant but are not using any form of contraception or family planning. Standard errors in parentheses are clustered bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 


*DiD for FG and Post in treat=1

reghdfe child_sex fg##i.post if rural==1 & mtempo==1 & treat==1, vce(robust) absorb(UID bord b2 )
eststo treat1

reghdfe child_sex fg##i.post if rural==1 & mtempo==1 & treat==0,  vce(robust) absorb(UID bord b2 )
eststo treat0

esttab treat1 treat0 using $js21/jope/KS/treat.tex, replace se b(%12.3f) mtitles("UDep Var: Girl")starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Robustness: Urban Sample}\label{urban}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports coefficients for proxy for supply side differences. Treat is the dummy variable that takes the value 1 if the mother is from our treatment group. Similarly, First Girl is an indicator for if the woman's first born child was a girl. Dependent variable is unmet need for contraception which is women who do not want more children or get pregnant but are not using any form of contraception or family planning. Standard errors in parentheses are clustered bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 


*ultrasound use with first girl


reghdfe child_sex treat##i.fg##c.lius10  if rural==1 & mtempo==1 & bord>1, cl(v024) absorb(b2   bord UID)

eststo ultra1

reghdfe child_sex treat##i.fg##s220b   if rural==1 & mtempo==1 & bord>1, cl(v024) absorb(b2   bord UID)
eststo ultra2


esttab ultra1 ultra2 using $js21/jope/KS/ultra_appendix.tex, replace se b(%12.3f) mtitles("Dep Var:" "Girl"  )starlevels(* 0.10 ** 0.05 *** 0.010) stats(N , labels ("No. of Obs.")fmt(0))  ///
nolines prehead("\begin{table}[htbp]" "\centering"    "\begin{threeparttable}" "\caption{Mechanism:  Ultrasound\label{ultra}}"   "\begin{tabular}{lc*{@M}{c}}" "\toprule") posthead(\midrule) ///
prefoot(\midrule) postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" ///
"\tiny" "\item \textit{Notes}: \$^{*}\$p\$<\$0.1; \$^{**}\$p\$<\$0.05;\$^{***}\$p\$<\$0.01 \\  The table reports coefficients of the impact of the likelihood of ultrasound avaibility in the neighbourhood on the likelihood of observing the child born to be a girl. The likelihood of ultrasound avaibility data is observed from 2010 onwards. Treat is the dummy variable that takes the value 0 if the mother is from our treatment group. Similarly, First Girl is an indicator for if the woman's first born child was a girl. Post compares post program years (2006-2015) to the pre program years (2000-2005). Post2006-10 and Post2011-15 are the early (2006-2010) and late diffusion (2011-2015) periods of the program. Main FE contains mother, birth and year fixed effects. All triple difference estimates are for the sex of the child born at birth order 2 or higher. Column 1 reports results using liklihood of ultrasound and column 2 reports results from reported ultrasound usage. Standard errors in parentheses are clustered Bootstrapped (1000 reps) at the state level." ///
"\end{tablenotes}" "\end{threeparttable}" "\end{table}") 
