********************************************************************************
* DATE: Jan 26th, 2021                                                         *
* FILE: growth_regressionsRR                                                   * 
* DATAIN: GBD CID PWT WDI FIW.dta, indicators.dta, WVS and BL.dta, pop1990,    *
*		 rail.dta, classification2016   									   *
* DATAOUT:   data/dataset_rr                                                   *
*                                                                    		   *                       
********************************************************************************

clear
set more off

/*
global root = "C:\Users\rocclor93312\dropbox\health and growth"
global data = "$root\data_check"
global data = "$root\data_check"
global do = "$root\do"
global log = "$root\log"
global output = "$root\output"
global graph = "$root\graphs"
*/

global root = " "																/* insert root */
global data = "$root/data"
global data = "$root/data_check"
global do = "$root/do"
global log = "$root/log"
global output = "$root/output"
global graph = "$root/graphs"


use "$data/GBD CID PWT WDI FIW.dta" 
merge m:1 countrycode using "$data/indicators.dta" 
tab countrycode _m
drop _m

merge m:1 countrycode using "$data/WVS and BL.dta" 
tab countrycode _m
drop if _merge==2
drop _m

merge m:1 countrycode using "$data/pop1990.dta" 
tab countrycode _m
drop if _merge==2
drop _m

*graph scheme

set scheme s1mono

**********************************GLOBALS***************************************
global age = "All Ages"															 		/* RR */
global measure = "Mortality"
global inst1 = "ME"
global inst2 = "KG_A KG_B KG_C" 		/* KG_D */
global inst3 = "lat elevation distcoast lnarea"

*****************************VARIABLES DEFINITION*******************************

keep if age == "$age"

encode countrycode, gen(cnt)

gen KG_A = (kg_a_af ==1 | kg_a_am==1 | kg_a_aw==1)
gen KG_B = (kg_a_bs ==1 | kg_a_bw==1)
gen KG_C = (kg_a_cf ==1 | kg_a_cs==1 | kg_a_cw==1)
gen KG_D = (kg_a_df ==1 | kg_a_dw==1)
gen KG_E = (kg_a_e ==1)
gen KG_H = (kg_a_h ==1)

ren cen_lat lat
ren elev elevation 
ren distc distcoast
ren areakm2 area
gen lnarea = log(area)

replace pop = pop * 1000000
gen lpop = log(pop)
gen openk = (q_x + q_m)/ q_gdp * 100
gen lpopXopenk = lpop*openk

gen gdppc = rgdpna*1000000/pop

sort cnt year
by cnt: replace Allcause_nDalys_both=Allcause_nDalys_both[_n+1] if _n==_N-1  /* 2014 = 2015 */
by cnt: replace RTI_nDalys_both=RTI_nDalys_both[_n+1] if _n==_N-1  /* 2014 = 2015 */

gen rtimort = RTI_rDeaths_both 
gen allmort = Allcause_rDeaths_both 
 
gen alldaly = Allcause_nDalys_both/pop*100
gen rtidaly = RTI_nDalys_both/pop*100


br gdppc year

list allmort if countryname == "Niger" & year == 1990
list allmort if countryname == "Niger" & year == 2014
list allmort if countryname == "United States" & year == 1990
list allmort if countryname == "United States" & year == 2014
list allmort if countryname == "India" & year == 1990
list allmort if countryname == "India" & year == 2014
list allmort if countryname == "China" & year == 1990
list allmort if countryname == "China" & year == 2014

global Niger1990 = 2063.802 
global Niger2014 = 979.3613  
global USA1990 =  862.1935  
global USA2014 = 826.5507 
global India1990 =  1077.983   
global India2014 = 790.4222  
global China1990 =  767.0129   
global China2014 =  681.925  



*I compute the variation in Mortality between 1990 and 2004 (I made it positive)                    

foreach a in Niger USA India China{
global D`a'Mort=-(($`a'2014-$`a'1990)/$`a'1990)
}


*I compute the variation in mortality D' (bigger decrease than D)
***D'=D-0.01 or D-0.05 (1pp/5pp decrease)**

foreach a in Niger USA India China {
foreach b in 01 05 {
global D`b'`a'Mort=${D`a'Mort}+0.`b'
}
}

***D'=D+0.01 D'=D+0.05 (A decrease 1 pp less than the status quo)**							*** RR ***

foreach a in Niger USA India China {
foreach b in 01 05 {
global DI`b'`a'Mort=${D`a'Mort}-0.`b'
}
}


display $DNigerMort " " $DIndiaMort " " $DChinaMort " " $DUSAMort
display $D01NigerMort " " $D01IndiaMort " "$D01ChinaMort " " $D01USAMort
display $D05NigerMort " " $D05IndiaMort " "$D05ChinaMort " " $D05USAMort
display $DI01NigerMort " " $DI01IndiaMort " "$DI01ChinaMort " " $DI01USAMort
display $DI05NigerMort " " $DI05IndiaMort " "$DI05ChinaMort " " $DI05USAMort


*I compute the average Mortality between 2014 and 2039 (status quo) M=Mort(2014)*(1-D(1-0.5/24))

*Niger
global MNigerMort= $Niger2014*(1-$DNigerMort*(1-0.5/24))
*Usa
global MUSAMort= $USA2014*(1-$DUSAMort*(1-0.5/24))
*India
global MIndiaMort= $India2014*(1-$DIndiaMort*(1-0.5/24))
*China
global MChinaMort= $China2014*(1-$DChinaMort*(1-0.5/24))

display $MNigerMort " " $MUSAMort " " $MIndiaMort " " $MChinaMort

*I compute the average Mortality between 2014 and 2039 (1% reduction) M=Mort(2014)*(1-D'(1-0.5/24))
*Niger
global M01NigerMort= $Niger2014*(1-$D01NigerMort*(1-0.5/24))
*Usa
global M01USAMort= $USA2014*(1-$D01USAMort*(1-0.5/24))
*India
global M01IndiaMort= $India2014*(1-$D01IndiaMort*(1-0.5/24))
*China
global M01ChinaMort= $China2014*(1-$D01ChinaMort*(1-0.5/24))

*I compute the average Mortality between 2014 and 2039 (5% reduction) M=Mort(2014)*(1-D'(1-0.5/24))
*Niger
global M05NigerMort= $Niger2014*(1-$D05NigerMort*(1-0.5/24))
*Usa
global M05USAMort= $USA2014*(1-$D05USAMort*(1-0.5/24))
*India
global M05IndiaMort= $India2014*(1-$D05IndiaMort*(1-0.5/24))
*China
global M05ChinaMort= $China2014*(1-$D05ChinaMort*(1-0.5/24))


*I compute the average Mortality between 2014 and 2039 (bad case 0.01) M=Mort(2014)*(1-D'(1-0.5/24))             ***RR***
*Niger
global MI01NigerMort= $Niger2014*(1-$DI01NigerMort*(1-0.5/24))
*Usa
global MI01USAMort= $USA2014*(1-$DI01USAMort*(1-0.5/24))
*India
global MI01IndiaMort= $India2014*(1-$DI01IndiaMort*(1-0.5/24))
*China
global MI01ChinaMort= $China2014*(1-$DI01ChinaMort*(1-0.5/24))

*I compute the average Mortality between 2004 and 2039 (bad case 0.01) M=Mort(2014)*(1-D'(1-0.5/24))             ***RR***
*Niger
global MI05NigerMort= $Niger2014*(1-$DI05NigerMort*(1-0.5/24))
*Usa
global MI05USAMort= $USA2014*(1-$DI05USAMort*(1-0.5/24))
*India
global MI05IndiaMort= $India2014*(1-$DI05IndiaMort*(1-0.5/24))
*China
global MI05ChinaMort= $China2014*(1-$DI05ChinaMort*(1-0.5/24))

display $MNigerMort " " $MUSAMort " " $MIndiaMort " " $MChinaMort
display $M01NigerMort " " $M01USAMort " " $M01IndiaMort " " $M01ChinaMort
display $M05NigerMort " " $M05USAMort " " $M05IndiaMort " " $M05ChinaMort
display $MI01NigerMort " " $MI01USAMort " " $MI01IndiaMort " " $MI01ChinaMort
display $MI05NigerMort " " $MI05USAMort " " $MI05IndiaMort " " $MI05ChinaMort

keep if year<=2014

gen lgdppc = log(gdppc)

table year, c(m lgdppc n lgdppc)

sort cnt year
*by cnt : gen growth = lgdppc[_N]-lgdppc[1]
by cnt : gen growth = (gdppc[_N]-gdppc[1])/gdppc[1]
replace growth = growth*100

table year, c(m growth n growth)
tab growth
drop if growth> 6*100 /* Equatorial Guinea 18 */ 
table year, c(m growth n growth)

by cnt : gen lgdppc90 = lgdppc[1]
 
gen G = q_g/q_gdp * 100 

gen na = country=="United States" | country=="Canada"
gen au = country=="Australia" | country=="New Zealand"
gen richasia = country=="Japan" | country == "Korea, Republic of" | country=="Singapore"
gen OECD1990 = eu ==1 | na==1 | country=="Turkey"
gen OECD2000= OECD1990==1 | au==1 | richasia==1


global X = "lgdppc90 lpop G urb openk cl popden lpopXopenk"  
global XX = "lgdppc90 lpop G urb openk cl popden lpopXopenk zpolar zboreal zdrytemp zwettemp zsubtrop ztropics"

*save "$data\dataset.dta", replace
save "$data/dataset.dta", replace

preserve
**Prepare the country averaged controls and add them**

collapse $XX alldaly rtidaly rtimort allmort trust m_trust percsecedu m_percsecedu pop*_1990, by(countrycode)
*save "$data\averages.dta", replace
save "$data/averages.dta", replace
restore

drop $XX alldaly rtidaly rtimort allmort trust m_trust percsecedu m_percsecedu pop*_1990
*merge m:1 countrycode using "$data\averages.dta"
merge m:1 countrycode using "$data/averages.dta"
drop _merge 

**Add rail data**
*merge m:1 countrycode using "$data\rail.dta"
merge m:1 countrycode using "$data/rail.dta"
tab countrycode
gen raildensity = railkm/area
gen raildensitybis = railkm/pop
drop if _merge==2
drop _merge

*merge m:1 countrycode using "$data\temp.dta"
merge m:1 countrycode using "$data/temp.dta"
drop if _merge==2
drop _merge

*merge m:1 countrycode using "$data\classification2016.dta"
merge m:1 countrycode using "$data/classification2016.dta"
drop if _merge==2
drop _merge

sum lgdppc90 if year==1990, det
gen highincome=lgdppc90>r(p75)

gen highgrowth = growth>4*100
gen rtidaly2 = rtidaly^2
gen daly = alldaly - rtidaly
*lowess growth gasoline if highgrowth==0
gen gasoline2 = gasoline^2
gen lgasoline = log(gasoline)

gen ratio = rtidaly/alldaly
replace ratio = ratio*100
sum ratio if year==1990, det

gen highratio = ratio>=r(p95)
gen bosnia = country=="Bosnia and Herzegovina"
drop if bosnia == 1 /*war in 1990 */

encode region, gen(reg)
encode income, gen(inc2016)

* I save the dataset to use it for the RR															**RR**


gen class = 1
replace class = 2 if low_middle == 1
replace class = 3 if upper_middle == 1
replace class = 4 if high == 1 

lab define classl 1 "Low" 2 "Low-middle" 3 "Upper-middle" 4 "High"
label values class classl  

save "$data/dataset_rr.dta", replace                                           

******************************GLOBALS*******************************************
*global XXX = "lgdppc90 OECD1990 lpop urban openk G cl internet zpolar zboreal zdrytemp zwettemp zsubtrop ztropics"

*This is the specification for the RR at PlosOne
global XXX = "lgdppc90 pop65over_1990 OECD1990 lpop urban openk G cl internet zpolar zboreal zdrytemp zwettemp zsubtrop ztropics percsecedu trust m_trust" 

*This is the specification when we regress by areas
global XX = "lgdppc90 pop65over_1990 OECD1990 lpop urban openk G cl internet percsecedu trust m_trust" 

* m_percsecedu has only 1 missing in the sample we use - is a singleton. I've removed if from the set of controls

global XXXX = "lgdppc90 OECD1990 lpop urban openk G cl internet zpolar zboreal zdrytemp zwettemp zsubtrop ztropics syr1565l"

global instrument = "malfal66"  /* specify ony one instrument */
global exog "zpolar zboreal zdrytemp zwettemp zsubtrop ztropics"

******************************SAMPLE SELECTION**********************************
qui: ivreg2 growth (allmort  = $instrument)  $XXX if year==1990, robust 
keep if e(sample)==1

******************************DESCRIPTIVE STAT**********************************				*** RR ***

su growth allmort $XXX

* Juts the not imputed values 

su trust if  m_trust==0

******************************REGRESSIONS***************************************
preserve
gen sample=e(sample)==1
keep countrycode sample
*save "$data\cnttouse.dta", replace
save "$data/cnttouse.dta", replace
restore

sum allmort,det
local meanX = r(mean)

display "`meanX'"


sum growth,det
local meang = r(p50)
sum allmort,det
local meanD = r(p50)
sum gdppc,det
local meangdp90 = r(p50)

**OLS table 3, 1st column**
reg growth allmort $XXX, robust
display _b[allmort]*`meanX'/100


local elasticity = abs((_b[allmort]*`meanD'/`meang')/100)
local yearly = (1+`meang'/100)^(1/24)-1
local yeffect = (1+`meang'*(1+`elasticity')/100)^(1/24)-1  -  `yearly'
*outreg2 using "$output\estimates $measure $age.xls", replace excel bdec(3) ctitle(OLS) addstat(semielasticity, `elasticity'*`meang', yearly growth, `yearly', yerly effect, `yeffect')

display _b[allmort]*`meanX'/100													// semielasticity		*** RR ***


********** REVISION: clustered errors (+ wild bootstrapping) & Region RE ********                       *** RR ***
reg growth allmort $XXX, robust
reg growth allmort $XXX, cluster(reg)											/* standard errors clustered at the region level */
boottest allmort, weight(webb)													


reg growth allmort $XXX, cluster(reg)
local elasticity = abs((_b[allmort]*`meanD'/`meang')/100)
local yearly = (1+`meang'/100)^(1/24)-1
local yeffect = (1+`meang'*(1+`elasticity')/100)^(1/24)-1  -  `yearly'

areg growth allmort $XXX, a(reg)												/* region fixed effect */

display _b[allmort]*`meanX'/100													// semielasticity


xtset reg
xtreg growth allmort $XXX, re													/* random effects */
display _b[allmort]*`meanX'/100	

jackknife: reg growth allmort $XXX


reg growth allmort $XXX
est store OLS
outreg2 using "$output/OLS $measure $age.doc", replace bdec(3) ctitle(OLS) 

tab country allmort if country=="Niger" | country=="United States" | country=="India" | country=="China"

*Macro with mortality in the selected coutries (average between 1990 and 2014)                        
global NigerALLMORT = 1509.938
global USAALLMORT = 836.7454
global IndiaALLMORT = 911.9863
global ChinaALLMORT = 713.7565 

**IV table 3, 2nd column**

**IV table 3, 2nd column**
ivreg2 growth (allmort  = $instrument)  $XXX if year==1990, robust first
estimates store lorentzen
matrix lorentzen = e(b)
margins, eyex(allmort) atmeans
matrix A = r(b)
local elasticity = abs((_b[allmort]*`meanD'/`meang')/100)
local yearly = (1+`meang'/100)^(1/24)-1
local yeffect = (1+`meang'*(1+`elasticity')/100)^(1/24)-1  -  `yearly'
*outreg2 using "$output\estimates $measure $age.xls", append excel bdec(3) ctitle(IV) addstat(F, e(widstat), semielasticity, `elasticity'*`meang', yearly growth, `yearly', yerly effect, `yeffect'
outreg2 lorentzen using "$output/OLS $measure $age.doc",  bdec(3) ctitle(IV)  append     
						

display _b[allmort]*`meanX'/100													// semielasticity		*** RR ***											


**Regressions with interactions (WB classification)**
gen lml = (low==1 | low_middle==1)
replace lml = . if (low==. | low_middle==.)

gen inter_LML = allmort*lml

**IV table 3, 3rd column**

foreach var of varlist $instrument {
gen `var'_LML = `var'*lml
}

*low and middle low
ivreg2 growth (allmort inter_LML = malfal66 malfal66_LML) $XXX, robust first
estimate store lorentzen_I
lincom allmort+inter_LML

ivreg2 growth (allmort inter_LML = malfal66 malfal66_LML) $XXX, robust first liml
estimate store lorentzen_I
lincom allmort+inter_LML

weakiv ivreg2 growth (allmort inter_LML = malfal66 malfal66_LML) $XXX, 			///
robust first graph(all) level(90)

*ols for comparability


reg growth allmort inter_LML $XXX, robust 
estimate store OLSlorentzen_I
lincom allmort+inter_LML


********** REVISION: another way to compute Heterogeneous effects ********                       *** RR ***


gen inter_lgdppc90 = allmort*lgdppc90		
gen malfal66_lgdppc90 = malfal66*lgdppc90

ivreg2 growth (allmort inter_lgdppc90 = malfal66 malfal66_lgdppc90) $XXX, robust first /// 


xtile lgdppc90ter = lgdppc90, nq(3) 
qui tab lgdppc90ter, gen (duincometer)

gen inter_1 = allmort*duincometer1		
gen malfal66_1 = malfal66*duincometer1

gen inter_2 = allmort*duincometer2		
gen malfal66_2 = malfal66*duincometer2

ivreg2 growth (allmort inter_1 inter_2 = malfal66 malfal66_1 malfal66_2) $XXX, robust first ///


** without instrumenting **

reg growth c.allmort  $XXX, robust
scalar define ols=_b[c.allmort]
local ols "ols"
display `ols' 

* linear effect
reg growth c.allmort##c.lgdppc90 $XXX, robust
margins, dydx(c.allmort) at(c.lgdppc90=(5.89797(0.2)11.70277)) 
marginsplot, yline(0, lp(solid)) yline(-.0876914, lp(dash)) xline(8.84, lp(dot)) ytitle(log income in 1990)

reg growth c.allmort##c.lgdppc90 $XXX, robust
margins, dydx(c.allmort) at(c.lgdppc90=(5.89791, 8.84, 11.70277))
marginsplot

* quadratic specification
reg growth c.allmort##c.lgdppc90##c.lgdppc90 $XXX, robust
margins, dydx(c.allmort) at(c.lgdppc90=(5.89797(0.2)11.70277)) 
marginsplot, yline(0, lp(solid)) yline(-.0876914, lp(dash)) xline(8.84, lp(dot)) ytitle(log income in 1990)


* REVISION - interaction with the proportion of young people aged 014 in 1990	                 *** RR ***
* Interaction with pop014_1990

sum pop014_1990, det
gen pop014 = pop014_1990>r(p50) if pop014_1990!=.

gen allmort_pop014 = allmort * pop014
gen malfal66_pop014 = malfal66 * pop014

ivreg2 growth (allmort allmort_pop014 = malfal66 malfal66_pop014) $XXX, robust first 

reg growth c.allmort##c.pop014_1990 $XXX, robust
margins, dydx(c.allmort) at(c.pop014_1990=(15 (5) 50))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) xtitle(percentage of young people aged 0-14 in 1990)

reg growth c.allmort##c.pop014_1990##c.pop014_1990 $XXX, robust
margins, dydx(c.allmort) at(c.pop014_1990=(15 (5) 50))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) xtitle(percentage of young people aged 0-14 in 1990)

reg growth c.allmort##c.pop014_1990##c.pop014_1990##c.pop014_1990 $XXX, robust
margins, dydx(c.allmort) at(c.pop014_1990=(15 (5) 50))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) xtitle(percentage of young people aged 0-14 in 1990)

*with old

su c.pop65over_1990, d

*LInear
reg growth c.allmort##c.pop65over_1990 $XXX, robust
margins, dydx(c.allmort) at(c.pop65over_1990=(2 (1) 16))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) 					///
xtitle(percentage of people over 65 in 1990) level(90)							///
title(Average treatment effect of Mortality) note("90% Confidence Intervals")

*Quadratic
reg growth c.allmort##c.pop65over_1990##c.pop65over_1990 $XXX, robust
margins, dydx(c.allmort) at(c.pop65over_1990=(2 (1) 16))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) 					///
xtitle(percentage of people over 65 in 1990) level(90)							///
title(Average treatment effect of Mortality) note("90% Confidence Intervals")

global Xold = "lgdppc90 OECD1990 lpop urban openk G cl internet zpolar zboreal zdrytemp zwettemp zsubtrop ztropics percsecedu trust m_trust" 

**with quartiles 

xtile old4=pop65over_1990, n(4)

*with dummies
reg growth c.allmort##i.old4 $Xold, robust
margins, dydx(c.allmort) at(old4=(1 (1) 4))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) 					///
xtitle(percentage of people over 65 in 1990) level(90)							///
title(Average treatment effect of Mortality) note("90% Confidence Intervals")


gen oldm=pop65over_1990<5
replace oldm = 2 if pop65over_1990 >=5 & pop65over_1990 <10
replace oldm = 3 if pop65over_1990 >=10

*with dummies
reg growth c.allmort##i.oldm $Xold, robust
margins, dydx(c.allmort) at(oldm=(1 (1) 3))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) 					///
xtitle(percentage of people over 65 in 1990) level(90)							///
title(Average treatment effect of Mortality) note("90% Confidence Intervals")

display _b[c.allmort]*`meanX'/100

forvalues a = 2/3 {											
display (_b[c.allmort]+_b[c.allmort#`a'.oldm])*`meanX'/100
}

*896.13

**Regressions with interactions (WB classification, all dummies)**				// 							*** RR ***	

reg growth c.allmort##i.upper_middle c.allmort##i.low_middle c.allmort##i.low  $XXX, robust 

* just to double-check that they are the same

lincom allmort
lincom allmort+1.upper_middle#allmort
lincom allmort+1.low_middle#allmort
lincom allmort+1.low#allmort


reg growth c.allmort##i.class  $XXX, robust
estimates store OLS_WB
margins, dydx(c.allmort) at(class=(1 (1) 4))
marginsplot,  yline(0, lp(solid)) yline(-.0876914, lp(dash)) level(90)					///
xtitle(WB Classification)														///
title(Average treatment effect of Mortality) note("90% Confidence Intervals")

forvalues a = 1/4 {											
display (_b[c.allmort]+_b[c.allmort#`a'.class])*`meanX'/100
}


twoway (scatter lgdppc90 pop014_1990) (lfit lgdppc90 pop014_1990), xtitle(percentage of young people aged 0-14 in 1990) ytitle(log income in 1990)
scatter lgdppc90 pop014_1990, xtitle(percentage of young people aged 0-14 in 1990) ytitle(log income in 1990)

corr class pop65over_1990

*check
lincom c.allmort+c.allmort#1.class

********************************************************************************
*	Tackling unobserved heterogeneity without using (excluded) instruments     *
********************************************************************************

***********Oster (2019) Journal of Business & Economic Statistics***************

**Specification on the paper**
reg growth allmort $XXX, robust
estimates store OLS

*coefficient if delta=1 rmax(1)
psacalc beta allmort, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///
ztropics)  

*coefficient if delta=1 rmax(0.4978*1.3)
psacalc beta allmort, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///  // This is in the paper
ztropics) rmax(.64714) 
scalar beta1 = r(beta)
estimates restore OLS
estadd scalar beta1
*coefficient if delta=1 rmax(0.4455*1.3),  30% increase
psacalc beta allmort, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		/// // This is in the paper
ztropics) rmax(0.7467) 
scalar beta2 = r(beta)
scalar rmax = r(rmax)
estimates restore OLS
estadd scalar beta2
estadd scalar rmax
*I dont know why it doesn't work
esttab OLS , stats(beta1 beta2 r2 rmax N) replace


/* Other estimates

*coefficient if delta=1 rmax(1) 
psacalc beta allmort, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///
ztropics) rmax(1)

*coefficient if delta=1 rmax(0.4455*1.5), delta(0.5) 50% increase
psacalc beta allmort, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///
ztropics) rmax(1) delta(0.5)

**Controlling for malfal66 (assuming there is a massive omitted variable bias)
reg growth allmort malfal66 $XXX, robust

 
*coefficient if delta=1 rmax(0.450*1.3), delta(0.5)
psacalc beta allmort, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///
ztropics) rmax(.585) delta(0.5)
*/

********************************************************************************
*								EXCLUDABILITY 	         		   		       *
********************************************************************************

**********Lewbel (2012) Journal of Business & Economic Statistics***************

gen trop=zsubtrop+ztropics

*This is the specification on the paper
global XXXXX = "trop zpolar zboreal zdrytemp zwettemp zsubtrop lgdppc90 pop65over_1990 OECD1990 lpop urban openk G cl internet percsecedu trust m_trust "                 
ivreg2h growth (allmort  = $instrument)  $XXXXX if year==1990, robust first rf 	///
z(trop) orthog(malfal66) 

ivreg2h growth (allmort  = $instrument)  $XXXXX if year==1990, robust first rf 	///				  *** Paper before RR ***
z(trop)
*Semielasticity with the generated and excluded instruments
display _b[allmort]*`meanX'/100	

*I use the zones to construct the instruments, assuming they are exogenous                        *** RR (all time zones)***
ivreg2h growth (allmort  = $instrument)  $XXX if year==1990, robust first rf 	///
z(zpolar zboreal zdrytemp zwettemp zsubtrop ztropics) 
*Semielasticity with the generated and excluded instruments
display _b[allmort]*`meanX'/100	

*I use all the controls, assuming they are exogenous                         					 *** RR (all the variables)***
ivreg2h growth (allmort  = $instrument)  $XXX if year==1990, robust first rf 	
*Semielasticity with the generated and excluded instruments
display _b[allmort]*`meanX'/100	

global XXXX = "zpolar zboreal zdrytemp zwettemp zsubtrop ztropics lgdppc90 OECD1990 lpop urban openk G cl internet "

ivreg2h growth (allmort  = $instrument)  $XXXX if year==1990, robust first rf 	///
z(zpolar zboreal zdrytemp zwettemp zsubtrop ztropics) 


*Is malfal66 excludable? (we cannot reject the null of excludability)
ivreg2h growth (allmort  = $instrument)  $XXX if year==1990, robust first 		///
orthog(malfal66) z(zpolar zboreal zdrytemp zwettemp zsubtrop ztropics)


ivreg2h growth (allmort  = $instrument)  $XXX trop if year==1990, robust first rf 	///
z(trop) 

ivreg2h growth (allmort  = $instrument)  $XXX if year==1990, robust first rf 	///
z(zsubtrop)

ivreg2h growth (allmort  = $instrument)  $XXX if year==1990, robust first rf 	///
z(zsubtrop ztropics)

ivreg2h growth (allmort  = )  $XXX if year==1990, robust first rf 	///
z(trop)


ivreg2 growth (allmort  = $instrument)  $XXX if year==1990, robust first gmm2s 



**********Conley et al. (2012) The Review of Economics and Statistics***********

**Take bounds without assuming a distribution (90% c.i.)**

*UCI*
plausexog uci growth $XXX (allmort  = $instrument), gmin(-40) gmax(40) 			///
grid(200) level(.90) vce(robust) graph(allmort) yline(0) legend(off) 			///
xtitle({&gamma}) ytitle({&pi}{sub:2})				
graph save "$graph/uci", replace
graph export "$graph/uci.pdf", replace

**Assume a Gamma distribution**
*LTZ, Gamma, Variance=delta^2*
plausexog ltz growth $XXX (allmort  = $instrument), omega(900) mu(0) level(.90) ///
vce(robust) graph(allmort) graphmu(-30 -10 0 10 30) 							///
graphomega(900 100 0 100 900) graphdelta(-30 -10 0 10 30) yline(0) legend(off) 	///
xtitle({&gamma}) ytitle({&pi}{sub:2})	
graph save "$graph/ltz_30", replace
graph export "$graph/ltz_30.pdf", replace 

*LTZ, Gamma, Variance=delta^2*
plausexog ltz growth $XXX (allmort  = $instrument), omega(900) mu(0) level(.90) ///
vce(robust) graph(allmort) graphmu(-40 -30 -10 0 10 30 40) 						///
graphomega(1600 900 100 0 100 900 1600) graphdelta(-40 -30 -10 0 10 30 40) 		///
yline(0) legend(off) xtitle({&gamma}) ytitle({&pi}{sub:2})	
graph save "$graph/ltz_40", replace
graph export "$graph/ltz_40.pdf", replace 

*LTZ, Gamma, Variance=delta*
plausexog ltz growth $XXX (allmort  = $instrument), omega(900) mu(0) level(.90) ///
vce(robust) graph(allmort) graphmu(-40 -30 -10 0 10 30 40) 						///
graphomega(40 30 10 0 10 30 40) graphdelta(-40 -30 -10 0 10 30 40)  yline(0)	///
legend(off) xtitle({&gamma}) ytitle({&pi}{sub:2})	
graph save "$graph/ltz_c40", replace
graph export "$graph/ltz_c40.pdf", replace 

graph combine "$graph/uci" "$graph/ltz_40" "$graph/ltz_c40", col(1) xsize(8) 	///
ysize(12) title(Mortality)
graph save "$graph/conley", replace
graph export "$graph/conley.pdf", replace 


*Notice: we can assume also a Normal distribution but we need Stata 15
		
*********Nevo and Rosen (2012) The Review of Economics and Statistics***********	

* NOTICE: the sign of the endogeneity and the sign of the direct effect of the 		
* instrument need to be the same. Here we have positive endogeneity and negative	
* direct effect. The sign of the first stage is positive. 
* We construct a new instrument = 1-malfal66 so:
* 1) both the direct effect and the endogeneity are positive
* 2) the sign of the first stage is now negative
* 3) the IV estimate is not affectes
* 1,2,3) imply beta is bounded (w/o assumption 4)  by beta_IV (lower) and 		
* beta_OLS (upper) and has tighter bounds with assumption four

**If instead the direct effect is positive, we have just an upper bound.

gen malfal66n=1-malfal66
lab var malfal66n "1-malfal66"

*Assuming the direct effect is negative
imperfectiv growth $XXX (allmort  = malfal66n)
imperfectiv growth $XXX (allmort  = malfal66n), noassumption4 

*Assuming the direct effect is positive
imperfectiv growth $XXX (allmort  = malfal66)
imperfectiv growth $XXX (allmort  = malfal66), noassumption4 

*I try to create two sided bounds with the other instrument but it looks weird
imperfectiv growth $XXX (allmort  = $instrument distcoast), prop5 


reg growth allmort $XXX
reg growth allmort malfal66 $XXX

imperfectiv growth $XXX (allmort  = $instrument distcoast), prop5 

**I apply Oster to the reduced form**

*I run the reduced form
reg  growth malfal66 $XXX, robust
estimates store reduced
 

*coefficient if delta=1 rmax(0.421*1.3)
psacalc beta malfal66, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///     // paper
ztropics) rmax(0.57135) 

reg  allmort malfal66 $XXX 

*coefficient if delta=1 rmax(0.598*1.3)
psacalc beta malfal66, mcontrol(zpolar zboreal zdrytemp zwettemp zsubtrop 		///     // paper
ztropics) rmax(.90389) 

***************
* Predictions *
***************


use "$data/dataset_rr.dta", clear												
keep if year == 2014

gen lml = (low==1 | low_middle==1)
replace lml = . if (low==. | low_middle==.)
gen inter_LML = allmort*lml

estimates restore OLS_WB														/* RR */
estimates


predict growthhat, xb
browse country growth growthhat

label var country "country"
su growth

keep if country=="China" | country=="India" | country=="Niger" | country=="United States"
tab growth

*Variation in the average mortality (status quo)
*Niger
global sqNigerMort= ($MNigerMort - $NigerALLMORT)/$NigerALLMORT*100
*Usa
global sqUSAMort=($MUSAMort - $USAALLMORT)/$USAALLMORT*100
*India
global sqIndiaMort=($MIndiaMort - $IndiaALLMORT)/$IndiaALLMORT*100
*China
global sqChinaMort=($MChinaMort - $ChinaALLMORT)/$ChinaALLMORT*100

display  $sqNigerMort " " $MNigerMort " " $NigerALLMORT


*Variation in the average mortality (0.01)
*Niger
global ch01NigerMort= ($M01NigerMort - $NigerALLMORT)/$NigerALLMORT*100
*Usa
global ch01USAMort=($M01USAMort - $USAALLMORT)/$USAALLMORT*100
*India
global ch01IndiaMort=($M01IndiaMort - $IndiaALLMORT)/$IndiaALLMORT*100
*China
global ch01ChinaMort=($M01ChinaMort - $ChinaALLMORT)/$ChinaALLMORT*100

*Variation in the average mortality (0.05)
*Niger
global ch05NigerMort= ($M05NigerMort - $NigerALLMORT)/$NigerALLMORT*100
*Usa
global ch05USAMort=($M05USAMort - $USAALLMORT)/$USAALLMORT*100
*India
global ch05IndiaMort=($M05IndiaMort - $IndiaALLMORT)/$IndiaALLMORT*100
*China
global ch05ChinaMort=($M05ChinaMort - $ChinaALLMORT)/$ChinaALLMORT*100


*Variation in the average mortality (bad scenario 0.01)											***RR***

*Niger
global chI01NigerMort= ($MI01NigerMort - $NigerALLMORT)/$NigerALLMORT*100
*Usa
global chI01USAMort=($MI01USAMort - $USAALLMORT)/$USAALLMORT*100
*India
global chI01IndiaMort=($MI01IndiaMort - $IndiaALLMORT)/$IndiaALLMORT*100
*China
global chI01ChinaMort=($MI01ChinaMort - $ChinaALLMORT)/$ChinaALLMORT*100


*Variation in the average mortality (bad scenario 0.05)											***RR***
*Niger
global chI05NigerMort= ($MI05NigerMort - $NigerALLMORT)/$NigerALLMORT*100
*Usa
global chI05USAMort=($MI05USAMort - $USAALLMORT)/$USAALLMORT*100
*India
global chI05IndiaMort=($MI05IndiaMort - $IndiaALLMORT)/$IndiaALLMORT*100
*China
global chI05ChinaMort=($MI05ChinaMort - $ChinaALLMORT)/$ChinaALLMORT*100

display $sqNigerMort " " $sqUSAMort " " $sqIndiaMort " " $sqChinaMort
display $ch01NigerMort " " $ch01USAMort " " $ch01IndiaMort " " $ch01ChinaMort
display $ch05NigerMort " " $ch05USAMort " " $ch05IndiaMort " " $ch05ChinaMort
display $chI01NigerMort " " $chI01USAMort " " $chI01IndiaMort " " $chI01ChinaMort
display $chI05NigerMort " " $chI05USAMort " " $chI05IndiaMort " " $chI05ChinaMort

*compare the average mortality between 2014 and 2038 to the average mortality between 1990 and 2004

display $sqNigerMort " " $ch05NigerMort " " $ch01NigerMort " " $MNigerMort " " $NigerALLMORT

***I compute two semielasticities because I have two different betas (for high and low income countries)***
*Elasticity/semielasticity for low income
gen elasticity_1 = _b[allmort]*`meanX'/growth															
gen semielasticity_1 = _b[allmort]*`meanX'/100



*Elasticity/semielasticity for middle low	

forvalues a = 2/4 {											/* change */
gen elasticity_`a' = (_b[allmort]+_b[c.allmort#`a'.class])*`meanX'/growth
gen semielasticity_`a' = (_b[allmort]+_b[c.allmort#`a'.class])*`meanX'/100
}



*This is the yearly growth if all the parameters stay the same
gen yearly = (1+growthhat/100)^(1/24)-1
  
drop year
expand 26
bysort country : gen year = 2013+_n

**I compute two yearly growth (status quo and change) adding to growthhat the 
* change in GDP due to a change in mortality in the status quo case (i.e. change
* equal to the change that took place between 1990 and 2004 and in a more optimistic
* case (1% more). It is country specific because it depends on the beta and on the
* change in mortality rate.

*High and high middle income countries
*USA
gen yearly_sqUSA = (1+(growthhat+semielasticity_4*(${sqUSAMort}))/100)^(1/24)-1  
gen yearly_ch01USA = (1+(growthhat+semielasticity_4*(${ch01USAMort}))/100)^(1/24)-1 
gen yearly_ch05USA = (1+(growthhat+semielasticity_4*(${ch05USAMort}))/100)^(1/24)-1 
gen yearly_chI01USA = (1+(growthhat+semielasticity_4*(${chI01USAMort}))/100)^(1/24)-1       /** RR **/
gen yearly_chI05USA = (1+(growthhat+semielasticity_4*(${chI05USAMort}))/100)^(1/24)-1       /** RR **/

*China
gen yearly_sqChina = (1+(growthhat+semielasticity_3*(${sqChinaMort}))/100)^(1/24)-1  
gen yearly_ch01China = (1+(growthhat+semielasticity_3*(${ch01ChinaMort}))/100)^(1/24)-1 
gen yearly_ch05China = (1+(growthhat+semielasticity_3*(${ch05ChinaMort}))/100)^(1/24)-1 
gen yearly_chI01China = (1+(growthhat+semielasticity_3*(${chI01ChinaMort}))/100)^(1/24)-1   /** RR **/
gen yearly_chI05China = (1+(growthhat+semielasticity_3*(${chI05ChinaMort}))/100)^(1/24)-1   /** RR **/

*Low and low middle income countries
*India
gen yearly_sqIndia = (1+(growthhat+semielasticity_2*(${sqIndiaMort}))/100)^(1/24)-1  
gen yearly_ch01India = (1+(growthhat+semielasticity_2*(${ch01IndiaMort}))/100)^(1/24)-1 
gen yearly_ch05India = (1+(growthhat+semielasticity_2*(${ch05IndiaMort}))/100)^(1/24)-1 
gen yearly_chI01India = (1+(growthhat+semielasticity_2*(${chI01IndiaMort}))/100)^(1/24)-1   /** RR **/
gen yearly_chI05India = (1+(growthhat+semielasticity_2*(${chI05IndiaMort}))/100)^(1/24)-1   /** RR **/

*Niger
gen yearly_sqNiger = (1+(growthhat+semielasticity_1*(${sqNigerMort}))/100)^(1/24)-1  
gen yearly_ch01Niger = (1+(growthhat+semielasticity_1*(${ch01NigerMort}))/100)^(1/24)-1 
gen yearly_ch05Niger = (1+(growthhat+semielasticity_1*(${ch05NigerMort}))/100)^(1/24)-1 
gen yearly_chI01Niger = (1+(growthhat+semielasticity_1*(${chI01NigerMort}))/100)^(1/24)-1   /** RR **/
gen yearly_chI05Niger = (1+(growthhat+semielasticity_1*(${chI05NigerMort}))/100)^(1/24)-1   /** RR **/

sort country year

by country: gen gdppchat = gdppc*(1+yearly)^(_n-1)

foreach c in USA India China Niger {
by country: gen gdppchat_sq`c' = gdppc*(1+yearly_sq`c')^(_n-1)
by country: gen gdppchat_ch01`c' = gdppc*(1+yearly_ch01`c')^(_n-1)
by country: gen gdppchat_ch05`c' = gdppc*(1+yearly_ch05`c')^(_n-1)
by country: gen gdppchat_chI01`c' = gdppc*(1+yearly_chI01`c')^(_n-1)							/** RR **/
by country: gen gdppchat_chI05`c' = gdppc*(1+yearly_chI05`c')^(_n-1)							/** RR **/
}


keep country year gdppc growthhat gdppchat elasticity* semielasticity* yearly* gdppchat*  

*High and high middle income countries
foreach c in USA India China Niger {
gen delta01`c' = gdppchat_ch01`c'-gdppchat_sq`c'
gen delta05`c' = gdppchat_ch05`c'-gdppchat_sq`c'
gen deltaI01`c' = gdppchat_chI01`c'-gdppchat_sq`c'											/** RR **/
gen deltaI05`c' = gdppchat_chI05`c'-gdppchat_sq`c'											/** RR **/
}

gen delta01=delta01USA
gen delta05=delta05USA
gen deltaI01=deltaI01USA 																	/** RR **/
gen deltaI05=deltaI05USA 																	/** RR **/

foreach c in India China Niger {
replace delta01=delta01`c' if country=="`c'"
replace delta05=delta05`c' if country=="`c'"
replace deltaI01=deltaI01`c' if country=="`c'"												/** RR **/
replace deltaI05=deltaI05`c' if country=="`c'"												/** RR **/
}

lab var delta01 "1 p.p. better than the status quo"
lab var delta05 "5 p.p. better than the status quo"
lab var deltaI01  "1 p.p. worse than the status quo"										/** RR **/
lab var deltaI05  "5 p.p. worse than the status quo"										/** RR **/

/*
twoway (line delta01 year, sort lwidth(thick) xlabel(2014(5)2038,angle(45)) ylabel(,angle(0)) by(country)) ///
(line delta05 year, sort lwidth(thick) xlabel(2014(5)2038,angle(45)) ylabel(,angle(0)) by(country)) ///
(line deltaI01 year, sort lwidth(thick) xlabel(2014(5)2038,angle(45)) ylabel(,angle(0)) by(country))
graph save "$graph/scenarios", replace
graph export "$graph/scenarios.pdf", replace 
*/

twoway 																								 ///
(line deltaI01 year, sort lwidth(thick) xlabel(2014(5)2038,angle(45)) ylabel(,angle(0))) ///
(line delta01 year, sort lwidth(thick) xlabel(2014(5)2038,angle(45)) ylabel(,angle(0))), by(country, legend(off))  ///
ytitle("Effect of a 1 p.p. change in mortality")
graph save "$graph/scenarios_rr", replace
graph export "$graph/scenarios_rr.pdf", replace 

line delta01 year, sort lwidth(thick) xlabel(2014(5)2038,angle(45)) ylabel(,angle(0)) by(country) 
graph save "$graph/scenarios_small", replace
graph export "$graph/scenarios_small.pdf", replace

gen year5 = 2014 if year==2014
replace year5 = 2019 if year==2019
replace year5 = 2024 if year==2024
replace year5 = 2029 if year==2029
replace year5 = 2034 if year==2034
replace year5 = 2038 if year==2038

table year5 country, c(m gdppchat m delta01 m delta05) format(%5.0f)

*I apply a discount rate to compute the total gain in 25 years and to present it as a percentage of the 2014 GDP
sort country year
by country : gen discount = (1+0.02)^-(_n-1)
gen disc_delta01 = delta01* discount
gen disc_delta05 = delta05* discount
gen disc_deltaI01 = deltaI01* discount												/*** RR ***/
gen disc_deltaI05 = deltaI05* discount												/*** RR ***/

by country : egen totalgain01 = total(disc_delta01)
by country : egen totalgain05 = total(disc_delta05)
by country : egen totalgainI01 = total(disc_deltaI01)								/*** RR ***/
by country : egen totalgainI05 = total(disc_deltaI05)								/*** RR ***/

by country : gen gainratio01 = totalgain01/gdppc*100 if year==2014
by country : gen gainratio05 = totalgain05/gdppc*100 if year==2014
by country : gen gainratioI01 = totalgainI01/gdppc*100 if year==2014				/*** RR ***/
by country : gen gainratioI05 = totalgainI05/gdppc*100 if year==2014				/*** RR ***/

*These are the tables in the paper (with just the right delta)
table year5 country if year5==2014, c(m totalgainI01 m totalgain01) format(%3.1f)
table year5 country if year5==2014, c(m gainratioI01 m gainratio01) format(%3.1f)

table year5 country if year5==2014, c(m totalgainI05 m totalgainI01 m totalgain01 m totalgain05) format(%5.0f)
table year5 country if year5==2014, c(m gainratioI05 m gainratioI01 m gainratio01 m gainratio05) format(%3.1f)   			/*** RR ***/




