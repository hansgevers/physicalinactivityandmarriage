/*
Stata code supporting the paper "The Influence of Health, Marriage and Cohabitation on Physical Inactivity: Evidence from SHARE 2015 to 2022 for 12 European Countries"
Author: Hans Gevers - Junior Research Fellow at the Estonian Business School
https://orcid.org/0009-0001-0249-4142 hans.gevers@ebs.ee
*/

clear all
log using output.smcl, replace name("InactivityAndMarriage")

use "C:\Users\hansg\Documents\working\SHARE data 5\dataset.dta"

asdoc, text(\par \qc The Influence of Health, Marriage and Cohabitation on Physical Inactivity: Evidence from SHARE 2015 to 2022 for 12 European Countries) fs(12)  save(report.doc) replace
asdoc, text(\par \qc Hans Gevers - Junior Research Fellow at the Estonian Business School https://orcid.org/0009-0001-0249-4142 hans.gevers@ebs.ee) fs(10)  save(report.doc) append

***variables preparation and data description

asdoc, text(\page) save(report.doc) append

*panel level id
egen id= group(mergeid)
drop *_f
drop if phinact==-99

xtset id Qyear

asdoc xttrans phinact, freq save(report.doc) append

tabulate country phinact

generate mst=0
replace mst=1 if mstat==3 | mstat==5
replace mst=2 if mstat==4
replace mst=3 if mstat==6
label define msL 0 "Officially with partner" 1 "Away from partner" 2 "Never married" 3 "Lost partner"
label values mst msL

spearman mst single

generate change=0
replace change=1 if mst==0
bysort id: generate changed = change - change[_n-1]
replace changed=changed+1
replace changed=1 if changed==.
label define chL 0 "No longer officially together" 1 "All other situations" 2 "Newly officially together"
label values changed chL
asdoc tabulate changed, save(report.doc) append

bysort id: generate schanged=single - single[_n-1]
replace schanged=0 if schange==.
replace schanged=schanged+1
label define schL 0 "Hooked up" 1 "Unchanged" 2 "Back single"
label values schanged schL
asdoc tabulate schanged, save(report.doc) append

generate mortg=0
replace mortg=1 if mort>0
label define mlab 0 "No mortgage" 1 "Mortgage"
label values mortg mlab

codebook phinact sphus thinc mst changed schanged cjs age yedu gender country bmi eurod, compact

drop if cjs==-99
drop if yedu==9997

winsor2 thinc, replace cuts(0 99)

codebook phinact sphus thinc mst changed schanged cjs age yedu gender country bmi eurod, compact

global names thinc
foreach num of numlist 1(1)1 {
	local name: word `num' of $names
	bysort country: egen mean`name'=mean(`name') if `name'!=0
	generate T=1
	replace T=0 if `name'==0
	replace T=2 if `name'>mean`name'
	drop mean`name'
	drop `name'
	rename T `name'
}

label define thL 0 "No income" 1 "Below country average income" 2 "Above country average income"
label values thinc thL

keep gali sphus thinc mst mstat changed schanged cjs age yedu gender country single id Qyear my_wgt phinact bmi eurod

asdoc spearman phinact gali sphus thinc single mst changed schanged cjs age yedu gender country bmi eurod, save(report.doc) append

label copy cjs cjsLab
label list cjsLab
replace cjs=6 if cjs==97
label define cjsLab 6 "Other", add
label values cjs cjsLab
tabulate cjs, nolab

bysort id: egen Mundlak_OthC_age=mean(age)
bysort id: egen Mundlak_OthC_yedu=mean(yedu)
bysort id: egen Mundlak_OthC_bmi=mean(bmi)
bysort id: egen Mundlak_OthC_eurod=mean(eurod)



*program for calculating Mundlak_Means
program define Mundlak_Means
	*syntax varname
	global variab changed schanged mst single sphus gali cjs thinc 
	foreach V of numlist 1(1)8 {
		local va: word `V' of $variab
		tab `va', gen(_dum_)
		levelsof `va', local(levels)
		local i = 1
		foreach l of local levels {
			local lbl : label (`va') `l'
			local clean = ustrword("`lbl'",1)
			rename _dum_`i' `va'_`clean'
			local ++i
		}
		foreach v of varlist `va'_* {
			if `V'==1 {
				bysort id: egen Mundlak_C_`v' = mean(`v')				
			}
			if `V'==2 {
				bysort id: egen Mundlak_sC_`v' = mean(`v')				
			}
			if `V'==3 {
				bysort id: egen Mundlak_Mar_`v' = mean(`v')				
			}
			if `V'==4 {
				bysort id: egen Mundlak_Sin_`v' = mean(`v')				
			}
			if `V'==5 {
				bysort id: egen Mundlak_Sph_`v' = mean(`v')				
			}
			if `V'==6 {
				bysort id: egen Mundlak_Gal_`v' = mean(`v')				
			}
			if `V'==7 | `V'==8 {
				bysort id: egen Mundlak_Oth_`v' = mean(`v')
			}
		}
	}
end

Mundlak_Means

generate iG1=1.changed_No#eurod
generate iG2=1.changed_All#eurod
generate iG3=1.changed_Newly#eurod

generate isG1=1.schanged_Hooked#eurod
generate isG2=1.schanged_Unchanged#eurod
generate isG3=1.schanged_Back#eurod

foreach n of numlist 1(1)3 {
	bysort id: egen Mundlak_Gi_`n'=mean(iG`n')
}

foreach n of numlist 1(1)3 {
	bysort id: egen Mundlak_sGi_`n'=mean(isG`n')
}


***descriptives

asdoc codebook phinact sphus thinc mst changed schanged cjs age yedu gender country bmi eurod, compact save(report.doc) append

graph bar (count), over(mst, label(angle(90) labsize(small))) over(Qyear, label(angle(0) labsize(small))) scheme(s2color) ytitle("Number of respondents", margin(small)) ylabel(0(2500)40000,angle(0) labsize(small) grid) 
graph export descrip1.png, replace height(2400)
graph bar (count), over(single, label(angle(90) labsize(small))) over(Qyear, label(angle(0) labsize(small))) scheme(s2color) ytitle("Number of respondents", margin(small)) ylabel(0(2500)42500,angle(0) labsize(small) grid)
graph export descrip2.png, replace height(2400)
graph bar (count), over(phinact, label(angle(90) labsize(small))) over(Qyear, label(angle(0) labsize(small))) scheme(s2color) ytitle("Number of respondents", margin(small)) ylabel(0(2500)52500,angle(0) labsize(small) grid)
graph export descrip3.png, replace height(2400)

asdoc tabulate phinact, save(report.doc) append

***regressions

xtset id Qyear, yearly

xtlogit phinact age yedu i.gender i.thinc i.mst ib1.changed i.gali i.cjs c.bmi c.eurod i.country
estimates store RE
xtlogit phinact age yedu i.gender i.thinc i.mst ib1.changed i.gali i.cjs c.bmi c.eurod i.country, fe
estimates store FE
asdoc hausman FE RE, alleqs save(report.doc) append

xtlogit phinact age yedu i.gender i.thinc i.mst i.gali i.cjs c.bmi ib1.changed##c.eurod i.country, fe or baselevels
outreg2 using results.xls, excel replace dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLimFE) stnum(replace coef=exp(coef), replace se=coef*se)

xtlogit phinact age yedu i.gender i.thinc i.mst i.gali i.cjs c.bmi ib1.changed##c.eurod i.country ///
Mundlak_Gal_* Mundlak_C_* Mundlak_Oth_* Mundlak_OthC_* Mundlak_Gi_* Mundlak_Mar_* ///
, or baselevels vce(robust)
outreg2 using results.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLim) stnum(replace coef=exp(coef), replace se=coef*se)
asdoc, text(xtologitLim) save(report.doc) append
asdoc test Mundlak_Gal_gali_Not Mundlak_Gal_gali_Limited Mundlak_C_changed_No Mundlak_C_changed_All Mundlak_C_changed_Newly Mundlak_Oth_cjs_Retired Mundlak_Oth_cjs_Employed Mundlak_Oth_cjs_Unemployed Mundlak_Oth_cjs_Permanently Mundlak_Oth_cjs_Homemaker Mundlak_Oth_cjs_Other Mundlak_Oth_thinc_No Mundlak_Oth_thinc_Below Mundlak_Oth_thinc_Above Mundlak_OthC_age Mundlak_OthC_yedu Mundlak_OthC_bmi Mundlak_OthC_eurod Mundlak_Gi_1 Mundlak_Gi_2 Mundlak_Gi_3 Mundlak_Mar_mst_Officially Mundlak_Mar_mst_Away Mundlak_Mar_mst_Never Mundlak_Mar_mst_Lost, save(report.doc) append

logit phinact age yedu i.gender i.thinc i.mst i.gali i.cjs c.bmi ib1.changed##c.eurod i.country i.Qyear [pweight=my_wgt], or baselevels vce(cluster id)
outreg2 using results.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(logitLim) stnum(replace coef=exp(coef), replace se=coef*se)


xtlogit phinact age yedu i.gender i.thinc i.single i.gali i.cjs c.bmi ib1.changed c.eurod i.country
estimates store RE
xtlogit phinact age yedu i.gender i.thinc i.single i.gali i.cjs c.bmi ib1.changed c.eurod i.country, fe
estimates store FE
asdoc hausman FE RE, alleqs save(report.doc) append


xtlogit phinact age yedu i.gender i.thinc i.single i.gali i.cjs c.bmi ib1.schanged##c.eurod i.country, fe or baselevels
outreg2 using results.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLimFE) stnum(replace coef=exp(coef), replace se=coef*se)

xtlogit phinact age yedu i.gender i.thinc i.single i.gali i.cjs c.bmi ib1.schanged##c.eurod i.country ///
Mundlak_Gal_* Mundlak_sC_* Mundlak_Oth_* Mundlak_OthC_* Mundlak_sGi_* Mundlak_Sin_* ///
, or baselevels vce(robust)
outreg2 using results.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLim) stnum(replace coef=exp(coef), replace se=coef*se)
asdoc, text(xtologitLim) save(report.doc) append
asdoc test Mundlak_Gal_gali_Not Mundlak_Gal_gali_Limited Mundlak_sC_schanged_Hooked Mundlak_sC_schanged_Unchanged Mundlak_sC_schanged_Back Mundlak_Oth_cjs_Retired Mundlak_Oth_cjs_Employed Mundlak_Oth_cjs_Unemployed Mundlak_Oth_cjs_Permanently Mundlak_Oth_cjs_Homemaker Mundlak_Oth_cjs_Other Mundlak_Oth_thinc_No Mundlak_Oth_thinc_Below Mundlak_Oth_thinc_Above Mundlak_OthC_age Mundlak_OthC_yedu Mundlak_OthC_bmi Mundlak_OthC_eurod Mundlak_sGi_1 Mundlak_sGi_2 Mundlak_sGi_3 Mundlak_Sin_single_No Mundlak_Sin_single_Yes, save(report.doc) append

logit phinact age yedu i.gender i.thinc i.single i.gali i.cjs c.bmi ib1.schanged##c.eurod i.country i.Qyear [pweight=my_wgt], or baselevels vce(cluster id)
outreg2 using results.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(logitLim) stnum(replace coef=exp(coef), replace se=coef*se)





xtlogit phinact age yedu i.gender i.thinc i.mst ib1.changed i.sphus i.cjs c.bmi c.eurod i.country
estimates store RE
xtlogit phinact age yedu i.gender i.thinc i.mst ib1.changed i.sphus i.cjs c.bmi c.eurod i.country, fe
estimates store FE
asdoc hausman FE RE, alleqs save(report.doc) append

xtlogit phinact age yedu i.gender i.thinc i.mst i.sphus i.cjs c.bmi ib1.changed##c.eurod i.country, fe or baselevels
outreg2 using results2.xls, excel replace dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLimFE) stnum(replace coef=exp(coef), replace se=coef*se)

xtlogit phinact age yedu i.gender i.thinc i.mst i.sphus i.cjs c.bmi ib1.changed##c.eurod i.country ///
Mundlak_Sph_* Mundlak_C_* Mundlak_Oth_* Mundlak_OthC_* Mundlak_Gi_* Mundlak_Mar_* ///
, or baselevels vce(robust)
outreg2 using results2.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLim) stnum(replace coef=exp(coef), replace se=coef*se)
asdoc, text(xtologitLim) save(report.doc) append
asdoc test Mundlak_Sph_sphus_Excellent Mundlak_Sph_sphus_Very Mundlak_Sph_sphus_Good Mundlak_Sph_sphus_Fair Mundlak_Sph_sphus_Poor Mundlak_C_changed_No Mundlak_C_changed_All Mundlak_C_changed_Newly Mundlak_Oth_cjs_Retired Mundlak_Oth_cjs_Employed Mundlak_Oth_cjs_Unemployed Mundlak_Oth_cjs_Permanently Mundlak_Oth_cjs_Homemaker Mundlak_Oth_cjs_Other Mundlak_Oth_thinc_No Mundlak_Oth_thinc_Below Mundlak_Oth_thinc_Above Mundlak_OthC_age Mundlak_OthC_yedu Mundlak_OthC_bmi Mundlak_OthC_eurod Mundlak_Gi_1 Mundlak_Gi_2 Mundlak_Gi_3 Mundlak_Mar_mst_Officially Mundlak_Mar_mst_Away Mundlak_Mar_mst_Never Mundlak_Mar_mst_Lost, save(report.doc) append

logit phinact age yedu i.gender i.thinc i.mst i.sphus i.cjs c.bmi ib1.changed##c.eurod i.country i.Qyear [pweight=my_wgt], or baselevels vce(cluster id)
outreg2 using results2.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(logitLim) stnum(replace coef=exp(coef), replace se=coef*se)


xtlogit phinact age yedu i.gender i.thinc i.single i.sphus i.cjs c.bmi ib1.schanged c.eurod i.country
estimates store RE
xtlogit phinact age yedu i.gender i.thinc i.single i.sphus i.cjs c.bmi ib1.schanged c.eurod i.country, fe
estimates store FE
asdoc hausman FE RE, alleqs save(report.doc) append


xtlogit phinact age yedu i.gender i.thinc i.single i.sphus i.cjs c.bmi ib1.schanged##c.eurod i.country, fe or baselevels
outreg2 using results2.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLimFE) stnum(replace coef=exp(coef), replace se=coef*se)

xtlogit phinact age yedu i.gender i.thinc i.single i.sphus i.cjs c.bmi ib1.schanged##c.eurod i.country ///
Mundlak_Sph_* Mundlak_sC_* Mundlak_Oth_* Mundlak_OthC_* Mundlak_sGi_* Mundlak_Sin_* ///
, or baselevels vce(robust)
outreg2 using results2.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(xtlogitLim) stnum(replace coef=exp(coef), replace se=coef*se)
asdoc, text(xtologitLim) save(report.doc) append
asdoc test Mundlak_Sph_sphus_Excellent Mundlak_Sph_sphus_Very Mundlak_Sph_sphus_Good Mundlak_Sph_sphus_Fair Mundlak_Sph_sphus_Poor Mundlak_sC_schanged_Hooked Mundlak_sC_schanged_Unchanged Mundlak_sC_schanged_Back Mundlak_Oth_cjs_Retired Mundlak_Oth_cjs_Employed Mundlak_Oth_cjs_Unemployed Mundlak_Oth_cjs_Permanently Mundlak_Oth_cjs_Homemaker Mundlak_Oth_cjs_Other Mundlak_Oth_thinc_No Mundlak_Oth_thinc_Below Mundlak_Oth_thinc_Above Mundlak_OthC_age Mundlak_OthC_yedu Mundlak_OthC_bmi Mundlak_OthC_eurod Mundlak_sGi_1 Mundlak_sGi_2 Mundlak_sGi_3 Mundlak_Sin_single_No Mundlak_Sin_single_Yes, save(report.doc) append

logit phinact age yedu i.gender i.thinc i.single i.sphus i.cjs c.bmi ib1.schanged##c.eurod i.country i.Qyear [pweight=my_wgt], or baselevels vce(cluster id)
outreg2 using results2.xls, excel dec(3) alpha(0.01, 0.05, 0.10) symbol(***, **, *) ctitle(logitLim) stnum(replace coef=exp(coef), replace se=coef*se)

log close InactivityAndMarriage
translate output.smcl output.pdf