*** Question 1  (Simple DD)
* Part A  (summary statistics)
use hh_9198, clear
count
egen hh_n = group(nh)
tabstat hh_n, stat(max) by(year)
tabstat exptot, by(year)

* Part B
gen exptot0=exptot if year==0
egen exptot91=max(exptot0), by(nh)
keep if year==1
gen lexptot91=ln(1+exptot91)
gen lexptot98=ln(1+exptot)
gen lexptot9891=lexptot98-lexptot91

* Part C
ttest lexptot9891, by (dfmfd)




*** Question 2  (DD method OLS)
clear
use hh_9198, clear
gen lexptot=ln(1+exptot)
gen dfmfd1=dfmfd==1 & year==1
egen dfmfd98=max(dfmfd1), by(nh)
gen dfmfdyr = dfmfd98*year

* Part A
reg lexptot dfmfdyr dfmfd98 year

* Part B  (including other covariates)
gen lnland=ln(1+hhland/100)
reg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]





*** Question 3  (fixed effect)
* Part A
xtreg lexptot year dfmfd98 dfmfdyr, fe i(nh)
estimates store fixed

* Part B  (including other covariates)
xtreg lexptot year dfmfd98 dfmfdyr sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, fe i(nh)

* Part C
clear
use hh_9198, clear
gen exptot0=exptot if dfmfd==0
egen exptotout=max(exptot0), by(nh)

keep if dfmfd==1
gen lexptotout=ln(1+exptotout)
gen lexptotin=ln(1+exptot)
gen lexptotinout=lexptotin-lexptotout

ttest lexptotinout, by (year)


* Calculating Difference-in-Differences Estimates by Taking the Difference between Before-After Difference in the Treatment and Comparison Groups
clear
use hh_9198, clear
gen lexptot=ln(1+exptot)
gen dfmfd1=dfmfd==1 & year==1
egen dfmfd98=max(dfmfd1), by(nh)
gen dfmfdyr = dfmfd98*year
gen lnland=ln(1+hhland/100)


keep lexptot dfmfd nh year sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg
reshape wide lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg, i(nh) j(year)

gen dy = lexptot1 - lexptot0 
replace dfmfd0=0
gen dpdfmfd = dfmfd1-dfmfd0
gen dpsexhead = sexhead1-sexhead0
gen dpagehead = agehead1-agehead0
gen dpeduchead = educhead1-educhead0
gen dplnland = lnland1-lnland0
gen dpvaccess = vaccess1-vaccess0
gen dppcirr = pcirr1-pcirr0
gen dprice = rice1-rice0
gen dpwheat = wheat1-wheat0
gen dpmilk = milk1-milk0
gen dpoil = oil1-oil0
gen dpegg = egg1-egg0

	
reg dy dpdfmfd
reg dy dpdfmfd dpsexhead dpagehead dpeduchead dplnland dpvaccess dppcirr dprice dpwheat dpmilk dpoil dpegg






