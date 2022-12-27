
log using november8.log, replace
clear
*  This is november8.log, picking up on what was done on Monday November 6
* // Let's redo the between effects model without altering the data structure





///1111111111111111111111111111111111111111111111111111111111111
use BritishHouseholdPanel.dta
* Now, let's do the between regression.  First we will collapse the data set at the level of id, taking
* the average of the variables by id

quietly regress mental age woman couple separated divorced never_married
collapse (mean) mental age woman couple separated divorced never_married if e(sample), by(id)  // this alters the data structure

* the data set is no longer panel.  It is a cross section.
* // between effects model
regress mental age woman couple separated divorced never_married 








///2222222222222222222222222222222222222222222222222222222222222
clear
use BritishHouseholdPanel.dta

xtreg mental age woman couple separated divorced never_married, be 

* note how the coefficients are under the be option compared to the regression done on the 
* collapsed (Averaged by id) data set
regress mental age woman couple separated divorced never_married 

 
* Now let's abstract from the functional form xb and let's concentrate on the error term
xtset id
xtsum mental age woman couple separated divorced never_married 

// fixed effects model
xtreg mental age woman couple separated divorced never_married, fe 

// time fixed effects
*xtset year
*xtreg menat age woman couple separated divorced never_married, fe
// random effects
xtset id
xtreg mental age woman couple separated divorced never_married, re

* Which model is the correct one?
* LEt's run a haussman test to comapre the coefficients.  
* Under the null of no covariance between the x's and the individual effect
*, one estimate is consistent and efficient (the RE) and the other one, while consistent, is innefficient (FE)
* under the alternative, one estimate is inconsistent (RE) and the other one is consistent (FE).

* LEt's get the estimates:
quietly xtreg mental age woman couple separated divorced never_married, fe
estimates store fixed
quietly xtreg mental age woman couple separated divorced never_married, re
estimates store random


* // Hausman test for model efficiency
* under the null of no covariance, the difference between the two estimators of the coefficients
* of the X's should be about zero (i.e. (b-B) about zero for the included x's)
hausman fixed random 
xtreg mental age woman couple separated divorced never_married, fe robust cluster(id)








///333333333333333333333333333333333333333333333333333333333333333333333
* Let's try with another data set
use traffic.dta
des
sum state year fatal spircons unrate beertax perincK

* let's get basic summary stats using the xtsum command
xtsum state year fatal spircons unrate beertax perincK

* determinants of fatalities
xtreg fatal beertax spircons unrate perincK,fe

* LEt's include some time factors
tab year
tab year, gen(yrdum)
des yrdu*

xtreg fatal beertax spircons unrate perincK yrdum1-yrdum6,fe

* looks like years are important as well
test yrdum2  yrdum3 yrdum4 yrdum5 yrdum6 yrdum1  

* between, fixed and random effects models
xtreg fatal beertax spircons unrate perincK yrdum1-yrdum6,be
xtreg fatal beertax spircons unrate perincK yrdum1-yrdum6,re
xtreg fatal beertax spircons unrate perincK, re

* Fixed or random?  LEt's do a hausman test
quietly xtreg fatal beertax spircons unrate perincK ,fe
estimates store fix
quietly xtreg fatal beertax spircons unrate perincK ,re
estimates store ran
hausman fix ran

xtreg fatal beertax spircons unrate perincK yrdum1-yrdum6,fe
xtreg fatal beertax spircons unrate perincK yrdum1-yrdum6,fe robust 










//44444444444444444444444444444444444444444444444444444444444444444444
clear
use TimeSeriesCrossSection.dta
des
xtdes
tsset cow year
tab cow

* // reports duplicates
duplicates report cow year  
duplicates list cow year

sum FDI, detail

*  // generating log transformed variable
gen lnFDI=ln(FDI) 
sum lnFDI, detail

hist FDI, normal
hist lnFDI, normal
graph twoway line lnFDI year if cow<100
graph twoway line lnFDI year if cow>=100 & cow<200
graph twoway line lnFDI year if cow>=200 & cow<300
graph twoway line lnFDI year if cow>=300 & cow<400
graph twoway line lnFDI year if cow>=400 & cow<500
graph twoway line lnFDI year if cow>=500 & cow<600
graph twoway line lnFDI year if cow>=600 & cow

* Looking at Stationary, using Dickey Fuller test   Right now, ding it for one country: Canada
* // Dickey-Fuller test
dfuller lnFDI if cow==20, lags(1)  
dfuller lnFDI if cow==20, lags(1) regress
corrgram lnFDI if cow==20, lags(10)
ac lnFDI if cow==20, lags(10)

* now pooling over all countries, running a regression  of determinants of lnFDI
* with lags of dependend variable and lags of GDPsper capita, GDP growth, ethnic fraction
 // regression 
regress lnFDI l.lnFDI l.GDPperCapita l.GDPGrowth ethfrac l.incidence, vce(cluster cow)

* One could also us the xt command with the random effects and fixed effects
* and one can also attempt to deal with heteroskedasticity using the
* xtpcse command
help xtpcse




* looking at limited dependent variables
tab onset2
