log using november6,replace
clear

//use Happiness.dta
use "Happiness.dta"
des

* // simple regression
reg happiness income1000 

generate Dbob=id==1 if !missing(id)
generate Dsarah=id==2 if !missing(id)
generate Dpeter=id==3 if !missing(id)
generate Dnicole=id==4 if !missing(id)

regress happiness Dsarah Dpeter Dnicole income1000

gen Bob = -4.107029 + 0.0824934*income1000
gen Sarah = -4.107029 + 5.111141 + 0.0824934*income1000
gen Peter = -4.107029 + 4.226857 + 0.0824934*income1000
gen Nicole = -4.107029 + 9.410411 + 0.0824934*income1000
graph twoway line Bob income1000 || line Sarah income1000 || line Peter income1000 || line Nicole income1000 // fixed effects model 

* // analogous fixed effects model
reg happinessDM income1000DM 

*  // simple regression
reg happiness income1000

* Now doing it using the xt commands
xtset id

// fixed effects model
xtreg happiness income1000,fe






* Another large panel data set
clear
use "BritishHouseholdPanel.dta"

xtset id year

tab year
xtdescribe
xtsum

* // simple regression
reg mental age woman couple separated divorced never_married  

* Let's look at heteroskedasticity
quietly reg mental age woman couple separated divorced never_married
predict Pmental
gen Rmental=mental-Pmental

* //graphs the residuals against the fitted values
scatter Rmental Pmental 

* //Breusch-Pagan/Cook-Weisberg test for heteroskedasticity
hettest 

* // regression using the robust and cluster options
regress mental age woman couple separated divorced never_married, vce(cluster id)
regress mental age woman couple separated divorced never_married,robust clust(id)

* Now, let's do the between regression.  First we will collapse the data set at the level of id, taking
* the average of the variables by id

quietly regress mental age woman couple separated divorced never_married
collapse (mean) mental age woman couple separated divorced never_married if e(sample), by(id)  // this alters the data structure

* the data set is no longer panel.  It is a cross section.
* // between effects model
regress mental age woman couple separated divorced never_married 






* // Let's redo the between effects model without altering the data structure
clear
use "BritishHouseholdPanel.dta"

xtreg mental age woman couple separated divorced never_married, be 

* note how the coefficients are under the be option compared to the regression done on the 
* collapsed (Averaged by id) data set
regress mental age woman couple separated divorced never_married 

 
* Now let's abstract from the functional form xb and let's concentrate on the error term
xtset id

* // fixed effects model
xtreg mental age woman couple separated divorced never_married, fe 

*xtset year
*xtreg menat age woman couple separated divorced never_married, fe // time fixed effects
xtset id
xtreg mental age woman couple separated divorced never_married, re // random effects

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
* of the X's should be about zero (i.e. (b-B) about zero for the included x's
hausman fixed random 



log close
