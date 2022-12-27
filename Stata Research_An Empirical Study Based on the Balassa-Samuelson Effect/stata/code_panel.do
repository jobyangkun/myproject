*** Kun Yang MA project---Panel analysis
clear all
use pwt90

/////////////////clean the data & generate variable////////////////////////
*** drop oil countries
drop if countrycode==("SAU")
drop if countrycode==("ARE")
drop if countrycode==("KWT")
drop if countrycode==("QAT")

*** drop the countries if its population smaller than 1.3 million
gen vardrop=1 if pop<1.3
sort countrycode vardrop
by countrycode: replace vardrop=vardrop[1]
drop if vardrop==1

*** generate the reference price level of usa in each years
generate pl_gdpousa=pl_gdpo if countrycode=="USA"
bysort year (pl_gdpousa): replace pl_gdpousa = pl_gdpousa[1]

*** Generate needed variables(log price level relative to USA & real GDP per capita)
generate PL=pl_gdpo/pl_gdpousa /* dividing Price Level of GDP for each country by that of the US (normalised to 100)*/
generate logPL=ln(PL)         /* log Price Level*/
generate rgdpc=rgdpo/pop       /*Real GDP per capita (Constant price: Chain series)*/
generate logrgdpc=ln(rgdpc)   /*Log of real GDP per capita */
generate logrgdpc2=(logrgdpc)^2 /*squared log of real GDP per capita*/


////////////////panel dimension/////////////////////
encode countrycode, gen(countrynum)
xtset countrynum year



/////////////// draw graphs of the full sample///////////////////////////////
*** OLS estimation for the full sample
twoway (scatter logPL logrgdpc, color(black)) ///
 (lfit logPL logrgdpc) ///
 (scatter logPL logrgdpc if countrycode=="CHN", color(red)), ///
 name(OLS) xtitle(Log GDP per Capita) ytitle(Log Price Level of GDP) ///
  legend(lab(1 "log PL") lab(2 "OLS Fit Line") lab(3 "observed China PL"))


 *** non-parametric estimation for the full sample
twoway (scatter logPL logrgdpc, color(black)) ///
 (qfit logPL logrgdpc) ///
 (scatter logPL logrgdpc if countrycode=="CHN", color(red)), ///
 name(nonparametric) xtitle(Log GDP per Capita) ytitle(Log Price Level of GDP) ///
 legend(lab(1 "log PL") lab(2 "Quadratic Fit Line") lab(3 "observed China PL"))


*** lowess estimation graph
lowess logPL logrgdpc, bwidth(.8) ///
 name(lowess) xtitle(Log GDP per Capita) ytitle(Log Price Level of GDP)



/////////////// run linear//////////////////////////////
reg logPL logrgdpc i.year, robust


/////////////// run with quadratic//////////////////////////////
reg logPL logrgdpc logrgdpc2 i.year, robust

 
/////////////// run fixed effect regression///////////////////////////////
xtreg logPL logrgdpc logrgdpc2 i.year, fe cluster(countrynum)


forval i=2004/2014 {
 predict yhat3_`i' if countrycode=="CHN" & year==`i', xb                                       /* predicted price level*/
 predict error3_`i' if countrycode=="CHN" & year==`i', stdp                                    /* standard error of yhat*/
 generate lb3_`i' = yhat3_`i' - invnormal(0.975)*error3_`i' if countrycode=="CHN" & year==`i'   /*  lower bound of 95% IC*/
 generate ub3_`i' = yhat3_`i' + invnormal(0.975)*error3_`i' if countrycode=="CHN" & year==`i'   /*  upper bound of 95% IC*/
 generate mis3_`i' = logPL - yhat3_`i' if countrycode=="CHN" & year==`i'                       /*  misalignment*/
 list yhat3_`i' error3_`i' lb3_`i' ub3_`i' mis3_`i' if countrycode=="CHN" & year==`i'
}


*** draw Predicted Log PL
*** generate upper bound, lower bound, Predicted Y  (for all years) by loop
gen ub3=.
gen lb3=.
gen yhat3=.

forval i=2004/2014 {
replace ub3=ub3_`i' if ub3_`i' !=.
replace lb3=lb3_`i' if lb3_`i' !=.
replace yhat3=yhat3_`i' if yhat3_`i' !=.
}

///
twoway (rcap ub3 lb3 year if countrycode=="CHN", lstyle(ci)) ///
   (scatter yhat3 year, color(black))  ///
   (scatter logPL year if countrycode=="CHN", color(red)) if year >2003, ///
   name(b_panel2)  legend(lab(1 "95% CI") lab(2 "Predicted Log PL") lab(3 "Observed Log PL")) ///
   note("with 95% confidence interval") ytitle(Log Price Level of GDP) xtitle(Year)
   
 
 //////////// Appendix 
list year PL if countrycode=="CHN" & year>2003
list year logPL if countrycode=="CHN"  & year>2003
list year rgdpo if countrycode=="CHN"   & year>2003
list year logrgdpc if countrycode=="CHN"   & year>2003
 
 
 
 
 
 
