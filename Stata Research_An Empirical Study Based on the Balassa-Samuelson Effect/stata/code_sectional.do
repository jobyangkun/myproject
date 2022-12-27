*** Kun Yang MA project---cross-sectional analysis
clear all
use pwt90

/////////////////clean the data & generate variable////////////////////////
*** generate the reference price level of usa in each years
generate pl_gdpousa=pl_gdpo if countrycode=="USA"
bysort year (pl_gdpousa): replace pl_gdpousa = pl_gdpousa[1]

*** Generate needed variables(log price level relative to USA & real GDP per capita)
generate PL=pl_gdpo/pl_gdpousa /* dividing Price Level of GDP for each country by that of the US (normalised to 100)*/
generate logPL=ln(PL)         /* log Price Level*/
generate rgdpc=rgdpo/pop       /*Real GDP per capita (Constant price: Chain series)*/
generate logrgdpc=ln(rgdpc)   /*Log of real GDP per capita */
generate logrgdpc2=(logrgdpc)^2 /*squared log of real GDP per capita*/
encode countrycode, gen(ncountrycode)

*** identify outliers
reg logPL logrgdpc logrgdpc2 if year==2011
predict cook if e(sample), cooksd
lvr2plot, mlabel(countrycode) name(cook)
dfbeta logrgdpc
scatter _dfbeta_1 ncountrycode, yline(0.148 -0.148) ylabel(-0.6(.2)0.6) mlabel(countrycode) ytitle(Countrycode) name(dfbeta)

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





/////////////////// draw timeline graphs///////////////////////////
*** Graph of Price Level of GDP for each year 
twoway (line logPL year if countrycode==("JPN"),sort color(yellow)) ///
 (line logPL year if countrycode==("IND"),sort color(blue)) ///
 (line logPL year if countrycode==("USA"),sort color(black)) ///
 (line logPL year if countrycode==("CHN"),sort color(red)), ///
 name(logPL) ytitle(Log Price Level of GDP) xlabel(1950(5)2014) ///
 legend(lab(1 "Japan") lab(2 "India") lab(3 "USA") lab(4 "China"))

*** Graph of Real GDP per capita for each year
twoway (line logrgdpc year if countrycode==("JPN"),sort color(yellow)) ///
 (line logrgdpc year if countrycode==("IND"),sort color(blue)) ///
 (line logrgdpc year if countrycode==("USA"),sort color(black)) ///
 (line logrgdpc year if countrycode==("CHN"),sort color(red)), ///
 name(logrgdpc) ytitle(Log Real GDP per Capita) xlabel(1950(5)2014) ///
 legend(lab(1 "Japan") lab(2 "India") lab(3 "USA") lab(4 "China"))


 

///////////////////// linear model/////////////////////////////////
*** ssc install estout, replace (ssc install estout, replace)
*** Cross-Country Regression(2004-2014), set a loop linear regression to test each year 
forval i=2004/2014 {
 eststo: quietly reg logPL logrgdpc if year==`i', robust    /* Linear model*/
 estat ic                                                                                  /* AIC BIC*/
 predict yhat_`i' if countrycode=="CHN" & year==`i', xb                                    /* predicted price level*/
 predict error_`i' if countrycode=="CHN" & year==`i', stdp                                 /* standard error of yhat*/
 predict ehat_`i', residuals                                                               /* predicted residual*/
 generate lb_`i' = yhat_`i' - invnormal(0.975)*error_`i' if countrycode=="CHN" & year==`i' /* lower bound of 95% IC*/
 generate ub_`i' = yhat_`i' + invnormal(0.975)*error_`i' if countrycode=="CHN" & year==`i' /* upper bound of 95% IC*/
 generate mis_`i' = logPL - yhat_`i' if countrycode=="CHN" & year==`i'                     /* misalignment*/
 list yhat_`i' error_`i' lb_`i' ub_`i' mis_`i' if countrycode=="CHN" & year==`i'
}

esttab using linear.rtf, ar2 se label legend ///
 mtitles(2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014)
eststo clear

*** generate upper bound, lower bound, Predicted Y  (for all years) by loop
gen ub=.
gen lb=.
gen yhat=.

forval i=2004/2014 {
replace ub=ub_`i' if ub_`i' !=.
replace lb=lb_`i' if lb_`i' !=.
replace yhat=yhat_`i' if yhat_`i' !=.
}

** draw the graph
twoway (rcap ub lb year if countrycode=="CHN", lstyle(ci)) ///
   (scatter yhat year, color(black))  ///
   (scatter logPL year if countrycode=="CHN", color(red)) if year >2003, ///
   name(linearall)  legend(lab(1 "95% CI") lab(2 "Predicted log PL") lab(3 "Observed log PL")) ///
   note("with 95% confidence interval") ytitle(Log Price Level of GDP) xtitle(Year)

   
***regression of 2014
twoway (lfitci logPL logrgdpc)   ///
 (scatter logPL logrgdpc, color(black))  ///
 (scatter logPL logrgdpc if countrycode=="CHN", mlabel(countrycode) color(red)) if year==2014, /// 
 name(linear2014) xtitle(Log GDP per Capita) ytitle(Log Price Level of GDP) ///
 legend(lab(1 "95% CI") lab(2 "Linear Fit") lab(4 "China's Log PL"))

reg logPL logrgdpc if year==2014
rvpplot logrgdpc, mlabel(countrycode) ylabel(-1(.2)1) xtitle(Log GDP per Capita) name(linear2014resp)










///////////////////// Quadratic model//////////////////////////////////////
*** Cross-Country Regression (2004-2014), set a loop quadratic regression to test each year
forval i=2004/2014 {
 eststo: quietly reg logPL logrgdpc logrgdpc2 if year==`i', robust                              /* Quadratic model*/
 estat ic                                                                                     /*  AIC BIC*/
 predict yhat2_`i' if countrycode=="CHN" & year==`i', xb                                       /* predicted price level*/
 predict error2_`i' if countrycode=="CHN" & year==`i', stdp                                    /* standard error of yhat*/
 predict ehat2_`i', residuals                                                                 /* predicted residual*/
 generate lb2_`i' = yhat2_`i' - invnormal(0.975)*error2_`i' if countrycode=="CHN" & year==`i'   /*  lower bound of 95% IC*/
 generate ub2_`i' = yhat2_`i' + invnormal(0.975)*error2_`i' if countrycode=="CHN" & year==`i'   /*  upper bound of 95% IC*/
 generate mis2_`i' = logPL - yhat2_`i' if countrycode=="CHN" & year==`i'                       /*  misalignment*/
 list yhat2_`i' error2_`i' lb2_`i' ub2_`i' mis2_`i' if countrycode=="CHN" & year==`i'
}

esttab using quadratic.rtf, ar2 se label legend ///
 mtitles(2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014)
eststo clear

*** generate upper bound, lower bound, Predicted Y  (for all years) by loop
gen ub2=.
gen lb2=.
gen yhat2=.

forval i=2004/2014 {
replace ub2=ub2_`i' if ub2_`i' !=.
replace lb2=lb2_`i' if lb2_`i' !=.
replace yhat2=yhat2_`i' if yhat2_`i' !=.
}
//

** draw the graph
twoway (rcap ub2 lb2 year if countrycode=="CHN", lstyle(ci)) ///
   (scatter yhat2 year, color(black))  ///
   (scatter logPL year if countrycode=="CHN", color(red)) if year >2003, ///
   name(quadra)  legend(lab(1 "95% CI") lab(2 "Predicted Log PL") lab(3 "Observed Log PL")) ///
   note("with 95% confidence interval") ytitle(Log Price Level of GDP) xtitle(Year)
   
   
***regression of 2014
twoway (qfitci logPL logrgdpc)  ///
 (scatter logPL logrgdpc, color(black))  ///
 (scatter logPL logrgdpc if countrycode=="CHN", mlabel(countrycode) color(red)) if year==2014, /// 
 name(A_quadra2014) xtitle(Log GDP per Capita) ytitle(Log Price Level of GDP) ylabel(-1(.2)1) ///
 legend(lab(1 "95% CI") lab(2 "Linear Fit") lab(4 "China"))
 
reg logPL logrgdpc logrgdpc2 if year==2014
rvpplot logrgdpc, mlabel(countrycode) ylabel(-1(.2)1) xtitle(Log GDP per Capita) name(qra_2014res2g)

 
 
 
///////////*** likelihood ratio tests/////////////////////////
forval i=2004/2014 {
 quietly reg logPL logrgdpc if year==`i'           /*  Linear model*/
 estimates store m1
 quietly reg logPL logrgdpc logrgdpc2 if year==`i' /*  Quadratic model*/
 estimates store m2
 lrtest m1 m2                                      /*  likelihood ratio tests*/
}
////




 //////////// Appendix ////////////////////////////////
list year PL if countrycode=="CHN" & year>2003
list year logPL if countrycode=="CHN"  & year>2003
list year rgdpo if countrycode=="CHN"   & year>2003
list year logrgdpc if countrycode=="CHN"   & year>2003
 
 
 
 
 
