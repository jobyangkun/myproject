
* This program is an example of an IV estimation.  It uses a data set from the Stock and Watson
* book on cigarettes consumption per US states.  
*It uses the old comand ivreg instead of ivregress.

* this command (#delimit) tells stata that the semi column sign implies to execute the command
* so stata presses "enter" anytime it reads ;, unless it is protected with a *

#delimit;
clear;
cap log close;
*************************************************************;
* Replication program for IV and cigarettes Chapter 12 in Stock and Watson 3E;
*************************************************************;
log using look_iv1.log,replace;

* This command allows the screen to stay only one second on, and then fills up with more information;
set more 1;

***********************************;
* Read In Data; 
* (Note: Change path name so that it is appropriate for your computer);
use cig_ch12_IV.dta;
sort state year;

des;

sum;
************************************************************;

*  Creating real values using the cpi;

gen ravgprs = avgprs/cpi;
 label var ravgpr "real average price during fiscal year, including sales
  taxes";
gen rtax = tax/cpi;
 label var rtax "real average Cig specifice tax during fiscal year";
gen rtaxs = taxs/cpi;
 label var rtaxs "real average total tax during fiscal year,including sales taxes";
gen rtaxso = rtaxs-rtax;
 label var rtaxso "real average sales tax per pack during fiscal year";
gen lpackpc = log(packpc);
gen lravgprs = log(ravgprs);
*;
* ---- Real Percapita State Income ;
gen perinc = income/(pop*cpi);
gen lperinc = log(perinc);
encode state, gen(snum);



* Creating 10-year differences in the variables to deal with unobserved fixef effects at the state level;
gen ltpackpc = log(packpc/packpc[_n-1]);
gen ltavgprs = log(ravgprs/ravgprs[_n-1]);
gen ltperinc = log(perinc/perinc[_n-1]);
gen dtrtaxs  = rtaxs-rtaxs[_n-1];
gen dtrtax   = rtax-rtax[_n-1];
gen dtrtaxso = rtaxso-rtaxso[_n-1];

keep if year==1995;
************************************************************;
* OLS and IV estimation - cross section -- 1995;
* Note that all regressions are run with the robust option: reg yvar xvar, r  where r ;
* stands for robust and yields the white variance covariance matrix;
* to give heteroskedasticity corrected standard errors;

************************************************************;

* -- Equation (12.9) in the Stock and Watson book;
reg lravgprs rtaxso, r;

* -- Equation (12.10) and (12.11);
* -- OLS -- Not Reported;
reg lpackpc lravgprs, r;

* -- IV -- Reported;
ivreg lpackpc (lravgprs = rtaxso), r;

* -- controlling for income: Equation (12.15) IV;
ivreg lpackpc (lravgprs = rtaxso) lperinc, r;
* --  controlling for income and using two instruments: Equation (12.16) IV;
ivreg lpackpc (lravgprs = rtaxso rtaxs) lperinc, r;

************************************************************;
* OLS and IV estimation - Differences 1995-1985;
* Creating Table 12.1 in the stock and watson chapter;
* using the first differences in the variables, accounting  for;
* unobserved fixed effects
************************************************************;
* -- OLS -- Not Reported;
reg ltpackpc ltavgprs ltperinc,r;
* col(1);
* -- IV -- Reported;
ivreg ltpackpc (ltavgprs = dtrtaxso)  ltperinc, r;

* checking first stage;
reg ltavgprs dtrtaxso ltperinc, r;
test dtrtaxso;

* -- IV -- Reported;
ivreg ltpackpc (ltavgprs = dtrtax)  ltperinc, r;
reg ltavgprs dtrtax ltperinc, r;
test dtrtax;

* -- IV -- Reported with two instruments: general sales tax and specific tax on cigarettes by state;
ivreg ltpackpc (ltavgprs = dtrtax dtrtaxso) ltperinc, r;

* We want to construct the test of overidentification to see about which instruments are;
* valid using both conditions;

* For the J test, first we take residuals from the previous IV regression;

predict e, resid;


* then we regress the residuals against the instruments and the exogenous variable;

 reg e dtrtaxso dtrtax ltperinc; 
 drop e;
 
 * we now test for the signicance of the instruments but recall we are overidentified m>k: 2>1, ;
* and the test has degrees of freedom m-k, ;
 
 * qui stands for quietly i.e. do not display output;
 
 qui test dtrtaxso dtrtax;
    dis "---- OverID stat:  " r(df)*r(F)
     _skip(10) "p-value:  "  chiprob(r(df)-1,r(df)*r(F)) " -----";
reg ltavgprs dtrtax dtrtaxso ltperinc, r;
test dtrtax dtrtaxso;
log close;



reg lpackpc rtaxso
reg ltavgprs rtaxso

ge see =.0332974/.0307289
tab see
ivreg lpackpc (ltavgprs = rtaxso), r first





