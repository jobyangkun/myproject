* This program uses the data set from the book 
* Impact Evaluation in Practice, second edition
* using the HISP Case Study. 
*  it is an edited version of the authors'  do file for tables and figures in book



clear
clear matrix
set more off

*============================*
*Specify the access path to the computer folder you will use for the analysis
*============================*
*cd "INSERT THE PATH OF THE FOLDER WHERE YOU SAVED THE DATASET, EG. C:\My Documents\HISP"
*============================*
*Initialize
*============================*
cap log close
log using hisp1, replace 
*Open the cleaned data set
use "evaluation.dta" 

*============================*
*Macros: make sure you run this piece of code before running the regressions
*============================*
*put together a standard list of explanatory variables to be used in multivariate analysis

global controls  age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance
describe $controls


* some information on important variables we will use
sum $controls
tab eligible
tab eligible enrolled
tab round
tab treatment_loca
tab enrolled
tab enrolled treatment_loc
* Another one
tab enrolled_rp






*============================*
*Start solution
*----------------------------------------------------*
*Method 1: No Design - before and after (Chapter 3)
* In this method, you compare the before and after situation of households who 
* enrolled in the program in villages covered by HISP.

*Select the relevant data
use "evaluation.dta", clear
* keep the treated locality and those who enrol only
keep if treatment_locality==1 
keep if enrolled ==1

*Table 3.1 Method 1-HISP Impact Using Before-After (Comparison of Means) 
ttest health_expenditures, by(round) 

*Table 3.2 Method 1-HISP Impact Using Before-After (Regression Analysis) Linear Regression
reg health_expenditures round, cl(locality_identifier)

*Table 3.2 Method 1-HISP Impact Using Before-After (Regression Analysis) Multivariate Linear Regression
reg health_expenditures round $controls, cl(locality_identifier)





*----------------------------------------------------*
*Method 2: No Design - enrolled-not enrolled (Chapter 3)
* In this method, you compare follow-up situation of enrolled and not enrolled households in villages covered by HISP.

*Select the relevant data
use "evaluation.dta", clear
* keep the treatment area only
keep if treatment_locality==1
* keep the endline
keep if round==1

*Table 3.3 Method 2-HISP Impact Using Enrolled-Nonenrolled (Comparison of Means)
ttest health_expenditures, by(enrolled)

*Table 3.4 Method 2-HISP Impact Using Enrolled-Nonenrolled (Regression Analysis) Linear Regression
reg health_expenditures enrolled, cl(locality_identifier)

*Table 3.4 Method 2-HISP Impact Using Enrolled-Nonenrolled (Regression Analysis) Multivariate Linear Regression
reg health_expenditures enrolled $controls, cl(locality_identifier)
* Of course, the question is whether the above offers valid counterfactuals.





*----------------------------------------------------*
*Method 3: Randomized assignment (Chapter 4)
* In this method, you compare follow-up situation of eligible households in treatment and comparison villages.
* remember that eligibility is a function of a poverty index: those below some poverty line are eligible, 
* those above are not eligible for the treatment.

*Select the relevant data
use "evaluation.dta", clear
keep if eligible==1

*Table 4.1 Method 3-Balance between Treatment and Comparison Villages at Baseline for Health Expenditures 
ttest health_expenditures if round==0, by(treatment_locality)
*Table 4.1 Method 3-Balance between Treatment and Comparison Villages at Baseline for Controls 

* let's do a do loop over the variables in the macro: for each variable, we will test the difference between treatment and controls
foreach x of global controls {
	describe `x'
	ttest `x' if round==0, by(treatment_locality)
	}

*Table 4.2 Method 3-HISP Impact Using Randomized Assignment (Comparison of Means) at Baseline 
ttest health_expenditures if round==0, by(treatment_locality) 

*Table 4.2 Method 3-HISP Impact Using Randomized Assignment (Comparison of Means) at Follow-up 
ttest health_expenditures if round==1, by(treatment_locality)

*Table 4.3 Method 3-HISP Impact Using Randomized Assignment (Regression Analysis) Linear Regression 
reg health_expenditures treatment_locality if round==1, cl(locality_identifier)

*Table 4.3 Method 3-HISP Impact Using Randomized Assignment (Regression Analysis) Multivariate Linear Regression 
reg health_expenditures treatment_locality $controls if round==1, cl(locality_identifier)







*----------------------------------------------------*
* Method 4: Instrumental Variables (Chapter 5)
* INSTRUMENTAL VARIABLES, ITT and LATE ESTIMATES 
* In this context, the program is randomized at the village level.
* While everyone is eligible for the program in treatment communities, not everyone participates. 

*Select the relevant data
use "evaluation.dta", clear
drop eligible

* You can estimate 'intent-to-treat estimates', i.e. program impact at the village-level irrespective of who takes up the program or not.
*Example 5 - "Intent-to-Treat" (ITT) Estimates
reg health_expenditures treatment_locality if round ==1, cl(locality_identifier)
* ITT=Takeup*LATE
* so take up is 0.59 in the population.  We can find LATE by LATE=ITT/0.59
* You can back out 'local average treatment effect' estimates on complier units that do take-up the program in treatment communities

*Example 6 - "Local Average Treatment Effect" (Late) 2SLS IV estimates
ivreg health_expenditures (enrolled = treatment_locality) if round ==1, first 

* note that the followign syntax is from more recent command has been introduced in stata for the same estimation
ivregress 2sls health_expenditures (enrolled = treatment_locality) if round ==1, first

*----------------------------------------------------*
* INSTRUMENTAL VARIABLES AND RANDOMIZED PROMOTION 
* Here, assum that HISP is offered universally in the country.  Assume further that
* a randomized promotion campaign takes place.  For instance, an intenisive promotion effort
* is undrtaken in a random subsample of villages aimed at increasing awareness of HISP.
* this promotion campaign is an IV: it increases enrollement in HISP, and it may not
* affect directly the outcome indicator (health expenditure), apart from its ability to
* affect participation . i.e. it may not affect the error term

* In this context, everyone is eligible for the program. You compare what happens in promoted and non-promoted villages.
 
*Select the relevant data
use "evaluation.dta", clear
drop eligible
drop treatment_locality
drop enrolled
tab enrolled_rp

*Table 5.1	Method 4-HISP Impact Using Randomized Promotion (Comparison of Means) at Baseline 
ttest health_expenditures if round ==0, by(promotion_locality)

*Table 5.1	Method 4-HISP Impact Using Randomized Promotion (Comparison of Means) at Follow-up 
ttest health_expenditures if round ==1, by(promotion_locality)

*Table 5.1	Method 4-HISP Impact Using Randomized Promotion (Comparison of Means) for Enrollment
ttest enrolled_rp if round ==1, by(promotion_locality)

* Example 7 - 2SLS IV Estimates for Randomized Promotion
ivreg health_expenditures (enrolled_rp = promotion_locality) if round ==1, first

* you could also use a multivariate regression
ivreg health_expenditures (enrolled_rp = promotion_locality) age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance if round ==1, first

* note that the followign syntax is from more recent command has been introduced in stata for the same estimation
ivregress 2sls health_expenditures (enrolled_rp = promotion_locality) if round ==1, first





*----------------------------------------------------*
* DIFFERENCE-IN-DIFFERENCES
*Method 6: Dif in Dif (Chapter 7)
* What if we use as counterfactuals the non-enrolled households?
* In this method, you compare the change in health expenditures over time 
* between enrolled and nonenrolled households in the treatment localities.

*Select the relevant data
use "evaluation.dta", clear
keep if treatment_locality==1

* Example 8 - Difference-in-Differences in a Regression Framework
* recall the round dummy
gen eligible_round=eligible*round
reg health_expenditures eligible_round round eligible, cl(locality_identifier)

* Example 9 - Difference-in-Differences in a Multivariate Regression Framework
reg health_expenditures eligible_round round eligible age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance, cl(locality_identifier)

* remember we have panel data!  We can use the xtcommands
* Example 10 - Household Fixed Effect Estimates for Difference-in-Differences
xtset household_identifier round
gen xtenrolled=0
replace xtenrolled=1 if enrolled==1 & round==1
xtreg health_expenditures xtenrolled round, fe vce(cluster locality_identifier)

* Example 11 -  Calculating Difference-in-Differences Estimates by Taking the Difference between Before-After Difference in the Treatment and Comparison Groups
keep health_expenditures treatment_locality locality_identifier enrolled household_identifier round
reshape wide health_expenditures enrolled, i(household_identifier) j(round)
 	
gen dy = health_expenditures1 - health_expenditures0 
replace enrolled0=0
gen dp = enrolled1-enrolled0 
	
reg dy dp if treatment_locality==1, cl(locality_identifier)
	

	
	
	

	
	
	
	
	
	
	
	
	
*  If we have time...
*----------------------------------------------------*
*Method 5: Regression Discontinuity Design RDD (Chapter 6)

* In this method, you compare health expenditures at follow-up between households just above 
* and just below the poverty index threshold, in the treatment localities.

*Select the relevant data
use "evaluation.dta", clear
keep if treatment_locality==1

*Additional Poverty Index and Health Expenditures at the Health Insurance Subsidy Program Baseline
reg health_expenditures poverty_index if round ==0
predict he_pred0
graph7 he_pred0 poverty_index if round ==0

*Figure 6.5 HISP Household Density by Poverty Index 
	* The below line of code creates a simple graph.
	kdensity poverty_index

	*The below lines of code allow you to reproduce figure 6.7 exactly as depicted in the book
	*Figure 6.5 HISP: Density of Households, by Baseline Poverty Index 
	#delimit;
	cap erase fig65.gph;
	kdensity poverty_index, 
		saving(fig65) 
		title("") 
		note("Technical note: Density estimated using univariate epanechnikov kernel method.") 
		ytitle("Estimated density") 
		xtitle("Baseline poverty index (20-100)") 
		ylabel(, angle(horizontal)) 
		plotregion(fcolor(white)) 
		graphregion(fcolor(white)) 
		lwidth(medthick) 
		lcolor(black)
		xline(58, lcolor(black) lwidth(medthick))
		text(.005 60 "Not eligible", placement(e) box)
		text(.005 56 "Eligible", placement(w) box)
		text( 0 58 "58", placement(nw)) ;

*Figure 6.6 Participation in HISP, by Baseline Poverty Index
* (Note: RDD uses the random assignment scenario, which assumes full compliance)
	* The below line of code creates a simple graph.
	rdplot enrolled poverty_index if treatment_locality==1, c(58) p(1) numbinl(58) numbinr(42) 

	*The below lines of code allow you to reproduce figure 6.6 exactly as depicted in the book
	#delimit;
	graph twoway scatter enrolled poverty_index , 
		title("") 
		ylabel(, angle(horizontal)) 
		ytitle("Participation rate in HISP") 
		xtitle("Baseline poverty index (20-100)") 
		plotregion(fcolor(white)) 
		graphregion(fcolor(white)) 
		xline(58, lcolor(black) lwidth(medthick))
		msize(medium) mcolor(black)
		text(.5 60 "Not eligible", placement(e) box)
		text(.5 56 "Eligible", placement(w) box);
	#delimit cr

*Figure 6.6 Poverty Index and Health Expenditures, HISP, Two Years Later 

/*Normalize the poverty index at 0*/ 
gen poverty_index_left=poverty_index-58 if poverty_index<=58 
	replace poverty_index_left=0 if poverty_index>58
gen poverty_index_right=poverty_index-58 if poverty_index>58 
	replace poverty_index_right=0 if poverty_index<=58

reg health_expenditures poverty_index_left poverty_index_right eligible if round ==1
predict he_pred1

	* The below line of code creates a simple graph.
	graph7 he_pred1 poverty_index if round ==1

	*The below lines of code allow you to reproduce figure 6.6 exactly as depicted in the book
	#delimit ;
	graph twoway scatter health_expenditures poverty_index if round==1 & treatment_locality==1 & health_expenditures<60, msize(vtiny) mcolor(black)||
	scatter he_pred1 poverty_index if round==1 , msize(medium) mcolor(black) msymbol(O) ||
	pcarrowi 20 59 10 59, msize(thick) mlwidth(thick) mcolor(black) lwidth(medthick) lcolor(black) ||, 
	title("")
	ytitle("Health expenditures ($)")
	xtitle("Baseline poverty index (20-100)") 
	ylabel(, angle(horizontal)) 
	plotregion(fcolor(white)) 
	graphregion(fcolor(white)) 
	xline(58, lcolor(black) lwidth(medthick))
	text(55 60 "Not eligible", placement(e) box)
	text(55 56 "Eligible", placement(w) box)
	text(21 55 "A", placement(w) box)
	text(7 60 "B", placement(e) box)
	text( -1 58 "58", placement(ne))
	legend (lab(3 "Estimated impact on health expenditures"));
	#delimit cr

*Table 6.1 Method 5-HISP Impact Using Regression Discontinuity Design (Regression Analysis) Multivariate Linear Regression
reg health_expenditures eligible poverty_index_left poverty_index_right $controls if round ==1

*----------------------------------------------------*
log close
