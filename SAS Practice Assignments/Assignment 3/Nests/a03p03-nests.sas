/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 3, Part 3  - Nests                    */
/*                                                   */
/*  Read in the Nest dataset.Produce univeriate,     */
/*  Paired t-test and RCB analysis. Finally, merge   */
/*  two worksheet and do a regression of the         */
/*  difference in the number of ectoparasites        */
/*  attracted vs. the weight of butts.                */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-21 KY First Edition                    */
/*****************************************************/

/* Assignment 3, Part 3 - Analysis of Nests data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 3 Part 3 - Nests';
title2 'Nests Analysis';

ods pdf file='a03p03-nests.pdf' style=styles.printer;

proc import file="nests.xls" dbms=xls out=expinfo_long replace;
   sheet = 'Experimental';
   guessingrows = 9999;
run;

proc print data=expinfo_long(obs=20);
   title2 'part of the Experimental data';
run;

/* converting data from the LONG format to the WIDE format */
proc sort data=expinfo_long out=expinfo_long;by nest;run;
proc transpose data=expinfo_long out=expinfo_wide;
   by nest;
   var number_of_mites;
   id treatment;
run;

data expinfo_wide;
   set expinfo_wide;
   if nest=. then delete;
run;

proc print data=expinfo_wide(obs=10);
   title2 'the wide format data';
run;

/* Compute the difference in the number of ectoparasites attracted */
data expinfo_wide;
   set expinfo_wide;
   difference=experimental-control;
run;

proc sgplot data=expinfo_wide;
   title2 'distribution of the hour of accidents';
   histogram difference;
   density difference /type=kernel;
run;


/* estimate the difference in the means along with a se and a 95% cl */
/* Use Proc Univariate */
proc univariate data=expinfo_wide mu0=0 cibasic;
   id Nest;
   var difference;
   ods output basicintervals=mydiff;
   qqplot difference / normal;
   title2 'Univariate analysis';
run;
proc print data=mydiff;run;

/* conduct a Paired t-test */
proc ttest data=expinfo_wide;
   paired control*experimental;
   title2 'Paired t-test analysis';
run;

/* conduct GLM for RCB analysis */
proc glm data=expinfo_long plots=(residuals diagnostics);
   title2 'GLM for RCB analysis';
   class nest treatment;
   model number_of_mites=nest treatment;
   lsmeans treatment / cl diff adjust=tukey stderr ;
run;

/* merge two datasets */
proc import file="nests.xls" dbms=xls out=nestinfo replace;
   sheet='Correlational';
   guessingrows=9999;
run;

data mergeddata;
   merge nestinfo expinfo_wide;
   by nest;
run;

proc print data=mergeddata(obs=10);
   title2 'part of the merged data';
run;


/* scatterplot of the difference vs. the weight of butts */
proc sgplot data=mergeddata noautolegend;
   title2 'scatterplot of the difference vs. the weight of butts';
   scatter y=difference x=Butts_weight;
   yaxis label="difference between experimental and control";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Butts weight';
run;

/* Do a regression of the difference in the number of ectoparasites attracted */
/*vs. the weight of butts in the nests. */
proc reg data=mergeddata;
   title2 'regression of the difference in the number of ectoparasites attracted vs butt weight';
   model difference = butts_weight;
   output out=modelfit uclm=uclm lclm=lclm pred=estmean;
run;
proc print data=modelfit(obs=10);
run;

/* create a scatterplot */
ods rtf file='assign03-part3-nests.rtf';
proc sort data=modelfit; by butts_weight; run;
proc sgplot data=modelfit;
   title2 'Fitted regression line of the difference in the number of ectoparasites attracted vs butt weight';
   band    x=butts_weight upper=uclm lower=lclm;
   scatter x=butts_weight y=difference;
   series  x=butts_weight y=estmean;
run;
ods rtf close;

ods pdf close;
