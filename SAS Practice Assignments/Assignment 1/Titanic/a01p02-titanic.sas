/*****************************************************/                                                */
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2013                            */
/*  Assignment 1, Part 2  - Titanic                  */
/*                                                   */
/*  Read in the titanic dataset, create a summary    */
/*  table,and then extract the odds ratios           */
/*  and do the plot.                                 */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-09 KY First Edition                    */
/*****************************************************/

/* Assignment 1, Part 2 - Analysis of Titanic data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

/* Label each page and put explanatory notes */
title justify=left 'Yang, Kun 301178299'
      justify=center 'Assignment 1 - Part 2';
title2 'Titanic Dataset';

/* Convert the output to a PDF file */
ods pdf file='a01p02-titanic.pdf' style=styles.printer;


/* Read the data into SAS */
data titanic;
   infile 'http://www.statsci.org/data/general/titanic.txt' url firstobs=2 dsd delimiter='09'x;
   length Name $100;
   input Name $ PClass $ Age $ Sex $ Survived;
run;

/* Print out the first few records */
proc print data=titanic(obs=10);
   title2 'part of the raw data';
run;


/* Create a summary table */
proc tabulate data=titanic;
   title2 'Table of the number of survivors given passenger class and sex';
   class PClass Sex Survived;
   table PClass*Sex, Survived;
run;

proc tabulate data=titanic;
   title2 'Table of the proportion of survival given passenger class and sex';
   class PClass Sex;
   var Survived;
   table PClass*Sex, Survived*mean*f=7.3;
run;


/* Use  ODS to extract the odds ratios */
proc freq data=titanic;
   by pclass;
   table sex*survived / relrisk;
   ods output RelativeRisks=myoddsratio;
run;

proc print data=myoddsratio;
   title2 'Odds ratio of Death for females vs males by passenger class';
run;


/* Use SGplot to plot the odds ratio for each passager class */
proc sgplot data=myoddsratio noautolegend;
   title3 'Comparison of ODDS ratio (and 95% confidence intervals) among passenger classes';
   where studytype='Case-Control (Odds Ratio)';
   scatter y=Value x=PClass;
   highlow x=PClass low=LowerCL high=UpperCL;
   yaxis label="Odds ratio of Death (female:male)";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Passenger Class';
run;

ods pdf close;
