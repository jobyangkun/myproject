/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 2, Part 3  - Nests                    */
/*                                                   */
/*  Read in the Nest dataset.Analyze the butts       */
/*  present, the butts weight and the number         */
/*  of mites among the species. Finally, analyze the */
/*  relationship between the butts weights           */
/*  and the number of mites.                         */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-15 KY First Edition                    */
/*****************************************************/

/* Assignment 2, Part 3 - Analysis of Nests data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 2 Part 3 - Nests';
title2 'Nests Analysis';

ods pdf file='a02p03-nests.pdf' style=styles.printer;

proc import file="nests.xls" dbms=xls out=nestinfo replace;
   sheet = 'Correlational';
   guessingrows = 9999;
run;

proc print data=nestinfo(obs=10);
   title2 'part of the raw data';
run;

/* Summary of NO. of nests by species and nest content */
proc tabulate data=nestinfo;
   title2 'Summary of number of nests by species and nest content';
   class species Nest_content;
   table Nest_content, species*n*f=5.0;
run;

/* Summary add the buttspresent */
data nestinfo;
   set nestinfo;
   length ButtsPresent $4;
   buttspresent = 'no';
   if Butts_weight>0 then buttspresent ='yes';
run;

proc print data=nestinfo(obs=10);
   title2 'part of the nestinfo WITH buttspresent';
run;




/* Analysis of the proportion of buttspresent FOR EACH species. */
/* sort by species and create the table of the proportion FOR EACH individual species */
proc sort data=nestinfo out=nestinfo;by Species;
run;

Proc freq data=nestinfo;
   by Species;
   table ButtsPresent/binomial(level='yes');
   output out=nestprop binomial;
   title2 'the number & the proportion of ButtsPresent FOR EACH individual species';
run;
proc print data=nestprop;run;

/* Create the sgplot */
proc sgplot data=nestprop noautolegend;
   title2 'the estimated proportion of nests with butts along with the 95% CI';
   scatter y=_BIN_ x=Species;
   highlow x=Species low=XL_BIN high=XU_BIN;
   yaxis label="Proportion of nests with butts (Yes:No)";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Species';
run;

/* Conduct a chi square test for equal proportions across species */
proc freq data=nestinfo;
   title2 'chi square test for equal proportions';
   table Species Species*buttspresent / chisq nocol nopercent;
run;




/* Analysis of the mean butt weight in nest of each species */
/* find the mean butt weight */
proc univariate data=nestinfo cibasic;
   by Species;
   var Butts_weight;
   ods output basicintervals=mycibuttweight;
   title2 'the mean butt weight';
run;
proc print data=mycibuttweight;run;

data nestinfo;/*eliminate null data */
   set nestinfo;
   if Butts_weight<0 then delete;
run;

/* create plot for the mean butt weight in nest of each species */
proc sgplot data=mycibuttweight noautolegend;
   title2 'the mean butt weight in nest of each species (and 95% CI) ';
   where Parameter='Mean';
   scatter y=Estimate x=Species;
   highlow x=Species low=lowercl high=uppercl;
   yaxis label="the mean butt weight";
   xaxis offsetmin=0.05 offsetmax=0.05 label='species';
run;

/* the two-sample t-test */
proc ttest data=nestinfo;
   title2 'comparison of mean butt weights';
   class Species;
   var Butts_weight;
run;




/* Analysis of the mean NUMBER OF MITES  */
/* univariate for No. of mites */
proc univariate data=nestinfo cibasic;
   title2 'the mean of No. of mites';
   by Species;
   var Number_of_mites;
   ods output basicintervals=mycinoofmi;
run;

/* Proc Univariate, SGplot, and Proc Univariate for the NUMBER OF MITES */
proc sgplot data=mycinoofmi noautolegend;
   title2 'the mean of No. of mites of each species (and 95% CI) ';
   where Parameter='Mean';
   scatter y=Estimate x=Species;
   highlow x=Species low=lowercl high=uppercl;
   yaxis label="the mean of No. of mites";
   xaxis offsetmin=0.05 offsetmax=0.05 label='species';
run;




/* Analysis of the number of mites vs weight of butts  */
/* create the scatter plot */
proc sgplot data=nestinfo;
   title2 'number of mites vs weight of butts';
   scatter x=Butts_weight y=Number_of_mites / group=Species;
run;

/* create the scatter plot with pbspline */
proc sgplot data=nestinfo;
   title2 'number of mites vs weight of butts with pbspline';
   pbspline x=Butts_weight y=Number_of_mites / group=Species;
   yaxis label="number of mites";
   xaxis label='butt weight';
run;

/* fit a regression line of log(parasites) vs. butt weight. */
data nestinfo;
   set nestinfo;
   logmites=log(Number_of_mites);
Run;

proc reg data=nestinfo;
   title2 'regression of log(number of mites) vs butt weight';
   model logmites = Butts_weight;
   output out=modelfit pred=estmean_log lclm=lclm_log uclm=uclm_log;
run;


/* ANTI-LOG the results */
data modelfit;
   set modelfit;
   estmean = exp(estmean_log);
   estmean_lcl = exp(lclm_log);
   estmean_ucl = exp(uclm_log);
run;
proc print data=modelfit(obs=10);
   title2 'modelfit';
run;

/* Fitted regression plot */
ods rtf file='sgplot.rtf';
proc sort data=modelfit; by butts_weight; run;
   proc sgplot data=modelfit;
   title2 'Fitted regression line of log(number mites) vs butt weight on the ANTI-log scale';
   band x=Butts_weight upper=estmean_ucl lower=estmean_lcl;
   scatter x=Butts_weight y=number_of_mites / group=species;
   series x=Butts_weight y=estmean;
run;
ods rtf close;
ods pdf close;
