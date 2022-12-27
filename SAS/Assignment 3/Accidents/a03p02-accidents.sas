/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 3, Part 2  - Accidents                */
/*                                                   */
/*  Read in the accedents & vehicle dataset. Merge   */
/*  them together. Check if the fatality rates       */
/*  varies by the number of female drivers involved  */
/*  in the accident. Use 'proc genmod' method.       */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-21 KY First Edition                    */
/*****************************************************/

/* Assignment 3, Part 2 - Analysis of accidents data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 3 Part 2 - Accidents';
title2 'Accidents Analysis';

ods pdf file='a03p02-accidents.pdf' style=styles.printer;

/* Read accidents data */
data accident_info;
   infile 'road-accidents-2010.csv' dlm=',' dsd firstobs=2;
   length dummy $1 accidentid $20;
   input accidentid dummy dummy dummy dummy dummy
   accident_severity number_of_vehicles;
   drop dummy;
   if number_of_vehicles<2 then delete;
   if number_of_vehicles>2 then delete;
   fatal='no';
   if accident_severity=1 then fatal='yes';
run;

proc print data=accident_info(obs=10) split="_";
   title2 'part of the accident_info data';
run;

/* check recoding */
proc tabulate data=accident_info missing;
   class Accident_Severity fatal;
   table Accident_Severity, fatal*n*f=comma8.;
run;


/* Read vehicle data */
data vehicle_info;
   infile 'road-accidents-vehicles-2010.csv' dlm=',' dsd firstobs=2;
   length dummy $1 accidentid $20;
   input accidentid dummy dummy dummy dummy dummy
   dummy dummy dummy dummy dummy dummy dummy dummy
   sex_of_driver;
   drop dummy;
run;

data vehicle_info;
   set vehicle_info;
   female=.;
   if sex_of_driver=2 then female=1;
   if sex_of_driver=1 then female=0;
run;

proc print data=vehicle_info(obs=20) split="_";
   title2 'vehicle information data';
run;

/* check recoding */
proc tabulate data=vehicle_info missing;
   class sex_of_driver female;
   table sex_of_driver, female*n*f=comma8.;
run;


/* get one record per accident */
proc means data=vehicle_info noprint;
   by accidentid;
   var  female;
   output out=vehicle_summary n=ndriver sum(female)=nfemale
   nmiss(female)=nmisssex;
run;

proc print data=vehicle_summary(obs=10) split="_";
   title2 ' vehicle_summary dataset';
run;

/* reduced vehicle_summary dataset */
data vehicle_summary;
   set vehicle_summary;
   if ndriver>2 then delete;
   if ndriver<2 then delete;
   if nmisssex>0 then delete;
run;

proc print data=vehicle_summary(obs=10) split="_";
   title2 'reduced vehicle_summary dataset';
run;

/* merge the accident information and vehicle summary data */
proc sort data=accident_info; by accidentid; run;
proc sort data=vehicle_summary; by accidentid; run;
data mergeddata;
   merge accident_info vehicle_summary;
   by accidentid;
run;

proc print data=mergeddata(obs=20) split="_";
   title2 'Merged dataset';
run;

/* compute percentages in tables */
ods rtf file='mergeddata.rtf';
proc tabulate data=mergeddata missing;
   class nfemale fatal;
   table nfemale, n*f=comma8. fatal*f=comma8. fatal*pctn<fatal>*f=5.1;
run;
ods rtf close;



/* if the fatality rates varies by the number of female drivers involved in the accident */
proc genmod data=mergeddata descending;
   title2 'examine if fatality rate is the same across number of females';
   class nfemale;
   model fatal = nfemale/ dist=binomial link=logit type3;
   lsmeans nfemale / cl diff ilink oddsratio;
   ods output lsmeans=myodds;
run;

proc print data=fatalityrates;title2 'raw lsmeans output';run;

proc print data=myodds label split=' ' noobs;
   title2 'the estimated odds of a fatality by the number of females along with a CI';
   var nfemale Mu LowerMu UpperMu ;
   format Mu LowerMu UpperMu 7.1;
   label nfemale  ='the number of females';
   label Mu ='the estimated odds of a fatality';
   label LowerMu='Lower 95% cl';
   label UpperMu='Upper 95% cl';
run;

ods rtf file='myoddsplot.rtf';
proc sgplot data=myodds noautolegend;
   title2 'the estimated odds of a fatality (along with a CI) by the number of females';
   scatter x=nfemale y=Mu;
   highlow x=nfemale low=LowerMu high=UpperMu;
   xaxis label='the number of females';
   yaxis label='the estimated odds of a fatality';
run;
ods rtf close;

ods pdf close;
