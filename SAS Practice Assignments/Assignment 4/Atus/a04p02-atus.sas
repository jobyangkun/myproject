/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 4, Part 2  - ATUS Analysis            */
/*  Read in the time of use dataset, find the average*/
/* time of tv watching for each year-sex conbination.*/
/*  test if the means are same for different sex     */
/*  or age.                                          */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-29 KY First Edition                    */
/*****************************************************/
/* Assignment 4, Part 2 - ATUS Analysis */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote ' ';

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 4 Part 2 - ATUS';
title2 'American Time of Use Study';

options orientation=landscape;
ods pdf file='a04p02-atus.pdf' style=styles.printer;

%include 'atussum_0313.sas';




/* Add total time watching TV and indicator of watching TV */
data atussum_0313;
   set atussum_0313;
   totaltv = t120304 + t120303; /* total time watching TV (min) */
   watchtv = 0;  /* indicator variable */
   if totaltv >0 then watchtv = 1;
run;

/* Add categorical variable of sex */
data atussum_0313;
   length sex $30;
   set atussum_0313;
   sex ='.';
   if tesex =1 then sex = 'male';
   if tesex =2 then sex= 'female';
run;

/* Add categorical variable of age */
data atussum_0313;
   length age $30;
   set atussum_0313;
   age ='.';
   if teage <=25 then age = '25 year or less';
   if teage >=26 and teage <=35 then age= '26 to 35 years or age';
   if teage >=36 and teage <=50 then age = '36 to 50 years or age';
   if teage >=51 and teage <=65 then age = '51 to 65 years or age';
   if teage >65  then age='65+ years of age';
run;

proc print data=atussum_0313(obs=10);
   title2 'part of the raw data';
   var TUCASEID t120304 t120303 totaltv watchtv tesex sex teage age;
run;




/* use sgplot or tabulate to check the recoding */

/* Check sex variable */
proc tabulate data=atussum_0313 missing;
   class tesex sex;
   table tesex,sex*n*f=6.0 ;
run;

/* Check age variable */
proc sgplot data=atussum_0313;
   scatter y=teage x=age;
run;




/* Compute the mean time spent watching TV  over time for each sex */
proc sort data=atussum_0313; by tuyear sex; run;
proc means data=atussum_0313 noprint;
  by tuyear sex;
  var totaltv watchtv;  /* TV watching and indicator*/
  weight TUFNWGTP;
  output out=meantv
         mean=meantv  proptv;
run;

proc print data=meantv;
   title2 'mean time spent watching TV  over time for each sex';
run;

/* Plot the Mean TV of population watching over time for each sex */
proc sort data=meantv; by sex tuyear; run;
ods rtf file='part2.rtf';
proc sgplot data=meantv noautolegend;
   title2 'Mean TV of population watching over time for each sex';
   scatter x=tuyear y=meantv  ;
   series  x=tuyear y=meantv / group=sex  ;
   reg     x=tuyear y=meantv / group=sex ;
   xaxis label='Year' offsetmin=.05 offsetmax=.05 integer;
   yaxis label='Mean total TV watching (minutes/day) over time for each sex';
run;
ods rtf close;




/* test if the slope is the same for males and females */
proc glm data=meantv;
   title2 'test if the slope is the same for males and females';
   class sex;
   model meantv = sex tuyear sex*tuyear;
run;




/* test if the slope is the same for different age groups */
proc sort data=atussum_0313; by tuyear age; run;
proc means data=atussum_0313 noprint;
  by tuyear age;
  var totaltv watchtv;
  weight TUFNWGTP;
  output out=meantv2
         mean=meantv  proptv;
run;

proc print data=meantv2;
   title2 'mean time spent watching TV for each year-age combination';
run;

proc glm data=meantv2;
   title2 'test if the slope is the same for different age groups';
   class age;
   model meantv = age tuyear age*tuyear;
run;


ods pdf close;
