/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 2, Part 1  - Cereal                   */
/*                                                   */
/*  Read in the cereal dataset.Do tabulate, SGplot   */
/*  and test hypotheses about means.Examine if the   */
/*  average amount of sugar per serving varies by    */
/*  shelf height.                                    */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-14 KY First Edition                    */
/*****************************************************/

/* Assignment 2, Part 1 - Analysis of cereal data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 2 Part 1 - Cereal';
title2 'Cereal Analysis';

ods pdf file='a02p01-cereal.pdf' style=styles.printer;

data cereal;
   proc import file="cereal.csv" dbms=csv out=cereal replace;
run;

proc print data=cereal(obs=10);
   title3 'part of the raw data';
run;

/* deal with -1 indicating a missing value & add 'display_shelf' variable*/
data old_cereal;
   set cereal;
   array variables(*) calories protein fat sodium fiber carbo sugars shelf potass vitamins weight cups;
   do i=1 to dim(variables);
      if variables(i)=-1 then variables(i)=.;
   end;
   drop i;

data new_cereal;
   set old_cereal;
   display_shelf='unknow';
   if shelf=1 then display_shelf='low';
   if shelf=2 then display_shelf='mid';
   if shelf=3 then display_shelf='top';
run;

proc print data=new_cereal(obs=10);
   title3 "part of the raw data after replacing -1 by missing values & add 'display_shelf' variable";
run;



/* Create a tabulate */
proc tabulate data=new_cereal;
   title3 'basic statistics';
   class display_shelf;
   var sugars;
   table display_shelf='Shelf', sugars*(n*f=5.0 mean*f=7.1 std*f=7.1);
run;


/* Create the dot plots of sugars per serving vs shelf */
Proc SGplot data=new_cereal;
   title3 'basic plot WITHOUT jittering';
   scatter x=display_shelf y=sugars;
   xaxis label='Shelf' offsetmax=0.10 offsetmin=0.10;
   yaxis label="Sugar/serving (g)";
run;

Proc SGplot data=new_cereal;
   title3 'basic plot WITH jittering';
   scatter x=display_shelf y=sugars / jitter;
   xaxis label='Shelf' offsetmax=0.10 offsetmin=0.10;
   yaxis label="Sugar/serving (g)";
run;


/* Create the box-plots */
Proc SGplot data=new_cereal;
   title3 'Vertical box-plots WITHOUT notches';
   vbox sugars / category=display_shelf ;
   xaxis label='Shelf' offsetmax=0.10 offsetmin=0.10;
   yaxis label="Sugar/serving (g)";
run;

Proc SGplot data=new_cereal;
   title3 'Vertical box-plots WITH notches';
   vbox sugars / category=display_shelf notches ;
   xaxis label='Shelf' offsetmax=0.10 offsetmin=0.10;
   yaxis label="Sugar/serving (g)";
run;

/* do a formal hypothesis test */
proc glm data=new_cereal PLOTS=(DIAGNOSTICS RESIDUALS);
   title3 'does shelf height affect MEAN amount of sugar per serving';
   class display_shelf;
   model sugars = display_shelf;
   lsmeans display_shelf / diff cl adjust=tukey lines;
   ods output LSmeanCL = mymeansCL;
run;

proc print data=mymeansCL;
   title3 'raw table';
run;

ods rtf file='mymeansCL.rtf';
proc print data=mymeansCL label split=' ';
   title3 'Estimated mean amount of sugar/serving by shelf location along with 95% CI';
   var display_shelf LowerCL LSMean UpperCL ;
   label display_shelf="shelf";
   attrib LSMean label="Estimated mean amount" format=7.1;
   attrib UpperCL label="95% CI Upper bound" format=7.1;
   attrib LowerCL label="95% CI lower bound" format=7.1;
run;
ods rtf close;


/* Create the sgplot */
ods rtf file='sgplot.rtf';
proc sgplot data=mymeansCL noautolegend;
   title3 'Estimated mean amount of sugar/serving by shelf location along with 95% CI';
   scatter x=display_shelf y=LSMean;
   highlow x=display_shelf low=LowerCL high=UpperCL;
   xaxis label='Shelf';
   yaxis label="Sugar/serving (g) - mean and 95% ci";
run;
ods rtf close;

ods pdf close;
