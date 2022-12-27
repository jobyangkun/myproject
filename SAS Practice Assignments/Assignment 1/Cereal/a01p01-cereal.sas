/*****************************************************/                                                */
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2013                            */
/*  Assignment 1, Part 1  - Cereal                   */
/*                                                   */
/*  Read in the cereal dataset, do some simple plots,*/
/*  and then do a regression of number of calores    */
/*  vs. grams of fat.                                */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-08 KY First Edition                    */
/*****************************************************/

/* Assignment 1, Part 1 - Analysis of cereal data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

/* Label each page and put explanatory notes*/
title justify=left 'Yang, Kun 301178299'
      justify=center 'Assignment 1 - Part 1';
title2 'Cereal Dataset';

/* Convert the output to a PDF file*/
ods pdf file='a01p01-cereal.pdf' style=styles.printer;



/* Read the data into SAS*/
data cereal;
   infile 'cereal.csv' missover dlm=',' dsd firstobs=2;
   length name $30 manufacturer $30;
   input name $ manufacturer $ type $ calories protein fat sodium fiber carbo
         sugars shelf potassium vitamins weight cups_per_serving;
   attrib calories label='Calories' format=7.0;
   attrib protein label='protein' format=7.0;
   attrib fat label='fat' format=7.0;
   attrib sodium label='sodium' format=7.0;
   attrib fiber label='fiber' format=7.2;
   attrib carbo label='carbos' format=7.2;
   attrib sugars label='sugars' format=7.0;
   attrib shelf label='shelf' format=7.0;
   attrib potassium label='potassium' format=7.0;
   attrib vitamins label='vitamins' format=7.0;
   attrib weight label='weight' format=7.2;
   attrib cups_per_serving label='cups_per_serving' format=7.2;
run;

/* Print out the first few records*/
proc print data=cereal(obs=10);
   title2 'part of the raw data';
run;

/* deal with -1 indicating a missing value */
data new_cereal;
   set cereal;
   array variables(*) calories protein fat sodium fiber carbo sugars
         shelf potassium vitamins weight cups_per_serving;
   do i=1 to dim(variables);
      if variables(i) = -1 then variables(i) = .;
   end;
   drop i;
run;

/* Check a few data records after any fixups*/
proc print data=new_cereal(obs=10);
   title2 'part of the raw data after replacing -1 by missing values';
run;



/* Create a scatter plot matrix*/
proc sgscatter data=new_cereal;
  title2 "Scatterplot Matrix of parts of the variables";
  matrix calories protein fat carbos sugars sodium vitamins ;
run;

/* Create a scatter plot of calories vs fat*/
proc sgplot data=new_cereal;
   title2 'Scatter plot of calories vs grams of fat';
   scatter y=calories x=fat;
run;

proc sgplot data=new_cereal;
   title2 'Scatter plot of calories vs grams of fat';
   scatter y=calories x=fat / jitter;
   footnote "Jittering used to display points hidden by overplotting";
run;


/* Do a regression of calories against grams of fat.*/
footnote;
ods graphics on;
proc reg data=new_cereal;
   title2 'regresssion of calories vs. grams of fat';
   model calories = fat  / clb;
   ods output ParameterEstimates = MyEst;
run;
ods graphics off;

/* Add a regression line*/
proc print data=MyEst;
   title2 'Estimated coefficients from the fit';
run;


proc print data=MyEst label split=' ' noobs;
   title2 'Estimated coefficients from the fit';
   var variable estimate stderr probt lowerCL upperCL;
   format estimate stderr lowerCL upperCL 7.2;
   label lowerCL   ="95% CI lower bound";
   label upperCL   ="95% CI upper bound";
   label probt     ="P-value";
   label variable  ="Coefficient";
   label estimate  ="Est";
   label stderr    ="SE";
run;

ods pdf close;
