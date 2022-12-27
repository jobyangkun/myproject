/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2013                            */
/*  Assignment 4, Part 1  - Cereal                   */
/*                                                   */
/*  Read in the cereal dataset, do bootstrapping to  */
/*  estimate SE and  CI of standard diviation and    */
/*  Gini standard deviation.*/                       */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-29 KY First Edition                    */
/*****************************************************/

/* Assignment 4, Part 1 - Analysis of cereal data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 4 Part 1 - Cereal';
title2 'Cereal Analysis';

options orientation=landscape;
ods pdf file='a04p01-cereal.pdf' style=styles.printer;

data cereal;
   infile 'cereal.csv' missover dlm=',' dsd firstobs=2;
   length name $30 manufacturer $30 type $8 ;
   input name $ manufacturer $ type $ calories protein
         fat sodium fiber complex_carbs sugars shelf potassium vit_rda weight cups_per_serving;
   attrib calories label='Calories' format=7.0;
run;
proc print data=cereal(obs=10);
   title3 'part of the raw data';
run;

data cereal; /* deal with missing values */
   set cereal; /*scan over the variables for the -1 missing value */
   array variables(*) calories protein fat sodium fiber complex_carbs sugars shelf
         potassium vit_rda weight cups_per_serving;
   do i=1 to dim(variables);
      if variables(i) = -1 then variables(i) = .;
   end;
   drop i;
run;
proc print data=cereal(obs=10);
   title3 'part of the raw data after replacing -1 by missing values';
run;

/* Some basic statistics */
proc univariate data=cereal robustscale;
   title2 'Basic statistics';
   var calories;
run;



/* survey select with 5 replicates */
%let nboot=5; /* number of bootstrap reps */
proc surveyselect data=cereal out=bootsample
   method=urs outhits rep=&nboot samprate=1 seed=2342332;
   title2 'Survey Select with 5 replicates';
run;
proc print data=bootsample(obs=20);
run;

proc tabulate data=bootsample missing;
   class replicate;
   table replicate, n*f=5.0;
run;

proc univariate data=bootsample noprint;
   by replicate;
   var calories;
   output out=bootstat
   mean=mean_calories
   stddev=std_calories
   std_gini=gini_std_calories;
run;
proc print data=bootstat(obs=10);
run;

proc univariate data=bootstat noprint;
   var mean_calories;
   output out=SE_boot_mean
   stddev=SE_boot_mean
   pctlpts=2.5 97.5 pctlpre=cl_mean;
run;
proc print data=SE_boot_mean;
run;




/* survey select with 1000 replicates */
%let nboot=1000; /* number of bootstrap reps */
proc surveyselect data=cereal out=bootsample
   method=urs outhits rep=&nboot samprate=1 seed=2342332;
   title2 'Survey Select with 1000 replicates';
run;

proc univariate data=bootsample noprint;
   by replicate;
   var calories;
   output out=bootstat
   mean=mean_calories
   stddev=std_calories
   std_gini=gini_std_calories;
run;
proc print data=bootstat(obs=10);
run;

proc univariate data=bootstat noprint;
   var mean_calories;
   output out=SE_boot_mean
   stddev=SE_boot_mean
   pctlpts=2.5 97.5 pctlpre=cl_mean;
run;
proc print data=SE_boot_mean;
run;




/* histogram of the sample mean of calories */
proc sgplot data=bootstat;
   title2 'The value of the sample mean of calories';
   histogram mean_calories;
   density mean_calories / type=kernel;
run;

data sgannods; /* Add CI to histogram */
   set SE_boot_mean;
   function= 'line';
   y1space='datapercent'; x1space='datavalue';
   y2space='datapercent'; x2space='datavalue';
   x1=cl_mean2_5; y1=0; x2=cl_mean2_5; y2=50; output;
   x1=cl_mean97_5; y1=0; x2=cl_mean97_5; y2=50; output;
run;
proc print data=sgannods;
title2 'annotation instructions';
run;

proc sgplot data=bootstat sganno=sgannods;  /* histogram With SE of mean */
   title2 'The sample mean of calories';
   histogram mean_calories;
   density mean_calories / type=kernel;
run;




/* histogram of the sample standard deviation of calories */
proc univariate data=bootstat noprint;
   var std_calories;
   output out=SE_boot_std
   stddev=SE_boot_std
   pctlpts=2.5 97.5 pctlpre=cl_std;
run;
proc print data=SE_boot_std;
run;

data sgannods2;
   set SE_boot_std;
   function= 'line';
   y1space='datapercent'; x1space='datavalue';
   y2space='datapercent'; x2space='datavalue';
   x1=cl_std2_5; y1=0; x2=cl_std2_5; y2=50; output;
   x1=cl_std97_5; y1=0; x2=cl_std97_5; y2=50; output;
run;
proc print data=sgannods2;
title2 'annotation instructions of sample standard deviation';
run;

proc sgplot data=bootstat sganno=sgannods2;  /* histogram With CI of sample standard deviation */
   title2 'The sample standard deviation of calories';
   histogram std_calories;
   density std_calories / type=kernel;
run;




/* histogram of the Gini standard deviation of calories */
proc univariate data=bootstat noprint;
   var gini_std_calories;
   output out=SE_boot_gini_std
   stddev=SE_boot_std
   pctlpts=2.5 97.5 pctlpre=cl_gini_std;
run;
proc print data=SE_boot_gini_std;
run;

data sgannods3;
   set SE_boot_gini_std;
   function= 'line';
   y1space='datapercent'; x1space='datavalue';
   y2space='datapercent'; x2space='datavalue';
   x1=cl_gini_std2_5; y1=0; x2=cl_gini_std2_5; y2=50; output;
   x1=cl_gini_std97_5; y1=0; x2=cl_gini_std97_5; y2=50; output;
run;
proc print data=sgannods3;
title2 'annotation instructions of Gini standard deviation';
run;

proc sgplot data=bootstat sganno=sgannods3;  /* histogram With CI of Gini standard deviation */
   title2 'The Gini standard deviation of calories';
   histogram gini_std_calories;
   density gini_std_calories / type=kernel;
run;


ods pdf close;
