/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 2, Part 2  - Accidents                */
/*                                                   */
/*  Read in the accedents dataset.Do tabulate,SGplot */
/*  and test hypotheses.Examine if the distribution  */
/*  of accidents varies by months or hours.          */
/*                                                   */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-15 KY First Edition                    */
/*****************************************************/

/* Assignment 2, Part 2 - Analysis of accidents data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 2 Part 2 - Accidents';
title2 'Accidents Analysis';

ods pdf file='a02p02-accidents.pdf' style=styles.printer;

data accidents;
   infile 'road-accidents-2010.csv' dlm=',' dsd firstobs=2;
   length AccidentID $20;
   input AccidentID $ Easting Northing Longitude Latitude Police
         Acc_Severity Vehicles Casualties AccidentDate:ddmmyy10. DayofWeek
         AccidentTime:hhmmss10. Auth_Dist Auth_HW $ st_RdCl
         st_RdNO Rd_Type Speed Junc_De Junc_Cont nd_RdCl
         nd_RdNO Hum_Cont Phyl_Faci Light Weather
         Rd_Surface Spec_Con Carr_Hazard Urb_Rur Pol_Attend Acc_Location $;
   attrib AccidentDate label='Accident Date' format=yymmdd10.;
   attrib AccidentTime label='Accident Time' format=hhmm5.;
run;
proc print data=accidents(obs=10);
   title2 'part of the raw data';
run;


/* Create a tabulate */
proc tabulate data=accidents missing;
   class Acc_Severity Rd_Type st_RdCl Light Urb_Rur;
   table Acc_Severity ALL, n*f=7.0;
   table Rd_Type ALL, n*f=7.0;
   table st_RdCl ALL, n*f=7.0;
   table Light ALL, n*f=7.0;
   table Urb_Rur ALL, n*f=7.0;
run;

proc sort data=accidents; by accidentdate; run;
proc means data=accidents noprint;
   by accidentDate;
   var accidentDate;
   output out=dailysummary n=naccidents;
run;
proc print data=dailysummary(obs=10);
   title2 'part of the daily summary';
run;


/* Create the scatter plots */
proc sgplot data=dailysummary;
   title2 'number of accidents by date';
   scatter y=naccidents x=AccidentDate;
   yaxis label="numbers of accidents";
   xaxis label='month';
run;

ods rtf file='loess.rtf';
proc sgplot data=dailysummary;/* Add the loess curve */
   title2 'number of accidents by date WITH loess curve';
   loess y=naccidents x=AccidentDate;
   yaxis label="numbers of accidents";
   xaxis label='month';
run;
ods rtf close;

/* Extracting information from dates */
data dailysummary;
   set dailysummary;
   month = month(AccidentDate);
   day = day(AccidentDate);
run;
proc print data=dailysummary(obs=10);
   title2 'Extracting information from dates';
run;

/* the mean & the std dev of the accidents/day for each month */
proc tabulate data=dailysummary;
   title2 'what is mean and std dev of daily accidents for each month';
   class month;
   var naccidents;
   table month, naccidents*(n*f=5.0 mean*f=7.1 std*f=7.1);
run;

/* do a glm hypothesis test */
proc glm data=dailysummary PLOTS=(DIAGNOSTICS RESIDUALS);
   title2 'is there a difference in the mean number of accidents by month?';
   class month;
   model naccidents = month;
   lsmeans month/ diff cl adjust=tukey lines;
   ods output lsmeancl = mymeanscl;
run;

/* Extracting information from mymeanscl */
ods rtf file='mymeanscl.rtf';
proc sgplot data=mymeansCL noautolegend;;
   title2 'Estimated mean number of accidents by month along with 95% CI';
   scatter x=month y=LSMean;
   highlow x=month low=LowerCL high=UpperCL;
   attrib LSMean label="Estimated mean number" format=7.1;
   attrib UpperCL label="95% CI Upper bound" format=7.1;
   attrib LowerCL label="95% CI lower bound" format=7.1;
run;
ods rtf close;


/* Extracting information from time */
data accidents;
   set accidents;
   hour = hour(AccidentTime);
   minute = minute(AccidentTime);
run;
proc print data=accidents(obs=10);
   title2 'Extracting information from time';
run;

/* Create a histogram of the minute of the hour for accidents  */
ods rtf file='accidents.rtf';
proc sgplot data=accidents;
   title2 'histogram of hour of accidents';
   histogram hour / binwidth=1;
   density hour /type=kernel;
run;
ods rtf close;

ods pdf close;
