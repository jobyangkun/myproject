/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 4, Part 3  - Short  Snappers          */
/*  Do some Short  Snappers for practising.          */
/*                                                   */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-29 KY First Edition                    */
/*****************************************************/
/* Assignment 4, Part 3 -Short  Snappers */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 4 Part 3 - Snappers';
options orientation =landscape;
ods pdf file='a04p03-snappers.pdf' style=styles.printer;


/********************************************************************************************/
/* Question 1 */
/* Problems with input data */
data ds01;
   infile 'http://people.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign04/assign04-part03-ds01.txt' url
           firstobs=2 missover;
   length City $30 Temp $30;
   input City $ Temp $;
   Temp=upcase(Temp);
run;

proc print data=ds01;
   title2 'Temperature dataset';
run;


/* Dealing with censored values */
data newds01;
   set ds01;
   length City $30 Temp $30 type $10 detectlimit $30.;
   if index(Temp,"F")>0 then do;
      type = 'F';
      whereplus = index(Temp,"F");
      detectlimit = substr(Temp,1,whereplus-1);  /* right censored values */
      newTemp = input(detectlimit,f30.0);
   end;
   if index(Temp,"F") = 0 then do;
      Type = 'C';
      whereplus = index(Temp,"C");
      detectlimit = substr(Temp,1,whereplus-1); /* right censored values */
      newTemp = input(detectlimit,f30.0);
   end;
   drop detectlimit whereplus;
run;

proc print data=newds01;
run;


/* Convert  fahrenheit to celsius */
data newds01;
   set newds01;
   degrees_celsius=.;
   if type="F" then degrees_celsius=(newTemp-32)/9*5;
   if type="C" then degrees_celsius=newTemp;
   format degrees_celsius 7.1;
   drop newTemp;
run;

proc print data=newds01;
run;




/********************************************************************************************/
/* Question 2 */
/* Column input */
data ds02;
   infile 'http://people.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign04/assign04-part03-ds02.txt' url
          firstobs=3 missover;
   length make $5 model $7 miles_per_gallon $2 weight $4 price $4 ;
   input make $ 1-5 model $ 6-12 miles_per_gallon $ 13-14 weight $ 15-18 price $19-22;
run;

proc print data=ds02;
   title2 'Dataset of Question 2';
run;




/********************************************************************************************/
/* Question 3 */
/* Split-Apply-Combine (SAC) paradigm */
data accident_info;
  infile 'road-accidents-2010.csv' dlm=',' dsd missover firstobs=2;
  length AccidentId $20. dummy $1.;
  input AccidentID dummy dummy dummy dummy dummy
        Accident_Severity dummy dummy AccidentDate:ddmmyy10.;
  format AccidentDate yymmdd10.;
  length fatal $4;
  fatal = ' ';
  if accident_severity = 1      then fatal = 'yes';
  if accident_severity in (2,3) then fatal = 'no';
  month = month(AccidentDate);
  drop dummy;
run;

proc print data=accident_info(obs=10);
   title2 'few records of the accidents data';
run;


/* Check the codes */
proc tabulate data=accident_info missing;
   title2 'summary of codes used';
   class Accident_Severity fatal ;
   table Accident_severity, fatal*n*f=7.0;
run;


/* Do the analysis (genmod) */
proc genmod data=accident_info descending;
  title2 'examine if fatality rate is the same across different months';
  class month;
  model fatal = month/ dist=binomial link=logit type3;
  lsmeans month / cl diff ilink oddsratio;
  ods output lsmeans=mylsmeans;
run;

proc print data=mylsmeans;
   title2 'Estimated probability of fatality by months';
run;


/* draw a plot of the analysis */
proc sgplot data=mylsmeans noautolegend;
   title2 'Estimated probability of fatality by months along with 95% ci';
   scatter x=month y=mu;
   highlow x=month low=Lowermu high=uppermu;
   xaxis label='months' offsetmin=0.10 offsetmax=0.10 integer;
   yaxis label='probability of fatality with 95% CI';
run;


ods pdf close;
