/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 3, Part 1  - snappers Analysis        */
/*  there are 4 parts of the different question.     */
/*                                                   */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-21 KY First Edition                    */
/*****************************************************/
/* Assignment 3, Part 1 - snappers Analysis */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 3 Part 1 - Snappers';
title2 'snappers Analysis';

ods pdf file='a03p01-snappers.pdf' style=styles.printer;

/* Question 1 */
/* Multiple records for each observation */
data ds01;
   infile 'http://www.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign03/assign03-part01-ds01.txt' url
   dlm=' ' missover firstobs=2;
   length name $70 sex $10  FavTvShow $70 FavSport $70;
   input Name$ Sex $  Birthday:yymmdd10./ Height $ FavTvShow $ FavSport $ SysBloodPres;
   format Birthday:yymmdd10.;
run;
proc print data=ds01;
   title2 'family members information';
run;

/* Question 2 */
/* a plot of side-by-side confidence intervals */
data ds02;
   infile 'http://www.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign03/assign03-part01-ds02.txt' url
   dlm=' ' missover firstobs=2;
   length faculty_name $70;
   input faculty_name $ sample_size mean_earnings standard_deviation;
run;
proc print data=ds02;
   title2 'information on summer earnings for students in different faculties';
run;


Proc SGplot


/* Question 3 */
/* Special numeric values */
data ds03;
   infile 'http://www.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign03/assign03-part01-ds03.txt' url
   dlm=' ' missover firstobs=1;
   length ccoliform $30.;
   input SampleDate:yymmdd10. ccoliform $;
   format SampleDate yymmdd10.;
run;
proc print data=ds03;
   title2 'Water quality data before fixups';
run;

data newds03;
   set ds03;
   length ccoliform detectlimit $30.;
   if index(ccoliform,"<") > 0 then do;  /* a ’<’ is found */
      lc_coliform = 'yes';
      whereless = index(ccoliform,"<");
      detectlimit = substr(ccoliform,whereless+1);
      coliform = input(detectlimit, f30.0);
   end;
   if index(ccoliform,"<") = 0 then do;  /* a ’<’ is NOT found */
      lc_coliform = 'no';
      coliform = input(ccoliform, f30.0);
   end;
run;
proc print data=newds03;
   title2 'Water quality data after fixups';
run;

proc tabulate data=newds03 missing;
    title2 'check your recoding';
   class ccoliform lc_coliform  ;
   table ccoliform,lc_coliform*n*f=5.0;
run;




data ds03;
   infile 'http://www.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign03/assign03-part01-ds03.txt' url
   dlm=' ' missover firstobs=1;
   length cturbidity $30.;
   input SampleDate:yymmdd10. dummy cturbidity $;
   format SampleDate yymmdd10.;
   drop dummy
run;
proc print data=ds03;
   title2 'Water quality data before fixups';
run;

data newds03;
   set ds03;
   if index(cturbidity,"<") > 0 then do; /* contains a < */
      lc_turbidity = 'yes';
      whereless = index(cturbidity,"<");
      detectlimit = substr(cturbidity,whereless+1);
      turbidity = input(detectlimit, f30.0);
   end;
   if index(cturbidity,"+") > 0 then do; /* contains a + */
      lc_turbidity = 'yes';
      whereless = index(cturbidity,"+");
      detectlimit = substr(cturbidity,whereless-7);
      turbidity = input(detectlimit, f30.0);
   end;
   if index(cturbidity,"<") = 0 AND
      index(cturbidity,"+") = 0 then do; /* no < or + */
      lc_turbidity = 'no';
      turbidity = input(cturbidity, f30.0);
   end;
run;
proc print data=newds03;
   title2 'Water quality data after fixups';
run;

proc tabulate data=newds03 missing;
    title2 'check your recoding';
   class turbidity lc_turbidity;
   table turbidity,lc_turbidity*n*f=5.0;
run;



/* Question 4 */
 /* Split-Apply-Combine */
data ds04;
   infile 'http://www.stat.sfu.ca/~cschwarz/Stat-340/Assignments/Assign03/assign03-part01-ds04.txt' url
   dlm=' ' missover firstobs=2;
   length sex $10 name $70;
   input year sex $ name $ grade;
run;
proc print data=ds04;
   title2 'final grades from two years of a course';
run;

Proc Means data=ds04 maxdec=1 alpha=0.5 clm mean;
   var grade;
   class year;
   title2 'mean & 95% Confidence Limits for final grades';
run;

/* conduct a Paired t-test */
data ds04;
   set ds04;
   female=.;
   if sex='f' then female=1;
   if sex='m' then female=0;
run;
proc print data=ds04;
   title2 'final grades from two years of a course';
run;

proc ttest data=ds04;
   paired female*grade ;
   title2 'Paired t-test analysis';
   ods output Ttests=ttest_output;
run;
proc print data=ttest_output;
   title2 'Ttests';
run;

ods pdf close;
