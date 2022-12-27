/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 5, Part 2  - Accidents                */
/*                                                   */
/*  Read in the accedents & vehicle dataset. Check   */
/*  the log-odds of fatality  and the probability    */
/*  of survival.                                     */
/*                                                   */
/*  Change log:                                      */
/*    2015-02-04 KY First Edition                    */
/*****************************************************/

/* Assignment 5, Part 2 - Analysis of accidents data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 5 Part 2 - Accidents';
title2 'Accidents Analysis';

ods pdf file='a05p02-accidents.pdf' style=styles.printer;


/* Read accidents data and add fatal,area,speed variables  */
data accident_info;
   infile 'road-accidents-2010.csv' dlm=',' dsd firstobs=2;
   length dummy $1 accidentid $20;
   input accidentid dummy dummy dummy dummy dummy
   accident_severity dummy dummy dummy dummy dummy
   dummy dummy dummy dummy dummy Speed_limit dummy
   dummy dummy dummy dummy dummy dummy dummy dummy
   dummy dummy Urban_or_Rural;
   drop dummy;
   length fatal $4 area $10 speed $10 ;
   speed='';
   if accident_severity=1 then fatal='yes';
   if accident_severity in (2,3) then fatal='no';
   area='';
   if Urban_or_Rural=1 then area='urban';
   if Urban_or_Rural=2 then area='rural';
   speed='';
   if 0<Speed_limit<=30 then speed='00+-30';
   if 30<Speed_limit<=50 then speed='30+-50';
   if Speed_limit>60 then speed='60+';
run;

proc print data=accident_info(obs=10) split="_";
   title2 'part of the accident_info data';
run;


/* check the recodes */
proc tabulate data=accident_info missing;
   title2 'check the recodes';
   class accident_severity fatal Urban_or_Rural area Speed_limit speed;
   table accident_severity, fatal*n*f=comma7.0;
   table Urban_or_Rural, area*n*f=comma7.0;
   table Speed_limit, speed*n*f=comma7.0;
run;

data accident_info;
   set accident_info;
   if fatal='' then delete;
   if area='' then delete;
   if speed='' then delete;
run;


/* look at log(odds) of fatality */
proc genmod data=accident_info descending;
   class area speed;
   model fatal = area speed area*speed / dist=multinomial type3;
   lsmeans area*speed / cl diff adjust=tukey;
   ods output lsmeans=myodds;
run;

ods rtf file='myodds.rtf';
proc print data=myodds label noobs;
   title2 'Estimated log-odds and 95% CIs';
   var Effect fatal area speed Estimate StdErr zValue Probz Alpha Lower Upper ;
   format Estimate StdErr zValue Lower Upper  6.2;
   label Estimate='log-odds' stderr='Standard Error' Lower='Lower limit 95% CI ' Upper='Upper limit 95% CI';
run;
ods rtf close;


/* plot the log-odds of fatality */
ods rtf file='myoddsplot.rtf';
proc sgplot data=myodds noautolegend;
   title2 'the estimated logodds of a fatality (along with a CI) by area and speed';
   scatter x=speed y=Estimate /datalabel=area group=area ;
   highlow x=speed low=Lower high=Upper;
   xaxis label='The Speed limit';
   yaxis label='The log-odds of fatality';
run;
ods rtf close;






/* Analyze the probability of survival */
proc genmod data=accident_info;
   class area speed;
   model fatal = area speed area*speed / dist=multinomial type3;
   lsmeans area*speed / cl diff ilink adjust=tukey;
   ods output lsmeans=myodds2;
run;


/* plot the probability of survival */
ods rtf file='myproportionplot.rtf';
proc sgplot data=myodds2 noautolegend;
   title2 'the estimated probability of survival (along with a CI) by area and speed';
   scatter x=speed y=Mu / datalabel=area group=area;
   highlow x=speed low=LowerMu high=UpperMu;
   xaxis label='The Speed limit';
   yaxis label='The probability of survival' offsetmin=0.10 offsetmax=0.10;
run;
ods rtf close;

ods pdf close;
