/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2015                            */
/*  Assignment 5, Part 3  - Interesting              */
/*  Do some Interesting plots for practising.        */
/*                                                   */
/*                                                   */
/*  Change log:                                      */
/*    2015-02-05 KY First Edition                    */
/*****************************************************/
/* Assignment 5, Part 3 -Short  Snappers */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 5 Part 3 - interesting';
options orientation =landscape;
ods pdf file='a05p03-interesting.pdf' style=styles.printer;


/********************************************************************************************/
/* PART 1 */
data ds01;
   infile 'interesting1.csv' dlm=',' dsd missover firstobs=2;
   input y x1 x2 x3 x4 x5 x6;
run;

proc print data=ds01(obs=10);
   title2 'interesting 1 dataset';
run;


/* create a scatterplot matrix of all variables */
proc sgscatter data=ds01;
  title2 "Scatterplot Matrix for interesting 1";
  matrix y x1 x2 x3 x4 x5 x6;
run;


/* do a multiple regression of Y vs. all of the X variables. */
proc reg data=ds01 noprint;
   title2 'Regresssion of y vs. x1 x2 x3 x4 x5 x6';
   model y=x1 x2 x3 x4 x5 x6 / r p;
   output out=interesting_pred p=predicted r=residual;
run;

proc print data=interesting_pred(obs=10)label;
   title2 'Predictions from interesting 1 dataset';
run;


/* Plot the residual against the predicted value */
proc sgplot data=interesting_pred noautolegend;
   title2 'the residual against the predicted value from interesting 1 dataset';
   scatter x=predicted y=residual;
   xaxis label='predicted values' MIN=-2  MAX=2 ;
   yaxis label='residuals';
run;





/********************************************************************************************/
/* PART 2 */
data ds02;
   infile 'interesting2.csv' dlm=',' dsd missover firstobs=2;
   input y x1 x2 x3 x4 x5 x6;
run;


/* do a multiple regression of Y vs. all of the X variables. */
proc reg data=ds02 noprint;
   title2 'Regresssion of y vs. x1 x2 x3 x4 x5 x6';
   model y=x1 x2 x3 x4 x5 x6 / r p;
   output out=interesting_pred2 p=predicted r=residual;
run;

proc print data=interesting_pred2(obs=10)label;
   title2 'Predictions from interesting 2 dataset';
run;


/* Plot the residual against the predicted value */
proc sgplot data=interesting_pred2 noautolegend;
   title2 'the residual against the predicted value from interesting 2 dataset';
   scatter x=predicted y=residual;
   xaxis label='predicted values' MIN=-2  MAX=2 ;
   yaxis label='residuals';
run;





/********************************************************************************************/
/* PART 3 */
data ds03;
   infile 'interesting3.csv' dlm=',' dsd missover firstobs=2;
   input y x1 x2 x3 x4 x5 x6;
run;


/* do a multiple regression of Y vs. all of the X variables. */
proc reg data=ds03 noprint;
   title2 'Regresssion of y vs. x1 x2 x3 x4 x5 x6';
   model y=x1 x2 x3 x4 x5 x6 / r p;
   output out=interesting_pred3 p=predicted r=residual;
run;

proc print data=interesting_pred3(obs=10)label;
   title2 'Predictions from interesting 3 dataset';
run;


/* Plot the residual against the predicted value */
proc sgplot data=interesting_pred3 noautolegend;
   title2 'the residual against the predicted value from interesting 3 dataset';
   scatter x=predicted y=residual;
   xaxis label='predicted values' MIN=-2  MAX=2 ;
   yaxis label='residuals';
run;


ods pdf close;
