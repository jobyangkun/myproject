/*****************************************************/
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2013                            */
/*  Assignment 5, Part 1  - Cereal                   */
/*                                                   */
/*  Read in the cereal dataset, create model space   */
/*  then do simulation and regrssion. check if the   */
/*  estimators unbiased.                             */
/*                                                   */
/*  Change log:                                      */
/*    2015-02-04 KY First Edition                    */
/*****************************************************/

/* Assignment 5, Part 1 - Analysis of cereal data */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

title justify=left 'Yang, Kun 301178299' justify=center 'Assignment 5 Part 1 - Cereal';
title2 'Cereal Analysis';

options orientation=landscape;
ods pdf file='a05p01-cereal.pdf' style=styles.printer;


/* input cereal dataset */
data cereal;
   infile 'cereal.csv' missover dlm=',' dsd firstobs=2;
   length name $30 manufacturer $30 type $8 ;
   input name $ manufacturer $ type $ calories protein
         fat sodium fiber complex_carbs sugars shelf potassium vit_rda weight cups_per_serving;
   array variables(*) calories protein fat sodium fiber complex_carbs sugars shelf
         potassium vit_rda weight cups_per_serving;
   do i=1 to dim(variables);
      if variables(i) = -1 then variables(i) = .;
   end;
   drop i;
   attrib calories label='Calories' format=7.0;
run;

proc print data=cereal(obs=10);
   title3 'part of the raw data after replacing -1 by missing values';
run;


/* create the model space */
data ModelSpace;
   set cereal;
   keep fat protein complex_carbs sugars;
run;

proc print data=ModelSpace(obs=10);
   title3 'part of the model space';
run;



/* create simulation program to investigate calories/serving when distribution is normal */
%let beta_0=0;
%let beta_fat=9;
%let beta_protein=4;
%let beta_complex_carbs=4;
%let beta_sugars=4;
%let sigma=5;
%let nsim=1000;

data simdata_no_violations;
   call streaminit(314159);
   do sim=1 to &nsim;
        do mycereal=1 to ncereals;
            set modelspace point=mycereal nobs=ncereals;
            mu_calories=&beta_0 +
                        &beta_fat*fat +
                        &beta_protein*protein +
                        &beta_complex_carbs*complex_carbs +
                        &beta_sugars*sugars;
            epsilon=rand('normal',0,&sigma);
            calories=mu_calories+epsilon;
            output;
        end;
   end;
   stop;
run;

proc print data=simdata_no_violations(obs=10);
   title3 'part of the simulated data';
run;


/* create regression for each set of simulated data when distribution is normal */
proc reg data=simdata_no_violations outest=MyEst_no_violation noprint;
   by sim;
   model calories=fat protein complex_carbs sugars;
run;

proc print data=MyEst_no_violation(obs=10);
   title3 'part of the coeficients for each set of simulated dataset when distribution is normal';
run;


/* check if the estimators unbiased by the sampling distribution of the estimated coeficients over simulation */
proc univariate data=MyEst_no_violation
   location=&beta_0 &beta_fat &beta_protein
            &beta_complex_carbs &beta_sugars;
   var intercept fat protein complex_carbs sugars;
   histogram / normal;
run;


/* create the histogram for the sampling distribution of the estimated */
/* slope associated with fat when normality is satisfied */
ods rtf file='samdist_of_fatslope.rtf';
proc sgplot data=MyEst_no_violation;
   title3 'the sampling distribution of the estimated slope associated with fat when normality is satisfied';
   histogram fat;
   density fat / type=kernel;
   refline &beta_fat / axis=x lineattrs=(thickness=10) label='Population slope';
   xaxis label='the estimated slope associated with fat' MIN=6  MAX=11 ;
   yaxis label='predicted values' ;
run;
ods rtf close;





/* create simulation program to investigate calories/serving when distribution is not normal */
data simdata_violations;
   call streaminit(314159);
   do sim=1 to &nsim;
      do mycereal=1 to ncereals;
         set modelspace point=mycereal nobs=ncereals;
         mu_calories=&beta_0 +
         &beta_fat*fat +
         &beta_protein*protein +
         &beta_complex_carbs*complex_carbs +
         &beta_sugars*sugars;
         W = rand('lognormal');
         epsilon = (W - exp(0.5))/ sqrt ((exp(1)-1)* exp(1));
         calories = mu_calories + epsilon;
         output;
      end;
   end;
   stop;
run;
proc print data=simdata_violations(obs=10);
   title3 'part of the simulated data';
run;


/* create regression for each set of simulated data when distribution is not normal */
proc reg data=simdata_violations outest=MyEst_violation noprint;
   by sim;
   model calories=fat protein complex_carbs sugars;
run;
proc print data=MyEst_violation(obs=10);
   title3 'part of the coeficients for each set of simulated dataset when distribution is not normal';
run;


/* check the sampling distribution of the estimated coeficients over simulation */
proc univariate data=MyEst_violation
   location=&beta_0 &beta_fat &beta_protein
            &beta_complex_carbs &beta_sugars;
   var intercept fat protein complex_carbs sugars;
   histogram / normal;
run;


/* create the histogram for the sampling distribution of the estimated */
/* slope associated with fat when normality is not satisfied */
ods rtf file='samdist_of_fatslope_no.rtf';
proc sgplot data=MyEst_violation;
   title3 'the sampling distribution of the estimated slope associated with fat when normality is not satisfied';
   histogram fat;
   density fat / type=kernel;
   refline &beta_fat / axis=x lineattrs=(thickness=10) label='Population slope';
   xaxis label='the estimated slope associated with fat' MIN=6  MAX=11 ;
   yaxis label='predicted values' ;
run;
ods rtf close;



ods pdf close;
