//do-file for "Simple regression"

use flat.dta,clear 

reg flat_price floor_size //estimates the simple regression model

/*you may get different display of your numbers in figure 4
depending on your formatting, to get the exact same outcome
in figure 4, you can use the following formatting before
running the regression analysis again*/
set cformat %9.1f
set sformat %7.4f
set pformat %4.3f
reg flat_price floor_size 

graph twoway (scatter flat_price floor_size) (lfit flat_price floor_size) //draws figure 5

margins, at(floor_size=(60(20)220)) //computes predicted mean-Y at chosen floor size values
marginsplot //run right after margins command drawing figure 7



 
