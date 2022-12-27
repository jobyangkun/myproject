use "dataset1.dta", clear
list
save dataset1


use "dataset2.dta", clear
save dataset2

dir*.dta

use dataset1
append using dataset2

use "fugure229.dta", clear
list
drop v1_15 v2_15
save data14

use "fugure229.dta", clear
drop v1_14 v2_14
save data15


use data15
list
use data14
list

merge 1:1 id using data15
tabulate _merge
list


reshape long v1_ v2_, i(id) j(year)
list

drop _merge
reshape wide
list




log using zach1
use "hh_98.dta", clear
des
su
tab milk
*hist milk

hist hhland
sum hhland,d

sum exptot,d
hist exptot

set more off

log using zach1
log close






