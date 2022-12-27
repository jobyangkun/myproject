use "workout1_data", clear
describe

ge gender = v01
replace gender=0 if v01==2
ge age = v02

sum age if gender==1
ge meanage1=39.83553 if gender ==1
sum age if gender==0
replace meanage1=38.06383  if gender ==0
codebook meanage

egen meanage2=mean(age), by (gender)
codebook meanage2


list v04
tabulate v04

recode v04 (1/3=1) (4/6=2) (7/9=3)
tabulate v04, nolabel
generate age2=age*age
gen nobs=_n
list nobs
sum age if gender==1 & v03==3
tab v03


egen stdage =sd(age), by (gender)
drop stdage
keep v01 v02 v03 v04 v05 v06 v07 gender age meanage1 meanage2 age2 nobs

tab v07
save test1
dir

drop if nobs==10
sum

clear 

use test1
tab v06 v07
sum v07

ge v07_nom=1 if v07=="yes"
replace v07_nom=0 if v07=="no"
tab v07_nom






