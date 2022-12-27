use "test1",clear

tab v07
des v07

ge v07_nom=1 if v07=="Yes"
replace v07_nom=0 if v07=="No"

tab v07_nom
tab v07_nom v07

histogram v07_nom, discrete percent addlabel xlab(0/1) by(v01)
histogram v07_nom, discrete percent addlabel xlab(0/1) by(v03)
histogram v07_nom, discrete percent addlabel xlab(0/1) by(v01 v03)

graph pie,over(v07_nom) plabel(_all percent) by(v01)
graph pie,over(v07_nom) plabel(_all percent) by(v03)






sysuse auto, clear
des
note

sum price
sum price,d
tab price
hist price, bin(25) normal

tabstat price weight length, stats(mean sd rang count)
tabstat price weight length, stats(mean sd rang count) by(foreign)

tab rep78
tab rep78 foreign
tab rep78 foreign, sum(price)
tab rep78 foreign, sum(mpg)
table rep78 foreign, c(mean price mean mpg)




sysuse nlsw88
note
hist wage
hist wage, normal

gen lnwage=ln(wage)
hist lnwage, normal
graph box wage
graph hbox wage
graph hbox wage, by(union race)


clear 
use dataset1 
browse
append using dataset2
browse 

clear 
use dataset1
gen var var3=7

use data14,clear
browse 
use data15,clear
browse 
use data14
merge 1:1 id using data15
browse 



















