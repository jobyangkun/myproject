** A. Women Wages, race and unions
use "C:\Users\Kun\Desktop\Econ 665\Assignment_2\nlsw88.dta"
keep if !missing(wage+race+union)
ge lwage=log(wage)


//Part A
tab race, gen(R)
des R*
reg lwage R1 R2 union
test R1 R2

//Part B
gen R1ten=R1*tenure
gen R2ten=R2*tenure
reg lwage R1 R2 union tenure R1ten R2ten
test R1ten R2ten
estat hettest
estat ovtest
linktest

//Part C
ge R1u=R1*union
ge R2u=R2*union
generate uten=union*tenure
reg lwage R1 R2 union tenure R1u R2u
test R1u R2u
estat hettest
estat ovtest
linktest



