use "nlsw88.dta", clear
des
sum

keep if !missing(wage+race+union)

hist wage, bin(500)
kdensity wage
ge lwage=log(wage)


tab race, gen(R)
des R*

reg lwage R1 R2
reg lwage R1 R2 union


ge R1u=R1*union
ge R2u=R2*union

reg lwage R1 R2 union R1u R2u
* performs Wald test
test R1u R2u

tab race union
reg lwage R1 R2 union tenure
generate uten=union*tenure
reg lwage R1 R2 union tenure uten

gen R1ten=R1*tenure
gen R2ten=R2*tenure
reg lwage R1 R2 union tenure R1ten R2ten
test R1ten R2ten

reg lwage R1 R2 union tenure R2ten
reg lwage union tenure uten
reg lwage tenure if union==1
reg lwage tenure if union==0




