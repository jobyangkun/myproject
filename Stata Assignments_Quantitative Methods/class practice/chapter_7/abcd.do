use "ESS5GBdiagnostics_data.dta"

sum
tab trstlgl

sum trstlgl age woman political_interest religious
reg trstlgl age woman political_interest religious
estat summarize
linktest
estat ovtest

ge agesq=age^2
reg trstlgl age woman political_interest religious
reg trstlgl age agesq woman political_interest religious
lowess trst age, gen(yhatlowess)
line yhatlowess age, sort

reg trstlgl c.age##c.age woman political_interest religious
margins,at (age=(15(1)98))
marginsplot

regress trstlgl c.age##c.age##i.woman political_interest religious
margins, at(age=(15(1)98) woman(0 1))
marginsplot

reg trstlgl age agesq woman political_interest religious
lowess happy age


regress trstlgl c.age##c.age political_interest religious if woman==1
margins,at (age=(15(1)98))
marginsplot

tab edage
des lrscale
tab lrscale

generate dleft=(lrscale<3) if !missing(lrscale)
tab dleft
tab lrscale dleft
generate dcentre=(lrscale>2 & lrscale<8) if !missing(lrscale)
tab lrscale dcentre
sum dcentre
sum woman

reg edagegb woman religious dleft Dcentre
reg trstlgl woman age political_interest religious
estat vif
estat vce
graph matrix trstlgl age woman political_interest religious


predict ehat,residuals
line ehat age,sort
line ehat woman,sort
scatter ehat woman,sort
rvfplot
rvpplot woman
rvpplot age
rvpplot religious
estat hettest
hist ehat
hist ehat,xline(0) normal
sktest ehat










