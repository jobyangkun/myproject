use "P:\665\flat\flat_data.dta"
des

tab location, gen(compus)
reg flat_price compus1 floor_size

gen centre=0
replace centre=1 if compus1==1

reg flat_price centre
reg flat_price compus1 compus2 compus3 compus4
reg flat_price compus2 compus3 compus4
reg flat_price compus2 compus3 compus4 floor_size





