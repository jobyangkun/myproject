use "P:\665\flat\flat_data.dta"
des

reg flat_price floor_size
sum flat_price floor_size

reg flat_price floor_size
predict yhat
predict ehat, residuals



twoway (scatter flat_price floor_size, sort), ylin(0)
twoway (scatter ehat floor_size, sort), ylin(0)

reg flat_price floor_size
sum yhat
sum yhat flat_price

margins, at(floor_size=(60(20)220))
marginsplot

twoway (scatter flat_price floor_size, sort) (lfit flat_price floor_size)
reg flat_price floor_size if flat_price<1600000
test floor_size=5273




ta loca
ta loca,nolab

ge centre=0
replace centre=1 if location==1

ge south=0
replace south=1 if location==2

ge west=0
replace west=1 if location==3

ge east=0
replace east=1 if location==4
sum centre south west east

list centre south west east
ge see=centre+south+west+east
sum see


reg flat_price centre
reg flat_price centre south west east
reg flat_price south west east


