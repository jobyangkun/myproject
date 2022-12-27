use "flat_data.dta"

des
sum

reg flat_price floor_size
help predict

predict pricehat
predict ehat, residuals
sum flat_price pricehat ehat


corr flat_price pricehat ehat


