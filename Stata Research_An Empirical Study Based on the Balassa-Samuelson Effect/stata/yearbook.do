use yearbook,clear


*** SUMMARY
twoway(connected gdp_com_primary gdp_com_secondary gdp_com_tertiary year, sort mcolor(red yellow blue)),legend(label(1 "Primary Industry") label(2 "secondary Industry") label(3 "Tertiary Industry")) ytitle("Composition of GDP") title("Composition of GDP by the three strata of industry")
twoway(line value_primary value_secondary value_tertiary year,sort mcolor(red yellow blue)),legend(label(1 "Primary Industry") label(2 "Secondary Industry") label(3 "Tertiary Industry"))
gen emp_tradable=emp_primary+emp_secondary
gen value_tradable=value_primary+value_secondary

gen apm=value_tradable/emp_tradable
gen aps=value_tertiary/emp_tertiary
gen relative = apm/aps
twoway(line relative year, sort)


gen log_reerCHINA = ln(reerCHINA)
gen log_reerapm = ln(apm)
gen log_reeraps = ln(aps)
gen log_er_rmbusd = ln(er_rmbusd)


twoway(line log_reerCHINA year,sort), legend(label(1 "log_reerCHINA"))
twoway(line log_reerapm log_reeraps year,sort), legend(label(1 "apm") label(2 "aps"))
twoway(line log_er_rmbusd year,sort), legend(label(1 "log_reerCHINA"))


keep if year > 2000
*** Baseline Specification
reg log_reerCHINA log_reerapm log_reeraps

reg log_rer log_rgdp_capita




*** Estimates of Balassa-Samuelson Effect (Cross-Country Regression for Year after 2008)

*** rer         – Real Exchange Rate is obtained by dividing Price Level of GDP for each country by that of the US (normalised to 100).
*** log_rer     – log of Real Exchange Rate
*** rgdp_pc     – Real GDP per capita (Constant price: Chain series)
*** log_rgdp_pc – Log of real GDP per capita


*** balassa 系数结果
***The regression log_rer vs. log_rgdp_pc was run for 118 countries, based on their year 2000 data for RER and Real GDP per capita. 
***It yields:logRER -4.15 0.382 loginc
***The coefficient on loginc is statistically significant.
reg logRER00 loginc00

*** 把中国带入
predict loginchat00, residual


*** di logRERhat[18] = -1.0152169
*** di logRER00[18]-logRERhat[18] = -.4484849

*** This means that
*** logRER00 - logRERhat -0.448
*** （RER00/RERhat） = 0.639
*** or in other words, the real exchange rate is 64% of the value predicted by the regression.







*** Estimates of Balassa-Samuelson Effect (Cross-Country Regression for Year from 2000 to 2008)

*** 结果
*** The regression log_rer vs. log_rgdp_pc was repeated for the same for 118 countries, based on their year 1990 data for RER and Real GDP per capita.
*** The regression yields: logRER 3.40 = - + 0.317 loginc
*** The coefficient on loginc is again statistically significant.

reg logRER90 loginc90

*** Again, the residual for each country was calculated and this value for China is (-.422)
*** di ehat90[18] = -.42194867 Thus
*** logRER00 - logRERhat -0.422
*** (RER00/RERhat) = 0.656
*** or in other words, the real exchange rate is 66% of its predicted value by the regression, very close to the value found for the 2000 data.
*** Exchange Rate Deviations for China



*** Estimates of Balassa-Samuelson Effect (Cross-Country Regression for Year from 1994 to 2000)













