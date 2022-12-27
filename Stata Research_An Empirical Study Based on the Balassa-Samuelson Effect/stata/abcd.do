*** SUMMARY
gen apm=value_tradable/emp_tradable
gen aps=value_untradable/emp_untradable
gen relative = apm/aps
twoway(line relative year, sort)

gen log_reerCHINA = ln(reerCHINA)
gen log_reerapm = ln(apm)
gen log_reeraps = ln(aps)

twoway(line log_reerapm log_reeraps year,sort), legend(label(1 "apm") label(2 "aps"))
