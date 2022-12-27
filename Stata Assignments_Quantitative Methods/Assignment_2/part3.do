** A. Village Program Placement and Female Participation in Microcredit program.
use "C:\Users\Kun\Desktop\Econ 665\Assignment_2\hh_98.dta"
egen progvillm=max(dmmfd), by(vill)
egen progvillf=max(dfmfd), by(vill)

//Part A
ttest lexptot, by(progvillf)
reg lexptot progvillf

//Part B
reg lexptot progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]

//Part C
ttest lexptot, by(dfmfd)
reg lexptot dfmfd

//Part D
reg lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]


//Part E
reg lexptot progvillf dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg [pw=weight]


reg lexptot dfmfd if progvillf==1 [pw=weight]
reg lexptot dfmfd sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if progvillf==1 [pw=weight]

reg lexptot progvillf if dfmfd==0 [pw=weight]
reg lexptot progvillf sexhead agehead educhead lnland vaccess pcirr rice wheat milk oil egg if dfmfd==0 [pw=weight]


















