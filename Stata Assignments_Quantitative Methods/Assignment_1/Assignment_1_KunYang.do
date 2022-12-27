use "C:\Users\Kun\Desktop\Econ 665\Assignment_1\hh_98.dta"


** Please answer the following sections:
** You should submit your answers in a wellwritten text, using Stata output as well as the do file that you used to get the answers. 
** In your report, you can use another document for your analysis but make sure it answers the questions in each section below. 

** A. Household characteristics
des
tabstat famsize hhasset hhland agehead educhead sexhead, by (dfmfd) stat(mean sd) format(%12.3f) c(s)
tabstat famsize educhead agehead hhasset hhland, by (sexhead) stat(mean sd) format(%12.3f) c(s)



** B. Village characteristics
tabstat vaccess pcirr, stat(mean sd) format(%11.3f) c(s)


** C. Prices
tabstat rice wheat oil milk potato, stat(mean sd) format(%11.3f) c(s)
tabstat rice wheat oil milk potato if dmmfd==1 | dfmfd==1, stat(mean sd) format(%11.3f) c(s)
tabstat rice wheat oil milk potato if dmmfd==0 & dfmfd==0, stat(mean sd) format(%11.3f) c(s)


** D. Expenditure
tabstat exptot expfd expnfd, by(sexhead) stat(mean sd) format(%11.3f) c(s)

gen edu=0
replace edu=1 if educhead >0
tab educhead edu 
tabstat exptot expfd expnfd, by(edu) stat(mean sd) format(%11.3f) c(s)


gen largesize=0
replace largesize=1 if famsize>5
tab famsize largesize
tabstat exptot expfd expnfd, by(largesize) stat(mean sd) format(%11.3f) c(s)


gen largeland=0
replace largeland=1 if hhland>50
tab hhland largeland
tabstat exptot expfd expnfd, by(largeland) stat(mean sd) format(%11.3f) c(s)

tabstat exptot expfd expnfd, by(dfmfd) stat(mean sd) format(%11.3f) c(s)



** E. Statistical Analysis

graph pie exptot, over(edu) title(Per capita expenditure by head education level) legend(lab(2 "some education") lab(1 "no education"))
graph pie expfd, over(edu) title(Per capita food expenditure by head education level) legend(lab(2 "some education") lab(1 "no education"))
graph pie expnfd, over(edu) title(Per capita nonfood expenditure by head education level) legend(lab(2 "some education") lab(1 "no education"))

graph pie exptot, over(largesize) title(Per capita expenditure by household size) legend(lab(2 "large size") lab(1 "small size"))
graph pie expfd, over(largesize) title(Per capita food expenditure by household size) legend(lab(2 "large size") lab(1 "small size"))
graph pie expnfd, over(largesize) title(Per capita nonfood expenditure by household size) legend(lab(2 "large size") lab(1 "small size"))

histogram exptot if exptot <15000, kdensity xscale(range(0 15000)) by(, title(Per capita expenditure by female participants)) by(dfmfd, total)
histogram expfd, kdensity xscale(range(0 15000)) by(, title(Per capita food expenditure by female participants)) by(dfmfd, total)
histogram expnfd if expnfd <15000, kdensity xscale(range(0 15000)) by(, title(Per capita nonfood expenditure by female participants)) by(dfmfd, total)


**ttest
ttest exptot, by(dfmfd)
ttest expfd, by(dfmfd)
ttest expnfd, by(dfmfd)

**« goodness of fit » test
tabulate largesize dfmfd, chi2
tabulate largeland dfmfd, chi2
tabulate edu dfmfd, chi2



**compare distribution
ksmirnov exptot, by( dfmfd )
ranksum exptot, by( dfmfd )

ksmirnov expfd, by(dfmfd)
ranksum expfd, by( dfmfd )

ksmirnov expnfd, by( dfmfd )
ranksum expnfd, by( dfmfd )




