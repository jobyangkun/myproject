** A. Distance to College and years of Completed Education
use "C:\Users\Kun\Desktop\Econ 665\Assignment_2\CollegeDistance.dta"

//Part A
reg ed dist


//Part B
reg ed dist bytest female black hispanic incomehi ownhome dadcoll cue80 stwmfg80
test bytest female black hispanic incomehi ownhome dadcoll cue80 stwmfg80
test black=hispanic



//Part E
margins, dydx(cue80 stwmfg80)
margins, eyex(cue80 stwmfg80)



//Part F
lincom _cons+2*dist+58*bytest+0*female+1*black+0*hispanic+1*incomehi+1*ownhome+0*dadcoll+7.5*cue80+9.75*stwmfg80





