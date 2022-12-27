clear
use "griliches.dta"

ivregress 2sls lw s expr tenure rns smsa _I* ( iq=med kww age mrt )
estat firststage
estat overid


ivregress 2sls lw s expr tenure rns smsa _I* ( iq=age )
estat firststage
estat overid

ivregress 2sls lw s expr tenure rns smsa _I* ( iq=med kww age ), robust
estat firststage
estat overid
estat endogenous

