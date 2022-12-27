use "Logistic_ESS5.dta"

des
tab polit
tab polit,nolab


reg vote woman age political
predict votehat
scatter votehat age, ylin(1)


logit vote woman age political_interest
predict voteloghat
sum voteloghat
sum voteloghat votehat
scatter votehat voteloghat age , ylin(1)


probit vote woman age political_interest
predict voteprobhat
sum vote voteprobhat voteloghat votehat
sum voteprobhat voteloghat votehat if vote!=.


logit vote woman age political_interest
margins, dydx(*) atmeans

mlogit party_voted woman age political_interest, base(0)
margins, predict(outcome(1))
margins, predict(outcome(2))
margins, predict(outcome(3))
margins, predict(outcome(0)) at(woman=1 age=27 political_interest=3)


tab political_interest 
tab political_interest, nol
ologit party_voted woman age political_interest
xtlogit party_voted woman age political_interest





