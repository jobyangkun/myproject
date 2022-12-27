# Econ 435  Replication   Kun Yang   301178299     9th DEC. 2015

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/435/Replication")

# load required library
require("foreign")
require("memisc")

# read the dta data
chns <- read.dta("chns.dta") #Dataset 1: China Health and Nutrition Survey
slcc <- read.dta("slcc.dta") #Dataset 2: State and Life Chances

# Keep only the needed observation.
chns <- subset(chns, single!=1 & male==1 & urban_type==1 & age>=22 & age<=45 & year>1989 & !is.na(married) & !is.na(yrs_married) & !is.na(health_status)  & !is.na(stat_work) & !is.na(stat_school))
slcc <- subset(slcc, year>=80 & age_male>=22 & age_male<=45 & div_male!=1 & !is.na(yrs_marr) & !is.na(age_male) & !is.na(incom_male) &  !is.na(emp_state_male)  & !is.na(live_mil_male) & !is.na(live_fil_male) )



# set a function to get the Summary Statistics for chns data set
calculate.fct <- function(x){
  table1.var<- c("age","hsedu","hrindinc_real", "dist_dadinlawti","dist_mominlawti", "emp_state","samehouse_mominlaw","samehouse_dadinlaw","live_dadinlaw","live_mominlaw")
  x <- x[table1.var] #keep the needed variables
  Mean <- colMeans(x, na.rm=TRUE) #calculate the mean
  SD <- sqrt(diag(var(x, na.rm=TRUE)))#calculate the standard deviation
  Minima <- apply(x,2,min, na.rm=TRUE)#clculate the minima
  Maxima <- apply(x,2,max, na.rm=TRUE)#calculate the maxima
  result <- data.frame(Mean, SD, Minima, Maxima) #the dataframe for the final results
  result  #return the result
}

# apply the function on chns data
chns.results <- calculate.fct(chns)

# calculate the number of observations
Observations <-nrow(chns)

# print the table 1 for chns dataset
table.chns <- round(chns.results,2) # round off to 3 decimal places
table.chns[nrow(table.chns)+1, ] <- Observations #add the number of observations into table
# rename the rows
rownames(table.chns) <- c("Age", "High school education", "Real hourly earnings", "Father-in-law distance (km)", "Mother-in-law distance (km)", "State sector job", "Reside with father-in-law", "Reside with mother-in-law", "Father-in-law alive", "Mother-in-law alive", "Observations")
table.chns



# set a function to get the Summary Statistics for slcc data set
calculate.fct2 <- function(x){
  table1.var<- c("age_male","hsedu_male", "incom_male","emp_state_male", "live_fil_male","live_mil_male")
  x <- x[table1.var] #keep the needed variables
  Mean <- colMeans(x, na.rm=TRUE) #calculate the mean
  SD <- sqrt(diag(var(x, na.rm=TRUE)))#calculate the standard deviation
  Minima <- apply(x,2,min, na.rm=TRUE)#clculate the minima
  Maxima <- apply(x,2,max, na.rm=TRUE)#calculate the maxima
  result <- data.frame(Mean, SD, Minima, Maxima) #the dataframe for the final results
  result  #return the result
}

# apply the function on slcc data
slcc.results <- calculate.fct2(slcc)

# calculate the number of observations
Observations <-nrow(slcc)

# print the table 1 for slcc dataset
table.slcc <- round(slcc.results,2) # round off to 3 decimal places
table.slcc[nrow(table.slcc)+1, ] <- Observations #add the number of observations into table

# rename the rows
rownames(table.slcc) <- c("Age", "High school education", "Real monthly earnings", "State sector job", "Father-in-law alive", "Mother-in-law alive", "Observations")
table.slcc






# Replicate Table 2
# Column 1 by chns
Table2.chns1 <- lm(loghrindinc_real ~ post_FILdie+age+age2+age3+factor(year)*factor(province)+factor(edulevel)+div+head+spouse+factor(id), data=chns) 

# Column 2 by chns
Table2.chns2 <- lm(loghrindinc_real ~ post_FILdie+age+age2+age3+factor(year)*factor(province)+factor(edulevel)+div+head+spouse+yrs_married+health_status+factor(id), data=chns) 

# make a table to show the result
table2 <- mtable("(1) No additional controls"=Table2.chns1, "(2) With additional controls"=Table2.chns2, summary.stats=c("N","adj. R-squared"))
table2$coefficients <- table2$coefficients[,,2,,drop=FALSE]
table2



# Table 4
# panel A column 1
Table4.chns1 <- lm(loghrindinc_real ~ post_MILdie+age+age2+age3+factor(year)*factor(province)+factor(edulevel)+div+head+spouse+factor(id), data=chns) 

# panel A column 2
Table4.chns2 <- lm(loghrindinc_real ~ post_MILdie+age+age2+age3+factor(year)*factor(province)+factor(edulevel)+div+head+spouse+yrs_married+health_status+factor(id), data=chns) 

# panel B column 1
Table4.chns3 <- lm(loghrindinc_real ~ post_MILdie+post_FILdie+age+age2+age3+factor(year)*factor(province)+factor(edulevel)+div+head+spouse+factor(id), data=chns) 

# panel B column 2
Table4.chns4 <- lm(loghrindinc_real ~ post_MILdie+post_FILdie+age+age2+age3+factor(year)*factor(province)+factor(edulevel)+div+head+spouse+yrs_married+health_status+factor(id), data=chns) 


# make a table to show the result
# table 4 panel A
table4.A <- mtable("(1) No additional controls"=Table4.chns1, "(2) With additional controls"=Table4.chns2, summary.stats=c("N","adj. R-squared"))
table4.A$coefficients <- table4.A$coefficients[,,2,,drop=FALSE]
table4.A

# table 4 panel B
table4.B <- mtable("(1) No additional controls"=Table4.chns3, "(2) With additional controls"=Table4.chns4, summary.stats=c("N","adj. R-squared"))
table4.B$coefficients <- table4.B$coefficients[,,2:3,,drop=FALSE]
table4.B

