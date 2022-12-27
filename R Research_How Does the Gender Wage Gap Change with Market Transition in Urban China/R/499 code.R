# ECON 499 Paper code Kun Yang(Chris)

# clear the workspace then set the current working directory.
rm(list=ls())
setwd("C:/Users/Kun/Desktop/499/R/data")

# load required library
require("foreign")
require("memisc")
require("oaxaca")
require("ggplot2")
require("xlsx")

# read the dta data
data2007 <- read.dta("2007data.dta")
data2002 <- read.dta("2002.dta")
data1998 <- read.dta("1999.dta")
data1995 <- read.dta("1995.dta")




####-------------------------add variables and clean data------------------------
# add male variables
data2007$male[data2007$gender=="male"] <- 0
data2007$male[data2007$gender=="female"] <- 1
data2002$male[data2002$P105=="male"] <- 0
data2002$male[data2002$P105=="female"] <- 1
data1998$male[data1998$male==1] <- 0
data1998$male[data1998$male==2] <- 1
data1995$male[data1995$A4=="male"] <- 0
data1995$male[data1995$A4=="female"] <- 1


# add log monthly wage variables
data2007$logincome <- log(data2007$primaryincome)
data2002$monthwage02 <- data2002$INCOME01/12
data2002$logincome <- log(data2002$monthwage02)
data1998$monthwage98 <- data1998$income98/12
data1998$logincome <- log(data1998$monthwage98)
data1995$monthwage95 <- data1995$A52/12
data1995$logincome <- log(data1995$monthwage95)


# add age variables
data1995$age <- data1995$A5
data2002$age <- data2002$P106


#add province vaeiables
data2002$province <- NA
data2002$province[data2002$PROVINCE==11] <- 'beijing'
data2002$province[data2002$PROVINCE==14] <- 'shanxi'
data2002$province[data2002$PROVINCE==21] <- 'liaoning'
data2002$province[data2002$PROVINCE==32] <- 'jiangsu'
data2002$province[data2002$PROVINCE==34] <- 'anhui'
data2002$province[data2002$PROVINCE==41] <- 'henan'
data2002$province[data2002$PROVINCE==42] <- 'hubei'
data2002$province[data2002$PROVINCE==44] <- 'guangdong'
data2002$province[data2002$PROVINCE==50] <- 'chongqing'
data2002$province[data2002$PROVINCE==51] <- 'sichuan'
data2002$province[data2002$PROVINCE==62] <- 'yunnan'
data2002$province[data2002$PROVINCE==62] <- 'gansu'

data1998$province <- NA
data1998$province[data1998$prov==11] <- 'beijing'
data1998$province[data1998$prov==21] <- 'liaoning'
data1998$province[data1998$prov==32] <- 'jiangsu'
data1998$province[data1998$prov==41] <- 'henan'
data1998$province[data1998$prov==51] <- 'sichuan'
data1998$province[data1998$prov==62] <- 'gansu'

data1995$province <- NA
data1995$province[data1995$PROVINCE==32] <- 'jiangsu'
data1995$province[data1995$PROVINCE==34] <- 'anhui'
data1995$province[data1995$PROVINCE==41] <- 'henan'
data1995$province[data1995$PROVINCE==42] <- 'hubei'
data1995$province[data1995$PROVINCE==44] <- 'guangdong'
data1995$province[data1995$PROVINCE==51] <- 'sichuan'
data1995$province[data1995$PROVINCE==53] <- 'yunnan'
data1995$province[data1995$PROVINCE==62] <- 'gansu'
data1995$province[data1995$PROVINCE==11] <- 'beijing'
data1995$province[data1995$PROVINCE==14] <- 'shanxi'
data1995$province[data1995$PROVINCE==21] <- 'liaoning'


# add ownership variables
#2007
data2007$owner <- NA
data2007$owner[data2007$ownership=='Solely state owned enterprises'] <- 'state-owned'
data2007$owner[data2007$ownership=='State holding enterprises'] <- 'state-owned'
data2007$owner[data2007$ownership=='State holding joint venture'] <- 'state-owned'
data2007$owner[data2007$ownership=='Solely private owned enterprises'] <- 'private or self-employed'
data2007$owner[data2007$ownership=='Private holding enterprises'] <- 'private or self-employed'
data2007$owner[data2007$ownership=='Private holding joint venture'] <- 'private or self-employed'
data2007$owner[data2007$ownership=='Self-employed Individuals'] <- 'private or self-employed'
data2007$owner[data2007$ownership=='Solely collective owned enterprise'] <- 'collective'
data2007$owner[data2007$ownership=='Collective holding enterprises'] <- 'collective'
data2007$owner[data2007$ownership=='Collective holding joint venture'] <- 'collective'
data2007$owner[data2007$ownership=='Solely Foreign owned enterprises'] <- 'joint or foreign'
data2007$owner[data2007$ownership=='Foreign holding joint venture'] <- 'joint or foreign'
data2007$owner[data2007$ownership=='Other enterprises'] <- 'other'

# 2002
data2002$owner <- NA  
data2002$owner[data2002$P135==1] <- 'state-owned'
data2002$owner[data2002$P135==2] <- 'state-owned'
data2002$owner[data2002$P135==8] <- 'state-owned'
data2002$owner[data2002$P135==4] <- 'private or self-employed'
data2002$owner[data2002$P135==5] <- 'private or self-employed'
data2002$owner[data2002$P135==3] <- 'collective'
data2002$owner[data2002$P135==6] <- 'joint or foreign'
data2002$owner[data2002$P135==7] <- 'joint or foreign'
data2002$owner[data2002$P135==9] <- 'other'
data2002$owner[data2002$P135==12] <- 'other'

# 1998
data1998$owner <- NA
data1998$owner[data1998$ownership==1] <- 'state-owned'
data1998$owner[data1998$ownership==2] <- 'state-owned'
data1998$owner[data1998$ownership==8] <- 'state-owned'
data1998$owner[data1998$ownership==4] <- 'private or self-employed'
data1998$owner[data1998$ownership==5] <- 'private or self-employed'
data1998$owner[data1998$ownership==3] <- 'collective'
data1998$owner[data1998$ownership==6] <- 'joint or foreign'
data1998$owner[data1998$ownership==7] <- 'joint or foreign'
data1998$owner[data1998$ownership==9] <- 'other'
data1998$owner[data1998$ownership==12] <- 'other'

# 1995
data1995$owner <- NA
data1995$owner[data1995$A27=='state-owned, at central or provincial level'] <- 'state-owned'
data1995$owner[data1995$A27=='private enterprise, including partnership'] <- 'private or self-employed'
data1995$owner[data1995$A27=='self-employed business/individual enterprise'] <- 'private or self-employed'
data1995$owner[data1995$A27=='urban collective'] <- 'collective'
data1995$owner[data1995$A27=='local publicly-owned'] <- 'collective'
data1995$owner[data1995$A27=='sino-foreign joint venture'] <- 'joint or foreign'
data1995$owner[data1995$A27=='foreign owned'] <- 'joint or foreign'
data1995$owner[data1995$A27=='other'] <- 'other'


#add occupation vaeiables
# 2007
data2007$occupation.new <- data2007$occupation
data2007$occupation <- NA
data2007$occupation[data2007$occupation.new=='Principals in State Agencies, Party organizations, enterprises and public service unit'] <- 'Head of institution'
data2007$occupation[data2007$occupation.new=='Professional technicians'] <- 'Professional'
data2007$occupation[data2007$occupation.new=='Other practitioner(difficult to classify)'] <- 'Other'
data2007$occupation[data2007$occupation.new=='Commercial and service personnel'] <- 'Unskilled worker'
data2007$occupation[data2007$occupation.new=='Clerk and relating personnel'] <- 'Office staff'
data2007$occupation[data2007$occupation.new=='Manufacturing and transporting equipment manipulator and relating personnel'] <- 'Skilled worker'   

# 2002
data2002$occupation <- NA
data2002$occupation[data2002$P141==4] <- 'Head of institution'
data2002$occupation[data2002$P141==5] <- 'Head of institution'
data2002$occupation[data2002$P141==6] <- 'Office staff'
data2002$occupation[data2002$P141==1] <- 'Manager (Owner) of private firm'
data2002$occupation[data2002$P141==2] <- 'Manager (Owner) of private firm'
data2002$occupation[data2002$P141==3] <- 'Professional'
data2002$occupation[data2002$P141==9] <- 'Unskilled worker'
data2002$occupation[data2002$P141==7] <- 'Skilled worker'
data2002$occupation[data2002$P141==8] <- 'Unskilled worker'
data2002$occupation[data2002$P141==11] <- 'Other'

# 1998
data1998$occupation.new <- data1998$occupation
data1998$occupation <- NA
data1998$occupation[data1998$occupation.new==3] <- 'Head of institution'
data1998$occupation[data1998$occupation.new==4] <- 'Head of institution'
data1998$occupation[data1998$occupation.new==5] <- 'Office staff'
data1998$occupation[data1998$occupation.new==2] <- 'Professional'
data1998$occupation[data1998$occupation.new==1] <- 'Manager (Owner) of private firm'
data1998$occupation[data1998$occupation.new==12] <- 'Other'
data1998$occupation[data1998$occupation.new==7] <- 'Skilled worker'
data1998$occupation[data1998$occupation.new==6] <- 'Skilled worker'
data1998$occupation[data1998$occupation.new==8] <- 'Unskilled worker'
data1998$occupation[data1998$occupation.new==9] <- 'Unskilled worker'

# 1995
data1995$occupation <- NA
data1995$occupation[data1995$A31=='division head in institution'] <- 'Head of institution'
data1995$occupation[data1995$A31=='head of institution'] <- 'Head of institution'
data1995$occupation[data1995$A31=='office worker'] <- 'Office staff'
data1995$occupation[data1995$A31=='professional or technical worker'] <- 'Professional'
data1995$occupation[data1995$A31=='owner of private or individual enterprise'] <- 'Manager (Owner) of private firm'
data1995$occupation[data1995$A31=='owner and manager of private enterprise'] <- 'Manager (Owner) of private firm'
data1995$occupation[data1995$A31=='other'] <- 'Other'
data1995$occupation[data1995$A31=='skilled worker'] <- 'Skilled worker'
data1995$occupation[data1995$A31=='unskilled worker'] <- 'Unskilled worker'


#add education level vaeiables
# 2007
data2007 <- subset(data2007, educationyears>=6)
data2007$educlevel <- cut(data2007$educationyears, breaks=c(6,9,11,12,14,16,Inf),right=FALSE,
                     labels=c("Elementary school","Junior middle school","Technical secondary school",
                              "Senior middle school","2-year college","4-year College and above"))
# 2002
data2002$educlevel <- NA
data2002$educlevel[data2002$P112==3] <- 'Elementary school'
data2002$educlevel[data2002$P112==4] <- 'Junior middle school'
data2002$educlevel[data2002$P112==5] <- 'Senior middle school'
data2002$educlevel[data2002$P112==8] <- 'Technical secondary school'
data2002$educlevel[data2002$P112==7] <- '2-year college'
data2002$educlevel[data2002$P112==8] <- '4-year College and above'
data2002$educlevel[data2002$P112==9] <- '4-year College and above'

# 1998
data1998$educlevel.new <- data1998$educlevel
data1998$educlevel <- NA
data1998$educlevel[data1998$educlevel.new==6] <- 'Elementary school'
data1998$educlevel[data1998$educlevel.new==5] <- 'Junior middle school'
data1998$educlevel[data1998$educlevel.new==4] <- 'Senior middle school'
data1998$educlevel[data1998$educlevel.new==3] <- 'Technical secondary school'
data1998$educlevel[data1998$educlevel.new==2] <- '2-year college'
data1998$educlevel[data1998$educlevel.new==1] <- '4-year College and above'

# 1995
data1995$educlevel <- NA
data1995$educlevel[data1995$A11=='elementary school'] <- 'Elementary school'
data1995$educlevel[data1995$A11=='lower middle school'] <- 'Junior middle school'
data1995$educlevel[data1995$A11=='upper midle school'] <- 'Senior middle school'
data1995$educlevel[data1995$A11=='middle level professional, technical or vocational school'] <- 'Technical secondary school'
data1995$educlevel[data1995$A11=='professional school'] <- '2-year college'
data1995$educlevel[data1995$A11=='college or above'] <- '4-year College and above'


# Add job nature variables
#2007
data2007$nature <- NA 
data2007$nature[data2007$jobnature=='permanent'] <- 'permanent'
data2007$nature[data2007$jobnature=='long term contract worker (one year and above)'] <- 'long term'
data2007$nature[data2007$jobnature=='short term contract worker (less than one year)'] <- 'short term'
data2007$nature[data2007$jobnature=='self-employed (skip to c25)'] <- 'Private business or self-employed'
data2007$nature[data2007$jobnature=='part-time-job'] <- 'short term'
data2007$nature[data2007$jobnature=='non-contract temp'] <- 'non-contract'

#2002
data2002$nature <- NA 
data2002$nature[data2002$P140==1] <- 'permanent'
data2002$nature[data2002$P140==2] <- 'long term'
data2002$nature[data2002$P140==3] <- 'short term'
data2002$nature[data2002$P140==5] <- 'Private business or self-employed'
data2002$nature[data2002$P140==4] <- 'non-contract'

#1998
data1998$nature <- NA 
data1998$nature[data1998$jobnature==1] <- 'permanent'
data1998$nature[data1998$jobnature==2] <- 'long term'
data1998$nature[data1998$jobnature==3] <- 'short term'
data1998$nature[data1998$jobnature==4] <- 'non-contract'
data1998$nature[data1998$jobnature==5] <- 'Private business or self-employed'

#1995
data1995$nature <- NA
data1995$nature[data1995$A29=='permanent worker'] <- 'permanent'
data1995$nature[data1995$A29=='long-term contract worker or employee'] <- 'long term'
data1995$nature[data1995$A29=='temporary (including short-term contract) worker'] <- 'short term'
data1995$nature[data1995$A29=='private enterprise proprietor or self-employed'] <- 'Private business or self-employed'


# Add industry variables
# 2007
data2007$industry[data2007$industry==""] <- NA
data2007$industry[data2007$industry=='Agriculture, Forestry, Animal husbandry'] <- NA
data2007$industry[data2007$industry=='Management of Water Conservancy, Environment and Public Facilities'] <- NA
data2007$industry[data2007$industry=='Mining'] <- NA

# 2002
data2002$industry <- NA
data2002$industry[data2002$P143==1] <- 'Farm, forest, husbandry and fishery'
data2002$industry[data2002$P143==2] <- 'Mineral'
data2002$industry[data2002$P143==3] <- 'Manufacturing'
data2002$industry[data2002$P143==4] <- 'Electricity, gas and water supply facilities'
data2002$industry[data2002$P143==5] <- 'Construction'
data2002$industry[data2002$P143==7] <- 'Transportation, storage, post office and communication'
data2002$industry[data2002$P143==8] <- 'Wholesale, retail and food services'
data2002$industry[data2002$P143==9] <- 'Finance and insurance'
data2002$industry[data2002$P143==11] <- 'Social services'
data2002$industry[data2002$P143==12] <- 'Health, sports and social welfare'
data2002$industry[data2002$P143==13] <- 'Education, culture and arts, mass media and entertainment'
data2002$industry[data2002$P143==14] <- 'Scientific research and professional services'
data2002$industry[data2002$P143==15] <- 'Health, sports and social welfare'
data2002$industry[data2002$P143==16] <- 'Government agents, party organisations and social groups'

# 1998
data1998$industry.new <- data1998$industry 
data1998$industry <- NA
data1998$industry[data1998$industry.new==1] <- 'Farm, forest, husbandry and fishery'
data1998$industry[data1998$industry.new==2] <- 'Mineral'
data1998$industry[data1998$industry.new==3] <- 'Manufacturing'
data1998$industry[data1998$industry.new==4] <- 'Electricity, gas and water supply facilities'
data1998$industry[data1998$industry.new==5] <- 'Construction'
data1998$industry[data1998$industry.new==7] <- 'Transportation, storage, post office and communication'
data1998$industry[data1998$industry.new==8] <- 'Wholesale, retail and food services'
data1998$industry[data1998$industry.new==9] <- 'Finance and insurance'
data1998$industry[data1998$industry.new==11] <- 'Social services'
data1998$industry[data1998$industry.new==12] <- 'Health, sports and social welfare'
data1998$industry[data1998$industry.new==13] <- 'Education, culture and arts, mass media and entertainment'
data1998$industry[data1998$industry.new==14] <- 'Scientific research and professional services'
data1998$industry[data1998$industry.new==15] <- 'Health, sports and social welfare'
data1998$industry[data1998$industry.new==16] <- 'Government agents, party organisations and social groups'

#1995
data1995$A34[data1995$A34=='other'] <- NA
data1995$A34[data1995$A34=='unknown code'] <- NA
data1995$industry <- data1995$A34


## clean NA data and set age from 25 to 60
data2007 <- subset(data2007, age>=25 & age<=65 & logincome>0 & primaryincome>=10 & primaryincome<60000 & !is.na(male) & !is.na(age) & !is.na(province) & !is.na(nature)
                   & !is.na(educlevel)  & !is.na(occupation) & !is.na(industry) & !is.na(owner) & !is.na(primaryincome) & !is.na(logincome))

data2002 <- subset(data2002, age>=25 & age<=65 & logincome>0 & INCOME01>=10 & INCOME01<100000 & !is.na(male) & !is.na(age) & !is.na(province) & !is.na(nature) 
                   & data2002$educlevel!= 'Below elementary school' & !is.na(educlevel) & !is.na(occupation) & !is.na(industry) & !is.na(owner) & !is.na(INCOME01) & !is.na(logincome))

data1998 <- subset(data1998, age>=25 & age<=65 & logincome>0 & !is.na(male) & !is.na(age) & !is.na(province) & !is.na(nature)
                   & !is.na(educlevel) & !is.na(occupation) & !is.na(industry) & !is.na(owner) & !is.na(income98) & !is.na(logincome))

data1995 <- subset(data1995, age>=25 & age<=65  & logincome>0 & !is.na(male) & !is.na(age) & !is.na(province) & !is.na(nature)
                   & !is.na(educlevel) & !is.na(occupation) & !is.na(industry) & !is.na(owner) & !is.na(A52) & !is.na(logincome))

# set the new needed datafram
select.vars <- c('male','logincome','age','educlevel','occupation','owner','province','industry','nature')
newdata2007 <- data2007[select.vars]
newdata2002 <- data2002[select.vars]
newdata1998 <- data1998[select.vars]
newdata1995 <- data1995[select.vars]




#---------------------Summary of statistics (new cleaned dataset)-----------------------------
# occupation
occupation.07 <- as.data.frame(table(newdata2007$occupation))
occupation.02 <- as.data.frame(table(newdata2002$occupation))
occupation.98 <- as.data.frame(table(newdata1998$occupation))
occupation.95 <- as.data.frame(table(newdata1995$occupation))
occupation <- Reduce(function(x,y) merge(x,y,by="Var1",all=T), list(occupation.07, occupation.02, occupation.98, occupation.95)) 
colnames(occupation) <- c("occupation","2007","2002","1998","1995")
occupation

# owner
owner.07 <- as.data.frame(table(newdata2007$owner))
owner.02 <- as.data.frame(table(newdata2002$owner))
owner.98 <- as.data.frame(table(newdata1998$owner))
owner.95 <- as.data.frame(table(newdata1995$owner))
owner <- Reduce(function(x,y) merge(x,y,by="Var1",all=T), list(owner.07, owner.02, owner.98, owner.95))
colnames(owner) <- c("ownership","2007","2002","1998","1995")
owner

# industry
industry.07 <- as.data.frame(table(newdata2007$industry))
industry.02 <- as.data.frame(table(newdata2002$industry))
industry.98 <- as.data.frame(table(newdata1998$industry))
industry.95 <- as.data.frame(table(newdata1995$industry))
industry <- Reduce(function(x,y) merge(x,y,by="Var1",all=T), list(industry.07, industry.02, industry.98, industry.95))
colnames(industry) <- c("industry","2007","2002","1998","1995")
industry

# nature
nature.07 <- as.data.frame(table(newdata2007$nature))
nature.02 <- as.data.frame(table(newdata2002$nature))
nature.98 <- as.data.frame(table(newdata1998$nature))
nature.95 <- as.data.frame(table(newdata1995$nature))
nature <- Reduce(function(x,y) merge(x,y,by="Var1",all=T), list(nature.07, nature.02, nature.98, nature.95))
colnames(nature) <- c("nature","2007","2002","1998","1995")
nature

# educlevel
educlevel.07 <- as.data.frame(table(newdata2007$educlevel))
educlevel.02 <- as.data.frame(table(newdata2002$educlevel))
educlevel.98 <- as.data.frame(table(newdata1998$educlevel))
educlevel.95 <- as.data.frame(table(newdata1995$educlevel))
educlevel <- Reduce(function(x,y) merge(x,y,by="Var1",all=T), list(educlevel.07, educlevel.02, educlevel.98, educlevel.95))
colnames(educlevel) <- c("educlevel","2007","2002","1998","1995")
educlevel

# province
province.07 <- as.data.frame(table(newdata2007$province))
province.02 <- as.data.frame(table(newdata2002$province))
province.98 <- as.data.frame(table(newdata1998$province))
province.95 <- as.data.frame(table(newdata1995$province))
province <- Reduce(function(x,y) merge(x,y,by="Var1",all=T), list(province.07, province.02, province.98, province.95))
colnames(province) <- c("province","2007","2002","1998","1995")
province

# age
age.07 <- summary(newdata2007$age)
age.02 <- summary(newdata2002$age)
age.98 <- summary(newdata1998$age)
age.95 <- summary(newdata1995$age)
age <- cbind(age.07, age.02,age.98,age.95)
age

# gender
gender.07 <- table(newdata2007$male)
gender.02 <- table(newdata2002$male)
gender.98 <- table(newdata1998$male)
gender.95 <- table(newdata1995$male)
gender <- cbind(gender.07,gender.02,gender.98,gender.95)
rownames(gender) <- c("male", "female")
gender 

# gender
income.07 <- summary(newdata2007$logincome)
income.02 <- summary(newdata2002$logincome)
income.98 <- summary(newdata1998$logincome)
income.95 <- summary(newdata1995$logincome)
income <- cbind(income.07,income.02,income.98,income.95)
income


### Do oaxaca Decomposition ( bootstrap may not always work for year 1995)
oaxaca.07 <- oaxaca(logincome~age+educlevel+occupation+owner+province+industry+nature | male, data=newdata2007)
oaxaca.02 <- oaxaca(logincome~age+educlevel+occupation+owner+province+industry+nature | male, data=newdata2002)
oaxaca.98 <- oaxaca(logincome~age+educlevel+occupation+owner+province+industry+nature | male, data=newdata1998)
oaxaca.95 <- oaxaca(logincome~age+educlevel+occupation+owner+province+industry+nature | male, data=newdata1995)

oaxaca.07$n
oaxaca.07$y

oaxaca.02$n
oaxaca.02$y

oaxaca.98$n
oaxaca.98$y

oaxaca.95$n
oaxaca.95$y


### Make graphs to show wage gap
# set a table for making graph
year <- c('1995', '1998', '2002', '2007', '1995', '1998', '2002', '2007')
logincome <- c(oaxaca.95$y$y.A, oaxaca.98$y$y.A, oaxaca.02$y$y.A, oaxaca.07$y$y.A, 
               oaxaca.95$y$y.B, oaxaca.98$y$y.B, oaxaca.02$y$y.B, oaxaca.07$y$y.B)
gender <- c('male','male','male','male','female','female','female','female')
income <- data.frame(year,gender,logincome)
income

year <- c('1995', '1998', '2002', '2007')
logincomegap <- c(oaxaca.95$y$y.diff, oaxaca.98$y$y.diff, oaxaca.02$y$y.diff, oaxaca.07$y$y.diff)
incomegap <- data.frame(year,logincomegap)
incomegap

# Make graphs
Figure1 <- ggplot(data=income, aes(x=year, y=logincome, group=gender, colour=gender, label=(round(logincome,3))))+
  ggtitle("Mean of Log Income for Each Year")+
  xlab("Year")+ylab("Mean of Log Income")+
  geom_text(size=6)+
  geom_line() +
  geom_point()
Figure1  

Figure2 <- ggplot(data=incomegap, aes(x=year, y=logincomegap, group=1, label=(round(logincomegap,3))))+
  ggtitle("Gender Wage Gap")+
  xlab("Year")+ylab("Gap of Log Income")+
  geom_text(size=8,hjust=0.5)+
  geom_path(colour="red")+
  geom_point(size=2)
Figure2



#------------------------Oaxaca------------------------------------------
# Check the result of the decomposition
decomposition.07 <- oaxaca.07$twofold$overall[2,c(2,3,4,5)]
decomposition.02 <- oaxaca.02$twofold$overall[2,c(2,3,4,5)]
decomposition.98 <- oaxaca.98$twofold$overall[2,c(2,3,4,5)]
decomposition.95 <- oaxaca.95$twofold$overall[2,c(2,3,4,5)]

total <- rbind(decomposition.95, decomposition.98, decomposition.02, decomposition.07)
upper <- total[,3]+1.96*total[,4]
lower <- total[,3]-1.96*total[,4]
upper.e <- total[,1]+1.96*total[,2]
lower.e <- total[,1]-1.96*total[,2]
total <- cbind(total,lower.e,upper.e, lower, upper)
total
incomegap

# Tansfer to percent of total log income
unexplained.percent <- total[,3]/incomegap[,2]
unexplained.lower <- total[,7]/incomegap[,2]
unexplained.upper <- total[,8]/incomegap[,2]
explained.percent <- total[,1]/incomegap[,2]
explained.lower <- total[,5]/incomegap[,2]
explained.upper <- total[,6]/incomegap[,2]
total2 <- cbind(explained.percent,explained.lower,explained.upper,unexplained.percent,unexplained.lower,unexplained.upper)
print(total2,3)

# Make a data frame for the categorical graph (explained, unexplained)
a <- c(total2[1:4,1])*100
b <- c(total2[1:4,4])*100
year <- data.frame(c(1995,1998,2002,2007,1995,1998,2002,2007))
group <- data.frame(c(rep('explained', 4), c(rep('unexplained', 4))))
c <- c(total2[1:4,2],total2[1:4,5])*100
d <- c(total2[1:4,3],total2[1:4,6])*100
total.graph <- data.frame(c(a,b),year,group, c,d)
colnames(total.graph) <- c("Estimates", "Year", "group", "lower","upper")
total.graph

# Make the plot 
Figure3 <- ggplot(data=total.graph, aes(x=Year,y=Estimates,group=group,colour=group,label=round(Estimates,3)))+
  ggtitle("Explained & Unexplained Percentage of Log income")+
  xlab("Year")+ylab("Percentage")+ 
  geom_text(size=6)+
  scale_x_continuous(breaks=c(1995,1998,2002,2007))+
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin=total.graph[,4], ymax=total.graph[,5]), alpha=0.2)
Figure3




#--------------------Function 2---------------------------------------------------------
# check each provinces
plot(oaxaca.95, decomposition="twofold", weight=1, components=c("unexplained"), 
     variables=c("provincebeijing","provinceguangdong","provincehenan","provincehubei","provincejiangsu","provinceliaoning","provinceshanxi"))

plot(oaxaca.98, decomposition="twofold", weight=1, components=c("unexplained"), 
     variables=c("provincegansu","provincehenan","provincejiangsu","provinceliaoning","provincesichuan"))

plot(oaxaca.02, decomposition="twofold", weight=1, components=c("unexplained"), 
     variables=c("provincebeijing","provincechongqing","provincegansu","provinceguangdong","provincehenan","provincehubei", 
                 "provincejiangsu", "provinceliaoning","provinceshanxi","provincesichuan"))

plot(oaxaca.07, decomposition="twofold", weight=1, components=c("unexplained"), 
     variables=c("provincechongqing","provinceguangdong","provincehenan","provincehubei",
                 "provincejiangsu","provinceshanghai", "provincesichuan", "provincezhejiang"))

# From above plots, we get that estimated coeficients not significant or marginal significant for each province.
province


oaxaca.95$beta$beta.diff[1]
oaxaca.98$beta$beta.diff[1]
oaxaca.02$beta$beta.diff[1]
oaxaca.07$beta$beta.diff[1]

oaxaca.95$beta$beta.diff
oaxaca.98$beta$beta.diff
oaxaca.02$beta$beta.diff
oaxaca.07$beta$beta.diff

#-------------------------Make dataframe for next step function-----------------------------------
combi.data <- read.xlsx("regression data table.xlsx", sheetName="Sheet1", header=TRUE)

function1 <- lm(unexplained ~ logtrade + private, data=combi.data) 
summary(function1)

function2 <- lm(unexplained ~ loggdp + logtrade + private, data=combi.data) 
summary(function2)

function3 <- lm(unexplained ~ loggdp + logtrade + private + factor(province), data=combi.data) 
summary(function3)

function4 <- lm(unexplained ~ loggdp + logtrade + private + factor(year), data=combi.data) 
summary(function4)

function5 <- lm(unexplained ~ loggdp + logtrade + private + factor(province) + factor(year), data=combi.data) 
summary(function5)


table <- mtable("(1)"=function1, "(2)"=function2, "(3)"=function3, "(4)"=function4, "(5)"=function5,
                summary.stats=c("N","adj. R-squared"))
table$coefficients <- table$coefficients[,,2:4,,drop=FALSE]
table


