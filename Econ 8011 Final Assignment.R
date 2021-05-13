rm(list = ls())
#read in data
data <- read.csv("final.paper.data.csv", header = T)

emp.pop <- ts(data$emp.pop)
avg.hr.earn <- ts(data$avg.hr.earn)
emp25to54 <- ts(data$emp25to54)
emp15to64 <- ts(data$emp15to64)
unemp <- ts(data$unemp)
cpi <- ts(data$cpi)
indpro <- ts(data$indpro)

#Check for Order of Integration
source("intord.R")
intord(emp.pop)
intord(avg.hr.earn)
intord(emp25to54)
intord(emp15to64)
intord(unemp)
intord(cpi)

#Difference all variables once
d.emp.pop <- diff(emp.pop)
d.avg.hr.earn <- diff(avg.hr.earn)
d.emp25to54 <- diff(emp25to54)
d.emp15to64 <- diff(emp15to64)
d.unemp <- diff(unemp)
d.cpi <- diff(cpi)

#Visualize differenced variables
par(mfrow=c(2,3))
plot(d.emp.pop, type='l',col=1)
plot(d.avg.hr.earn, type='l',col=2)
plot(d.emp25to54, type='l',col=3)
plot(d.emp15to64, type='l',col=4)
plot(d.unemp, type='l',col=5)
plot(d.cpi, type='l',col=6)

#undifferenced
#take logs of each variable
l.emp.pop <- log(emp.pop)
l.avg.hr.earn <- log(avg.hr.earn)
l.emp25to54 <- log(emp25to54)
l.emp15to64 <- log(emp15to64)
l.unemp <- log(unemp)
l.cpi <- log(cpi)

#visualize log variables
par(mfrow=c(2,3))
plot(l.emp.pop, type='l',col=1)
plot(l.avg.hr.earn, type='l',col=2)
plot(l.emp25to54, type='l',col=3)
plot(l.emp15to64, type='l',col=4)
plot(l.unemp, type='l',col=5)
plot(l.cpi, type='l',col=6)

# Create sub-sample removing last 8 obs.
ntot = length(l.emp.pop)
l.emp.pop.train = l.emp.pop[1:(ntot-8)]
l.avg.hr.earn.train = l.avg.hr.earn[1:(ntot-8)]
l.emp25to54.train = l.emp25to54[1:(ntot-8)]
l.emp15to64.train = l.emp15to64[1:(ntot-8)]
l.unemp.train = l.unemp[1:(ntot-8)]
l.cpi.train = l.cpi[1:(ntot-8)]

#seasonality code is from his example.  my data is sesonally corrected so no issue
# Clear evidence of seasonality, which will 
# compromise the test results, so will include seasonal dummies
# to "deseasonalize".

source(file="seas.R")
n = length(l.emp15to64.train)
s = seas(n,12)

# "Seasonally adjusting"
#llt = embed(lt,5) # one year's worth of lags
#llc = embed(lc,5)
#llf = embed(lf,5)


# rsc = lm(dc~s$seas[2:n,1:3])
# summary(rsc)


#check order of integration of log variables
#Check for Order of Integration
source("intord.R")
intord(l.emp.pop)
intord(l.avg.hr.earn)
intord(l.emp25to54)
intord(l.emp15to64)
intord(l.unemp)
intord(l.cpi)
#first two variables need to be differenced once
#other 4 variables don't need to be differenced, but could be

#difference variables then look at summary stats of differenced, log variables

# summary stats of logged variables
summary(l.emp.pop.train); sd(l.emp.pop.train)
summary(l.avg.hr.earn.train); sd(l.avg.hr.earn.train)
summary(l.emp25to54.train); sd(l.emp25to54.train)
summary(l.emp15to64.train); sd(l.emp15to64.train)
summary(l.unemp.train); sd(l.unemp.train)
summary(l.cpi.train); sd(l.cpi.train)

# Cointegration
# Johansen test
library(urca)
library(vars)

# First step in Johansen test is determine order of the VAR undifferenced
yy = cbind(l.avg.hr.earn.train, l.cpi.train, l.emp15to64.train)
# lag selection criteria
VARselect(yy, lag.max=8, type="const", season=12)

# Now perform Johansen tests:
jc <- ca.jo(yy, type="eigen", K=6,ecdet="const", season=4) 
summary(jc)
jct <- ca.jo(yy, type="trace", K=6, ecdet="const", season=4) 
summary(jct)

# Engle-Granger:
rc = lm(l.avg.hr.earn.train ~ l.cpi.train + l.emp15to64.train)
summary(rc)
res1 = rc$residuals
intord(res1)

rc2 = lm(l.cpi.train ~ l.avg.hr.earn.train + l.emp15to64.train)
summary(rc2)
res2 = rc2$residuals
intord(res2)

rc3 = lm(l.emp15to64.train ~ l.avg.hr.earn.train + l.cpi.train)
summary(rc3)
res3 = rc3$residuals
intord(res3)











#Follow his video from Tuesday
#Check for cointegration
#check for seasonality
#Check for serial correlation
#build dynamic regression
#error correction mechanism
#Subset data
#forecast
#judge forecast
#build hybrid forecast

