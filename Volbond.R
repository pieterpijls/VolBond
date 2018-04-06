#------------------------------------------------------------
# Volatility Bond # PIETER PIJLS # CREATED: 4/4/2018 
#------------------------------------------------------------

rm(list=ls())

# Load packages
#--------------
packages <- c("data.table","mgcv","evtree", "classInt", "magrittr",
              "rgdal", "ggplot2", "ggmap","grid","gridExtra","xtable","sas7bdat",
              "plyr","knitr","kableExtra","xtable","readxl","xlsx","PerformanceAnalytics",
              "reshape","dplyr","gtable","ggforce","lattice","shinydashboard","rsconnect","rdrop2",
              "plotly", "shiny", "openxlsx","rsconnect")

suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))

# Import Data (swap rates)
setwd("C:/PIETER/INTERNSHIP P&V/Volbond")
rates <- read.xlsx("./EURIBOR vanilla interest rate swap contract.xlsx",colNames=TRUE,sheet=1)
# create date object
rates$DATE   <- seq(as.Date("2001/1/1"), by = "month", length.out = 206)

rates20 <- as.data.frame(cbind(seq(as.Date("2001/1/1"), by = "month", length.out = 206), rates$`20`))
rates20$V1 <- as.Date(rates20$V1)

# plot evolution swap rate 20y
ggplot(rates20, aes(x=rates20$V1,y=rates20$V2)) + geom_line() + ggtitle("Evolution swap rate 20Y") +
           xlab("Date") + ylab("Rate")


# Import data for the Volbond
volbond20 <- rates <- read.xlsx("./Volbond20Y.xlsx",colNames=TRUE,sheet=1) 
volbond20$DATE <- seq(as.Date("2001/1/1"), by = "month", length.out = 206)
volbond20 <- volbond20[-1,]

# plot evolution swap rate 20y and coupon 
ggplot(volbond20, aes(x=volbond20$DATE)) + geom_line(aes(y=volbond20$Advanced, colour = "EUR20Y")) + geom_line(aes(y=volbond20$Coupon, colour = "Coupon (%)")) + ggtitle("Evolution swap rate 20Y") +
           xlab("Date") + ylab("Rate") + theme_bw()

# histogram coupons
hist(volbond20$Coupon, breaks=20, xlab = "Coupon (%)")

# summary statistics coupons
summary(volbond20$Coupon)

# standard deviation
sd(volbond20$Coupon[12:nrow(volbond20)])

#-------------------------------------------------------------------------------------------------------------
#1# Example without shout option

n = 10000000 # Investment of 10mio
t = 15 # duration bond

coupon <- matrix(ncol = 3,nrow=t)

for (i in 1:t) # nyears(volbond20$DATE)
{
  coupon[i,1] <- as.Date(volbond20$DATE[12*i])
  coupon[i,2] <- volbond20$Coupon[12*i]
  coupon[i,3] <- coupon[i,2]*n
}

coupon[,1]<- as.Date(coupon[,1])

# total return
totalreturn <- (sum(coupon[,3])+n)/n
totalreturn

# yearly return
yearlyreturn <- totalreturn^(1/t)-1
yearlyreturn

#-------------------------------------------------------------------------------------------------------------
#2# Example with shout option (e.g. lock in rate from 3%)

n = 10000000 # Investment of 10mio
t = 15 # duration bond
shout = 0.03 # treshhold when rate is locked in

couponshout <- matrix(ncol = 3,nrow=t)

for (i in 1:t) # nyears(volbond20$DATE)
{
  couponshout[i,1] <- as.Date(volbond20$DATE[12*i])
  couponshout[i,2] <- volbond20$Coupon[12*i]
  couponshout[i,3] <- couponshout[i,2]*n
}

# loop to lock in values higher as 3%
for (i in 2:t)
{
if (couponshout[i-1,2]>shout)
  {
    couponshout[i,2] = couponshout[i-1,2]
    couponshout[i,3] = couponshout[i-1,3]
  }
  else
  {
   couponshout[i,2] = couponshout[i,2]
   couponshout[i,3] = couponshout[i,3]
  }
}

# total return
totalreturn <- (sum(couponshout[,3])+n)/n
totalreturn

# yearly return
yearlyreturn <- totalreturn^(1/t)-1
yearlyreturn



#-------------------------------------------------------------------------------------------------------------
#3# Example without shout option

n = 10000000 # Investment of 10mio
t = 15 # duration bond

coupon <- matrix(ncol = 13, nrow=t)

for (j in 1:13)
{
  
for (i in 1:t) # nyears(volbond20$DATE)
{
  coupon[i,j] <- volbond20$Coupon[(12*i)+j]
}

}  
  
colnames(coupon)<- as.Date(volbond20$DATE[13:25]) # from JAN02,FB02,MAR02,... to FEB03

# total return
totalreturn <- colSums(coupon)
totalreturn

# yearly return
yearlyreturn <- (1+totalreturn)^(1/t)
yearlyreturn

summary(yearlyreturn)

