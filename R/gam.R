library(dplyr)
library(mgcv)
library(zoo)

data <- read.csv('../data/siteA.csv')
head(data)

total_daily_revenue = data %>% group_by(Date_Name) %>% 
  summarise(daily_revenue = sum(sales))
head(total_daily_revenue)

total_daily_revenue$Date_Name[1]
total_daily_revenue$nDate = seq(1:346)
total_daily_revenue$nDate


total_daily_revenue$Date_Name = as.Date(total_daily_revenue$Date_Name,format='%Y-%m-%d')
total_daily_revenue$Date_Name = as.POSIXct(total_daily_revenue$Date_Name)
# na.locf(total_daily_revenue)
sapply(total_daily_revenue, function(x) sum(is.na(x))) # Check no. of missing values


plot(daily_revenue ~ Date_Name, data=total_daily_revenue, 
     type="l", ylab='sales', main = "Daily Sales")


m <- gamm(daily_revenue ~ s(nDate, bs='cc'), data=total_daily_revenue)
summary(m$gam)
layout(matrix(1:2, ncol = 2))
plot(m$gam, scale = 0)
layout(1)

layout(matrix(1:2, ncol = 2))
acf(resid(m$lme), lag.max = 36, main = "ACF")
pacf(resid(m$lme), lag.max = 36, main = "pACF")
layout(1)

want <- seq(1, nrow(total_daily_revenue), length.out = 90)
pdat <- with(total_daily_revenue, data.frame(Date = Date_Name[want], nDate = nDate[want]))
## Predict trend contributions
p  <- predict(m$gam,  newdata = pdat, type = "terms", se.fit = TRUE)
pdat <- transform(pdat, p=p$fit[,2],  se=p$se.fit[,2])








