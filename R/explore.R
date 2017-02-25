library(ggplot2)
library(dplyr)
data <- read.csv('../data/siteA.csv')
head(data)
total_daily_revenue = data %>% group_by(Date_Name) %>% 
  summarise(daily_revenue = sum(sales)) 
ggplot(total_daily_revenue, aes(x=Date_Name, y=daily_revenue)) + geom_point()
