library(ggplot2)
library(dplyr)
data <- read.csv('../data/siteA.csv')
head(data)
total_daily_revenue = data %>% group_by(Date_Name, subCategory) %>% 
  summarise(daily_revenue = sum(sales)) 

# Boxplots for all categories
total_daily_revenue %>%
  ggplot(aes(x=reorder(subCategory, daily_revenue), y=daily_revenue)) + geom_boxplot()

total_daily_revenue %>%
  ggplot(aes(x=reorder(subCategory, daily_revenue, median), y=daily_revenue)) + geom_boxplot()

total_daily_revenue %>% 
  ggplot(aes(x=subCategory)) + geom_bar()

total_daily_revenue %>% 
  ggplot(aes(x=daily_revenue)) + geom_density(aes(color=subCategory))
  # geom_histogram(binwidth=1000, aes(fill=subCategory)) +
  # facet_wrap( ~ subCategory)
