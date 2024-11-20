library(tidyverse)
library(janitor)
library(scales)
# library(tidyr)
# library(readr)
#library(viridisLite)

avocado_data <- read_csv("data/avocado.csv")
avocado_data <- clean_names(avocado_data)

colSums(is.na(avocado_data))
sum(is.na(avocado_data))

#Univariate analysis
mean(avocado_data$average_price)
median(avocado_data$average_price)

ggplot(avocado_data, aes(x=average_price)) +
  geom_density(fill="red", alpha=0.5) +
  ggtitle("Distribution Price") +
  theme_minimal()

ggplot(avocado_data, aes(x=average_price)) +
  geom_boxplot(fill= "darkgreen")+
  ggtitle("Distribution Price") +
  theme_minimal()

ggplot(avocado_data, aes(x=average_price))+
  geom_histogram(fill="lightgreen")+
  ggtitle("Price Distribution")+
  theme_minimal()

#Categorical Data Analysis
avocado_data %>%
  ggplot(aes(year))+
  geom_bar()

avocado_data %>%
  ggplot(aes(type))+
  geom_bar()

#Bivariate DA
ggplot(avocado_data, aes(x = average_price, y = type))+
  geom_boxplot(fill = c("#704012", "#4a7337"))+
  theme_minimal()

ggplot(avocado_data, aes(x = average_price, y = factor(as.character(year))))+
  geom_boxplot(fill = c("#704012", "#4a7337", "#302341","#9e4845" ))+
  theme_minimal()+
  labs(x="Average Price", y = "Year")

#Filter for only organic 
data_organic <- avocado_data%>%
  filter(type == "organic")

data_conventional <- avocado_data%>%
  filter(type == "conventional")

View(avocado_data)

#Homework
# Filter by year to see the total volumes of each avocado type sold
by_type_total_volume <- avocado_data %>%
  filter(year == "2015") %>%
  group_by(type) %>%
  summarise(total_volume_type = sum(total_volume)) %>% 
  mutate(total_vol_type_str = scales::number(
      total_volume_type,big.mark ="," )
    )

by_type_total_volume

#Then visualize it - total_volume_type vs type from the
# by_type_volume
ggplot(by_type_volume, aes(x=total_volume_type, y = type))+
  geom_bar(stat = "identity",fill = c("yellow", "green"))+
  ggtitle("Total Volume of Avocadoes by Type")+
  theme_minimal()+
  coord_flip()

by_type_total_avg_price <- avocado_data %>%
  filter(year == "2015") %>%
  group_by(type) %>%
  summarise(total_avg_price = sum(average_price)) %>% 
  mutate(total_avg_price_str = scales::dollar(
    total_avg_price))

by_type_total_avg_price

#Then visualize it - total_volume_type vs type from the
# by_type_total_avg_price
ggplot(by_type_total_avg_price, aes(x=total_avg_price,
                                    y = type))+
  geom_bar(stat = "identity",fill = c("yellow", "green"))+
  ggtitle("Total Avg Price of Avocadoes by Type")+
  theme_minimal()+
  coord_flip()

# ggplot(avocado_data, aes(x = type, y = total_volume))+
#   geom_line()+
#   theme_minimal()

avocado_data%>%
  count(region)%>%
  arrange(desc(n))%>%
  ggplot(aes(y = fct_reorder(region,n), x = n))+
  geom_bar(stat = "identity")


date_breakdown <- avocado_data %>%
  mutate(week_day = lubridate::day(date), 
         wk_day_name = wday(date, 
                                                                    label = TRUE,
                                                                    abbr = TRUE),
         date_month = month(date, label=TRUE, abbr = FALSE), date_year = year(date)) %>%
  select(date, week_day, wk_day_name, date_month, everything())

date_breakdown

date_breakdown %>%
  count(date_month)%>%
  arrange(desc(n)) %>%
  ggplot(aes(fct_reorder(date_month, n, .desc = T), n))+
  geom_bar(stat = "identity") 


ggplot(avocado_data, aes(x = total_volume, y = type))+
  geom_boxplot(fill = c("red", "blue"))+
  theme_minimal()

ggplot(avocado_data, aes(x=total_volume))+
  geom_density(fill="purple")+
  ggtitle("Total Volume Distribution")+
  theme_minimal()




#we use geom_line for trends 


