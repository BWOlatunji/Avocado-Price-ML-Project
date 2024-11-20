library(tidyverse)
library(janitor)
# library(gridExtra)
# library(grid)
# library(viridis)

# import data
avocado_data <- read_csv("data/avocado.csv") %>% 
  select(-1) %>% clean_names()



# EDA
## **Understanding the Structure of the Data**
str(avocado_data)

### Get a quick summary
summary(avocado_data)

## **Missing Data Exploration**

# Check for missing values 
colSums(is.na(avocado_data))
sum(is.na(avocado_data))

## Univariate analysis of numeric variables
# Average mean
mean(avocado_data$average_price)
median(avocado_data$average_price)


# Create the plot
ggplot(avocado_data, aes(x = average_price)) +
  geom_density(fill = "#4a7337", alpha = 0.5) +
  ggtitle("Distribution Price") +
  theme_minimal()


ggplot(avocado_data, 
       aes(x = average_price)) +
  geom_boxplot(fill= "#704012") +
  ggtitle("Distribution Price") +
  theme_minimal()

ggplot(avocado_data, aes(x = average_price)) +
  geom_histogram(fill = "#4a7337", alpha = 0.5) +
  ggtitle("Distribution Price") +
  theme_minimal()


## **Categorical Data Analysis**
avocado_data %>% 
  ggplot(aes(year))+
  geom_bar()

avocado_data %>% 
  ggplot(aes(type))+
  geom_bar()



# Bivariate data analysis
ggplot(avocado_data, aes(x = average_price, 
                         y = type)) +
  geom_boxplot(fill = c("#704012", "#4a7337")) +
  theme_minimal()

ggplot(avocado_data, aes(x = average_price, 
                         y = factor(
                           as.character(year)
                           )
                         )
       ) +
  geom_boxplot(fill = c("#704012", "#4a7337", "#6b8c21", "#ddd48f")) +
  theme_minimal()+
  labs(x="Average Price", y="Year")


# Filter the data to only include 'organic' type
data_organic <- avocado_data %>% 
  filter(type == 'organic')

data_conventional <- avocado_data %>% 
  filter(type == 'conventional')


