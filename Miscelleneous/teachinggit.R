library(tidyverse)

df <- rChambua::wafanyikazi

tab1 <- df %>% 
  group_by(Gender, Role) %>% 
  summarise(Count = n()) %>% 
  mutate(perc = round(Count/ sum(Count)*100,1))

tab2 <- df %>% 
  group_by(Role, Gender) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = round(Count/ sum(Count)*100,1))

df2 <- df %>% 
  rename(Province = County,
         Umri = Age)

Year <-c(2011:2014)
Q1 <- c(50,60,70,80)
Q2 <- c(121,123,NA,130)
Q3 <- c(72,45,63,78)
Q4 <- c(34,56,23,54)

sales_df <- data.frame(Year, Q1,Q2,Q3,Q4)

## Reshaping the dataset from wide to long

## Method 1: Using gather()
sales_long1 <- sales_df %>% 
  gather(Quarters,Sales,-Year, na.rm = TRUE)

## Method 2: pivot_longer()
sales_long2 <- sales_df %>% 
  pivot_longer(cols = c(Q1,Q2,Q3,Q4),
               names_to = "Quarters",
               values_to = "Sales",
               values_drop_na = TRUE)

## Reshaping the dataset from long to wide

## Method 1: Using spread()

sales_wide1 <- sales_long1 %>% 
  spread(Quarters, Sales)

## Method 2: Using pivot_wider()
sales_wider2 <- sales_long2 %>% 
  pivot_wider(names_from = "Quarters",
              values_from = "Sales")

## Question X: You are provided with the following dataset.

Year <- c(2010,2011,2012,2013,2014)
Q1 <-c(1003,1532,954,841,823)
Q2 <-c(1359,933,992,1434,1034)
Q3 <-c(1326,904,845,1480,1184)
Q4 <-c(1122,1479,889,1174,1317)

sales = data.frame(Year,Q1 ,Q2, Q3, Q4)

## Question X.1 Calculate the total sales per year.


## Question X.2 Calculate the average sales per quarter.

