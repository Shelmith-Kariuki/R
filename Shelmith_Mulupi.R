library(tidyverse)

df <- data.frame(country=c("1", "2", "3"), 
                 cost_care= as.integer(c(100, 200, 400)), 
                 home_ownership=as.numeric(c(1, 2, 10)), 
                 age_x= as.character(c("20", "24", "40")), 
                 weight_x= as.integer(45, 50, 64), 
                 day_of_week=c(1, 2,3))

## Convert cost_care, weight_x and age_x to numeric(dbl) variables
df  <- df %>% 
  mutate(cost_care = as.numeric(cost_care),
         weight_x = as.numeric(weight_x),
         age_x = as.numeric(age_x))

## Advanced way: all three in one
# df  <- df %>% 
#   mutate(across(c(cost_care, weight_x, age_x), ~as.numeric(.)))

## Convert day_of_week and home_ownership into integer variables
df  <- df %>% 
  mutate(day_of_week = as.integer(day_of_week),
         home_ownership = as.integer(home_ownership))

## Advanced way: all two in one
# df  <- df %>% 
#   mutate(across(c(day_of_week, home_ownership), ~as.numeric(.)))

reprex()
