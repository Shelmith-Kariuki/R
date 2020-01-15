## Loading the dplyr package

library(dplyr)

## Functions found in dplyr

## %>% : pipe operator
## filter(): select observations
## select(): select variables
## mutate(): used for generating variables
## group_by(): used for grouping the data, by certain variables
## summarise(): used for calculation of summary stats


## datarium::heartattack
heart <- datarium::heartattack

## %>% 

## Woke up %>% showered %>% brushed %>% appliedlotion %>% breakfast %>% kids

##1. filter()

males_data <- heart %>% 
              filter(gender == "male")

males_data<- heart %>% 
              filter(gender != "female")

females_data <- heart %>% 
              filter(gender != "male")

cholestrol_data <- heart %>% 
              filter(cholesterol > 5)

## Q1: Generate a dataset that only contains drug B
## Q2: Generate a dataset that contains only males who are low risk
### Hint: filter(Gender == "males" & risk == "low")
## Q3: 

##2. select():
gender_drug <- heart %>% 
  select(gender, drug)

dropped_data <- heart %>% 
  select(-cholesterol, -id)

## Q4: Generate a dataset that only contains drug A, and only keep
## the gender and risk variables. 

Q4 <- heart %>% 
  filter(drug == "A") %>% 
  select(gender, risk)
## Q5:
## Q6:

##2. mutate():
heart <- heart %>% 
  mutate(Jinsia = ifelse (gender == "male","mme","mke"))

heart$Jinsia2 = ifelse(heart$gender == "male","mme","mke")

heart <- heart %>% 
  mutate(cholesterol_cat = ifelse(cholesterol >5,">5","<5"))

## Q7:
## Q8:





