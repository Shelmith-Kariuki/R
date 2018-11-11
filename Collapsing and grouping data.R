################### 
#### Title: Collapsing and Grouping Data
#### Author: Shelmith Kariuki
#### Project: 2018 Q4 Data Practicum
#### Date created: 11th November, 2018

# Lesson 

# dplyr: Package used for data manipulation.
library(dplyr)

## There are five dplyr verbs that are mostly used.
### select() :    Used for selecting variables.
### filter() :    Used for selecting rows based on certain conditions.
### mutate() :    Used for generating new variables.
### group_by():   Used for grouping data based on certain conditions.
### summarise():  Used for summary statistics.
### piping (%>%): Used for performing sequential tasks.

# tidyr: Package used for reshaping data.

### spread() : Converts data from long to wide
### gather() : Converts data from wide to long


# Assignment
## Read in the wafanyikazi dataset
## NB: This dataset is hypothetical. I simulated all of these values.

wafanyikazi <- read.csv("/Users/shelmith/Documents/Personal Development/R/wafanyikazi.csv")

## Question 1: Calculate the minimum value, maximum value,  median,
## and Mean of age and income in wafanyikazi dataset.

age_summstats <- wafanyikazi %>%
  select(Age)%>%
  summarise(min_value = min(Age),
            max_value = max(Age),
            median_value = median(Age),
            mean_value = mean(Age)) 

Income_summstats <- wafanyikazi %>%
  select(Income) %>%
  summarise(min_value = min(Income),
            max_value = max(Income),
            median_value = median(Income),
            mean_value = mean(Income)) 

## Question 2: Are there more males than females in the dataset? What is the % gender gap?

gender_count<- wafanyikazi %>%
  group_by(Gender)%>%
  summarise(count = n())%>%
  mutate(percentage = round((count/sum(count))*100,1))%>%
  mutate(gender_gap = abs(lead(percentage)-percentage))
### Males are more, by a difference of 5.2%

## Question 3: How many single people ladies come from Nyeri? What is the mean age, 
###and median income of these ladies

single_nyeri <- wafanyikazi %>%
  filter(Gender == "Female" & Marital_Status == "Single" & County == "Nyeri")
### 12 of them

single_nyeri_counts <- single_nyeri%>%
  summarise(mean_age = mean(Age),
            median_income = median(Income))
###Mean age is 36
###Median Income is 5557

## Question 4: How many juniors are below the age of 26,except those in the Data Department
## and those in Operations? How many of these are in each department?

junior_26 <- wafanyikazi %>%
  filter(Role == "Junior" & Department != "Data" & Department != "Operations" & Age <26)

### 20 people

junior_26_dept <- junior_26 %>%
  group_by(Department)%>%
  summarise( count = n())

### 5 Associates, 11 in finance, and 4 research analysts.


## Question 5: Calculate the mean income, per department per gender group.

gender_income <-  wafanyikazi %>%
  group_by(Department, Gender)%>%
  summarise(mean_income = mean(Income))



## PIU HGSF recently conducted a Quick Polls Survey recently. Among the questions asked was 
## "What role do you think you have to play in ensuring the success of the program in your community?"
## The question was a multiple choice question. Tabulate the responses given. 
## NB: Only CEq17 = "Yes", were asked that question.

HGSF_QP = read.csv("/Users/shelmith/Documents/Personal Development/R/HGSF_QP.csv")

HGSF_QP<-HGSF_QP %>%
  filter(CEq17 == "Yes")%>%
  gather("Role_Number","Roles",-sid,-X,-CEq17,na.rm = T)%>%
  group_by(Roles)%>%
  summarise(count = n())


## Question 6: Which department has the largest difference in mean income,between males and females?

diff_average_income <- wafanyikazi %>%
  group_by(Department, Gender) %>%
  summarise(mean_income = mean(Income))%>%
  spread(Gender,mean_income)%>%
  mutate(diff_mean_income = abs(Female - Male))

### Operations

## Question 7: You are provided with the following data.

Year <- c(2010,2011,2012,2013,2014)
Q1 <-c(1003,1532,954,841,823)
Q2 <-c(1359,933,992,1434,1034)
Q3 <-c(1326,904,845,1480,1184)
Q4 <-c(1122,1479,889,1174,1317)

sales = data.frame(Year,Q1 ,Q2, Q3, Q4)

## Question 7:1. Calculate the total sales per year.
## Question 7:2 Calculate the average sales per quarter.
## Question 7:3 Which year has the largest difference between Q1 sales and Q4 sales?
