################### 
#### Title: Collapsing and Grouping Data
#### Author: Shelmith Kariuki
#### Project: 2018 Q4 Data Practicum
#### Date created: 11th November, 2018


##*******************************dplyr*****************************************
# dplyr: Package used for data manipulation.


## There are five dplyr verbs that are commonly used.
### select() :    Used for selecting variables.
### filter() :    Used for selecting rows based on certain conditions.
### mutate() :    Used for generating new variables.
### group_by():   Used for grouping data based on certain conditions.
### summarise():  Used for summary statistics.
### piping (%>%): Used for performing sequential tasks.
  #### I came %>% I saw %>% I conquered

## Loading the dplyr package.
library(dplyr)

## Reading in the data
### Change the path below to suit the location of your dataset
loans_df<-read.csv("/Users/shelmith/Documents/Personal Development/R/Loans.csv")

###1. select() : 

#### Example 1: selecting variables that we need.
loans_df1 <- loans_df %>%
  select(Loan_ID,Gender,Married, Dependents,Education,Self_Employed)

#### Example 2: dropping variables that we do not need.
loans_df2 <- loans_df %>%
  select(-CoapplicantIncome, -Credit_History)

#### Example 3: selecting variables that follow a certain pattern.
loans_df3 <- loans_df %>%
  select(starts_with("C"), ends_with("e"),contains("Amount"))

###2. filter() : 

#### Example 1: Select all females who are graduates

female_graduates <- loans_df %>%
  filter(Gender == "Female" & Education == "Graduate")

#### Example 2: Select all loan applicants who have a loan greater 
  ##### than 10,000 and have not repaid it yet.
loan_nonrepaid <- loans_df %>%
  filter(LoanAmount < 10000 & Loan_Status == "N")

###3. mutate() :

#### Example 1: Generate a variable that is the sum of the applicant income, and co-applicant income.

loans_df <- loans_df %>%
  mutate(summed_income = ApplicantIncome + CoapplicantIncome)

###4. group_by() and summarise():

#### Example 1: What is the distribution of loan status?

loanstatus_count <- loans_df %>%
  group_by(Loan_Status) %>%
  summarise(count = n())

#### Example 2: Calculate the mean loan amount for the different Education categories.

mean_loanamount <- loans_df %>%
  filter(Loan_Status=="Y") %>%
  group_by(Education)%>%
  summarise(mean_loan = mean(LoanAmount,na.rm = T))

#### Example 3: What percentage of loan applicants live in the rural areas?

rural_perc <- loans_df %>%
  group_by(Property_Area) %>%
  summarise(count = n())%>%
  mutate(perc =round((count / sum(count))*100,1))

##*******************************tidyr*****************************************


# tidyr: Package used for reshaping data.


### spread() : Converts data from long to wide.
### gather() : Converts data from wide to long.

## Loading the tidyr package.
library(tidyr)

## Simulate a treatment, control dataset showing number of respondents pre treatment arm,
 ### with four variables, Region, Control, Treatment1_GoldCoin and Treatment2_Messages.

set.seed(45) ## These makes it possible to replicate results obtained from sample() function.
Region = c("Kibera","UoN","Kawangware","Kirinyaga")
Control<-sample(50:90,4,replace = F)
Treatment1_GoldCoin<-sample(50:90,4,replace=F)
Treatment2_Messages<-sample(50:90,4,replace=F)

treatment_df <- data.frame(Region, Control, Treatment1_GoldCoin,Treatment2_Messages)


## 1: gather() 

### Example 1

treatment_df_long <- treatment_df %>%
  gather(Treatment_Category, No_of_Respondents, -Region)

###Quiz1: How many repondents are in the control group, in total?
###Quiz2: Where do most of the respondents come from?

## 2: spread() 

### Example 1

treatment_df_wide <- treatment_df_long %>%
  spread(Treatment_Category, No_of_Respondents)


##*******************************Assignment*****************************************

## Read in the wafanyikazi dataset
## NB: This dataset is hypothetical. I simulated all of these values.


## Question 1: Calculate the minimum value, maximum value,  median,
### and Mean of age and income in wafanyikazi dataset.


## Question 2: Are there more males than females in the dataset? What is the % gender gap?



## Question 3: How many single people ladies come from Nyeri? What is the mean age, 
### and median income of these ladies



## Question 4: How many juniors are below the age of 26,except those in the Data Department
### and those in Operations? How many of these are in each department?


## Question 5: Calculate the mean income, per department per gender group.


## Question 6: Which department has the largest difference in mean income,between males and females?



## Question 7:

### PIU HGSF recently conducted a Quick Polls Survey recently. Among the questions asked was 
### "What role do you think you have to play in ensuring the success of the program in your community?"
### The question was a multiple choice question. Tabulate the responses given. 
### NB: Only CEq17 = "Yes", were asked that question.



## Question 8: You are provided with the following data.

Year <- c(2010,2011,2012,2013,2014)
Q1 <-c(1003,1532,954,841,823)
Q2 <-c(1359,933,992,1434,1034)
Q3 <-c(1326,904,845,1480,1184)
Q4 <-c(1122,1479,889,1174,1317)

sales = data.frame(Year,Q1 ,Q2, Q3, Q4)

## Question 8.1. Calculate the total sales per year.

## Question 8.2 Calculate the average sales per quarter.


## Question 9 (Optional): For the weather dataset shared, reshape the data to result 
### to a dataset with the columns id,year, month,day,tmax and tmin. 
### Hint: The dataset should be a dataframe of 33 rows and 6 columns.


