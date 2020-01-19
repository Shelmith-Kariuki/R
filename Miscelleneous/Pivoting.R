## Name: Shelmith Kariuki
## Date: 20/01/2020
## Description: This file shows comparison between the old reshape functions
### (spread(), gather()) and the new functions (pivot_wider(), pivot_longer())

### 0. Load the packages that we need
library(dplyr)
library(tidyr)

### 1. The data generated below shows the quarterly perfomance of students, in a certain campus

set.seed(2020)

cols<-c("2011Q1","2011Q2","2011Q3","2011Q4",
        "2012Q1","2012Q2","2012Q3","2012Q4",
        "2013Q1","2013Q2","2013Q3","2013Q4",
        "2014Q1","2014Q2","2014Q3","2014Q4",
        "2015Q1","2015Q2","2015Q3","2015Q4")
rows<-c("Males","Females")
data<-data.frame(matrix(sample(100:500,length(cols)*length(rows),replace = F),ncol=length(cols), nrow = length(rows), byrow = T))
colnames(data) = cols
rownames(data)=rows

### 2. Reshape this data from wide to long

#### 2.1 Old method, gather()
old_long <- data %>% 
  mutate(Gender = rownames(data)) %>% 
  gather("Year_Quarter","No_of_Students",-Gender) %>% 
  mutate(Year = substr(Year_Quarter,1,4),
         Quarter = substr(Year_Quarter,5,6)) %>% 
  select(Gender, Year, Quarter, No_of_Students,-Year_Quarter)

#### 2.2 New method, pivot_longer()
new_long <- data %>% 
  mutate(Gender = rownames(data)) %>% 
  pivot_longer(-Gender, names_to = "Year_Quarter",values_to = "No_of_Students") %>% 
  mutate(Year = substr(Year_Quarter,1,4),
         Quarter = substr(Year_Quarter,5,6)) %>% 
  select(Gender, Year, Quarter, No_of_Students,-Year_Quarter)


### 3. Reshape the long datasets, back to wide

#### 3.1 Old method, spread()

old_wide <- old_long %>% 
  unite(Year_Quarter,Year,Quarter,sep = "") %>% 
  spread("Year_Quarter","No_of_Students")

#### 3.2 New method, gather()
new_wide <- new_long %>% 
  unite(Year_Quarter,Year,Quarter,sep = "") %>%
  pivot_wider(names_from = "Year_Quarter",values_from = "No_of_Students")
  

## Reshaping from wide to long
gather("New variable name to contain the columns to be collapsed","New column name to contain the current values in the columns to be collapsed") 
pivot_longer(names_to = "New variable name to contain the columns to be collapsed",values_to = "New column name to contain the current values in the columns to be collapsed")

## Reshaping from long to wide
spread("The variable whose values will form the new columns","The current variable containing the values that will be the data points of the new columns") 
pivot_wider(names_from = "The variable whose values will form the new columns",values_from ="The current variable containing the values that will be the data points of the new columns") 

