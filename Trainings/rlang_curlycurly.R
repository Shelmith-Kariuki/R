## Author: Shelmith Kariuki
## Date: 22/01/2020
## Description: Using the rlang's curly curly operator in user defined functions.

##  0. Install and load the packages required
pkgs <- c("tidyverse","data.table","DT","lubridate","ggthemes",
          "randomForest","e1071","rpart","pROC","caret","rlang")## create a vector of packages to be installed
miss_pkgs <- pkgs[!pkgs %in% installed.packages()[,1]]## Check if there are packages you want to load, that are not already installed.  

if(length(miss_pkgs)>0){
  install.packages(miss_pkgs)
}## Installing the missing packages

invisible(lapply(pkgs,library,character.only=TRUE))## Loading all the packages

rm(miss_pkgs,pkgs)## Remove the objects that are no longer required
library(rChambua)

##  1. Load the wafanyikazi dataset
df <- wafanyikazi
  
##  2. Writing a function that displays the frequencies of each of the categorical variables.
# freq_func <- function(dataframe, var){
#   tab <- dataframe %>% 
#     group_by({{ var }}) %>% 
#     count(name = "Frequency")
# }

freq_func <- function(dataframe, var){
  
  tab <- dataframe %>% 
    group_by(!!as.symbol(var)) %>% 
    count(name = "Frequency")
}

##  3. Evaluating the funcion on each of the variables.
tab1 <- freq_func(df,"Gender")
tab2 <- freq_func(df, "Role")

##  4. Evaluating the function on all the variables at once, using a loop.

vars <- c("Gender","Role","Department","County","Promotion")
tab_list <- list()

for(i in 1: length(vars)){
  tab_list[[i]] <- freq_func(df, vars[i])
}

tab_list <- rbind(tab_list)

