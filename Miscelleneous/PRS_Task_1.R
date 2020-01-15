# Name: Shelmith Nyagathiri Kariuki
# Project: PRS (Self Development)
# Task Number: Task Number 1 (Reshaping Data)
# Date: 23/07/2018

## i). Setting the working directory
setwd("Documents/Github/")

## ii). Loading the required packages
pkgs<-c("dplyr", "ggplot2","tidyr","tidyverse")

miss_pkgs <- pkgs[!pkgs %in% installed.packages()[,1]] # vector of missing packages

### install the missing packages
if(length(miss_pkgs)>0){
  install.packages(miss_pkgs)
}
### load the packages
lapply(pkgs,library,character.only=TRUE)

## 1. Read in the data
SalesData<-read.csv("SalesData.csv")
SalesData<-SalesData[,-1]

## 2. Reshape the data, from wide to long, to contain the following columns: County, Year, Quarters and Sales.
SalesData_long<-SalesData%>%
  gather("Quarters","Sales",-Year)

## 3. Calculate the average yearly sales.
sales_mean<-SalesData_long%>%
  group_by(Year)%>%
  summarise(Sales = mean(Sales))

## 4. Draw a neatly labelled line plot, that shows the average sales for each year.
mean_sales1<-ggplot(data = sales_mean,
                    aes(x = as.factor(Year), y=Sales,group = 1))+
  geom_line(color = "red",size =1.5)+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.line=element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0.5),
        plot.subtitle = element_text(size = rel(1.4), hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.text.x=element_text(vjust = 1,angle = 30,hjust = 0.9),
        axis.title = element_text(size =12),
        panel.background = element_rect(fill = NA))+
  labs(title ="Average Sales Values per Year", x = "Year", y = "Average Sales")
mean_sales1

## 5. Draw a neatly labelled line plot, that shows the quarterly sales for each year.

mean_sales2<-ggplot(data = SalesData_long,
                    aes(x = as.factor(Year), y=Sales,group = Quarters,color =Quarters))+
  geom_point()+
  geom_line(size =1.5)+
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.line=element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0.5),
        plot.subtitle = element_text(size = rel(1.4), hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.text.x=element_text(vjust = 1,angle = 30,hjust = 0.9),
        axis.title = element_text(size =12),
        panel.background = element_rect(fill = NA))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title ="Quarterly Sales Values per Year", x = "Year", y = "Sales")
mean_sales2


