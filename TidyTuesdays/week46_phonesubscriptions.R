# Author: Shelmith Kariuki (@Shel_Kariuki)
# Date: 12th November, 2020
# Project: #TidyTuesday
# Week: 46

#1. Load the packages required
library(tidyverse)
library(extrafont)
loadfonts()

#2. Get the Data
## Read in with tidytuesdayR package 
## Install from CRAN via: install.packages("tidytuesdayR")
## This loads the readme and all the datasets for the week of interest
## Either ISO-8601 date or year/week works!
#tuesdata <- tidytuesdayR::tt_load('2020-11-10')
tuesdata <- tidytuesdayR::tt_load(2020, week = 46)
mobile <- tuesdata$mobile # mobile data
landline <- tuesdata$landline # landline data

## Generate a dataset of Kenyan mobile subscriptions
kenya_df <- mobile %>% filter(entity == "Kenya")

#3. Read in the images that will be embedded on the plots
img <- jpeg::readJPEG("images/motorolla.jpeg")
motorolla <- grid::rasterGrob(img, interpolate=TRUE)
img <- png::readPNG("images/safaricom.png")
safaricom <- grid::rasterGrob(img, interpolate=TRUE)
img <- png::readPNG("images/mpesa-logo-AE44B6F8EB-seeklogo.com.png")
mpesa <- grid::rasterGrob(img, interpolate=TRUE)
img <- jpeg::readJPEG("images/kenya-3d-flag-vector.jpg")
kenya <- grid::rasterGrob(img, interpolate=TRUE)
img <- jpeg::readJPEG("images/firstandroid.jpg")
firstandroid <- grid::rasterGrob(img, interpolate=TRUE)


#4. Create a theme object that will be used on the chart
my_theme <- theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  axis.line = element_line(size = 1.5),
                  plot.title = ggtext::element_markdown(
                    fill = NULL,
                    face = 'bold',
                    box.colour = 'white',
                    size = 20,
                    hjust = 0.5),
                  plot.subtitle = ggtext::element_markdown(size = 28, colour = 'red'),
                  plot.caption = element_text(family = "Chilanka",size = rel(1)),
                  axis.text = element_text(family = "Chilanka", size = rel(0.8)),
                  axis.text.x = element_text(vjust = 1, hjust = 0.6),
                  axis.title = element_text(family = "Chilanka", size = rel(1)),
                  legend.text = element_text(family = "Chilanka", size = rel(0.8)),
                  plot.background = element_rect(fill = '#FFFEF2',  size = 4),
                  panel.background = element_rect(fill = NA))


#5. Generate the plot
my_plot <- ggplot(data = kenya_df, aes(x = as.factor(year), y = mobile_subs, group = 1))+
            geom_point(shape = 8, color = "black", size = 3,size = 3)+
            geom_line(color = "red")+
            my_theme +
            scale_y_continuous(breaks = seq(0, 110, by = 10)) + 
            labs(title = '<span style="font-family: Chilanka; color:red">Mobile phone subscriptions in Kenya</p>',
                  y = "Fixed mobile subscriptions \n(per 100 people)",
                  x = "Year",
                  caption = "Credit: @Shel_Kariuki\nData: Historical Phone Usage\nGraphic: #TidyTuesday Week 46") +

## Annotations---------------------------------------------------------------------  

## 5.1 An example of mobile phones used at the onset
  
  ## image of motorola c113 phone
  annotation_custom(motorolla, xmin=8, xmax=11, ymin=55, ymax=70) +
  
  ## description box
  geom_label(
    label='Motorolla C113: One of the initial \nmobile phones used in Kenya', 
    x=12,
    y=62,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.2,
    color = "red", size = 3) + 

## 5.2 Onset of mobile phone usage
  
  ## description box
  geom_label(
    label='In 2002 Kenya had just two \nmobile phones for every \n100 of its 38 million people', 
    x=10,
    y=40,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.2,
    color = "black", size = 3) +
  
  ## arrow connecting the point and the description box
  annotate(
    geom = "curve", x = 13, y = 3.5, xend = 10, yend = 35, 
    curvature = "-.3",
    arrow = arrow(length = unit(2, "mm")))+

## 5.3 Increase in subscriptions between 2002 and 2006  
  
  ## description box 
  geom_label(
    label='Between 2002 and 2006 \nthe number of mobile phones in Kenya \nincreased from 1 million to 10 million.', 
    x=15,
    y=40,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.2,
    color = "black", size = 3) +
  
  ## arrow showing the range between 2002 and 2006
  annotate(
    geom = "segment", x = 13, y = 4.5, xend = 17, yend = 20, colour = "blue",
    arrow = arrow(length = unit(2, "mm")),
      lty = 2)+
  
  ## arrow connecting the point and the description box
  annotate(
    geom = "segment", xend = 15, yend = 35, x = 15, y = 12, 
    arrow = arrow(length = unit(2, "mm"))) +

## 5.4  M-Pesa piloting (2005)
  
  ## description box
  geom_label(
    label='In October 2005, Safaricom and Vodafone launched M-PESA, \na mobile-based payment service targeting the un-banked, \npre-pay mobile subscribers in Kenya on a pilot basis', 
    x=21.5,
    y=5,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.2,
    color = "black", size = 3) +
  
  ## arrow connecting the point and the description box
  annotate(
    geom = "curve", x = 16, y = 12, xend = 18.2, yend = 4, 
    curvature = ".3",arrow = arrow(length = unit(2, "mm"))) +
  
  ## Safaricom image
  annotation_custom(safaricom, xmin=24.5, xmax=28, ymin=10, ymax=20) +
  
  ## Mpesa image
  annotation_custom(mpesa, xmin=24.5, xmax=28, ymin=0, ymax=10) +
  
## 5.3  M-Pesa official launch (2007)
  
  ## vertical line showcasing the year when M-PESA was officially launched
  geom_vline(xintercept = 18, color = "green", linetype = "dashed") +
  geom_text(aes(x=18, label="Official launch of M-PESA", y=70), colour="black", angle=90, vjust = -1.2) +
 
   ## description box
  geom_label(
    label='As a result of a successful pilot,\n M-PESA was officially launched\n in October 2007', 
    x=21,
    y=20,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.2,
    color = "black", size = 3) +
  
  ## arrow connecting the point and the description box
  annotate(
    geom = "curve", x = 18, y = 30, xend = 18.7, yend = 20, 
    curvature = ".3",arrow = arrow(length = unit(2, "mm"))) +

## 5.6  First android in Kenya (2010)
  ## First android in Kenya image
  annotation_custom(firstandroid, xmin=21, xmax=23, ymin=45, ymax=60) +
  
  ## arrow connecting the point and the image
  annotate(
    geom = "curve", x = 21, y = 60.2, xend = 21.5, yend = 57, 
    curvature = ".3",arrow = arrow(length = unit(2, "mm"))) +
  
  ## description box
  geom_label(
    label='The Huawei U8220 was the first Android \n smartphone to be launched in the Kenyan market.\nIt got to Nairobi in July, 2010', 
    x=25.5,
    y=55,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.2,
    color = "red", size = 3)+ 

## 5.7 Kenyan flag located at the top left of the chat  
  annotation_custom(kenya, xmin=1, xmax=3, ymin=80, ymax=90) 

my_plot
