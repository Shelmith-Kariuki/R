## Creating maps with R
## ISSU-fifth day
## 27/09/2019
## Jan-Philip Kolb

## Install the maptools package
## install.packages("maptools")

## load the package
library(maptools)

## use the wrld data in the maptools package
data("wrld_simpl")
dat <- wrld_simpl@data
View(dat)

## plot the map
plot(wrld_simpl)

## plot one country 
plot(wrld_simpl[2,])

alg <- wrld_simpl[2,]
plot(alg)

## to see the regions that are available
table(wrld_simpl$REGION)

## plot Africa
plot(wrld_simpl[wrld_simpl$REGION==2,])

africa <- wrld_simpl[wrld_simpl$REGION==2,]
plot(africa, col="yellow")
plot(africa, col=rgb(1,0,0,0.5))

## Access the countries in africa
africa$NAME

## plot Uganda
plot(africa[50,],col = "yellow")

## plot Kenya
plot(africa[22,],col = "green")


## plot several countries together

TUK <- africa[c(49,50,22),]
plot(TUK,col=c(1,2,3))
plot(TUK,col=c("yellow","black","red"))


## install.packages("raster")
library(raster)

## Get UGA district data
UGA <- getData("GADM",country="UGA",level=1)
##level0 = state, level1 = districts, level2=municipality
## The higher the level numbers, the bigger the dataset

## assess the data
dat1 <-UGA@data

## plot the data
plot(UGA)

## Get
UGA2 <- getData("GADM",country="UGA",level=2)
dat2 <-UGA2@data

## plot the data
plot(UGA2)

##bonus, from the day I was at the airport
africa <- wrld_simpl[wrld_simpl$REGION==2,]
myCountries = africa$NAME %in% c("Kenya", "Tanzania", "Uganda", "Ghana", "Nigeria", "Congo", "Egypt")
plot(africa, col = c(gray(0.9), "green")[myCountries+1])