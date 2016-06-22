---
title: "An analysis of the effects of extreme weather events on public health,
        property & crops in the US 1950-2011; RepData_PeerAssessment2"
author: "james"
date: "June 12, 2016"
output: html_document

---
# **SYNOPSIS:**

## Storms and other severe weather events can cause both public health and
## economic problems for communities and municipalities. These events
## can result in fatalities, injuries, property and crop damage. A key concern
## among policy makers is preventing such outcomes to the extent possible.

## This analysis involves exploring the U.S. National Oceanic and Atmospheric
## Administration's (NOAA) storm database between 1950 and 2011. This database
## tracks characteristics of major storms and weather events in the United
## States, including when and where they occur, as well as estimates of any
## fatalities, injuries as well as property and crop losses.

## This analysis found that tornados had the greatest impact on public health,
## floods caused the greatests economic losses to property and that drought
## caused the greatest losses to crops.

---
# **RAW DATA**

## The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:*

##      *Storm Data [47Mb]*
##      *There is also some documentation of the database available. Here you*
        *will find how some of the variables are constructed/defined.*

##      *National Weather Service Storm Data Documentation*
##      *National Climatic Data Center Storm Events FAQ*
##      *The events in the database start in the year 1950 and end in November*
        *2011. In the earlier years of the database there are generally fewer*
        *events recorded, most likely due to a lack of good records. More recent*
        *years should be considered more complete.*




#################################################
## 1 - **PROJECT - CONFIGURATION**

---
#### 1a. Set wd, options
setwd("~/Desktop/Coursera_R/RepData_PeerAssessment2")
options(echo = TRUE, cache = TRUE, scipen = 0)

---
#### 1b. Load packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(reshape2)
library(stringr)
library(testthat)



#################################################


## 2 - **ACCESS, DOWNLOAD & READ & PRE-PROCESS THE DATA**

---
#### 2a. Source data with url, download zipfile, unzip, read the .csv
#### data file, record date and time of download, print list of files #### in this directory
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipFile <- "StormData.csv.bz2"
if (!file.exists("StormData.csv.bz2")) {
        message(paste("Downloding", zipFile))
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileUrl, destfile="StormData.csv.bz2", method = "curl")
}else{
        message(paste("File exists;", zipFile))
}

---
#### 2b. Get time stamp on download
dateDownLoaded <- date()
dateDownLoaded

---
#### 2c. Unzip & read the downloaded files
rawstormdata <<- read.csv(bzfile("StormData.csv.bz2"), header=TRUE, stringsAsFactors = FALSE)

---
#### 2d. Print contents - current directory
ls()




#################################################


## 3 - **DATA PROCESSING**

---
#### 3a. Subset relevant columns to this analysis
newstormdata <<- rawstormdata[, c(8,23:28)]

---
#### 3b. Determine public health effects by event type for fatalities
dfhealth <- newstormdata[,c(1:3)]
totaldeaths <- dfhealth %>% group_by(EVTYPE) %>% summarise(sum(FATALITIES))
arrange(totaldeaths, desc(`sum(FATALITIES)`))

---
#### 3c. Determine public health effects by event type for injuries
dfhealth <- newstormdata[,c(1:3)]
totalinjuries <- dfhealth %>% group_by(EVTYPE) %>% summarise(sum(INJURIES))
arrange(totalinjuries, desc(`sum(INJURIES)`))

---
#### 3d. Write a function "useexp" that converts the "PROPDMGEXP"
#### & "CROPDMGEXP alpha codes to integers
useexp <- function(e) {
        if (e %in% c("H"))
                return(2)
        else if (e %in% c("K"))
                return(3)
        else if (e %in% c("M"))
                return(6)
        else if (e %in% c("B"))
                return(9)
        else if (!is.na(as.numeric(e)))
                return(as.numeric(e))
        else if (e %in% c("","+","?","-"))
                return(0)
        else {
                stop("Not a valid exponent")
        }
}

---
#### 3e. Make a smaller data subset of the property, convert the damage
#### exponent codes to all upper case lestter, apply the 'useexp' function
#### to convert the the exponential sums of losses into dollar amounts
#### then group by event type, then arrange the top 15 items in decreasing
#### magnitude and save as a new data set
dfproperty <- newstormdata[,c(1,4,5)]
dfproperty$PROPDMGEXP <- toupper(dfproperty$PROPDMGEXP)
dfproperty$PROPDMGEXP <- sapply(dfproperty$PROPDMGEXP, FUN=useexp)
dfproperty$loss <- dfproperty$PROPDMG * (10 ** dfproperty$PROPDMGEXP)
dfproperty2 <- dfproperty %>% group_by(EVTYPE) %>% summarise(sum(loss))
dfpropertylossbytype <<- arrange(dfproperty2,desc(`sum(loss)`))[1:15,]
dfpropertylossbytype


---
#### 3e. Make a smaller data subset of the crops, convert the damage
#### exponent codes to all upper case lestter, apply the 'useexp' function
#### to convert the the exponential sums of losses into dollar amounts
#### then group by event type, then arrange the top 15 items in decreasing
#### magnitude and save as a new data set
dfcrops <- newstormdata[,c(1,6,7)]
dfcrops$CROPDMGEXP <- toupper(dfcrops$CROPDMGEXP)
dfcrops$CROPDMGEXP <- sapply(dfcrops$CROPDMGEXP, FUN=useexp)
dfcrops$loss <- dfcrops$CROPDMG * (10 ** dfcrops$CROPDMGEXP)
dfcrops2 <- dfcrops %>% group_by(EVTYPE) %>% summarise(sum(loss))
dfcropslossbytype <<- arrange(dfcrops2,desc(`sum(loss)`))[1:15,]
dfcropslossbytype




#################################################


## 4 - **DISPLAY RESULTS**

---
#### 4a. Isolate the top ten values indicating total deaths and injuries by
#### type of weather event
totaldeaths <- arrange(totaldeaths, desc(`sum(FATALITIES)`))[1:10,]
totalinjuries <- arrange(totalinjuries, desc(`sum(INJURIES)`))[1:10,]

---
#### 4b. Plot with barplot, total fatalities and injuries caused by severe
#### weather event types
par(mfrow = c(1, 2), mar = c(11.5, 5.5, 3.1, 2.2), mgp = c(3, 1, 0), cex = 0.7, las = 3)
options(scipen = 0)
barplot(totaldeaths$`sum(FATALITIES)`, names.arg = totaldeaths$EVTYPE, col = 'brown', main = "Fatalities From Weather", ylab = "Number of Fatalities")
barplot(totalinjuries$`sum(INJURIES)`, names.arg = totalinjuries$EVTYPE, col = 'orange', main = "Injuries From Weather", ylab = "Number of Injuries")

---
#### 4c. Plot the total dollar losses to property and crops caused by
#### each type of severe weather event

par(mfrow = c(1, 2), mar = c(11.5, 5.5, 3.1, 2.2), mgp = c(3, 1, 0), cex = 0.7, las = 3)
options(scipen = 0)

dfpropertylossbytype
barplot(dfpropertylossbytype$`sum(loss)`, names.arg = dfpropertylossbytype$EVTYPE, col = 'brown', main = "Value of Property Loss From Weather", ylab = "US Dollar Amount")


dfcropslossbytype
barplot(dfcropslossbytype$`sum(loss)`, names.arg = dfcropslossbytype$EVTYPE, col = 'green', main = "Value of Crop Loss From Weather", ylab = "US Dollar Amount")



