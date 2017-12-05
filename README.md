---
title: "Recessions and Crime"
author: 
date: "December 2017"
output: 
  html_document:
    fig_height: 9
    fig_width: 13
---
<!-- Don't edit in between this line and the one below -->
```{r, include=FALSE}
# Don't delete this chunk if you are using the DataComputing package
library(DataComputing)
library(tidyr)
```
*Source file* 
```{r, results='asis', echo=FALSE}
includeSourceDocuments()
```
<!-- Don't edit the material above this line -->

###Introduction
The goal of this project is to determine if there is any correlation between a recession and crime rates. In other words, is there more crime during a recession? In order to do this, I have quarterly GDP data from the St. Louis FED and annual crime statistics from the FBI. 


###Data Sources
[Real Gross Domestic Product from FRED](https://fred.stlouisfed.org/series/GDPC1)
<br>
[Crime in the United States from the FBI](https://ucr.fbi.gov/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/1tabledatadecoverviewpdf/table_1_crime_in_the_united_states_by_volume_and_rate_per_100000_inhabitants_1994-2013.xls)
  
  
###The Process

First, I need to load the two data sets into R  
```{r}
#Load the data into R

GDP <- read.csv("GDP.csv")
CRIME <-  read.csv("CRIME.csv")

```


In order to find which quarters were in a recession, I will calculate the percentage change for GDP. Then I can filter out all of the positive values to get only recession entries.
```{r} 
#calculate pecentage change of GDP and only display the values for a negative value to show recession years. 

GDP_Recession <- GDP%>%                                    #name the table GDP_Recessions and use the GDP data
      mutate(pChange=(GDPC1-lag(GDPC1))/lag(GDPC1)*100)%>% #creates the percentage change variable
      filter(pChange<0) %>%                                #Only display the negative percentage change values
      na.omit()                                            #omit any entries that say NA

knitr::kable(head(GDP_Recession))                          #create neat table for GDP_Recession
```


Now I will finalize my GDP data. GDP is reported quarterly, but I need to make it annual so that it will match with the Crime table. To make it annual data, I am averaging the percentage change values for each year.
```{r}
#Crime data is only yearly. so need to get the GDP_Recession data into yearly. To do this, find the yearly average rate for GDP_Recession. Probably better to tell an actual recession because it'll give a closer estimate of consecutive periods and official recessions. 

library(stringi)                                         #Required for stri_extract_last_regex

GDP_Recession2 <- GDP_Recession %>%                      #Extracts the year (last four digits) from the date
  mutate(Year=(stri_extract_last_regex(DATE, "\\d{4}")))

GDP_Final<-
group_by(GDP_Recession2,Year ) %>%                       #Finds the average annual percent change in GDP
  summarize(GDP_Rate= mean(pChange))


knitr::kable(head(GDP_Final))                            #create neat table for GDP_Final
```


Next, I need to take some steps to clean my Crime data-set so that R will be able to use it.
```{r,message=FALSE,warning=FALSE}
#Need to clean up the Crime dataset

Crime2<-CRIME[-c(1,2,24,25,26,27,28,29,30,31,32,33,34,35,36,37),]  #Delete unnecessary rows from Crime and rename the table

colnames(Crime2)[1] <- "Year"                                      #Renaming the column names because they were wrong
colnames(Crime2)[2] <- "Population"
colnames(Crime2)[3] <- "Violent"
colnames(Crime2)[4] <- "Violent_Rate"
colnames(Crime2)[5] <- "Murder_and_Manslaughter"
colnames(Crime2)[6] <- "Murder_and_Manslaughter_Rate"
colnames(Crime2)[7] <- "Rape"
colnames(Crime2)[8] <- "Rape_Rate"
colnames(Crime2)[9] <- "Robbery"
colnames(Crime2)[10] <- "Robbery_Rate"
colnames(Crime2)[11] <- "Aggravated_Assault"
colnames(Crime2)[12] <- "Aggravated_Assault_Rate"
colnames(Crime2)[13] <- "Property"
colnames(Crime2)[14] <- "Property_Crime"
colnames(Crime2)[15] <- "Burglary"
colnames(Crime2)[16] <- "Burglary_Rate"
colnames(Crime2)[17] <- "Larceny"
colnames(Crime2)[18] <- "Larceny_Rate"
colnames(Crime2)[19] <- "Motor_Vehicle_Theft"
colnames(Crime2)[20] <- "Motor_Vehicle_Theft_Rate"

Crime3<- Crime2[-c(1),]                      #Delete the first row because it had header information instead of data

Crime3$X.19<- NULL                           # Delete the last four columns because they all had NA for data
Crime3$X.20<-NULL
Crime3$X.21 <-NULL
Crime3$X.22<-NULL

Crime3[1] <- lapply(Crime3, as.character)    #convert Year to a character so it can be joinged with GDP Year

CRIME_Final <- Crime3                        #Rename the finished clean table CRIME_Final

knitr::kable(head(CRIME_Final))              #create neat table for CRIME_Final
```


Now I have two tables that are ready to be joined. I will use the year to match them so that each case is a year with crime statistics and a GDP percentage change value
```{r}
#Need to Join the two tables so each year has a percentage change and crime statistics

Total <-
  CRIME_Final %>%                     #Join CRIME_Final and GDP_Final by year
  full_join(GDP_Final, by="Year")%>%
  na.omit() 

knitr::kable(head(Total))             #create neat table for Total
```


The tables are joined and now I can plot all the data to see if there is any correlation.
```{r,message=FALSE,warning=FALSE}
#Time to plot the data! Is there any correlation between recessions and the amount of crime?

a<-ggplot(Total, aes(x=GDP_Rate,y=Violent))+
  geom_point(colour = "red", size = 5)+
  ggtitle("Violent Crime")

b<-ggplot(Total, aes(x=GDP_Rate,y=Murder_and_Manslaughter))+
  geom_point(colour = "gray", size = 5)+
  ggtitle("Murder and Nonviolent Manslaughter")
  
c<-ggplot(Total, aes(x=GDP_Rate,y=Rape))+
  geom_point(colour = "pink", size = 5)+
  ggtitle("Rape")

d<-ggplot(Total, aes(x=GDP_Rate,y=Robbery))+
  geom_point(colour = "blue", size = 5)+
  ggtitle("Robbery")

e<-ggplot(Total, aes(x=GDP_Rate,y=Aggravated_Assault))+
  geom_point(colour = "green", size = 5)+
  ggtitle("Aggravated Assault")

f<-ggplot(Total, aes(x=GDP_Rate,y=Property_Crime))+
  geom_point(colour = "purple", size = 5)+
  ggtitle("Property Crime")

g<-ggplot(Total, aes(x=GDP_Rate,y=Burglary))+
  geom_point(colour = "yellow", size = 5)+
  ggtitle("Burglary")
  
h<-ggplot(Total, aes(x=GDP_Rate,y=Larceny))+
  geom_point(colour = "orange", size = 5)+
  ggtitle("Larceny")

i<-ggplot(Total, aes(x=GDP_Rate, y=Motor_Vehicle_Theft))+
  geom_point(colour = "black", size = 5)+
  ggtitle("MOtor Vehicle Theft")

library(ggpubr)                 #Required for ggarrange()

ggarrange(a,b,c,d,e,f,g,h,i)    #Puts all the plots together
```

###Findings
From the above graphs, there seems to be a relatively linear correlation between recessions and crime. However, it seems as if 2001 is an outlier (except in the Burglary and Motor Vehicle Theft Graphs). Disregarding the 2001 outlier, the data show that there is a positive relationship between recessions and crime.
