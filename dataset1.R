#clear objects 
rm(list=ls())

#important packages
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

#import dataset1 and view -unchanged
Origional_dataset_1 <- read_excel("Desktop/dataset1.xlsx")
View(dataset1)


#new copy of dataset1 for you to add to 
dataset1 <- Origional_dataset_1


#creating a new column of mass per pellet 
dataset1 <- cbind(dataset1, ((dataset1$LastOfSampleMass)/(dataset1$LastOfPelletCount)) )
names(dataset1)[names(dataset1)=="((dataset1$LastOfSampleMass)/(dataset1$LastOfPelletCount))"] <- "MassPerPellet"

#changing numbers to male and female
dataset1$Sex <- ifelse(test=dataset1$Sex == 1, yes="Female", no="Male") 
dataset1$Sex <- as.factor(dataset1$Sex)

#removing all NAs
dataset1 <- dataset1 %>% drop_na()

#creating column with age
#age_in_years<- (difftime(dataset1$ExactDoD , dataset1$ExactDoB, units = c("days"))/365.25) %>% as.double()

death_age_years <- ((dataset1$ExactDoD-dataset1$ExactDoB)/365.25) %>% as.double() 

dataset1$death_age_years <- death_age_years

#create new column with samples months 
dataset1$samplemonth <- format(as.Date(dataset1$LastOfSampleDate), "%m")


#creating new column with age categories, calf, 1-3 yrs and Adults
dataset1$age_group <- cut(dataset1$death_age_years,c(0,1,3,23))
levels(dataset1$age_group) = c("Calf","1-3 yrs","Adult")

#create new dataset with just samples of november before death  
Ndata1 <- dataset1 %>% filter(`samplemonth` == 11)

#new dataset with november samples of juveniles <3 years 
NdataJuv <- Ndata1 %>% filter( `age_in_years` < 3 )


#new dataset with all month samples of juveniles <3 years 
data1Juv <- dataset1 %>% filter( `age_in_years` < 3 )

