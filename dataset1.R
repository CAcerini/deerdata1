#clear objects 
rm(list=ls())

#important packages
library(readxl); library(tidyverse); library(car)


#import dataset1 and view -unchanged
Origional_dataset_1 <- read_excel("Origional_dataset_1.xlsx")



#new copy of dataset1 for you to add to 
dataset1 <- Origional_dataset_1

#creating a new column of mass per pellet 
dataset1 <- dataset1 %>% mutate(MassPerPellet= LastOfSampleMass/LastOfPelletCount) %>% drop_na(MassPerPellet)


#changing numbers to male and female
dataset1$Sex <- ifelse(test=dataset1$Sex == 1, yes="Female", no="Male") %>%  as.factor()

#removing all NAs
dataset1 <- dataset1 %>% drop_na()


#calculating death age 
dataset1 <- dataset1 %>% separate(LastOfSampleDate, sep = "-", into = c("SampYear", "SampMonth", "SampDay")) %>% 
  mutate_at(c("SampYear", "SampMonth", "SampDay"), as.numeric)

dataset1$deeryear <- dataset1$`SampYear`
dataset1$deeryear[dataset1$`SampMonth` == 4] <- dataset1$deeryear[dataset1$`SampMonth` == 4]-1

dataset1 <- dataset1 %>% separate(ExactDoD, sep = "-", into = c("DeathYear", "DeathMonth", "DeathDay")) %>% 
  mutate_at(c("DeathYear", "DeathMonth", "DeathDay"), as.numeric)

dataset1$deathdeeryear <- dataset1$`DeathYear`
dataset1$deathdeeryear[(dataset1$`DeathMonth` <5) & !is.na(dataset1$DeathMonth)] <- 
  dataset1$deathdeeryear[(dataset1$`DeathMonth` <5) & !is.na(dataset1$DeathMonth)]-1

deathage1 <- (dataset1$deathdeeryear-dataset1$BirthYear) %>% as.double()  
dataset1$deathage <- deathage1


#years and sample month as factors  
dataset1$deathdeeryear <- dataset1$deathdeeryear %>% as.factor()

dataset1$SampMonth <- dataset1$SampMonth %>% as.factor()


#create new dataset with just samples of November before death  
Ndata1 <- dataset1 %>% filter(`SampMonth` == 11)

Ndata1 <- Ndata1 %>% filter(deeryear == deathdeeryear) # removing deer that have a long time between sample and death. 
                                                      #should just be deaths in december-april (same deer year)





#creating calf subset with both august and november data 

dataCalf <- dataset1 %>% filter( `deathage` == 0 )

dataCalf<- dataCalf %>% filter(!(SampMonth == 4))  #removing that one april sample 



#creating calf subset with only november samples 
NdataCalf <- dataCalf %>%  filter(SampMonth == 11)




#-------------------------------------------------------------------------- code graveyard


#creating column with age
#age_in_years<- (difftime(dataset1$ExactDoD , dataset1$ExactDoB, units = c("days"))/365.25) %>% as.double()

#death_age_years <- ((dataset1$ExactDoD-dataset1$ExactDoB)/365.25) %>% as.double() 

#dataset1$death_age_years <- death_age_years
#dataset1$death_age_years %>% as.integer() %>% table()





#creating new column with age categories, calf, 1-3 yrs and Adults
#dataset1$age_group <- cut(dataset1$death_age_years,c(0,1,3,23))
#levels(dataset1$age_group) = c("Calf","1-3 yrs","Adult")



#new dataset with november samples of juveniles <3 years 
#NdataJuv <- Ndata1 %>% filter( `deathage` < 3 )


#new dataset with all month samples of juveniles <3 years 
#data1Juv <- dataset1 %>% filter( `death_age_years` < 3 )
