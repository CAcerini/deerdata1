#age and sex effects on size all data
model1 <- lm(JawLengthA~age_in_years+Sex+age_in_years*Sex, data=dataset1)
anova(model1)

par(mfrow=c(2,2)) 
plot(model1)
par(mfrow=c(1,1))

summary(model1)

ggplot(dataset1, aes(x=age_in_years, y=JawLengthA, colour=Sex, group=Sex)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#age and sex effects on size in juveniles only 
model2 <- lm(JawLengthA~age_in_years+Sex+age_in_years*Sex, data=data1Juv)
anova(model1)

par(mfrow=c(2,2)) 
plot(model2)
par(mfrow=c(1,1))

summary(model2)

ggplot(data1Juv, aes(x=age_in_years, y=JawLengthA, colour=Sex, group=Sex)) +
  geom_point() +
  theme_bw()



#pelletmass variation with season 
#does samplemonth effect pelletweight once the age group has been controlled for ?

model1 <- lm(MassPerPellet~deathage+Sex+samplemonth+deathage*samplemonth+Sex*samplemonth, data=dataset1)
anova(model1)

summary (model1)

par(mfrow=c(2,2)) 
plot(model3)
par(mfrow=c(1,1))

model3.2 <- lm(MassPerPellet~age_group+samplemonth, data=dataset1)
anova(model1.1)

summary(model3.2)

ggplot(dataset1, aes(x = samplemonth, y = MassPerPellet, fill = samplemonth)) + geom_boxplot() +
  facet_grid(~age_group)

  
#does sex have an effect on pellet mass - all data 
sexVmass <- lm(MassPerPellet~Sex, data=dataset1)
anova(sexVmass)

sexagemass <- lm(MassPerPellet~age_group+Sex, data=dataset1)
anova(sexagemass)

summary(sexagemass)
ggplot(dataset1, aes(x = Sex, y = MassPerPellet, fill = Sex)) + geom_boxplot() +
  facet_grid(~age_group)


#AGE AND SEX EFFECT ON NOVEMBER WEIGHTS

model4 <- lm(MassPerPellet~age_in_years+Sex+age_in_years*Sex, data=Ndata1)
anova(model1)


par(mfrow=c(2,2)) 
plot(model4)
par(mfrow=c(1,1))

summary(model4)

#AGE AND SEX EFFECT ON NOVEMBER WEIGHTS IN JUVENILLES ONLY
model5 <- lm(MassPerPellet~age_in_years+Sex+age_in_years*Sex, data=NdataJuv)
anova(model5)


par(mfrow=c(2,2)) 
plot(model5)
par(mfrow=c(1,1))

summary(model5)


ggplot(NdataJuv, aes(x=age_in_years, y=MassPerPellet)) +
  geom_point(aes(colour=Sex)) 



#correlations between mass, jaw length, sex. - KAW LENGTH AS RESPONSE VARIABLE (NO)
model1 <- lm(JawLengthA~MassPerPellet+Sex+MassPerPellet*Sex, data=dataset1)
anova(model1)

par(mfrow=c(2,2)) 
plot(model1)
par(mfrow=c(1,1))

summary(model1)
ggplot(dataset1, aes(x = MassPerPellet, y = JawLengthA)) + geom_point(aes(colour=Sex))
warnplot(JawLengthA~MassPerPellet, group=Sex, data=dataset1)


#age effect on jaw length GLM
data1 <- lm(JawLengthA~age_in_days, data=dataset1)
anova(data1)

par(mfrow=c(2,2)) 
plot(data1)
par(mfrow=c(1,1)) #assumptions seem okay 

summary(data1)


ggplot(dataset1, aes(x=age_in_days, y=JawLengthA)) +
  geom_point() +
  geom_smooth()
theme_bw()


#-------------------------------------------------------------------------------------------------------
#lol not this

data2 <- cbind(dataset1 (dataset1$age_in_years,dataset1$JawLengthA))



plot(dataset1$age_in_years,dataset1$JawLengthA)

library(easynls)

model1 <- nlsfit(dataset1, model = 10, start = c(a=250, b=0.5, c=20))
model1

model2 <- nls(JawLengthA ~ c * age_in_years ^ z, data=dataset1, start=c(c=160, z=0.2))
model2

nlsplot <- nls(JawLengthA ~ c * age_in_years ^ z, data=dataset1, start=c(c=160, z=0.2))

lines(dataset1$age_in_years, predict(nlsplot), col = "red")

#-------------------------------------------------------------------------------------------------------

#RESPONSE VARIABLE MASS PER PELLET- 

modelx <- lm(MassPerPellet~Sex+JawLengthA+deathage, data=Ndata1)

anova(modelx)


#type 3 SS 
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
modelY <- lm(MassPerPellet~Sex+JawLengthA+deathage+deathdeeryear, data=Ndata1)
Anova(modelY, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

summary(modelY)
#assumptions
par(mfrow=c(2,2)) 
plot(modelY)
par(mfrow=c(1,1))


modelY <- lm(MassPerPellet~Sex+JawLengthA+deathage, data=Ndata1)
summary(modelY)

#..removing the interaction

#modely <- lm(MassPerPellet~JawLengthA+Sex+age_in_years, data=Ndata1)
#anova(modely)
#summary(modely)




#plotting jawlength and pellet mass
ggplot(Ndata1, aes(x=JawLengthA, y=MassPerPellet)) +
  geom_point(aes(colour=Sex))+
  xlab("Jaw length (mm)") + ylab("Mass per pellet (g)") +
  geom_smooth(method=lm, se=FALSE, colour="mediumpurple1")




#no of individuals
length(unique(Ndata1$Code))

ggplot(dataset1, aes(x=JawLengthA, y=MassPerPellet)) +
  geom_point(aes(colour=Sex))+
  xlab("Jaw length (mm)") + ylab("Mass per pellet (g)")

### calfs 

options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
modelC <- lm(MassPerPellet~JawLengthA, data=NdataCalf)
Anova(modelC, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

summary(modelC)





options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
modelCAll <- lm(MassPerPellet~JawLengthA+Sex+SampMonth, data=dataCalf)
Anova(modelCAll, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

summary(modelCAll)

ggplot(dataCalf, aes(x=JawLengthA, y=MassPerPellet, col=SampMonth)) +
  geom_point()+
  xlab("Jaw length (mm)") + ylab("Mass per pellet (g)") +
  geom_smooth(method=lm, se=FALSE, colour="mediumpurple1")

#November calves 

options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
modelCN <- lm(MassPerPellet~JawLengthA+Sex, data=NdataCalf)
Anova(modelCN, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

summary(modelCN)

ggplot(dataCalf, aes(x=JawLengthA, y=MassPerPellet, col=SampMonth)) +
  geom_point()+
  xlab("Jaw length (mm)") + ylab("Mass per pellet (g)") +
  geom_smooth(method=lm, se=FALSE, colour="mediumpurple1")



#both months

options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
modelAll <- lm(MassPerPellet~JawLengthA+Sex+SampMonth+deathage, data=dataset1)
Anova(modelAll, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

summary(modelAll)

ggplot(dataCalf, aes(x=JawLengthA, y=MassPerPellet, col=SampMonth)) +
  geom_point()+
  xlab("Jaw length (mm)") + ylab("Mass per pellet (g)") +
  geom_smooth(method=lm, se=FALSE, colour="mediumpurple1")





options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))
modelAll <- lm(MassPerPellet~JawLengthA+Sex+SampMonth+deathage, data=dataset1)
Anova(modelAll, type=3)
options(contrasts=c(unordered="contr.treatment", ordered="contr.poly"))

summary(modelAll)

ggplot(dataCalf, aes(x=JawLengthA, y=MassPerPellet, col=SampMonth)) +
  geom_point()+
  xlab("Jaw length (mm)") + ylab("Mass per pellet (g)") +
  geom_smooth(method=lm, se=FALSE, colour="mediumpurple1")
