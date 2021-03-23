library(cowplot)
theme_set(theme_cowplot())


# 1A: all ages November --------------------------------------------------------


#1A : deer of all ages


#max model
maxmodel1A <- lm(MassPerPellet~Sex+JawLengthA+deathage+Sex*JawLengthA+
             JawLengthA*deathage+Sex*deathage, data=Ndata1)


par(mfrow=c(2,2)) 
plot(maxmodel1A)
par(mfrow=c(1,1))

Anova(model1A.1, type=3)

summary(maxmodel1A)


#model simplification
#order of removing terms (remove least significant each time, until final model all terms signifcant)
#age*jaw interaction, sex*jaw interaction, sex*age interaction, sex, age

# minimal model 
minmodel1A <- lm(MassPerPellet~JawLengthA, data=Ndata1)


par(mfrow=c(2,2)) 
plot(minmodel1A)
par(mfrow=c(1,1))

Anova(minmodel1A, type=3)

summary(model1A.3)

#plotting (minmodel 1A)
plot1A<- ggplot(Ndata1, aes(x=JawLengthA, y=MassPerPellet)) +
              geom_point()+
              geom_smooth(method=lm, se=FALSE, colour="steel blue") +
              xlab("Post-mortem jaw length (mm)") + ylab(" Faecal pellet mass (g)") +
              scale_y_continuous(expand = c(0,0), limits = c(0.0,4.0), breaks=seq(0.0,4.0, by=1.0))  +
              labs(tag="A") +
              theme(panel.border = element_rect(colour = "white", fill=NA),
                    panel.background = element_rect(fill="white"),
                    axis.line.x = element_line(color="black", size = 0.4),
                    axis.line.y = element_line(color="black", size = 0.4),
                    axis.text=element_text(size=15),
                    axis.title=element_text(size=20),
                    plot.tag = element_text(size=30))




# 1B: calves November ---------------------------------------------------------

#no need to fit year we don't expect differences in body size across years - stop forgetting this omg...

#max model
maxmodel1B <- lm(MassPerPellet~Sex*JawLengthA, data=NdataCalf)


par(mfrow=c(2,2)) 
plot(maxmodel1B)
par(mfrow=c(1,1))

Anova(model1A.1, type=3)

summary(maxmodel1B)


#model simplification
#order of removing terms (remove least significant each time, until final model all terms signifcant)
#sex*jaw interaction, sex. 

# minimal model 
minmodel1B <- lm(MassPerPellet~JawLengthA, data=NdataCalf)


par(mfrow=c(2,2)) 
plot(modelCN)
par(mfrow=c(1,1))

Anova(minmodel1B, type=3)

summary(minmodel1B)


#plotting(minmodel1B)
plot1B<- ggplot(NdataCalf, aes(x=JawLengthA, y=MassPerPellet)) +
              geom_point()+
              geom_smooth(method=lm, se=FALSE, colour="steel blue") +
              xlab("Post-mortem jaw length (mm)") + ylab(" Faecal pellet mass (g)") +
              scale_y_continuous(expand = c(0,0), limits = c(0,2.25), breaks=seq(0,2.25, by=0.5))  +
              labs(tag="B") +
              theme(panel.border = element_rect(colour = "white", fill=NA),
                    panel.background = element_rect(fill="white"),
                    axis.line.x = element_line(color="black", size = 0.4),
                    axis.line.y = element_line(color="black", size = 0.4),
                    axis.text=element_text(size=15),
                    axis.title=element_text(size=20),
                    plot.tag = element_text(size=30)) 







# plotting ----------------------------------------------------------------


plot_grid(plot1A, plot1B)



# calves Aug and Nov  - no ------------------------------------------------


#both months
#Sex*JawLengthA+JawLengthA*SampMonth - both dont have significant effect
#modelCAll <- lm(MassPerPellet~JawLengthA+Sex+SampMonth, data=dataCalf)
#Anova(modelCAll, type=3)

#summary(modelCAll)


#par(mfrow=c(2,2)) 
#plot(modelCAll)
#par(mfrow=c(1,1))

#ggplot(dataCalf, aes(x=JawLengthA, y=MassPerPellet, colour=SampMonth)) +
geom_point()+
  geom_smooth(method=lm, se=FALSE, colour="mediumpurple1") +
  xlab("post-mortem jaw length (mm)") + ylab(" faecal pellet mass (g)") +
  scale_colour_manual(name="Sample Month", labels=c("August","November"), values = c("#F8766D", "#00BFC4"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,2), breaks=seq(0,2, by=0.5))  + 
  theme_bw()





# yearlings (November) you really don't have enough data lol  ----------------------------------------------------

modelYN <- lm(MassPerPellet~JawLengthA, data=NdataYling)
Anova(modelYN, type=3)

summary(modelYN)

#lol.. nope vvv
ggplot(NdataYling, aes(x=JawLengthA, y=MassPerPellet)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, colour="steel blue") 


