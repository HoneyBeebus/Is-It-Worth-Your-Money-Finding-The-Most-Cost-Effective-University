# Graphing Studen Faculty ratio and Graduation Rate #

######################
#Read in the database#
######################

####################
#Create Data Frames#
#  And variables   #
####################

library(corrplot)
library(sm)
library(vioplot)
library(RColorBrewer)
library(ggplot2)

schoolData = read.csv(file = "College.csv", header=TRUE, sep = ",")

AR = rep(1:777)
PriAR = rep(1:565)
PubAR = rep(1:212)

Accept = 0
App = 0
CalcAR = 0

schoolData$AR=AR

for(i in c(1:777)) {
  Accept = schoolData$Accept[i]
  App = schoolData$Apps[i]
  CalcAR = (Accept/App)
  schoolData$AR[i] = CalcAR
  TotTuition[i] = schoolData$Outstate[i] + schoolData$Room.Board[i] + schoolData$Books[i] + schoolData$Personal[i]
}

tui<-c(11805,10440,6813)
Ace<-c(75.43, 74.69, 72.65)


private = schoolData[schoolData$Private == "Yes" & schoolData$Grad.Rate <= 100, ]
public = schoolData[schoolData$Private == "No" & schoolData$Grad.Rate <= 100, ]

TopSF = schoolData[schoolData$S.F.Ratio < 10 & schoolData$Grad.Rate <= 100, ]
BottomSF = schoolData[schoolData$S.F.Ratio > 25 & schoolData$Grad.Rate <= 100, ]
MiddleSF = schoolData[schoolData$S.F.Ratio > 10 & schoolData$S.F.Ratio <25 & schoolData$Grad.Rate <= 100, ]

TopSFHT = schoolData[schoolData$S.F.Ratio < 10 & schoolData$Grad.Rate <= 100 & schoolData$Outstate > 10440.669, ]
BottomSFHT = schoolData[schoolData$S.F.Ratio > 25 & schoolData$Grad.Rate <= 100 & schoolData$Outstate > 10440.669, ]
MiddleSFHT = schoolData[schoolData$S.F.Ratio > 10 & schoolData$S.F.Ratio <25 & schoolData$Grad.Rate <= 100 & schoolData$Outstate > 10440.669, ]
TopSFLT = schoolData[schoolData$S.F.Ratio < 10 & schoolData$Grad.Rate <= 100 & schoolData$Outstate < 10440.669, ]
BottomSFLT = schoolData[schoolData$S.F.Ratio > 25 & schoolData$Grad.Rate <= 100 & schoolData$Outstate < 10440.669, ]
MiddleSFLT = schoolData[schoolData$S.F.Ratio > 10 & schoolData$S.F.Ratio <25 & schoolData$Grad.Rate <= 100 & schoolData$Outstate < 10440.669, ]

PubTopSF=public[public$S.F.Ratio<10 & public$Grad.Rate <=100, ]
PubBottomSF=public[public$S.F.Ratio>25 & public$Grad.Rate <=100, ]
PubMiddleSF=public[public$S.F.Ratio>10 & public$S.F.Ratio<25 & public$Grad.Rate <=100, ]

PriTopSF=private[private$S.F.Ratio<10 & private$Grad.Rate <=100, ]
PriBottomSF=private[private$S.F.Ratio>25 & private$Grad.Rate <=100, ]
PriMiddleSF=private[private$S.F.Ratio>10 & private$S.F.Ratio<25 & private$Grad.Rate <=100, ]

Ratio = schoolData$S.F.Ratio
Graduation = schoolData$Grad.Rate
Price = schoolData$Outstate

PriGraduation = private$Grad.Rate
PriSF = private$S.F.Ratio
PriPrice = private$Outstate

PubGraduation = public$Grad.Rate
PubSF = public$S.F.Ratio
PubPrice = public$Outstate

TotTuition = rep(1:777)
PriTotT = rep(1:565)
PubTotT = rep(1:212)

####################
#     Plotting     #
####################

plot(Ratio, Graduation, main = "Student Faculty Ratio and Graduation Rate",xlab="SFR",ylab="Graduation Rate")
model1=lm(Grad.Rate~S.F.Ratio, data=schoolData)
abline(model1$coefficients)

#model1prime = lm(Grad.Rate~S.F.Ratio+Enroll, data=schoolData)

h<-hist(Ratio,breaks=25,col="red",xlab="SFR",main="Distribution of SFR")
xfit<-seq(min(Ratio),max(Ratio),length=90)
yfit<-dnorm(xfit,mean=mean(Ratio),sd=sd(Ratio))
yfit<-yfit*diff(h$mids[1:2])*length(Ratio)
lines(xfit,yfit,col="blue",lwd=2)

h1<-hist(PriSF,breaks=25,col="red",xlab="S.F.Ratio",main="Distribution of SFR in Private Schools")
xfit1<-seq(min(PriSF),max(PriSF),length=90)
yfit1<-dnorm(xfit1,mean=mean(PriSF),sd=sd(PriSF))
yfit1<-yfit1*diff(h1$mids[1:2])*length(PriSF)
lines(xfit1,yfit1,col="blue",lwd=2)

h2<-hist(PubSF,breaks=25,col="red",xlab="S.F.Ratio",main="Distribution of SFR in Public Schools")
xfit2<-seq(min(PubSF),max(PubSF),length=90)
yfit2<-dnorm(xfit2,mean=mean(PubSF),sd=sd(PubSF))
yfit2<-yfit2*diff(h2$mids[1:2])*length(PubSF)
lines(xfit2,yfit2,col="blue",lwd=2)

vioplot(TopSF$Grad.Rate, MiddleSF$Grad.Rate, BottomSF$Grad.Rate, names = c("High SFR", "Middle SFR", "Bottom SFR"), col = "gold")
title("Violin Plots of Graduation Rate")

vioplot(PubTopSF$Grad.Rate, PubMiddleSF$Grad.Rate, PubBottomSF$Grad.Rate, names=c("High SFR", "Middle SFR", "Bottom SFR"), col = "gold")
title("Violin Plots of Graduation Rate in Public Schools")

vioplot(PriTopSF$Grad.Rate, PriMiddleSF$Grad.Rate, PriBottomSF$Grad.Rate, names=c("High SFR", "Middle SFR", "Bottom SFR"), col = "gold")
title("Violin Plots of Graduation Rate in Private Schools")

plot(schoolData$AR, Ratio, main = "Acceptance Rate (AR) vs Student Faculty Ratio (SFR)",xlab="SFR",ylab="AR")
model2=lm(Ratio~schoolData$AR)
abline(model2$coefficients)

boxplot(schoolData$AR, private$AR, public$AR, names=c("General AR" ,"Private AR", "Public AR"), col="green")
title("Acceptance Rate (AR)")

boxplot(MiddleSF$AR, PriMiddleSF$AR, PubMiddleSF$AR, names=c("General AR" ,"Private AR", "Public AR"), col="green")
title("Acceptance Rate (AR) For Middle SFR Schools")

boxplot(BottomSF$AR, PriBottomSF$AR, PubBottomSF$AR, names=c("General AR" ,"Private AR", "Public AR"), col="green")
title("Acceptance Rate (AR) For Bottom SFR Schools")

boxplot(TopSF$AR, PriTopSF$AR, PubTopSF$AR, names=c("General AR" ,"Private AR", "Public AR"), col="green")
title("Acceptance Rate (AR) For Top SFR Schools")

h3<-hist(schoolData$Outstate,breaks=20,col="coral1",xlab="Cost of Tuition",main="Distribution of Tuition Cost")
xfit3<-seq(min(schoolData$Outstate),max(schoolData$Outstate),length=90)
yfit3<-dnorm(xfit3,mean=mean(schoolData$Outstate),sd=sd(schoolData$Outstate))
yfit3<-yfit3*diff(h3$mids[1:2])*length(schoolData$Outstate)
lines(xfit3,yfit3,col="darkslateblue",lwd=2)

h4<-hist(private$Outstate,breaks=20,col="coral1",xlab="Cost of Tuition",main="Distribution of Tuition Cost in Private Schools")
xfit4<-seq(min(private$Outstate),max(private$Outstate),length=90)
yfit4<-dnorm(xfit4,mean=mean(private$Outstate),sd=sd(private$Outstate))
yfit4<-yfit4*diff(h4$mids[1:2])*length(private$Outstate)
lines(xfit4,yfit4,col="darkslateblue",lwd=2)

h5<-hist(public$Outstate,breaks=20,col="coral1",xlab="Cost of Tuition",main="Distribution of Tuition Cost in Public Schools")
xfit5<-seq(min(public$Outstate),max(public$Outstate),length=90)
yfit5<-dnorm(xfit5,mean=mean(public$Outstate),sd=sd(public$Outstate))
yfit5<-yfit5*diff(h5$mids[1:2])*length(public$Outstate)
lines(xfit5,yfit5,col="darkslateblue",lwd=2)

meanAR = mean(schoolData$Outstate)
Pri_meanT = mean(private$Outstate)
Pub_meanT = mean(public$Outstate)
meanAR = mean(schoolData$AR)
Pri_meanAR = mean(private$AR)
Pub_meanAR = mean(public$AR)
Pri_meanGR = mean(PriTopSF$Grad.Rate)
Pub_meanGR = mean(PubTopSF$Grad.Rate)

Dif_in_AR = (abs(Pri_meanAR - Pub_meanAR)/((Pri_meanAR + Pub_meanAR)/2)) * 100
Dif_in_T = (abs(Pri_meanT - Pub_meanT)/((Pri_meanT + Pub_meanT)/2)) * 100
Dif_in_GR = (abs(Pri_meanGR - Pub_meanGR)/((Pri_meanGR + Pub_meanGR)/2)) * 100

huly = mean(private$S.F.Ratio)

