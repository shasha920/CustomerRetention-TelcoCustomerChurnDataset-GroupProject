install.packages(c("forecast",
                   "ggplot2",
                   "gplots",
                   "reshape",
                   "GGally",
                   "MASS"))
#Library packages
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
install.packages("corrr")
library(corrr)
telco.df<-read.csv("Telco-Customer-Churn.csv")
#test cor or each other numerica value
telco.df%>%
  correlate()%>%
  focus("TotalCharges")
#summary Continuous Value
data.frame(mean=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],mean),
           sd=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],sd),
           min=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")], min),
           max=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],max),
           median=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],median),
           length=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],length),
           miss.val=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],function(x)sum(is.na(x))))
#Summary Categorical Value
#gender
gender.freq <- telco.df %>%
  group_by(gender)%>%
  summarise(n=n())
#SeniorCitizen
SeniorCitizen.freq <- telco.df %>%
  group_by(SeniorCitizen)%>%
  summarise(n=n())
#Partner
Partner.freq <- telco.df %>%
  group_by(Partner)%>%
  summarise(n=n())
#Dependents
Dependents.freq <- telco.df %>%
  group_by(Dependents)%>%
  summarise(n=n())
#Churn
Churn.freq <- telco.df %>%
  group_by(Churn)%>%
  summarise(n=n())
#Data valisuation
#boxplot SeniorCitizen vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(SeniorCitizen),y=tenure))+xlab("SeniorCitizen")
#boxplot SeniorCitizen vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(SeniorCitizen),y=TotalCharges))+xlab("SeniorCitizen")
#boxplot SeniorCitizen vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(SeniorCitizen),y=MonthlyCharges))+xlab("SeniorCitizen")
#scatterplot TotalCharges vs tenure
ggplot(telco.df)+
  geom_point(aes(x=tenure,y=TotalCharges),color="pink",alpha=0.5)
#scatterplot MonthlyCharges vs TotalCharges
ggplot(telco.df)+
  geom_point(aes(x=MonthlyCharges,y=TotalCharges),color="steelblue",alpha=0.5)
#scatterplot MonthlyCharges vs tenure
ggplot(telco.df)+
  geom_point(aes(x=tenure,y=MonthlyCharges),color="darkgreen",alpha=0.5)
#ggpair MonthlyCharges TotalCharges tenure
ggpairs(telco.df[,c("MonthlyCharges","TotalCharges","tenure")])
#barPot Churn vs MonthlyCharges
data.for.plot<-aggregate(telco.df$MonthlyCharges,by=list(telco.df$Churn),FUN=mean)
names(data.for.plot)<-c("Churn","MeanMonthlyCharges")
ggplot(data.for.plot)+
  geom_bar(aes(x=Churn,y=MeanMonthlyCharges),stat = "identity")
#barPot Churn vs tenure
data.for.plot<-aggregate(telco.df$tenure,by=list(telco.df$Churn),FUN=mean)
names(data.for.plot)<-c("Churn","Meantenure")
ggplot(data.for.plot)+
  geom_bar(aes(x=Churn,y=Meantenure),stat = "identity")
#barPot Churn vs TotalCharges
data.for.plot<-aggregate(telco.df$TotalCharges,by=list(telco.df$Churn),FUN=mean)
names(data.for.plot)<-c("Churn","MeanTotalCharges")
ggplot(data.for.plot)+
  geom_bar(aes(x=Churn,y=MeanTotalCharges),stat = "identity")
#no obviously result
#barPot gender vs MonthlyCharges
data.for.plot<-aggregate(telco.df$MonthlyCharges,by=list(telco.df$gender),FUN=mean)
names(data.for.plot)<-c("gender","MeanMonthlyCharges")
ggplot(data.for.plot)+
  geom_bar(aes(x=gender,y=MeanMonthlyCharges),stat = "identity")
#no obviously result
#barPot gender vs tenure
data.for.plot<-aggregate(telco.df$tenure,by=list(telco.df$gender),FUN=mean)
names(data.for.plot)<-c("gender","Meantenure")
ggplot(data.for.plot)+
  geom_bar(aes(x=gender,y=Meantenure),stat = "identity")
#no obviously result
#barPot gender vs TotalCharges
data.for.plot<-aggregate(telco.df$TotalCharges,by=list(telco.df$gender),FUN=mean)
names(data.for.plot)<-c("gender","MeanTotalCharges")
ggplot(data.for.plot)+
  geom_bar(aes(x=gender,y=MeanTotalCharges),stat = "identity")
#no obviously result
#boxplot gender vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(gender),y=TotalCharges))+xlab("gender")
#no obviously result
#boxplot gender vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(gender),y=tenure))+xlab("gender")
#no obviously result
#boxplot gender vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(gender),y=MonthlyCharges))+xlab("gender")
#no obviously result
#boxplot Partner  vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Partner),y=tenure))+xlab("Partner")
#boxplot Partner  vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Partner),y=TotalCharges))+xlab("Partner")
#boxplot Partner  vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Partner),y=MonthlyCharges))+xlab("Partner")
#boxplot Dependents  vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Dependents),y=tenure))+xlab("Dependents")
#boxplot Dependents  vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Dependents),y=TotalCharges))+xlab("Dependents")
#boxplot Dependents  vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Dependents),y=MonthlyCharges))+xlab("Dependents")