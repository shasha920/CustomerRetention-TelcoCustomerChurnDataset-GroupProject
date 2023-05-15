install.packages("FNN")
install.packages("caret")
install.packages("e1071")
install.packages("ggplot2")
install.packages("forecast")
install.packages("gplots")
install.packages("reshape")
install.packages("GGally")
install.packages("MASS")
install.packages("tidyverse")
install.packages("corrr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("gain")
#Library packages
library(rpart)
library(rpart.plot)
library(dplyr)
library(fastDummies)
library(FNN)
library(caret)
library(e1071)
library(tidyverse)
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
library(corrr)
library(gains)
options(scipen = 999)
#Total 7043 obs, 21 columns
telco.df<-read.csv("Telco-Customer-Churn.csv")

#remove null
telco.df[is.na(telco.df)] <- NA
telco.df<-telco.df[complete.cases(telco.df),]
#now Total 7032 obs, 21 columns

#data dimension reduction
telco.df<-telco.df[,-c(1,2)]

#Professor Plese skip this step!
#This is for Data exploration!
#Based everyone's code inside, for more convience, please jump into Naive Bayes model!
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

#boxplot DeviceProtection vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(DeviceProtection),y=tenure))+xlab("DeviceProtection")
#boxplot DeviceProtection vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(DeviceProtection),y=TotalCharges))+xlab("DeviceProtection")
#boxplot DeviceProtection vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(DeviceProtection),y=MonthlyCharges))+xlab("DeviceProtection")

#boxplot TechSupport vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(TechSupport),y=tenure))+xlab("TechSupport")
#boxplot TechSupport vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(TechSupport),y=TotalCharges))+xlab("TechSupport")
#boxplot TechSupport vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(TechSupport),y=MonthlyCharges))+xlab("TechSupport")

#boxplot StreamingTV vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingTV),y=tenure))+xlab("StreamingTVt")
#boxplot StreamingTV vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingTV),y=TotalCharges))+xlab("StreamingTV")
#boxplot StreamingTV vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingTV),y=MonthlyCharges))+xlab("StreamingTV")

#boxplot StreamingMovies vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingMovies),y=tenure))+xlab("StreamingMovies")
#boxplot StreamingMovies vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingMovies),y=TotalCharges))+xlab("StreamingMovies")
#boxplot StreamingMovies vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingMovies),y=MonthlyCharges))+xlab("StreamingMovies")

#boxplot PaymentMethod vs tenure
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaymentMethod),y=tenure))+xlab('PaymentMethod')
#boxplot PaymentMethod vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaymentMethod),y=TotalCharges))+xlab('PaymentMethod')
#boxplot PaymentMethod vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaymentMethod),y=MonthlyCharges))+xlab('PaymentMethod')

dim(telco.df)
head(telco.df)
telco.df[,8]
telco.df[,9]
telco.df[,10]
telco.df[,11]
telco.df[,21]
telco.df[,20]
telco.df[,19]
length(telco.df$MultipleLines)
length(telco.df$InternetService)
length(telco.df$OnlineSecurity)
length(telco.df$OnlineBackup)
summary(telco.df)

telco.df%>%
  correlate()%>%
  focus("TotalCharges")
telco.df%>%
  correlate()%>%
  focus("MonthlyCharges")

data.frame(mean=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],mean),
           sd=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],sd),
           min=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")], min),
           max=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],max),
           median=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],median),
           length=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],length),
           miss.val=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],function(x)sum(is.na(x))))


data.visual1.1 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$MultipleLines), FUN = mean)
names(data.visual1.1) <- c("MultipleLines", "MonthlyCharges")
barplot(data.visual1.1$MonthlyCharges, names.arg = data.visual1.1$MultipleLines,xlab = "MultipleLines", ylab = "MonthlyCharges")

data.visual1.2 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$InternetService), FUN = mean)
names(data.visual1.2) <- c("InternetService", "MonthlyCharges")
barplot(data.visual1.2$MonthlyCharges, names.arg = data.visual1.2$InternetService, xlab = "InternetService", ylab = "MonthlyCharges")

data.visual1.3 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$OnlineSecurity), FUN = mean)
names(data.visual1.3) <- c("OnlineSecurity", "MonthlyCharges")
barplot(data.visual1.3$MonthlyCharges, names.arg = data.visual1.3$OnlineSecurity
        ,xlab = "OnlineSecurity", ylab = "MonthlyCharges")

data.visual1.4 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$OnlineBackup), FUN = mean)
names(data.visual1.4) <- c("OnlineBackup", "MonthlyCharges")
barplot(data.visual1.4$MonthlyCharges, names.arg = data.visual1.4$OnlineBackup
        ,xlab = "OnlineBackup", ylab = "MonthlyCharges")


data.visual2.1 <- aggregate(telco.df$TotalCharges, by = list(telco.df$MultipleLines), FUN = mean, na.rm =TRUE)
names(data.visual2.1) <- c("MultipleLines", "TotalCharges")
barplot(data.visual2.1$TotalCharges, names.arg = data.visual2.1$MultipleLines,
        xlab = "MultipleLines", ylab = "TotalCharges")

data.visual2.2 <- aggregate(telco.df$TotalCharges, by = list(telco.df$InternetService), FUN = mean, na.rm = TRUE)
names(data.visual2.2) <- c("InternetService", "TotalCharges")
barplot(data.visual2.2$TotalCharges, names.arg = data.visual2.2$InternetService,
        xlab = "Intervet Service", ylab = "TotalCharges")

data.visual2.3 <- aggregate(telco.df$TotalCharges, by = list(telco.df$OnlineSecurity), FUN = mean, na.rm = TRUE)
names(data.visual2.3) <- c("OnlineSecurity", "TotalCharges")
barplot(data.visual2.3$TotalCharges, names.arg = data.visual2.3$OnlineSecurity,
        xlab = "Internet Security", ylab = "Total Charges")

data.visual2.4 <- aggregate(telco.df$TotalCharges, by = list(telco.df$OnlineBackup), FUN = mean, na.rm = TRUE)
names(data.visual2.4) <- c("OnlineBackup", "TotalCharges")
barplot(data.visual2.4$TotalCharges, names.arg = data.visual2.4$OnlineBackup,
        xlab = "Online Backup", ylab = "Total Charges")


data.visual3.1 <- aggregate(telco.df$tenure, by = list(telco.df$MultipleLines), FUN = mean, na.rm =TRUE)
names(data.visual3.1) <- c("MultipleLines", "Tenure")
barplot(data.visual3.1$Tenure, names.arg = data.visual3.1$MultipleLines,
        xlab = "MultipleLines", ylab = "Tenure")

data.visual3.2 <- aggregate(telco.df$tenure, by = list(telco.df$InternetService), FUN = mean, na.rm =TRUE)
names(data.visual3.2) <- c("InternetService", "Tenure")
barplot(data.visual3.2$Tenure, names.arg = data.visual3.2$InternetService,
        xlab = "InternetService", ylab = "Tenure")

data.visual3.3 <- aggregate(telco.df$tenure, by = list(telco.df$OnlineSecurity), FUN = mean, na.rm =TRUE)
names(data.visual3.3) <- c("OnlineSecurity", "Tenure")


barplot(data.visual3.3$Tenure, names.arg = data.visual3.3$OnlineSecurity,
        xlab = "OnlineSecurity", ylab = "Tenure")

data.visual3.4 <- aggregate(telco.df$tenure, by = list(telco.df$OnlineBackup), FUN = mean, na.rm = TRUE)
names(data.visual3.4) <- c("OnlineBackup", "Tenure")
barplot(data.visual3.4$Tenure, names.arg = data.visual3.4$OnlineBackup,
        xlab = "OnlineBackup", ylab = "Tenure")

telco.df%>%
  correlate()%>%
  focus("TotalCharges")
#summary Continuous Value
data.frame(mean=sapply(telco.df[c("MonthlyCharges","TotalCharges")],mean),
           sd=sapply(telco.df[c("MonthlyCharges","TotalCharges")],sd),
           min=sapply(telco.df[c("MonthlyCharges","TotalCharges")], min),
           max=sapply(telco.df[c("MonthlyCharges","TotalCharges")],max),
           median=sapply(telco.df[c("MonthlyCharges","TotalCharges")],median),
           length=sapply(telco.df[c("MonthlyCharges","TotalCharges")],length),
           miss.val=sapply(telco.df[c("MonthlyCharges","TotalCharges")],function(x)sum(is.na(x))))

#Summary Categorical Value

#customerID
customerID.freq <- telco.df %>%
  group_by(customerID)%>%
  summarise(n=n())
#Contract
Contract.freq <- telco.df %>%
  group_by(Contract)%>%
  summarise(n=n())
#PaperlessBilling
ParperlessBilling.freq <- telco.df %>%
  group_by(PaperlessBilling)%>%
  summarise(n=n())
#PaymentMethod
PaymentMethod.freq <- telco.df %>%
  group_by(PaymentMethod)%>%
  summarise(n=n())
#PhoneService
PhoneService.freq <- telco.df %>%
  group_by(PhoneService)%>%
  summarise(n=n())
#Churn
Churn.freq <- telco.df %>%
  group_by(Churn)%>%
  summarise(n=n())

#Data visualization
#boxplot customerID vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(customerID),y=tenure))+xlab("customerID")
#boxplot customerID vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(customerID),y=TotalCharges))+xlab("customerID")


#scatterplot MonthlyCharges vs TotalCharges
ggplot(telco.df)+
  geom_point(aes(x=MonthlyCharges,y=TotalCharges),color="purple",alpha=0.5)

#ggpair MonthlyCharges vs TotalCharges 
ggpairs(telco.df[,c("MonthlyCharges","TotalCharges")])


#barPot Churn vs MonthlyCharges
data.for.plot<-aggregate(telco.df$MonthlyCharges,by=list(telco.df$Churn),FUN=mean)
names(data.for.plot)<-c("Churn","MeanMonthlyCharges")
ggplot(data.for.plot)+
  geom_bar(aes(x=Churn,y=MeanMonthlyCharges),stat = "identity")

#barPot Churn vs TotalCharges
data.for.plot<-aggregate(telco.df$TotalCharges,by=list(telco.df$Churn),FUN=mean)
names(data.for.plot)<-c("Churn","MeanTotalCharges")
ggplot(data.for.plot)+
  geom_bar(aes(x=Churn,y=MeanTotalCharges),stat = "identity")


#boxplot PhoneService vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PhoneService),y=TotalCharges))+xlab("PhoneService")
#boxplot PhoneService  vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PhoneService),y=MonthlyCharges))+xlab("PhoneService")

#boxplot PaperlessBilling vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaperlessBilling),y=TotalCharges))+xlab("PaperlessBilling")
#boxplot PaperlessBilling vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaperlessBilling),y=MonthlyCharges))+xlab("PaperlessBilling")


#boxplot Contract vs TotalCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Contract),y=TotalCharges))+xlab("Contract")
#boxplot PaperlessBilling vs MonthlyCharges
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Contract),y=MonthlyCharges))+xlab("Contract")


#test Churn cor with other values
#barPot Churn vs tenure
data.for.plot<-aggregate(df$Churn,by=list(telco.df$gender),FUN=mean)
names(data.for.plot)<-c("Churn","Meantenure")
ggplot(data.for.plot)+
  geom_bar(aes(x=Churn,y=Meantenure),stat = "identity")

#boxplot PaymentMethod vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaymentMethod),y=Churn))+xlab('PaymentMethod')

#boxplot Contract vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Contract),y=Churn))+xlab('Contract')

#boxplot PaperlessBilling vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PaperlessBilling),y=Churn))+xlab('PaperlessBilling')

#boxplot PhoneService vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(PhoneService),y=Churn))+xlab('PhoneService')

#boxplot gender vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(gender),y=Churn))+xlab('gender')

#boxplot SeniorCitizen vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(SeniorCitizen),y=Churn))+xlab('SeniorCitizen')

#boxplot Partner vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Partner),y=Churn))+xlab('Partner')

#boxplot Dependents vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(Dependents),y=Churn))+xlab('Dependents')

#boxplot DeviceProtection vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(DeviceProtection),y=Churn))+xlab('DeviceProtection')

#boxplot TechSupport vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(TechSupport),y=Churn))+xlab('TechSupport')

#boxplot StreamingTV vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingTV),y=Churn))+xlab('StreamingTV')

#boxplot StreamingMovies vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(StreamingMovies),y=Churn))+xlab('StreamingMovies')

#boxplot MultipleLines vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(MultipleLines),y=Churn))+xlab('MultipleLines')

#boxplot InternetService vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(InternetService),y=Churn))+xlab('InternetService')

#boxplot OnlineSecurity vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(OnlineSecurity),y=Churn))+xlab('OnlineSecurity')

#boxplot OnlineBackup vs Churn
ggplot(telco.df)+
  geom_boxplot(aes(x=as.factor(OnlineBackup),y=Churn))+xlab('OnlineBackup')


#before normalization data do model1 naive bayes model
#naive bayes model before I build other model need train every catagorical to numeric
#factor all catagorical value
telco.df$Contract<-factor(telco.df$Contract)
telco.df$PaperlessBilling<-factor(telco.df$PaperlessBilling)
telco.df$PaymentMethod<-factor(telco.df$PaymentMethod)
telco.df$PhoneService<-factor(telco.df$PhoneService)
telco.df$SeniorCitizen<-factor(telco.df$SeniorCitizen)
telco.df$Partner<-factor(telco.df$Partner)
telco.df$Dependents<-factor(telco.df$Dependents)
telco.df$DeviceProtection<-factor(telco.df$DeviceProtection)
telco.df$TechSupport<-factor(telco.df$TechSupport)
telco.df$StreamingTV<-factor(telco.df$StreamingTV)
telco.df$StreamingMovies<-factor(telco.df$StreamingMovies)
telco.df$MultipleLines<-factor(telco.df$MultipleLines)
telco.df$InternetService<-factor(telco.df$InternetService)
telco.df$OnlineSecurity<-factor(telco.df$OnlineSecurity)
telco.df$OnlineBackup<-factor(telco.df$OnlineBackup)
#select a subset of variables for classification
#cut continuous values
selected.var<- -c(1,4,18,17)
names(telco.df)[selected.var]
nb.df<-telco.df[,selected.var]
#set Churn level
nb.df[,15]<-factor(nb.df[,15],levels = c("Yes","No"))
levels(nb.df$Churn)
#Partition the data use 10-fold cross
set.seed(1)
fold <- createFolds(nb.df$Churn,k=10)
fold

valid.result <- data.frame()
train.result <- data.frame()

for(i in 1:10){
  valid.data <- nb.df[fold[[i]],]
  train.data <- nb.df[-fold[[i]],]
  
  tel.nb<-naiveBayes(Churn~.,data = train.data)
  # Compute propensity
  valid.pred.prob <- predict(tel.nb, newdata = valid.data, type = "raw")
  train.pred.prob <- predict(tel.nb, newdata = train.data, type = "raw")
  # Classification
  nb.valid.pred.class <- predict(tel.nb,newdata = valid.data)
  nb.train.pred.class <- predict(tel.nb,newdata = train.data)
  
  valid.sub.fold <- data.frame('prob' = valid.pred.prob, 'class' = nb.valid.pred.class, 'actual' = valid.data[, 15])
  train.sub.fold <- data.frame('prob' = train.pred.prob, 'class' = nb.train.pred.class, 'actual' = train.data[, 15])
  valid.result<- rbind(valid.result, valid.sub.fold)
  train.result <- rbind(train.result, train.sub.fold)
}

tel.nb

#Performance evaluating of traning set
confusionMatrix(train.result$class,train.result$actual)
#Performance evaluation of validation set
confusionMatrix(valid.result$class,valid.result$actual)

#ROC curve
a<-ifelse(train.result$class=='Yes',1,0) 
b<-ifelse(train.result$actual=='Yes',1,0)
nb.train.roc <- roc(predictor = a,response=b,levels = c(0,1),direction='<')
c<-ifelse(valid.result$class =='Yes',1,0)
d<- ifelse(valid.result$actual=='Yes',1,0)
nb.valid.roc <- roc(predictor=c,response=d,levels = c(0,1),direction='<')
plot.roc(nb.train.roc, col = 'blue', print.auc = TRUE)
plot.roc(nb.valid.roc, add = TRUE, col = 'pink', print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)

#Lift chart on train set
gain<-gains(ifelse(train.result$actual=="Yes",1,0),train.result$prob.Yes,groups=100)
plot(c(0,gain$cume.pct.of.total*sum(train.result$actual=="Yes"))~c(0,gain$cume.obs),
     xlab="# of cases",ylab="cumulative #of Churn detected",main="NB Train Lift Chart of tel",type="l")
lines(c(0,sum(train.result$actual=="Yes"))~c(0,dim(train.result)[1]),lty=2)

#Lift chart on valid set
gain<-gains(ifelse(valid.result$actual=="Yes",1,0),valid.result$prob.Yes,groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.result$actual=="Yes"))~c(0,gain$cume.obs),
     xlab="# of cases",ylab="cumulative #of Churn detected",main="NB Valid Lift Chart of tel",type="l")
lines(c(0,sum(valid.result$actual=="Yes"))~c(0,dim(valid.result)[1]),lty=2)
#good


#Normalization Data
#set Churn yes as 1 no as 0
telco.df<-telco.df%>%
  mutate(Churn=ifelse(Churn=="Yes",1,0))

#create Contract to dummy dummy 0(one year above) or 1(month to month)
telco.df<-telco.df%>%
  mutate(Contract=ifelse(Contract=="Month-to-month",1,0))


#create InternetService to dummy 1(Fiber optic) or 0(No,DSL,)
telco.df<-telco.df%>%
  mutate(InternetService=ifelse(InternetService=="Fiber optic",1,0))

#create OnlineSecurity to dummy 1(No) or 0(Yes,No internet service)
telco.df<-telco.df%>%
  mutate(OnlineSecurity=ifelse(OnlineSecurity=="No",1,0))

#create Partner to dummy 1(No) or 0(Yes)
telco.df<-telco.df%>%
  mutate(Partner=ifelse(Partner=="No",1,0))

#create Dependents to dummy 0(Yes) or 1(No)
telco.df<-telco.df%>%
  mutate(Dependents=ifelse(Dependents=="No",1,0))

#create PhoneService to dummy 1(Yes) or 0(No)
telco.df<-telco.df%>%
  mutate(PhoneService=ifelse(PhoneService=="No",0,1))

#create MultipleLines to dummy 1(Yes,No) or 0(No phone service)
telco.df<-telco.df%>%
  mutate(MultipleLines=ifelse(MultipleLines=="No phone service",0,1))

#create OnlineBackup to dummy 1(No) or 0(Yes,No internet service)
telco.df<-telco.df%>%
  mutate(OnlineBackup=ifelse(OnlineBackup=="No",1,0))

#create DeviceProtection to dummy 1(No) or 0(Yes,No internet service)
telco.df<-telco.df%>%
  mutate(DeviceProtection=ifelse(DeviceProtection=="No",1,0))

#create StreamingTV to dummy 1(Yes,No) or 0(No internet service)
telco.df<-telco.df%>%
  mutate(StreamingTV=ifelse(StreamingTV=="No internet service",0,1))

#create StreamingMovies to dummy 1(Yes,No) or 0(No internet service)
telco.df<-telco.df%>%
  mutate(StreamingMovies=ifelse(StreamingMovies=="No internet service",0,1))

#create PaperlessBilling to dummy 1(Yes) or 0(No)
telco.df<-telco.df%>%
  mutate(PaperlessBilling=ifelse(PaperlessBilling=="No",0,1))

#create PaymentMethod to dummy 0(Bank transfer(automatic),Mailed check,Credit card(automatic))
#or 1(Electronic check)
telco.df<-telco.df%>%
  mutate(PaymentMethod=ifelse(PaymentMethod=="Electronic check",1,0))

#create TechSupport to dummy 1(No) or 0(Yes,No internet service)
telco.df<-telco.df%>%
  mutate(TechSupport=ifelse(TechSupport=="No",1,0))
#Normalization data save to cvs, give other teammate to use it
#write.csv(telco.df, "telco.csv", row.names=FALSE)

#Model2 Logistic Regression
#Partition the data use 10-fold cross
set.seed(1)
fold <- createFolds(telco.df$Churn,k=10)
fold

valid.result <- data.frame()
train.result <- data.frame()

for(i in 1:10){
  valid.data <- telco.df[fold[[i]],]
  train.data <- telco.df[-fold[[i]],]
  
  logit.reg <- glm(Churn ~ ., data = train.data, family = "binomial")
  # Compute propensity
  valid.pred.prob <- predict(logit.reg, valid.data[,-19], type = "response")
  train.pred.prob <- predict(logit.reg, train.data[,-19], type = "response")
  # Classification
  logit.valid.pred.class <- ifelse(valid.pred.prob >= 0.5,1,0)
  logit.train.pred.class <- ifelse(train.pred.prob >= 0.5,1,0)
  
  valid.sub.fold <- data.frame('prob' = valid.pred.prob, 'class' = logit.valid.pred.class, 'actual' = valid.data[, 19])
  train.sub.fold <- data.frame('prob' = train.pred.prob, 'class' = logit.train.pred.class, 'actual' = train.data[, 19])
  valid.result<- rbind(valid.result, valid.sub.fold)
  train.result <- rbind(train.result, train.sub.fold)
}

summary(logit.reg)

#Performance evaluation on training set
confusionMatrix(factor(train.result$class,levels=c(1,0)),
                factor(train.result$actual,levels = c(1,0)))
#Performance evaluation on valid set
confusionMatrix(factor(valid.result$class,levels=c(1,0)),
                factor(valid.result$actual,levels = c(1,0)))

#ROC curve
logit.train.roc <- roc(predictor = train.result$prob, response = train.result$actual, levels = c(0,1), direction='<')
logit.valid.roc <- roc(predictor = valid.result$prob, response = valid.result$actual, levels = c(0,1), direction='<')
plot.roc(logit.train.roc, col = 'blue', print.auc = TRUE)
plot.roc(logit.valid.roc, add = TRUE, col = 'pink', print.auc = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)


#Lift chart on train set
gain<-gains(train.result$actual,train.result$prob,groups=length(train.result$prob))
plot(c(0,gain$cume.pct.of.total*sum(train.result$actual))~c(0,gain$cume.obs),
     xlab="#cases",ylab="Cumulative # of responses", main="glm Train Lift Chart of Telco Case",type="l")
lines(c(0,sum(train.result$actual))~c(0,dim(train.result)[1]),lty=2)

#Lift chart on valid set
gain<-gains(valid.result$actual,valid.result$prob,groups=length(valid.result$prob))
plot(c(0,gain$cume.pct.of.total*sum(valid.result$actual))~c(0,gain$cume.obs),
     xlab="#cases",ylab="Cumulative # of responses", main="glm Valid Lift Chart of Telco Case",type="l")
lines(c(0,sum(valid.result$actual))~c(0,dim(valid.result)[1]),lty=2)
#good


#build model3 classification tree
#set Churn level
telco.df[,19]<-factor(telco.df[,19],levels = c(1,0))
levels(telco.df$Churn)
#Partition data into training(70%) and validation(30%) sets
set.seed(1)
train.index<-sample(row.names(telco.df),dim(telco.df)[1]*0.7)
valid.index<-setdiff(row.names(telco.df),train.index)
train.df<-telco.df[train.index,]
valid.df<-telco.df[valid.index,]
#Build the default (best_pruned) classification tree
telco.default.ct<-rpart(Churn~.,data=train.df,method = "class",
                        control = rpart.control(xval=10))

#Plot tree
prp(telco.default.ct,type = 1,extra=1,under=TRUE,split.font = 2,
    under.font = 1,nn.font = 3,varlen = -10,
    box.col = ifelse(telco.default.ct$frame$var=="<leaf>","gray","pink"))

#Performance evaluation on training set
telco.default.ct.pred.train<-predict(telco.default.ct,train.df,type="class")
confusionMatrix(telco.default.ct.pred.train,as.factor(train.df$Churn))

#Performance evaluation on validation set
telco.default.ct.pred.valid<-predict(telco.default.ct,valid.df,type="class")
confusionMatrix(telco.default.ct.pred.valid,as.factor(valid.df$Churn))
#good


#Build a fully grown classification tree
telco.full.ct<-rpart(Churn~., data=train.df,method = "class",
                     control = rpart.control(minsplit = 1,cp=0))

#Plot tree
prp(telco.full.ct,type=1,extra=1,under=TRUE,split.font = 2,
    under.font = 1,nn.font = 3,varlen = -10,
    box.col = ifelse(telco.full.ct$frame$var=="<leaf>","gray","pink"))

#Performance evaluation on training set
telco.full.ct.pred.train<-predict(telco.full.ct,train.df,type = "class")
confusionMatrix(telco.full.ct.pred.train,as.factor(train.df$Churn))

#Performance evaluation on validation set
telco.full.ct.pred.valid<-predict(telco.full.ct,valid.df,type = "class")
confusionMatrix(telco.full.ct.pred.valid,as.factor(valid.df$Churn))
