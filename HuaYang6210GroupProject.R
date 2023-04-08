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
options(scipen = 999)
#Total 7043 obs, 21 columns
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


#remove null
telco.df[is.na(telco.df)] <- NA
telco.df<-telco.df[complete.cases(telco.df),]
#now Total 7032 obs, 21 columns

#data dimension reduction
telco.df<-telco.df[,-c(1)]

#create Contract to dummy dummy 1(one year above) or 0(month to month)1(DSL,Fiber optic) or 0(No)
telco.df<-telco.df%>%
  mutate(Contract=ifelse(Contract=="Month-to-month",0,1))

#create InternetService to dummy 1(DSL,Fiber optic) or 0(No)
telco.df<-telco.df%>%
  mutate(InternetService=ifelse(InternetService=="No",0,1))

#create OnlineSecurity to dummy 1(Yes) or 0(No,No internet service)
telco.df<-telco.df%>%
  mutate(OnlineSecurity=ifelse(OnlineSecurity=="Yes",1,0))

#create Partner to dummy 1(Yes) or 0(No)
telco.df<-telco.df%>%
  mutate(Partner=ifelse(Partner=="No",0,1))

#create Dependents to dummy 1(Yes) or 0(No)
telco.df<-telco.df%>%
  mutate(Dependents=ifelse(Dependents=="No",0,1))

#create PhoneService to dummy 1(Yes) or 0(No)
telco.df<-telco.df%>%
  mutate(PhoneService=ifelse(PhoneService=="No",0,1))

#create MultipleLines to dummy 1(Yes) or 0(No,No phone service)
telco.df<-telco.df%>%
  mutate(MultipleLines=ifelse(MultipleLines=="Yes",1,0))

#create OnlineBackup to dummy 1(Yes) or 0(No,No internet service)
telco.df<-telco.df%>%
  mutate(OnlineBackup=ifelse(OnlineBackup=="Yes",1,0))

#create DeviceProtection to dummy 1(Yes) or 0(No,No internet service)
telco.df<-telco.df%>%
  mutate(DeviceProtection=ifelse(DeviceProtection=="Yes",1,0))

#create StreamingTV to dummy 1(Yes) or 0(No,No internet service)
telco.df<-telco.df%>%
  mutate(StreamingTV=ifelse(StreamingTV=="Yes",1,0))

#create StreamingMovies to dummy 1(Yes) or 0(No,No internet service)
telco.df<-telco.df%>%
  mutate(StreamingMovies=ifelse(StreamingMovies=="Yes",1,0))

#create PaperlessBilling to dummy 1(Yes) or 0(No)
telco.df<-telco.df%>%
  mutate(PaperlessBilling=ifelse(PaperlessBilling=="No",0,1))

#create PaymentMethod to dummy 1(Bank transfer(automatic),Credit card(automatic))
#or 0(Electronic check,Mailed check)
telco.df<-telco.df%>%
  mutate(PaymentMethod=ifelse((PaymentMethod=="Bank transfer (automatic)")| 
                              (PaymentMethod=="Credit card (automatic)"),
                                1,0))

#create TechSupport to dummy 1(Yes) or 0(No,No internet service)
telco.df<-telco.df%>%
  mutate(TechSupport=ifelse(TechSupport=="Yes",1,0))

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