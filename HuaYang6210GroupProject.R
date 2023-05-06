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

#This is for Data exploration
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


#before normalization data do model 1 naive bayes model
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
#set Churn level
telco.df[,19]<-factor(telco.df[,19],levels = c("Yes","No"))
levels(telco.df$Churn)
#Partition data into training(70%) and validation(30%) sets
set.seed(1)
train.index<-sample(row.names(telco.df),dim(telco.df)[1]*0.7)
valid.index<-setdiff(row.names(telco.df),train.index)
train.df<-telco.df[train.index,selected.var]
valid.df<-telco.df[valid.index,selected.var]
tel.nb<-naiveBayes(Churn~.,data = train.df)
tel.nb

#compute the propensity
pred.prob <- predict(tel.nb, newdata = valid.df, type = "raw")

#Performance evaluating of traning set
pred.class<-predict(tel.nb,newdata = train.df)
confusionMatrix(pred.class,train.df$Churn)
#Performance evaluation of validation set
pred.class<-predict(tel.nb,newdata = valid.df)
confusionMatrix(pred.class,valid.df$Churn)

#Draw lift chart
gain<-gains(ifelse(valid.df$Churn=="Yes",1,0),pred.prob[,1],groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Churn=="Yes"))~c(0,gain$cume.obs),
     xlab="# of cases",ylab="cumulative #of Churn detected",main="Lift Chart of tel",type="l")
lines(c(0,sum(valid.df$Churn=="Yes"))~c(0,dim(valid.df)[1]),lty=2)
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

#Partition the data use 10-fold cross
fold.10 <- createFolds(telco.df$Churn,k=10)
fold.10

train.data <- list()
vaild.data <- list()

for(i in 1:10){
  valid.data <- telco.df[fold.10[[i]],]
  train.data <- telco.df[-fold.10[[i]],]
}


valid.result <- data.frame()
train.result <- data.frame()

for(i in 1:10){  
  logit.reg <- glm(Churn ~ ., data = train.data, family = "binomial") 
  # Compute propensity
  valid.pred.prob <- predict(logit.reg, valid.data[,-19], type = "response")
  train.pred.prob <- predict(logit.reg, train.data[,-19], type = "response")
  #Classification: Equally important
  logit.valid.pred.class <- ifelse(valid.pred.prob >= 0.5, 1, 0)
  logit.train.pred.class <- ifelse(train.pred.prob >= 0.5,1, 0)
  valid.sub.fold <- data.frame('prob' = valid.pred.prob, 'class' = logit.valid.pred.class,'actual' = valid.data[, 19])
  train.sub.fold <- data.frame('prob' = train.pred.prob, 'class' = logit.train.pred.class,'actual' = train.data[, 19])
  logit.valid.result<- rbind(valid.result, valid.sub.fold)
  logit.train.result <- rbind(train.result, train.sub.fold)
}

#Performance evaluation of validation set
confusionMatrix(factor(logit.valid.pred.class,levels=c(1,0)),
                factor(valid.data$Churn,levels=c(1,0)))
#Performance evaluating of traning set
confusionMatrix(factor(logit.train.pred.class,levels=c(1,0)),
                factor(train.data$Churn,levels=c(1,0)))

#lift chart
gain<-gains(valid.data$Churn,valid.pred.prob,groups=length(valid.pred.prob))
plot(c(0,gain$cume.pct.of.total*sum(valid.data$Churn))~c(0,gain$cume.obs),
     xlab="#cases",ylab="cumulative #of responses",main="Lift Chart of tel",type="l")
lines(c(0,sum(valid.data$Churn))~c(0,dim(valid.data)[1]),lty=2)
#good


#build model2 classification tree
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
#telco.full.ct<-rpart(Churn~., data=train.df,method = "class",
                     #control = rpart.control(minsplit = 1,cp=0))

#Plot tree
#prp(telco.full.ct,type=1,extra=1,under=TRUE,split.font = 2,
    #under.font = 1,nn.font = 3,varlen = -10,
    #box.col = ifelse(telco.full.ct$frame$var=="<leaf>","gray","pink"))

#Performance evaluation on training set
#telco.full.ct.pred.train<-predict(telco.full.ct,train.df,type = "class")
#confusionMatrix(telco.full.ct.pred.train,as.factor(train.df$Churn))

#Performance evaluation on validation set
#telco.full.ct.pred.valid<-predict(telco.full.ct,valid.df,type = "class")
#confusionMatrix(telco.full.ct.pred.valid,as.factor(valid.df$Churn))

#Perform cross validation within training dataset and record Compexity Parameters(cp)
#telco.ct<-rpart(Churn~., data=train.df,method = "class",
                #control = rpart.control(cp=0.00001,minsplit = 1,xval=10))
#rel error (relative error): training erro
#xerror (relative error):validation error
#xstd (relative stdev):validation stdev
#Each row represents a different level of tree
#which is the best tree with the same level
#printcp(telco.ct)
#The plot shows the change of xerror with CP
#The minimum line is the minimum xerror plus xstd
#The first xerror that drops below the minmum line
#corresponds to the best cp - best pruned tree
#plotcp(telco.ct)
#get reuslt nsplit best for 23




