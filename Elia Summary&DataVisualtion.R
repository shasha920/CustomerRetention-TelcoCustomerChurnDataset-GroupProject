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
#test cor or each other numerical value
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


