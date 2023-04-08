setwd("~/Desktop/Spring 2023 MSDSCM/Data Mining Spring 2023")
telco.df <- read.csv("Telco-Customer-Churn.csv")
View(telco.df)
View(telco.df)
#4 categorical variables: 1. mutiple lines, 2.internet service, 3.online security, 4. online backup
#2 everyone analyzes: 1. customer ID, 2. Churn 
#2 continuous variables: 1. monthly charges, 2. total charges 3.tenure
 
#1. Data Exploration: proper summary statistics and visualization tools to understand the distribution of individual variables and relationship between two or more variables. 
#1.1: use summary statistics to understand the distribution & relationships between variables
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

#libary packages
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
library(corrr)

telco.df%>%
  correlate()%>%
  focus("TotalCharges")
telco.df%>%
  correlate()%>%
  focus("MonthlyCharges")

#summarize continuous values (1. monthly charges 2. total charges 3. tenure)
data.frame(mean=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],mean),
           sd=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],sd),
           min=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")], min),
           max=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],max),
           median=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],median),
           length=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],length),
           miss.val=sapply(telco.df[c("MonthlyCharges","TotalCharges","tenure")],function(x)sum(is.na(x))))

#summarize categorical values (1. mutiple lines, 2.internet service, 3.online security, 4. online backup)
data.visual.1 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$MultipleLines), FUN = mean)
names(data.visual.1) <- c("MultipleLines", "MonthlyCharges")
barplot(data.visual.1$MonthlyCharges, names.arg = data.visual.1$MultipleLines,xlab = "MultipleLines", ylab = "MonthlyCharges")

data.visual.2 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$InternetService), FUN = mean)
names(data.visual.2) <- c("InternetService", "MonthlyCharges")
barplot(data.visual.2$MonthlyCharges, names.arg = data.visual.2$InternetService, xlab = "InternetService", ylab = "MonthlyCharges")

data.visual.3 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$OnlineSecurity), FUN = mean)
names(data.visual.3) <- c("OnlineSecurity", "MonthlyCharges")
barplot(data.visual.3$MonthlyCharges, names.arg = data.visual.3$OnlineSecurity
        ,xlab = "OnlineSecurity", ylab = "MonthlyCharges")

data.visual.4 <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$OnlineBackup), FUN = mean)
names(data.visual.4) <- c("OnlineBackup", "MonthlyCharges")
barplot(data.visual.4$MonthlyCharges, names.arg = data.visual.4$OnlineBackup
        ,xlab = "OnlineBackup", ylab = "MonthlyCharges")

