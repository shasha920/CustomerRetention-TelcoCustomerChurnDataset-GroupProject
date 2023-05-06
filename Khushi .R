# Install and load necessary packages
install.packages(c("forecast", "ggplot2", "gplots", "reshape", "GGally", "MASS", "corrr"))
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
library(corrr)

# Read in the data
telco.df <- read.csv("C:/Users/panch/OneDrive/Desktop/Khushi Project/Telco-Customer-Churn.csv")
telco.df

# Summary statistics for continuous variables
cont_vars <- c("MonthlyCharges", "TotalCharges", "tenure")
cont_summary <- data.frame(
  mean = sapply(telco.df[cont_vars], mean),
  sd = sapply(telco.df[cont_vars], sd),
  min = sapply(telco.df[cont_vars], min),
  max = sapply(telco.df[cont_vars], max),
  median = sapply(telco.df[cont_vars], median),
  length = sapply(telco.df[cont_vars], length),
  miss.val = sapply(telco.df[cont_vars], function(x) sum(is.na(x)))
)
print(cont_summary)

# Summary statistics for categorical variables
cat_vars <- c("DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies", "Churn")
for (var in cat_vars) {
  cat_summary <- telco.df %>%
    group_by(!!sym(var)) %>%
    summarise(n = n())
  print(cat_summary)
}

# Data visualization
# Boxplot DeviceProtection vs tenure
ggplot(telco.df) +
  geom_boxplot(aes(x = as.factor(DeviceProtection), y = tenure)) +
  xlab("DeviceProtection")

# Boxplot DeviceProtection vs TotalCharges
ggplot(telco.df) +
  geom_boxplot(aes(x = as.factor(DeviceProtection), y = TotalCharges)) +
  xlab("DeviceProtection")

# Boxplot DeviceProtection vs MonthlyCharges
ggplot(telco.df) +
  geom_boxplot(aes(x = as.factor(DeviceProtection), y = MonthlyCharges)) +
  xlab("DeviceProtection")

# Scatterplot TotalCharges vs tenure
ggplot(telco.df) +
  geom_point(aes(x = tenure, y = TotalCharges), color = "pink", alpha = 0.5)

# Scatterplot MonthlyCharges vs TotalCharges
ggplot(telco.df) +
  geom_point(aes(x = MonthlyCharges, y = TotalCharges), color = "steelblue", alpha = 0.5)

# Scatterplot MonthlyCharges vs tenure
ggplot(telco.df) +
  geom_point(aes(x = tenure, y = MonthlyCharges), color = "darkgreen", alpha = 0.5)

# Pairwise scatterplots
ggpairs(telco.df[, c("MonthlyCharges", "TotalCharges", "tenure")])

# Barplot Churn vs MonthlyCharges
data.for.plot <- aggregate(telco.df$MonthlyCharges, by = list(telco.df$Churn), FUN = mean)
names(data.for.plot) <- c("Churn", "MeanMonthlyCharges")
ggplot(data.for.plot) +
  geom_bar(aes(x = Churn, y = MeanMonthlyCharges), stat = "identity")

# Barplot Churn vs tenure
data.for.plot <- aggregate(telco.df$tenure, by = list(telco.df$Churn), FUN = mean)
names(data.for.plot)
