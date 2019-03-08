library(readxl)
library(lubridate)
library(moments)

options(warn = -1)

data = read_excel("data_holding.xlsx")
data = data[complete.cases(data),]
data$MC = abs(data$`Price or Bid/Ask Average`)*data$`Shares Outstanding`
test = data[order(data$PERMNO),]
test$`Names Date` = as.Date(as.character(test$`Names Date`), format = "%Y%m%d")
n = length(data$PERMNO)
nS = length(unique(test$PERMNO))
len = vector()
sep = vector()
name = vector()
MC = vector()
Year = vector()

mc = test$MC[1]
X = 1+test$Returns[1]
count = 1
for (i in 1:(n-1))
{
  if (test$PERMNO[i+1] == test$PERMNO[i] &&
      year(test$`Names Date`[i+1]) == year(test$`Names Date`[i]))
  {
    X = X*(1+test$Returns[i+1])
    count = count + 1
    mc = mc + test$MC[i+1]
  }
  else
  {
    len = append(len,count)
    Y = X^(12/count)-1
    sep = append(sep,Y)
    name = append(name,test$`Ticker Symbol`[i])
    MC = append(MC, mc/count)
    Year = append(Year,year(test$`Names Date`[i+1]))
    X = 1 + test$Returns[i+1]
    count = 1
    mc = test$MC[i+1]
  }
}

full = data.frame(Year,name,MC/1000,sep)
quant = quantile(sep, probs = seq(0,1,0.01))
effect = sep[which(sep>(-0.9385828))]
effect = effect[which(effect < 8.1856168)]
write.csv(full, file = "data summary.csv")

hist(effect,breaks = 60)
summary(effect)
summary(MC /1000)

Mstd = sd(effect)
Mm = mean(effect)
Kurt = kurtosis(effect)
Skew = skewness(effect)

stat = data.frame(Mm, Mstd, Kurt, Skew)
names(stat) = c("Mean", "Standard Deviation", "Kurtosis", "Skewness")
print(stat)
