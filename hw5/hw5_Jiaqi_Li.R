library(readxl)
hw2_data = read_excel("hw2_data.xlsx")
hw3_data = read_excel("stocks.xlsx")

mine_hw2 = read_excel("sp500_stock_data.xlsx")
mine_hw3 = read_excel("data_holding.xlsx")

#hw2-------------------------------------------------
plot(hw2_data$RETX, col = "red", type = "l",
     xlab = "time period", ylab = "return without dividend",
     main = "Data Comparision")
lines(mine_hw2$`Returns without Dividends`, col = "green")
legend("topright",legend = c("my data","peer data"),
       lty = c(1,1), col = c("green","red"), cex = 0.8)

peerR2 = hw2_data[,c("PERMNO","date","RETX")]
mineR2 = mine_hw2[,c("PERMNO","Names Date","Returns without Dividends")]
names(mineR2) = names(peerR2)

peerR_hw2 = peerR2[complete.cases(peerR2),]
mineR_hw2 = mineR2[complete.cases(mineR2),]
mean(peerR_hw2$RETX)
mean(mineR_hw2$RETX)
sd(peerR_hw2$RETX)
sd(mineR_hw2$RETX)

#run code
options(warn = -1)
snp = hw2_data

snp$RETX[is.na(snp$RETX)] = 0

p.all = which(!is.na(snp$PAYDT))
event = snp[p.all,]
n = length(p.all)
pos = vector()
for (i in 2:n)
{
  if (snp$PERMNO[p.all[i]] != snp$PERMNO[p.all[i]-21]
      || snp$PERMNO[p.all[i]] != snp$PERMNO[p.all[i]+20])
  {
    pos = append(pos,i)
  }
}

p = p.all[-pos]
p = p[-1]
RETX = snp$RETX
sprtrn = snp$sprtrn

row_EXR = matrix(0,ncol = 41)
for (i in 1:length(p))
{
  row_EXR = rbind(row_EXR,RETX[(p[i]-20):(p[i]+20)]
                  -sprtrn[(p[i]-20):(p[i]+20)])
}
row_EXR = row_EXR[-1,]
row_EXR[is.na(row_EXR)] = 0

mu = vector()
for (i in 1:41) { mu = append(mu,mean(row_EXR[,i]))}

X = seq(-20,20,1)
plot(X, mu, type = "o", main = "Event Study (Peer)",
     ylab = "Excess Return", xlab = "Day")

#hw3-------------------------------------------------
plot(mine_hw3$Returns, col = "green", type = "l",
     xlab = "time period", ylab = "return without dividend",
     main = "Data Comparision")
lines(hw3_data$Returns, col = "red")
legend("topright",legend = c("my data","peer data"),
       lty = c(1,1), col = c("green","red"), cex = 0.8)

peerR3 = hw3_data[,c("PERMNO","Names Date","Returns")]
mineR3 = mine_hw3[,c("PERMNO","Names Date","Returns")]
names(mineR3) = names(peerR3)

peerR_hw3 = peerR3[complete.cases(peerR3),]
temp = as.numeric(peerR_hw3$Returns)
peerR_hw3 = peerR_hw3[complete.cases(temp),]
peerR_hw3$Returns = as.numeric(peerR_hw3$Returns)
mineR_hw3 = mineR3[complete.cases(mineR3),]

mean(peerR_hw3$Returns)
mean(mineR_hw3$Returns)
sd(peerR_hw3$Returns)
sd(mineR_hw3$Returns)

#code run
library(lubridate)
library(moments)

options(warn = -1)

data = hw3_data
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

temppp = as.numeric(test$Returns)
test = test[complete.cases(temppp),]
test$Returns = as.numeric(test$Returns)

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

hist(effect,breaks = 60, main = "Histogram of effect (Peer)")
summary(effect)
summary(MC/1000)

Mstd = sd(effect)
Mm = mean(effect)
Kurt = kurtosis(effect)
Skew = skewness(effect)

stat = data.frame(Mm, Mstd, Kurt, Skew)
names(stat) = c("Mean", "Standard Deviation", "Kurtosis", "Skewness")
print(stat)