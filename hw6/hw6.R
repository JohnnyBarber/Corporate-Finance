library("readxl")
snp = read_excel("PE vs MV.xlsx")
snp = snp[complete.cases(snp),]

#EP calculating --------------------------------------------------
com = length(unique(snp$`Ticker Symbol`))
n = length(snp$`Data Year - Fiscal`)
temp = c()
EP = c() #matrix(0,nrow = 1, ncol = com)
for (i in 1:(n-1))
{
  if (snp$`Company Name`[i] == snp$`Company Name`[i+1]){
    temp = append(temp,snp$`Earnings Per Share (Basic) - Excluding Extraordinary Items`[i]/snp$`Price Close - Annual - Fiscal`[i])
  } else if (snp$`Company Name`[i] != snp$`Company Name`[i+1]){
    temp = append(temp,snp$`Earnings Per Share (Basic) - Excluding Extraordinary Items`[i]/snp$`Price Close - Annual - Fiscal`[i])
    EP = c(EP,mean(temp))
    temp = c()
  }
}
EP = c(EP,mean(append(temp,snp$`Earnings Per Share (Basic) - Excluding Extraordinary Items`[i+1]/snp$`Price Close - Annual - Fiscal`[i+1])))
#make change if the last 2 observations belongs to 2 different companies

#percentage change calculating ------------------------------------
temp = c()
delta = c()
j = 1
for (i in 1:(n-1))
{
  if (snp$`Company Name`[i] == snp$`Company Name`[i+1]){
    temp = append(temp,snp$`Earnings Before Interest and Taxes`[i+1]/snp$`Earnings Before Interest and Taxes`[i] - 1)
  } else if (snp$`Company Name`[i] != snp$`Company Name`[i+1]){
    temp = append(temp,snp$`Earnings Before Interest and Taxes`[i]/snp$`Earnings Before Interest and Taxes`[i] - 1)
    delta = c(delta,mean(temp))
    temp = c()
    j = j + 1
  }
}
delta = c(delta,mean(temp))

#market cap avg calculating ------------------------------------
temp = c()
mcap = c()
j = 1
for (i in 1:(n-1))
{
  if (snp$`Company Name`[i] == snp$`Company Name`[i+1]){
    temp = append(temp,log(snp$`Market Value - Total - Fiscal`[i]))
  } else if (snp$`Company Name`[i] != snp$`Company Name`[i+1]){
    temp = append(temp,log(snp$`Market Value - Total - Fiscal`[i]))
    mcap = c(mcap,mean(temp))
    temp = c()
    j = j + 1
  }
}
mcap = c(mcap,mean(append(temp,log(snp$`Market Value - Total - Fiscal`[i+1]))))

#combine---------------------------------------------------------
All = data.frame(EP,delta,mcap)
interval1 = quantile(All$delta, probs = seq(0,1,0.01))
ready = All[which(All$delta >= interval1[11]),]
ready = ready[which(ready$delta <= interval1[length(interval1)-10]),]
interval2 = quantile(ready$EP, probs = seq(0,1,0.01))
ready = ready[which(ready$EP >= interval2[11]),]
ready = ready[which(ready$EP <= interval2[length(interval1)-10]),]
names(ready) = c("EP Ratio","Lagged Change in Percent Earnings",
                 "Market Cap")

#ggplot ---------------------------------------------------------
library("ggplot2")
Companies = c(1:length(ready$EP))
ggplot(ready, aes(x = `Lagged Change in Percent Earnings`,
                  y = `EP Ratio`)) + 
  geom_point(aes(size = `Market Cap`, color = Companies))

summary(ready)
