options(warn = -1)
library(readxl)
snp = read_excel("sp500_stock_data.xlsx")

snp$`Returns without Dividends`[is.na(snp$`Returns without Dividends`)] = 0

p.all = which(!is.na(snp$`Payment Date`))
event = snp[p.all,]
n = length(p.all)
pos = vector()
for (i in 2:n)
{
  if (snp$`Ticker Symbol`[p.all[i]] != snp$`Ticker Symbol`[p.all[i]-21]
      || snp$`Ticker Symbol`[p.all[i]] != snp$`Ticker Symbol`[p.all[i]+20])
  {
    pos = append(pos,i)
  }
}

p = p.all[-pos]
p = p[-1]
RETX = snp$`Returns without Dividends`
sprtrn = snp$`Return on the S&P 500 Index`

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
plot(X, mu, type = "o", main = "Event Study",
     ylab = "Excess Return", xlab = "Day")
