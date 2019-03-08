snp = read.csv("sp500_stock_data.csv")

p.all = which(!is.na(snp$`Payment Date`))
event = snp[p.all,]
n = length(p.all)
pos = vector()
for (i in 2:n)
{
  if (snp$TICKER[p.all[i]] != snp$TICKER[p.all[i]-21]
      || snp$TICKER[p.all[i]] != snp$TICKER[p.all[i]+20])
  {
    pos = append(pos,i)
  }
}

p = p.all[-pos]
p = p[-1]

row_EXR = matrix(0,ncol = 41)
for (i in 1:length(p))
{
  row_EXR = rbind(row_EXR,as.numeric(as.vector(snp$RETX[(p[i]-20):(p[i]+20)]))
                  -as.numeric(as.vector(snp$sprtrn[(p[i]-20):(p[i]+20)])))
}
row_EXR = row_EXR[-1,]
row_EXR[is.na(row_EXR)] = 0

mu = vector()
for (i in 1:41) { mu = append(mu,mean(row_EXR[,i]))}

plot(mu, type = "o", main = "Event Study",
     ylab = "Excess Return", xlab = "Day")
