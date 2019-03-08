#error computation-----------------------------
#mean rate of return
mon = read.csv("hw1 monthly.csv")
return.mon = mon$vwretd
n.mon = length(return.mon)
ar.mon = mean(return.mon)*12
geo.mon = prod(1+return.mon)^(12/n.mon) - 1

day = read.csv("hw1 daily.csv")
return.day = day$vwretd
n.day = length(return.day)
ar.day = mean(return.day)*252
geo.day = prod(1+return.day)^(252/n.day) - 1

library(xts)
date = as.Date(as.character(day$caldt),format = "%Y%m%d")
day.xts = xts(day$vwretd, order.by = date)
year.p = endpoints(day.xts, on = "years", k = 1)
n.year = length(year.p)
return.year = vector()

i = 1
while (i < n.year)
{
  return.year = append(return.year,prod(1+day$vwretd[year.p[i]:year.p[i+1]])-1)
  i = i + 1
}

year5.p = endpoints(day.xts, on = "years", k = 5)
n.year5 = length(year5.p)
return.year5 = vector()

j = 1
while (j < n.year5)
{
  return.year5 = append(return.year5, prod(1+day$vwretd[year5.p[j]:year5.p[j+1]])-1)
  j = j + 1
}

# j = 1
# while (j < n.year)
# {
#   return.year5 = append(return.year5, prod(1+return.year[j:(j+4)])-1)
#   if (j == 41)
#     return.year5 = append(return.year5, prod(1+return.year[j:(j+2)])-1)
#   j = j+5
# }

ar.year = mean(return.year)
ar.year5 = mean(return.year5)/5

arithmetic = c(ar.day, ar.mon, ar.year, ar.year5)

geo.year = prod(1+return.year)^(1/n.year)-1
geo.year5 = prod(1+return.year5)^(1/n.year5/5)-1

geometric = c(geo.day, geo.mon, geo.year, geo.year5)

#mean rate of excess return
dataE = read.csv("my5y treasury monthly.csv")
dateE = as.Date(as.character(dataE$caldt), format = "%Y%m%d")
dataE.xts = xts(dataE, order.by = dateE)[,-1]

ar.ER.mon = mean(return.mon-dataE.xts$t30ret)*12

n.monE = length(dataE.xts$t30ret)
geo.ER.mon = prod(1+(return.mon-dataE.xts$t30ret))^(12/n.monE)-1

yearE.p = endpoints(dataE.xts$b1ret, on = "years", k = 1)
n.yearE = length(yearE.p)
TR.year = vector()

i = 1
while (i < n.yearE)
{
  TR.year = append(TR.year,prod(1+dataE.xts$b1ret[yearE.p[i]:yearE.p[i+1]])-1)
  i = i + 1
}

ar.ER.year = mean(return.year - TR.year)


yearE5.p = endpoints(dataE.xts$b5ret, on = "years", k = 5)
n.yearE5 = length(yearE5.p)
TR.year5 = vector()

j = 1
while (j < n.yearE5)
{
  TR.year5 = append(TR.year5, prod(1+day$vwretd[yearE5.p[j]:yearE5.p[j+1]])-1)
  j = j + 1
}

ar.ER.year5 = mean(return.year5-TR.year5)/5

arithmeticER = c(ar.ER.mon,ar.ER.year,ar.ER.year5)

#Final------------------------------------
#data-------------------------------------
mrf = read.csv("my5y treasury monthly.csv")
mon = read.csv("hw1 monthly.csv")
day = read.csv("hw1 daily.csv")
rf = read.csv("FederalFunds.csv")
date.rf = as.Date(as.character(rf$date), format = "%Y%m%d")
day.rf = xts(rf$FF_O, order.by = date.rf)
day.rf = na.locf(day.rf)
day.rf[1] = day.rf[2]
day.rf = day.rf/100/252
day.rf = cbind(rf$date,as.vector(day.rf))
colnames(day.rf) = colnames(rf)
day.rf = merge(day.rf,day,by.x = "date", by.y = "caldt")

#function---------------------------------
M2Y = function(data, period)
{
  spot = vector()
  n = length(data)
  x = 12*period
  cur = 1
  for (i in 1:n)
  {
    if ( i%%x == 1){
      spot = append(spot, cur-1)
      cur = 1
      print(paste("get",(i-1)/x,"period"))
    }
    cur = cur*(1+data[i])
  }
  spot = append(spot, cur-1)
  out = spot[-1]
  return(out)
}

#method-------------------------------------
ry = M2Y(mon$vwretd,1)
r5y = M2Y(mon$vwretd,5)

nd = length(day$vwretd)
rd.mean = mean(day$vwretd)*252
rd.geom = prod(1+day$vwretd)^(252/nd)-1

nm = length(mon$vwretd)
rm.mean = mean(mon$vwretd)*12
rm.geom = prod(1+mon$vwretd)^(12/nm)-1

ny = length(ry)
ry.mean = mean(ry)
ry.geom = prod(1+ry)^(1/ny)-1

n5y = length(r5y)
r5y.mean = mean(r5y)/5
r5y.geom = prod(1+r5y)^(1/n5y/5)-1

arith = c(rd.mean,rm.mean,ry.mean,r5y.mean)
geom = c(rd.geom,rm.geom,ry.geom,r5y.geom)

rfy = M2Y(mrf$b1ret,1)
rf5y = M2Y(mrf$b5ret,5)

ERd.mean = mean(day.rf$vwretd-day.rf$FF_O)*252
ERm.mean = mean(mon$vwretd-mrf$t30ret)*12
ERy.mean = mean(ry-rfy)
ER5y.mean = mean(r5y-rf5y)/5

nd.rf = length(day.rf$FF_O)

ERd.geom = prod(1+(day.rf$vwretd-day.rf$FF_O))^(252/nd.rf)-1
ERm.geom = prod(1+(mon$vwretd-mrf$t30ret))^(12/nm)-1
ERy.geom = prod(1+(ry-rfy))^(1/ny)-1
ER5y.geom = prod(1+(r5y-rf5y))^(1/n5y/5)-1

arith.ER = c(ERd.mean,ERm.mean,ERy.mean,ER5y.mean)
geom.ER = c(ERd.geom,ERm.geom,ERy.geom,ER5y.geom)