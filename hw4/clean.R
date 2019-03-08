# data = read.csv("fundamental data.csv")
# 
# data.noNA = data[complete.cases(data),]
# 
# MLR = (data.noNA$dlc+data.noNA$dltt)/data.noNA$at
# BLR = (data.noNA$dlc+data.noNA$dltt)/(data.noNA$dlc+data.noNA$dltt+abs(data.noNA$ceq))
# 
# hist(MLR)
# hist(BLR)
# 
# print(paste("MLR mean: ", mean(MLR)))
# print(paste("BLR mean: ", mean(BLR)))
# 
# summary(MLR)
# summary(BLR)
# 
# hist(data.noNA$ebitda)

data2 = read.csv("data3.csv")
data2 = data2[complete.cases(data2),]
data2.noNA = data2[which(data2$at != 0),]

MLR2 = (abs(data2.noNA$dlc)+abs(data2.noNA$dltt))/abs(data2.noNA$at)
BLR2 = (abs(data2.noNA$dlc)+abs(data2.noNA$dltt))/(abs(data2.noNA$dlc)+abs(data2.noNA$dltt)+abs(data2.noNA$ceq))

hist(MLR2,breaks = 5000, xlim = c(0,2), main = "Market Leverage Ratio")
hist(BLR2,breaks = 200, main = "Book Leverage Ratio")

print(paste("MLR mean: ", mean(MLR2)))
print(paste("BLR mean: ", mean(BLR2)))

summary(data2.noNA$oancf)
summary(data2.noNA$ivncf)
summary(data2.noNA$fincf)
summary(data2.noNA$ebitda)

boxplot(MLR2~data2.noNA$gsector, main = "Market Leverage Ratio")
boxplot(BLR2~data2.noNA$gsector, main = "Book Leverage Ratio")

data2.noNA$MR = MLR2
data2.noNA$BR = BLR2
library(dplyr)
dataIndM = data2.noNA %>% group_by(data2.noNA$gsector) %>% summarise(MR = mean(MR))
dataIndB = data2.noNA %>% group_by(data2.noNA$gsector) %>% summarise(BR = mean(BR))

plot(dataIndM$`data2.noNA$gsector`, dataIndM$MR, main = "Mean Market Leverage Ratio")
plot(dataIndB$`data2.noNA$gsector`, dataIndB$BR, main = "Mean Book Leverage Ratio")

ret = data2.noNA %>% group_by(data2.noNA$gsector) %>% summarise(ebitda = mean(ebitda))
plot(ret$`data2.noNA$gsector`, ret$ebitda, main = "Mean Earning by Industry")
