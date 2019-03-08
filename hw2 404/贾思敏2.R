library(readxl)
library(xts)
library(data.table)
RM1<-read_excel("~/Desktop/Return Matrix.xlsx")

RM1$`Names Date`<- as.Date(as.character(RM1$`Names Date`), "%Y%m%d")
RM1$Excess_Return<-RM1$`Returns without Dividends`-RM1$`Return on the S&P 500 Index`
RM1$Excess_Return[is.na(RM1$Excess_Return)] <-0
p<-which(!is.na(RM1$`Payment Date`))
rexcess <- matrix(0, nrow = 41)
mean<-c()

for (i in p){
    if (i>20){
      if (RM1$`Names Date`[i+20]>RM1$`Names Date`[i] && RM1$`Names Date`[i-20]<RM1$`Names Date`[i]){
        rexcess<-cbind(rexcess,RM1$Excess_Return[(i-20):(i+20)])
      }
    }
}


for (i in 1:41){
  mean[i]<-mean(rexcess[i,])
}
plot(mean,type = "l",xlab="Time Period",ylab="Excess Return",mean="Event Study")
abline(v=21,col="red")

