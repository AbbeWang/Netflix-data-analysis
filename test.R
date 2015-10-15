visitor = read.csv("web_traffic_data_-_updated.csv",header=FALSE, nrows = 2914,skip = 27602)
stockprice = read.csv("table.csv")
return = diff(log(stockprice$Adj.Close))
revenue = read.csv("revenue.csv", nrows = 54)

library(ggplot2)
library(lubridate)
revenue$Period = dmy_hm(revenue$Period)
#qplot(revenue$Period, revenue$NFLX.Revenue..Quarterly., geom = c("point","smooth"))
qplot(revenue$Period, log(revenue$NFLX.Revenue..Quarterly.), geom = c("point","smooth"))



stockprice$Date = ymd(stockprice$Date)
qplot(stockprice$Date, stockprice$Adj.Close, geom = c("point","smooth"))
qplot(stockprice$Date,log(stockprice$Adj.Close), geom = c("point","smooth"))


visitor$V3 = gsub(x=visitor$V3,pattern="04:00:00+00:00",replacement="",fixed=T)
visitor$V3 = gsub(x=visitor$V3,pattern="05:00:00+00:00",replacement="",fixed=T)
visitor$V3 = ymd(visitor$V3)
#qplot(visitor$V3,visitor$V4, geom = c("point","smooth"))
qplot(visitor$V3, log(visitor$V4), geom = c("point","smooth"))



revenue1 = revenue[1:30,]
ml = lm(revenue1$NFLX.Revenue..Quarterly. ~ revenue1$Period)
plot(ml$residuals ~ revenue1$Period)
abline(h = 0, lty = 3)

qplot(revenue1$Period, revenue1$NFLX.Revenue..Quarterly., geom = c("point","smooth"))
qplot(revenue$Period, revenue$NFLX.Revenue..Quarterly., geom = c("point","smooth"))

qplot(visitor$V3,visitor$V4, geom = c("point","smooth"))
plot(ml$residuals ~ revenue1$Period)
abline(h = 0, lty = 3)



ml2 = lm(log(revenue1$NFLX.Revenue..Quarterly.) ~ revenue1$Period)
plot(ml2$residuals ~ revenue1$Period)
abline(h = 0, lty = 3)

summary(ml2)

plot(log(revenue1$NFLX.Revenue..Quarterly.) ~ revenue1$Period)
abline(ml2)

x = ymd("2015/09/30")
y = ml2$coefficients[1] + ml2$coefficients[2]*as.numeric(x)


#best
ml3 = lm(log(revenue$NFLX.Revenue..Quarterly.[1:10]) ~ revenue$Period[1:10])
plot(ml3$residuals ~ revenue$Period[1:10])
abline(h = 0, lty = 3)

x = ymd("2015/09/30")
y = ml3$coefficients[1] + ml3$coefficients[2]*as.numeric(x)


plot(log(revenue1$NFLX.Revenue..Quarterly.[1:10]) ~ revenue1$Period[1:10])
abline(ml3)


#merge
r = data.frame(revenue$Period, revenue$NFLX.Revenue..Quarterly.)
v = data.frame(visitor$V3, visitor$V4)
s = data.frame(stockprice$Date, stockprice$Adj.Close)


m = merge(r,v,by.x = "revenue.Period", by.y = "visitor.V3", all = TRUE)
m1 = merge(m,s,by.x = "revenue.Period", by.y = "stockprice.Date", all = TRUE)


ggplot(m1, aes(revenue.Period)) + 
  geom_line(aes(y=log(revenue.NFLX.Revenue..Quarterly.)), colour="red") + 
  geom_line(aes(y=log(visitor.V4)), colour="green") +
  geom_line(aes(y=stockprice.Adj.Close), colour = "yellow")




plot(m1$revenue.Period, m1$revenue.NFLX.Revenue..Quarterly.)
par(new = TRUE)
plot(m1$revenue.Period, m1$stockprice.Adj.Close, axes = FALSE, xlab = "", ylab = "", col = "red", type = "l")
par(new = TRUE)
plot(m1$revenue.Period, m1$visitor.V4, axes = FALSE, xlab = "", ylab = "", col = "green", type = "l")




m2 = m1[1467:4284,]
plot(m2$revenue.Period, m2$visitor.V4, axes = FALSE, xlab = "", ylab = "", col = "green", type = "l")
par(new = TRUE)
plot(m2$revenue.Period, m2$stockprice.Adj.Close, axes = FALSE, xlab = "", ylab = "", col = "red", type = "l")
par(new = TRUE)
plot(m2$revenue.Period, m2$revenue.NFLX.Revenue..Quarterly., col = "black")




