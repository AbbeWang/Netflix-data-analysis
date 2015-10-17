# getting amazon visitor data
avisitor = read.csv("web_traffic_data_-_updated.csv",header=FALSE, nrows = 45394,skip = 77197)
avisitor = avisitor[c(1:21863, 45299:45394),]
avisitor$V3 = as.character(avisitor$V3)
avisitor$V3 = gsub(x=avisitor$V3,pattern=" 04:00:00+00:00",replacement="",fixed=T)
avisitor$V3 = gsub(x=avisitor$V3,pattern="05:00:00+00:00",replacement="",fixed=T)
avisitor$V3 = ymd(avisitor$V3)


#getting amazon revenue data
arevenue = read.csv("revenue.csv")
arevenue$Period = dmy_hm(arevenue$Period)

#plot revenue data
qplot(arevenue$Period, arevenue$AMZN.Revenue..Quarterly., geom = c("point","smooth"))
qplot(arevenue$Period, log(arevenue$AMZN.Revenue..Quarterly.), geom = c("point","smooth"))

#subtract 2010-2015 revenue
arevenue1 = arevenue[1:22,]
qplot(arevenue1$Period, arevenue1$AMZN.Revenue..Quarterly., geom = c("point","smooth"),method = "lm")
qplot(arevenue1$Period, log(arevenue1$AMZN.Revenue..Quarterly.), geom = c("point","smooth"), method = "lm")


#regression line
m1 = lm(arevenue1$AMZN.Revenue..Quarterly. ~ arevenue1$Period)
summary(m1)

plot(m1$residuals ~ arevenue1$Period)
abline(h = 0, lty = 3)

plot(arevenue1$AMZN.Revenue..Quarterly. ~ arevenue1$Period)
abline(m1)


# Q3 revenue data
q3_revenue = arevenue[month(arevenue[,1]) == 9,c(1,2)]
qplot(q3_revenue$Period, q3_revenue$AMZN.Revenue..Quarterly., geom = c("point","smooth"))
qplot(q3_revenue$Period, log(q3_revenue$AMZN.Revenue..Quarterly.), geom = c("point","smooth"))

# 2005-2014
qplot(q3_revenue$Period[1:10], q3_revenue$AMZN.Revenue..Quarterly.[1:10], geom = c("point","smooth"),method = "lm")
qplot(q3_revenue$Period[1:10], log(q3_revenue$AMZN.Revenue..Quarterly.[1:10]), geom = c("point","smooth"),method = "lm")


## no log 
m2 = lm(q3_revenue$AMZN.Revenue..Quarterly.[1:10] ~ q3_revenue$Period[1:10])
summary(m2)

plot(m2$residuals ~ q3_revenue$Period[1:10])
abline(h = 0, lty = 3)

plot(q3_revenue$AMZN.Revenue..Quarterly.[1:10] ~ q3_revenue$Period[1:10])
abline(m2)

## log -- better
m3 = lm(log(q3_revenue$AMZN.Revenue..Quarterly.[1:10]) ~ q3_revenue$Period[1:10])
summary(m3)

plot(m3$residuals ~ q3_revenue$Period[1:10])
abline(h = 0, lty = 3)

plot(log(q3_revenue$AMZN.Revenue..Quarterly.[1:10]) ~ q3_revenue$Period[1:10])
abline(m3)



# 2010-2014
qplot(q3_revenue$Period[1:5], q3_revenue$AMZN.Revenue..Quarterly.[1:5], geom = c("point","smooth"),method = "lm")
qplot(q3_revenue$Period[1:5], log(q3_revenue$AMZN.Revenue..Quarterly.[1:5]), geom = c("point","smooth"),method = "lm")

## no log -- better -- best
m2 = lm(q3_revenue$AMZN.Revenue..Quarterly.[1:5] ~ q3_revenue$Period[1:5])
summary(m2)

plot(m2$residuals ~ q3_revenue$Period[1:5])
abline(h = 0, lty = 3)

plot(q3_revenue$AMZN.Revenue..Quarterly.[1:5] ~ q3_revenue$Period[1:5])
abline(m2)

## log
m3 = lm(log(q3_revenue$AMZN.Revenue..Quarterly.[1:5]) ~ q3_revenue$Period[1:5])
summary(m3)

plot(m3$residuals ~ q3_revenue$Period[1:5])
abline(h = 0, lty = 3)

plot(log(q3_revenue$AMZN.Revenue..Quarterly.[1:5]) ~ q3_revenue$Period[1:5])
abline(m3)


# calculating q3 2015
x = ymd("2015-09-30")
y = m2$coefficients[1] + m2$coefficients[2]*as.numeric(x)


# q3 ~ q1 + q2
q3_revenue$q1 = arevenue$AMZN.Revenue..Quarterly.[month(arevenue[,1]) == 3 & year(arevenue[,1]) < 2015]
q3_revenue$q2 = arevenue$AMZN.Revenue..Quarterly.[month(arevenue[,1]) == 6 & year(arevenue[,1]) < 2015]


# q1 & q2 has high dependency, therefore only examine q2
cor(q3_revenue$q1, q3_revenue$q2)

m4 = lm(q3_revenue$AMZN.Revenue..Quarterly.[1:5] ~ q3_revenue$q2[1:5])
summary(m4)

plot(m4$residuals ~ q3_revenue$q2[1:5])
abline(h = 0, lty = 3)

plot(q3_revenue$AMZN.Revenue..Quarterly.[1:5] ~ q3_revenue$q2[1:5])
abline(m4)


# recalculating q3 2015
x = arevenue1[1,2]
y = m4$coefficients[1] + m4$coefficients[2]*x



# traffic * weather
qplot(avisitor$V3,avisitor$V4, geom = c("point","smooth"))
qplot(avisitor$V3, log(avisitor$V4), geom = c("point","smooth"))

avisitor$V3 = as.factor(avisitor$V3)
df1 = tapply(avisitor$V4, avisitor$V3, sum)
n = names(df1)
df = data.frame(as.POSIXct(n), df1, row.names = NULL)

qplot(df$as.POSIXct.n.,df$df1, geom = c("point","smooth"))
qplot(df$as.POSIXct.n.,log(df$df1), geom = c("point","smooth"))


#plot together
plot(arevenue$Period, arevenue$AMZN.Revenue..Quarterly., axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(df$as.POSIXct.n.,df$df1, xlab = "", ylab = "", col = "red", type = "l")




