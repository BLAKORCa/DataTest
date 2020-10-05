# Removes anything that is currently stored in the R environment
rm(list = ls()) 

# Load in a dataset, i.e. x and y values
load(file.choose())

x1<-data$X5#Total Population
x2<-data$X9# Number of Hospital beds
x3<-data$X16#Total personal income
y <- data$X8#Number of active physicians 

n <- length(y)
linmod1 <- lm(y~x1)
linmod2 <- lm(y~x2)
linmod3 <- lm(y~x3)
res1<-linmod1$residuals
res2<-linmod2$residuals
res3<-linmod3$residuals

fit1<-rstandard(linmod1)
fit2<-rstandard(linmod2)
fit3<-rstandard(linmod3)

#x1 plot
plot(x=x1,y=res1,pch=16,xlab='Total Population',ylab='Number of active physicians',main='Residual against X graph')
qqnorm(fit1,ylab='standardized residuals',pch=16)

#x2 plot
plot(x=x2,y=res2,pch=16,xlab='Number of Hospital beds',ylab='Number of active physicians',main='Residual against X graph')
qqnorm(fit2,ylab='standardized residuals',pch=16)

#x3 plot
plot(x=x3,y=res3,pch=16,xlab='Total personal income',ylab='Number of active physicians',main='Residual against X graph')
qqnorm(fit3,ylab='standardized residuals',pch=16)



b1<--6.416

b1 <- linmod$coefficients["x"]
b0 <- linmod$coefficients["(Intercept)"]

x.new = sqrt(x)
y.new = log10(y) 
linmod <- lm(y~x.new)
linmod
res<-y-linmod$fitted.values

plot(x=x.new,y=y,pch=16,xlab='transformed X',ylab='Y')

abline( 1.396,3.654 )
plot(res,pch=16)

plot(y=res,x=linmod$fitted.values,pch=16,xlab='y.hat',ylab='residual')
fit<-rstandard(linmod)
qqnorm(fit,ylab='standardized residuals',pch=16,xlab='X')
qqline(fit,lwd=2)

plot(y=res,x=x,pch=16,xlab='Xi',ylab='residual')
boxplot(x,pch=16,ylab='ACT scores')

R2<-function(xdata,ydata){
  linmod <- lm(ydata~xdata)
  b1 <- linmod$coefficients["xdata"]
  b0 <- linmod$coefficients["(Intercept)"]
  SSR<-sum((b0+b1*xdata-mean(ydata))^2)
  SSTO<- sum((ydata-mean(ydata))^2)
  rr<-SSR/SSTO
  rr
}

plot(x, y)
abline(a=b0,b=b1)

anova(linmod)


s <- summary(linmod)$sigma



alpha = 0.1
Ftest =2*qf(1-alpha,2,n-2)
sqrt(Ftest)
Fstar
SSR<-sum((b0+b1*x-mean(y))^2)
E_MS<-s^2+b1^2 *sum((x-mean(x))^2)
SSE<-sum((y-(b1*x+b0))^2)


n<-241
s.b1<-0.510


summary(linmod)
s.b1 <- sqrt(s^2/(sum((x - mean(x))^2)))
s.b0 <- sqrt(s^2*(1/n + mean(x)^2/sum((x - mean(x))^2)))

summary(linmod)

alpha <- 0.01 # 99% interval

# Get our t-quantiles
qt.05 <- qt(alpha/2, df = n )
qt.95 <- qt(1 - alpha/2, df = n - 2)
b0<-0.379
s.b1<-3.430
# Here's our 99% CI for b1!
c(b0 - qt.95*s.b1, b0 - qt.05*s.b1)
# Note - because the t distribution is symmetric about 0, we can pick either
# qt.975 or qt.025 and save some work.
b1 + c(-1, 1)*qt.975*s.b1
b0 + c(-1, 1)*qt.975*s.b0

b1=1.6

tstar= b1/s.b1
tstar
abs(qt(0.01/2,119))
pt(tstar,n-2)
b0



X_28=b1*28+b0
alpha <- 0.05 
s <- summary(linmod)$sigma
snew <- sqrt(s^2*(1/n + (28-mean(x))^2/sum((x - mean(x))^2)))
t1=abs(qt(alpha/2,n-2))
c(X_28-t1*snew,X_28+t1*snew)



sn<-sqrt(s^2*(1+1/n + (60-mean(x))^2/sum((x - mean(x))^2)))

c(X_60-t1*sn,X_60+t1*sn)


mse=sum((y-(b1*x+b0))^2)/(n-2)
a1=sqrt(mse/sum((x-mean(x))^2))
s^2

c((X_6-t1*snew)/6,(X_6+t1*snew)/6)








#3.29

x<-data$X
y<-data$Y
n<-length(y)
linmod <- lm(y~x)
band1=c()
band2=c()
band3=c()
band4=c()
band5=c()
band6=c()


for(i in 1:n){
  
  if (x[i] >=0.5 && x[i]<=2.5){
    band1 = append(band1,y[i]) 
  }
  else if (x[i] >=2.5 && x[i]<=4.5){
    band2 = append(band2,y[i]) 
  }
  else if (x[i] >=4.5 && x[i]<=6.5){
    band3 = append(band3,y[i])  
  }
  else if (x[i] >=6.5 && x[i]<=8.5){
    band4 =append(band4,y[i]) 
  }
}
x1<-seq(0.5,2.5,length.out = length(band1))
x2<-seq(2.5,4.5,length.out = length(band2))
x3<-seq(4.5,6.5,length.out = length(band3))
x4<-seq(6.5,8.5,length.out = length(band4))
band11<-cbind(median(x1),median(band1))
band22<-cbind(median(x2),median(band2))
band33<-cbind(median(x3),median(band3))
band44<-cbind(median(x4),median(band4))
plot(x=x,y=y,pch=16)
bands <- rbind(band11,band22,band33,band44)
lines(bands)

for(i in 1:n){
  
  if (x[i] >=0.5 && x[i]<=3.5){
    band1 = append(band1,y[i]) 
  }
  else if (x[i] >=1.5 && x[i]<=4.5){
    band2 = append(band2,y[i]) 
  }
  else if (x[i] >=2.5 && x[i]<=5.5){
    band3 = append(band3,y[i])  
  }
  else if (x[i] >=3.5 && x[i]<=6.5){
    band4 =append(band4,y[i]) 
  }
  else if (x[i] >=4.5 && x[i]<=7.5){
    band5 =append(band4,y[i]) 
  }
  else if (x[i] >=5.5 && x[i]<=8.5){
    band6 =append(band4,y[i]) 
  }
  else {
    
  }
  }

  x1<-seq(0.5,3.5,length.out = length(band1))
  x2<-seq(1.5,4.5,length.out = length(band2))
  x3<-seq(2.5,5.5,length.out = length(band3))
  x4<-seq(3.5,6.5,length.out = length(band4))
  x5<-seq(4.5,7.5,length.out = length(band5))
  x6<-seq(5.5,8.5,length.out = length(band6))
  





x.median1=median(x1)
x.median2=median(x2)
x.median3=median(x3)
x.median4=median(x4)
x.median5=median(x5)
x.median6=median(x6)

lm1<- lm(band1~x1)
lm2<- lm(band2~x2)
lm3<- lm(band3~x3)
lm4<- lm(band4~x4)
lm5<- lm(band5~x5)
lm6<- lm(band6~x6)

y1.hat<- lm1$coefficients["x1"]*x.median1 +lm1$coefficients["(Intercept)"]
y2.hat<- lm2$coefficients["x2"]*x.median2 +lm2$coefficients["(Intercept)"]
y3.hat<- lm3$coefficients["x3"]*x.median3 +lm3$coefficients["(Intercept)"]
y4.hat<- lm4$coefficients["x4"]*x.median4 +lm4$coefficients["(Intercept)"]
y5.hat<- lm5$coefficients["x5"]*x.median5 +lm5$coefficients["(Intercept)"]
y6.hat<- lm6$coefficients["x6"]*x.median6 +lm6$coefficients["(Intercept)"]

c(y1.hat,y2.hat,y3.hat,y4.hat,y5.hat,y6.hat)


band1.meadian=median(band1)
band2.meadian=median(band2)
band3.meadian=median(band3)
band4.meadian=median(band4)



band11<-cbind(median(x1),y1.hat)
band22<-cbind(median(x2),y2.hat)
band33<-cbind(median(x3),y3.hat)
band44<-cbind(median(x4),y4.hat)
band55<-cbind(median(x5),y5.hat)
band66<-cbind(median(x6),y6.hat)

band11<-cbind(median(x1),median(band1))
band22<-cbind(median(x2),median(band2))
band33<-cbind(median(x3),median(band3))
band44<-cbind(median(x4),median(band4))
band55<-cbind(median(x5),median(band5))
band66<-cbind(median(x6),median(band6))

bands<-rbind(band11,band22,band33,band44,band55,band66)

xdata<-c(x.median1,x.median2,x.median3,x.median4,x.median5,x.median6)
ydata<-c(y1.hat,y2.hat,y3.hat,y4.hat,y5.hat,y6.hat)

plot(x=x,y=y,pch=16)
lines(bands)
abline(15.52,13.77)

xdata
ydata











    