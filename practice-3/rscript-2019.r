###计算置信区间

###套用公式计算
x<-c(1.23, 1.24, 1.26, 1.29, 1.20, 1.32, 1.23, 1.23, 1.29, 1.28)
n<-length(x)
xbar<-mean(x)
sigma<-sqrt(0.042/n)  ####分母为样本平均数的标准误
z0.05<-abs(qnorm(0.025))
conf.lower<-xbar-z0.05*sigma
conf.upper<-xbar+z0.05*sigma


###使用z.test检验
install.packages("BSDA") ##只需要安装一次
library(BSDA)  ##每次打开R都需运行该命令
?z.test
z.test(x,sigma.x=sqrt(0.042))  ##sigma为样本标准差




###假设检验
###套用公式计算
x<-c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
n<-length(x)
xbar<-mean(x)
sigma<-sqrt(18/n)
mu<-75
z<-(xbar-mu)/sigma
z
2*pnorm(z) ###双尾检验


###使用z.test检验
z.test(x,mu=75,sigma.x=sqrt(18))


###方差未知时，计算置信区间
x<-c(1.23, 1.24, 1.26, 1.29, 1.20, 1.32, 1.23, 1.23, 1.29, 1.28)
n<-length(x)
xbar<-mean(x)
sigma<-sqrt(var(x)/n)  ####分母为样本平均数的标准误
t0.05<-abs(qt(0.025,n-1))
conf.lower<-xbar-t0.05*sigma
conf.upper<-xbar+t0.05*sigma

t.test(x)


##方差未知时，单个样本检验
x<-c(0.95,0.92,0.88,0.92,0.93,0.95,0.89,0.98,0.92)
t.test(x,mu=0.90,alternative="greater")
