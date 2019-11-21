data=read.csv('D:\\R\\A.csv')
tapply(data$weight,data$A,shapiro.test)
tapply(data$weight,data$B,shapiro.test)
bartlett.test(data$weight~data$A)
bartlett.test(data$weight~data$B)
n=lm(data$weight~data$A+data$B)
n
anova(n)
