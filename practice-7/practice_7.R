setwd("C:\\sjtu\\courses\\生物统计学\\2017\\2017统计\\上机实验课\\practice-8\\practice-8")

###example_10_1

### 二因交叉分组无重复


piglet <- read.csv(file="example_10_1.csv")
tapply(piglet$weight,piglet$A,shapiro.test)
tapply(piglet$weight,piglet$B,shapiro.test)
bartlett.test(piglet$weight~piglet$A)
bartlett.test(piglet$weight~piglet$B)
result<- lm(piglet$weight~piglet$A+piglet$B)
result
anova(result)
 
  
fit<-aov(piglet$weight~piglet$A+piglet$B)  
summary(fit)
library(agricolae)
duncan.test(fit,"piglet$A",console=T)
duncan.test(fit,"piglet$B",console=T)

###example_10_2
###二因交叉分组等重复
plant <- read.csv(file="example_10_2.csv")
a<- plant$A
b<- plant$B
yield<- plant$yield
tapply(yield,a,shapiro.test)
tapply(yield,b,shapiro.test)
bartlett.test(yield~a)
bartlett.test(yield~b)

result<- lm(yield~a+b+a*b)
result
anova(result)
duncan.test(result,a:b,group=F,console=T)
 
 #####example_10_3
 ###嵌套分组
 
milk <- read.csv(file="example_10_3.csv")
a<- milk$cattle
b<- milk$cow

yield<- milk$yield
result<- lm(yield~a+a*b)
tapply(yield,a,shapiro.test)
bartlett.test(yield~a)
#result
anova(result)
duncan.test(result,"a",console=T)
duncan.test(result,"b",console=T)


 
###example_10_5 
###正反交试验
press <- read.csv(file="example_10_5.csv")
a<- as.factor(press$time)
b<- as.factor(press$patient)
treat<- as.factor(press$treat)
pressure<- press$pressure
result<- lm(pressure~a+b+treat)
#result
anova(result)
 
###example_10_7 
###正交设计，R使用模型

weight <- read.csv(file="example_10_7.csv")
a<- as.factor(weight$A)
b<- as.factor(weight$B)
c<- as.factor(weight$C)
y<- weight$y
result<- lm(y~a+b+c+a*b+a*c)

anova(result) 
 

 
 
 

