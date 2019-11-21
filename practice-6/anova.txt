setwd("D:\\2019-生物统计\\上机\\practice-6") 
### 1 分类变量是字母，默认是因子
wheat <- read.csv(file="example_9_1.csv")

####正态性检验
shapiro.test(wheat$height[wheat$treat=="A"])
shapiro.test(wheat$height[wheat$treat=="B"])
shapiro.test(wheat$height[wheat$treat=="C"])
shapiro.test(wheat$height[wheat$treat=="D"])
shapiro.test(wheat$height[wheat$treat=="E"])
###方差齐次性检验
bartlett.test(wheat$height~wheat$treat)

###方差分析
result<- aov(wheat$height~wheat$treat)
summary(result)
result$coefficients

###多重比较

install.packages("agricolae")
library(agricolae)
LSD.test(result,"wheat$treat", group=FALSE, p.adj= "bon",console=T)
LSD.test(result,"wheat$treat", group=TRUE, p.adj= "bon",console=T)
duncan.test(result,"wheat$treat",console=T)


 
 #### 分类变量是数字， R会默认是回归
 wheat <- read.csv(file="example_9_1_2.csv")
 result<- lm(wheat$height~wheat$treat)
 result
 anova(result)   ### 注意自由度，这相当于对回归系数做检验
 
  
 #####  分类变量是数字， as.factor 变成因子，可以不管是什么都用as.factor函数
 wheat <- read.csv(file="example_9_1_2.csv")
 wheat$treat<- as.factor(wheat$treat)
 result<- lm(wheat$height~wheat$treat)
 anova(result)
 


###
sub<-read.csv("9_4.csv")

###正态性检验
shapiro.test(sub$value[sub$class=="A"])
shapiro.test(sub$value[sub$class=="B"])
shapiro.test(sub$value[sub$class=="C"])
shapiro.test(sub$value[sub$class=="D"])
###方差同质检验
bartlett.test(sub$value~sub$class)

###数据转换
sub$cv<-sqrt(sub$value+1)

###正态及同质性检验
shapiro.test(sub$cv[sub$class=="A"])
shapiro.test(sub$cv[sub$class=="B"])
shapiro.test(sub$cv[sub$class=="C"])
shapiro.test(sub$cv[sub$class=="D"])
bartlett.test(sub$cv~sub$class)
###方差分析
res<-aov(sub$cv~sub$class)
summary(res)

###多重比较
LSD.test(res,"sub$class", group=FALSE, p.adj= "bon",console=T)
LSD.test(res,"sub$class", group=TRUE, p.adj= "bon",console=T)
duncan.test(res,"sub$class",console=T)