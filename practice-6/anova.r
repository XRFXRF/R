setwd("D:\\2019-����ͳ��\\�ϻ�\\practice-6") 
### 1 �����������ĸ��Ĭ��������
wheat <- read.csv(file="example_9_1.csv")

####��̬�Լ���
shapiro.test(wheat$height[wheat$treat=="A"])
shapiro.test(wheat$height[wheat$treat=="B"])
shapiro.test(wheat$height[wheat$treat=="C"])
shapiro.test(wheat$height[wheat$treat=="D"])
shapiro.test(wheat$height[wheat$treat=="E"])
###��������Լ���
bartlett.test(wheat$height~wheat$treat)

###�������
result<- aov(wheat$height~wheat$treat)
summary(result)
result$coefficients

###���رȽ�

install.packages("agricolae")
library(agricolae)
LSD.test(result,"wheat$treat", group=FALSE, p.adj= "bon",console=T)
LSD.test(result,"wheat$treat", group=TRUE, p.adj= "bon",console=T)
duncan.test(result,"wheat$treat",console=T)


 
 #### ������������֣� R��Ĭ���ǻع�
 wheat <- read.csv(file="example_9_1_2.csv")
 result<- lm(wheat$height~wheat$treat)
 result
 anova(result)   ### ע�����ɶȣ����൱�ڶԻع�ϵ��������
 
  
 #####  ������������֣� as.factor ������ӣ����Բ�����ʲô����as.factor����
 wheat <- read.csv(file="example_9_1_2.csv")
 wheat$treat<- as.factor(wheat$treat)
 result<- lm(wheat$height~wheat$treat)
 anova(result)
 


###
sub<-read.csv("9_4.csv")

###��̬�Լ���
shapiro.test(sub$value[sub$class=="A"])
shapiro.test(sub$value[sub$class=="B"])
shapiro.test(sub$value[sub$class=="C"])
shapiro.test(sub$value[sub$class=="D"])
###����ͬ�ʼ���
bartlett.test(sub$value~sub$class)

###����ת��
sub$cv<-sqrt(sub$value+1)

###��̬��ͬ���Լ���
shapiro.test(sub$cv[sub$class=="A"])
shapiro.test(sub$cv[sub$class=="B"])
shapiro.test(sub$cv[sub$class=="C"])
shapiro.test(sub$cv[sub$class=="D"])
bartlett.test(sub$cv~sub$class)
###�������
res<-aov(sub$cv~sub$class)
summary(res)

###���رȽ�
LSD.test(res,"sub$class", group=FALSE, p.adj= "bon",console=T)
LSD.test(res,"sub$class", group=TRUE, p.adj= "bon",console=T)
duncan.test(res,"sub$class",console=T)