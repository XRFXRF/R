
####单个样本方差检验
n<-25
s2<-1600
sigma2<-900
chisq<-(n-1)*s2/sigma2
1-pchisq(chisq,(n-1))   ####备择假设是大于号，因此是右侧检验，应查看大于chisq的概率值。


### 适合性检验 R 语言没有自动校正，可以自己手动算
 # 2×2 列联表独立性检验可以自动校正 

chisq.test(x=c(124,36),p=c(3/4,1/4))

##手动计算
o<-c(124,36)
e<-c(120,40)
chisq<-sum((abs(o-e)-0.5)^2/e)
1-pchisq(chisq,1)

###使用置换检验
chisq.test(x=c(124,36),p=c(3/4,1/4),simulate.p.value = TRUE,B = 2000)



##2*2 独立性检验会自动校正

x <- matrix(c(2197, 583, 614, 7897),2,2)
chisq.test(x)

###对角线互换也是一样的
x <- matrix(c(2197, 614, 583, 7897),2,2)
chisq.test(x)


#### 注意 按列排
x <- matrix(c(192,319,194,3378,3297,3620),3,2)
chisq.test(x)






















