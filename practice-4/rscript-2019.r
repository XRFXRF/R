###方差已知时
n.a<-31
n.b<-48
abar<-81.23
bbar<-70.23
var.a<-15.64^2
var.b<-12.07^2
zeta <- (abar-bbar) / (sqrt(var.a/n.a + var.b/n.b))  ##构建Z统计量
(1-pnorm(zeta))*2



###两个样本方差检验

la<-c(510,773,836,505,765,780,235,790,440,435,815,460,690)
lb<-c(650,600,600,575,452,320,660)
n1<-length(la)  ##数据个数
n2<-length(lb)
fvalue<-var(la)/var(lb) ##构建f值
(1-pf(fvalue,n1-1,n2-1))*2 ##双尾检验对应的pvalue 
qf(0.025, n1-1, n2-1)   ##左侧临界值
qf(0.975, n1-1, n2-1)   ##右侧临界值

var.test(la,lb)  ###使用var.test检测

###方差未知，成组检验默认方差不等


