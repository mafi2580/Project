#semester_Final_2019
#(a)----------
#install.packages("readxl")
a=read.csv(file.choose())
a
attach(a)
b=cbind(a)
b
c=b[, -1]
c
attach(c)
#(b)---------------
plot(x2,x3)
identify(c$x2,c$x3)
dotchart(x2,x3)
plot(x1,x3)
dotchart(x1,x3)
#(c)----------------
xbar=colMeans(c)
xbar
sn=var(c)
sn
r=cor(c)
r

#(d)----Q-Q_plot-------------
qqnorm(x1)
qqline(x1)
qqnorm(x2)
qqline(x2)
qqnorm(x3)
qqline(x3)
#(e)Mahalanobis_Distance------
#install.packages("mvnormalTest")
library(mvnormalTest)
w=cbind(x1,x2)
d2 = mahalanobis(w, colMeans(w), cov(w))
d2

#(f)50%_probability_contour
sortd2=sort(d2)
sortd2
q1<-qchisq(.5, 2)
q1
t<-NULL
for(i in 1:length(sortd2)){
  if (sortd2[i]<=q1){
    t[i]<-1
  } else {
    t[i]<-0
  }
}
mean(t)
t
prop.normality.test<-mean(t)
prop.normality.test
#or------------------
critical_value <- qchisq(0.5, df = 2)
critical_value
within_contour <-sortd2 <= critical_value
within_contour
sum(within_contour)
proportion_within_contour <- sum(within_contour) / length(within_contour)
proportion_within_contour

#g)--construct_chisquare--------
chi_square_values <- qchisq((1:length(sortd2))/(length(sortd2)+1), df = 2)
chi_square_values
plot(chi_square_values, sortd2, main = "Chi-Square Plot", xlab = "Chi-Square Values", ylab = "Ordered Distances")
abline(0, 1, col = "red")

#i)--x_bar_chart------------
xbar<-mean(x2)
xbar
varx<-var(x2)
stdx<-sqrt(varx)
LCL=xbar-(3*stdx)
UCL=xbar+(3*stdx)
CL=xbar
CL
LCL
UCL
plot(x2,ylim=c(-800,900))
abline(h=c(CL,LCL,UCL))
identify(x2)
d=c[-5 , ]
d
cor(d)
#-------
xbar<-mean(x1)
varx<-var(x1)
stdx<-sqrt(varx)
LCL=xbar-(3*stdx)
UCL=xbar+(3*stdx)
CL=xbar
CL
LCL
UCL
plot(x1,ylim=c(-140,450))
abline(h=c(CL,LCL,UCL))



###j)Evaluate the observed tsquare
cvar=cov(c);cvar
Mu0<-c(165.2,12.3,678.7)
Mu0
Sinver=solve(cvar)
Sinver

N=10
p=3
xbar
a=t(xbar-Mu0)
a
b=(xbar-Mu0)
T2=N*a %*% Sinver %*% b
T2
#now the critical value
alpha<-0.05
F0<- qf(1-alpha, df1=p, df2=N-p)
F0
T21<-(((N-1)*p*F0)/(N-p))[1][1];T21

#Marginal_dot_plot--------------

install.packages("ggExtra")
library(ggExtra)
library(ggplot2)
g=ggplot(c, aes(x1, x2))+geom_count() + geom_smooth(method="lm", se=F)
g
ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")


#2020----------(2)
# (a)
 mu <- c(7,11)
 n <- 4
 p <- 2
 X <- matrix(c(2,8,6,8,12,9,9,10),nrow=n,ncol=p)
 X
 xbar <- colMeans(X)
 S <- cov(X)
 Sinv <- solve(S)
 T2 <- n*t(xbar-mu) %*% Sinv %*% (xbar-mu)
 T2
  # (c)
 alpha <- 0.05
 qf(1-alpha,df1=2,df2=2)


 library(DescTools)
 HotellingsT2Test(c, mu=Mu0)

#semester_final_2021
m= read.csv(file.choose())
attach(m)

#a-------scatter_plot
plot(m)
cor(m)
#b----inter_the_sign
scatter.smooth(m)
#c----
x1bar=mean(y1)
x1bar
x2bar=mean(y2)
x2bar
s11=cov(y1,y1)
s11
s22=cov(y2,y2)
s22
s12=cov(y1,y2)
s12
r12=cor(y1,y2)

r12
#d------sample_mean_array
colMeans(m)
#variance_covariance_array
cov(m)
#correlation_arry
cor(m)

#e-----------
cvar=cov(m);cvar
Mu0<-c(550,3)
Mu0
Sinver=solve(cvar)
Sinver

N=15
p=2
xbar=colMeans(m)
a=t(xbar-Mu0)
a
b=(xbar-Mu0)
T2=N*a %*% Sinver %*% b
T2
#now the critical value
alpha<-0.05
F0<- qf(1-alpha, df1=p, df2=N-p)
F0
T21<-(((N-1)*p*F0)/(N-p))[1][1];T21

