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






