#semester_Final_2019
#(a)----------
install.packages("readxl")
a=read.csv(file.choose())
attach(a)
b=cbind(a)
c=b[, -1]
c
attach(c)
#(b)---------------
plot(x2,x3)
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











