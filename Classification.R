#=====Exercise 11.1=====
rm(list = ls())

#-----(a)
x1 = matrix(c(3, 7, 2, 4, 4, 7), nr=3,byrow = T)
x1
x2 = matrix(c(6, 9, 5, 7, 4, 8), nr=3,byrow =T);x2
x1.bar = matrix(c(3, 6), nr = 2);x1.bar
x2.bar = matrix(c(5, 8), nr = 2);x2.bar
S.pooled = matrix(c(1, 1, 1, 2), nr=2,byrow = T)
S.pooled.inv = solve(S.pooled)
S.pooled.inv 
y.hat = t(x1.bar - x2.bar) %*% S.pooled.inv # * x
y.hat

#Answer:
#-2x1

#-----(b)
m.hat = (y.hat %*% x1.bar + y.hat %*% x2.bar) / 2
m.hat
x0 = matrix(c(2, 7), nr = 2)

# Assign x0' to py1 if
y0.hat = y.hat %*% x0 # >= m.hat
# and assign x0' to py2 otherwise.
y0.hat


#=====Exercise 11.2=====
rm(list = ls())

#-----(a)
p11 = read.csv(file.choose()) #11.1
p11
p = as.matrix(p11)
p1=p[,1:2]
p2=p[,3:4]
p2
x1.bar = as.matrix(colMeans(p1))
x1.bar
x2.bar = as.matrix(colMeans(p2))
x2.bar
S1 = cov(p1)
S1
S2 = cov(p2)
S2
S.pooled = (S1 + S2) /2
S.pooled

S.pooled.inv = solve(S.pooled)
S.pooled.inv
y.hat = t(x1.bar - x2.bar) %*% S.pooled.inv # * x
y.hat
# Where
m.hat = (y.hat %*% x1.bar + y.hat %*% x2.bar) / 2
m.hat

#-----(b)
Owners = data.frame(y0.hat = 0.1002303 * p1[,1] + 0.7851847 * p1[, 2])
Owners
NonOwners = data.frame(y0.hat = 0.1002303 * p2[,1] + 0.7851847 * p2[, 2])
NonOwners
# Assign an observation x to py1 if (0.100x1 +0.785x2) >= 24.72
Owners$Classification = ifelse(Owners >= 24.74567,'Owners', 'NonOwners')
NonOwners$Classsification = ifelse(NonOwners >= 24.74567,'Owners', 'NonOwners')
data.frame(Owners, NonOwners)




