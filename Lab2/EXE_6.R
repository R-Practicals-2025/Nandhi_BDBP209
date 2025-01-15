X = matrix(c(1:24), nrow = 3)
X
Y = (1:10 * 5)
Y
colMeans(X)
Z <- X[1:4] %o% Y[1:3]
Z
YoX <- Y[1:3] %o% X[1:4]
YoX
t(Z)
t(t(Z))
t(YoX)
X = matrix(c(1:24), nrow = 3)
Y = (1:8 * 5)
X %*% Y
sum(X * Y)
crossprod(X[1:4], Z)
diag(4)
class(X)
attributes(X)
X = matrix(c(1:24), nrow = 3)
Y = (1:8 * 5)
X[2, 3] = NA
X
X %*% Y
A = matrix(c(1:50), nrow = 5)
A
B = matrix(c(60:90), nrow = 4)
B

