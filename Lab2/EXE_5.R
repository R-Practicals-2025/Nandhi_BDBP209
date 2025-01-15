y = 1:24
dim(y) = c(2, 4, 3)
y
X <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)
X
vector <- c(1, 2, 3, 4, 4, 3, 2, 1)
V <- matrix(vector, byrow = T, nrow = 2)
V
vector <- c(1, 2, 3, 4, 4, 3, 2, 1)
V <- matrix(vector, byrow = F, nrow = 2)
V
dim(vector) <- c(4, 2)
vector
is.matrix(vector)
x = c(1, 2, 3, 4, "AB", "R")
dim(x) = c(3, 2, 1)
x
vec = 1:64
dim(vec) = c(2, 2, 4, 4)
vec
vec[1, 1, 2, 4]
vec[,,1,1]

