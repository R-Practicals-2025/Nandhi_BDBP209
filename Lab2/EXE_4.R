vec = c(90, 5, 8, 9, 10, 45, 55)
class(vec)
vec1 = c(3, 7, 4, 10, "Aritri")
class(vec1)
max(vec)
min(vec)
vec2 = scan()
vec[4]
ind = c(2, 3, 4)
vec[ind]
vec[c(2, 3, 8)]
vec[length(vec)]
vec[-1]
vec_red = vec[-1]
length(vec_red)
vec[-length(vec)]
vec[c(-2, -3)]
vec[-2:-4]
trim = function(x) {
  vec_sort = sort(x)
  print(vec_sort)
  trimmed_x = vec_sort[3:(length(vec_sort) - 2)]
  print(trimmed_x)
}
trim(vec)
trim1 = function(x) sort(x)[c(-1, -2, -(length(x) - 1), -length(x))]
trim1(vec)
vec[seq(2, length(vec), 2)]
vec[1:length(vec) %% 2 == 0]
x = 1:10
x
x[x < 5]
sum(x[x < 5])
add = function(x) {
  s = sort(x)
  ss = x[c((length(x) - 2), length(x) - 1, length(x))]
  r = sum(ss)
  print(r)
}
add(vec)
which.max(x)
which.min(x)
cbind(1:10, 10:1)
rbind(1:10, 10:1)
X <- c(1:10)
X
Y <- c(1:10 * 5)
Y
X * Y
X + Y
X / Y
X ^ Y
log(X)
exp(Y)
