# Creating a 3-row matrix with values from 1 to 24
X = matrix(c(1:24), nrow = 3)  
X  # Displaying the matrix

# Creating a vector of multiples of 5 from 5 to 50
Y = (1:10 * 5)  
Y  # Displaying the vector

# Calculating the column-wise means of X
colMeans(X)  

# Outer product of the first 4 elements of X and the first 3 elements of Y
Z <- X[1:4] %o% Y[1:3]  
Z  # Displaying the outer product matrix

# Outer product of the first 3 elements of Y and the first 4 elements of X
YoX <- Y[1:3] %o% X[1:4]  
YoX  # Displaying the outer product matrix

# Transposing the matrix Z
t(Z)  

# Double transposition (returns original matrix)
t(t(Z))  

# Transposing the matrix YoX
t(YoX)  

# Creating a 3-row matrix with values from 1 to 24
X = matrix(c(1:24), nrow = 3)  

# Creating a vector of multiples of 5 from 5 to 40
Y = (1:8 * 5)  

# Matrix multiplication of X and Y
X %*% Y  

# Element-wise multiplication of X and Y, then summing all elements
sum(X * Y)  

# Computing the cross product of the first 4 elements of X and matrix Z
crossprod(X[1:4], Z)  

# Creating a 4x4 identity matrix
diag(4)  

# Checking the class type of matrix X
class(X)  

# Displaying the attributes of matrix X
attributes(X)  

# Reassigning X and Y
X = matrix(c(1:24), nrow = 3)  
Y = (1:8 * 5)  

# Assigning NA to the element at row 2, column 3 of X
X[2, 3] = NA  
X  # Displaying the modified matrix

# Attempting matrix multiplication with NA values (will result in NA output)
X %*% Y  

# Creating a 5-row matrix A with values from 1 to 50
A = matrix(c(1:50), nrow = 5)  
A  # Displaying matrix A

# Creating a 4-row matrix B with values from 60 to 90
B = matrix(c(60:90), nrow = 4)  
B  # Displaying matrix B
