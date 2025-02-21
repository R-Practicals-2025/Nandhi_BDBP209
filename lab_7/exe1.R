# Creating the first matrix with byrow = TRUE
new_matrix1 <- matrix(seq(15, 150, by=15), nrow=3, ncol=5, byrow=TRUE)
print(new_matrix1)

# Creating the second matrix with byrow = FALSE
new_matrix2 <- matrix(seq(15, 150, by=15), nrow=3, ncol=5, byrow=FALSE)
print(new_matrix2)
# The elements in new_matrix1 are filled row by row, while in new_matrix2, elements are filled column by column.

# Assigning row names and column names to the matrix 'new_matrix1'
rownames(new_matrix1) <- c("Row1", "Row2", "Row3")
colnames(new_matrix1) <- c("Column1", "Column2", "Column3", "Column4", "Column5")
print(new_matrix1)

# Creating a new matrix A with modified elements
MatrixA <- matrix(c(1, 8, 3, 2, 5, 6, 7, 9, 4, 3, 12, 13, 15, 17, 10, 11), nrow=4, ncol=4)
# Creating a new matrix B with different elements
MatrixB <- matrix(c(14, 6, 5, 3, 2, 18, 8, 7, 11, 4, 9, 6, 16, 13, 10, 5), nrow=4, ncol=4)
print(MatrixA)
print(MatrixB)
# Element-wise multiplication of matrices A and B
multiplication_result <- MatrixA * MatrixB
print(multiplication_result)

# Define two new vectors, X and Y
X_new <- c(10, 5, 2, 7)
Y_new <- c(6, 9, 3, 8)
# Outer product of X and Y
outer_prod <- outer(X_new, Y_new)
# Dot product (inner product) of X and Y
dot_product <- sum(X_new * Y_new)
print(outer_prod)
print(dot_product)

# Create a diagonal matrix using the vector X_new
diag_matrix <- diag(X_new)
print(diag_matrix)
# Print the diagonal elements of the matrix
print(diag(diag_matrix))

# Create a 6x6 identity matrix in one line
identity_matrix6x6 <- diag(6)
print(identity_matrix6x6)

# Define a new 3x3 matrix A
MatrixA_3x3 <- matrix(c(5, 3, -4, 2, -6, 1, 9, -2, 6), nrow=3, ncol=3)
print(MatrixA_3x3)
# Define a 3x1 matrix B
MatrixB_3x1 <- matrix(c(2, -5, 8), nrow=3, ncol=1)
print(MatrixB_3x1)

# Solve the linear system A * X = B to find X
solutionX <- solve(MatrixA_3x3, MatrixB_3x1)
print(solutionX)
print(class(solutionX))  # X is a matrix
print(typeof(solutionX))

# Find the inverse of MatrixA_3x3
inverse_matrixA <- solve(MatrixA_3x3)
print(inverse_matrixA)
# Check if multiplying the inverse of A by A gives the identity matrix
identity_check <- inverse_matrixA %*% MatrixA_3x3
print(identity_check)  # It should give an identity matrix

# Find the eigenvalues and eigenvectors of MatrixA_3x3
eigen_results <- eigen(MatrixA_3x3)
# Print the eigenvalues and eigenvectors
print(eigen_results$values)
print(eigen_results$vectors)

# Perform matrix-vector multiplication of MatrixA_3x3 and the second eigenvector
second_eigenvector_new <- eigen_results$vectors[, 2]  # Second eigenvector
result_matrix_vec <- MatrixA_3x3 %*% second_eigenvector_new
print(result_matrix_vec)

# Check if the result is approximately equal to the second eigenvalue times the second eigenvector
eigenvalue_times_eigenvector <- eigen_results$values[2] * second_eigenvector_new
print(eigenvalue_times_eigenvector)
# The result of MatrixA_3x3 multiplied by the second eigenvector should be approximately the second eigenvalue times the eigenvector
