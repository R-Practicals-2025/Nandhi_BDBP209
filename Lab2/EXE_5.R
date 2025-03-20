# Creating a sequence from 1 to 24
y = 1:24  

# Reshaping y into a 3D array with dimensions 2x4x3
dim(y) = c(2, 4, 3)  
y  # Displaying the array

# Creating a 3x3 identity-like matrix
X <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)  
X  # Displaying the matrix

# Defining a vector
vector <- c(1, 2, 3, 4, 4, 3, 2, 1)  

# Creating a 2-row matrix by filling rows first
V <- matrix(vector, byrow = TRUE, nrow = 2)  
V  # Displaying the matrix

# Re-defining the same vector
vector <- c(1, 2, 3, 4, 4, 3, 2, 1)  

# Creating a 2-row matrix by filling columns first
V <- matrix(vector, byrow = FALSE, nrow = 2)  
V  # Displaying the matrix

# Converting the vector into a 4x2 matrix
dim(vector) <- c(4, 2)  
vector  # Displaying the reshaped vector

# Checking if vector is now a matrix
is.matrix(vector)  

# Creating a mixed-type vector (converts everything to character)
x = c(1, 2, 3, 4, "AB", "R")  

# Reshaping x into a 3D array (3x2x1)
dim(x) = c(3, 2, 1)  
x  # Displaying the reshaped array

# Creating a sequence from 1 to 64
vec = 1:64  

# Reshaping vec into a 4D array with dimensions 2x2x4x4
dim(vec) = c(2, 2, 4, 4)  
vec  # Displaying the 4D array

# Accessing a specific element from the 4D array (1st row, 1st col, 2nd matrix, 4th layer)
vec[1, 1, 2, 4]  

# Extracting an entire slice of the array (all rows & cols from the first 2D slice of the first 3D block)
vec[,,1,1]  
