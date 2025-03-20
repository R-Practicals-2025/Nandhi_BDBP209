# Creating a numeric vector
vec = c(90, 5, 8, 9, 10, 45, 55)

# Checking the class of vec
class(vec)  

# Creating a character vector (mixed types convert everything to character)
vec1 = c(3, 7, 4, 10, "Aritri")  
class(vec1)  

# Finding the maximum and minimum values of vec
max(vec)  
min(vec)  

# Reading values from user input
vec2 = scan()  

# Accessing elements from the vector
vec[4]        # Accessing the 4th element
ind = c(2, 3, 4)  
vec[ind]      # Accessing elements at positions 2, 3, and 4
vec[c(2, 3, 8)]  # Trying to access an out-of-bound index (8)
vec[length(vec)]  # Accessing the last element

# Removing the first element
vec[-1]  
vec_red = vec[-1]  
length(vec_red)  # Getting the new length after removal

# Removing the last element
vec[-length(vec)]  

# Removing specific elements using negative indexing
vec[c(-2, -3)]  
vec[-2:-4]  

# Function to trim the smallest and largest elements from the sorted vector
trim = function(x) {
  vec_sort = sort(x)  # Sorting the vector
  print(vec_sort)  
  trimmed_x = vec_sort[3:(length(vec_sort) - 2)]  # Removing smallest and largest elements
  print(trimmed_x)
}
trim(vec)  # Calling the function

# Alternative trimming function using indexing
trim1 = function(x) sort(x)[c(-1, -2, -(length(x) - 1), -length(x))]  
trim1(vec)  # Calling the function

# Accessing every second element
vec[seq(2, length(vec), 2)]  
vec[1:length(vec) %% 2 == 0]  

# Creating a sequence from 1 to 10
x = 1:10  
x  

# Filtering elements less than 5
x[x < 5]  
sum(x[x < 5])  # Sum of elements less than 5  

# Function to sum the last three elements of the vector
add = function(x) {
  s = sort(x)  # Sorting the vector
  ss = x[c((length(x) - 2), length(x) - 1, length(x))]  # Selecting the last three elements
  r = sum(ss)  # Summing them
  print(r)
}
add(vec)  # Calling the function

# Finding the index of the maximum and minimum values
which.max(x)  
which.min(x)  

# Column and row binding two sequences
cbind(1:10, 10:1)  # Column-wise binding
rbind(1:10, 10:1)  # Row-wise binding  

# Creating two vectors X and Y
X <- c(1:10)  
X  
Y <- c(1:10 * 5)  
Y  

# Performing arithmetic operations on vectors
X * Y  # Element-wise multiplication
X + Y  # Element-wise addition
X / Y  # Element-wise division
X ^ Y  # Element-wise exponentiation
log(X)  # Natural logarithm of X
exp(Y)  # Exponential function applied to Y
