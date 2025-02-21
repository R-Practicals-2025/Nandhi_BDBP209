# Define two vectors X and Y with specified elements
X = c('a', 'b', 'c', 'd', 'e')
Y = c('d', 'e', 'f', 'g')

# Print the vectors
print(X)
print(Y)

# Perform a union operation between X and Y and print the result
print(union(X, Y))

# Combine set difference (X - Y), intersection (X ∩ Y), and set difference (Y - X)
print(c(setdiff(X, Y), intersect(X, Y), setdiff(Y, X)))

# Perform an intersection operation between X and Y and print the result
print(intersect(X, Y))
print(X[X %in% Y])  # Another approach for intersection

# Perform a difference operation between X and Y and print the result
print(setdiff(X, Y))  # X - Y
print(setdiff(Y, X))  # Y - X

# Use setequal() to check if the union of X and Y is equal to the sequence of X - Y, X ∩ Y, and Y - X
print(setequal(c(setdiff(X, Y), intersect(X, Y), setdiff(Y, X)), union(X, Y)))

# List the elements of Y that are present in X using two different approaches
print(intersect(X, Y))  # Approach 1
print(Y[Y %in% X])      # Approach 2

# Print the elements of X that are present in Y
print(intersect(X, Y))
