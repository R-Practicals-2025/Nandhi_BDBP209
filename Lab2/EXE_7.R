# Introduction to Lists in R
# Lists are useful for mixing data types and can store different dimensions, 
# such as cells, vectors, and higher-order arrays.
# R often uses lists to store function outputs, making it essential to understand them.

# Creating a character and numeric vector
Xvec <- c("Jayashree", "Nagesh", 119, "quantum", 2000, 2003, 2006, 2012, 2017, 2023)
Xvec  # Displaying the vector

# Creating a list containing different data types
Mylist <- list(name = "JN", mixedlist = Xvec, numbers = 1:10)
Mylist  # Displaying the list

# Accessing elements of the list using $
Mylist$numbers  # Extracting the 'numbers' element

# Accessing elements using single bracket []
Mylist[3]  # Returns a list containing the third element

# Accessing elements using double brackets [[]]
Mylist[[3]]  # Returns the actual content (a numeric vector in this case)

# Explanation of difference:
# - Mylist[3] returns a list with the third element still inside a list structure.
# - Mylist[[3]] extracts only the third element, removing the list structure.

# Checking class types
class(Mylist[3])   # This is still a list
class(Mylist[[3]]) # This is a numeric vector
