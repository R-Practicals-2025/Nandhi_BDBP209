# Exercise-11
data = read.csv("/home/ibab/Desktop/BrainCancer.csv")  # Load the CSV file into a dataframe

# Create a 4x5 matrix X with some data
X = matrix(c(1, 0, 2, 5, 3, 1, 1, 3, 1, 3, 3, 1, 0, 2, 2, 1, 0, 2, 1, 0), nrow = 4)
print(X)  # Print the matrix X

# Print the row names and column names of the matrix X
print(rownames(X))  
print(colnames(X))

# Set custom row names for the matrix X using a prefix 'Trial'
rownames(X) = rownames(X, do.NULL = FALSE, prefix = 'Trial')
print(rownames(X))  # Print row names after modification

# Print the matrix X after modifying the row names
print(X)

# Create a vector of drug names to use as column names for the matrix X
drugs = c('aspirin', 'paracetamol', 'nurofen', 'placebo', 'hedex')
colnames(X) = drugs  # Assign the vector as column names
print(colnames(X))  # Print the column names
print(X)  # Print the matrix X with updated column names

# Change column names to "drug1", "drug2", ..., "drug5"
dimnames(X) = list(NULL, paste("drug", 1:5, sep = ""))
print(X)  # Print the matrix X with new column names

# Exercise-12
# Print the mean of the values in the 5th column (hedex)
print(mean(X[, 5]))  

# Print the variance of the values in the 4th row
print(var(X[4,]))  

# Sum of elements along each row
print(rowSums(X))

# Sum of elements along each column
print(colSums(X))

# Apply the sum function across each row and column
print(apply(X, 1, sum))  # Sum across rows
print(apply(X, 2, sum))  # Sum across columns

# Row means
print(rowMeans(X))  # Mean of each row

# Column means
print(colMeans(X))  # Mean of each column

# Apply the mean function across each row and column
print(apply(X, 1, mean))  # Mean across rows
print(apply(X, 2, mean))  # Mean across columns

# Apply square root function to each column
print(apply(X, 2, sqrt))

# Apply a custom function to each column (X^2 + X)
print(apply(X, 2, function(X) X^2 + X))

# Grouping and summing rows based on a grouping factor
group = c("A", "B", "B", "A")
print(rowsum(X, group))  # Sum rows based on group labels

# Get the row and column indices
print(row(X))  # Row indices
print(col(X))  # Column indices

# Sum by group and column
print(tapply(X, list(group[row(X)], col(X)), sum))

# Aggregate sum by group
print(aggregate(X, list(group), sum))

# Apply sampling function to each column
print(apply(X, 2, sample))

# Add a new row containing the means of each column
X = rbind(X, apply(X, 2, mean))
print(X)

# Add a new column containing the variance of each row
X = cbind(X, apply(X, 1, var))
print(X)

# Create a heading with drug names and 'var' for the new column
heading = c(paste('drug', 1:5, sep = ''), 'var')

# Update column names and print the matrix
dimnames(X) = list(NULL, heading)
print(X)

# Add 'mean' as the last row and update row names
rowhead = c(paste('Trial', 1:4, sep = ''), 'mean')
dimnames(X) = list(rowhead, heading)
print(X)

# Exercise-13
eg_sweep = data.frame(data$ki, data$gtv, data$time)

# Method 1 to perform sweep action (subtracting column means)
cols = apply(eg_sweep, 2, mean)  # Compute mean of each column
print(cols)

# Repeat the column means to match the dimensions of the data frame
col.means = matrix(rep(cols, rep(dim(eg_sweep)[1], dim(eg_sweep)[2])), nrow = dim(eg_sweep)[1])
print(col.means)

# Subtract column means from the data frame
eg_sweep_alt = eg_sweep - col.means
print(eg_sweep_alt)

# Perform sweep using the sweep function
eg_sweep_alt2 = sweep(eg_sweep, 2, cols)
print(eg_sweep_alt2)

# Exercise-14
pgdata = read.table("/home/ibab/Downloads/pgfull.txt")  # Load data from file
print(pgdata)  # Print the data

# Print the names (column headers) of the data
print(names(pgdata))

# Convert the first 54 columns into a matrix 'species'
species = as.matrix(pgdata[, 1:54])

# Replace any missing values (NA) with 0
species[is.na(species)] <- 0

# Find the index of the maximum value in each column
print(max.col(species))  # Get column indices of the maximum values

# Get the names of the species corresponding to the maximum values
print(names(species)[max.col(species)])

# Print the table of species with the most occurrences
print(table(names(species)[max.col(species)]))

# Exercise-15-(i) Lists

# Create vectors for apples, oranges, chalk, and cheese
apples <- c(4, 4.5, 4.2, 5.1, 3.9)
oranges <- c(TRUE, TRUE, FALSE)
chalk <- c("limestone", "marl", "oolite", "CaCO3")
cheese <- c(3.2 - 4.5i, 12.8 + 2.2i)

# Combine these vectors into a list 'items'
items <- list(apples, oranges, chalk, cheese)

# Print the list 'items'
print(items)

# Attempt to create a data frame directly from the vectors (this will not work as vectors have different lengths)
# data.frame(apples, oranges, chalk)

# Accessing elements in the list using double square brackets
print(items[[3]])  # Access the 3rd element (chalk)
print(items[[3]][3])  # Access the 3rd element of chalk
print(items[3])  # Access the whole list element (chalk)

# Accessing the second element of the first list element (apples)
print(items[[1]][2])  # Access the 2nd element of apples
print(items[1][2])  # Access the second element of the first list

# Exercise-15-(ii) Named lists

# Create a named list
items <- list(first = apples, second = oranges, third = chalk, fourth = cheese)

# Print the names of the list elements
print(names(items))

# Access the 'fourth' element (cheese) from the list using the name
print(items$fourth)

# Print the class of the list
print(class(items))

# Apply functions to each element in the list (length, class, and mean)
print(lapply(items, length))  # Length of each element
print(lapply(items, class))   # Class of each element
print(lapply(items, mean))    # Calculate mean for each element (works for numeric elements only)

# Print summary and structure of the list
print(summary(items))
print(str(items))
