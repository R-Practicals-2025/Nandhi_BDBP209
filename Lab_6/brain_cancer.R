# Reading data from a CSV file
data1 = read.csv("/home/ibab/Downloads/BrainCancer.csv", header = TRUE)

# Printing the dimensions of the dataset (rows and columns)
print(dim(data1))  

# Printing the number of columns in the dataset
print(length(data1))

# Accessing the 'sex' column (Before factor conversion)
print(data1$sex)

# Converting 'sex' column into a factor with specific levels
data1$sex = factor(data1$sex, levels = c("Male", "Female"))

# Checking if the 'sex' column is now a factor
print(is.factor(data1$sex))  

# Printing the class of the 'sex' column
print(class(data1$sex))  

# Displaying the levels in the 'sex' factor
print(levels(data1$sex))

# Counting the number of levels in 'sex' factor
print(nlevels(data1$sex))

# Generating factor levels using gl() function
print(gl(4, 3))        # 4 groups, each repeated 3 times
print(gl(4, 3, 24))    # 4 groups, repeated 3 times within 24 elements
print(gl(4, 3, 5))     # 4 groups, repeated 3 times within 5 elements

# Creating a factor with custom labels
temp <- gl(3, 8, 24, labels = c(0, 1, 2))
print(temp)

# Creating another factor with categorical labels
temp1 <- gl(3, 8, 24, labels = c("Hard", "Medium", "Soft"))
print(temp1)

# Combining factors into a data frame
fac_df = data.frame(temp, temp1)
print(fac_df)

# Adding a categorical 'temperature' variable to the dataset
Temp_col = c("Hot", "Cold", "Lukewarm")
temperature = gl(3, 2, nrow(data1), labels = Temp_col)
new_data = data.frame(data1, temperature)
print(new_data)

# Using pmin() to find the element-wise minimum values
print(pmin(data1$gtv, data1$time, data1$ki))

# Using pmax() to find element-wise maximum values between two vectors
x1 = c(1, 2, 3, 4)
x2 = c(5, 6, 7, 8)
print(pmax(x1, x2))

# Difference between rank, sort, and order functions
ranks = rank(data1$gtv)      # Assigning ranks to values
sorted = sort(data1$gtv)     # Sorting values
ordered = order(data1$gtv)   # Getting the index order for sorting

# Creating a data frame to compare the original, ranked, and sorted values
view = data.frame(data1$gtv, ranks, sorted, ordered)
print(view)

# Using the order() function to sort 'gtv' column
print(data1$gtv[ordered])

# Converting a subset of the data frame into a matrix
filter1 = data1[1:6, 3:8]   # Selecting a subset of rows and columns
filter1_matrix = as.matrix(filter1)  
print(filter1_matrix)

# Checking the class and attributes of the matrix
print(class(filter1_matrix))
print(attributes(filter1_matrix))

# Creating a new column by summing existing columns
newcol = data1$ki + data1$gtv + data1$time
new_col = data.frame(data1, newcol)
print(new_col)

# Printing column names of the new data frame
print(colnames(new_col))

# Adding the new column using cbind()
new2 = cbind(data1, newcol)
print(new2)

# Selecting specific rows and appending them to the dataset
filt2 = data1[c(1, 3, 8),]
new3 = rbind(data1, filt2)
print(new3)

# Appending multiple new rows to the dataset
new_rows = data1[25:36,]
new_dataframe = rbind(data1, new_rows)
print(dim(new_dataframe))  # Printing the dimensions after adding rows
print(dim(data1))          # Original dimensions of the dataset

# Using tapply() to calculate the mean of 'gtv' based on different groupings
tapply(data1$gtv, data1$ki, mean)
tapply(data1$gtv, data1$ki, mean, trim=0.1) # Trimming 10% of extreme values
tapply(data1$gtv, data1$sex, mean)
tapply(data1$gtv, data1$diagnosis, mean)

# Naming rows and columns in a matrix
x <- matrix(c(1, 0, 5, 3, 1, 1, 3, 1, 3, 3, 1, 0, 2, 2, 1, 0), nrow=4)
print(x)

# Printing row and column names before assigning names
print(rownames(x))
print(colnames(x))

# Assigning row names with prefix 'Trial.'
rownames(x) = rownames(x, do.NULL=FALSE, prefix='Trial.')
print(x)

# Assigning column names using a vector
drug = c("Aspirin", "Paracetamol", "Dolo", "Other")
colnames(x) = drug
print(x)

# Another way to assign column names using paste()
x1 = matrix(c(1, 0, 5, 3, 1, 1, 3, 1, 3, 3, 1, 0, 2, 2, 1, 0), nrow=4)
dimnames(x1) = list(NULL, paste("drug", 1:4, sep=""))
print(x1)

# Performing row-wise and column-wise calculations
print(mean(x[,4]))  # Mean of the 4th column
print(var(x[4,]))   # Variance of the 4th row
print(rowSums(x))   # Sum of each row
print(colSums(x))   # Sum of each column

# Applying functions using apply()
print(apply(x, 1, sum))    # Sum of each row
print(apply(x, 2, sum))    # Sum of each column

# Applying square root function to matrix elements
print(apply(x, 2, sqrt))
print(apply(x, 2, function(x) x^2 + x)) # Applying custom function

# Calculating row and column means
print(rowMeans(x))
print(colMeans(x))
print(apply(x, 1, mean))

# Using rowsum() function to compute row-wise sums based on a grouping variable
group = c("A", "B", "B", "A")  # Defining groups
print(rowsum(x, group))

# Getting row and column indices of a matrix
print(row(x))
print(col(x))

# Applying tapply() function with row and column grouping
print(tapply(x, list(group[row(x)], col(x)), sum))

# Using aggregate() to summarize the matrix data
print(aggregate(x, list(group), sum))
