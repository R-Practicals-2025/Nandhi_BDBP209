# Reading the heart disease dataset from a CSV file
data = read.csv("/home/ibab/Downloads/Heart.csv", header = TRUE)

# Printing the dimensions of the dataset (number of rows and columns)
print(dim(data))

# Checking the class of the 'ChestPain' column before factor conversion
print(class(data$ChestPain))

# Converting 'ChestPain' into a factor with specified levels
data$ChestPain = factor(data$ChestPain, levels = c('typical', 'asymptomatic', 'nontypical'))

# Checking if 'ChestPain' is now a factor
print(is.factor(data$ChestPain))

# Printing the class of 'ChestPain' after conversion
print(class(data$ChestPain))

# Printing unique values present in the 'ChestPain' column
print(unique(data$ChestPain))

# Displaying the levels of the factor variable
print(levels(data$ChestPain))

# Printing the number of levels in 'ChestPain'
print(nlevels(data$ChestPain))

# Creating a categorical 'temperature' variable with three levels
Temp_col = c("Hot", "Cold", "Lukewarm")
temperature = gl(3, 2, nrow(data), labels = Temp_col)

# Adding 'temperature' as a new column to the dataset
new_data = data.frame(data, temperature)
print(new_data)

# Finding the element-wise minimum values among selected columns
print(pmin(data$Age, data$RestBP, data$Chol))

# Finding the element-wise maximum values among selected columns
print(pmax(data$Age, data$RestBP, data$Chol))

# Ranking, sorting, and ordering the 'Chol' column
ranks = rank(data$Chol)     # Assigning ranks
sorted = sort(data$Chol)    # Sorting values in ascending order
ordered = order(data$Chol)  # Getting the index positions for sorting

# Creating a data frame to compare original, ranked, and sorted values
view = data.frame(data$Chol, ranks, sorted, ordered)
print(view)

# Creating a new data frame with sorted cholesterol values and corresponding blood pressure values
view1 = data.frame(sorted, data$RestBP[ordered])
print(view1)

# Accessing the 203rd row of the dataset
print(data[203,])

# Writing the sorted and ordered dataset to a CSV file
write.csv(view1, file = "lab5_ordered_data.csv")

# Extracting a subset of the data (rows 1 to 6 and columns 3 to 8)
sub_data = data[1:6, 3:8]
print(sub_data)

# Converting the subset data frame into a matrix
sub_data_mat = as.matrix(sub_data)

# Printing the attributes of the converted matrix
print(attributes(sub_data_mat))

# Creating a new column by summing 'Age', 'RestBP', and 'Chol'
newcol = data$Age + data$RestBP + data$Chol

# Adding the new column to the existing dataset
newcoladded = data.frame(data, newcol)
print(colnames(newcoladded))  # Printing column names after addition

# Adding the same column again using cbind()
newcoladded2 = cbind(newcoladded, newcol)
print(colnames(newcoladded2))  # Printing column names after second addition

# Selecting rows 25 to 36 from the dataset
new_rows = data[25:36,]

# Appending the selected rows to the original dataset
new_dataframe = rbind(data, new_rows)

# Printing dimensions of the new dataset after row binding
print(dim(new_dataframe))
print(dim(data))  # Checking the original dataset dimensions for comparison

# Using tapply() to compute the mean of 'Chol' based on different groupings
tapply(data$Chol, data$AHD, mean)           # Grouped by 'AHD'
tapply(data$Chol, data$AHD, mean, trim=0.1) # Trimmed mean (ignoring extreme values)
tapply(data$Chol, data$Sex, mean, trim=0.1) # Mean grouped by 'Sex' with trimming
tapply(data$Chol, data$Thal, mean)          # Mean grouped by 'Thal'
tapply(data$Chol, data$RestECG, mean)       # Mean grouped by 'RestECG'
