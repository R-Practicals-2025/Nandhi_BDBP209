# Load the dataset from a CSV file
brain_data <- read.csv("/home/ibab/Downloads/BrainCancer.csv", header=TRUE)
print(colnames(brain_data))

# Create a new column by squaring the 'gtv' column and adding the 'time' column
new_column <- (brain_data$gtv^2) + brain_data$time

# Add the new column to the original data
modified_data <- cbind(brain_data, new_column)
print(modified_data)

# Print the row and column names of the modified data
print(colnames(modified_data))
print(rownames(modified_data))

# Change row names using a more descriptive naming convention
rownames(modified_data) <- paste("Subject-", 1:nrow(modified_data), sep="")
print(modified_data)

# Remove the 'ki' column by assigning NULL to it
modified_data$ki <- NULL
print(colnames(modified_data))
