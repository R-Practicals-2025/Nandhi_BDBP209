# Load the dataset from a CSV file
data = read.csv("/home/ibab/Downloads/Heart.csv", header = TRUE)

# Print the entire data frame
print(data)

# Print the dimensions of the dataset (rows, columns)
print(dim(data))

# Print the length of the dataset (number of columns)
print(length(data))

# Print the column names of the dataset
print(colnames(data))

# Print the row names of the dataset
print(rownames(data))

# Display the first few rows of the data (default is 6)
print(head(data))

# Display the last few rows of the data (default is 6)
print(tail(data))

# Print a frequency table for the 'ChestPain' column
print(table(data$ChestPain))

# Check the unique values in the 'ChestPain' column
print(unique(data$ChestPain))

# Convert 'Sex' column to a factor (0 = Female, 1 = Male)
data$sex = factor(data$Sex, levels = c(0, 1))

# Check if 'sex' column is a factor
print(is.factor(data$sex))

# Print the class of the 'sex' column
print(class(data$sex))

# Print the levels of the 'sex' factor
print(levels(data$sex))

# Print the number of levels in the 'sex' factor
print(nlevels(data$sex))

# Print the first 5 values of the 'ChestPain' column
print(data$ChestPain[1:5])

# Calculate and print the mean of 'Chol' (Cholesterol) column
print(mean(data$Chol))

# Calculate and print the mean of 'RestBP' (Resting Blood Pressure) column
print(mean(data$RestBP))

# Calculate and print the median of 'Chol' (Cholesterol) column
print(median(data$Chol))

# Load the 'modeest' library to calculate the mode of the 'Chol' column
library(modeest)

# Calculate and print the mode of the 'Chol' column
print(mlv(data$Chol))

# Calculate and print the frequency of the mode (most frequent value)
print(mfv(data$Chol))

# Find the index of the maximum value in the 'Chol' column
print(which.max(data$Chol))

# Find the index of the maximum value in the 'Sex' column
print(which.max(data$Sex))

# Calculate and print the standard deviation of 'Chol' (Cholesterol) column
print(sd(data$Chol))

# Calculate and print the standard deviation of 'MaxHR' (Maximum Heart Rate) column
print(sd(data$MaxHR))

# Print a summary of the 'Chol' column (includes min, max, mean, etc.)
print(summary(data$Chol))

# Attach the data to access columns directly (use carefully, could overwrite objects)
attach(data)

# Create a histogram of the 'Chol' column
hist(Chol)

# Load the 'moments' library to calculate skewness and kurtosis
library(moments)

# Calculate and print the skewness of 'Chol' (Cholesterol) column
print(skewness(data$Chol))

# Calculate and print the kurtosis of 'Chol' (Cholesterol) column
print(kurtosis(data$Chol))

# Create a boxplot for 'Chol' (Cholesterol) column
boxplot(data$Chol)

# Create a customized boxplot for 'Chol', with yellow color and blue border, horizontal orientation
boxplot(data$Chol, xlabel = "Spread of Chol", ylabel = "Chol", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Boxplot for 'Chol' with different range settings
boxplot(data$Chol, range = 0.1, xlabel = "Spread of Chol", ylabel = "Chol", horizontal = TRUE, border = c("blue"), col = c("yellow"))
boxplot(data$Chol, range = 0.2, xlabel = "Spread of Chol", ylabel = "Chol", horizontal = TRUE, border = c("blue"), col = c("yellow"))
boxplot(data$Chol, range = 0.05, xlabel = "Spread of Chol", ylabel = "Chol", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Boxplot for 'RestBP' (Resting Blood Pressure) column
boxplot(data$RestBP, xlabel = "Spread of RestBP", ylabel = "RestBP", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Boxplot for 'MaxHR' (Maximum Heart Rate) column
boxplot(data$MaxHR, xlabel = "Spread of MaxHR", ylabel = "MaxHR", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Filter rows where Cholesterol is greater than or equal to 20
filter1 = subset(data, data$Chol >= 20)
print(filter1)

# Print the dimensions of the filtered dataset
print(dim(filter1))

# Filter rows where ChestPain is 'asymptomatic'
filter2 = unique(subset(data, data$ChestPain == "asymptomatic"))
print(filter2)

# Manually select specific rows by their indices and print the result
filter3 = data[c(2, 4, 6, 9, 23, 28, 31), ]
print(filter3)

# Find the indices of rows where Sex is "1" (Male)
filter4_ind = which(data$Sex == "1")
print(filter4_ind)

# Filter rows based on the previously identified indices (males)
filter4 = data[filter4_ind, ]
print(filter4)

# Write the filtered data (males) to a new CSV file
write.csv(filter4, file = "lab4_female_Heart.csv")

# Create a new dataframe with Chol, RestBP, and a new calculated column
new_data <- data.frame(Chol = data$Chol, RestBP = data$RestBP, new_column = data$Chol * data$RestBP / 234)

# Print the dimensions of the new dataframe
print(dim(new_data))

# Print the new dataframe
print(new_data)
