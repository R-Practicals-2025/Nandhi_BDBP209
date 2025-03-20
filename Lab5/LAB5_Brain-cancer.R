# Load the dataset from the CSV file
data1 = read.csv("/home/ibab/Downloads/BrainCancer.csv", header = TRUE)

# Print the entire dataset
print(data1)

# Print the dimensions of the dataset (rows and columns)
print(dim(data1))

# Print the number of columns in the dataset
print(length(data1))

# Print the column names of the dataset
print(colnames(data1))

# Print the row names of the dataset
print(rownames(data1))

# Display the first 30 rows of the data
print(head(data1, 30))

# Display the last few rows of the data (default is 6 rows)
print(tail(data1))

# Print a frequency table for the 'diagnosis' column (e.g., benign or malignant)
print(table(data1$diagnosis))

# Print a frequency table for the 'gtv' column (Gross Tumor Volume)
print(table(data1$gtv))

# Convert 'sex' column to a factor with levels "Male" and "Female"
data1$sex = factor(data1$sex, levels = c("Male", "Female"))

# Check if 'sex' column is a factor
print(is.factor(data1$sex))

# Print the class of the 'sex' column
print(class(data1$sex))

# Print the levels of the 'sex' factor
print(levels(data1$sex))

# Print the number of levels in the 'sex' factor
print(nlevels(data1$sex))

# Print the first 5 values of the 'diagnosis' column
print(data1$diagnosis[1:5])

# Calculate and print the mean of the 'gtv' column (Gross Tumor Volume)
print(mean(data1$gtv))

# Calculate and print the mean of the 'time' column (Time variable)
print(mean(data1$time))

# Calculate and print the median of the 'gtv' column
print(median(data1$gtv))

# Load the 'modeest' library to calculate the mode of the 'gtv' column
library(modeest)

# Calculate and print the mode of the 'gtv' column
print(mlv(data1$gtv))

# Calculate and print the frequency of the mode (most frequent value) of 'gtv'
print(mfv(data1$gtv))

# Calculate and print the mode using the 'mode()' function
print(mode(data1$gtv))

# Find the index of the maximum value in the 'gtv' column
print(which.max(data1$gtv))

# Find the index of the maximum frequency in the 'gtv' table (frequency of values)
print(which.max(table(data1$gtv)))

# Calculate and print the standard deviation of the 'gtv' column
print(sd(data1$gtv))

# Print a summary of the 'gtv' column (includes min, max, mean, etc.)
print(summary(data1$gtv))

# Attach the data to access columns directly (use cautiously as it could overwrite objects)
attach(data1)

# Create a histogram of the 'gtv' column
hist(gtv)

# Load the 'moments' library to calculate skewness and kurtosis
library(moments)

# Calculate and print the skewness of 'gtv' (measures the asymmetry of the data)
print(skewness(data1$gtv))

# Calculate and print the kurtosis of 'gtv' (measures the "tailedness" of the data distribution)
print(kurtosis(data1$gtv))

# Create a boxplot for 'gtv' (Gross Tumor Volume)
boxplot(data1$gtv)

# Create a customized boxplot for 'gtv', with yellow color and blue border, horizontal orientation
boxplot(data1$gtv, xlabel = "Spread of gtv", ylabel = "GTV", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Create boxplots for 'gtv' with different range settings
boxplot(data1$gtv, range = 0.1, xlabel = "Spread of gtv", ylabel = "GTV", horizontal = TRUE, border = c("blue"), col = c("yellow"))
boxplot(data1$gtv, range = 0.2, xlabel = "Spread of gtv", ylabel = "GTV", horizontal = TRUE, border = c("blue"), col = c("yellow"))
boxplot(data1$gtv, range = 0.05, xlabel = "Spread of gtv", ylabel = "GTV", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Boxplots for other columns ('time' and 'ki') with customized labels and colors
boxplot(data1$time, xlabel = "Spread of time", ylabel = "GTV", horizontal = TRUE, border = c("blue"), col = c("yellow"))
boxplot(data1$ki, xlabel = "Spread of ki", ylabel = "GTV", horizontal = TRUE, border = c("blue"), col = c("yellow"))

# Filter the rows where 'gtv' is greater than or equal to 20 and print the result
filter1 = subset(data1, data1$gtv >= 20)
print(filter1)

# Print the dimensions of the filtered dataset (number of rows and columns)
print(dim(filter1))

# Filter the rows where 'gtv' equals 2.5 and print the result
filter5 = subset(data1, data1$gtv == 2.5)
print(filter5)

# Print the dimensions of the filtered dataset for 'gtv' == 2.5
print(dim(filter5))

# Filter the rows where 'loc' is "Infratentorial" and print the result
filter2 = unique(subset(data1, data1$loc == "Infratentorial"))
print(filter2)

# Manually select specific rows by their indices and print the result
filter3 = data1[c(2, 4, 6, 9, 23, 28, 31), ]
print(filter3)

# Find the indices of rows where 'sex' is "Female"
filter4_ind = which(data1$sex == "Female")
print(filter4_ind)

# Filter the rows for females based on the indices
filter4 = data1[filter4_ind, ]
print(filter4)

# Write the filtered data (for females) to a new CSV file
write.csv(filter4, file = "lab4_female_BrainCancer.csv")

# Create a new dataframe with the 'gtv', 'ki', and a new calculated column
new_data <- data.frame(GTV = data1$gtv, ki = data1$ki, new_column = data1$gtv * data1$ki / 234)

# Print the dimensions of the new dataframe
print(dim(new_data))

# Print the new dataframe with the calculated column
print(new_data)



tapply(df$gtv, df$ki, mean) #to take out the mean of the 2 columns
tapply(df$gtv, df$ki, mean,trim=0.1) #trim helps in removing the outliers i.e when trim=0.1, 10% high and low values are discarded


print(pmin(df$gtv,df$time,df$ki)) #gives the minimum value along 3 parallel columns
print(pmax(df$gtv,df$time,df$ki)) #gives the maximum value along 3 parallel columns


ranks=rank(df$gtv) #to get the rank of the column
sorted=sort(df$gtv) #to sort the column
ordered=order(df$gtv) #gives indices of sorted column
view=data.frame(df$gtv,ranks,sorted,ordered) #adding the sorted,ordered and rank columns to the dataframe
print(view)
output=data.frame(df$diagnosis,df$gtv,ordered) #adding the ordered column from previous along with diagnosis and gtv columns
print(output)
write.csv(output,file="/home/ibab/Downloads/lab4_ordered_data_BrainCancer.csv") #writing the subset to a new file


filter1=df[1:6,3:8] #extracting rows from 1-6 and columns from 3-8
filter1mat=as.matrix(filter1) #printing the above extracted rows and columns as a matrix
print(filter1mat)
print(class(filter1mat)) #to get the class
print(mode(filter1mat)) #to get the mode
print(attributes(filter1mat)) #to get the attributes
newcol=df$ki+df$gtv+df$time #creating a new column
newcoladded=data.frame(df,newcol) #adding the new column to the exsisting dataframe
print(newcoladded)
newcoladded2=cbind(df,newcol) #2nd method
print(newcoladded2)
filter4=df[c(26,35),] #creating a subset of 26th and 35th rows
newrowadded=rbind(df,filter4) #new row created using the above subset
print(newrowadded)
print(dim(newrowadded)) #printing the dimensions of the new dataframe
print(dim(df)) #the dimensions of original dataset
