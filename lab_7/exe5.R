
# Define a vector with elements
num_vector <- c(8, 10, 12, 7, 14, 16, 2, 4, 9, 19, 20, 3, 6)

# a Values greater than 12
filtered_values_a <- num_vector[num_vector > 12]
print(filtered_values_a)

# b Values greater than 10 and less than 20
filtered_values_b <- num_vector[num_vector > 10 & num_vector < 20]
print(filtered_values_b)

# Form an array with the given elements
numeric_array <- c(2, 7, 29, 32, 41, 11, 15, NA, NA, 55, 32, NA, 42, 109)

# Create a new array without NAs and keeping values less than 100
filtered_array <- numeric_array[!is.na(numeric_array) & numeric_array < 100]
print(filtered_array)

# Replace NA with 0 in the array
array_no_na <- ifelse(is.na(numeric_array), 0, numeric_array)
print(array_no_na)

# Create a vector of gene names
gene_ids <- paste("gene", 1:7, sep="-")

# Create a vector for gender
gender_vector <- c("M", "M", "F", "M", "F", "F", "M")
print(gene_ids)
print(gender_vector)

# Define 7 result vectors
exp1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
exp2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
exp3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
exp4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
exp5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
exp6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
exp7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

# Create a data frame
experiment_data <- data.frame(gene_ids = gene_ids, gender_vector = gender_vector, exp1 = exp1, exp2 = exp2, exp3 = exp3, exp4 = exp4, exp5 = exp5, exp6 = exp6, exp7 = exp7)
print(experiment_data)

# Add new column names to the dataframe
colnames(experiment_data) <- c("Gene", "Sex", "Experiment1", "Experiment2", "Experiment3", "Experiment4", "Experiment5", "Experiment6", "Experiment7")
print(experiment_data)

# Create a subset where Experiment2 > 20
subset_exp2_gt_20 <- subset(experiment_data, Experiment2 > 20)
print(subset_exp2_gt_20)

# Create a subset where gender is Female
subset_female_gender <- subset(experiment_data, Sex == "F")
print(subset_female_gender)

# Create a subset where gender is Male and Experiment2 < 30
subset_male_exp2_lt_30 <- subset(experiment_data, Sex == "M" & Experiment2 < 30)
print(subset_male_exp2_lt_30)
