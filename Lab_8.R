# Lab8.1: Check if an integer is a palindrome
is_palindrome_int <- function(num){
  num = as.character(num)  # Convert integer to string
  reversed_num <- paste(rev(strsplit(num, NULL)[[1]]), collapse = "")  # Reverse the string
  return(num == reversed_num)  # Return TRUE if the string equals its reverse
}
is_palindrome_int(121)  # Should return TRUE
is_palindrome_int(123)  # Should return FALSE

# Lab8.2: Slice the string 'seemerightnow' into specific substrings
input_string <- "seemerightnow"
# 8.2.a: Extract "see" (characters 1 to 3)
substring1 <- substr(input_string, 1, 3)
print(substring1)
# 8.2.b: Extract "me" (characters 4 to 5)
substring2 <- substr(input_string, 4, 5)
print(substring2)
# 8.2.c: Extract "right" (characters 6 to 10)
substring3 <- substr(input_string, 6, 10)
print(substring3)

# Lab8.3: Determine the fraction of G and C bases in the sequence
dna_sequence <- "ATTGCGCATAGTCCGGG"
bases <- strsplit(dna_sequence, "")[[1]]  # Split sequence into individual characters
gc_fraction <- sum(bases == "G" | bases == "C") / nchar(dna_sequence)  # Count G and C and divide by total length
print(gc_fraction)

# Lab8.4: Check if a DNA sequence is palindromic (equal to its reverse complement)
is_palindrome_dna <- function(dna_seq){
  dna_seq <- toupper(dna_seq)  # Ensure sequence is in uppercase
  complement <- chartr("ATGC", "TACG", dna_seq)  # Get complement: A->T, T->A, G->C, C->G
  reverse_complement <- paste(rev(strsplit(complement, NULL)[[1]]), collapse = "")  # Reverse the complement
  return(dna_seq == reverse_complement)  # Return TRUE if sequence equals its reverse complement
}
is_palindrome_dna("TGGATCCA")  # Should return TRUE
is_palindrome_dna("CCTGA")     # Should return FALSE

# Lab8.5: Group words in a sentence by length and print the largest and second largest groups
sentence_text <- "She sells hundreds of sea oysters on the sea shore."
clean_sentence <- gsub("[[:punct:]]", "", sentence_text)  # Remove punctuation
word_list <- unlist(strsplit(clean_sentence, " "))  # Split sentence into words
word_lengths <- nchar(word_list)  # Get length of each word
words_by_length_group <- split(word_list, word_lengths)  # Group words by their length
print("Words grouped by length:")
print(words_by_length_group)
# Print the group(s) of words with the largest length
print("Largest word group:")
print(words_by_length_group[length(words_by_length_group)])
# Print the group(s) of words with the second largest length
print("Second largest word group:")
print(words_by_length_group[length(words_by_length_group)-1])

# Lab8.6: Analysis on worldfloras data by continent
library(moments)
# 8.6.a: Load data and create separate dataframes for each continent
world_data <- read.table("/home/ibab/R/worldfloras.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
unique_continents <- unique(world_data$Continent)
for(continent in unique_continents) {
  # Create a new dataframe for each continent (df_Asia, df_Europe, etc.)
  assign(paste0("df_", continent), subset(world_data, Continent == continent))
}

# 8.6.b: Boxplot and summary statistics for Floral Count by continent
boxplot(Flora ~ Continent, data = world_data,
        main = "Floral Count Distribution by Continent",
        xlab = "Continent", ylab = "Floral Count", col = "lightblue")
# Calculate statistics: Mean, SD, Skewness, and Kurtosis for each continent's floral count
floral_statistics <- lapply(split(world_data$Flora, world_data$Continent), function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),    # Right-skew: few high floral counts
    Kurtosis = kurtosis(x, na.rm = TRUE))     # High kurtosis: peaked distribution with outliers
})
print("Floral Count Statistics by Continent:")
print(floral_statistics)
# Interpretation:
# Most continents show a right-skewed (positive skew) and leptokurtic distribution,
# indicating a few countries have very high floral counts.

# 8.6.c: Boxplot and histogram for Population by continent, with summary stats
boxplot(Population ~ Continent, data = world_data,
        main = "Population Distribution by Continent",
        xlab = "Continent", ylab = "Population", col = "lightgreen")
# Calculate statistics for population
population_statistics <- lapply(split(world_data$Population, world_data$Continent), function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),   # Right-skew: a few countries have very high populations
    Kurtosis = kurtosis(x, na.rm = TRUE))    # High kurtosis: distribution is peaked with extreme values
})
print("Population statistics by Continent:")
print(population_statistics)
# Interpretation:
# Population data are highly right-skewed in some continents,
# suggesting a few countries dominate in population size.
# There may be a relation with floral count patterns, but formal correlation is needed.

# Lab8.7: Process HumanBones.txt to create a dataframe with Category, Bone, and Count
lines <- readLines("/home/ibab/R/HumanBones.txt")
bone_records <- list()  # List to store bone records
current_category <- NA  # Current category holder

for (line in lines) {
  line <- trimws(line)  # Remove extra spaces
  if(line == "") next  # Skip empty lines
  if (!grepl("\\(", line)) {  # If line doesn't contain '(', it's a category name
    current_category <- line
  } else {  # Otherwise, it's a bone entry
    parts <- unlist(strsplit(line, "\\("))
    bone_name <- trimws(parts[1])
    count_text <- gsub("\\).*", "", parts[2])  # Remove closing parenthesis and extra text
    count <- as.numeric(unlist(strsplit(count_text, " "))[1])  # Take the first number
    bone_records[[length(bone_records) + 1]] <- data.frame(Category = current_category,
                                                           Bone = bone_name,
                                                           Count = count,
                                                           stringsAsFactors = FALSE)
  }
}
# Combine all individual bone records into one dataframe
human_bones_data <- do.call(rbind, bone_records)
print(human_bones_data)

# Lab8.8: Create a frequency table for total bones per category and plot a bar plot
bone_frequency <- aggregate(Count ~ Category, data = human_bones_data, FUN = function(x) sum(x))
print(bone_frequency)
max_bone_category <- bone_frequency[which.max(bone_frequency$Count), ]
cat("Category with maximum bones:\n")
print(max_bone_category)
barplot(bone_frequency$Count,
        names.arg = bone_frequency$Category,
        main = "Total Number of Bones by Category",
        xlab = "Category",
        ylab = "Total Bones",
        col = "skyblue",
        las = 2)  # Vertical labels for clarity

# Lab8.9: Subset "Legs" category and print bone names longer than 5 letters
legs_bones <- subset(human_bones_data, Category == "Legs")
print(legs_bones$Bone[nchar(legs_bones$Bone) > 5])

# Lab8.10: List all bones starting with "M" and change lowercase "a" to uppercase "A"
bones_starting_with_M <- human_bones_data$Bone[grep("^M", human_bones_data$Bone)]
print(gsub("a", "A", bones_starting_with_M))

# Lab8.11: List bones ending with "e" and convert them to lowercase
bones_ending_in_e <- subset(human_bones_data, grepl("e$", Bone))
bones_ending_in_e$Bone <- tolower(bones_ending_in_e$Bone)
print(bones_ending_in_e$Bone)

# Lab8.12: List bones with two "o"s in their names
bones_with_two_o <- subset(human_bones_data, grepl("o.*o", Bone))
print(bones_with_two_o$Bone)
