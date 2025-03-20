# Rounding numbers to a specific number of decimal places
round(12.1343, digits=3)
round(123.12344, digits=3)
round(1234.12344, digits=3)
round(12345.12344, digits=3)

# Setting the global digit display option
options(digits=15)
round(12345.123466664, digits=3)

# Formatting rounded values to 3 decimal places
formatC(round(12345.12344, digits=3), format="f", digits=3)

# Printing numbers
print(1234.12344)
print(1234.723, digits=3)
print(1234.723, digits=5)

# Rounding large numbers
round(123456788.123, digits=3)
print(round(123456788.123, digits=2), digits=20)
print(round(123456789.1234, digits=4), digits=20)

# Concatenating strings using paste
paste("Hello World")
paste("Hello", "World")

# Creating sequences using paste
paste(1:10) 
paste(1:10)[4]

# Converting to numeric
as.numeric(paste(1:10))

# Collapsing elements in paste
paste(1:10, collapse=".")

# Concatenating vectors with separators
paste(c('Hello',"World"), 1:10, sep='-')
print(paste('Hello', 1:10, sep='-'))

# Generating numeric sequences
0:15
15:5
seq(0, 1.5, 0.1)
seq(6, 4, -0.2)

# Creating a numeric vector
N <- c(55,76,92,103,84,88,121,91,65,77,99)
N

# Creating sequences with different parameters
seq(from=0.04, by=0.01, length=11)
seq(0.04, by=0.01, along=N)
seq(from=0.04, to=0.14, along=N)

# Generating repeating sequences
sequence(c(4,3,4,4,4,5))
rep(9, 5)
rep(1:4, 2)
rep(1:4, each=2)
rep(1:4, each=2, times=3)
rep(1:4, 1:4)
rep(1:4, c(4,1,4,2))
rep(c("cat", "dog", "goldfish", "rat"), c(2,3,2,1))

# Generating sequences with different methods
seq(-1,1, by=0.1) 
seq(-1,1, 0.1)
seq(-1,1, length=7)
-1 + 0.1 * 0:20

# Testing invalid inputs in seq
seq('TRue')
seq("FALSE")    
seq("11,3,4")

# Exploring special numerical cases
3/0  # Infinity
exp(-Inf)  # Exponential of negative infinity
(0:4)**Inf  # Raising numbers to infinity
0/0  # Undefined (NaN)
Inf - Inf  # Undefined (NaN)
Inf/Inf  # Undefined (NaN)

# Checking for finite and infinite numbers
is.finite(10)
is.infinite(10)
is.infinite(Inf)

# Handling missing values (NA)
y <- c(4, NA, 7)
is.na(y)  # Check for NA values
y[!is.na(y)]  # Exclude NA values

# Creating a data frame with missing values
c1 <- c(1,2,3,NA)
c2 <- c(5,6,NA,8)
c3 <- c(9,NA,11,12)
c4 <- c(NA,14,15,16)
full.frame <- data.frame(c1, c2, c3, c4)
full.frame

# Removing rows with missing values in column c1
reduced.frame <- full.frame[!is.na(full.frame$c1),]
reduced.frame

# Finding NA values in a vector
v <- c(1:6, NA, NA, 9:12)
seq(along=v)[is.na(v)]
which(is.na(v))

# Selecting specific rows from a data frame
full.frame[c(seq(11:21)),]
