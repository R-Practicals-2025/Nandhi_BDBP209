# Function to replace all the negative values in a vector by zeros
replace_negatives <- function(input_vector) {
  input_vector[input_vector < 0] <- 0
  return(input_vector)
}
# Test the replace_negatives function
replace_negatives(c(-1, 3, -5, -8, 6))


# Function to calculate the factorial of a number using Stirling’s approximation
stirling_factorial <- function(number) {
  if (number < 0) {
    print("Factorial is not defined for negative numbers.")
  } else {
    result <- sqrt(2 * pi * number) * (number / exp(1))^number *
      (1 + 1 / (12 * number) + 1 / (288 * number^2) - 139 / (51840 * number^3) - 571 / (2488320 * number^4))
    return(result)
  }
}
# Test the stirling_factorial function
stirling_factorial(6)


# Function to sum the digits of a number
sum_digits <- function(input_number) {
  # Convert the number to string, split it into individual digits, and sum them
  digit_list <- as.numeric(unlist(strsplit(as.character(input_number), "")))
  return(sum(digit_list))
}
# Test the sum_digits function
sum_digits(123)
